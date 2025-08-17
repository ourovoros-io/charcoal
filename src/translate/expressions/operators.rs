use crate::{error::Error, project::Project, sway, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_binary_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    operator: &str,
    lhs: &solidity::Expression,
    rhs: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    // HACK: x.code.length == 0 => x.as_contract_id().is_none()
    if let solidity::Expression::MemberAccess(_, x, member2) = lhs
        && let solidity::Expression::MemberAccess(_, x, member1) = x.as_ref()
        && member1.name == "code"
        && member2.name == "length"
    {
        let expression = translate_expression(project, module.clone(), scope.clone(), x)?;
        let type_name = get_expression_type(project, module.clone(), scope.clone(), &expression)?;

        if type_name.is_identity()
            && let solidity::Expression::NumberLiteral(_, value, _, _) = rhs
            && value == "0"
        {
            return Ok(expression.with_as_contract_id_call().with_is_none_call());
        }
    }

    let mut lhs = translate_expression(project, module.clone(), scope.clone(), lhs)?;
    let mut lhs_type = get_expression_type(project, module.clone(), scope.clone(), &lhs)?;

    let mut rhs = translate_expression(project, module.clone(), scope.clone(), rhs)?;
    let mut rhs_type = get_expression_type(project, module.clone(), scope.clone(), &rhs)?;

    if lhs_type.is_storage_key() {
        scope
            .borrow_mut()
            .set_function_storage_accesses(module.clone(), true, false);

        lhs = lhs.with_read_call();
        lhs_type = get_expression_type(project, module.clone(), scope.clone(), &lhs)?;
    }

    if rhs_type.is_storage_key() {
        scope
            .borrow_mut()
            .set_function_storage_accesses(module.clone(), true, false);

        rhs = rhs.with_read_call();
        rhs_type = get_expression_type(project, module.clone(), scope.clone(), &rhs)?;
    }

    // HACK: de-cast identity abi cast comparisons
    let mut abi_check = |lhs_type: &sway::TypeName,
                         rhs: &mut sway::Expression,
                         rhs_type: &mut sway::TypeName|
     -> bool {
        if lhs_type.is_identity()
            && let sway::Expression::FunctionCall(expr) = &rhs
            && let Some(ident) = expr.function.as_identifier()
            && ident == "abi"
            && expr.parameters.len() == 2
        {
            *rhs = expr.parameters[1].clone();

            if let sway::Expression::FunctionCall(f) = &rhs
                && let sway::Expression::MemberAccess(e) = &f.function
                && e.member == "bits"
            {
                *rhs = e.expression.clone();
            }

            *rhs_type = get_expression_type(project, module.clone(), scope.clone(), rhs).unwrap();

            return true;
        }

        false
    };

    if !abi_check(&lhs_type, &mut rhs, &mut rhs_type) {
        abi_check(&rhs_type, &mut lhs, &mut lhs_type);
    }

    rhs = coerce_expression(
        project,
        module.clone(),
        scope.clone(),
        &rhs,
        &rhs_type,
        &lhs_type,
    )
    .unwrap();

    Ok(sway::Expression::from(sway::BinaryExpression {
        operator: operator.into(),
        lhs,
        rhs,
    }))
}

#[inline]
pub fn translate_unary_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    operator: &str,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    let expression = translate_expression(project, module.clone(), scope.clone(), expression)?;

    // NOTE: Sway does not have a negate operator, so we need to make sure to use the correct translation
    if operator == "-" {
        let type_name = get_expression_type(project, module.clone(), scope.clone(), &expression)?;

        match &type_name {
            sway::TypeName::Identifier {
                name,
                generic_parameters,
            } => match (name.as_str(), generic_parameters.as_ref()) {
                ("I8" | "I16" | "I32" | "I64" | "I128" | "I256", None) => {
                    return Ok(expression.with_function_call("wrapping_neg", None, vec![]));
                }

                ("u8" | "u16" | "u32" | "u64" | "u256", None) => {
                    let bits: usize = name.trim_start_matches('u').parse().unwrap();

                    module
                        .borrow_mut()
                        .ensure_dependency_declared("signed_int = \"0.26.0\"");
                    module
                        .borrow_mut()
                        .ensure_use_declared(format!("signed_int::i{bits}::*").as_str());

                    return Ok(sway::Expression::create_function_call(
                        format!("I{bits}::from_uint").as_str(),
                        None,
                        vec![expression.clone()],
                    )
                    .with_function_call("wrapping_neg", None, vec![])
                    .with_function_call("underlying", None, vec![]));
                }

                _ => {
                    // HACK: allow literals to be negated
                    if let sway::Expression::Literal(
                        sway::Literal::DecInt(_, _) | sway::Literal::HexInt(_, _),
                    ) = &expression
                    {
                        return Ok(expression.with_function_call("wrapping_neg", None, vec![]));
                    }

                    panic!("Unhandled {type_name} negate operator translation")
                }
            },

            _ => panic!("Unhandled {type_name} negate operator translation"),
        }
    }

    Ok(sway::Expression::from(sway::UnaryExpression {
        operator: operator.into(),
        expression,
    }))
}

#[inline]
pub fn translate_power_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    lhs: &solidity::Expression,
    rhs: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    // lhs ** rhs => lhs.pow(rhs)

    // Ensure std::math::Power is imported for the pow function
    module.borrow_mut().ensure_use_declared("std::math::Power");

    let lhs = translate_expression(project, module.clone(), scope.clone(), lhs)?;

    let mut rhs = translate_expression(project, module.clone(), scope.clone(), rhs)?;
    let rhs_type = get_expression_type(project, module.clone(), scope.clone(), &rhs)?;

    rhs = coerce_expression(
        project,
        module.clone(),
        scope.clone(),
        &rhs,
        &rhs_type,
        &sway::TypeName::create_identifier("u32"),
    )
    .unwrap();

    Ok(lhs.with_function_call("pow", None, vec![rhs]))
}
