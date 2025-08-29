use crate::{error::Error, project::Project, sway, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_pre_or_post_operator_value_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    match expression {
        solidity::Expression::PreIncrement(loc, x) => {
            translate_pre_operator_expression(project, module.clone(), scope.clone(), loc, x, "+=")
        }
        solidity::Expression::PreDecrement(loc, x) => {
            translate_pre_operator_expression(project, module.clone(), scope.clone(), loc, x, "-=")
        }
        solidity::Expression::PostIncrement(loc, x) => {
            translate_post_operator_expression(project, module.clone(), scope.clone(), loc, x, "+=")
        }
        solidity::Expression::PostDecrement(loc, x) => {
            translate_post_operator_expression(project, module.clone(), scope.clone(), loc, x, "-=")
        }

        _ => translate_expression(project, module.clone(), scope.clone(), expression),
    }
}

#[inline]
pub fn translate_pre_operator_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    loc: &solidity::Loc,
    x: &solidity::Expression,
    operator: &str,
) -> Result<sway::Expression, Error> {
    let assignment = sway::Statement::from(translate_assignment_expression(
        project,
        module.clone(),
        scope.clone(),
        operator,
        x,
        &solidity::Expression::NumberLiteral(*loc, "1".into(), String::new(), None),
    )?);

    let Some(ir::VariableAccess { variable, expression }) =
        translate_variable_access_expression(project, module.clone(), scope.clone(), x)?
    else {
        panic!("Variable not found: {x}");
    };

    if let Some(variable) = variable {
        let mut variable = variable.borrow_mut();
        variable.read_count += 1;
    }

    Ok(sway::Expression::from(sway::Block {
        statements: vec![assignment],
        final_expr: Some(expression),
    }))
}

#[inline]
pub fn translate_post_operator_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    loc: &solidity::Loc,
    x: &solidity::Expression,
    operator: &str,
) -> Result<sway::Expression, Error> {
    let assignment = sway::Statement::from(translate_assignment_expression(
        project,
        module.clone(),
        scope.clone(),
        operator,
        x,
        &solidity::Expression::NumberLiteral(*loc, "1".into(), String::new(), None),
    )?);

    let Some(ir::VariableAccess { variable, expression }) =
        translate_variable_access_expression(project, module.clone(), scope.clone(), x)?
    else {
        panic!("Variable not found: {x}");
    };

    if let Some(variable) = variable.as_ref() {
        variable.borrow_mut().read_count += 1;
    }

    let variable_name = scope.borrow_mut().generate_unique_variable_name(
        match variable {
            Some(variable) => variable.borrow().new_name.clone(),
            None => "x".into(),
        }
        .as_str(),
    );

    Ok(sway::Expression::from(sway::Block {
        statements: vec![
            sway::Statement::from(sway::Let {
                pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                    is_mutable: false,
                    name: variable_name.clone(),
                }),
                type_name: None,
                value: expression,
            }),
            assignment,
        ],
        final_expr: Some(sway::Expression::create_identifier(variable_name.as_str())),
    }))
}
