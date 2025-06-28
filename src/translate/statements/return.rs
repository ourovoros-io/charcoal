use crate::{error::Error, project::Project, sway, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_return_statement(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    expression: &Option<solidity::Expression>,
) -> Result<sway::Statement, Error> {
    let Some(expression) = expression else {
        return Ok(sway::Statement::from(sway::Expression::Return(None)));
    };

    let current_function_name = scope.borrow().get_function_name().unwrap();
    let function = module
        .borrow()
        .functions
        .iter()
        .find(|f| {
            let sway::TypeName::Function { new_name, .. } = &f.signature else {
                unreachable!()
            };
            *new_name == current_function_name
        })
        .cloned()
        .unwrap();

    let sway::TypeName::Function { return_type, .. } = function.signature.clone() else {
        panic!("Invalid function type name: {:#?}", function.signature)
    };

    let mut expression = translate_expression(project, module.clone(), scope.clone(), expression)?;

    // HACK: remove `.read()` if present
    if let sway::Expression::FunctionCall(f) = &expression
        && let sway::Expression::MemberAccess(m) = &f.function
        && m.member == "read"
        && f.parameters.is_empty()
    {
        let container_type =
            get_expression_type(project, module.clone(), scope.clone(), &m.expression)?;

        if container_type.is_storage_key() {
            expression = m.expression.clone();
        }
    }

    if return_type.is_none() {
        return Ok(sway::Statement::from(expression));
    }

    let return_type = return_type.unwrap();

    let expression_type = get_expression_type(project, module.clone(), scope.clone(), &expression)?;

    expression = coerce_expression(
        project,
        module.clone(),
        scope.clone(),
        &expression,
        &expression_type,
        &return_type,
    )
    .unwrap();

    Ok(sway::Statement::from(sway::Expression::Return(Some(
        Box::new(expression),
    ))))
}
