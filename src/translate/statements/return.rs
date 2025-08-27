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

    // use solang_parser::helpers::CodeLocation;
    // println!(
    //     "Translating return statement at: {}",
    //     project.loc_to_file_location_string(module.clone(), &expression.loc())
    // );

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
    if let Some(container) = expression.to_read_call_parts()
        && get_expression_type(project, module.clone(), scope.clone(), container)?.is_storage_key()
    {
        expression = container.clone();
    }

    if return_type.is_none() {
        return Ok(sway::Statement::from(expression));
    }

    let return_type = return_type.unwrap();

    let expression_type = get_expression_type(project, module.clone(), scope.clone(), &expression)?;

    let Some(expression) = coerce_expression(
        project,
        module.clone(),
        scope.clone(),
        &expression,
        &expression_type,
        &return_type,
    ) else {
        panic!(
            "Failed to coerce from `{}` to `{}`: `{}`",
            expression_type,
            return_type,
            sway::TabbedDisplayer(&expression)
        );
    };

    Ok(sway::Statement::from(sway::Expression::Return(Some(
        Box::new(expression),
    ))))
}
