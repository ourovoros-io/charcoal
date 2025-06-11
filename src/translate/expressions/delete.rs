use crate::{error::Error, project::Project, sway, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_delete_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    let Some(ir::VariableAccess {
        variable,
        expression: expr,
    }) = translate_variable_access_expression(project, module.clone(), scope.clone(), expression)?
    else {
        panic!("Variable not found: {}", sway::TabbedDisplayer(&expression));
    };

    let type_name = match variable.as_ref() {
        Some(variable) => variable.borrow().type_name.clone(),
        None => module
            .borrow_mut()
            .get_expression_type(project, scope.clone(), &expr)?,
    };

    if let solidity::Expression::ArraySubscript(_, expression, index) = expression {
        if let Some(storage_type_name) = type_name.storage_key_type() {
            if let sway::TypeName::Identifier {
                name,
                generic_parameters,
            } = storage_type_name
            {
                if let ("StorageMap" | "StorageVec", Some(_)) =
                    (name.as_str(), generic_parameters.as_ref())
                {
                    let mut expression =
                        translate_expression(project, module.clone(), scope.clone(), expression)?;

                    let index = translate_expression(
                        project,
                        module.clone(),
                        scope.clone(),
                        index.as_ref().unwrap(),
                    )?;

                    // HACK: remove `.read()`
                    if let sway::Expression::FunctionCall(f) = &expression {
                        if let sway::Expression::MemberAccess(m) = &f.function {
                            if m.member == "read" && f.parameters.is_empty() {
                                expression = m.expression.clone();
                            }
                        }
                    }

                    return Ok(sway::Expression::create_function_calls(
                        Some(expression),
                        &[("remove", Some((None, vec![index])))],
                    ));
                }
            }
        }
    }

    let value = create_value_expression(project, module.clone(), scope.clone(), &type_name, None);

    create_assignment_expression(
        project, module, scope, "=", &expr, variable, &value, &type_name,
    )
}
