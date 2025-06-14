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
    // TODO
    // let current_function_name = module.current_functions.last().unwrap();
    // let function = scope
    //     .borrow()
    //     .find_function(|f| f.borrow().new_name == *current_function_name)
    //     .unwrap();

    // let sway::TypeName::Function { return_type, .. } = function.borrow().type_name.clone() else {
    //     panic!(
    //         "Invalid function type name: {:#?}",
    //         function.borrow().type_name
    //     )
    // };

    let mut expression = translate_expression(project, module.clone(), scope.clone(), expression)?;
    let mut expression_type =
        get_expression_type(project, module.clone(), scope.clone(), &expression)?;

    // HACK: remove `.read()` is underlying expression type is StorageVec or StorageMap
    if let sway::Expression::FunctionCall(f) = &expression {
        if let sway::Expression::MemberAccess(m) = &f.function {
            if m.member == "read" && f.parameters.is_empty() {
                if expression_type.is_storage_map() || expression_type.is_storage_vec() {
                    expression = m.expression.clone();
                    expression_type = sway::TypeName::Identifier {
                        name: "StorageKey".to_string(),
                        generic_parameters: Some(sway::GenericParameterList {
                            entries: vec![sway::GenericParameter {
                                type_name: expression_type,
                                implements: None,
                            }],
                        }),
                    }
                }
            }
        }
    }
    // TODO
    // if return_type.is_none() {
    //     return Ok(sway::Statement::from(expression));
    // }

    // let return_type = return_type.unwrap();

    // expression = coerce_expression(&expression, &expression_type, &return_type).unwrap();

    Ok(sway::Statement::from(sway::Expression::Return(Some(
        Box::new(expression),
    ))))
}
