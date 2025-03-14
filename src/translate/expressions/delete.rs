use std::{cell::RefCell, rc::Rc};
use solang_parser::pt as solidity;
use crate::{errors::Error, project::Project, sway, translate::{TranslatedDefinition, TranslationScope}};
use super::{assignment::create_assignment_expression, create_value_expression, translate_expression, variable::translate_variable_access_expression};

#[inline]
pub fn translate_delete_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    let (variable, expr) = translate_variable_access_expression(project, translated_definition, scope.clone(), expression)?;
    
    if variable.is_none() {
        panic!("Variable not found: {}", sway::TabbedDisplayer(&expression));
    }

    let variable = variable.unwrap();
    let type_name = variable.borrow().type_name.clone();
    
    if let solidity::Expression::ArraySubscript(_, expression, index) = expression {
        if let Some(storage_type_name) = type_name.storage_key_type() {
            if let sway::TypeName::Identifier { name, generic_parameters } = storage_type_name {
                if let ("StorageMap" | "StorageVec", Some(_)) = (name.as_str(), generic_parameters.as_ref()) {
                    let expression = translate_expression(project, translated_definition, scope.clone(), expression)?;
                    let index = translate_expression(project, translated_definition, scope, index.as_ref().unwrap())?;
                    
                    return Ok(sway::Expression::create_function_calls(Some(expression), &[("remove", Some((None, vec![index])))]));
                }
            }
        }
    }

    let value = create_value_expression(translated_definition, scope.clone(), &type_name, None);
    create_assignment_expression(project, translated_definition, scope.clone(), "=", &expr, variable, &value, &type_name)
}
