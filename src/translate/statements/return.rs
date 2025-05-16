use crate::{errors::Error, project::Project, sway, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_return_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &Rc<RefCell<TranslationScope>>,
    expression: &Option<solidity::Expression>,
) -> Result<sway::Statement, Error> {
    let Some(expression) = expression else {
        return Ok(sway::Statement::from(sway::Expression::Return(None)));
    };

    let current_function_name = translated_definition.current_functions.last().unwrap();
    let function = scope.borrow().find_function(|f| f.borrow().new_name == *current_function_name).unwrap();

    let sway::TypeName::Function { return_type, .. } = function.borrow().type_name.clone() else {
        panic!("Invalid function type name: {:#?}", function.borrow().type_name)
    };

    let mut expression = translate_expression(project, translated_definition, scope, expression)?;
    let mut expression_type = translated_definition.get_expression_type(scope, &expression)?;
    
    // HACK: remove `.read()` is underlying expression type is StorageVec or StorageMap
    if let sway::Expression::FunctionCall(f) = &expression {
        if let sway::Expression::MemberAccess(m) = &f.function {
            if m.member == "read" && f.parameters.is_empty() {
                if expression_type.is_storage_map() || expression_type.is_storage_vec() {
                    expression = m.expression.clone();
                    expression_type = sway::TypeName::Identifier { 
                        name: "StorageKey".to_string(),
                        generic_parameters: Some(sway::GenericParameterList { 
                            entries: vec![
                                sway::GenericParameter { 
                                    type_name: expression_type,
                                    implements: None 
                                }
                            ] 
                        })
                    }
                }
            }
        }
    }

    if return_type.is_none() {
        return Ok(sway::Statement::from(expression));
    }
    
    let return_type = return_type.unwrap();

    expression = coerce_expression(&expression, &expression_type, &return_type).unwrap();

    Ok(sway::Statement::from(sway::Expression::Return(Some(Box::new(expression)))))
}
