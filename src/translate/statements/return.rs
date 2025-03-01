use std::{cell::RefCell, rc::Rc};
use solang_parser::pt as solidity;
use crate::{errors::Error, project::Project, sway, translate::{translate_expression, TranslatedDefinition, TranslationScope}};

#[inline]
pub fn translate_return_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
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
    
    let return_type = return_type.unwrap();

    let expression = translate_expression(project, translated_definition, scope.clone(), expression)?;
    
    Ok(sway::Statement::from(sway::Expression::Return(Some(Box::new(
        modify_return_expression(translated_definition, scope.clone(), return_type.as_ref(), &expression)
    )))))
}

fn modify_return_expression(
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    type_name: &sway::TypeName,
    expression: &sway::Expression,
) -> sway::Expression {
    let value_type = translated_definition.get_expression_type(scope.clone(), expression).unwrap();
    
    match type_name {
        sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
            ("String", None) => match value_type {
                sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
                    ("todo!", None) => expression.clone(),
                    ("String", None) => expression.clone(),
                    _ => todo!(),
                }
                
                // String::from_ascii_str(x)
                sway::TypeName::StringSlice => sway::Expression::from(sway::FunctionCall{
                    function: sway::Expression::Identifier("String::from_ascii_str".into()),
                    generic_parameters: None,
                    parameters: vec![
                        expression.clone(),
                    ],
                }),
                
                // String::from_ascii_str(from_str_array(x))
                sway::TypeName::StringArray { .. } => sway::Expression::from(sway::FunctionCall{
                    function: sway::Expression::Identifier("String::from_ascii_str".into()),
                    generic_parameters: None,
                    parameters: vec![
                        sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier("from_str_array".into()),
                            generic_parameters: None,
                            parameters: vec![
                                expression.clone(),
                            ],
                        }),
                    ],
                }),
                
                _ => todo!("{}", sway::TabbedDisplayer(&value_type)),
            }
            
            _ => expression.clone()
        }
        
        sway::TypeName::Tuple { type_names } => match value_type {
            sway::TypeName::Tuple { .. } => {
                if let sway::Expression::Tuple(values) = expression {
                    sway::Expression::Tuple(type_names.iter().zip(values.iter()).map(|v| {
                        modify_return_expression(translated_definition, scope.clone(), v.0, v.1)
                    }).collect())
                } else {
                    expression.clone()
                }
            }

            _ => todo!()
        }

        _ => expression.clone()
    }
}
