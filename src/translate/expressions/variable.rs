use std::{cell::RefCell, rc::Rc};
use convert_case::Case;
use solang_parser::{helpers::CodeLocation, pt as solidity};

use crate::{errors::Error, project::Project, sway, translate::{TranslatedDefinition, TranslatedVariable, TranslationScope}};

use super::translate_expression;

#[inline]
pub fn translate_variable_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    //
    // NOTE:
    // Variable expressions should only ever be encountered for reading the value.
    // Writes are handled when translating assignment expressions.
    //

    let solidity::Expression::Variable(variable) = expression else {
        panic!("this should only be used with variable expressions")
    };

    // Check for built-in variables
    match variable.name.as_str() {
        "now" => {
            // now => std::block::timestamp().as_u256()
            return Ok(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::from(sway::MemberAccess {
                    expression: sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("std::block::timestamp".into()),
                        generic_parameters: None,
                        parameters: vec![],
                    }),
                    member: "as_u256".into(),
                }),
                generic_parameters: None,
                parameters: vec![],
            }));
        }

        _ => {}
    }

    let Ok((Some(variable), expression)) = translate_variable_access_expression(project, translated_definition, scope.clone(), expression) else {
        panic!(
            "{}ERROR: Variable not found in scope: \"{}\"",
            match project.loc_to_line_and_column(&translated_definition.path, &expression.loc()) {
                Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                None => format!("{} - ", translated_definition.path.to_string_lossy()),
            },
            sway::TabbedDisplayer(&expression),
        );
    };

    let mut variable = variable.borrow_mut();

    variable.read_count += 1;
    
    if variable.is_storage {
        match &variable.type_name {
            sway::TypeName::Identifier { name, .. } if name == "StorageString" => {
                Ok(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::from(sway::MemberAccess {
                        expression: sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::from(sway::MemberAccess {
                                expression,
                                member: "read_slice".into(),
                            }),
                            generic_parameters: None,
                            parameters: vec![],
                        }),
                        member: "unwrap".into(),
                    }),
                    generic_parameters: None,
                    parameters: vec![],
                }))
            }

            _ => Ok(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::from(sway::MemberAccess {
                    expression,
                    member: "read".into(),
                }),
                generic_parameters: None,
                parameters: vec![],
            }))
        }
    } else {
        Ok(expression)
    }
}

pub fn translate_variable_access_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    solidity_expression: &solidity::Expression,
) -> Result<(Option<Rc<RefCell<TranslatedVariable>>>, sway::Expression), Error> {
    // println!(
    //     "{}Translating variable access expression: {solidity_expression}",
    //     match project.loc_to_line_and_column(&translated_definition.path, &solidity_expression.loc()) {
    //         Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
    //         None => format!("{} - ", translated_definition.path.to_string_lossy()),
    //     }
    // );

    match solidity_expression {
        solidity::Expression::Variable(solidity::Identifier { name, .. }) => {
            let Some(variable) = scope.borrow().get_variable_from_old_name(name) else {
                return Err(Error::Wrapped(Box::new(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!(
                        "{}error: Variable not found in scope: \"{name}\" - {solidity_expression}",
                        match project.loc_to_line_and_column(&translated_definition.path, &solidity_expression.loc()) {
                            Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                            None => format!("{} - ", translated_definition.path.to_string_lossy()),
                        }
                    ),
                ))));
            };

            let variable_name = variable.borrow().new_name.clone();
            let is_storage = variable.borrow().is_storage;

            Ok((
                Some(variable),
                if is_storage {
                    sway::Expression::from(sway::MemberAccess {
                        expression: sway::Expression::Identifier("storage".into()),
                        member: variable_name,
                    })
                } else {
                    sway::Expression::Identifier(variable_name)
                }
            ))
        }

        solidity::Expression::ArraySubscript(_, array_expression, Some(index)) => {
            let index = translate_expression(project, translated_definition, scope.clone(), index.as_ref())?;
            
            let (variable, expression) = translate_variable_access_expression(project, translated_definition, scope.clone(), array_expression)?;

            if variable.is_none() {
                return Ok((None, sway::Expression::from(sway::ArrayAccess {
                    expression,
                    index,
                })));
            }

            let variable = variable.unwrap();
            let type_name = translated_definition.get_expression_type(scope.clone(), &expression)?;
            let is_storage = variable.borrow().is_storage;

            Ok((
                Some(variable),
                match type_name {
                    sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
                        ("Bytes", None) => sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::from(sway::MemberAccess {
                                expression: sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::from(sway::MemberAccess {
                                        expression,
                                        member: "get".into(),
                                    }),
                                    generic_parameters: None,
                                    parameters: vec![index,
                                    ],
                                }),
                                member: "unwrap".into(),
                            }),
                            generic_parameters: None,
                            parameters: vec![],
                        }),

                        ("StorageKey", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => {
                            match &generic_parameters.entries[0].type_name {
                                sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
                                    ("StorageMap", Some(_)) => sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::from(sway::MemberAccess {
                                            expression,
                                            member: "get".into(),
                                        }),
                                        generic_parameters: None,
                                        parameters: vec![index],
                                    }),
            
                                    ("StorageVec", Some(_)) => sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::from(sway::MemberAccess {
                                            expression: sway::Expression::from(sway::FunctionCall {
                                                function: sway::Expression::from(sway::MemberAccess {
                                                    expression,
                                                    member: "get".into(),
                                                }),
                                                generic_parameters: None,
                                                parameters: vec![index],
                                            }),
                                            member: "unwrap".into(),
                                        }),
                                        generic_parameters: None,
                                        parameters: vec![],
                                    }),
            
                                    (name, _) => todo!(
                                        "{}TODO: translate {name} array subscript expression: {solidity_expression} - {} {expression:#?}",
                                        match project.loc_to_line_and_column(&translated_definition.path, &solidity_expression.loc()) {
                                            Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                                            None => format!("{} - ", translated_definition.path.to_string_lossy()),
                                        },
                                        sway::TabbedDisplayer(&expression),
                                    ),
                                }
                                
                                _ => todo!(
                                    "{}TODO: translate {name} array subscript expression: {solidity_expression} - {} {expression:#?}",
                                    match project.loc_to_line_and_column(&translated_definition.path, &solidity_expression.loc()) {
                                        Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                                        None => format!("{} - ", translated_definition.path.to_string_lossy()),
                                    },
                                    sway::TabbedDisplayer(&expression),
                                ),
                            }
                        }

                        ("Vec", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => {
                            sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::from(sway::MemberAccess {
                                    expression: sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::from(sway::MemberAccess {
                                            expression,
                                            member: "get".into(),
                                        }),
                                        generic_parameters: None,
                                        parameters: vec![index,
                                        ],
                                    }),
                                    member: "unwrap".into(),
                                }),
                                generic_parameters: None,
                                parameters: vec![],
                            })
                        }

                        (name, _) => todo!(
                            "{}TODO: translate {name} array subscript expression: {solidity_expression} - {} {expression:#?}",
                            match project.loc_to_line_and_column(&translated_definition.path, &solidity_expression.loc()) {
                                Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                                None => format!("{} - ", translated_definition.path.to_string_lossy()),
                            },
                            sway::TabbedDisplayer(&expression),
                        ),
                    }

                    _ => if is_storage {
                        sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::from(sway::MemberAccess {
                                expression,
                                member: "get".into(),
                            }),
                            generic_parameters: None,
                            parameters: vec![index],
                        })
                    } else {
                        sway::Expression::from(sway::ArrayAccess {
                            expression,
                            index,
                        })
                    },
                }
            ))
        }
        
        solidity::Expression::MemberAccess(_, container, member) => {
            if let solidity::Expression::Variable(solidity::Identifier { name, .. }) = container.as_ref() {
                for external_definition in project.translated_definitions.iter() {
                    if !matches!(external_definition.kind, Some(solidity::ContractTy::Library(_))) {
                        continue;
                    }

                    if external_definition.name != *name {
                        continue;
                    }

                    let member_name = crate::translate_naming_convention(&member.name, Case::Snake);
                    let new_name = format!("{}_{}", crate::translate_naming_convention(&external_definition.name, Case::Snake), member_name);
                    
                    if translated_definition.toplevel_scope.borrow().find_function(|f| f.borrow().new_name == new_name).is_none() {
                        // Get the scope entry for the library function
                        let Some(scope_entry) = external_definition.toplevel_scope.borrow().find_function(|f| f.borrow().new_name == new_name) else {
                            panic!("Failed to find function in scope: \"{}\"", new_name);
                        };

                        // Add the function to the current definition's toplevel scope
                        if !translated_definition.toplevel_scope.borrow().functions.iter().any(|f| {
                            f.borrow().old_name == scope_entry.borrow().old_name
                            && f.borrow().parameters == scope_entry.borrow().parameters
                            && f.borrow().return_type == scope_entry.borrow().return_type
                        }) {
                            translated_definition.toplevel_scope.borrow_mut().functions.push(Rc::new(RefCell::new(scope_entry.borrow().clone())));
                        }

                        // Add the function name to the current definition's function name list
                        *translated_definition.function_name_counts.entry(new_name.clone()).or_insert(0) += 1;

                        let function = external_definition.functions.iter().find(|f| f.name == new_name).unwrap();

                        // Add the function definition to the current definition
                        if !translated_definition.functions.contains(function) {
                            translated_definition.functions.push(function.clone());
                        }

                        // Add the function call count from the library definition to the current definition
                        translated_definition.function_call_counts.insert(
                            function.name.clone(),
                            if let Some(function_call_count) = external_definition.function_call_counts.get(&function.name) {
                                *function_call_count
                            } else {
                                0
                            }
                        );

                        // Add the functions called from the library definition to the current definition
                        for (lib_calling_fn, lib_called_fns) in external_definition.functions_called.iter() {
                            let called_functions = translated_definition.functions_called.entry(lib_calling_fn.clone()).or_default();

                            for lib_called_fn in lib_called_fns.iter() {
                                if !called_functions.contains(lib_called_fn) {
                                    called_functions.push(lib_called_fn.clone());
                                }
                            }
                        }
                    }

                    return Ok((None, sway::Expression::Identifier(new_name)));
                }
            }
            
            let mut translated_container = translate_expression(project, translated_definition, scope.clone(), container)?;
        
            let mut container_type_name = translated_definition.get_expression_type(scope.clone(), &translated_container)?;
            let mut container_type_name_string = container_type_name.to_string();

            // HACK: tack `.read()` onto the end if the container is a StorageKey
            if let Some(storage_key_type) = container_type_name.storage_key_type() {
                translated_container = sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::from(sway::MemberAccess {
                        expression: translated_container,
                        member: "read".into(),
                    }),
                    generic_parameters: None,
                    parameters: vec![],
                });

                container_type_name = storage_key_type;
                container_type_name_string = container_type_name.to_string();
            }

            let (variable, _) = translate_variable_access_expression(project, translated_definition, scope.clone(), container)?;
        
            // Check if container is a struct
            if let Some(struct_definition) = translated_definition.structs.iter().find(|s| s.name == container_type_name_string) {
                let field_name = crate::translate_naming_convention(member.name.as_str(), Case::Snake);
        
                if struct_definition.fields.iter().any(|f| f.name == field_name) {
                    return Ok((
                        variable,
                        sway::Expression::from(sway::MemberAccess {
                            expression: translated_container,
                            member: field_name,
                        })
                    ))
                }
            }
        
            todo!(
                "{}TODO: translate variable {container_type_name_string} member access expression: {solidity_expression} - {solidity_expression:#?}",
                match project.loc_to_line_and_column(&translated_definition.path, &solidity_expression.loc()) {
                    Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                    None => format!("{} - ", translated_definition.path.to_string_lossy()),
                },
            )
        }

        solidity::Expression::FunctionCall(_, function, arguments) => {
            let arguments = arguments.iter()
                .map(|a| translate_expression(project, translated_definition, scope.clone(), a))
                .collect::<Result<Vec<_>, _>>()?;

            match translate_variable_access_expression(project, translated_definition, scope.clone(), function) {
                Ok((variable, expression)) => Ok((
                    variable,
                    sway::Expression::from(sway::FunctionCall {
                        function: expression,
                        generic_parameters: None,
                        parameters: arguments,
                    })
                )),

                Err(_) => {
                    Ok((None, translate_expression(project, translated_definition, scope, solidity_expression)?))
                }
            }
        }

        solidity::Expression::Type(_, _) => Err(Error::Wrapped(Box::new(
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!("type expression as variable access expression: {solidity_expression} - {solidity_expression:#?}")
            )
        ))),

        _ => todo!("translate variable access expression: {solidity_expression} - {solidity_expression:#?}"),
    }
}

