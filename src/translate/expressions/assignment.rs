use crate::{error::Error, project::Project, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_assignment_expression(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: &Rc<RefCell<TranslationScope>>,
    operator: &str,
    lhs: &solidity::Expression,
    rhs: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    // use solang_parser::helpers::CodeLocation;
    // println!(
    //     "Translating assignment expression: {lhs} {operator} {rhs}; from {}",
    //     match project.loc_to_line_and_column(&module.path, &lhs.loc()) {
    //         Some((line, col)) => format!("{}:{}:{}: ", module.path.to_string_lossy(), line, col),
    //         None => format!("{}: ", module.path.to_string_lossy()),
    //     },
    // );

    let rhs = match operator {
        "=" => {
            // println!("taking translate_pre_or_post_operator_value_expression path...");
            translate_pre_or_post_operator_value_expression(project, module.clone(), scope, rhs)?
        }
        _ => {
            // println!("taking translate_expression path...");
            translate_expression(project, module.clone(), scope, rhs)?
        }
    };

    let rhs_type_name = module.borrow_mut().get_expression_type(scope, &rhs)?;

    let (variable, expression) = translate_variable_access_expression(project, module.clone(), scope, lhs)?;

    // HACK: struct field lookup
    if variable.is_none() {
        if let sway::Expression::MemberAccess(member_access) = &expression {
            if let sway::TypeName::Identifier { name, .. } = module
                .borrow_mut()
                .get_expression_type(scope, &member_access.expression)?
            {
                if let Some(struct_definition) = module
                    .borrow()
                    .structs
                    .iter()
                    .find(|s| s.borrow().name == name)
                {
                    if struct_definition
                        .borrow()
                        .fields
                        .iter()
                        .any(|f| f.name == member_access.member)
                    {
                        return Ok(expression);
                    }
                }
            }
        }

        panic!("Variable not found: {}", sway::TabbedDisplayer(&expression));
    }

    let variable = variable.unwrap();

    create_assignment_expression(
        project,
        module.clone(),
        scope,
        operator,
        &expression,
        &variable,
        &rhs,
        &rhs_type_name,
    )
}

#[inline]
pub fn create_assignment_expression(
    _project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: &Rc<RefCell<TranslationScope>>,
    operator: &str,
    expression: &sway::Expression,
    variable: &Rc<RefCell<TranslatedVariable>>,
    rhs: &sway::Expression,
    rhs_type_name: &sway::TypeName,
) -> Result<sway::Expression, Error> {
    // Generate a unique name for our variable
    let variable_name = scope.borrow_mut().generate_unique_variable_name("x");

    variable.borrow_mut().mutation_count += 1;

    let type_name = variable.borrow().type_name.clone();

    let expr_type_name = module.borrow_mut().get_expression_type(scope, expression)?;
    
    let (is_storage, namespace_name) = match variable.borrow().storage_namespace.as_ref() {
        Some(namespace_name) => (true, namespace_name.clone()),
        None => (false, String::new()),
    };

    // Check for assignments to fields of struct variables defined in scope
    if !type_name.is_compatible_with(&expr_type_name) {
        if let sway::Expression::MemberAccess(member_access) = expression {
            if member_access.expression != sway::Expression::create_identifier(format!("storage::{namespace_name}")) {
                let type_name = module.borrow_mut().get_expression_type(scope, &member_access.expression)?;

                if let Some(struct_definition) = module.borrow().structs.iter().find(|s| {
                    let s = s.borrow();
        
                    let sway::TypeName::Identifier { name, generic_parameters } = &type_name else {
                        return false;
                    };
        
                    if s.name != *name || s.generic_parameters.is_some() != generic_parameters.is_some() {
                        return false;
                    }
        
                    if let (Some(lhs_generic_parameters), Some(rhs_generic_parameters)) = (s.generic_parameters.as_ref(), generic_parameters.as_ref()) {
                        if lhs_generic_parameters.entries.len() != rhs_generic_parameters.entries.len() {
                            return false;
                        }
                    }

                    if !s.fields.iter().any(|f| f.name == member_access.member) {
                        return false;
                    }
        
                    true
                }) {
                    let struct_definition = struct_definition.borrow();
                    let field = struct_definition.fields.iter().find(|f| f.name == member_access.member).unwrap();

                    let mut expression = member_access.expression.clone();

                    if is_storage {
                        // Remove the `.read()` from the end of the expression
                        if let sway::Expression::FunctionCall(function_call) = &member_access.expression {
                            if let sway::Expression::MemberAccess(member_access) = &function_call.function {
                                if member_access.member == "read" {
                                    expression = member_access.expression.clone();
                                }
                            }
                        };

                        // Create a local variable to store the local mutable copy of the struct
                        let variable_name = scope.borrow_mut().generate_unique_variable_name("x");

                        //
                        // TODO:
                        // We should really assign to a mutable local variable copy of the struct if it's in storage,
                        // but for now we're just gonna write one in place at a time...
                        //

                        // Create a block that:
                        // 1. Reads the struct from storage into a local mutable variable
                        // 2. Updates the field of the local copy of the struct
                        // 3. Writes the local copy of the struct back to storage
                        return Ok(sway::Expression::from(sway::Block {
                            statements: vec![
                                // let x = blah.read();
                                sway::Statement::from(sway::Let {
                                    pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                                        is_mutable: true,
                                        name: variable_name.clone(),
                                    }),
                                    type_name: None,
                                    value: sway::Expression::create_function_calls(Some(expression.clone()), &[
                                        ("read", Some((None, vec![]))),
                                    ]),
                                }),
                                // x.field = value;
                                sway::Statement::from(sway::Expression::from(sway::BinaryExpression {
                                    operator: "=".into(),
                                    lhs: sway::Expression::from(sway::MemberAccess {
                                        expression: sway::Expression::create_identifier(variable_name.clone()),
                                        member: field.name.clone(),
                                    }),
                                    rhs: coerce_expression(rhs, rhs_type_name, &field.type_name).unwrap(),
                                })),
                                // blah.write(x);
                                sway::Statement::from(sway::Expression::create_function_calls(Some(expression.clone()), &[
                                    ("write", Some((None, vec![
                                        sway::Expression::create_identifier(variable_name),
                                    ]))),
                                ])),
                            ],
                            final_expr: None,
                        }));
                    }
                    
                    // Non-storage struct field assignment
                    //   blah.field = value;
                    return Ok(sway::Expression::from(sway::BinaryExpression {
                        operator: "=".into(),
                        lhs: sway::Expression::from(sway::MemberAccess {
                            expression: member_access.expression.clone(),
                            member: field.name.clone(),
                        }),
                        rhs: coerce_expression(rhs, rhs_type_name, &field.type_name).unwrap(),
                    }));
                }
            }
        }
    }

    // Handle assignments to storage
    if is_storage {
        let Some(storage_key_type) = expr_type_name.storage_key_type() else {
            todo!(
                "check to see if {type_name} `{}` expression is a struct field assignment ({expr_type_name})",
                sway::TabbedDisplayer(expression),
            )
        };

        let member = match (&type_name, &rhs_type_name) {
            (
                sway::TypeName::Identifier { name: lhs_name, generic_parameters },
                sway::TypeName::StringSlice
            ) => match (lhs_name.as_str(), generic_parameters.as_ref()) {
                ("StorageKey", Some(generic_parameters))
                if generic_parameters.entries.len() == 1 
                && generic_parameters.entries[0].type_name.is_storage_string() => {
                   "write_slice"
                }

                _ => "write",
            }

            _ => "write",
        };

        return Ok(sway::Expression::create_function_calls(Some(expression.clone()), &[
            (member, Some((None, vec![
                match operator {
                    "=" => match (&type_name, &rhs_type_name) {
                        (
                            sway::TypeName::Identifier { name: lhs_name, generic_parameters },
                            sway::TypeName::StringSlice
                        ) => match (lhs_name.as_str(), generic_parameters.as_ref()) {
                            ("StorageKey", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => {
                                match &generic_parameters.entries[0].type_name {
                                    sway::TypeName::Identifier { name, generic_parameters } => {
                                        match (name.as_str(), generic_parameters.as_ref()) {
                                            ("StorageString", None) => {
                                                // Ensure `std::string::*` is imported
                                                module.borrow_mut().ensure_use_declared("std::string::*");
                                                
                                                sway::Expression::create_function_calls(None, &[
                                                    ("String::from_ascii_str", Some((None, vec![rhs.clone()]))),
                                                ])
                                            }

                                            _ => coerce_expression(rhs, rhs_type_name, &storage_key_type).unwrap()
                                        }
                                    }

                                    _ => coerce_expression(rhs, rhs_type_name, &storage_key_type).unwrap()
                                }
                            }

                            _ => coerce_expression(rhs, rhs_type_name, &storage_key_type).unwrap()
                        }

                        _ => {
                            coerce_expression(rhs, rhs_type_name, &storage_key_type).unwrap()
                        }
                    },

                    _ => {
                        variable.borrow_mut().read_count += 1;
                        
                        sway::Expression::from(sway::BinaryExpression {
                            operator: operator.trim_end_matches('=').into(),
                            lhs: sway::Expression::create_function_calls(Some(expression.clone()), &[
                                ("read", Some((None, vec![]))),
                            ]),
                            rhs: coerce_expression(rhs, rhs_type_name, &storage_key_type).unwrap(),
                        })
                    }
                },
            ]))),
        ]));
    }

    // Handle vector assignments
    if let sway::TypeName::Identifier { name, generic_parameters } = &type_name {
        match (name.as_str(), generic_parameters.as_ref()) {
            ("Vec", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => {
                match &expression {
                    sway::Expression::ArrayAccess(array_access) => {
                        let index_type = module.borrow_mut().get_expression_type(scope, &array_access.index)?;
                        let u64_type = sway::TypeName::Identifier { name: "u64".into(), generic_parameters: None };
                        let index = coerce_expression(&array_access.index, &index_type, &u64_type).unwrap();
                        
                        let rhs_type = module.borrow_mut().get_expression_type(scope, rhs)?;
                        let rhs = coerce_expression(rhs, &rhs_type, &generic_parameters.entries[0].type_name).unwrap();

                        return Ok(sway::Expression::create_function_calls(
                            Some(array_access.expression.clone()), &[
                                ("set", Some((None, vec![
                                    index.clone(),
                                    match operator {
                                        "=" => rhs.clone(),
                                        
                                        _ => {
                                            variable.borrow_mut().read_count += 1;
                                            
                                            sway::Expression::from(sway::BinaryExpression {
                                                operator: operator.trim_end_matches('=').into(),
                
                                                lhs: sway::Expression::create_function_calls(
                                                    Some(array_access.expression.clone()), &[
                                                        ("get", Some((None, vec![index]))),
                                                        ("unwrap", Some((None, vec![]))),
                                                    ]),
                
                                                rhs: rhs.clone(),
                                            })
                                        }
                                    }
                                ]))),
                            ],
                        ));
                    }
        
                    sway::Expression::MemberAccess(member_access) => match &member_access.expression {
                        sway::Expression::ArrayAccess(array_access) => {
                            // x[i].member = value => {
                            //     let mut x = expr.get(i).unwrap();
                            //     x.member = value;
                            //     expr.set(i, a);
                            // }
        
                            return Ok(sway::Expression::from(sway::Block {
                                statements: vec![
                                    // let mut x = expr.get(i).unwrap();
                                    sway::Statement::from(sway::Let {
                                        pattern: sway::LetPattern::from(sway::LetIdentifier {
                                            is_mutable: true,
                                            name: variable_name.clone(),
                                        }),
                                        type_name: None,
                                        value: sway::Expression::create_function_calls(
                                            Some(array_access.expression.clone()), &[
                                                ("get", Some((None, vec![array_access.index.clone()]))),
                                                ("unwrap", Some((None, vec![]))),
                                            ],
                                        ),
                                    }),
                                    // x.member = value;
                                    sway::Statement::from(sway::Expression::from(sway::BinaryExpression {
                                        operator: "=".into(),
                                        lhs: sway::Expression::from(sway::MemberAccess {
                                            expression: sway::Expression::create_identifier(variable_name.clone()),
                                            member: member_access.member.clone(),
                                        }),
                                        rhs: rhs.clone(),
                                    })),
                                    // expr.set(i, a);
                                    sway::Statement::from(sway::Expression::create_function_calls(
                                        Some(array_access.expression.clone()), &[
                                            ("set", Some((None, vec![
                                                sway::Expression::create_identifier(variable_name.clone()),
                                            ]))),
                                        ],
                                    )),
                                ],
                                final_expr: None,
                            }));
                        }
        
                        _ => todo!("translation assignment expression: {}", sway::TabbedDisplayer(expression)),
                    }
                    
                    sway::Expression::PathExpr(path_expr) => {
                        let Some(ident) = path_expr.as_identifier() else {
                            todo!("translation non-identifier assignment expression: {}", sway::TabbedDisplayer(expression))
                        };

                        return Ok(sway::Expression::from(sway::BinaryExpression {
                            operator: "=".into(),
                            lhs: expression.clone(),
                            rhs: sway::Expression::create_identifier(ident.into())
                        }));
                    }
        
                    sway::Expression::FunctionCall(function_call) => match &function_call.function {
                        sway::Expression::MemberAccess(member_access) => match member_access.member.as_str() {
                            "get" if function_call.parameters.len() == 1 => {
                                let container_type = module.borrow_mut().get_expression_type(scope, &member_access.expression)?;
        
                                todo!("translation {container_type} assignment expression: {}", sway::TabbedDisplayer(expression))
                            }
        
                            "unwrap" if function_call.parameters.is_empty() => match &member_access.expression {
                                sway::Expression::FunctionCall(function_call) => match &function_call.function {
                                    sway::Expression::MemberAccess(member_access) => match member_access.member.as_str() {
                                        "get" if function_call.parameters.len() == 1 => {
                                            let rhs_type = module.borrow_mut().get_expression_type(scope, rhs)?;
                                            let rhs = coerce_expression(rhs, &rhs_type, &generic_parameters.entries[0].type_name).unwrap();

                                            return Ok(sway::Expression::create_function_calls(
                                                Some(member_access.expression.clone()), &[
                                                    ("set", Some((None, vec![
                                                        function_call.parameters[0].clone(),
                                                        rhs.clone(),
                                                    ]))),
                                                ],
                                            ));
                                        }
        
                                        _ => todo!("translation assignment expression: {}", sway::TabbedDisplayer(expression)),
                                    }
        
                                    _ => todo!("translation assignment expression: {}", sway::TabbedDisplayer(expression)),
                                }
        
                                _ => todo!("translation assignment expression: {}", sway::TabbedDisplayer(expression)),
                            }
        
                            _ => todo!("translation assignment expression: {}", sway::TabbedDisplayer(expression)),
                        }
        
                        _ => todo!("translation assignment expression: {}", sway::TabbedDisplayer(expression)),
                    }
        
                    _ => todo!("translation assignment expression: {}", sway::TabbedDisplayer(expression)),
                }
            }

            _ => {}
        }
    }
    
    // All other assignments
    let rhs = coerce_expression(rhs, rhs_type_name, &expr_type_name).unwrap();

    match operator {
        "&=" | "|=" | "^=" => {
            //
            // NOTE:
            // Sway doesn't have these operators, so we have to implement them manually.
            //
            
            variable.borrow_mut().read_count += 1;
            
            Ok(sway::Expression::from(sway::BinaryExpression {
                operator: "=".into(),
                lhs: expression.clone(),
                rhs: sway::Expression::from(sway::BinaryExpression {
                    operator: operator.trim_end_matches('=').into(),
                    lhs: expression.clone(),
                    rhs: rhs.clone(),
                }),
            }))
        }

        _ => Ok(sway::Expression::from(sway::BinaryExpression {
            operator: operator.into(),
            lhs: expression.clone(),
            rhs: rhs.clone(),
        })),
    }
}
