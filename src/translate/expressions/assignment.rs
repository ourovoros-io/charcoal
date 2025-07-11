use crate::{error::Error, project::Project, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_assignment_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    operator: &str,
    lhs: &solidity::Expression,
    rhs: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    // use solang_parser::helpers::CodeLocation;
    // println!(
    //     "Translating assignment expression: {lhs} {operator} {rhs}; from {}",
    //     project.loc_to_file_location_string(module.clone(), &lhs.loc()),
    // );

    let rhs = match operator {
        "=" => translate_pre_or_post_operator_value_expression(
            project,
            module.clone(),
            scope.clone(),
            rhs,
        )?,
        _ => translate_expression(project, module.clone(), scope.clone(), rhs)?,
    };

    let rhs_type_name = get_expression_type(project, module.clone(), scope.clone(), &rhs)?;

    let Some(ir::VariableAccess {
        variable,
        mut expression,
    }) = translate_variable_access_expression(project, module.clone(), scope.clone(), lhs)?
    else {
        panic!("Failed to translate variable access expression: {}", lhs)
    };

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

    let expr_type_name = get_expression_type(project, module.clone(), scope.clone(), &expression)?;

    if let Some(storage_key_type) = expr_type_name.storage_key_type() {
        let member = match &storage_key_type {
            sway::TypeName::Identifier {
                name,
                generic_parameters,
            } => match (name.as_str(), generic_parameters.as_ref()) {
                ("StorageString", None) => match &rhs_type_name {
                    sway::TypeName::Identifier {
                        name,
                        generic_parameters,
                    } => match (name.as_str(), generic_parameters.as_ref()) {
                        ("String", None) => "write_slice",

                        _ => todo!("write {rhs_type_name} to StorageString"),
                    },

                    sway::TypeName::StringSlice => "write_slice",

                    _ => todo!("write {rhs_type_name} to StorageString"),
                },

                ("StorageMap", Some(_)) => {
                    if let sway::Expression::FunctionCall(function_call) = &expression
                        && let sway::Expression::MemberAccess(member_access) =
                            &function_call.function
                        && member_access.member == "get"
                        && function_call.parameters.len() == 1
                    {
                        if let Some(function_name) = scope.borrow().get_function_name() {
                            module
                                .borrow_mut()
                                .function_storage_accesses
                                .entry(function_name)
                                .or_default()
                                .1 = true;
                        }

                        return Ok(sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::from(sway::MemberAccess {
                                expression: member_access.expression.clone(),
                                member: "insert".into(),
                            }),
                            generic_parameters: None,
                            parameters: vec![rhs],
                        }));
                    }

                    todo!()
                }

                _ => "write",
            },

            _ => "write",
        };

        let value = match operator {
            "=" => match (&storage_key_type, &rhs_type_name) {
                (
                    sway::TypeName::Identifier {
                        name: lhs_name,
                        generic_parameters,
                    },
                    sway::TypeName::Array {
                        type_name: rhs_element_type_name,
                        length: rhs_length,
                    },
                ) => match (lhs_name.as_str(), generic_parameters.as_ref()) {
                    ("StorageVec", Some(generic_parameters))
                        if generic_parameters.entries.len() == 1 =>
                    {
                        // {
                        //     let array = rhs;
                        //     let mut v = Vec::new();
                        //     let mut i = 0;
                        //     while i < length {
                        //         v.push(a[i]);
                        //         i += 1;
                        //     }
                        //     lhs.store_vec(v);
                        // }

                        let array_var_name = scope.borrow_mut().generate_unique_variable_name("a");
                        let vec_var_name = scope.borrow_mut().generate_unique_variable_name("v");
                        let i_var_name = scope.borrow_mut().generate_unique_variable_name("i");

                        if let Some(function_name) = scope.borrow().get_function_name() {
                            module
                                .borrow_mut()
                                .function_storage_accesses
                                .entry(function_name)
                                .or_default()
                                .1 = true;
                        }

                        return Ok(sway::Expression::from(sway::Block {
                            statements: vec![
                                // let a = rhs;
                                sway::Statement::from(sway::Let {
                                    pattern: sway::LetPattern::from(sway::LetIdentifier {
                                        is_mutable: false,
                                        name: array_var_name.clone(),
                                    }),
                                    type_name: None,
                                    value: rhs.clone(),
                                }),
                                // let mut v = Vec::new();
                                sway::Statement::from(sway::Let {
                                    pattern: sway::LetPattern::from(sway::LetIdentifier {
                                        is_mutable: true,
                                        name: vec_var_name.clone(),
                                    }),
                                    type_name: None,
                                    value: sway::Expression::create_function_calls(
                                        None,
                                        &[("Vec::new", Some((None, vec![])))],
                                    ),
                                }),
                                // let mut i = 0;
                                sway::Statement::from(sway::Let {
                                    pattern: sway::LetPattern::from(sway::LetIdentifier {
                                        is_mutable: true,
                                        name: i_var_name.clone(),
                                    }),
                                    type_name: None,
                                    value: sway::Expression::from(sway::Literal::DecInt(
                                        BigUint::zero(),
                                        None,
                                    )),
                                }),
                                // while i < length {
                                //     v.push(a[i]);
                                //     i += 1;
                                // }
                                sway::Statement::from(sway::Expression::from(sway::While {
                                    condition: sway::Expression::from(sway::BinaryExpression {
                                        operator: "<".into(),
                                        lhs: sway::Expression::create_identifier(i_var_name.clone()),
                                        rhs: sway::Expression::from(sway::Literal::DecInt(
                                            rhs_length.clone().into(),
                                            None,
                                        )),
                                    }),
                                    body: sway::Block {
                                        statements: vec![
                                            // v.push(a[i]);
                                            sway::Statement::from(
                                                sway::Expression::create_function_calls(
                                                    None,
                                                    &[
                                                        (vec_var_name.as_str(), None),
                                                        ("push", Some((None, vec![
                                                            coerce_expression(
                                                                project,
                                                                module.clone(),
                                                                scope.clone(),
                                                                &sway::Expression::from(sway::ArrayAccess {
                                                                    expression: sway::Expression::create_identifier(array_var_name.clone()),
                                                                    index: sway::Expression::create_identifier(i_var_name.clone()),
                                                                }),
                                                                rhs_element_type_name,
                                                                &generic_parameters.entries[0].type_name,
                                                            ).unwrap()
                                                        ]))),
                                                    ],
                                                ),
                                            ),
                                            // i += 1;
                                            sway::Statement::from(sway::Expression::from(sway::BinaryExpression {
                                                operator: "+=".into(),
                                                lhs: sway::Expression::create_identifier(i_var_name.clone()),
                                                rhs: sway::Expression::from(sway::Literal::DecInt(
                                                    BigUint::one(),
                                                    None,
                                                )),
                                            })),
                                        ],
                                        final_expr: None,
                                    },
                                })),
                                // lhs.store_vec(v);
                                sway::Statement::from(sway::Expression::create_function_calls(
                                    Some(expression.clone()),
                                    &[
                                        ("store_vec", Some((None, vec![
                                            sway::Expression::create_identifier(vec_var_name.clone()),
                                        ]))),
                                    ],
                                )),
                            ],
                            final_expr: None,
                        }));
                    }

                    _ => coerce_expression(
                        project,
                        module.clone(),
                        scope.clone(),
                        &rhs,
                        &rhs_type_name,
                        &storage_key_type,
                    )
                    .unwrap(),
                },

                (
                    sway::TypeName::Identifier {
                        name: lhs_name,
                        generic_parameters,
                    },
                    sway::TypeName::StringSlice,
                ) => match (lhs_name.as_str(), generic_parameters.as_ref()) {
                    ("StorageString", None) => {
                        // Ensure `std::string::*` is imported
                        module.borrow_mut().ensure_use_declared("std::string::*");

                        sway::Expression::create_function_calls(
                            None,
                            &[("String::from_ascii_str", Some((None, vec![rhs.clone()])))],
                        )
                    }

                    _ => coerce_expression(
                        project,
                        module.clone(),
                        scope.clone(),
                        &rhs,
                        &rhs_type_name,
                        &storage_key_type,
                    )
                    .unwrap(),
                },

                (
                    sway::TypeName::Identifier {
                        name: lhs_name,
                        generic_parameters: lhs_generic_parameters,
                    },
                    sway::TypeName::Identifier {
                        name: rhs_name,
                        generic_parameters: rhs_generic_parameters,
                    },
                ) => match (
                    (lhs_name.as_str(), lhs_generic_parameters.as_ref()),
                    (rhs_name.as_str(), rhs_generic_parameters.as_ref()),
                ) {
                    (("StorageString", None), ("String", None)) => rhs,

                    _ => coerce_expression(
                        project,
                        module.clone(),
                        scope.clone(),
                        &rhs,
                        &rhs_type_name,
                        &storage_key_type,
                    )
                    .unwrap(),
                },

                _ => coerce_expression(
                    project,
                    module.clone(),
                    scope.clone(),
                    &rhs,
                    &rhs_type_name,
                    &storage_key_type,
                )
                .unwrap(),
            },

            _ => {
                if let Some(function_name) = scope.borrow().get_function_name() {
                    module
                        .borrow_mut()
                        .function_storage_accesses
                        .entry(function_name)
                        .or_default()
                        .0 = true;
                }

                sway::Expression::from(sway::BinaryExpression {
                    operator: operator.trim_end_matches('=').into(),
                    lhs: sway::Expression::create_function_calls(
                        Some(expression.clone()),
                        &[("read", Some((None, vec![])))],
                    ),
                    rhs: coerce_expression(
                        project,
                        module.clone(),
                        scope.clone(),
                        &rhs,
                        &rhs_type_name,
                        &storage_key_type,
                    )
                    .unwrap(),
                })
            }
        };

        if let Some(function_name) = scope.borrow().get_function_name() {
            module
                .borrow_mut()
                .function_storage_accesses
                .entry(function_name)
                .or_default()
                .1 = true;
        }

        return Ok(sway::Expression::create_function_calls(
            Some(expression.clone()),
            &[(member, Some((None, vec![value])))],
        ));
    }

    create_assignment_expression(
        project,
        module.clone(),
        scope.clone(),
        operator,
        &expression,
        variable,
        &rhs,
        &rhs_type_name,
    )
}

#[inline]
pub fn create_assignment_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    operator: &str,
    expression: &sway::Expression,
    variable: Option<Rc<RefCell<ir::Variable>>>,
    rhs: &sway::Expression,
    rhs_type_name: &sway::TypeName,
) -> Result<sway::Expression, Error> {
    // Generate a unique name for our variable
    let variable_name = scope.borrow_mut().generate_unique_variable_name("x");

    if let Some(variable) = variable.clone() {
        variable.borrow_mut().mutation_count += 1;
    }

    let type_name = match variable.clone() {
        Some(variable) => variable.borrow().type_name.clone(),
        None => get_expression_type(project, module.clone(), scope.clone(), expression)?,
    };

    let expr_type_name = get_expression_type(project, module.clone(), scope.clone(), expression)?;

    // Check for assignments to fields of struct variables defined in scope
    if !type_name.is_compatible_with(&expr_type_name)
        && let sway::Expression::MemberAccess(member_access) = expression
    {
        let type_name = get_expression_type(
            project,
            module.clone(),
            scope.clone(),
            &member_access.expression,
        )?;

        if let Some(struct_definition) = module.borrow().structs.iter().find(|s| {
            let s = s.implementation.as_ref().unwrap().borrow();

            let sway::TypeName::Identifier {
                name,
                generic_parameters,
            } = &type_name
            else {
                return false;
            };

            if s.name != *name || s.generic_parameters.is_some() != generic_parameters.is_some() {
                return false;
            }

            if let (Some(lhs_generic_parameters), Some(rhs_generic_parameters)) =
                (s.generic_parameters.as_ref(), generic_parameters.as_ref())
                && lhs_generic_parameters.entries.len() != rhs_generic_parameters.entries.len()
            {
                return false;
            }

            if !s.fields.iter().any(|f| f.new_name == member_access.member) {
                return false;
            }

            true
        }) {
            let struct_definition = struct_definition.implementation.as_ref().unwrap().borrow();

            let field = struct_definition
                .fields
                .iter()
                .find(|f| f.new_name == member_access.member)
                .unwrap();

            // Non-storage struct field assignment
            //   blah.field = value;
            return Ok(sway::Expression::from(sway::BinaryExpression {
                operator: "=".into(),
                lhs: sway::Expression::from(sway::MemberAccess {
                    expression: member_access.expression.clone(),
                    member: field.new_name.clone(),
                }),
                rhs: coerce_expression(
                    project,
                    module.clone(),
                    scope.clone(),
                    rhs,
                    rhs_type_name,
                    &field.type_name,
                )
                .unwrap(),
            }));
        }
    }

    if let sway::TypeName::Identifier {
        name,
        generic_parameters,
    } = &type_name
    {
        match (name.as_str(), generic_parameters.as_ref()) {
            ("StorageKey", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => {
                match &generic_parameters.entries[0].type_name {
                    sway::TypeName::Identifier {
                        name,
                        generic_parameters,
                    } => match (name.as_str(), generic_parameters.as_ref()) {
                        ("StorageString", None) => {
                            if let Some(function_name) = scope.borrow().get_function_name() {
                                module
                                    .borrow_mut()
                                    .function_storage_accesses
                                    .entry(function_name)
                                    .or_default()
                                    .1 = true;
                            }

                            return Ok(sway::Expression::create_function_calls(
                                Some(expression.clone()),
                                &[(
                                    "write_slice",
                                    Some((
                                        None,
                                        vec![match operator {
                                            "=" => rhs.clone(),

                                            _ => {
                                                todo!()
                                            }
                                        }],
                                    )),
                                )],
                            ));
                        }

                        _ => {}
                    },

                    _ => {}
                }
            }

            ("Vec", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => {
                match &expression {
                    sway::Expression::ArrayAccess(array_access) => {
                        let index_type = get_expression_type(
                            project,
                            module.clone(),
                            scope.clone(),
                            &array_access.index,
                        )?;

                        let u64_type = sway::TypeName::Identifier {
                            name: "u64".into(),
                            generic_parameters: None,
                        };

                        let index = coerce_expression(
                            project,
                            module.clone(),
                            scope.clone(),
                            &array_access.index,
                            &index_type,
                            &u64_type,
                        )
                        .unwrap();

                        let rhs_type =
                            get_expression_type(project, module.clone(), scope.clone(), rhs)?;

                        let rhs = coerce_expression(
                            project,
                            module.clone(),
                            scope.clone(),
                            rhs,
                            &rhs_type,
                            &generic_parameters.entries[0].type_name,
                        )
                        .unwrap();

                        return Ok(sway::Expression::create_function_calls(
                            Some(array_access.expression.clone()),
                            &[(
                                "set",
                                Some((
                                    None,
                                    vec![
                                        index.clone(),
                                        match operator {
                                            "=" => rhs.clone(),

                                            _ => {
                                                if let Some(variable) = variable.clone() {
                                                    variable.borrow_mut().read_count += 1;
                                                }

                                                sway::Expression::from(sway::BinaryExpression {
                                                    operator: operator.trim_end_matches('=').into(),

                                                    lhs: sway::Expression::create_function_calls(
                                                        Some(array_access.expression.clone()),
                                                        &[
                                                            ("get", Some((None, vec![index]))),
                                                            ("unwrap", Some((None, vec![]))),
                                                        ],
                                                    ),

                                                    rhs: rhs.clone(),
                                                })
                                            }
                                        },
                                    ],
                                )),
                            )],
                        ));
                    }

                    sway::Expression::MemberAccess(member_access) => {
                        match &member_access.expression {
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
                                                Some(array_access.expression.clone()),
                                                &[
                                                    (
                                                        "get",
                                                        Some((
                                                            None,
                                                            vec![array_access.index.clone()],
                                                        )),
                                                    ),
                                                    ("unwrap", Some((None, vec![]))),
                                                ],
                                            ),
                                        }),
                                        // x.member = value;
                                        sway::Statement::from(sway::Expression::from(
                                            sway::BinaryExpression {
                                                operator: "=".into(),
                                                lhs: sway::Expression::from(sway::MemberAccess {
                                                    expression: sway::Expression::create_identifier(
                                                        variable_name.clone(),
                                                    ),
                                                    member: member_access.member.clone(),
                                                }),
                                                rhs: rhs.clone(),
                                            },
                                        )),
                                        // expr.set(i, a);
                                        sway::Statement::from(
                                            sway::Expression::create_function_calls(
                                                Some(array_access.expression.clone()),
                                                &[(
                                                    "set",
                                                    Some((
                                                        None,
                                                        vec![sway::Expression::create_identifier(
                                                            variable_name.clone(),
                                                        )],
                                                    )),
                                                )],
                                            ),
                                        ),
                                    ],
                                    final_expr: None,
                                }));
                            }

                            _ => todo!(
                                "translation assignment expression: {}",
                                sway::TabbedDisplayer(expression)
                            ),
                        }
                    }

                    sway::Expression::FunctionCall(function_call) => {
                        match &function_call.function {
                            sway::Expression::MemberAccess(member_access) => {
                                match member_access.member.as_str() {
                                    "get" if function_call.parameters.len() == 1 => {
                                        let container_type = get_expression_type(
                                            project,
                                            module.clone(),
                                            scope.clone(),
                                            &member_access.expression,
                                        )?;

                                        todo!(
                                            "translation {container_type} assignment expression: {}",
                                            sway::TabbedDisplayer(expression)
                                        )
                                    }

                                    "unwrap" if function_call.parameters.is_empty() => {
                                        match &member_access.expression {
                                            sway::Expression::FunctionCall(function_call) => {
                                                match &function_call.function {
                                                    sway::Expression::MemberAccess(
                                                        member_access,
                                                    ) => match member_access.member.as_str() {
                                                        "get"
                                                            if function_call.parameters.len()
                                                                == 1 =>
                                                        {
                                                            let lhs_type = get_expression_type(
                                                                project,
                                                                module.clone(),
                                                                scope.clone(),
                                                                &expression,
                                                            )?;

                                                            let rhs_type = get_expression_type(
                                                                project,
                                                                module.clone(),
                                                                scope.clone(),
                                                                rhs,
                                                            )?;

                                                            let rhs = coerce_expression(
                                                                project,
                                                                module.clone(),
                                                                scope.clone(),
                                                                rhs,
                                                                &rhs_type,
                                                                &lhs_type,
                                                            )
                                                            .unwrap();

                                                            return Ok(sway::Expression::create_function_calls(
                                                                Some(member_access.expression.clone()), &[
                                                                    ("set", Some((None, vec![
                                                                        function_call.parameters[0].clone(),
                                                                        rhs.clone(),
                                                                    ]))),
                                                                ],
                                                            ));
                                                        }

                                                        _ => todo!(
                                                            "translation assignment expression: {}",
                                                            sway::TabbedDisplayer(expression)
                                                        ),
                                                    },

                                                    _ => todo!(
                                                        "translation assignment expression: {}",
                                                        sway::TabbedDisplayer(expression)
                                                    ),
                                                }
                                            }

                                            _ => todo!(
                                                "translation assignment expression: {}",
                                                sway::TabbedDisplayer(expression)
                                            ),
                                        }
                                    }

                                    _ => todo!(
                                        "translation assignment expression: {}",
                                        sway::TabbedDisplayer(expression)
                                    ),
                                }
                            }

                            _ => todo!(
                                "translation assignment expression: {}",
                                sway::TabbedDisplayer(expression)
                            ),
                        }
                    }

                    _ => {
                        return Ok(sway::Expression::from(sway::BinaryExpression {
                            operator: "=".into(),
                            lhs: expression.clone(),
                            rhs: coerce_expression(
                                project,
                                module.clone(),
                                scope.clone(),
                                rhs,
                                rhs_type_name,
                                &expr_type_name,
                            )
                            .unwrap(),
                        }));
                    }
                }
            }

            _ => {}
        }
    }

    // All other assignments
    let rhs = coerce_expression(
        project,
        module.clone(),
        scope.clone(),
        rhs,
        &rhs_type_name,
        &expr_type_name,
    )
    .unwrap();

    match operator {
        "&=" | "|=" | "^=" => {
            //
            // NOTE:
            // Sway doesn't have these operators, so we have to implement them manually.
            //

            if let Some(variable) = variable.clone() {
                variable.borrow_mut().read_count += 1;
            }

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

        _ => {
            if let sway::Expression::Tuple(elements) = &expression
                && let (
                    sway::TypeName::Tuple {
                        type_names: lhs_type_names,
                    },
                    sway::TypeName::Tuple {
                        type_names: rhs_type_names,
                    },
                ) = (expr_type_name, rhs_type_name)
                && lhs_type_names.len() == rhs_type_names.len()
            {
                let variable_name = scope.borrow_mut().generate_unique_variable_name("x");
                let mut statements = vec![];

                statements.push(sway::Statement::from(sway::Let {
                    pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                        is_mutable: false,
                        name: variable_name.clone(),
                    }),
                    type_name: None,
                    value: rhs.clone(),
                }));

                for (i, element) in elements.iter().enumerate() {
                    statements.push(sway::Statement::from(create_assignment_expression(
                        project,
                        module.clone(),
                        scope.clone(),
                        "=",
                        element,
                        None,
                        &sway::Expression::from(sway::MemberAccess {
                            expression: sway::Expression::create_identifier(variable_name.clone()),
                            member: i.to_string(),
                        }),
                        &rhs_type_names[i],
                    )?));
                }

                return Ok(sway::Expression::from(sway::Block {
                    statements,
                    final_expr: None,
                }));
            }

            Ok(sway::Expression::from(sway::BinaryExpression {
                operator: operator.into(),
                lhs: expression.clone(),
                rhs: rhs.clone(),
            }))
        }
    }
}
