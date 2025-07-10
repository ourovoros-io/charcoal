use crate::{error::Error, project::Project, sway, translate::*};
use convert_case::Case;
use num_bigint::BigUint;
use num_traits::{One, Zero};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_variable_definition_statement(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    variable_declaration: &solidity::VariableDeclaration,
    initializer: Option<&solidity::Expression>,
) -> Result<sway::Statement, Error> {
    let old_name = variable_declaration.name.as_ref().unwrap().name.clone();
    let new_name = translate_naming_convention(old_name.as_str(), Case::Snake);

    let type_name = translate_type_name(
        project,
        module.clone(),
        scope.clone(),
        &variable_declaration.ty,
        variable_declaration.storage.as_ref(),
    );

    let mut value = None;

    if let Some(solidity::Expression::New(_, new_expression)) = initializer.as_ref() {
        let solidity::Expression::FunctionCall(_, ty, args) = new_expression.as_ref() else {
            panic!("Unexpected new expression: {new_expression} - {new_expression:#?}",);
        };

        let new_type_name = translate_type_name(project, module.clone(), scope.clone(), ty, None);

        if type_name != new_type_name {
            panic!(
                "Invalid new expression type name: expected `{type_name}`, found `{new_type_name}` - `{}`",
                project.loc_to_file_location_string(module.clone(), &variable_declaration.loc)
            );
        }

        match &type_name {
            sway::TypeName::Identifier {
                name,
                generic_parameters: Some(generic_parameters),
            } if name == "Vec" => {
                // {
                //     let mut v = Vec::with_capacity(length);
                //     let mut i = 0;
                //     while i < length {
                //         v.push(0);
                //         i += 1;
                //     }
                //     v
                // }

                if args.len() != 1 {
                    panic!(
                        "Invalid new array expression: expected 1 argument, found {}",
                        args.len()
                    );
                }

                let element_type_name = &generic_parameters.entries.first().unwrap().type_name;
                let length =
                    translate_expression(project, module.clone(), scope.clone(), &args[0])?;

                value = Some(sway::Expression::from(sway::Block {
                    statements: vec![
                        // let mut v = Vec::with_capacity(length);
                        sway::Statement::from(sway::Let {
                            pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                                is_mutable: true,
                                name: "v".into(),
                            }),
                            type_name: Some(type_name.clone()),
                            value: sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::create_identifier(
                                    "Vec::with_capacity".into(),
                                ),
                                generic_parameters: None,
                                parameters: vec![length.clone()],
                            }),
                        }),
                        // let mut i = 0;
                        sway::Statement::from(sway::Let {
                            pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                                is_mutable: true,
                                name: "i".into(),
                            }),
                            type_name: None,
                            value: sway::Expression::from(sway::Literal::DecInt(
                                BigUint::zero(),
                                None,
                            )),
                        }),
                        // while i < length {
                        //     v.push(0);
                        //     i += 1;
                        // }
                        sway::Statement::from(sway::Expression::from(sway::While {
                            // i < length
                            condition: sway::Expression::from(sway::BinaryExpression {
                                operator: "<".into(),
                                lhs: sway::Expression::create_identifier("i".into()),
                                rhs: length.clone(),
                            }),

                            body: sway::Block {
                                statements: vec![
                                    // v.push(0);
                                    sway::Statement::from(sway::Expression::create_function_calls(
                                        None,
                                        &[
                                            ("v", None),
                                            (
                                                "push",
                                                Some((
                                                    None,
                                                    vec![create_value_expression(
                                                        project,
                                                        module.clone(),
                                                        scope.clone(),
                                                        element_type_name,
                                                        None,
                                                    )],
                                                )),
                                            ),
                                        ],
                                    )),
                                    // i += 1;
                                    sway::Statement::from(sway::Expression::from(
                                        sway::BinaryExpression {
                                            operator: "+=".into(),
                                            lhs: sway::Expression::create_identifier("i".into()),
                                            rhs: sway::Expression::from(sway::Literal::DecInt(
                                                BigUint::one(),
                                                None,
                                            )),
                                        },
                                    )),
                                ],
                                final_expr: None,
                            },
                        })),
                    ],

                    // v
                    final_expr: Some(sway::Expression::create_identifier("v".into())),
                }));
            }

            _ => {}
        }
    }

    let value = if let Some(value) = value {
        let value_type = get_expression_type(project, module.clone(), scope.clone(), &value)?;
        coerce_expression(
            project,
            module.clone(),
            scope.clone(),
            &value,
            &value_type,
            &type_name,
        )
        .unwrap()
    } else if let Some(x) = initializer.as_ref() {
        let mut x = translate_expression(project, module.clone(), scope.clone(), &x)?;

        if let sway::Expression::FunctionCall(f) = &x
            && let sway::Expression::MemberAccess(m) = &f.function
            && m.member == "read"
            && f.parameters.is_empty()
        {
            let container_type =
                get_expression_type(project, module.clone(), scope.clone(), &m.expression)?;

            if container_type.is_storage_key() {
                x = m.expression.clone();
            }
        }

        let value_type = get_expression_type(project, module.clone(), scope.clone(), &x)?;

        coerce_expression(
            project,
            module.clone(),
            scope.clone(),
            &x,
            &value_type,
            &type_name,
        )
        .unwrap()
    } else {
        create_value_expression(project, module.clone(), scope.clone(), &type_name, None)
    };

    let statement = sway::Statement::from(sway::Let {
        pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
            is_mutable: false,
            name: new_name.clone(),
        }),

        type_name: if (type_name.is_uint() || type_name.is_int())
            && matches!(value, sway::Expression::Literal(_))
        {
            Some(type_name.clone())
        } else {
            None
        },

        value,
    });

    scope
        .borrow_mut()
        .add_variable(Rc::new(RefCell::new(ir::Variable {
            old_name,
            new_name,
            type_name,
            ..Default::default()
        })));

    Ok(statement)
}
