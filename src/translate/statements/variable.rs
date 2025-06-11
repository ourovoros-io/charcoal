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
    let mut type_name = translate_type_name(
        project,
        module.clone(),
        &variable_declaration.ty,
        false,
        false,
    );
    let mut abi_type_name = None;

    // Check if the parameter's type is an ABI
    if let sway::TypeName::Identifier {
        name,
        generic_parameters: None,
    } = &type_name
    {
        if project.find_module_with_contract(&name).is_some() {
            abi_type_name = Some(type_name.clone());

            type_name = sway::TypeName::Identifier {
                name: "Identity".into(),
                generic_parameters: None,
            };
        }
    }

    let mut value = None;

    if let Some(solidity::Expression::New(_, new_expression)) = initializer.as_ref() {
        let solidity::Expression::FunctionCall(_, ty, args) = new_expression.as_ref() else {
            panic!("Unexpected new expression: {new_expression} - {new_expression:#?}",);
        };

        let new_type_name = translate_type_name(project, module.clone(), ty, false, false);

        if type_name != new_type_name {
            panic!(
                "Invalid new expression type name: expected `{type_name}`, found `{new_type_name}`"
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
        let value_type = module
            .borrow_mut()
            .get_expression_type(project, scope.clone(), &value)?;
        coerce_expression(&value, &value_type, &type_name).unwrap()
    } else if let Some(x) = initializer.as_ref() {
        let x = translate_pre_or_post_operator_value_expression(
            project,
            module.clone(),
            scope.clone(),
            x,
        )?;
        let value_type = module
            .borrow_mut()
            .get_expression_type(project, scope.clone(), &x)?;
        coerce_expression(&x, &value_type, &type_name).unwrap()
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
        .variables
        .push(Rc::new(RefCell::new(ir::Variable {
            old_name,
            new_name,
            type_name,
            abi_type_name,
            ..Default::default()
        })));

    Ok(statement)
}
