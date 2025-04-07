use super::{translate_expression, variable::translate_variable_access_expression};
use crate::{
    errors::Error,
    project::Project,
    sway,
    translate::{function_call::utils::coerce_expression, TranslatedDefinition, TranslationScope},
};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_array_literal_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &Rc<RefCell<TranslationScope>>,
    expressions: &[solidity::Expression],
) -> Result<sway::Expression, Error> {
    Ok(sway::Expression::Array(sway::Array {
        elements: expressions.iter()
            .map(|x| translate_expression(project, translated_definition, scope, x))
            .collect::<Result<Vec<_>, _>>()?,
    }))
}

#[inline]
pub fn translate_array_subscript_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    //
    // NOTE:
    // Array subscript expressions should only ever be encountered for reading the value.
    // Writes are handled when translating assignment expressions.
    //

    let (variable, expression) = translate_variable_access_expression(project, translated_definition, scope, expression)?;

    if variable.is_none() {
        return Ok(expression);
    }
    
    let variable = variable.unwrap();
    let mut variable = variable.borrow_mut();

    variable.read_count += 1;

    Ok(expression)
}

#[inline]
pub fn translate_array_slice_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    let solidity::Expression::ArraySlice(_, array_expression, from_index, to_index) = expression else {
        panic!("Expected ArraySlice, found {expression:#?}")
    };

    let expression = translate_expression(project, translated_definition, scope, array_expression)?;
    let type_name = translated_definition.get_expression_type(scope, &expression)?;

    let u64_type = sway::TypeName::Identifier {
        name: "u64".into(),
        generic_parameters: None,
    };

    let mut from_index = from_index.as_ref().map(|x| {
        translate_expression(project, translated_definition, scope, x.as_ref()).unwrap()
    });

    let mut from_index_type = from_index.as_ref().map(|x| {
        translated_definition.get_expression_type(scope, x).unwrap()
    });

    // Check if from_index needs to be cast to u64
    if let (Some(from_index), Some(from_index_type)) = (from_index.as_mut(), from_index_type.as_mut()) {
        *from_index = coerce_expression(from_index, from_index_type, &u64_type).unwrap();
        *from_index_type = u64_type.clone();
    }

    let mut to_index = to_index.as_ref().map(|x| {
        translate_expression(project, translated_definition, scope, x.as_ref()).unwrap()
    });

    let mut to_index_type = to_index.as_ref().map(|x| {
        translated_definition.get_expression_type(scope, x).unwrap()
    });

    // Check if to_index needs to be cast to u64
    if let (Some(to_index), Some(to_index_type)) = (to_index.as_mut(), to_index_type.as_mut()) {
        *to_index = coerce_expression(to_index, to_index_type, &u64_type).unwrap();
        *to_index_type = u64_type.clone();
    }

    //
    // TODO: Check if to_index needs to be cast to u64
    //

    match &type_name {
        sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
            ("Bytes", None) => {
                let element_type = sway::TypeName::Identifier {
                    name: "u8".into(),
                    generic_parameters: None,
                };

                // x.ptr().add::<T>(from_index)
                // OR
                // x.ptr()
                let ptr_expr = match from_index.as_ref() {
                    // x.ptr().add::<T>(from_index)
                    Some(from_index) => sway::Expression::create_function_calls(Some(expression.clone()), &[
                        ("ptr", Some((None, vec![]))),
                        ("add", Some((
                            Some(sway::GenericParameterList {
                                entries: vec![
                                    sway::GenericParameter {
                                        type_name: element_type.clone(),
                                        implements: None,
                                    },
                                ],
                            }),
                            vec![
                                from_index.clone(),
                            ],
                        ))),
                    ]),

                    // x.ptr()
                    None => sway::Expression::create_function_calls(Some(expression.clone()), &[("ptr", Some((None, vec![])))]),
                };

                // ((to_index + 1) - from_index) * __size_of::<T>()
                // OR
                // (x.len() - from_index) * __size_of::<T>()
                // OR
                // (to_index + 1) * __size_of::<T>()
                // OR
                // x.len() * __size_of::<T>()
                let len_expr = match from_index.as_ref() {
                    // ((to_index + 1) - from_index) * __size_of::<T>()
                    // OR
                    // (x.len() - from_index) * __size_of::<T>()
                    Some(from_index) => sway::Expression::from(sway::BinaryExpression {
                        operator: "*".into(),
                        lhs: sway::Expression::Tuple(vec![
                            sway::Expression::from(sway::BinaryExpression {
                                operator: "-".into(),
                                
                                lhs: match to_index {
                                    // (to_index + 1)
                                    Some(to_index) => sway::Expression::Tuple(vec![
                                        sway::Expression::from(sway::BinaryExpression {
                                            operator: "+".into(),
                                            lhs: to_index.clone(),
                                            rhs: sway::Expression::from(sway::Literal::DecInt(1u8.into(), None)),
                                        })
                                    ]),
    
                                    // x.len()
                                    None => sway::Expression::create_function_calls(Some(expression.clone()), &[("len", Some((None, vec![])))]),
                                },
    
                                rhs: from_index.clone(),
                            }),
                        ]),
                        rhs: sway::Expression::create_function_calls(None, &[
                            ("__size_of", Some((
                                Some(sway::GenericParameterList {
                                    entries: vec![
                                        sway::GenericParameter {
                                            type_name: element_type.clone(),
                                            implements: None,
                                        },
                                    ],
                                }),
                                vec![],
                            ))),
                        ]),
                    }),
                    
                    // (to_index + 1) * __size_of::<T>()
                    // OR
                    // x.len() * __size_of::<T>()
                    None => sway::Expression::from(sway::BinaryExpression {
                        operator: "*".into(),

                        lhs: match to_index {
                            // (to_index + 1)
                            Some(to_index) => sway::Expression::Tuple(vec![
                                sway::Expression::from(sway::BinaryExpression {
                                    operator: "+".into(),
                                    lhs: to_index.clone(),
                                    rhs: sway::Expression::from(sway::Literal::DecInt(1u8.into(), None)),
                                })
                            ]),

                            // x.len()
                            None => sway::Expression::create_function_calls(Some(expression.clone()), &[("len", Some((None, vec![])))]),
                        },

                        rhs: sway::Expression::create_function_calls(None, &[
                            ("__size_of", Some((
                                Some(sway::GenericParameterList {
                                    entries: vec![
                                        sway::GenericParameter {
                                            type_name: element_type.clone(),
                                            implements: None,
                                        },
                                    ],
                                }),
                                vec![],
                            ))),
                        ]),
                    }),
                };

                // x[from_index:to_index] => raw_slice::from_parts::<T>(ptr_expr, len_expr)
                return Ok(sway::Expression::create_function_calls(None, &[
                    ("raw_slice::from_parts", Some((
                        Some(sway::GenericParameterList {
                            entries: vec![
                                sway::GenericParameter {
                                    type_name: element_type.clone(),
                                    implements: None,
                                },
                            ],
                        }),
                        vec![
                            ptr_expr,
                            len_expr,
                        ],
                    ))),
                ]));
            }

            _ => {}
        }

        _ => {}
    }

    todo!("translate {} array slice expression: {array_expression} - {expression:#?}", sway::TabbedDisplayer(&type_name))
}
