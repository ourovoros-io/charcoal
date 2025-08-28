use crate::{
    ir,
    project::Project,
    sway,
    translate::{create_value_expression, get_expression_type, get_underlying_type, translate_naming_convention},
};
use convert_case::{Case, Casing};
use num_bigint::BigUint;
use num_traits::{One, Zero};
use std::{cell::RefCell, rc::Rc};

struct CoerceContext<'a, 'b> {
    project: &'a mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    expression: &'b sway::Expression,
    from_type_name: &'b sway::TypeName,
    to_type_name: &'b sway::TypeName,
}

/// Coerces an expression from one type to another
pub fn coerce_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    expression: &sway::Expression,
    from_type_name: &sway::TypeName,
    to_type_name: &sway::TypeName,
) -> Option<sway::Expression> {
    // println!(
    //     "Coercing from `{from_type_name:#?}` to `{to_type_name:#?}`: {}",
    //     sway::TabbedDisplayer(expression)
    // );

    let from_type_name = get_underlying_type(project, module.clone(), from_type_name);
    let to_type_name = get_underlying_type(project, module.clone(), to_type_name);

    // HACK: no coercion necessary when going to `todo!` type
    if to_type_name.is_todo() {
        return Some(expression.clone());
    }

    let mut context = CoerceContext {
        project,
        module: module.clone(),
        scope: scope.clone(),
        expression,
        from_type_name: &from_type_name,
        to_type_name: &to_type_name,
    };

    if let Some(result) = coerce_abi_cast(&mut context) {
        return Some(result);
    }

    if let Some(result) = coerce_storage_types(&mut context) {
        return Some(result);
    }

    // Broken storage coercion hacks
    // TODO: remove these when sway doesn't suck
    if let Some(result) = coerce_broken_types(&mut context) {
        return Some(result);
    }

    if let Some(result) = coerce_generic_types(&mut context) {
        return Some(result);
    }

    if let Some(result) = coerce_identity_types(&mut context) {
        return Some(result);
    }

    if let Some(result) = coerce_to_integer_types(&mut context) {
        return Some(result);
    }

    if let Some(result) = coerce_to_u8_arrays(&mut context) {
        return Some(result);
    }

    if let Some(result) = coerce_to_bytes(&mut context) {
        return Some(result);
    }

    if let Some(result) = coerce_to_string_slice(&mut context) {
        return Some(result);
    }

    if let Some(result) = coerce_to_string(&mut context) {
        return Some(result);
    }

    if let Some(result) = coerce_to_vec(&mut context) {
        return Some(result);
    }

    if let Some(result) = coerce_array_types(&mut context) {
        return Some(result);
    }

    if let Some(result) = coerce_tuple_types(&mut context) {
        return Some(result);
    }

    if let Some(result) = coerce_contract_storage_struct_types(&mut context) {
        return Some(result);
    }

    None
}

/// Coerce abi casts
fn coerce_abi_cast(context: &mut CoerceContext) -> Option<sway::Expression> {
    if !context.from_type_name.is_compatible_with(&context.to_type_name) {
        return None;
    }

    // Check for abi cast to `Identity` coercions
    if context.to_type_name.is_identity() {
        let mut comment = None;
        let mut expression = context.expression.clone();

        if let sway::Expression::Commented(c, e) = &expression {
            comment = Some(c.clone());
            expression = e.as_ref().clone();
        }

        if let sway::Expression::FunctionCall(f) = &expression
            && let Some("abi") = f.function.as_identifier()
            && f.parameters.len() == 2
        {
            return Some(
                coerce_expression(
                    context.project,
                    context.module.clone(),
                    context.scope.clone(),
                    &if let Some(comment) = comment {
                        sway::Expression::Commented(comment, Box::new(f.parameters[1].clone()))
                    } else {
                        f.parameters[1].clone()
                    },
                    &sway::TypeName::create_identifier("b256"),
                    &context.to_type_name,
                )
                .unwrap(),
            );
        }
    }

    // Check for `Identity` to abi cast coercions
    if context.from_type_name.is_identity()
        && let Some(abi_type) = context.to_type_name.abi_type()
        && context
            .project
            .find_contract(context.module.clone(), abi_type.to_string().as_str())
            .is_some()
    {
        return Some(sway::Expression::create_function_call(
            "abi",
            None,
            vec![
                sway::Expression::create_identifier(abi_type.to_string().as_str()),
                context.expression.with_bits_call(),
            ],
        ));
    }

    Some(context.expression.clone())
}

/// Coerce storage types. These are done *before* regular coercion so that generated code looks nicer.
fn coerce_storage_types(context: &mut CoerceContext) -> Option<sway::Expression> {
    let mut expression = context.expression.clone();
    let mut from_type_name = context.from_type_name.clone();

    // HACK: If the expression is reading from a `StorageKey<T>`, remove the `.read()` temporarily
    if let Some(container) = expression.to_read_call_parts() {
        let container_type = get_expression_type(
            context.project,
            context.module.clone(),
            context.scope.clone(),
            container,
        )
        .unwrap();

        if container_type.is_storage_key() {
            expression = container.clone();
            from_type_name = get_underlying_type(context.project, context.module.clone(), &container_type);
        }
    }

    // HACK: if the expression is a `Option<T>`, add a `.unwrap()` temporarily
    if let Some(option_type) = from_type_name.option_type() {
        expression = expression.with_unwrap_call();
        from_type_name = get_underlying_type(context.project, context.module.clone(), &option_type);
    }

    // Check for `Struct` to `StorageStruct` coercion
    if let (sway::TypeName::Identifier { name: lhs_name, .. }, sway::TypeName::Identifier { name: rhs_name, .. }) =
        (&from_type_name, &context.to_type_name)
    {
        let mut expression = expression.clone();
        let mut lhs_name = lhs_name.clone();
        let mut rhs_name = rhs_name.clone();
        let mut skip = false;

        if let Some(storage_key_type) = from_type_name.storage_key_type()
            && !context.to_type_name.is_storage_key()
        {
            expression = expression.with_read_call();

            if let sway::TypeName::Identifier {
                name,
                generic_parameters,
            } = storage_key_type
            {
                if generic_parameters.is_some() {
                    skip = true;
                }

                lhs_name = name;
            } else {
                skip = true;
            }
        }

        if let Some(storage_key_type) = context.to_type_name.storage_key_type()
            && !from_type_name.is_storage_key()
        {
            // expression = expression.with_read_call();

            if let sway::TypeName::Identifier {
                name,
                generic_parameters,
            } = storage_key_type
            {
                if generic_parameters.is_some() {
                    skip = true;
                }

                rhs_name = name;
            } else {
                skip = true;
            }
        }

        if !skip {
            if let (Some(lhs_struct_definition), Some(rhs_struct_definition)) = (
                context
                    .project
                    .find_struct(context.module.clone(), context.scope.clone(), &lhs_name),
                context
                    .project
                    .find_struct(context.module.clone(), context.scope.clone(), &rhs_name),
            ) && lhs_struct_definition == rhs_struct_definition
            {
                if lhs_name == rhs_name {
                    return Some(expression);
                }

                let mut check_struct_memory_to_storage = |memory: bool| -> Option<sway::Expression> {
                    let lhs_struct_name = if memory {
                        lhs_struct_definition.borrow().memory.name.clone()
                    } else {
                        lhs_struct_definition.borrow().storage.name.clone()
                    };

                    let rhs_struct_name = if memory {
                        rhs_struct_definition.borrow().storage.name.clone()
                    } else {
                        rhs_struct_definition.borrow().memory.name.clone()
                    };

                    if lhs_name != lhs_struct_name || rhs_name != rhs_struct_name {
                        return None;
                    }

                    let struct_definition = lhs_struct_definition.borrow();

                    let lhs_fields = if memory {
                        struct_definition.memory.fields.as_slice()
                    } else {
                        struct_definition.storage.fields.as_slice()
                    };

                    let rhs_fields = if memory {
                        struct_definition.storage.fields.as_slice()
                    } else {
                        struct_definition.memory.fields.as_slice()
                    };

                    if let sway::Expression::Constructor(constructor) = &expression {
                        if memory {
                            let struct_field_name = translate_naming_convention(&lhs_name, Case::Snake);
                            let instance_field_name = format!("{struct_field_name}_instance_count");

                            let mut value = create_value_expression(
                                context.project,
                                context.module.clone(),
                                context.scope.clone(),
                                &sway::TypeName::create_identifier(&rhs_name),
                                None,
                            );

                            let mut instance_index_statement = None;
                            let mut storage_fields_to_write = vec![];

                            if let sway::Expression::Constructor(c) = &mut value {
                                for (
                                    (memory_constructor_field, storage_constructor_field),
                                    (memory_struct_field, storage_struct_field),
                                ) in constructor
                                    .fields
                                    .iter()
                                    .zip(c.fields.iter_mut())
                                    .zip(lhs_fields.iter().zip(rhs_fields.iter()))
                                {
                                    let mut field_type_name = storage_struct_field.type_name.clone();
                                    let mut needs_wrap = false;

                                    if let Some(option_type) = field_type_name.option_type()
                                        && option_type.is_storage_key()
                                    {
                                        field_type_name = option_type;
                                        needs_wrap = true;
                                    }

                                    if memory_struct_field
                                        .type_name
                                        .is_compatible_with(&storage_struct_field.type_name)
                                    {
                                        storage_constructor_field.value = memory_constructor_field.value.clone();
                                    } else if let Some(storage_key_type) = field_type_name.storage_key_type() {
                                        let storage_key_type = get_underlying_type(
                                            context.project,
                                            context.module.clone(),
                                            &storage_key_type,
                                        );

                                        if instance_index_statement.is_none() {
                                            instance_index_statement = Some(sway::Statement::from(sway::Let {
                                                pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                                                    is_mutable: false,
                                                    name: "instance_index".to_string(),
                                                }),
                                                type_name: None,
                                                value: sway::Expression::create_identifier("storage_struct")
                                                    .with_member(&instance_field_name)
                                                    .with_read_call(),
                                            }));

                                            storage_fields_to_write.push(sway::Statement::from(
                                                sway::Expression::create_identifier("storage_struct")
                                                    .with_member(&instance_field_name)
                                                    .with_write_call(sway::Expression::from(sway::BinaryExpression {
                                                        operator: "+".to_string(),
                                                        lhs: sway::Expression::create_identifier(&instance_field_name),
                                                        rhs: sway::Expression::from(sway::Literal::DecInt(
                                                            1_u64.into(),
                                                            None,
                                                        )),
                                                    })),
                                            ));
                                        }

                                        let mapping_field_name =
                                            format!("{struct_field_name}_{}_instances", storage_struct_field.new_name,);

                                        storage_constructor_field.value =
                                            sway::Expression::create_identifier("storage_struct")
                                                .with_member(&mapping_field_name)
                                                .with_get_call(sway::Expression::create_identifier("instance_index"));

                                        if needs_wrap {
                                            storage_constructor_field.value =
                                                storage_constructor_field.value.into_some_call();
                                        }

                                        if storage_key_type.is_storage_bytes() || storage_key_type.is_storage_string() {
                                            storage_fields_to_write.push(sway::Statement::from(
                                                sway::Expression::create_identifier("storage_struct")
                                                    .with_member(&mapping_field_name)
                                                    .with_get_call(sway::Expression::create_identifier(
                                                        "instance_index",
                                                    ))
                                                    .with_clear_call(),
                                            ));

                                            storage_fields_to_write.push(sway::Statement::from(
                                                sway::Expression::create_identifier("storage_struct")
                                                    .with_member(&mapping_field_name)
                                                    .with_get_call(sway::Expression::create_identifier(
                                                        "instance_index",
                                                    ))
                                                    .with_write_slice_call(memory_constructor_field.value.clone()),
                                            ));

                                            continue;
                                        }

                                        if storage_key_type.is_uint()
                                            || storage_key_type.is_identity()
                                            || storage_key_type.is_bool()
                                        {
                                            continue;
                                        }

                                        todo!("storage key type: {storage_key_type}");
                                    } else {
                                        todo!()
                                    }
                                }
                            }

                            if let Some(instance_index_statement) = instance_index_statement {
                                storage_fields_to_write.insert(0, instance_index_statement);

                                return Some(sway::Expression::from(sway::Block {
                                    statements: storage_fields_to_write,
                                    final_expr: Some(value),
                                }));
                            }
                        }

                        return Some(sway::Expression::from(sway::Constructor {
                            type_name: context.to_type_name.clone(),
                            fields: lhs_fields
                                .iter()
                                .zip(rhs_fields.iter())
                                .zip(constructor.fields.iter())
                                .map(|((lhs_field, rhs_field), constructor_field)| sway::ConstructorField {
                                    name: rhs_field.new_name.clone(),
                                    value: coerce_expression(
                                        context.project,
                                        context.module.clone(),
                                        context.scope.clone(),
                                        &constructor_field.value,
                                        &lhs_field.type_name,
                                        &rhs_field.type_name,
                                    )
                                    .unwrap(),
                                })
                                .collect(),
                        }));
                    }

                    let variable_name = context.scope.borrow_mut().generate_unique_variable_name("x");

                    return Some(sway::Expression::from(sway::Block {
                        statements: vec![sway::Statement::from(sway::Let {
                            pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                                is_mutable: false,
                                name: variable_name.clone(),
                            }),
                            type_name: None,
                            value: expression.clone(),
                        })],
                        final_expr: Some(sway::Expression::from(sway::Constructor {
                            type_name: context.to_type_name.clone(),
                            fields: lhs_fields
                                .iter()
                                .zip(rhs_fields.iter())
                                .map(|(lhs_field, rhs_field)| sway::ConstructorField {
                                    name: rhs_field.new_name.clone(),
                                    value: sway::Expression::from(sway::MemberAccess {
                                        expression: sway::Expression::create_identifier(variable_name.as_str()),
                                        member: lhs_field.new_name.clone(),
                                    }),
                                })
                                .collect(),
                        })),
                    }));
                };

                // Check for memory to storage coercions
                if let Some(result) = check_struct_memory_to_storage(true) {
                    return Some(result);
                }

                // Check for storage to memory coercions
                if let Some(result) = check_struct_memory_to_storage(false) {
                    return Some(result);
                }
            }
        }
    }

    // Check for `StorageKey<StorageString>` to `T` coercions
    if let Some(storage_key_type) = from_type_name.storage_key_type()
        && storage_key_type.is_storage_string()
    {
        // Check for `StorageKey<StorageString>` to `Bytes` coercions
        if context.to_type_name.is_bytes() {
            context
                .scope
                .borrow_mut()
                .set_function_storage_accesses(context.module.clone(), true, false);

            return Some(
                expression
                    .with_read_slice_call()
                    .with_unwrap_call()
                    .with_as_bytes_call(),
            );
        }

        // Check for `StorageKey<StorageString>` to `String` coercions
        if context.to_type_name.is_string() {
            context
                .scope
                .borrow_mut()
                .set_function_storage_accesses(context.module.clone(), true, false);

            return Some(expression.with_read_slice_call().with_unwrap_call());
        }
    }

    // Check for `StorageKey<StorageVec<T>>` to `Vec<T>` coercions
    if let Some(storage_key_type) = from_type_name.storage_key_type()
        && let Some(storage_vec_type) = storage_key_type.storage_vec_type()
        && let Some(vec_type) = context.to_type_name.vec_type()
    {
        context
            .scope
            .borrow_mut()
            .set_function_storage_accesses(context.module.clone(), true, false);

        // Use `x.load_vec()` if the element types are compatible
        if storage_vec_type.is_compatible_with(&vec_type) {
            return Some(expression.with_load_vec_call());
        }

        let get_expression = expression
            .with_get_call(sway::Expression::create_identifier("i"))
            .with_unwrap_call()
            .with_read_call();

        let element_expression = coerce_expression(
            context.project,
            context.module.clone(),
            context.scope.clone(),
            &get_expression,
            &storage_vec_type,
            &vec_type,
        )
        .unwrap();

        return Some(sway::Expression::from(sway::Block {
            statements: vec![
                sway::Statement::from(sway::Let {
                    pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                        is_mutable: false,
                        name: "len".to_string(),
                    }),
                    type_name: None,
                    value: expression.with_len_call(),
                }),
                sway::Statement::from(sway::Let {
                    pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                        is_mutable: true,
                        name: "v".to_string(),
                    }),
                    type_name: None,
                    value: sway::Expression::create_function_call(
                        "Vec::with_capacity",
                        None,
                        vec![sway::Expression::create_identifier("len")],
                    ),
                }),
                sway::Statement::from(sway::Let {
                    pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                        is_mutable: true,
                        name: "i".to_string(),
                    }),
                    type_name: None,
                    value: sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                }),
                sway::Statement::from(sway::Expression::from(sway::While {
                    condition: sway::Expression::from(sway::BinaryExpression {
                        operator: "<".to_string(),
                        lhs: sway::Expression::create_identifier("i"),
                        rhs: sway::Expression::create_identifier("len"),
                    }),
                    body: sway::Block {
                        statements: vec![
                            sway::Statement::from(
                                sway::Expression::create_identifier("v").with_push_call(element_expression.clone()),
                            ),
                            sway::Statement::from(sway::Expression::from(sway::BinaryExpression {
                                operator: "+=".to_string(),
                                lhs: sway::Expression::create_identifier("i"),
                                rhs: sway::Expression::from(sway::Literal::DecInt(BigUint::one(), None)),
                            })),
                        ],
                        final_expr: None,
                    },
                })),
            ],
            final_expr: Some(sway::Expression::create_identifier("v")),
        }));
    }

    // Check for `StorageKey<StorageBytes>` to `Bytes` coercion
    if let Some(storage_key_type) = from_type_name.storage_key_type()
        && storage_key_type.is_storage_bytes()
        && context.to_type_name.is_bytes()
    {
        return Some(
            expression
                .with_read_slice_call()
                .with_unwrap_or_call(sway::Expression::create_function_call("Bytes::new", None, vec![])),
        );
    }

    None
}

/// Coerce broken types
fn coerce_broken_types(context: &mut CoerceContext) -> Option<sway::Expression> {
    // HACK: Check for `Bytes` to `StorageBytes` coercions
    if context.from_type_name.is_bytes() && context.to_type_name.is_storage_bytes() {
        return Some(context.expression.clone());
    }

    // HACK: Check for `Bytes` to `StorageKey<StorageBytes>` coercions
    if context.from_type_name.is_bytes()
        && let Some(storage_key_type) = context.to_type_name.storage_key_type()
        && storage_key_type.is_storage_bytes()
    {
        return Some(context.expression.clone());
    }

    // HACK: Check for `Bytes` to `Option<StorageKey<StorageBytes>>` coercions
    if context.from_type_name.is_bytes()
        && let Some(option_type) = context.to_type_name.option_type()
        && let Some(storage_key_type) = option_type.storage_key_type()
        && storage_key_type.is_storage_bytes()
    {
        return Some(context.expression.clone());
    }

    // HACK: Check for `String` to `StorageString` coercions
    if context.from_type_name.is_string() && context.to_type_name.is_storage_string() {
        return Some(context.expression.with_as_str_call());
    }

    // HACK: Check for `StorageString` to `str` coercions
    if context.from_type_name.is_storage_string() && context.to_type_name.is_string_slice() {
        return Some(context.expression.with_as_str_call());
    }

    // HACK: Check for `T` to `Option<StorageKey<T>>` coercions
    if let Some(option_type) = context.to_type_name.option_type()
        && let Some(storage_key_type) = option_type.storage_key_type()
    {
        let storage_key_type = get_underlying_type(context.project, context.module.clone(), &storage_key_type);

        if storage_key_type.is_compatible_with(&context.from_type_name) {
            return Some(context.expression.clone());
        }

        if context.from_type_name.is_uint() && storage_key_type.is_uint() {
            return Some(
                coerce_expression(
                    context.project,
                    context.module.clone(),
                    context.scope.clone(),
                    &context.expression,
                    &context.from_type_name,
                    &storage_key_type,
                )
                .unwrap(),
            );
        }
    }

    // HACK: Check for `String` to `Option<StorageKey<String>>` coercions
    if context.from_type_name.is_string()
        && let Some(option_type) = context.to_type_name.option_type()
        && let Some(storage_key_type) = option_type.storage_key_type()
        && storage_key_type.is_storage_string()
    {
        return Some(context.expression.clone());
    }

    // HACK: Check for `Vec<T>` to `StorageVec<T>` coercions
    if let Some(vec_type) = context.from_type_name.vec_type()
        && let Some(storage_vec_type) = context.to_type_name.storage_vec_type()
    {
        let vec_type = get_underlying_type(context.project, context.module.clone(), &vec_type);
        let storage_vec_type = get_underlying_type(context.project, context.module.clone(), &storage_vec_type);

        if vec_type.is_compatible_with(&storage_vec_type)
            || (vec_type.is_bytes() && storage_vec_type.is_storage_bytes())
            || ((vec_type.is_string() || vec_type.is_string_slice()) && storage_vec_type.is_storage_string())
        {
            return Some(context.expression.clone());
        }
    }

    None
}

/// Coerce generic types
fn coerce_generic_types(context: &mut CoerceContext) -> Option<sway::Expression> {
    // Check for `Option<T>` to `T` coercions
    if let Some(option_type) = context.from_type_name.option_type()
        && option_type.is_compatible_with(&context.to_type_name)
    {
        return Some(context.expression.with_unwrap_call());
    }

    // Check for `Option<StorageKey<T>>` to `T` coercions
    if let Some(option_type) = context.from_type_name.option_type()
        && let Some(storage_key_type) = option_type.storage_key_type()
        && let Some(result) = coerce_expression(
            context.project,
            context.module.clone(),
            context.scope.clone(),
            &context.expression.with_unwrap_call().with_read_call(),
            &storage_key_type,
            &context.to_type_name,
        )
    {
        return Some(result);
    }

    // Check for `T` to `Option<T>` coercions
    if let Some(option_type) = context.to_type_name.option_type()
        && option_type.is_compatible_with(&context.from_type_name)
    {
        // If the expression ends in `.unwrap()`, remove it
        if let Some(container) = context.expression.to_unwrap_call_parts() {
            return Some(container.clone());
        }

        // Otherwise wrap it in `Some(_)`
        return Some(context.expression.into_some_call());
    }

    // Check for `StorageKey<T>` to `T` coercions
    if let Some(storage_key_type) = context.from_type_name.storage_key_type() {
        let storage_key_type = get_underlying_type(context.project, context.module.clone(), &storage_key_type);

        if context.to_type_name.is_compatible_with(&storage_key_type) {
            context
                .scope
                .borrow_mut()
                .set_function_storage_accesses(context.module.clone(), true, false);

            return Some(context.expression.with_read_call());
        }
    }

    // Check for `T` to `StorageKey<T>` coercions
    if let Some(storage_key_type) = context.to_type_name.storage_key_type() {
        let storage_key_type = get_underlying_type(context.project, context.module.clone(), &storage_key_type);

        if storage_key_type.is_compatible_with(&context.from_type_name)
            && let Some(expression) = context.expression.to_read_call_parts()
        {
            return Some(expression.clone());
        }
    }

    None
}

/// Coerce identity types
fn coerce_identity_types(context: &mut CoerceContext) -> Option<sway::Expression> {
    // Check for `Identity` to `b256` coercions
    if context.from_type_name.is_identity() && context.to_type_name.is_b256() {
        return Some(context.expression.with_bits_call());
    }

    // Check for `Identity` to `u256` coercions
    if context.from_type_name.is_identity() && context.to_type_name.is_u256() {
        return Some(context.expression.with_bits_call().with_as_u256_call());
    }

    // Check for uint to `Identity` coercions
    if (context.from_type_name.is_uint() || context.from_type_name.is_b256()) && context.to_type_name.is_identity() {
        let mut expression = context.expression.clone();

        if context.from_type_name.is_uint() {
            expression = coerce_expression(
                context.project,
                context.module.clone(),
                context.scope.clone(),
                &expression,
                &context.from_type_name,
                &sway::TypeName::create_identifier("b256"),
            )
            .unwrap();
        }

        return Some(sway::Expression::create_function_call(
            "Identity::Address",
            None,
            vec![sway::Expression::create_function_call(
                "Address::from",
                None,
                vec![expression],
            )],
        ));
    }

    // Check for `ContractId` to `Identity` coercions
    if context.from_type_name.is_contract_id() && context.to_type_name.is_identity() {
        return Some(sway::Expression::create_function_call(
            "Identity::ContractId",
            None,
            vec![context.expression.clone()],
        ));
    }

    // Check for `[u8; N]` to `Identity` coercions
    if let Some((from_type_array_element_type, from_byte_count)) = context.from_type_name.array_info()
        && from_type_array_element_type.is_u8()
        && from_byte_count <= 32
        && context.to_type_name.is_identity()
    {
        let mut expression = context.expression.clone();

        if from_byte_count < 32 {
            let Some(result) = coerce_expression(
                context.project,
                context.module.clone(),
                context.scope.clone(),
                &expression,
                &context.from_type_name,
                &sway::TypeName::create_array(sway::TypeName::create_identifier("u8"), 32),
            ) else {
                panic!(
                    "Failed to coerce `{}` from `{}` to `[u8; 32]`",
                    sway::TabbedDisplayer(&expression),
                    context.from_type_name
                )
            };

            expression = result;
        }

        expression = coerce_expression(
            context.project,
            context.module.clone(),
            context.scope.clone(),
            &expression,
            &context.from_type_name,
            &sway::TypeName::create_identifier("b256"),
        )
        .unwrap();

        return Some(sway::Expression::create_function_call(
            "Identity::Address",
            None,
            vec![sway::Expression::create_function_call(
                "Address::from",
                None,
                vec![expression],
            )],
        ));
    }

    None
}

/// Coerce to integer types
fn coerce_to_integer_types(context: &mut CoerceContext) -> Option<sway::Expression> {
    // Check for uint to int coercions
    if context.from_type_name.is_uint() && context.to_type_name.is_int() {
        let lhs_bits: usize = context
            .from_type_name
            .to_string()
            .trim_start_matches('u')
            .trim_start_matches('U')
            .trim_start_matches('I')
            .parse()
            .unwrap();

        let rhs_bits: usize = context
            .to_type_name
            .to_string()
            .trim_start_matches('u')
            .trim_start_matches('U')
            .trim_start_matches('I')
            .parse()
            .unwrap();

        let mut expression = context.expression.clone();

        if lhs_bits > rhs_bits {
            expression = sway::Expression::create_function_call(
                format!("u{rhs_bits}::try_from").as_str(),
                None,
                vec![expression.clone()],
            )
            .with_unwrap_call();
        } else if lhs_bits < rhs_bits {
            expression = expression.with_function_call(format!("as_u{rhs_bits}").as_str(), None, vec![]);
        }

        return Some(sway::Expression::create_function_call(
            format!("I{rhs_bits}::from_uint").as_str(),
            None,
            vec![expression.clone()],
        ));
    }

    // Check for int to uint coercions
    if context.from_type_name.is_int() && context.to_type_name.is_uint() {
        let lhs_bits: usize = context
            .from_type_name
            .to_string()
            .trim_start_matches('u')
            .trim_start_matches('U')
            .trim_start_matches('I')
            .parse()
            .unwrap();

        let rhs_bits: usize = context
            .to_type_name
            .to_string()
            .trim_start_matches('u')
            .trim_start_matches('U')
            .trim_start_matches('I')
            .parse()
            .unwrap();

        let mut expression = context.expression.clone();

        if lhs_bits > rhs_bits {
            expression = sway::Expression::create_function_call(
                format!("u{rhs_bits}::try_from").as_str(),
                None,
                vec![sway::Expression::from(sway::MemberAccess {
                    expression: expression.clone(),
                    member: "underlying".to_string(),
                })],
            )
            .with_unwrap_call();
        } else if lhs_bits < rhs_bits {
            expression = expression.with_member("underlying").with_function_call(
                format!("as_u{rhs_bits}").as_str(),
                None,
                vec![],
            );
        } else {
            expression = expression.with_member("underlying");
        }

        return Some(expression);
    }

    // Check for uint/int coercions of different bit lengths
    if (context.from_type_name.is_uint() && context.to_type_name.is_uint())
        || (context.from_type_name.is_int() && context.to_type_name.is_int())
    {
        let lhs_bits: usize = context
            .from_type_name
            .to_string()
            .trim_start_matches('u')
            .trim_start_matches('U')
            .trim_start_matches('I')
            .parse()
            .unwrap();

        let rhs_bits: usize = context
            .to_type_name
            .to_string()
            .trim_start_matches('u')
            .trim_start_matches('U')
            .trim_start_matches('I')
            .parse()
            .unwrap();

        match &context.expression {
            sway::Expression::Literal(sway::Literal::DecInt(i, suffix)) => {
                if suffix.is_none() {
                    return Some(sway::Expression::Literal(sway::Literal::DecInt(
                        i.clone(),
                        Some(format!(
                            "{}{}",
                            context.to_type_name.to_string().chars().next().unwrap(),
                            rhs_bits
                        )),
                    )));
                }

                if lhs_bits > rhs_bits {
                    // x.as_u256()
                    // u64::try_from(x).unwrap()
                    return Some(
                        sway::Expression::create_function_call(
                            format!("{}::try_from", context.to_type_name).as_str(),
                            None,
                            vec![context.expression.clone()],
                        )
                        .with_unwrap_call(),
                    );
                }

                if lhs_bits < rhs_bits {
                    return Some(context.expression.with_function_call(
                        format!("as_{}", context.to_type_name).as_str(),
                        None,
                        vec![],
                    ));
                }
            }

            _ => {
                if lhs_bits > rhs_bits {
                    // x.as_u256()
                    // u64::try_from(x).unwrap()
                    return Some(
                        sway::Expression::create_function_call(
                            format!("{}::try_from", context.to_type_name).as_str(),
                            None,
                            vec![context.expression.clone()],
                        )
                        .with_unwrap_call(),
                    );
                }

                if lhs_bits < rhs_bits {
                    return Some(context.expression.with_function_call(
                        format!("as_{}", context.to_type_name).as_str(),
                        None,
                        vec![],
                    ));
                }
            }
        }
    }

    // Check for uint to `b256` coercions
    if context.from_type_name.is_uint() && context.to_type_name.is_b256() {
        // Remove suffix from explicitly tagged `u256` literals
        if let sway::Expression::Literal(sway::Literal::DecInt(value, _) | sway::Literal::HexInt(value, _)) =
            &context.expression
        {
            return Some(sway::Expression::from(sway::Literal::HexInt(
                value.clone(),
                Some("b256".to_string()),
            )));
        }

        let mut expression = context.expression.clone();

        expression = coerce_expression(
            context.project,
            context.module.clone(),
            context.scope.clone(),
            &expression,
            &context.from_type_name,
            &sway::TypeName::create_identifier("u256"),
        )
        .unwrap();

        return Some(expression.with_as_b256_call());
    }

    // Check for b256 to `u256` coercions
    if context.from_type_name.is_b256() && context.to_type_name.is_u256() {
        return Some(context.expression.with_as_u256_call());
    }

    // Check for uint to `Bytes` coercions
    if (context.from_type_name.is_uint() || context.from_type_name.is_b256()) && context.to_type_name.is_bytes() {
        if context.from_type_name.is_u8() {
            todo!()
        }

        return Some(context.expression.with_to_be_bytes_call());
    }

    // Check for `str` to `u8` coercions
    if context.from_type_name.is_string_slice() && context.to_type_name.is_u8() {
        context.module.borrow_mut().ensure_use_declared("std::string::*");

        // String::from_ascii_str(input).as_bytes().get(0).unwrap()
        return Some(
            coerce_expression(
                context.project,
                context.module.clone(),
                context.scope.clone(),
                &sway::Expression::create_function_call(
                    "String::from_ascii_str",
                    None,
                    vec![context.expression.clone()],
                )
                .with_as_bytes_call()
                .with_get_call(sway::Expression::from(sway::Literal::DecInt(0u8.into(), None)))
                .with_unwrap_or_call(sway::Expression::from(sway::Literal::DecInt(0u8.into(), None))),
                &sway::TypeName::create_identifier("Bytes"),
                &context.to_type_name,
            )
            .unwrap(),
        );
    }

    // Check for `[u8; N]` to `u8` coercions
    if context.from_type_name.is_u8_array() && context.to_type_name.is_u8() {
        return Some(sway::Expression::from(sway::ArrayAccess {
            expression: context.expression.clone(),
            index: sway::Expression::from(sway::Literal::DecInt(0u8.into(), None)),
        }));
    }

    // Check for `[u8; N]` to uint coercions
    if let Some(u8_array_length) = context.from_type_name.u8_array_length()
        && let Some(uint_bits) = context.to_type_name.uint_bits()
    {
        let uint_byte_count = uint_bits / 8;

        let missing = if u8_array_length < uint_byte_count {
            uint_byte_count - u8_array_length
        } else {
            0
        };

        // {
        //     let a = x;
        //     let mut b = Bytes::new();
        //     let mut i = 0;
        //     while i < u8_array_length {
        //         b.push(a[i]);
        //         i += 1;
        //     }
        //     i = 0;
        //     while i < missing {
        //         b.push(0);
        //         i += 1;
        //     }
        //     uN::from_be_bytes(b)
        // }

        context
            .module
            .borrow_mut()
            .ensure_use_declared(format!("std::bytes_conversions::u{uint_bits}::*").as_str());

        let variable_name1 = context.scope.borrow_mut().generate_unique_variable_name("a");
        let variable_name2 = context.scope.borrow_mut().generate_unique_variable_name("b");
        let variable_name3 = context.scope.borrow_mut().generate_unique_variable_name("i");

        return Some(sway::Expression::from(sway::Block {
            statements: vec![
                // let a = x;
                sway::Statement::from(sway::Let {
                    pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                        is_mutable: false,
                        name: variable_name1.clone(),
                    }),
                    type_name: None,
                    value: context.expression.clone(),
                }),
                // let mut b = Bytes::new();
                sway::Statement::from(sway::Let {
                    pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                        is_mutable: true,
                        name: variable_name2.clone(),
                    }),
                    type_name: None,
                    value: sway::Expression::create_function_call("Bytes::new", None, vec![]),
                }),
                // let mut i = 0;
                sway::Statement::from(sway::Let {
                    pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                        is_mutable: true,
                        name: variable_name3.clone(),
                    }),
                    type_name: None,
                    value: sway::Expression::from(sway::Literal::DecInt(0u8.into(), None)),
                }),
                // while i < u8_array_length
                sway::Statement::from(sway::Expression::from(sway::While {
                    // i < u8_array_length
                    condition: sway::Expression::from(sway::BinaryExpression {
                        operator: "<".into(),
                        lhs: sway::Expression::create_identifier(&variable_name3),
                        rhs: sway::Expression::from(sway::Literal::DecInt(u8_array_length.into(), None)),
                    }),
                    body: sway::Block {
                        statements: vec![
                            // b.push(a[i]);
                            sway::Statement::from(sway::Expression::create_identifier(&variable_name2).with_push_call(
                                sway::Expression::from(sway::ArrayAccess {
                                    expression: sway::Expression::create_identifier(&variable_name1),
                                    index: sway::Expression::create_identifier(&variable_name3),
                                }),
                            )),
                            // i += 1;
                            sway::Statement::from(sway::Expression::from(sway::BinaryExpression {
                                operator: "+=".into(),
                                lhs: sway::Expression::create_identifier(&variable_name3),
                                rhs: sway::Expression::from(sway::Literal::DecInt(1u8.into(), None)),
                            })),
                        ],
                        final_expr: None,
                    },
                })),
                // i = 0;
                sway::Statement::from(sway::Expression::from(sway::BinaryExpression {
                    operator: "=".into(),
                    lhs: sway::Expression::create_identifier(&variable_name3),
                    rhs: sway::Expression::from(sway::Literal::DecInt(0u8.into(), None)),
                })),
                // while i < missing
                sway::Statement::from(sway::Expression::from(sway::While {
                    // i < missing
                    condition: sway::Expression::from(sway::BinaryExpression {
                        operator: "<".into(),
                        lhs: sway::Expression::create_identifier(&variable_name3),
                        rhs: sway::Expression::from(sway::Literal::DecInt(missing.into(), None)),
                    }),
                    body: sway::Block {
                        statements: vec![
                            // b.push(0);
                            sway::Statement::from(
                                sway::Expression::create_identifier(&variable_name2)
                                    .with_push_call(sway::Expression::from(sway::Literal::DecInt(0u8.into(), None))),
                            ),
                            // i += 1;
                            sway::Statement::from(sway::Expression::from(sway::BinaryExpression {
                                operator: "+=".into(),
                                lhs: sway::Expression::create_identifier(&variable_name3),
                                rhs: sway::Expression::from(sway::Literal::DecInt(1u8.into(), None)),
                            })),
                        ],
                        final_expr: None,
                    },
                })),
            ],
            // uN::from_be_bytes(b)
            final_expr: Some(sway::Expression::create_function_call(
                format!("u{uint_bits}::from_be_bytes").as_str(),
                None,
                vec![sway::Expression::create_identifier(&variable_name2)],
            )),
        }));
    }

    // Check for `Bytes` to `u8` coercions
    if context.from_type_name.is_bytes() && context.to_type_name.is_u8() {
        return Some(
            context
                .expression
                .with_get_call(sway::Expression::from(sway::Literal::DecInt(0u8.into(), None)))
                .with_unwrap_or_call(sway::Expression::from(sway::Literal::DecInt(0u8.into(), None))),
        );
    }

    // Check for `Bytes` to `b256` coercions
    if context.from_type_name.is_bytes() && context.to_type_name.is_b256() {
        context
            .module
            .borrow_mut()
            .ensure_use_declared("std::bytes_conversions::b256::*");

        return Some(sway::Expression::create_function_call(
            "b256::from_be_bytes",
            None,
            vec![context.expression.clone()],
        ));
    }

    // Check for `raw_slice` to `b256` coercions
    if context.from_type_name.is_raw_slice() && context.to_type_name.is_b256() {
        // {
        //     let mut b = Bytes::from(x);
        //     while b.len() < 32 {
        //         b.push(0);
        //     }
        //     while b.len() > 32 {
        //         b.pop();
        //     }
        //     b256::from_be_bytes(b)
        // }

        context
            .module
            .borrow_mut()
            .ensure_use_declared(format!("std::bytes_conversions::b256::*").as_str());

        let variable_name = context.scope.borrow_mut().generate_unique_variable_name("b");

        return Some(sway::Expression::from(sway::Block {
            statements: vec![
                // let mut b = Bytes::from(x);
                sway::Statement::from(sway::Let {
                    pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                        is_mutable: true,
                        name: variable_name.clone(),
                    }),
                    type_name: None,
                    value: sway::Expression::create_function_call(
                        "Bytes::from",
                        None,
                        vec![context.expression.clone()],
                    ),
                }),
                // while b.len() < 32
                sway::Statement::from(sway::Expression::from(sway::While {
                    // b.len() < 32
                    condition: sway::Expression::from(sway::BinaryExpression {
                        operator: "<".into(),
                        lhs: sway::Expression::create_identifier(&variable_name).with_len_call(),
                        rhs: sway::Expression::from(sway::Literal::DecInt(32u8.into(), None)),
                    }),
                    body: sway::Block {
                        statements: vec![
                            // b.push(0);
                            sway::Statement::from(
                                sway::Expression::create_identifier(&variable_name)
                                    .with_push_call(sway::Expression::from(sway::Literal::DecInt(0u8.into(), None))),
                            ),
                        ],
                        final_expr: None,
                    },
                })),
                // while b.len() > 32
                sway::Statement::from(sway::Expression::from(sway::While {
                    // b.len() > 32
                    condition: sway::Expression::from(sway::BinaryExpression {
                        operator: ">".into(),
                        lhs: sway::Expression::create_identifier(&variable_name).with_len_call(),
                        rhs: sway::Expression::from(sway::Literal::DecInt(32u8.into(), None)),
                    }),
                    body: sway::Block {
                        statements: vec![
                            // b.pop();
                            sway::Statement::from(sway::Expression::create_identifier(&variable_name).with_pop_call()),
                        ],
                        final_expr: None,
                    },
                })),
            ],
            // b256::from_be_bytes(b)
            final_expr: Some(sway::Expression::create_function_call(
                format!("b256::from_be_bytes").as_str(),
                None,
                vec![sway::Expression::create_identifier(&variable_name)],
            )),
        }));
    }

    // Check for `[u8; 32]` to `b256` coercions
    if let Some(length) = context.from_type_name.u8_array_length()
        && length == 32
        && context.to_type_name.is_b256()
    {
        return Some(sway::Expression::create_function_call(
            "b256::from_be_bytes",
            None,
            vec![
                coerce_expression(
                    context.project,
                    context.module.clone(),
                    context.scope.clone(),
                    &context.expression,
                    &context.from_type_name,
                    &sway::TypeName::create_identifier("Bytes"),
                )
                .unwrap(),
            ],
        ));
    }

    // Check for `[u8; N]` to `b256` coercions
    if context.from_type_name.is_u8_array() && context.to_type_name.is_b256() {
        // `[u8; N]` -> `[u8; 32]`
        let expression = coerce_expression(
            context.project,
            context.module.clone(),
            context.scope.clone(),
            &context.expression,
            &context.from_type_name,
            &sway::TypeName::create_array(sway::TypeName::create_identifier("u8"), 32),
        )
        .unwrap();

        return Some(sway::Expression::create_function_call(
            "b256::from_be_bytes",
            None,
            vec![
                coerce_expression(
                    context.project,
                    context.module.clone(),
                    context.scope.clone(),
                    &expression,
                    &context.from_type_name,
                    &sway::TypeName::create_identifier("Bytes"),
                )
                .unwrap(),
            ],
        ));
    }

    None
}

/// Coerce to `[u8; N]` arrays
fn coerce_to_u8_arrays(context: &mut CoerceContext) -> Option<sway::Expression> {
    // Check for `[u8; N]` coercions
    let Some(to_byte_count) = context.to_type_name.u8_array_length() else {
        return None;
    };

    // Check for `[u8; N]` to `[u8; N]` coercions
    if let Some(from_byte_count) = context.from_type_name.u8_array_length() {
        if from_byte_count == to_byte_count {
            return Some(context.expression.clone());
        }

        let variable_name = context.scope.borrow_mut().generate_unique_variable_name("x");

        return Some(sway::Expression::from(sway::Block {
            statements: vec![sway::Statement::from(sway::Let {
                pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                    is_mutable: false,
                    name: variable_name.clone(),
                }),
                type_name: None,
                value: coerce_expression(
                    context.project,
                    context.module.clone(),
                    context.scope.clone(),
                    &context.expression,
                    &context.from_type_name,
                    &sway::TypeName::create_identifier("Bytes"),
                )
                .unwrap(),
            })],
            final_expr: Some(sway::Expression::from(sway::Array {
                elements: (0..to_byte_count)
                    .map(|i| {
                        sway::Expression::create_identifier(&variable_name)
                            .with_get_call(sway::Expression::from(sway::Literal::DecInt((i as u32).into(), None)))
                            .with_unwrap_or_call(sway::Expression::from(sway::Literal::DecInt(0u8.into(), None)))
                    })
                    .collect(),
            })),
        }));
    }

    // Check for uint to `[u8; N]` coercions
    let mut from_bits = 0;
    let mut expression = context.expression.clone();
    let mut from_type_name = context.from_type_name.clone();

    if let Some(bits) = from_type_name.uint_bits() {
        from_bits = bits;
    } else if from_type_name.is_b256() {
        from_bits = 32;
        expression = expression.with_as_u256_call();
        from_type_name = sway::TypeName::create_identifier("u256");
    }

    if from_bits != 0 {
        let from_byte_count = from_bits / 8;
        let to_bits = match to_byte_count * 8 {
            0..=8 => 8,
            9..=16 => 16,
            17..=32 => 32,
            33..=64 => 64,
            65..=128 => 128,
            129..=256 => 256,
            _ => panic!("Unsupported uint bit size: {from_bits}"),
        };
        let to_byte_count = to_bits / 8;

        if from_byte_count == to_byte_count {
            return Some(expression.with_to_be_bytes_call());
        } else if from_byte_count < to_byte_count {
            return Some(
                expression
                    .with_function_call(format!("as_u{to_bits}").as_str(), None, vec![])
                    .with_to_be_bytes_call(),
            );
        } else if from_byte_count > to_byte_count {
            if to_byte_count == 1 {
                return Some(sway::Expression::Array(sway::Array {
                    elements: vec![
                        expression
                            .with_function_call(format!("try_as_u{to_bits}").as_str(), None, vec![])
                            .with_unwrap_call(),
                    ],
                }));
            }

            return Some(
                expression
                    .with_function_call(format!("try_as_u{to_bits}").as_str(), None, vec![])
                    .with_unwrap_call()
                    .with_to_be_bytes_call(),
            );
        }
    }

    // Check for `Identity` to `[u8; N]` coercions
    if from_type_name.is_identity() && to_byte_count <= 32 {
        return Some(sway::Expression::from(sway::Block {
            statements: vec![sway::Statement::from(sway::Let {
                pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                    is_mutable: false,
                    name: "x".into(),
                }),
                type_name: None,
                value: sway::Expression::create_function_call("Bytes::from", None, vec![expression.with_bits_call()]),
            })],
            final_expr: Some(sway::Expression::from(sway::Array {
                elements: (0..to_byte_count)
                    .map(|i| {
                        sway::Expression::create_identifier("x")
                            .with_get_call(sway::Expression::from(sway::Literal::DecInt(i.into(), None)))
                            .with_unwrap_call()
                    })
                    .collect(),
            })),
        }));
    }

    // Check for `raw_slice` to `[u8; N]` coercions
    if from_type_name.is_raw_slice() {
        // {
        //     let x = Bytes::from(slice);
        //     [x.get(0).unwrap(), x.get(1).unwrap(), ...]
        // }
        let variable_name = context.scope.borrow_mut().generate_unique_variable_name("x");

        return Some(sway::Expression::from(sway::Block {
            statements: vec![sway::Statement::from(sway::Let {
                pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                    is_mutable: false,
                    name: variable_name.clone(),
                }),
                type_name: None,
                value: sway::Expression::create_function_call("Bytes::from", None, vec![expression]),
            })],
            final_expr: Some(sway::Expression::from(sway::Array {
                elements: (0..20)
                    .map(|i| {
                        let i: u32 = i.try_into().unwrap();
                        sway::Expression::create_identifier(variable_name.as_str())
                            .with_get_call(sway::Expression::from(sway::Literal::DecInt(i.into(), None)))
                            .with_unwrap_call()
                    })
                    .collect(),
            })),
        }));
    }

    // Check for `str` to `[u8; N]` coercions
    if from_type_name.is_string_slice() && context.to_type_name.is_u8_array() {
        context.module.borrow_mut().ensure_use_declared("std::string::*");

        // String::from_ascii_str(input).as_bytes()
        return Some(
            coerce_expression(
                context.project,
                context.module.clone(),
                context.scope.clone(),
                &sway::Expression::create_function_call("String::from_ascii_str", None, vec![expression.clone()])
                    .with_as_bytes_call(),
                &sway::TypeName::create_identifier("Bytes"),
                &context.to_type_name,
            )
            .unwrap(),
        );
    }

    // Check for `Bytes` to `[u8; N]` coercions
    if from_type_name.is_bytes()
        && let Some(to_u8_array_length) = context.to_type_name.u8_array_length()
    {
        let variable_name = context.scope.borrow_mut().generate_unique_variable_name("x");

        return Some(sway::Expression::from(sway::Block {
            statements: vec![sway::Statement::from(sway::Let {
                pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                    is_mutable: false,
                    name: variable_name.clone(),
                }),
                type_name: None,
                value: expression,
            })],
            final_expr: Some(sway::Expression::from(sway::Array {
                elements: (0..to_u8_array_length)
                    .map(|i| {
                        sway::Expression::create_identifier(&variable_name)
                            .with_get_call(sway::Expression::from(sway::Literal::DecInt((i as u32).into(), None)))
                            .with_unwrap_or_call(sway::Expression::from(sway::Literal::DecInt(0u8.into(), None)))
                    })
                    .collect(),
            })),
        }));
    }

    None
}

/// Coerce to `Bytes`
fn coerce_to_bytes(context: &mut CoerceContext) -> Option<sway::Expression> {
    // Check for `String` to `Bytes` coercions
    if context.from_type_name.is_string() && context.to_type_name.is_bytes() {
        return Some(context.expression.with_as_bytes_call());
    }

    // Check for `str` to `Bytes` coercions
    if context.from_type_name.is_string_slice() && context.to_type_name.is_bytes() {
        context.module.borrow_mut().ensure_use_declared("std::string::*");

        // String::from_ascii_str(input).as_bytes()
        return Some(
            sway::Expression::create_function_call("String::from_ascii_str", None, vec![context.expression.clone()])
                .with_as_bytes_call(),
        );
    }

    // Check for `[u8; N]` to `Bytes` coercions
    if let Some(from_u8_array_length) = context.from_type_name.u8_array_length()
        && context.to_type_name.is_bytes()
    {
        // {
        //     let a = expressioncontext.;
        //     let mut x = Bytes::new();
        //     x.push(e[N]);
        //     ...
        //     x
        // }

        let expression_name = context.scope.borrow_mut().generate_unique_variable_name("a");
        let variable_name = context.scope.borrow_mut().generate_unique_variable_name("x");

        let mut block = sway::Block {
            statements: vec![
                sway::Statement::from(sway::Let {
                    pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                        is_mutable: false,
                        name: expression_name.clone(),
                    }),
                    type_name: None,
                    value: context.expression.clone(),
                }),
                sway::Statement::from(sway::Let {
                    pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                        is_mutable: true,
                        name: variable_name.clone(),
                    }),
                    type_name: None,
                    value: sway::Expression::create_function_call("Bytes::new", None, vec![]),
                }),
            ],
            final_expr: Some(sway::Expression::create_identifier(variable_name.as_str())),
        };

        block.statements.extend((0..from_u8_array_length).map(|i| {
            sway::Statement::from(sway::Expression::create_identifier(&variable_name).with_push_call(
                sway::Expression::from(sway::ArrayAccess {
                    expression: sway::Expression::create_identifier(&expression_name),
                    index: sway::Expression::from(sway::Literal::DecInt((i as u32).into(), None)),
                }),
            ))
        }));

        return Some(sway::Expression::from(block));
    }

    // Check for `raw_slice` to `Bytes` coercions
    if context.from_type_name.is_raw_slice() && context.to_type_name.is_bytes() {
        return Some(sway::Expression::create_function_call(
            "Bytes::from",
            None,
            vec![context.expression.clone()],
        ));
    }

    // Check for `str[n]` to `Bytes` coercions
    if context.from_type_name.is_string_array() && context.to_type_name.is_bytes() {
        context.module.borrow_mut().ensure_use_declared("std::string::*");

        // String::from_ascii_str(from_str_array(input)).as_bytes()
        return Some(
            sway::Expression::create_function_call(
                "String::from_ascii_str",
                None,
                vec![sway::Expression::create_function_call(
                    "from_str_array",
                    None,
                    vec![context.expression.clone()],
                )],
            )
            .with_as_bytes_call(),
        );
    }

    None
}

/// Coerce to `str`
fn coerce_to_string_slice(context: &mut CoerceContext) -> Option<sway::Expression> {
    // Check for `String` to `str` coercions
    if context.from_type_name.is_string() && context.to_type_name.is_string_slice() {
        return Some(context.expression.with_as_str_call());
    }

    // Check for `Bytes` to `str` coercions
    if context.from_type_name.is_bytes() && context.to_type_name.is_string_slice() {
        context.module.borrow_mut().ensure_use_declared("std::string::*");

        return Some(
            sway::Expression::create_function_call("String::from_ascii", None, vec![context.expression.clone()])
                .with_as_str_call(),
        );
    }

    None
}

/// Coerce to `String`
fn coerce_to_string(context: &mut CoerceContext) -> Option<sway::Expression> {
    // Check for `str` to `String` coercions
    if context.from_type_name.is_string_slice() && context.to_type_name.is_string() {
        context.module.borrow_mut().ensure_use_declared("std::string::*");

        // String::from_ascii_str(x)
        return Some(sway::Expression::create_function_call(
            "String::from_ascii_str",
            None,
            vec![context.expression.clone()],
        ));
    }

    // Check for `str[]` to `String` coercions
    if context.from_type_name.is_string_array() && context.to_type_name.is_string() {
        context.module.borrow_mut().ensure_use_declared("std::string::*");

        // String::from_ascii_str(from_str_array(x))
        return Some(sway::Expression::create_function_call(
            "String::from_ascii_str",
            None,
            vec![sway::Expression::create_function_call(
                "from_str_array",
                None,
                vec![context.expression.clone()],
            )],
        ));
    }

    // Check for `Bytes` to `String` coercions
    if context.from_type_name.is_bytes() && context.to_type_name.is_string() {
        context.module.borrow_mut().ensure_use_declared("std::string::*");

        return Some(sway::Expression::create_function_call(
            "String::from_ascii",
            None,
            vec![context.expression.clone()],
        ));
    }

    None
}

/// Coerce to vec
fn coerce_to_vec(context: &mut CoerceContext) -> Option<sway::Expression> {
    let Some(from_vec_type) = context.from_type_name.vec_type() else {
        return None;
    };

    let Some(to_vec_type) = context.to_type_name.vec_type() else {
        return None;
    };

    if from_vec_type.is_compatible_with(&to_vec_type) {
        return None;
    }

    // {
    //     let a = x;
    //     let mut b: Vec<B> = Vec::new();
    //     let mut i = 0;
    //     while i < a.len() {
    //         b.push(coerce(a.get(i).unwrap()));
    //         i += 1;
    //     }
    //     b
    // }

    let variable_name1 = context.scope.borrow_mut().generate_unique_variable_name("a");
    let variable_name2 = context.scope.borrow_mut().generate_unique_variable_name("b");
    let variable_name3 = context.scope.borrow_mut().generate_unique_variable_name("i");

    Some(sway::Expression::from(sway::Block {
        statements: vec![
            sway::Statement::from(sway::Let {
                pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                    is_mutable: false,
                    name: variable_name1.clone(),
                }),
                type_name: None,
                value: context.expression.clone(),
            }),
            sway::Statement::from(sway::Let {
                pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                    is_mutable: true,
                    name: variable_name2.clone(),
                }),
                type_name: Some(context.to_type_name.clone()),
                value: sway::Expression::create_function_call("Vec::new", None, vec![]),
            }),
            sway::Statement::from(sway::Let {
                pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                    is_mutable: true,
                    name: variable_name3.clone(),
                }),
                type_name: Some(context.to_type_name.clone()),
                value: sway::Expression::from(sway::Literal::DecInt(0u8.into(), None)),
            }),
            sway::Statement::from(sway::Expression::from(sway::While {
                condition: sway::Expression::from(sway::BinaryExpression {
                    operator: "<".into(),
                    lhs: sway::Expression::create_identifier(&variable_name3),
                    rhs: sway::Expression::create_identifier(&variable_name1).with_len_call(),
                }),
                body: sway::Block {
                    statements: vec![sway::Statement::from(
                        sway::Expression::create_identifier(&variable_name2).with_push_call(
                            coerce_expression(
                                context.project,
                                context.module.clone(),
                                context.scope.clone(),
                                &sway::Expression::create_identifier(&variable_name1)
                                    .with_get_call(sway::Expression::create_identifier(&variable_name3))
                                    .with_unwrap_call(),
                                &from_vec_type,
                                &to_vec_type,
                            )
                            .unwrap(),
                        ),
                    )],
                    final_expr: None,
                },
            })),
        ],
        final_expr: Some(sway::Expression::create_identifier(&variable_name2)),
    }))
}

/// Coerce array types
fn coerce_array_types(context: &mut CoerceContext) -> Option<sway::Expression> {
    let (Some((lhs_type_name, lhs_len)), Some((rhs_type_name, rhs_len))) =
        (context.from_type_name.array_info(), context.to_type_name.array_info())
    else {
        return None;
    };

    if lhs_len < rhs_len {
        return None;
    }

    if let sway::Expression::Array(array) = &context.expression {
        return Some(sway::Expression::from(sway::Array {
            elements: array
                .elements
                .iter()
                .map(|e| {
                    coerce_expression(
                        context.project,
                        context.module.clone(),
                        context.scope.clone(),
                        e,
                        &lhs_type_name,
                        &rhs_type_name,
                    )
                    .unwrap()
                })
                .collect(),
        }));
    }

    Some(sway::Expression::from(sway::Array {
        elements: (0..rhs_len)
            .map(|i| {
                coerce_expression(
                    context.project,
                    context.module.clone(),
                    context.scope.clone(),
                    &sway::Expression::from(sway::ArrayAccess {
                        expression: context.expression.clone(),
                        index: sway::Expression::from(sway::Literal::DecInt(i.into(), None)),
                    }),
                    &lhs_type_name,
                    &rhs_type_name,
                )
                .unwrap()
            })
            .collect(),
    }))
}

/// Coerce tuple types
fn coerce_tuple_types(context: &mut CoerceContext) -> Option<sway::Expression> {
    let (Some(lhs_type_names), Some(rhs_type_names)) = (
        context.from_type_name.tuple_type_names(),
        context.to_type_name.tuple_type_names(),
    ) else {
        return None;
    };

    match &context.expression {
        sway::Expression::Tuple(expressions) => {
            let mut expressions = expressions.clone();

            if expressions.len() != rhs_type_names.len() {
                return None;
            }

            for (i, (lhs, rhs)) in lhs_type_names.iter().zip(rhs_type_names.iter()).enumerate() {
                match coerce_expression(
                    context.project,
                    context.module.clone(),
                    context.scope.clone(),
                    &expressions[i],
                    lhs,
                    rhs,
                ) {
                    Some(expr) => expressions[i] = expr,
                    None => return None,
                }
            }

            Some(sway::Expression::Tuple(expressions))
        }

        _ => {
            let component_names = ('a'..='z')
                .enumerate()
                .take_while(|(i, _)| *i < lhs_type_names.len())
                .map(|(_, c)| sway::LetIdentifier {
                    is_mutable: false,
                    name: c.to_string(),
                })
                .collect::<Vec<_>>();

            let let_stmt = sway::Statement::from(sway::Let {
                pattern: sway::LetPattern::Tuple(component_names.clone()),
                type_name: None,
                value: context.expression.clone(),
            });

            let exprs = component_names
                .iter()
                .enumerate()
                .map(|(i, c)| {
                    let expr = sway::Expression::create_identifier(c.name.as_str());
                    coerce_expression(
                        context.project,
                        context.module.clone(),
                        context.scope.clone(),
                        &expr,
                        &lhs_type_names[i],
                        &rhs_type_names[i],
                    )
                })
                .collect::<Vec<_>>();

            if exprs.iter().any(|x| x.is_none()) {
                return None;
            }

            Some(sway::Expression::from(sway::Block {
                statements: vec![let_stmt],
                final_expr: Some(sway::Expression::Tuple(exprs.iter().flatten().cloned().collect())),
            }))
        }
    }
}

/// Check for storage struct inheritance coercions
fn coerce_contract_storage_struct_types(context: &mut CoerceContext) -> Option<sway::Expression> {
    let Some(from_storage_struct) = context.project.find_struct(
        context.module.clone(),
        context.scope.clone(),
        context.from_type_name.to_string().as_str(),
    ) else {
        return None;
    };

    let to_contract_name = context.to_type_name.to_string().trim_end_matches("Storage").to_string();

    let Some(external_module) = context
        .project
        .find_module_containing_contract(context.module.clone(), to_contract_name.as_str())
    else {
        return None;
    };

    let external_scope = Rc::new(RefCell::new(ir::Scope::new(
        Some(&to_contract_name),
        None,
        Some(context.scope.clone()),
    )));

    let Some(to_storage_struct) = context.project.find_struct(
        external_module.clone(),
        external_scope.clone(),
        context.to_type_name.to_string().as_str(),
    ) else {
        return None;
    };

    let from_contract_name = from_storage_struct
        .borrow()
        .name
        .trim_end_matches("Storage")
        .to_string();

    let to_contract_name = to_storage_struct.borrow().name.trim_end_matches("Storage").to_string();

    get_inheritance_storage_struct_field(
        context.project,
        context.module.clone(),
        context.scope.clone(),
        &from_contract_name,
        &to_contract_name,
        context.expression.clone(),
    )
}

/// Get the storage struct field associated with an inherited contract
fn get_inheritance_storage_struct_field(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    from_contract_name: &str,
    to_contract_name: &str,
    expression: sway::Expression,
) -> Option<sway::Expression> {
    let Some(module) = project.find_module_containing_contract(module.clone(), from_contract_name) else {
        return None;
    };

    let scope = Rc::new(RefCell::new(ir::Scope::new(
        Some(from_contract_name),
        None,
        Some(scope.clone()),
    )));

    let Some(from_contract) = project.find_contract(module.clone(), &from_contract_name) else {
        return None;
    };

    for inherited_contract_name in from_contract.borrow().abi.inherits.clone() {
        if to_contract_name == inherited_contract_name.to_string() {
            return Some(sway::Expression::from(sway::MemberAccess {
                expression: expression.clone(),
                member: inherited_contract_name.to_string().to_case(Case::Snake),
            }));
        }

        if let Some(result) = get_inheritance_storage_struct_field(
            project,
            module.clone(),
            scope.clone(),
            inherited_contract_name.to_string().as_str(),
            to_contract_name,
            expression.clone(),
        ) {
            return Some(result);
        }
    }
    None
}
