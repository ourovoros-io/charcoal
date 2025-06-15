use crate::{error::Error, project::Project, sway, translate::*};
use solang_parser::helpers::CodeLocation;
use std::{cell::RefCell, rc::Rc};

mod builtins;
mod casting;
mod member_access;
mod utils;

pub use self::{builtins::*, casting::*, member_access::*, utils::*};

#[inline]
pub fn translate_function_call_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    expression: &solidity::Expression,
    function: &solidity::Expression,
    named_arguments: Option<&[solidity::NamedArgument]>,
    arguments: &[solidity::Expression],
) -> Result<sway::Expression, Error> {
    if named_arguments.is_some() && !arguments.is_empty() {
        panic!(
            "Invalid call to translate_function_call_expression: named_arguments is Some(_) and arguments is not empty"
        );
    }

    // println!(
    //     "Translating function call: {expression}; from {}",
    //     project.loc_to_file_location_string(module.clone(), &expression.loc()),
    // );

    match function {
        solidity::Expression::Type(_, ty) => {
            // Type casting
            if arguments.len() != 1 {
                panic!("Invalid type cast expression: {expression}");
            }

            match ty {
                solidity::Type::Address => translate_address_type_cast_function_call(
                    project,
                    module.clone(),
                    scope.clone(),
                    expression,
                    &arguments[0],
                ),

                solidity::Type::Payable => translate_payable_type_cast_function_call(
                    project,
                    module.clone(),
                    scope.clone(),
                    expression,
                    arguments,
                ),

                solidity::Type::Int(bits) => translate_int_types_cast_function_call(
                    project,
                    module.clone(),
                    scope.clone(),
                    expression,
                    &arguments[0],
                    *bits as usize,
                ),

                solidity::Type::Uint(bits) => translate_uint_types_cast_function_call(
                    project,
                    module.clone(),
                    scope.clone(),
                    expression,
                    &arguments[0],
                    *bits as usize,
                ),

                solidity::Type::Bytes(byte_count) => translate_bytes_type_cast_function_call(
                    project,
                    module.clone(),
                    scope.clone(),
                    &arguments[0],
                    *byte_count as usize,
                    function,
                ),

                solidity::Type::DynamicBytes => translate_dynamic_bytes_type_cast_function_call(
                    project,
                    module.clone(),
                    scope.clone(),
                    &arguments[0],
                ),

                solidity::Type::String => translate_string_type_cast_function_call(
                    project,
                    module.clone(),
                    scope.clone(),
                    expression,
                    &arguments[0],
                ),

                _ => todo!("translate type cast: {}", expression),
            }
        }

        solidity::Expression::Variable(solidity::Identifier { name, .. }) => {
            if name == "type" {
                return Ok(sway::Expression::create_todo(Some(
                    sway::TabbedDisplayer(&expression).to_string(),
                )));
            }

            let parameters = arguments
                .iter()
                .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
                .collect::<Result<Vec<_>, _>>()?;

            if let Some(expression) = translate_builtin_function_call(
                project,
                module.clone(),
                scope.clone(),
                name,
                expression,
                parameters.clone(),
            )? {
                return Ok(expression);
            }

            let parameter_types = parameters
                .iter()
                .map(|p| get_expression_type(project, module.clone(), scope.clone(), p))
                .collect::<Result<Vec<_>, _>>()?;

            // Check to see if the expression is a by-value struct constructor
            let structs = {
                let module = module.borrow();
                module.structs.clone()
            };

            if let Some(result) = resolve_struct_constructor(
                project,
                module.clone(),
                scope.clone(),
                structs.as_slice(),
                &name,
                named_arguments,
                parameters.clone(),
                parameter_types.clone(),
            )? {
                return Ok(result);
            }

            // Check to see if the expression is an ABI cast
            if parameters.len() == 1 {
                if let Some(_external_definition) = project.find_contract(module.clone(), name) {
                    match get_expression_type(
                        project,
                        module.clone(),
                        scope.clone(),
                        &parameters[0],
                    )? {
                        sway::TypeName::Identifier {
                            name: type_name,
                            generic_parameters,
                        } => match (type_name.as_str(), generic_parameters.as_ref()) {
                            ("Identity", None) => {
                                //
                                // TODO: Ensure a use statement for the ABI is added to the current module
                                //

                                // abi(T, x.as_contract_id().unwrap().into())
                                return Ok(sway::Expression::create_function_calls(
                                    None,
                                    &[(
                                        "abi",
                                        Some((
                                            None,
                                            vec![
                                                sway::Expression::create_identifier(name.into()),
                                                sway::Expression::create_function_calls(
                                                    Some(parameters[0].clone()),
                                                    &[
                                                        ("as_contract_id", Some((None, vec![]))),
                                                        ("unwrap", Some((None, vec![]))),
                                                        ("into", Some((None, vec![]))),
                                                    ],
                                                ),
                                            ],
                                        )),
                                    )],
                                ));
                            }

                            ("u256" | "b256", None) => {
                                // Thing(x) => abi(Thing, Identity::from(ContractId::from(x)))

                                //
                                // TODO Ensure a use statement for the ABI is added to the current module
                                //

                                // abi(T, Identity::from(ContractId::from(x)))
                                return Ok(sway::Expression::create_function_calls(None, &[
                                    ("abi", Some((None, vec![
                                        sway::Expression::create_identifier(name.into()),
                                        sway::Expression::create_function_calls(None, &[
                                            ("Identity::from", Some((None, vec![
                                                // ContractId::from(x)
                                                sway::Expression::create_function_calls(None, &[("ContractId::from", Some((None, vec![parameters[0].clone()])))]),
                                            ]))),
                                        ]),
                                    ]))),
                                ]));
                            }

                            _ => {}
                        },

                        _ => {}
                    }
                }
            }

            // Try to resolve the function call
            if let Some(result) = resolve_function_call(
                module.clone(),
                name,
                named_arguments,
                parameters.clone(),
                parameter_types.clone(),
            )? {
                return Ok(result);
            }

            // Check all of the module's `use` statements for crate-local imports,
            // find the module being imported, then check if the function lives there.
            for use_item in module.borrow().uses.iter() {
                if let Some(found_module) = project.resolve_use(use_item) {
                    // Try to resolve the function call
                    if let Some(result) = resolve_function_call(
                        found_module.clone(),
                        name,
                        named_arguments,
                        parameters.clone(),
                        parameter_types.clone(),
                    )? {
                        return Ok(result);
                    }
                }
            }

            panic!(
                "{}: ERROR: Failed to find function `{name}({})` in scope: {function}({})",
                project.loc_to_file_location_string(module.clone(), &function.loc()),
                parameter_types
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                parameters
                    .iter()
                    .map(|t| sway::TabbedDisplayer(t).to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
            )
        }

        solidity::Expression::MemberAccess(_, container, member) => {
            // Check for built-in type member access function calls
            if let solidity::Expression::Type(_, ty) = container.as_ref() {
                if let Some(result) = translate_builtin_type_member_access_function_call(
                    project,
                    module.clone(),
                    scope.clone(),
                    ty,
                    member.name.as_str(),
                    arguments,
                    named_arguments,
                )? {
                    return Ok(result);
                }
            }

            // Check for built-in variable member access function calls
            if let solidity::Expression::Variable(identifier) = container.as_ref() {
                if let Some(value) = translate_builtin_variable_member_access_function_call(
                    project,
                    module.clone(),
                    scope.clone(),
                    identifier.name.as_str(),
                    member.name.as_str(),
                    arguments,
                    named_arguments,
                )? {
                    return Ok(value);
                }
            }

            match container.as_ref() {
                solidity::Expression::Variable(solidity::Identifier { name, .. }) => {
                    let parameters = arguments
                        .iter()
                        .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
                        .collect::<Result<Vec<_>, _>>()?;

                    let parameter_types = parameters
                        .iter()
                        .map(|p| get_expression_type(project, module.clone(), scope.clone(), p))
                        .collect::<Result<Vec<_>, _>>()?;

                    // TODO: check full inheritance hierarchy

                    // Check for explicit super function calls
                    // if module.borrow().inherits.iter().any(|i| i == name) {
                    //     if let Some(inherited_definition) =
                    //         project.find_definition_with_abi(name).cloned()
                    //     {
                    //         if let Some(result) = resolve_function_call(
                    //             project,
                    //             module.clone(),
                    //             scope.clone(),
                    //             &inherited_definition.toplevel_scope,
                    //             member.name.as_str(),
                    //             named_arguments,
                    //             parameters.clone(),
                    //             parameter_types.clone(),
                    //         )? {
                    //             return Ok(result);
                    //         }
                    //     }
                    // }

                    // Check for user-defined type value wrapping
                    if let "wrap" | "unwrap" = member.name.as_str() {
                        if let Some(SymbolData::TypeDefinition(_)) = resolve_symbol(
                            project,
                            module.clone(),
                            scope.clone(),
                            Symbol::TypeDefinition(name.clone()),
                        ) {
                            return Ok(parameters[0].clone());
                        }
                    }

                    // Check if function is contained in an external definition
                    if let Some(external_module) =
                        project.find_module_with_contract(module.clone(), &name)
                    {
                        // Check to see if the expression is a by-value struct constructor
                        let structs = {
                            let module = external_module.borrow();
                            module.structs.clone()
                        };

                        if let Some(result) = resolve_struct_constructor(
                            project,
                            module.clone(),
                            scope.clone(),
                            structs.as_slice(),
                            member.name.as_str(),
                            named_arguments,
                            parameters.clone(),
                            parameter_types.clone(),
                        )? {
                            return Ok(result);
                        }

                        // Try to resolve the function call
                        if let Some(result) = resolve_function_call(
                            external_module.clone(),
                            member.name.as_str(),
                            named_arguments,
                            parameters.clone(),
                            parameter_types.clone(),
                        )? {
                            return Ok(result);
                        }
                    }

                    // TODO: is this still necessary?
                    // Check if function is contained in the current contract (self-referential: This.func())
                    // if name == module.borrow().name {
                    //     // Check to see if the expression is a by-value struct constructor
                    //     if let Some(result) = resolve_struct_constructor(
                    //         project,
                    //         module.clone(),
                    //         scope.clone(),
                    //         module.borrow().structs.clone().as_slice(),
                    //         member.name.as_str(),
                    //         named_arguments,
                    //         parameters.clone(),
                    //         parameter_types.clone(),
                    //     )? {
                    //         return Ok(result);
                    //     }
                    //
                    //     // Try to resolve the function call
                    //     if let Some(result) = resolve_function_call(
                    //         project,
                    //         module.clone(),
                    //         scope.clone(),
                    //         module.clone(),
                    //         member.name.as_str(),
                    //         named_arguments,
                    //         parameters.clone(),
                    //         parameter_types.clone(),
                    //     )? {
                    //         return Ok(result);
                    //     }
                    // }

                    let container =
                        translate_expression(project, module.clone(), scope.clone(), container)?;

                    let type_name =
                        get_expression_type(project, module.clone(), scope.clone(), &container)?;

                    let mut abi_type_name = None;

                    // Check to see if the container is a variable defined in scope
                    if let Some(variable) = scope
                        .borrow()
                        .find_variable(|v| v.borrow().old_name == *name)
                    {
                        abi_type_name = variable.borrow().abi_type_name.clone();
                    }

                    //
                    // TODO: Check to see if the container is a constant or configurable
                    //

                    // Check to see if the container is a storage field
                    if abi_type_name.is_none() {
                        let mut module = module.borrow_mut();

                        let storage_namespace =
                            module.get_storage_namespace(scope.clone()).unwrap();

                        if let Some(storage_field) = storage_namespace
                            .borrow()
                            .fields
                            .iter()
                            .find(|f| f.old_name == *name)
                        {
                            abi_type_name = storage_field.abi_type_name.clone();
                        }
                    }

                    // Check to see if the member function is defined in the ABI type
                    if let Some(abi_type_name) = abi_type_name.as_ref() {
                        let abi_type_name_string = abi_type_name.to_string();

                        if let Some(contract) =
                            project.find_contract(module.clone(), &abi_type_name_string)
                        {
                            let abi = contract.borrow().abi.clone();

                            if let Some(result) = resolve_abi_function_call(
                                project,
                                module.clone(),
                                scope.clone(),
                                &abi,
                                &container,
                                member.name.as_str(),
                                named_arguments,
                                parameters.clone(),
                                parameter_types.clone(),
                            )? {
                                return Ok(result);
                            }
                        }
                    }

                    // Check if the member call is an address-specific built-in function
                    if type_name.is_identity() {
                        match member.name.as_str() {
                            "call" if arguments.len() == 1 => {
                                let payload = translate_expression(
                                    project,
                                    module.clone(),
                                    scope.clone(),
                                    &arguments[0],
                                )?;
                                return translate_address_call_expression(
                                    project,
                                    module.clone(),
                                    scope.clone(),
                                    &payload,
                                    None,
                                    None,
                                    None,
                                );
                            }

                            "delegatecall" => {
                                //
                                // TODO: is delegatecall possible?
                                //

                                return Ok(sway::Expression::create_todo(Some(
                                    expression.to_string(),
                                )));
                            }

                            "staticcall" => {
                                //
                                // TODO: is staticcall possible?
                                //

                                return Ok(sway::Expression::create_todo(Some(
                                    expression.to_string(),
                                )));
                            }

                            _ => {}
                        }
                    }

                    // TODO
                    // Check to see if the function is from using library
                    // if let Some(f) =  module.borrow().using_directives.iter().find_map(|using| using.functions.iter().find(|fnc| fnc.old_name == member.name)) {
                    //     return Ok(sway::Expression::create_function_calls(Some(container), &[
                    //         (&f.new_name, Some((None, parameters)))
                    //     ]))
                    // }
                }

                _ => {}
            }

            let solidity_container = container;

            let mut container =
                translate_expression(project, module.clone(), scope.clone(), container)?;

            let mut type_name =
                get_expression_type(project, module.clone(), scope.clone(), &container)?;

            // println!(
            //     "type of {} is {}",
            //     sway::TabbedDisplayer(&container),
            //     sway::TabbedDisplayer(&type_name),
            // );

            // HACK: tack `.read()` onto the end if the container is a StorageKey
            if let Some(storage_key_type) = type_name.storage_key_type() {
                container = sway::Expression::create_function_calls(
                    Some(container),
                    &[("read", Some((None, vec![])))],
                );
                type_name = storage_key_type;
            }

            match &type_name {
                sway::TypeName::Undefined => panic!("Undefined type name"),

                sway::TypeName::Identifier {
                    name,
                    generic_parameters,
                } => {
                    match (name.as_str(), generic_parameters.as_ref()) {
                        ("Identity", None) => {
                            return translate_identity_member_access_function_call(
                                project,
                                module.clone(),
                                scope.clone(),
                                expression,
                                arguments,
                                container,
                                member,
                                solidity_container,
                                name.to_string(),
                                function,
                            );
                        }

                        ("StorageVec", Some(_)) => {
                            return translate_storage_vec_member_access_function_call(
                                project,
                                module.clone(),
                                scope.clone(),
                                arguments,
                                member,
                                solidity_container,
                                &container,
                            );
                        }

                        ("Vec", Some(_)) => {
                            return translate_vec_member_access_function_call(
                                project,
                                module.clone(),
                                scope.clone(),
                                arguments,
                                member,
                                solidity_container,
                                &container,
                            );
                        }

                        _ => {}
                    }

                    let parameters = arguments
                        .iter()
                        .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
                        .collect::<Result<Vec<_>, _>>()?;

                    let parameter_types = parameters
                        .iter()
                        .map(|p| get_expression_type(project, module.clone(), scope.clone(), p))
                        .collect::<Result<Vec<_>, _>>()?;

                    // Check if this is a function from a using directive
                    let mut using_parameters = parameters.clone();
                    using_parameters.insert(0, container.clone());

                    let mut using_parameter_types = parameter_types.clone();
                    using_parameter_types.insert(
                        0,
                        get_expression_type(project, module.clone(), scope.clone(), &container)?,
                    );

                    for using_directive in module.borrow().using_directives.clone() {
                        // Make sure the type names match
                        if let Some(for_type) = using_directive.for_type.as_ref() {
                            if *for_type != type_name {
                                continue;
                            }
                        }

                        // Look up the definition of the using directive
                        let Some(external_module) = project.find_module_with_contract(
                            module.clone(),
                            &using_directive.library_name,
                        ) else {
                            continue;
                        };

                        if let Some(result) = resolve_function_call(
                            external_module.clone(),
                            member.name.as_str(),
                            named_arguments,
                            using_parameters.clone(),
                            using_parameter_types.clone(),
                        )? {
                            return Ok(result);
                        }
                    }

                    // Check if this is a function from an ABI
                    for contract in module.borrow().contracts.clone() {
                        let abi = contract.borrow().abi.clone();

                        if let Some(result) = resolve_abi_function_call(
                            project,
                            module.clone(),
                            scope.clone(),
                            &abi,
                            &container,
                            member.name.as_str(),
                            named_arguments,
                            parameters.clone(),
                            parameter_types.clone(),
                        )? {
                            return Ok(result);
                        }
                    }

                    panic!(
                        "{}: TODO: translate {name} member function call: {}.{member}({})",
                        project.loc_to_file_location_string(module.clone(), &function.loc()),
                        sway::TabbedDisplayer(&container),
                        parameter_types
                            .iter()
                            .map(|t| t.to_string())
                            .collect::<Vec<_>>()
                            .join(", "),
                    )
                }

                sway::TypeName::Array { .. } => {
                    let mut parameters = arguments
                        .iter()
                        .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
                        .collect::<Result<Vec<_>, _>>()?;

                    let mut parameter_types = parameters
                        .iter()
                        .map(|p| get_expression_type(project, module.clone(), scope.clone(), p))
                        .collect::<Result<Vec<_>, _>>()?;

                    parameters.insert(0, container.clone());
                    parameter_types.insert(
                        0,
                        get_expression_type(project, module.clone(), scope.clone(), &container)?,
                    );

                    // Check if this is a function from a using directive
                    for using_directive in module.borrow().using_directives.clone() {
                        // Make sure the type names match
                        if let Some(for_type) = using_directive.for_type.as_ref() {
                            if *for_type != type_name {
                                continue;
                            }
                        }

                        // TODO
                        // Look up the definition of the using directive
                        // let external_scope = if matches!(module.borrow().kind, Some(solidity::ContractTy::Library(_))) && using_directive.library_name == module.borrow().name {
                        //     module.borrow().toplevel_scope.clone()
                        // } else {
                        //     match project.find_module_with_contract(module.clone(), &using_directive.library_name)
                        //     .map(|d| d.toplevel_scope.clone()) {
                        //         Some(s) => s,
                        //         None => continue,
                        //     }
                        // };

                        // TODO
                        // Try to resolve the function call
                        // if let Some(result) = resolve_function_call(
                        //     project,
                        //     module.clone(),
                        //     scope.clone(),
                        //     &external_scope,
                        //     member.name.as_str(),
                        //     named_arguments,
                        //     parameters.clone(),
                        //     parameter_types.clone(),
                        // )? {
                        //     return Ok(result);
                        // }
                    }

                    panic!(
                        "{}: TODO: translate array member function call: {} - {}",
                        project.loc_to_file_location_string(module.clone(), &function.loc()),
                        expression,
                        sway::TabbedDisplayer(&container),
                    )
                }

                sway::TypeName::Tuple { .. } => todo!(
                    "translate tuple member function call: {}",
                    sway::TabbedDisplayer(&container)
                ),

                sway::TypeName::StringSlice => {
                    let mut parameters = arguments
                        .iter()
                        .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
                        .collect::<Result<Vec<_>, _>>()?;

                    let mut parameter_types = parameters
                        .iter()
                        .map(|p| get_expression_type(project, module.clone(), scope.clone(), p))
                        .collect::<Result<Vec<_>, _>>()?;

                    parameters.insert(0, container.clone());
                    parameter_types.insert(
                        0,
                        get_expression_type(project, module.clone(), scope.clone(), &container)?,
                    );

                    // Check if this is a function from a using directive
                    for using_directive in module.borrow().using_directives.clone() {
                        // Make sure the type names match
                        if let Some(for_type) = using_directive.for_type.as_ref() {
                            if *for_type != type_name {
                                continue;
                            }
                        }

                        // TODO
                        // Look up the definition of the using directive
                        // let Some(external_scope) = project.find_module_with_contract(module.clone(), &using_directive.library_name)
                        //     .map(|d| d.toplevel_scope.clone())
                        // else { continue };

                        // TODO
                        // Try to resolve the function call
                        // if let Some(result) = resolve_function_call(
                        //     project,
                        //     module.clone(),
                        //     scope.clone(),
                        //     &external_scope,
                        //     member.name.as_str(),
                        //     named_arguments,
                        //     parameters.clone(),
                        //     parameter_types.clone(),
                        // )? {
                        //     return Ok(result);
                        // }
                    }

                    panic!(
                        "{}: TODO: translate string slice member function call: {function}({})",
                        project.loc_to_file_location_string(module.clone(), &function.loc()),
                        parameter_types
                            .iter()
                            .map(|t| t.to_string())
                            .collect::<Vec<_>>()
                            .join(", "),
                    )
                }

                sway::TypeName::StringArray { .. } => todo!(
                    "translate string array member function call: {}",
                    sway::TabbedDisplayer(&container)
                ),

                sway::TypeName::Function { .. } => todo!(
                    "translate fn member function call: {}",
                    sway::TabbedDisplayer(&container)
                ),
            }
        }

        solidity::Expression::FunctionCall(_, function, args) => {
            translate_member_access_function_call(
                project,
                module.clone(),
                scope.clone(),
                function,
                args,
                expression,
                arguments,
            )
        }

        solidity::Expression::FunctionCallBlock(_, function, block) => match function.as_ref() {
            solidity::Expression::MemberAccess(_, container, member) => {
                translate_function_call_block_member_access(
                    project,
                    module.clone(),
                    scope.clone(),
                    expression,
                    arguments,
                    container,
                    block,
                    member,
                )
            }

            _ => todo!("translate function call block expression: {expression}"),
        },

        _ => todo!("translate function call expression: {expression}"),
    }
}

#[inline]
pub fn translate_function_call_block_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    function: &solidity::Expression,
    block: &solidity::Statement,
) -> Result<sway::Expression, Error> {
    if block.is_empty() {
        return translate_expression(project, module.clone(), scope.clone(), function);
    }

    panic!(
        "{}: TODO: Translate function call block expression: `{function}{block}`",
        project.loc_to_file_location_string(module.clone(), &function.loc()),
    )
}

fn translate_builtin_type_member_access_function_call(
    _project: &mut Project,
    _module: Rc<RefCell<ir::Module>>,
    _scope: Rc<RefCell<ir::Scope>>,
    ty: &solidity::Type,
    member: &str,
    arguments: &[solidity::Expression],
    named_arguments: Option<&[solidity::NamedArgument]>,
) -> Result<Option<sway::Expression>, Error> {
    //
    // TODO: Check named_arguments
    //

    match ty {
        solidity::Type::String => match member {
            "concat" => {
                // string.concat(x) => ???

                //
                // TODO: how should this be handled?
                //

                Ok(Some(sway::Expression::create_todo(Some(format!(
                    "string.concat({})",
                    arguments
                        .iter()
                        .map(|p| p.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )))))
            }

            _ => Ok(None),
        },

        solidity::Type::DynamicBytes => match member {
            "concat" => {
                // bytes.concat(x) => ???

                //
                // TODO: how should this be handled?
                //

                Ok(Some(sway::Expression::create_todo(Some(format!(
                    "bytes.concat({})",
                    arguments
                        .iter()
                        .map(|p| p.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )))))
            }

            _ => Ok(None),
        },

        _ => Ok(None),
    }
}

fn translate_builtin_variable_member_access_function_call(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    name: &str,
    member: &str,
    arguments: &[solidity::Expression],
    named_arguments: Option<&[solidity::NamedArgument]>,
) -> Result<Option<sway::Expression>, Error> {
    match name {
        "abi" => translate_builtin_abi_member_access_function_call(
            project,
            module.clone(),
            scope.clone(),
            member,
            arguments,
            named_arguments,
        ),

        "super" => translate_super_member_access_function_call(
            project,
            module.clone(),
            scope.clone(),
            member,
            arguments,
            named_arguments,
        ),

        "this" => translate_this_member_access_function_call(
            project,
            module.clone(),
            scope.clone(),
            member,
            arguments,
            named_arguments,
        ),

        _ => Ok(None),
    }
}
