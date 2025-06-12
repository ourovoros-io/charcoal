use crate::{error::Error, project::Project, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_contract_definition(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    contract_definition: &solidity::ContractDefinition,
    contract: Rc<RefCell<ir::Contract>>,
) -> Result<(), Error> {
    // println!("Translating contract `{}`", contract_definition.name.as_ref().map(|x| x.name.as_str()).unwrap());

    // Collect each contract part into separate collections
    let mut using_directives = vec![];
    let mut type_definitions = vec![];
    let mut enum_definitions = vec![];
    let mut struct_definitions = vec![];
    let mut event_definitions = vec![];
    let mut error_definitions = vec![];
    let mut function_definitions = vec![];
    let mut variable_definitions = vec![];

    for part in contract_definition.parts.iter() {
        match part {
            solidity::ContractPart::StructDefinition(struct_definition) => {
                struct_definitions.push(struct_definition.clone())
            }

            solidity::ContractPart::EventDefinition(event_definition) => {
                event_definitions.push(event_definition.clone())
            }

            solidity::ContractPart::EnumDefinition(enum_definition) => {
                enum_definitions.push(enum_definition.clone())
            }

            solidity::ContractPart::ErrorDefinition(error_definition) => {
                error_definitions.push(error_definition.clone())
            }

            solidity::ContractPart::VariableDefinition(variable_definition) => {
                variable_definitions.push(variable_definition.clone())
            }

            solidity::ContractPart::FunctionDefinition(function_definition) => {
                function_definitions.push(function_definition.clone())
            }

            solidity::ContractPart::TypeDefinition(type_definition) => {
                type_definitions.push(type_definition.clone())
            }

            solidity::ContractPart::Annotation(_annotation) => {}

            solidity::ContractPart::Using(using_directive) => {
                using_directives.push(using_directive.clone())
            }

            solidity::ContractPart::StraySemicolon(_loc) => {}
        }
    }

    // Translate contract using directives
    let contract_name = contract.borrow().name.clone();

    for using_directive in using_directives {
        translate_using_directive(
            project,
            module.clone(),
            Some(&contract_name),
            &using_directive,
        )?;
    }

    // Collect the signatures of the contract type definitions
    let type_definitions_index = module.borrow().type_definitions.len();

    for type_definition in type_definitions.iter() {
        module.borrow_mut().type_definitions.push(ir::Item {
            signature: sway::TypeName::Identifier {
                name: type_definition.name.name.clone(),
                generic_parameters: None,
            },
            implementation: None,
        });
    }

    // Collect the signatures of the contract enum definitions
    let enums_index = module.borrow().enums.len();

    for enum_definition in enum_definitions.iter() {
        module.borrow_mut().enums.push(ir::Item {
            signature: sway::TypeName::Identifier {
                name: enum_definition.name.as_ref().unwrap().name.clone(),
                generic_parameters: None,
            },
            implementation: None,
        });
    }

    // Collect the signatures of the contract struct definitions
    let structs_index = module.borrow().structs.len();

    for struct_definition in struct_definitions.iter() {
        module.borrow_mut().structs.push(ir::Item {
            signature: sway::TypeName::Identifier {
                name: struct_definition.name.as_ref().unwrap().name.clone(),
                generic_parameters: None,
            },
            implementation: None,
        });
    }

    // Translate contract type definitions
    for (i, type_definition) in type_definitions.into_iter().enumerate() {
        module.borrow_mut().type_definitions[type_definitions_index + i].implementation =
            Some(translate_type_definition(
                project,
                module.clone(),
                Some(&contract_name),
                type_definition.as_ref(),
            )?);
    }

    // Translate contract enum definitions
    for (i, enum_definition) in enum_definitions.into_iter().enumerate() {
        module.borrow_mut().enums[enums_index + i].implementation = Some(
            translate_enum_definition(project, module.clone(), enum_definition.as_ref())?,
        );
    }

    // Translate contract struct definitions
    for (i, struct_definition) in struct_definitions.into_iter().enumerate() {
        module.borrow_mut().structs[structs_index + i].implementation =
            Some(translate_struct_definition(
                project,
                module.clone(),
                Some(&contract_name),
                struct_definition.as_ref(),
            )?);
    }

    // Translate contract event definitions
    for event_definition in event_definitions {
        translate_event_definition(
            project,
            module.clone(),
            Some(&contract_name),
            &event_definition,
        )?;
    }

    // Create the abi encoding function for the events enum (if any)
    let events_enum_name = format!("{}Event", module.borrow().name);

    if let Some((events_enum, abi_encode_impl)) = module
        .borrow()
        .events_enums
        .iter()
        .find(|(e, _)| e.borrow().name == events_enum_name)
        .cloned()
    {
        generate_enum_abi_encode_function(
            project,
            module.clone(),
            events_enum.clone(),
            abi_encode_impl.clone(),
        )?;
    }

    // Translate contract error definitions
    for error_definition in error_definitions {
        translate_error_definition(
            project,
            module.clone(),
            Some(&contract_name),
            &error_definition,
        )?;
    }

    // Create the abi encoding function for the errors enum (if any)
    let errors_enum_name = format!("{}Error", module.borrow().name);

    if let Some((errors_enum, abi_encode_impl)) = module
        .borrow()
        .errors_enums
        .iter()
        .find(|(e, _)| e.borrow().name == errors_enum_name)
        .cloned()
    {
        generate_enum_abi_encode_function(
            project,
            module.clone(),
            errors_enum.clone(),
            abi_encode_impl.clone(),
        )?;
    }

    // Translate contract state variables
    let mut deferred_initializations = vec![];
    let mut mapping_names = vec![];

    for variable_definition in variable_definitions {
        let (d, m) = translate_state_variable(
            project,
            module.clone(),
            Some(&contract_name),
            &variable_definition,
        )?;
        deferred_initializations.extend(d);
        mapping_names.extend(m);
    }

    // Collect the signatures of the contract functions
    let functions_index = module.borrow().functions.len();

    for function_definition in function_definitions.iter() {
        let is_modifier = matches!(function_definition.ty, solidity::FunctionTy::Modifier);
        if is_modifier {
            continue;
        }

        let signature = translate_function_declaration(
            project,
            module.clone(),
            Some(&contract_name),
            function_definition,
        )?
        .type_name;

        module.borrow_mut().functions.push(ir::Item {
            signature,
            implementation: None,
        });
    }

    // Translate each function
    let mut i = 0;

    for function_definition in function_definitions.iter() {
        let is_modifier = matches!(function_definition.ty, solidity::FunctionTy::Modifier);
        if is_modifier {
            continue;
        }

        let (function, mut abi_fn, impl_item) = translate_function_definition(
            project,
            module.clone(),
            Some(&contract_name),
            &function_definition,
        )?;

        assert_eq!(abi_fn.is_some(), impl_item.is_some());

        if abi_fn.is_none() && function.body.is_none() {
            abi_fn = Some(function.clone());
        }

        if let Some(function) = abi_fn.take() {
            if !contract.borrow().abi.functions.contains(&function) {
                contract.borrow_mut().abi.functions.push(function);
            }
        }

        if let Some(impl_item) = impl_item {
            if !contract.borrow().abi_impl.items.contains(&impl_item) {
                contract.borrow_mut().abi_impl.items.push(impl_item);
            }
        }

        module.borrow_mut().functions[functions_index + i].implementation = Some(function);

        i += 1;
    }

    // Translate each modifier
    for function_definition in function_definitions.iter() {
        let is_modifier = matches!(function_definition.ty, solidity::FunctionTy::Modifier);
        if !is_modifier || function_definition.body.is_none() {
            continue;
        }

        translate_modifier_definition(
            project,
            module.clone(),
            Some(&contract_name),
            function_definition,
        )?;
    }

    // Propagate deferred initializations into the constructor
    if !deferred_initializations.is_empty() {
        let namespace_name = translate_naming_convention(&contract_name, Case::Snake);
        let scope = Rc::new(RefCell::new(ir::Scope::new(
            Some(contract_name.as_str()),
            None,
        )));

        let mut assignment_statements = vec![];

        // Create assignment statements for all of the deferred initializations
        for deferred_initialization in deferred_initializations.iter().rev() {
            let lhs = sway::Expression::create_member_access(
                sway::Expression::create_identifier(format!("storage::{namespace_name}")),
                &[deferred_initialization.name.as_str()],
            );

            let value_type_name = module.borrow_mut().get_expression_type(
                project,
                scope.clone(),
                &deferred_initialization.value,
            )?;

            match &deferred_initialization.value {
                sway::Expression::Array(sway::Array { elements }) => {
                    for element in elements {
                        assignment_statements.push(sway::Statement::from(
                            sway::Expression::create_function_calls(
                                None,
                                &[
                                    (format!("storage::{namespace_name}").as_str(), None),
                                    (deferred_initialization.name.as_str(), None),
                                    ("push", Some((None, vec![element.clone()]))),
                                ],
                            ),
                        ));
                    }
                }

                _ => {
                    assignment_statements.push(sway::Statement::from(
                        create_assignment_expression(
                            project,
                            module.clone(),
                            scope.clone(),
                            "=",
                            &lhs,
                            None,
                            &deferred_initialization.value,
                            &value_type_name,
                        )?,
                    ));
                }
            }
        }

        // Create the constructor if it doesn't exist
        if !contract.borrow().abi_impl.items.iter().any(|i| {
            let sway::ImplItem::Function(f) = i else {
                return false;
            };
            f.name == "constructor"
        }) {
            let mut function = sway::Function {
                attributes: None,
                is_public: false,
                old_name: String::new(),
                name: "constructor".into(),
                generic_parameters: None,
                parameters: sway::ParameterList::default(),
                return_type: None,
                body: None,
            };

            contract
                .borrow_mut()
                .abi
                .functions
                .insert(0, function.clone());

            function.body = Some(sway::Block::default());
            let function_body = function.body.as_mut().unwrap();

            let prefix = translate_naming_convention(contract_name.as_str(), Case::Snake);
            let constructor_called_variable_name = format!("{prefix}_constructor_called");

            // Add the `constructor_called` field to the storage block
            module
                .borrow_mut()
                .get_storage_namespace(scope)
                .unwrap()
                .borrow_mut()
                .fields
                .push(sway::StorageField {
                    old_name: String::new(),
                    name: constructor_called_variable_name.clone(),
                    type_name: sway::TypeName::Identifier {
                        name: "bool".into(),
                        generic_parameters: None,
                    },
                    abi_type_name: None,
                    value: sway::Expression::from(sway::Literal::Bool(false)),
                });

            // Add the `constructor_called` requirement to the beginning of the function
            // require(!storage.initialized.read(), "The Contract constructor has already been called");
            function_body.statements.insert(
                0,
                sway::Statement::from(sway::Expression::create_function_calls(
                    None,
                    &[(
                        "require",
                        Some((
                            None,
                            vec![
                                sway::Expression::from(sway::UnaryExpression {
                                    operator: "!".into(),
                                    expression: sway::Expression::create_function_calls(
                                        None,
                                        &[
                                            (format!("storage::{namespace_name}").as_str(), None),
                                            (constructor_called_variable_name.as_str(), None),
                                            ("read", Some((None, vec![]))),
                                        ],
                                    ),
                                }),
                                sway::Expression::from(sway::Literal::String(format!(
                                    "The {} constructor has already been called",
                                    contract.borrow().name
                                ))),
                            ],
                        )),
                    )],
                )),
            );

            // Set the `constructor_called` storage field to `true` at the end of the function
            // storage.initialized.write(true);
            function_body.statements.push(sway::Statement::from(
                sway::Expression::create_function_calls(
                    None,
                    &[
                        (format!("storage::{namespace_name}").as_str(), None),
                        (constructor_called_variable_name.as_str(), None),
                        (
                            "write",
                            Some((
                                None,
                                vec![sway::Expression::from(sway::Literal::Bool(true))],
                            )),
                        ),
                    ],
                ),
            ));

            contract
                .borrow_mut()
                .abi_impl
                .items
                .insert(0, sway::ImplItem::Function(function));

            //
            // TODO: We need to insert a top level function for inheritence
            //
        }

        let mut contract = contract.borrow_mut();

        let constructor_function = contract
            .abi_impl
            .items
            .iter_mut()
            .find(|i| {
                let sway::ImplItem::Function(f) = i else {
                    return false;
                };

                f.name == "constructor"
            })
            .map(|i| {
                let sway::ImplItem::Function(f) = i else {
                    return None;
                };
                Some(f)
            })
            .flatten()
            .unwrap();

        if constructor_function.body.is_none() {
            constructor_function.body = Some(sway::Block::default());
        }

        let constructor_body = constructor_function.body.as_mut().unwrap();

        let mut statement_index = 0;

        // Skip past the initial constructor requirements
        for (i, statement) in constructor_body.statements.iter().enumerate() {
            let sway::Statement::Expression(sway::Expression::FunctionCall(function_call)) =
                statement
            else {
                statement_index = i;
                break;
            };

            let Some(function_name) = function_call.function.as_identifier() else {
                statement_index = i;
                break;
            };

            if function_name != "require" {
                statement_index = i;
                break;
            }
        }

        // Add the deferred initializations to the constructor body
        for statement in assignment_statements.into_iter().rev() {
            constructor_body
                .statements
                .insert(statement_index, statement);
        }
    }

    Ok(())
}

#[inline]
pub fn translate_using_directive(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    contract_name: Option<&str>,
    using_directive: &solidity::Using,
) -> Result<(), Error> {
    let scope = Rc::new(RefCell::new(ir::Scope::new(contract_name, None)));

    let for_type = using_directive
        .ty
        .as_ref()
        .map(|t| translate_type_name(project, module.clone(), scope.clone(), t, false, false))
        .map_or(Ok(None), |t| Ok(Some(t)))?;

    match &using_directive.list {
        solidity::UsingList::Library(using_library) => {
            let library_name = using_library
                .identifiers
                .iter()
                .map(|i| i.name.clone())
                .collect::<Vec<_>>()
                .join(".");

            if library_name == module.borrow().name {
                // Add a self-referential using directive to the current definition
                module
                    .borrow_mut()
                    .using_directives
                    .push(ir::UsingDirective {
                        library_name,
                        for_type,
                        functions: vec![],
                    });

                return Ok(());
            }

            // Find the translated library definition
            let Some(library_definition) = project.find_module_with_contract(&library_name) else {
                panic!(
                    "Failed to find translated library: \"{library_name}\"; from {}",
                    match project.loc_to_line_and_column(module.clone(), &using_directive.loc) {
                        Some((line, col)) => format!(
                            "{}:{}:{}: ",
                            project
                                .options
                                .input
                                .join(module.borrow().path.clone())
                                .with_extension("sol")
                                .to_string_lossy(),
                            line,
                            col
                        ),
                        None => format!(
                            "{}: ",
                            project
                                .options
                                .input
                                .join(module.borrow().path.clone())
                                .with_extension("sol")
                                .to_string_lossy()
                        ),
                    },
                )
            };

            let mut translated_using_directive = ir::UsingDirective {
                library_name,
                for_type,
                functions: vec![],
            };

            // Collect all functions that support the `for_type`
            for function in library_definition.borrow().functions.iter() {
                // If we're using the library for a specific type, ensure the first function parameter matches that type
                if translated_using_directive.for_type.is_some()
                    && translated_using_directive.for_type
                        != function
                            .implementation
                            .as_ref()
                            .unwrap()
                            .parameters
                            .entries
                            .first()
                            .and_then(|p| p.type_name.clone())
                {
                    continue;
                }

                // Add the function to the translated using directive so we know where it came from
                translated_using_directive.functions.push({
                    let sway::TypeName::Function { new_name, .. } = &function.signature else {
                        unreachable!()
                    };
                    new_name.clone()
                });
            }

            // Add the using directive to the current definition
            module
                .borrow_mut()
                .using_directives
                .push(translated_using_directive);
        }

        solidity::UsingList::Functions(_) => todo!(
            "using directive function list: {}",
            using_directive.to_string()
        ),

        solidity::UsingList::Error => panic!("Failed to parse using directive"),
    }

    Ok(())
}
