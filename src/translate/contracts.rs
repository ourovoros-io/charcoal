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
    // println!(
    //     "Translating contract `{}` at {}",
    //     contract_definition.name.as_ref().map(|x| x.name.as_str()).unwrap(),
    //     project.loc_to_file_location_string(module.clone(), &contract_definition.loc),
    // );

    // Collect each contract part into separate collections
    let mut using_directives = vec![];
    let mut event_definitions = vec![];
    let mut error_definitions = vec![];
    let mut function_definitions = vec![];
    let mut variable_definitions = vec![];

    let mut has_constructor = false;

    for part in contract_definition.parts.iter() {
        match part {
            solidity::ContractPart::EventDefinition(event_definition) => {
                event_definitions.push(event_definition.clone())
            }

            solidity::ContractPart::ErrorDefinition(error_definition) => {
                error_definitions.push(error_definition.clone())
            }

            solidity::ContractPart::VariableDefinition(variable_definition) => {
                let is_constant = variable_definition
                    .attrs
                    .iter()
                    .any(|x| matches!(x, solidity::VariableAttribute::Constant(_)));

                let is_immutable = variable_definition
                    .attrs
                    .iter()
                    .any(|x| matches!(x, solidity::VariableAttribute::Immutable(_)));

                let is_configurable = is_immutable && !is_constant;

                // Only translate regular state variables.
                // Constants and configurables are translated at toplevel ahead of time.
                if !is_constant && !is_configurable {
                    variable_definitions.push(variable_definition);
                }
            }

            solidity::ContractPart::FunctionDefinition(function_definition) => {
                if matches!(function_definition.ty, solidity::FunctionTy::Constructor) {
                    has_constructor = true;
                }

                function_definitions.push(function_definition.clone())
            }

            solidity::ContractPart::Annotation(_annotation) => {}

            solidity::ContractPart::Using(using_directive) => {
                using_directives.push(using_directive.clone())
            }

            solidity::ContractPart::StraySemicolon(_loc) => {}

            _ => {}
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
    let events_enum_name = format!("{}Event", contract_name);

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
    let errors_enum_name = format!("{}Error", contract_name);

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

    let scope = Rc::new(RefCell::new(ir::Scope::new(
        Some(contract_name.as_str()),
        None,
        None,
    )));

    // HACK: Add an implicit `constructor_called` state variable if we have a constructor function
    if has_constructor {
        ensure_constructor_called_fields_exist(project, module.clone(), scope.clone());
    }

    // Translate contract state variables
    let mut deferred_initializations = vec![];
    let mut mapping_names = vec![];
    let mut state_variable_infos = vec![];

    for variable_definition in variable_definitions.iter() {
        let state_variable_info = translate_state_variable(
            project,
            module.clone(),
            Some(&contract_name),
            &variable_definition,
        )?;

        deferred_initializations.extend(state_variable_info.deferred_initializations.clone());
        mapping_names.extend(state_variable_info.mapping_names.clone());

        state_variable_infos.push(state_variable_info);
    }

    for state_variable_info in state_variable_infos {
        let Some((abi_fn, toplevel_fn, impl_fn)) = generate_state_variable_getter_functions(
            project,
            module.clone(),
            scope.clone(),
            Some(contract_name.as_str()),
            &state_variable_info,
        )?
        else {
            continue;
        };

        contract.borrow_mut().abi.functions.push(abi_fn);

        if let Some(function) = module
            .borrow_mut()
            .functions
            .iter_mut()
            .find(|f| f.signature == toplevel_fn.get_type_name())
        {
            assert!(function.implementation.is_none());
            function.implementation = Some(toplevel_fn);
        } else {
            module.borrow_mut().functions.push(ir::Item {
                signature: toplevel_fn.get_type_name(),
                implementation: Some(toplevel_fn),
            });
        }

        contract
            .borrow_mut()
            .abi_impl
            .items
            .push(sway::ImplItem::Function(impl_fn));
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

    // Translate each function
    for function_definition in function_definitions {
        let is_modifier = matches!(function_definition.ty, solidity::FunctionTy::Modifier);
        if is_modifier {
            continue;
        }

        let (Some(function), impl_item) = translate_function_definition(
            project,
            module.clone(),
            Some(&contract_name),
            &function_definition,
        )?
        else {
            continue;
        };

        if let Some(impl_item) = impl_item
            && !contract.borrow().abi_impl.items.contains(&impl_item)
        {
            contract.borrow_mut().abi_impl.items.push(impl_item);
        }

        let mut module = module.borrow_mut();

        let function_signature = sway::TypeName::Function {
            old_name: function.old_name.clone(),
            new_name: function.new_name.clone(),
            generic_parameters: function.generic_parameters.clone(),
            parameters: function.parameters.clone(),
            storage_struct_parameter: function.storage_struct_parameter.clone().map(Box::new),
            return_type: function.return_type.clone().map(Box::new),
        };

        let Some(function_entry) = module.functions.iter_mut().find(|f| {
            let sway::TypeName::Function { new_name, .. } = &f.signature else {
                unreachable!()
            };

            *new_name == function.new_name && f.signature.is_compatible_with(&function_signature)
        }) else {
            panic!(
                "Failed to find function {} - {} - in list:\n{}",
                function.new_name,
                function_signature,
                module
                    .functions
                    .iter()
                    .map(|f| {
                        let sway::TypeName::Function { new_name, .. } = &f.signature else {
                            unreachable!()
                        };

                        format!("    {} - {}", new_name, f.signature)
                    })
                    .collect::<Vec<_>>()
                    .join("\n"),
            );
        };

        function_entry.implementation = Some(function);
    }

    // Propagate deferred initializations into the constructor
    if !deferred_initializations.is_empty() {
        let namespace_name = translate_naming_convention(&contract_name, Case::Snake);

        let scope = Rc::new(RefCell::new(ir::Scope::new(
            Some(contract_name.as_str()),
            None,
            None,
        )));

        let mut assignment_statements = vec![];

        // Create assignment statements for all of the deferred initializations
        for deferred_initialization in deferred_initializations.iter().rev() {
            let lhs = sway::Expression::create_member_access(
                sway::Expression::create_identifier(format!("storage::{namespace_name}")),
                &[deferred_initialization.name.as_str()],
            );

            let value_type_name = get_expression_type(
                project,
                module.clone(),
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

        ensure_constructor_functions_exist(project, module.clone(), scope.clone(), contract.clone());

        let mut module = module.borrow_mut();
        let constructor_name = format!("{namespace_name}_constructor");

        let constructor_function = module
            .functions
            .iter_mut()
            .find(|f| {
                let sway::TypeName::Function { new_name, .. } = &f.signature else {
                    unreachable!()
                };
                *new_name == constructor_name
            })
            .map(|f| f.implementation.as_mut())
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
    let scope = Rc::new(RefCell::new(ir::Scope::new(contract_name, None, None)));

    let for_type = using_directive
        .ty
        .as_ref()
        .map(|t| translate_type_name(project, module.clone(), scope.clone(), t, None))
        .map_or(Ok(None), |t| Ok(Some(t)))?;

    match &using_directive.list {
        solidity::UsingList::Library(using_library) => {
            let library_name = using_library
                .identifiers
                .iter()
                .map(|i| i.name.clone())
                .collect::<Vec<_>>()
                .join(".");

            // Find the translated library definition
            let Some(library_definition) =
                project.find_module_containing_contract(module.clone(), &library_name)
            else {
                panic!(
                    "Failed to find translated library: \"{library_name}\"; from {}",
                    project.loc_to_file_location_string(module.clone(), &using_directive.loc),
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
                let sway::TypeName::Function { parameters, .. } = &function.signature else {
                    unreachable!()
                };

                if translated_using_directive.for_type.is_some()
                    && translated_using_directive.for_type
                        != parameters.entries.first().and_then(|p| p.type_name.clone())
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
