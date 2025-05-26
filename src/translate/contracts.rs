use crate::{error::Error, project::Project, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_using_directive(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    using_directive: &solidity::Using,
) -> Result<(), Error> {
    let for_type = using_directive
        .ty
        .as_ref()
        .map(|t| translate_type_name(project, module.clone(), t, false, false))
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
                    .push(TranslatedUsingDirective {
                        library_name,
                        for_type,
                        functions: vec![],
                    });

                return Ok(());
            }

            // Find the translated library definition
            let Some(library_definition) = project
                .translated_modules
                .iter()
                .find(|d| d.borrow().name == library_name)
            else {
                panic!(
                    "Failed to find translated library: \"{library_name}\"; from {}",
                    match project.loc_to_line_and_column(module.clone(), &using_directive.loc) {
                        Some((line, col)) => format!(
                            "{}:{}:{}: ",
                            module.borrow().path.to_string_lossy(),
                            line,
                            col
                        ),
                        None => format!("{}: ", module.borrow().path.to_string_lossy()),
                    },
                )
            };

            let mut translated_using_directive = TranslatedUsingDirective {
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
                translated_using_directive
                    .functions
                    .push(function.implementation.as_ref().unwrap().name.clone());
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

#[inline]
pub fn translate_contract_definition(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    contract_definition: &solidity::ContractDefinition,
) -> Result<(), Error> {
    // Translate contract using directives
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::Using(using_directive) = part else {
            continue;
        };
        translate_using_directive(project, module.clone(), using_directive)?;
    }

    // Collect the signatures of the contract type definitions
    let mut type_definitions = vec![];

    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::TypeDefinition(type_definition) = part else {
            continue;
        };

        module.borrow_mut().type_definitions.push(TranslatedItem {
            signature: sway::TypeName::Identifier {
                name: type_definition.name.name.clone(),
                generic_parameters: None,
            },
            implementation: None,
        });

        type_definitions.push(type_definition.clone());
    }

    // Collect the signatures of the contract enum definitions
    let mut enum_definitions = vec![];

    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::EnumDefinition(enum_definition) = part else {
            continue;
        };

        module.borrow_mut().enums.push(TranslatedItem {
            signature: sway::TypeName::Identifier {
                name: enum_definition.name.as_ref().unwrap().name.clone(),
                generic_parameters: None,
            },
            implementation: None,
        });

        enum_definitions.push(enum_definition.clone());
    }

    // Collect the signatures of the contract struct definitions
    let mut struct_definitions = vec![];

    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::StructDefinition(struct_definition) = part else {
            continue;
        };

        module.borrow_mut().structs.push(TranslatedItem {
            signature: sway::TypeName::Identifier {
                name: struct_definition.name.as_ref().unwrap().name.clone(),
                generic_parameters: None,
            },
            implementation: None,
        });

        struct_definitions.push(struct_definition.clone());
    }

    // Translate contract type definitions
    for (i, type_definition) in type_definitions.into_iter().enumerate() {
        module.borrow_mut().type_definitions[i].implementation = Some(translate_type_definition(
            project,
            module.clone(),
            type_definition.as_ref(),
        )?);
    }

    // Translate contract enum definitions
    for (i, enum_definition) in enum_definitions.into_iter().enumerate() {
        module.borrow_mut().enums[i].implementation = Some(translate_enum_definition(
            project,
            module.clone(),
            enum_definition.as_ref(),
        )?);
    }

    // Translate contract struct definitions
    for (i, struct_definition) in struct_definitions.into_iter().enumerate() {
        module.borrow_mut().structs[i].implementation = Some(translate_struct_definition(
            project,
            module.clone(),
            struct_definition.as_ref(),
        )?);
    }

    // Translate contract event definitions
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::EventDefinition(event_definition) = part else {
            continue;
        };
        translate_event_definition(project, module.clone(), event_definition)?;
    }

    // Translate contract error definitions
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::ErrorDefinition(error_definition) = part else {
            continue;
        };
        translate_error_definition(project, module.clone(), error_definition)?;
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
        generate_enum_abi_encode_function(project, module.clone(), &events_enum, &abi_encode_impl)?;
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
        generate_enum_abi_encode_function(project, module.clone(), &errors_enum, &abi_encode_impl)?;
    }

    // Translate contract state variables
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::VariableDefinition(variable_definition) = part else {
            continue;
        };
        translate_state_variable(project, module.clone(), variable_definition)?;
    }

    // Translate each modifier
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::FunctionDefinition(function_definition) = part else {
            continue;
        };

        let is_modifier = matches!(function_definition.ty, solidity::FunctionTy::Modifier);

        if !is_modifier || function_definition.body.is_none() {
            continue;
        }

        translate_modifier_definition(project, module.clone(), function_definition)?;
    }

    let mut function_definitions = vec![];

    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::FunctionDefinition(function_definition) = part else {
            continue;
        };

        let is_modifier = matches!(function_definition.ty, solidity::FunctionTy::Modifier);

        if is_modifier {
            continue;
        }

        let signature =
            translate_function_declaration(project, module.clone(), function_definition)?.type_name;

        module.borrow_mut().functions.push(TranslatedItem {
            signature,
            implementation: None,
        });

        function_definitions.push(function_definition.clone());
    }

    // Translate each function
    for (i, function_definition) in function_definitions.into_iter().enumerate() {
        module.borrow_mut().functions[i].implementation = Some(translate_function_definition(
            project,
            module.clone(),
            contract_definition.name.as_ref().map(|n| n.name.clone()),
            &function_definition,
        )?);
    }

    // // Propagate deferred initializations into the constructor
    // if !module.borrow().deferred_initializations.is_empty() {
    //     let mut assignment_statements = vec![];
    //     let deferred_initializations = module.deferred_initializations.clone();

    //     let namespace_name = module.get_storage_namespace_name();

    //     // Create assignment statements for all of the deferred initializations
    //     for deferred_initialization in deferred_initializations.iter().rev() {
    //         let lhs = sway::Expression::create_member_access(
    //             sway::Expression::create_identifier(format!("storage::{namespace_name}")),
    //             &[deferred_initialization.name.as_str()],
    //         );

    //         let value_type_name = module.get_expression_type(
    //             &module.toplevel_scope.clone(),
    //             &deferred_initialization.value,
    //         )?;
    //         let variable = module
    //             .toplevel_scope
    //             .borrow()
    //             .get_variable_from_new_name(&deferred_initialization.name)
    //             .unwrap();

    //         match &deferred_initialization.value {
    //             sway::Expression::Array(sway::Array { elements }) => {
    //                 for element in elements {
    //                     assignment_statements.push(sway::Statement::from(
    //                         sway::Expression::create_function_calls(
    //                             None,
    //                             &[
    //                                 (format!("storage::{namespace_name}").as_str(), None),
    //                                 (deferred_initialization.name.as_str(), None),
    //                                 ("push", Some((None, vec![element.clone()]))),
    //                             ],
    //                         ),
    //                     ));
    //                 }
    //             }

    //             _ => {
    //                 let scope = module.toplevel_scope.clone();

    //                 assignment_statements.push(sway::Statement::from(
    //                     create_assignment_expression(
    //                         project,
    //                         module.clone(),
    //                         &scope,
    //                         "=",
    //                         &lhs,
    //                         &variable,
    //                         &deferred_initialization.value,
    //                         &value_type_name,
    //                     )?,
    //                 ));
    //             }
    //         }
    //     }

    //     let mut constructor_function = module
    //         .functions
    //         .iter_mut()
    //         .find(|f| f.name == "constructor");

    //     // Create the constructor if it doesn't exist
    //     if constructor_function.is_none() {
    //         let mut function = sway::Function {
    //             attributes: None,
    //             is_public: false,
    //             old_name: String::new(),
    //             name: "constructor".into(),
    //             generic_parameters: None,
    //             parameters: sway::ParameterList::default(),
    //             return_type: None,
    //             body: None,
    //         };

    //         module.get_abi().functions.insert(0, function.clone());

    //         function.body = Some(sway::Block::default());
    //         let function_body = function.body.as_mut().unwrap();

    //         let prefix = translate_naming_convention(module.name.as_str(), Case::Snake);
    //         let constructor_called_variable_name = translate_storage_name(
    //             project,
    //             module.clone(),
    //             format!("{prefix}_constructor_called").as_str(),
    //         );

    //         // Add the `constructor_called` field to the storage block
    //         module
    //             .get_storage_namespace()
    //             .fields
    //             .push(sway::StorageField {
    //                 name: constructor_called_variable_name.clone(),
    //                 type_name: sway::TypeName::Identifier {
    //                     name: "bool".into(),
    //                     generic_parameters: None,
    //                 },
    //                 value: sway::Expression::from(sway::Literal::Bool(false)),
    //             });

    //         // Add the `constructor_called` requirement to the beginning of the function
    //         // require(!storage.initialized.read(), "The Contract constructor has already been called");
    //         function_body.statements.insert(
    //             0,
    //             sway::Statement::from(sway::Expression::create_function_calls(
    //                 None,
    //                 &[(
    //                     "require",
    //                     Some((
    //                         None,
    //                         vec![
    //                             sway::Expression::from(sway::UnaryExpression {
    //                                 operator: "!".into(),
    //                                 expression: sway::Expression::create_function_calls(
    //                                     None,
    //                                     &[
    //                                         (format!("storage::{namespace_name}").as_str(), None),
    //                                         (constructor_called_variable_name.as_str(), None),
    //                                         ("read", Some((None, vec![]))),
    //                                     ],
    //                                 ),
    //                             }),
    //                             sway::Expression::from(sway::Literal::String(format!(
    //                                 "The {} constructor has already been called",
    //                                 module.name
    //                             ))),
    //                         ],
    //                     )),
    //                 )],
    //             )),
    //         );

    //         // Set the `constructor_called` storage field to `true` at the end of the function
    //         // storage.initialized.write(true);
    //         function_body.statements.push(sway::Statement::from(
    //             sway::Expression::create_function_calls(
    //                 None,
    //                 &[
    //                     (format!("storage::{namespace_name}").as_str(), None),
    //                     (constructor_called_variable_name.as_str(), None),
    //                     (
    //                         "write",
    //                         Some((
    //                             None,
    //                             vec![sway::Expression::from(sway::Literal::Bool(true))],
    //                         )),
    //                     ),
    //                 ],
    //             ),
    //         ));

    //         module
    //             .get_contract_impl()
    //             .items
    //             .insert(0, sway::ImplItem::Function(function));
    //         constructor_function = module
    //             .get_contract_impl()
    //             .items
    //             .iter_mut()
    //             .find(|i| {
    //                 let sway::ImplItem::Function(f) = i else {
    //                     return false;
    //                 };
    //                 f.name == "constructor"
    //             })
    //             .map(|i| {
    //                 let sway::ImplItem::Function(f) = i else {
    //                     unreachable!()
    //                 };
    //                 f
    //             });
    //     }

    //     let constructor_function = constructor_function.unwrap();

    //     if constructor_function.body.is_none() {
    //         constructor_function.body = Some(sway::Block::default());
    //     }

    //     let constructor_body = constructor_function.body.as_mut().unwrap();

    //     let mut statement_index = 0;

    //     // Skip past the initial constructor requirements
    //     for (i, statement) in constructor_body.statements.iter().enumerate() {
    //         let sway::Statement::Expression(sway::Expression::FunctionCall(function_call)) =
    //             statement
    //         else {
    //             statement_index = i;
    //             break;
    //         };

    //         let Some(function_name) = function_call.function.as_identifier() else {
    //             statement_index = i;
    //             break;
    //         };

    //         if function_name != "require" {
    //             statement_index = i;
    //             break;
    //         }
    //     }

    //     // Add the deferred initializations to the constructor body
    //     for statement in assignment_statements.into_iter().rev() {
    //         constructor_body
    //             .statements
    //             .insert(statement_index, statement);
    //     }
    // }

    Ok(())
}
