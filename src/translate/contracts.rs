use super::{
    assignment::create_assignment_expression, generate_enum_abi_encode_function, resolve_import,
    translate_enum_definition, translate_error_definition, translate_event_definition,
    translate_function_declaration, translate_function_definition, translate_modifier_definition,
    translate_state_variable, translate_storage_name, translate_struct_definition,
    translate_type_definition, translate_type_name, TranslatedDefinition, TranslatedUsingDirective,
};
use crate::{project::Project, sway, Error};
use convert_case::Case;
use solang_parser::pt as solidity;
use std::{cell::RefCell, path::PathBuf, rc::Rc};

#[inline]
pub fn translate_using_directive(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    using_directive: &solidity::Using,
) -> Result<(), Error> {
    let for_type = using_directive.ty.as_ref()
        .map(|t| translate_type_name(project, translated_definition, t, false, false))
        .map_or(Ok(None), |t| Ok(Some(t)))?;

    match &using_directive.list {
        solidity::UsingList::Library(using_library) => {
            let library_name = using_library.identifiers.iter().map(|i| i.name.clone()).collect::<Vec<_>>().join(".");
            
            if library_name == translated_definition.name {
                // Add a self-referential using directive to the current definition
                translated_definition.using_directives.push(TranslatedUsingDirective {
                    library_name,
                    for_type,
                    functions: vec![],
                });

                return Ok(());
            }

            // Find the translated library definition
            let Some(library_definition) = project.translated_definitions.iter().find(|d| {
                d.name == library_name && matches!(d.kind.as_ref().unwrap(), solidity::ContractTy::Library(_))
            }) else {
                panic!(
                    "Failed to find translated library: \"{library_name}\"; from {}",
                    match project.loc_to_line_and_column(&translated_definition.path, &using_directive.loc) {
                        Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                        None => format!("{} - ", translated_definition.path.to_string_lossy()),
                    },
                )
            };

            let mut translated_using_directive = TranslatedUsingDirective {
                library_name,
                for_type,
                functions: vec![],
            };

            // Collect all functions that support the `for_type`
            for function in library_definition.functions.iter() {
                // If we're using the library for a specific type, ensure the first function parameter matches that type
                if translated_using_directive.for_type.is_some() && translated_using_directive.for_type != function.parameters.entries.first().and_then(|p| p.type_name.clone()) {
                    continue;
                }

                // Get the scope entry for the library function
                let Some(scope_entry) = library_definition.toplevel_scope.borrow().find_function(|f| f.borrow().new_name == function.name) else {
                    panic!("Failed to find function in scope: \"{}\"", function.name);
                };

                // Add the function to the current definition's toplevel scope
                if !translated_definition.toplevel_scope.borrow().functions.iter().any(|f| {
                    f.borrow().old_name == scope_entry.borrow().old_name
                    && f.borrow().parameters == scope_entry.borrow().parameters
                    && f.borrow().return_type == scope_entry.borrow().return_type
                }) {
                    translated_definition.toplevel_scope.borrow_mut().functions.push(Rc::new(RefCell::new(scope_entry.borrow().clone())));
                }

                // Add the function to the translated using directive so we know where it came from
                translated_using_directive.functions.push(scope_entry.borrow().clone());

                // Add the function name to the current definition's function name list
                *translated_definition.function_name_counts.entry(function.name.clone()).or_insert(0) += 1;

                // Add the function definition to the current definition
                if !translated_definition.functions.contains(function) {
                    translated_definition.functions.push(function.clone());
                }

                // Add the function call count from the library definition to the current definition
                translated_definition.function_call_counts.insert(
                    function.name.clone(),
                    if let Some(function_call_count) = library_definition.function_call_counts.get(&function.name) {
                        *function_call_count
                    } else {
                        0
                    }
                );

                // Add the functions called from the library definition to the current definition
                for (lib_calling_fn, lib_called_fns) in library_definition.functions_called.iter() {
                    let called_functions = translated_definition.functions_called.entry(lib_calling_fn.clone()).or_default();

                    for lib_called_fn in lib_called_fns.iter() {
                        if !called_functions.contains(lib_called_fn) {
                            called_functions.push(lib_called_fn.clone());
                        }
                    }
                }
            }

            // Add the using directive to the current definition
            translated_definition.using_directives.push(translated_using_directive);
        }

        solidity::UsingList::Functions(_) => todo!("using directive function list: {}", using_directive.to_string()),

        solidity::UsingList::Error => panic!("Failed to parse using directive"),
    }

    Ok(())
}

#[allow(clippy::too_many_arguments)]
#[inline]
pub fn translate_contract_definition(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    import_directives: &[solidity::Import],
    contract_definition: &solidity::ContractDefinition,
) -> Result<(), Error> {
    // Propagate inherited definitions
    propagate_inherited_definitions(project, import_directives, translated_definition)?;

    // Translate contract using directives
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::Using(using_directive) = part else { continue };
        translate_using_directive(project, translated_definition, using_directive)?;
    }

    // Translate contract type definitions
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::TypeDefinition(type_definition) = part else { continue };
        translate_type_definition(project, translated_definition, type_definition)?;
    }

    // Translate contract enum definitions
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::EnumDefinition(enum_definition) = part else { continue };
        translate_enum_definition(project, translated_definition, enum_definition)?;
    }

    // Collect contract struct names ahead of time for contextual reasons
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::StructDefinition(struct_definition) = part else { continue };
        let struct_name = struct_definition.name.as_ref().unwrap().name.clone();

        if !translated_definition.struct_names.contains(&struct_name) {
            translated_definition.struct_names.push(struct_name);
        }
    }

    // Translate contract struct definitions
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::StructDefinition(struct_definition) = part else { continue };
        translate_struct_definition(project, translated_definition, struct_definition)?;
    }

    // Translate contract event definitions
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::EventDefinition(event_definition) = part else { continue };
        translate_event_definition(project, translated_definition, event_definition)?;
    }

    // Translate contract error definitions
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::ErrorDefinition(error_definition) = part else { continue };
        translate_error_definition(project, translated_definition, error_definition)?;
    }

    // Create the abi encoding function for the events enum (if any)
    let events_enum_name = format!("{}Event", translated_definition.name);

    if let Some((events_enum, abi_encode_impl)) = translated_definition.events_enums.iter().find(|(e, _)| e.borrow().name == events_enum_name).cloned() {
        generate_enum_abi_encode_function(project, translated_definition, events_enum.clone(), abi_encode_impl.clone())?;
    }

    // Create the abi encoding function for the errors enum (if any)
    let errors_enum_name = format!("{}Error", translated_definition.name);

    if let Some((errors_enum, abi_encode_impl)) = translated_definition.errors_enums.iter().find(|(e, _)| e.borrow().name == errors_enum_name).cloned() {
        generate_enum_abi_encode_function(project, translated_definition, errors_enum.clone(), abi_encode_impl.clone())?;
    }

    // Translate contract state variables
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::VariableDefinition(variable_definition) = part else { continue };
        translate_state_variable(project, translated_definition, variable_definition)?;
    }
    
    // Collect each toplevel function ahead of time for contextual reasons
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::FunctionDefinition(function_definition) = part else { continue };

        let is_modifier = matches!(function_definition.ty, solidity::FunctionTy::Modifier);

        if is_modifier {
            continue;
        }

        // Add the toplevel function to the list of toplevel functions for the toplevel scope
        let function = translate_function_declaration(project, translated_definition, function_definition)?;
        
        let mut function_exists = false;

        for f in translated_definition.toplevel_scope.borrow().functions.iter() {
            let mut f = f.borrow_mut();

            if ((!f.old_name.is_empty() && (f.old_name == function.old_name)) || (f.new_name == function.new_name)) && f.parameters == function.parameters && f.return_type == function.return_type {
                f.new_name = function.new_name.clone();
                function_exists = true;
                break;
            }
        }

        if !function_exists {
            translated_definition.toplevel_scope.borrow_mut().functions.push(Rc::new(RefCell::new(function)));
        }
    }

    // Translate each modifier
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::FunctionDefinition(function_definition) = part else { continue };
        
        let is_modifier = matches!(function_definition.ty, solidity::FunctionTy::Modifier);

        if !is_modifier || function_definition.body.is_none() {
            continue;
        }
        
        translate_modifier_definition(project, translated_definition, function_definition)?;
    }

    // Translate each function
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::FunctionDefinition(function_definition) = part else { continue };

        let is_modifier = matches!(function_definition.ty, solidity::FunctionTy::Modifier);

        if is_modifier {
            continue;
        }

        translate_function_definition(project, translated_definition, function_definition)?;
    }

    // Propagate deferred initializations into the constructor
    if !translated_definition.deferred_initializations.is_empty() {
        let mut assignment_statements = vec![];
        let deferred_initializations = translated_definition.deferred_initializations.clone();

        // Create assignment statements for all of the deferred initializations
        for deferred_initialization in deferred_initializations.iter().rev() {
            let lhs = sway::Expression::create_member_access(
                sway::Expression::Identifier("storage".into()),
                &[deferred_initialization.name.as_str()],
            );
            
            let value_type_name = translated_definition.get_expression_type(translated_definition.toplevel_scope.clone(), &deferred_initialization.value)?;
            let variable = translated_definition.toplevel_scope.borrow().get_variable_from_new_name(&deferred_initialization.name).unwrap();

            match &deferred_initialization.value {
                sway::Expression::Array(sway::Array { elements }) => {
                    for element in elements {
                        assignment_statements.push(sway::Statement::from(sway::Expression::create_function_calls(None, &[
                            ("storage", None),
                            (deferred_initialization.name.as_str(), None),
                            ("push", Some((None, vec![
                                element.clone(),
                            ]))),
                        ])));
                    }
                }

                _ => {
                    let scope = translated_definition.toplevel_scope.clone();
                    
                    assignment_statements.push(sway::Statement::from(create_assignment_expression(
                        project,
                        translated_definition,
                        scope,
                        "=",
                        &lhs,
                        variable,
                        &deferred_initialization.value,
                        &value_type_name,
                    )?));
                }
            }
        }
        
        let mut constructor_function = translated_definition.functions.iter_mut().find(|f| f.name == "constructor");
    
        // Create the constructor if it doesn't exist
        if constructor_function.is_none() {
            let mut function = sway::Function {
                attributes: None,
                is_public: false,
                old_name: "".into(),
                name: "constructor".into(),
                generic_parameters: None,
                parameters: sway::ParameterList::default(),
                return_type: None,
                body: None,
            };
    
            translated_definition.get_abi().functions.insert(0, function.clone());
    
            function.body = Some(sway::Block::default());
            let function_body = function.body.as_mut().unwrap();
    
            let prefix = crate::translate_naming_convention(translated_definition.name.as_str(), Case::Snake);
            let constructor_called_variable_name =  translate_storage_name(project, translated_definition, format!("{prefix}_constructor_called").as_str());
            
            // Add the `constructor_called` field to the storage block
            translated_definition.get_storage().fields.push(sway::StorageField {
                name: constructor_called_variable_name.clone(),
                type_name: sway::TypeName::Identifier {
                    name: "bool".into(),
                    generic_parameters: None,
                },
                value: sway::Expression::from(sway::Literal::Bool(false)),
            });
    
            // Add the `constructor_called` requirement to the beginning of the function
            // require(!storage.initialized.read(), "The Contract constructor has already been called");
            function_body.statements.insert(0, sway::Statement::from(sway::Expression::create_function_calls(None, &[
                ("require", Some((None, vec![
                    sway::Expression::from(sway::UnaryExpression {
                        operator: "!".into(),
                        expression: sway::Expression::create_function_calls(None, &[
                            ("storage", None),
                            (constructor_called_variable_name.as_str(), None),
                            ("read", Some((None, vec![]))),
                        ]),
                    }),
                    sway::Expression::from(sway::Literal::String(format!("The {} constructor has already been called", translated_definition.name))),
                ]))),
            ])));
    
            // Set the `constructor_called` storage field to `true` at the end of the function
            // storage.initialized.write(true);
            function_body.statements.push(sway::Statement::from(sway::Expression::create_function_calls(None, &[
                ("storage", None),
                (constructor_called_variable_name.as_str(), None),
                ("write", Some((None, vec![
                    sway::Expression::from(sway::Literal::Bool(true)),
                ]))),
            ])));

            translated_definition.get_contract_impl().items.insert(0, sway::ImplItem::Function(function));
            constructor_function = translated_definition.get_contract_impl().items.iter_mut()
                .find(|i| {
                    let sway::ImplItem::Function(f) = i else { return false };
                    f.name == "constructor"
                })
                .map(|i| {
                    let sway::ImplItem::Function(f) = i else { unreachable!() };
                    f
                });
        }

        let constructor_function = constructor_function.unwrap();

        if constructor_function.body.is_none() {
            constructor_function.body = Some(sway::Block::default());
        }

        let constructor_body = constructor_function.body.as_mut().unwrap();

        let mut statement_index = 0;

        // Skip past the initial constructor requirements
        for (i, statement) in constructor_body.statements.iter().enumerate() {
            let sway::Statement::Expression(sway::Expression::FunctionCall(function_call)) = statement else {
                statement_index = i;
                break;
            };

            let sway::Expression::Identifier(function_name) = &function_call.function else {
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
            constructor_body.statements.insert(statement_index, statement);
        }
    }

    // Expand function attributes to support the attributes of any other functions they call
    for (calling_name, called_names) in translated_definition.functions_called.iter() {
        for called_name in called_names.iter() {
            // HACK: if we can't find the function, skip for now...
            // TODO: look into why some functions are showing up here
            let Some(called_function) = translated_definition.functions.iter().find(|f| f.name == *called_name).cloned() else { continue };
            let Some(calling_function) = translated_definition.functions.iter_mut().find(|f| f.name == *calling_name) else { continue };
            
            if let Some(called_attrs) = called_function.attributes.as_ref() {
                if calling_function.attributes.is_none() {
                    calling_function.attributes = Some(sway::AttributeList::default());
                }

                let calling_attrs = calling_function.attributes.as_mut().unwrap();

                for called_attr in called_attrs.attributes.iter() {
                    if !calling_attrs.attributes.iter().any(|a| a.name == called_attr.name) {
                        calling_attrs.attributes.push(sway::Attribute {
                            name: called_attr.name.clone(),
                            parameters: None,
                        });
                    }

                    let calling_attr = calling_attrs.attributes.iter_mut().find(|a| a.name == called_attr.name).unwrap();

                    if let Some(called_params) = called_attr.parameters.as_ref() {
                        if calling_attr.parameters.is_none() {
                            calling_attr.parameters = Some(vec![]);
                        }

                        let calling_params = calling_attr.parameters.as_mut().unwrap();
                        
                        for called_param in called_params.iter() {
                            if !calling_params.contains(called_param) {
                                calling_params.push(called_param.clone());
                            }
                        }
                    }
                }
            }
        }
    }

    // Look for toplevel functions that are never called, move their implementation to the abi wrapper function if it exists
    if !matches!(translated_definition.kind.as_ref(), Some(solidity::ContractTy::Abstract(_))) {
        let function_names = translated_definition.functions.iter().map(|f| f.name.clone()).collect::<Vec<_>>();
        for function_name in function_names {
            let (None | Some(0)) = translated_definition.function_call_counts.get(&function_name) else { continue };
            let Some((toplevel_function_index, _)) = translated_definition.functions.iter().enumerate().find(|(_, f)| f.name == function_name) else { continue };
            let body = translated_definition.functions[toplevel_function_index].body.clone();

            let Some(contract_impl) = translated_definition.find_contract_impl_mut() else { continue };
            let Some(contract_impl_function) = contract_impl.items.iter_mut()
                .filter_map(|i| match i {
                    sway::ImplItem::Function(f) => Some(f),
                    _ => None,
                })
                .find(|f| f.name == function_name) else { continue };
            
            contract_impl_function.body = body;
            translated_definition.functions.remove(toplevel_function_index);
        }
    }
    
    Ok(())
}

#[inline]
pub fn propagate_inherited_definitions(
    project: &mut Project,
    import_directives: &[solidity::Import],
    translated_definition: &mut TranslatedDefinition,
) -> Result<(), Error> {
    let source_unit_directory = translated_definition.path.parent().map(PathBuf::from).unwrap();

    for inherit in translated_definition.inherits.clone() {
        let mut inherited_definition = None;

        // Find inherited import directive
        for import_directive in import_directives.iter() {
            let filename = match import_directive {
                solidity::Import::Plain(solidity::ImportPath::Filename(filename), _) => filename,
                
                solidity::Import::Rename(solidity::ImportPath::Filename(filename), identifiers, _) => {
                    if !identifiers.iter().any(|i| i.0.name == *inherit) {
                        continue;
                    }

                    filename
                }

                _ => panic!("Unsupported import directive: {import_directive:#?}"),
            };
            
            let mut import_path = project.get_project_type_path(&source_unit_directory, filename.string.as_str())?;
            
            if !import_path.exists() {
                import_path = match translated_definition.path.parent() {
                    Some(path) => path.join(filename.string.as_str()),
                    None => PathBuf::from(filename.string.as_str())
                };

                if !import_path.exists() {
                    return Err(Error::Wrapped(Box::new(
                        std::io::Error::new(
                            std::io::ErrorKind::NotFound,
                            format!("File not found: {}", import_path.to_string_lossy()),
                        )
                    )));
                }
            }

            let import_path = crate::get_canonical_path(import_path, false, false)
                .map_err(|e| Error::Wrapped(Box::new(e)))?;

            if let Some(t) = resolve_import(project, &inherit, &import_path)? {
                inherited_definition = Some(t);
                break;
            }
        }

        // Check to see if the definition was defined in the current file
        if inherited_definition.is_none() {
            if let Some(t) = resolve_import(project, &inherit, &translated_definition.path)? {
                inherited_definition = Some(t);
            } else if translated_definition.abis.iter().find(|d| d.name == inherit).is_some() {
                return Ok(());
            }
        }

        let Some(inherited_definition) = inherited_definition else {
            panic!("Failed to find inherited definition \"{inherit}\" for \"{}\"", translated_definition.name);
        };

        // Extend the toplevel scope
        let inherited_variables = inherited_definition.toplevel_scope.borrow().variables.clone();
        let inherited_functions = inherited_definition.toplevel_scope.borrow().functions.clone();
        translated_definition.toplevel_scope.borrow_mut().variables.extend(inherited_variables);
        translated_definition.toplevel_scope.borrow_mut().functions.extend(inherited_functions);

        // Extend the use statements
        for inherited_use in inherited_definition.uses.iter() {
            if !translated_definition.uses.contains(inherited_use) {
                translated_definition.uses.push(inherited_use.clone());
            }
        }

        // Extend the inherit statements
        for inherited_inherit in inherited_definition.inherits.iter() {
            if !translated_definition.inherits.contains(inherited_inherit) {
                translated_definition.inherits.push(inherited_inherit.clone());
            }
        }

        // Extend the type definitions
        for inherited_type_definition in inherited_definition.type_definitions.iter() {
            if !translated_definition.type_definitions.contains(inherited_type_definition) {
                translated_definition.type_definitions.push(inherited_type_definition.clone());
            }
        }

        // Extend the structs
        for inherited_struct in inherited_definition.structs.iter() {
            translated_definition.ensure_struct_included(project, inherited_struct.clone());
        }

        // Extend the struct names
        for inherited_struct_name in inherited_definition.struct_names.iter() {
            if !translated_definition.struct_names.contains(inherited_struct_name) {
                translated_definition.struct_names.push(inherited_struct_name.clone());
            }
        }

        // Extend the enums
        for inherited_enum in inherited_definition.enums.iter() {
            translated_definition.add_enum(inherited_enum);
        }

        // Extend the events enum
        for inherited_enum in inherited_definition.events_enums.iter() {
            if !translated_definition.events_enums.contains(inherited_enum) {
                translated_definition.events_enums.push(inherited_enum.clone());
            }
        }

        // Extend the errors enum
        for inherited_enum in inherited_definition.errors_enums.iter() {
            if !translated_definition.errors_enums.contains(inherited_enum) {
                translated_definition.errors_enums.push(inherited_enum.clone());
            }
        }

        // Extend the constants
        for constant in inherited_definition.constants.iter() {
            if !translated_definition.constants.contains(constant) {
                translated_definition.constants.push(constant.clone());
            }
        }

        // Extend the ABIs
        for abi in inherited_definition.abis.iter() {
            if !translated_definition.abis.contains(abi) {
                translated_definition.abis.push(abi.clone());
            }
        }

        // Extend the abi
        if let Some(inherited_abi) = inherited_definition.abi.as_ref() {
            for inherited_function in inherited_abi.functions.iter() {
                if inherited_function.name == "constructor" {
                    continue;
                }

                let abi = translated_definition.get_abi();

                if !abi.functions.contains(inherited_function) {
                    abi.functions.push(inherited_function.clone());
                }
            }
        }

        // Extend the configurable fields
        if let Some(inherited_configurable) = inherited_definition.configurable.as_ref() {
            let configurable = translated_definition.get_configurable();

            for inherited_field in inherited_configurable.fields.iter() {
                if !configurable.fields.contains(inherited_field) {
                    configurable.fields.push(inherited_field.clone());
                }
            }
        }

        // Extend the storage fields
        if let Some(inherited_storage) = inherited_definition.storage.as_ref() {
            let storage = translated_definition.get_storage();

            for inherited_field in inherited_storage.fields.iter() {
                if storage.fields.contains(inherited_field) {
                    continue;
                }
                
                storage.fields.push(inherited_field.clone());
            }
        }

        // Extend the modifiers
        for inherited_modifier in inherited_definition.modifiers.iter() {
            if !translated_definition.modifiers.contains(inherited_modifier) {
                translated_definition.modifiers.push(inherited_modifier.clone());
            }
        }

        // Extend the functions
        for inherited_function in inherited_definition.functions.iter() {
            if !translated_definition.functions.contains(inherited_function) {
                translated_definition.functions.push(inherited_function.clone());
            }
        }

        // Extend function name count mapping
        for (function_name, count) in inherited_definition.function_name_counts.iter() {
            *translated_definition.function_name_counts.entry(function_name.clone()).or_insert(0) += *count;
        }

        // Extend function name mapping
        for (signature, function_name) in inherited_definition.function_names.iter() {
            if !translated_definition.function_names.contains_key(signature) {
                translated_definition.function_names.insert(signature.clone(), function_name.clone());
            }
        }

        // Extend function call count mapping
        for (function_call, count) in inherited_definition.function_call_counts.iter() {
            *translated_definition.function_call_counts.entry(function_call.clone()).or_insert(0) += *count;
        }

        // Extend functions called mapping
        for (inherited_calling_function, inherited_called_functions) in inherited_definition.functions_called.iter() {
            let called_functions = translated_definition.functions_called.entry(inherited_calling_function.clone()).or_default();

            for inherited_called_function in inherited_called_functions {
                if !called_functions.contains(inherited_called_function) {
                    called_functions.push(inherited_called_function.clone());
                }
            }
        }

        // Extend the contract impl block
        if let Some(inherited_impl) = inherited_definition.find_contract_impl() {
            for inherited_impl_item in inherited_impl.items.iter() {
                if let sway::ImplItem::Function(inherited_function) = inherited_impl_item {
                    if inherited_function.name == "constructor" {
                        let mut inherited_function = inherited_function.clone();
                        
                        let prefix = crate::translate_naming_convention(inherited_definition.name.as_str(), Case::Snake);
                        inherited_function.name = format!("{prefix}_constructor");

                        if !translated_definition.functions.contains(&inherited_function) {
                            translated_definition.functions.push(inherited_function);
                        }

                        continue;
                    }
                }

                let contract_impl = translated_definition.get_contract_impl();

                if !contract_impl.items.contains(inherited_impl_item) {
                    contract_impl.items.push(inherited_impl_item.clone());
                }
            }
        }
    }

    Ok(())
}
