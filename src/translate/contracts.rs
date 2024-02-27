use super::{
    generate_enum_abi_encode_function, resolve_import, translate_enum_definition,
    translate_error_definition, translate_event_definition, translate_function_declaration,
    translate_function_definition, translate_import_directives, translate_modifier_definition,
    translate_state_variable, translate_struct_definition, translate_type_definition,
    translate_type_name, TranslatedDefinition, TranslatedUsingDirective,
};
use crate::{project::Project, sway, Error};
use convert_case::Case;
use solang_parser::pt as solidity;
use std::path::{Path, PathBuf};

#[inline]
pub fn translate_using_directive(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    using_directive: &solidity::Using,
) -> Result<(), Error> {
    let for_type = using_directive.ty.as_ref()
        .map(|t| translate_type_name(project, translated_definition, t, false))
        .map_or(Ok(None), |t| Ok(Some(t)))?;

    match &using_directive.list {
        solidity::UsingList::Library(using_library) => {
            let library_name = using_library.identifiers.iter().map(|i| i.name.clone()).collect::<Vec<_>>().join(".");

            // Find the translated library definition
            let Some(library_definition) = project.translated_definitions.iter().find(|d| {
                d.name == library_name && matches!(d.kind.as_ref().unwrap(), solidity::ContractTy::Library(_))
            }) else {
                panic!("Failed to find translated library: \"{library_name}\"");
            };

            let mut translated_using_directive = TranslatedUsingDirective {
                library_name,
                for_type,
                functions: vec![],
            };

            // Collect all functions that support the `for_type`
            for function in library_definition.functions.iter() {
                // If we're using the library for a specific type, ensure the first function parameter matches that type
                if translated_using_directive.for_type != function.parameters.entries.first().and_then(|p| p.type_name.clone()) {
                    continue;
                }

                // Get the scope entry for the library function
                let Some(scope_entry) = library_definition.toplevel_scope.find_function(|f| f.new_name == function.name) else {
                    panic!("Failed to find function in scope: \"{}\"", function.name);
                };

                // Add the function to the current definition's toplevel scope
                if !translated_definition.toplevel_scope.functions.contains(scope_entry) {
                    translated_definition.toplevel_scope.functions.push(scope_entry.clone());
                }

                // Add the function to the translated using directive so we know where it came from
                translated_using_directive.functions.push(scope_entry.clone());

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
            }

            // Add the using directive to the current definition
            translated_definition.using_directives.push(translated_using_directive);
        }

        solidity::UsingList::Functions(_) => todo!("using directive function list: {}", using_directive.to_string()),

        solidity::UsingList::Error => panic!("Failed to parse using directive"),
    }

    Ok(())
}

#[inline]
pub fn translate_contract_definition(
    project: &mut Project,
    source_unit_path: &Path,
    import_directives: &[solidity::Import],
    toplevel_using_directives: &[solidity::Using],
    toplevel_type_definitions: &[solidity::TypeDefinition],
    toplevel_enums: &[solidity::EnumDefinition],
    toplevel_structs: &[solidity::StructDefinition],
    toplevel_events: &[solidity::EventDefinition],
    toplevel_errors: &[solidity::ErrorDefinition],
    contract_definition: &solidity::ContractDefinition,
) -> Result<(), Error> {
    let definition_name = contract_definition.name.as_ref().unwrap().name.clone();
    let inherits: Vec<String> = contract_definition.base.iter().map(|b| b.name.identifiers.iter().map(|i| i.name.clone()).collect::<Vec<_>>().join(".")).collect();

    // Create a new translation container
    let mut translated_definition = TranslatedDefinition::new(
        source_unit_path,
        contract_definition.ty.clone(),
        definition_name.clone(),
        inherits.clone()
    );

    // Translate import directives
    translate_import_directives(project, &mut translated_definition, import_directives)?;

    // Translate toplevel using directives
    for using_directive in toplevel_using_directives {
        translate_using_directive(project, &mut translated_definition, using_directive)?;
    }

    // Translate toplevel type definitions
    for type_definition in toplevel_type_definitions {
        translate_type_definition(project, &mut translated_definition, type_definition)?;
    }

    // Translate toplevel enum definitions
    for enum_definition in toplevel_enums {
        translate_enum_definition(project, &mut translated_definition, enum_definition)?;
    }

    // Translate toplevel struct definitions
    for struct_definition in toplevel_structs {
        translate_struct_definition(project, &mut translated_definition, struct_definition)?;
    }

    // Translate toplevel event definitions
    for event_definition in toplevel_events {
        translate_event_definition(project, &mut translated_definition, event_definition)?;
    }

    // Translate toplevel error definitions
    for error_definition in toplevel_errors {
        translate_error_definition(project, &mut translated_definition, error_definition)?;
    }

    // Propagate inherited definitions
    propagate_inherited_definitions(project, import_directives, inherits.as_slice(), &mut translated_definition)?;

    // Translate contract using directives
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::Using(using_directive) = part else { continue };
        translate_using_directive(project, &mut translated_definition, using_directive)?;
    }

    // Translate contract type definitions
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::TypeDefinition(type_definition) = part else { continue };
        translate_type_definition(project, &mut translated_definition, type_definition)?;
    }

    // Translate contract enum definitions
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::EnumDefinition(enum_definition) = part else { continue };
        translate_enum_definition(project, &mut translated_definition, enum_definition)?;
    }

    // Translate contract struct definitions
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::StructDefinition(struct_definition) = part else { continue };
        translate_struct_definition(project, &mut translated_definition, struct_definition)?;
    }

    // Translate contract event definitions
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::EventDefinition(event_definition) = part else { continue };
        translate_event_definition(project, &mut translated_definition, event_definition)?;
    }

    // Translate contract error definitions
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::ErrorDefinition(error_definition) = part else { continue };
        translate_error_definition(project, &mut translated_definition, error_definition)?;
    }

    // Create the abi encoding function for the events enum (if any)
    let events_enum_name = format!("{}Event", translated_definition.name);

    if let Some((events_enum, abi_encode_impl)) = translated_definition.events_enums.iter_mut().find(|(e, _)| e.name == events_enum_name) {
        generate_enum_abi_encode_function(project, events_enum, abi_encode_impl)?;
    }

    // Create the abi encoding function for the errors enum (if any)
    let errors_enum_name = format!("{}Error", translated_definition.name);

    if let Some((errors_enum, abi_encode_impl)) = translated_definition.errors_enums.iter_mut().find(|(e, _)| e.name == errors_enum_name) {
        generate_enum_abi_encode_function(project, errors_enum, abi_encode_impl)?;
    }

    // Translate contract state variables
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::VariableDefinition(variable_definition) = part else { continue };
        translate_state_variable(project, &mut translated_definition, variable_definition)?;
    }
    
    // Collect each toplevel function ahead of time for contextual reasons
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::FunctionDefinition(function_definition) = part else { continue };

        let is_modifier = matches!(function_definition.ty, solidity::FunctionTy::Modifier);

        if is_modifier {
            continue;
        }

        // Add the toplevel function to the list of toplevel functions for the toplevel scope
        let function = translate_function_declaration(project, &mut translated_definition, function_definition)?;
        
        let mut function_exists = false;

        for f in translated_definition.toplevel_scope.functions.iter_mut() {
            if f.old_name == function.old_name && f.parameters == function.parameters && f.return_type == function.return_type {
                f.new_name = function.new_name.clone();
                function_exists = true;
                break;
            }
        }

        if !function_exists {
            translated_definition.toplevel_scope.functions.push(function);
        }
    }

    // Translate each modifier
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::FunctionDefinition(function_definition) = part else { continue };
        
        let is_modifier = matches!(function_definition.ty, solidity::FunctionTy::Modifier);

        if !is_modifier || function_definition.body.is_none() {
            continue;
        }
        
        translate_modifier_definition(project, &mut translated_definition, function_definition)?;
    }

    // Translate each function
    for part in contract_definition.parts.iter() {
        let solidity::ContractPart::FunctionDefinition(function_definition) = part else { continue };

        let is_modifier = matches!(function_definition.ty, solidity::FunctionTy::Modifier);

        if is_modifier {
            continue;
        }

        translate_function_definition(project, &mut translated_definition, function_definition)?;
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
    

    project.translated_definitions.push(translated_definition);
    
    Ok(())
}

#[inline]
pub fn propagate_inherited_definitions(
    project: &mut Project,
    import_directives: &[solidity::Import],
    inherits: &[String],
    translated_definition: &mut TranslatedDefinition,
) -> Result<(), Error> {
    let source_unit_directory = translated_definition.path.parent().map(PathBuf::from).unwrap();

    for inherit in inherits.iter() {
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

            if filename.string.starts_with('@') {
                todo!("handle global import paths (i.e: node_modules)")
            }

            let import_path = source_unit_directory.join(filename.string.clone());

            if !import_path.exists() {
                return Err(
                    Error::Wrapped(Box::new(std::io::Error::new(std::io::ErrorKind::NotFound, format!("File not found: {}", import_path.to_string_lossy()))))
                );
            }

            let import_path = crate::get_canonical_path(import_path, false, false)
                .map_err(|e| Error::Wrapped(Box::new(e)))?;

            if let Some(t) = resolve_import(project, inherit, &import_path)? {
                inherited_definition = Some(t);
                break;
            }
        }

        // Check to see if the definition was defined in the current file
        if inherited_definition.is_none() {
            if let Some(t) = resolve_import(project, inherit, &translated_definition.path)? {
                inherited_definition = Some(t);
            }
        }

        let Some(inherited_definition) = inherited_definition else {
            panic!("Failed to find inherited definition \"{inherit}\" for \"{}\"", translated_definition.name);
        };

        // Extend the toplevel scope
        translated_definition.toplevel_scope.variables.extend(inherited_definition.toplevel_scope.variables.clone());
        translated_definition.toplevel_scope.functions.extend(inherited_definition.toplevel_scope.functions.clone());

        // Extend the use statements
        for inherited_use in inherited_definition.uses.iter() {
            if !translated_definition.uses.contains(inherited_use) {
                translated_definition.uses.push(inherited_use.clone());
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
            if !translated_definition.structs.contains(inherited_struct) {
                translated_definition.structs.push(inherited_struct.clone());
            }
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
                if !storage.fields.contains(inherited_field) {
                    storage.fields.push(inherited_field.clone());
                }
            }
        }

        // Extend the modifiers
        for inherited_modifier in inherited_definition.modifiers.iter() {
            if !translated_definition.modifiers.contains(inherited_modifier) {
                translated_definition.modifiers.push(inherited_modifier.clone());
            }
        }

        // Extend function name mapping
        for (signature, function_name) in inherited_definition.function_names.iter() {
            if !translated_definition.function_names.contains_key(signature) {
                translated_definition.function_names.insert(signature.clone(), function_name.clone());
            }
        }

        // Extend function name count mapping
        for (function_name, count) in inherited_definition.function_name_counts.iter() {
            *translated_definition.function_name_counts.entry(function_name.clone()).or_insert(0) += *count;
        }

        // Extend the functions
        for inherited_function in inherited_definition.functions.iter() {
            if !translated_definition.functions.contains(inherited_function) {
                translated_definition.functions.push(inherited_function.clone());

                if let Some(function_call_count) = inherited_definition.function_call_counts.get(&inherited_function.name) {
                    *translated_definition.function_call_counts.entry(inherited_function.name.clone()).or_insert(0) += *function_call_count;
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

                if !contract_impl.items.contains(&inherited_impl_item) {
                    contract_impl.items.push(inherited_impl_item.clone());
                }
            }
        }
    }

    Ok(())
}
