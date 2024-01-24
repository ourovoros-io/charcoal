use crate::{
    errors::Error,
    sway,
    translate::{
        TranslatedDefinition,
        TranslatedEnum,
        TranslatedFunction,
        TranslatedModifier,
        TranslatedVariable,
        TranslationScope,
    },
};
use convert_case::{Case, Casing};
use solang_parser::pt as solidity;
use std::{
    cell::RefCell,
    collections::HashMap,
    path::{Path, PathBuf},
    rc::Rc,
};

#[derive(Default)]
pub struct Project {
    line_ranges: HashMap<PathBuf, Vec<(usize, usize)>>,
    solidity_source_units: Rc<RefCell<HashMap<PathBuf, solidity::SourceUnit>>>,
    translated_definitions: Vec<TranslatedDefinition>,
}

impl Project {
    /// Attempts to parse the file from the supplied `path`.
    #[inline]
    fn parse_solidity_source_unit<P: AsRef<Path>>(&mut self, path: P) -> Result<(), Error> {
        let path = PathBuf::from(
            path.as_ref()
                .to_string_lossy()
                .replace("\\\\", "\\")
                .replace("//", "/")
        )
        .canonicalize()
        .map_err(|e| Error::Wrapped(Box::new(e)))?;
        
        let source = std::fs::read_to_string(path.clone())
            .map_err(|e| Error::Wrapped(Box::new(e)))?;
        
        self.load_line_ranges(path.clone(), source.as_str());

        let line_ranges = self.line_ranges.get(&path).unwrap();

        let (source_unit, _comments) = solang_parser::parse(source.as_str(), 0)
            .map_err(|e| Error::SolangDiagnostics(path.clone(), line_ranges.clone(), e))?;

        // TODO: do we need the comments for anything?

        self.solidity_source_units.borrow_mut().insert(path, source_unit);

        Ok(())
    }

    /// Loads line ranges in a specific file `path` from the provided `source` text.
    #[inline]
    fn load_line_ranges(&mut self, path: PathBuf, source: &str) {
        let mut line_range = (0usize, 0usize);

        for (i, c) in source.chars().enumerate() {
            if c == '\n' {
                line_range.1 = i;
                self.line_ranges.entry(path.clone()).or_default().push(line_range);
                line_range = (i + 1, 0);
            }
        }

        if line_range.1 > line_range.0 {
            self.line_ranges.entry(path.clone()).or_default().push(line_range);
        }
    }

    pub fn translate(&mut self, definition_name: Option<String>, source_unit_path: &Path) -> Result<(), Error> {
        let solidity_source_units = self.solidity_source_units.clone();

        // Ensure the source unit has been parsed
        if !solidity_source_units.borrow().contains_key(source_unit_path) {
            self.parse_solidity_source_unit(source_unit_path)?;
        }
        
        // Get the parsed source unit
        let source_unit = solidity_source_units.borrow().get(source_unit_path).unwrap().clone();

        // Collect import directives ahead of time for contextual reasons
        let mut import_directives = vec![];

        for source_unit_part in source_unit.0.iter() {
            let solidity::SourceUnitPart::ImportDirective(import_directive) = source_unit_part else { continue };
            import_directives.push(import_directive.clone());
        }

        // Handle the first translation pass
        for source_unit_part in source_unit.0.iter() {
            match source_unit_part {
                solidity::SourceUnitPart::PragmaDirective(_, _, _) => {
                    // TODO: check if any are actually important
                }
    
                solidity::SourceUnitPart::ImportDirective(_) => {
                    // NOTE: we don't need to handle this because we did already for the conversion queue
                }
    
                solidity::SourceUnitPart::ContractDefinition(contract_definition) => {
                    if let Some(definition_name) = definition_name.as_ref() {
                        if contract_definition.name.as_ref().unwrap().name != *definition_name {
                            continue;
                        }
                    }

                    self.translate_contract_definition(source_unit_path, import_directives.as_slice(), contract_definition)?;
                }
    
                solidity::SourceUnitPart::EnumDefinition(_) => {
                    todo!("toplevel enums")
                }
    
                solidity::SourceUnitPart::StructDefinition(_) => {
                    todo!("toplevel structs")
                }
    
                solidity::SourceUnitPart::EventDefinition(_) => {
                    unimplemented!("toplevel custom events")
                }
    
                solidity::SourceUnitPart::ErrorDefinition(_) => {
                    unimplemented!("toplevel custom errors")
                }
    
                solidity::SourceUnitPart::FunctionDefinition(_) => {
                    unimplemented!("toplevel functions")
                }
    
                solidity::SourceUnitPart::VariableDefinition(_) => {
                    unimplemented!("toplevel variable definitions")
                }
    
                solidity::SourceUnitPart::TypeDefinition(_) => {
                    unimplemented!("toplevel type definitions")
                }
    
                solidity::SourceUnitPart::Annotation(_) => {}
    
                solidity::SourceUnitPart::Using(_) => {
                    unimplemented!("toplevel using-for statements")
                }
    
                solidity::SourceUnitPart::StraySemicolon(_) => {}
            }
        }

        //
        // TODO:
        // Flatten final contract
        // Create forc project on disk
        //

        Ok(())
    }

    #[inline]
    fn translate_naming_convention(&mut self, name: &str, case: Case) -> String {
        let prefix = name.chars().take_while(|c| *c == '_').collect::<String>();
        let postfix = name.chars().rev().take_while(|c| *c == '_').collect::<String>();
        format!("{prefix}{}{postfix}", name.to_case(case))
    }

    #[inline]
    fn translate_function_name(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        function_definition: &solidity::FunctionDefinition,
    ) -> String {
        let mut signature = function_definition.name.as_ref().map(|i| i.name.clone()).unwrap_or_default();
        
        signature.push('(');
        
        for (i, (_, parameter)) in function_definition.params.iter().enumerate() {
            signature = format!(
                "{signature}{}{}",
                if i > 0 { ", " } else { "" },
                parameter.as_ref().unwrap().ty,
            );
        }

        signature.push(')');

        if !translated_definition.function_names.contains_key(&signature) {
            let old_name = function_definition.name.as_ref().map(|i| i.name.clone()).unwrap_or_default();
            let mut new_name = self.translate_naming_convention(old_name.as_str(), Case::Snake);

            let count = translated_definition.function_name_counts.entry(new_name.clone()).or_insert(0);
            *count += 1;

            if *count > 1 {
                new_name = format!("{new_name}_{}", *count);
            }

            translated_definition.function_names.insert(signature.clone(), new_name);
        }

        translated_definition.function_names.get(&signature).unwrap().clone()
    }

    #[inline]
    fn translate_storage_name(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        name: &str,
    ) -> String {
        if !translated_definition.storage_fields_names.contains_key(name) {
            let mut new_name = self.translate_naming_convention(name, Case::Snake);

            let count = translated_definition.storage_fields_name_counts.entry(new_name.clone()).or_insert(0);
            *count += 1;

            if *count > 1 {
                new_name = format!("{new_name}_{}", *count);
            }

            translated_definition.storage_fields_names.insert(name.into(), new_name);
        }

        translated_definition.storage_fields_names.get(name).unwrap().clone()
    }

    fn translate_type_name(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        type_name: &solidity::Expression,
        is_storage: bool,
    ) -> sway::TypeName {
        //
        // TODO: check mapping for previously canonicalized user type names?
        //

        match type_name {
            solidity::Expression::Type(_, type_expression) => match type_expression {
                solidity::Type::Address => sway::TypeName::Identifier {
                    name: "Identity".into(),
                    generic_parameters: None,
                },

                // TODO: should we note that this address was marked payable?
                solidity::Type::AddressPayable => sway::TypeName::Identifier {
                    name: "Identity".into(),
                    generic_parameters: None,
                },

                solidity::Type::Payable => todo!("payable types (used for casting)"),
                
                solidity::Type::Bool => sway::TypeName::Identifier {
                    name: "bool".into(),
                    generic_parameters: None,
                },

                solidity::Type::String => sway::TypeName::String {
                    length: 32, // TODO: is using a fixed 32-character string ok?
                },

                solidity::Type::Int(_) => todo!("signed integer types"),

                solidity::Type::Uint(bits) => sway::TypeName::Identifier {
                    name: match *bits {
                        8 => "u8".into(),
                        16 => "u16".into(),
                        32 => "u32".into(),
                        64 => "u64".into(),
                        256 => "u256".into(),
                        _ => todo!("unsigned integers of non-standard bit sizes")
                    },
                    generic_parameters: None,
                },

                solidity::Type::Bytes(length) => match *length {
                    32 => sway::TypeName::Identifier {
                        name: "b256".into(),
                        generic_parameters: None,
                    },

                    _ => sway::TypeName::Array {
                        type_name: Box::new(sway::TypeName::Identifier {
                            name: "u8".into(),
                            generic_parameters: None,
                        }),
                        length: *length as usize,
                    }
                },

                solidity::Type::Rational => todo!("rational types"),

                solidity::Type::DynamicBytes => sway::TypeName::Identifier {
                    name: "std::bytes::Bytes".into(), // TODO: is this ok?
                    generic_parameters: None,
                },

                solidity::Type::Mapping { key, value, .. } => sway::TypeName::Identifier {
                    name: "StorageMap".into(),
                    generic_parameters: Some(sway::GenericParameterList {
                        entries: vec![
                            sway::GenericParameter {
                                type_name: self.translate_type_name(translated_definition, key.as_ref(), is_storage),
                                implements: None,
                            },
                            sway::GenericParameter {
                                type_name: self.translate_type_name(translated_definition, value.as_ref(), is_storage),
                                implements: None,
                            },
                        ],
                    }),
                },

                solidity::Type::Function { .. } => todo!("function types"),
            }

            solidity::Expression::Variable(solidity::Identifier { name, .. }) => {
                // Check if type is a type definition
                if translated_definition.type_definitions.iter().any(|t| match &t.name {
                    sway::TypeName::Identifier { name: type_name, generic_parameters: None } if type_name == name => true,
                    _ => false,
                }) {
                    return sway::TypeName::Identifier {
                        name: name.clone(),
                        generic_parameters: None,
                    };
                }
                
                // Check if type is a struct
                if translated_definition.structs.iter().any(|t| t.name == *name) {
                    return sway::TypeName::Identifier {
                        name: name.clone(),
                        generic_parameters: None,
                    };
                }
                
                // Check if type is an enum
                if translated_definition.enums.iter().any(|t| match &t.type_definition.name {
                    sway::TypeName::Identifier { name: type_name, generic_parameters: None } => type_name == name,
                    _ => false,
                }) {
                    return sway::TypeName::Identifier {
                        name: name.clone(),
                        generic_parameters: None,
                    };
                }
                
                todo!("type name variable expression: {type_name:#?}")
            }

            solidity::Expression::ArraySubscript(_, type_name, length) => match length.as_ref() {
                Some(length) => sway::TypeName::Array {
                    type_name: Box::new(self.translate_type_name(translated_definition, type_name, is_storage)),
                    length: {
                        // Create an empty scope to translate the array length expression
                        let mut scope = TranslationScope {
                            parent: None,
                            variables: vec![],
                            functions: vec![],
                        };

                        match self.translate_expression(translated_definition, &mut scope, length.as_ref()) {
                            Ok(sway::Expression::Literal(sway::Literal::DecInt(length) | sway::Literal::HexInt(length))) => length as usize,
                            Ok(_) => panic!("Invalid array length expression: {length:#?}"),
                            Err(e) => panic!("Failed to translate array length expression: {e}"),
                        }
                    },
                },

                None => sway::TypeName::Identifier {
                    name: if is_storage {
                        // Ensure that `std::storage::storage_vec::*` is imported
                        translated_definition.ensure_use_declared("std::storage::storage_vec::*");

                        "StorageVec".into()
                    } else {
                        "Vec".into()
                    },
                    generic_parameters: Some(sway::GenericParameterList {
                        entries: vec![
                            sway::GenericParameter {
                                type_name: self.translate_type_name(translated_definition, type_name, is_storage),
                                implements: None,
                            },
                        ],
                    }),
                },
            }

            _ => unimplemented!("type name expression: {type_name:#?}"),
        }
    }

    #[inline]
    fn translate_contract_definition(
        &mut self,
        source_unit_path: &Path,
        import_directives: &[solidity::Import],
        solidity_definition: &solidity::ContractDefinition,
    ) -> Result<(), Error> {
        let definition_name = solidity_definition.name.as_ref().unwrap().name.clone();
        let inherits: Vec<String> = solidity_definition.base.iter().map(|b| b.name.identifiers.iter().map(|i| i.name.clone()).collect::<Vec<_>>().join(".")).collect();

        // Create a new translation container
        let mut translated_definition = TranslatedDefinition::new(
            source_unit_path,
            solidity_definition.ty.clone(),
            definition_name.clone(),
            inherits.clone()
        );

        // Translate import directives
        self.translate_import_directives(&mut translated_definition, import_directives)?;

        // Propagate inherited definitions
        self.propagate_inherited_definitions(import_directives, inherits.as_slice(), &mut translated_definition)?;

        // Collect each type definition ahead of time for contextual reasons
        for part in solidity_definition.parts.iter() {
            let solidity::ContractPart::TypeDefinition(type_definition) = part else { continue };
            
            let underlying_type = self.translate_type_name(&mut translated_definition, &type_definition.ty, false);

            translated_definition.type_definitions.push(sway::TypeDefinition {
                is_public: true,
                name: sway::TypeName::Identifier {
                    name: type_definition.name.name.clone(),
                    generic_parameters: None,
                },
                underlying_type: Some(underlying_type),
            });
        }

        // Collect each struct ahead of time for contextual reasons
        for part in solidity_definition.parts.iter() {
            let solidity::ContractPart::StructDefinition(struct_definition) = part else { continue };

            let struct_definition = sway::Struct {
                attributes: None,
                is_public: true,
                name: struct_definition.name.as_ref().unwrap().name.clone(),
                generic_parameters: None,
                fields: struct_definition.fields.iter().map(|f| {
                    sway::StructField {
                        is_public: true,
                        name: self.translate_naming_convention(f.name.as_ref().unwrap().name.as_str(), Case::Snake), // TODO: keep track of original name
                        type_name: self.translate_type_name(&mut translated_definition, &f.ty, false),
                    }
                }).collect(),
            };

            translated_definition.structs.push(struct_definition);
        }

        // Collect each enum ahead of time for contextual reasons
        for part in solidity_definition.parts.iter() {
            let solidity::ContractPart::EnumDefinition(enum_definition) = part else { continue };

            // Create the enum's type definition
            let type_definition = sway::TypeDefinition {
                is_public: false,
                name: sway::TypeName::Identifier {
                    name: enum_definition.name.as_ref().unwrap().name.clone(),
                    generic_parameters: None,
                },
                underlying_type: Some(sway::TypeName::Identifier {
                    name: "u8".into(),
                    generic_parameters: None,
                }),
            };
            
            // Create the enum's variants impl block
            let mut variants_impl = sway::Impl {
                generic_parameters: None,
                type_name: type_definition.name.clone(),
                for_type_name: None,
                items: vec![],
            };

            // Add each variant to the variants impl block
            for (i, value) in enum_definition.values.iter().enumerate() {
                variants_impl.items.push(sway::ImplItem::Constant(sway::Constant {
                    is_public: false,
                    name: self.translate_naming_convention(value.as_ref().unwrap().name.as_str(), Case::ScreamingSnake),
                    type_name: type_definition.name.clone(),
                    value: Some(sway::Expression::from(sway::Literal::DecInt(i as u64))),
                }));
            }

            // Add the translated enum to the translated definition
            translated_definition.enums.push(TranslatedEnum {
                type_definition,
                variants_impl,
            });
        }

        // Collect each event and error ahead of time for contextual reasons
        for part in solidity_definition.parts.iter() {
            match part {
                solidity::ContractPart::EventDefinition(event_definition) => {
                    let type_name = if event_definition.fields.len() == 1 {
                        self.translate_type_name(&mut translated_definition, &event_definition.fields[0].ty, false)
                    } else {
                        sway::TypeName::Tuple {
                            type_names: event_definition.fields.iter().map(|f| {
                                self.translate_type_name(&mut translated_definition, &f.ty, false)
                            }).collect(),
                        }
                    };

                    let (events_enum, _) = translated_definition.get_events_enum();

                    let variant = sway::EnumVariant {
                        name: event_definition.name.as_ref().unwrap().name.clone(),
                        type_name,
                    };

                    if !events_enum.variants.contains(&variant) {
                        events_enum.variants.push(variant);
                    }
                }

                solidity::ContractPart::ErrorDefinition(error_definition) => {
                    let type_name = if error_definition.fields.len() == 1 {
                        self.translate_type_name(&mut translated_definition, &error_definition.fields[0].ty, false)
                    } else {
                        sway::TypeName::Tuple {
                            type_names: error_definition.fields.iter().map(|f| {
                                self.translate_type_name(&mut translated_definition, &f.ty, false)
                            }).collect(),
                        }
                    };

                    let (errors_enum, _) = translated_definition.get_errors_enum();

                    let variant = sway::EnumVariant {
                        name: error_definition.name.as_ref().unwrap().name.clone(),
                        type_name,
                    };

                    if !errors_enum.variants.contains(&variant) {
                        errors_enum.variants.push(variant);
                    }
                }
                
                _ => {}
            }
        }

        // Create the abi encoding function for the events enum (if any)
        if let Some((events_enum, abi_encode_impl)) = translated_definition.events_enum.as_mut() {
            self.generate_enum_abi_encode_function(events_enum, abi_encode_impl)?;
        }

        // Create the abi encoding function for the errors enum (if any)
        if let Some((errors_enum, abi_encode_impl)) = translated_definition.errors_enum.as_mut() {
            self.generate_enum_abi_encode_function(errors_enum, abi_encode_impl)?;
        }

        // Resolve all using-for statements ahead of time for contextual reasons
        for part in solidity_definition.parts.iter() {
            let solidity::ContractPart::Using(using) = part else { continue };

            todo!("using-for statements: {} - {using:#?}", using.to_string())
        }

        // Collect each storage field ahead of time for contextual reasons
        for part in solidity_definition.parts.iter() {
            let solidity::ContractPart::VariableDefinition(variable_definition) = part else { continue };
            self.translate_state_variable(&mut translated_definition, variable_definition)?;
        }
        
        // Collect each toplevel function ahead of time for contextual reasons
        for part in solidity_definition.parts.iter() {
            let solidity::ContractPart::FunctionDefinition(function_definition) = part else { continue };

            let is_modifier = matches!(function_definition.ty, solidity::FunctionTy::Modifier);

            if is_modifier {
                continue;
            }
    
            // Add the toplevel function to the list of toplevel functions for the toplevel scope
            let function = self.translate_function_declaration(&mut translated_definition, function_definition)?;
            translated_definition.toplevel_scope.functions.push(function);
        }

        // Translate each modifier
        for part in solidity_definition.parts.iter() {
            let solidity::ContractPart::FunctionDefinition(function_definition) = part else { continue };
            
            let is_modifier = matches!(function_definition.ty, solidity::FunctionTy::Modifier);

            if !is_modifier || function_definition.body.is_none() {
                continue;
            }
            
            self.translate_modifier_definition(&mut translated_definition, function_definition)?;
        }

        // Translate each function
        for part in solidity_definition.parts.iter() {
            let solidity::ContractPart::FunctionDefinition(function_definition) = part else { continue };

            let is_modifier = matches!(function_definition.ty, solidity::FunctionTy::Modifier);

            if is_modifier {
                continue;
            }

            self.translate_function_definition(&mut translated_definition, function_definition)?;
        }

        // Look for toplevel functions that are never called, move their implementation to the abi wrapper function if it exists
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

        println!("// First translation pass of \"{}\" in \"{}\":", translated_definition.name, translated_definition.path.to_string_lossy());
        println!("{translated_definition}");

        self.translated_definitions.push(translated_definition);
        
        Ok(())
    }

    #[inline]
    fn translate_import_directives(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        import_directives: &[solidity::Import],
    ) -> Result<(), Error> {
        let source_unit_directory = translated_definition.path.parent().map(PathBuf::from).unwrap();

        for import_directive in import_directives.iter() {
            let mut translate_import_directive = |definition_name: Option<String>, filename: &solidity::StringLiteral| -> Result<(), Error> {
                if filename.string.starts_with('@') {
                    todo!("handle global import paths (i.e: node_modules)")
                }
    
                let import_path = source_unit_directory.join(filename.string.clone())
                    .canonicalize()
                    .map_err(|e| Error::Wrapped(Box::new(e)))?;
    
                if !self.translated_definitions.iter().any(|t| t.path == import_path) {
                    self.translate(definition_name, &import_path)?;
                }

                Ok(())
            };

            match import_directive {
                solidity::Import::Plain(solidity::ImportPath::Filename(filename), _) => {
                    translate_import_directive(None, filename)?;
                }

                solidity::Import::Rename(solidity::ImportPath::Filename(filename), identifiers, _) => {
                    for (identifier, _) in identifiers.iter() {
                        translate_import_directive(Some(identifier.name.clone()), filename)?;
                    }
                }

                _ => panic!("Unsupported import directive: {import_directive:#?}"),
            }
        }

        Ok(())
    }

    #[inline]
    fn propagate_inherited_definitions(
        &mut self,
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

                let import_path = source_unit_directory.join(filename.string.clone())
                    .canonicalize()
                    .map_err(|e| Error::Wrapped(Box::new(e)))?;

                if let Some(t) = self.translated_definitions.iter().find(|t| t.path == import_path && t.name == *inherit) {
                    inherited_definition = Some(t);
                    break;
                }
            }

            let Some(inherited_definition) = inherited_definition else {
                panic!("Failed to find inherited definition \"{inherit}\" for \"{}\"", translated_definition.name);
            };

            // Extend the toplevel scope
            translated_definition.toplevel_scope.variables.extend(inherited_definition.toplevel_scope.variables.clone());
            translated_definition.toplevel_scope.functions.extend(inherited_definition.toplevel_scope.functions.clone());

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
            if let Some((inherited_events_enum, _)) = inherited_definition.events_enum.as_ref() {
                let (events_enum, _) = translated_definition.get_events_enum();

                for inherited_variant in inherited_events_enum.variants.iter() {
                    if !events_enum.variants.contains(inherited_variant) {
                        events_enum.variants.push(inherited_variant.clone());
                    }
                }
            }

            // Extend the errors enum
            if let Some((inherited_errors_enum, _)) = inherited_definition.errors_enum.as_ref() {
                let (errors_enum, _) = translated_definition.get_errors_enum();

                for inherited_variant in inherited_errors_enum.variants.iter() {
                    if !errors_enum.variants.contains(inherited_variant) {
                        errors_enum.variants.push(inherited_variant.clone());
                    }
                }
            }

            // Extend the abi
            if let Some(inherited_abi) = inherited_definition.abi.as_ref() {
                let abi = translated_definition.get_abi();

                for inherited_function in inherited_abi.functions.iter() {
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
                let contract_impl = translated_definition.get_contract_impl();

                for inherited_impl_item in inherited_impl.items.iter() {
                    if !contract_impl.items.contains(inherited_impl_item) {
                        contract_impl.items.push(inherited_impl_item.clone());
                    }
                }
            }
        }

        Ok(())
    }

    #[inline]
    fn generate_enum_abi_encode_function(
        &mut self,
        sway_enum: &sway::Enum,
        abi_encode_impl: &mut sway::Impl,
    ) -> Result<(), Error> {
        let mut match_expr = sway::Match {
            expression: sway::Expression::Identifier("self".into()),
            branches: vec![],
        };

        for variant in sway_enum.variants.iter() {
            let mut block = sway::Block::default();

            // "VariantName".abi_encode(buffer);
            block.statements.push(sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::from(sway::MemberAccess {
                    expression: sway::Expression::from(sway::Literal::String(variant.name.clone())),
                    member: "abi_encode".into(),
                }),
                generic_parameters: None,
                parameters: vec![
                    sway::Expression::Identifier("buffer".into())
                ],
            })));

            let mut add_encode_statement_to_block = |name: &str, type_name: &sway::TypeName| {
                let sway::TypeName::Identifier { name: type_name, .. } = type_name else {
                    todo!("ABI encoding for enum parameter type: {type_name}");
                };

                block.statements.push(sway::Statement::from(match type_name.as_str() {
                    "u8" | "u16" | "u32" | "u64" | "u256" | "b256" => sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::from(sway::MemberAccess {
                            expression: sway::Expression::Identifier(name.into()),
                            member: "abi_encode".into(),
                        }),
                        generic_parameters: None,
                        parameters: vec![
                            sway::Expression::Identifier("buffer".into()),
                        ],
                    }),

                    "Identity" => {
                        let identity_variant_branch = |name: &str| -> sway::MatchBranch {
                            sway::MatchBranch {
                                pattern: sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::Identifier(format!("Identity::{name}")),
                                    generic_parameters: None,
                                    parameters: vec![
                                        sway::Expression::Identifier("x".into()),
                                    ],
                                }),
                                value: sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::from(sway::MemberAccess {
                                        expression: sway::Expression::Identifier("x".into()),
                                        member: "abi_encode".into(),
                                    }),
                                    generic_parameters: None,
                                    parameters: vec![
                                        sway::Expression::Identifier("buffer".into())
                                    ],
                                }),
                            }
                        };

                        sway::Expression::from(sway::Match {
                            expression: sway::Expression::Identifier(name.into()),
                            branches: vec![
                                identity_variant_branch("Address"),
                                identity_variant_branch("ContractId"),
                            ],
                        })
                    },

                    _ => todo!("encode enum member type: {type_name}"),
                }));
            };

            let parameter_count = match &variant.type_name {
                sway::TypeName::Tuple { type_names } => type_names.len(),
                _ => 1,
            };

            let parameter_names: Vec<String> = ('a'..'z').enumerate()
                .take_while(|(i, _)| *i < parameter_count)
                .map(|(_, c)| c.into())
                .collect();

            match &variant.type_name {
                sway::TypeName::Undefined => panic!("Undefined type name"),
                
                sway::TypeName::Identifier { .. } => add_encode_statement_to_block(&parameter_names[0], &variant.type_name),
                
                sway::TypeName::Tuple { type_names } => {
                    for (name, type_name) in parameter_names.iter().zip(type_names) {
                        add_encode_statement_to_block(name.as_str(), type_name);
                    }
                }

                type_name => todo!("ABI encoding for enum parameter type: {type_name}"),
            }

            match_expr.branches.push(sway::MatchBranch {
                pattern: sway::Expression::Identifier(format!(
                    "{}::{}{}",
                    sway_enum.name,
                    variant.name,
                    if parameter_count == 0 {
                        String::new()
                    } else if parameter_count == 1 {
                        format!("({})", parameter_names[0])
                    } else {
                        format!("(({}))", parameter_names.join(", "))
                    },
                )),
                value: sway::Expression::from(block),
            });
        }

        // Add the `abi_encode` function to the `core::codec::AbiEncode` impl
        abi_encode_impl.items.push(sway::ImplItem::Function(sway::Function {
            attributes: None,
            is_public: false,
            name: "abi_encode".into(),
            generic_parameters: None,
            parameters: sway::ParameterList {
                entries: vec![
                    sway::Parameter {
                        name: "self".into(),
                        type_name: None,
                        ..Default::default()
                    },
                    sway::Parameter {
                        is_ref: true,
                        is_mut: true,
                        name: "buffer".into(),
                        type_name: Some(sway::TypeName::Identifier {
                            name: "core::codec::Buffer".into(),
                            generic_parameters: None,
                        }),
                    },
                ],
            },
            return_type: None,
            body: Some(sway::Block {
                statements: vec![
                    sway::Statement::from(sway::Expression::from(match_expr)),
                ],
                final_expr: None,
            }),
        }));

        Ok(())
    }

    #[inline]
    fn translate_state_variable(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        variable_definition: &solidity::VariableDefinition,
    ) -> Result<(), Error> {
        // Collect information about the variable from its attributes
        let is_public = variable_definition.attrs.iter().any(|x| matches!(x, solidity::VariableAttribute::Visibility(solidity::Visibility::External(_) | solidity::Visibility::Public(_))));
        let is_constant = variable_definition.attrs.iter().any(|x| matches!(x, solidity::VariableAttribute::Constant(_)));
        let is_immutable = variable_definition.attrs.iter().any(|x| matches!(x, solidity::VariableAttribute::Immutable(_)));

        // If the state variable is not constant or immutable, it is a storage field
        let is_storage = !is_constant && !is_immutable;

        // If the state variable is immutable and not a constant, it is a configurable field
        let is_configurable = is_immutable && !is_constant;

        // Translate the variable's naming convention
        let old_name = variable_definition.name.as_ref().unwrap().name.clone();
        let new_name = if is_constant {
            self.translate_naming_convention(old_name.as_str(), Case::ScreamingSnake)
        } else {
            self.translate_storage_name(translated_definition, old_name.as_str())
        };

        // Translate the variable's type name
        let variable_type_name = self.translate_type_name(translated_definition, &variable_definition.ty, is_storage);

        // Ensure std::hash::Hash is imported for StorageMap storage fields
        translated_definition.ensure_use_declared("std::hash::Hash");

        // Handle constant variable definitions
        if is_constant {
            translated_definition.constants.push(sway::Constant {
                is_public,
                name: new_name.clone(),
                type_name: variable_type_name.clone(),
                value: if let Some(x) = variable_definition.initializer.as_ref() {
                    let x = self.translate_literal_expression(x)?;
                    Some(sway::Expression::create_value_expression(&variable_type_name, Some(&x)))
                } else {
                    None
                },
            });
        }
        // Handle immutable variable definitions
        else if is_immutable {
            //
            // TODO: we need to check if the value is supplied to the constructor and remove it from there
            //

            translated_definition.get_configurable().fields.push(sway::ConfigurableField {
                name: new_name.clone(), 
                type_name: variable_type_name.clone(),
                value: if let Some(x) = variable_definition.initializer.as_ref() {
                    let x = self.translate_literal_expression(x)?;
                    sway::Expression::create_value_expression(&variable_type_name, Some(&x))
                } else {
                    sway::Expression::create_value_expression(&variable_type_name, None)
                },
            });
        }
        // Handle regular state variable definitions
        else {
            translated_definition.get_storage().fields.push(sway::StorageField {
                name: new_name.clone(),
                type_name: variable_type_name.clone(),
                value: if let Some(x) = variable_definition.initializer.as_ref() {
                    let x = self.translate_literal_expression(x)?;
                    sway::Expression::create_value_expression(&variable_type_name, Some(&x))
                } else {
                    sway::Expression::create_value_expression(&variable_type_name, None)
                },
            });
        }
        
        // Add the storage variable for function scopes
        translated_definition.toplevel_scope.variables.push(TranslatedVariable {
            old_name,
            new_name: new_name.clone(),
            type_name: variable_type_name.clone(),
            is_storage,
            is_configurable,
            ..Default::default()
        });

        // Generate a getter function if the storage field is public
        if !is_public {
            return Ok(());
        }

        // Generate parameters and return type for the public getter function
        let mut parameters = vec![];
        let mut return_type = variable_type_name.clone();

        if let Some((inner_parameters, inner_return_type)) = variable_type_name.getter_function_parameters_and_return_type() {
            parameters = inner_parameters;
            return_type = inner_return_type;
        }

        // Create the function declaration for the abi
        let mut sway_function = sway::Function {
            attributes: Some(sway::AttributeList {
                attributes: if is_storage {
                    vec![
                        sway::Attribute {
                            name: "storage".into(),
                            parameters: Some(vec![
                                "read".into(),
                            ]),
                        },
                    ]
                } else {
                    vec![]
                },
            }),
            is_public: false,
            name: new_name.clone(),
            generic_parameters: None,
            parameters: sway::ParameterList {
                entries: parameters.iter().map(|(p, _)| p.clone()).collect(),
            },
            return_type: Some(return_type),
            body: None,
        };

        if let Some(abi) = translated_definition.abi.as_mut() {
            // Only add the function to the abi if it doesn't already exist
            if !abi.functions.contains(&sway_function) {
                abi.functions.push(sway_function.clone());
            }
        } else {
            // Add the function to the abi
            translated_definition.get_abi().functions.push(sway_function.clone());
        }

        // Create the body for the toplevel function
        sway_function.body = Some(sway::Block {
            statements: vec![],
            final_expr: Some(if is_storage {
                let mut expression = sway::Expression::from(sway::MemberAccess {
                    expression: sway::Expression::Identifier("storage".into()),
                    member: new_name.clone(),
                });

                for (parameter, needs_unwrap) in parameters.iter() {
                    expression = sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::from(sway::MemberAccess {
                            expression,
                            member: "get".into(),
                        }),
                        generic_parameters: None,
                        parameters: vec![
                            sway::Expression::Identifier(parameter.name.clone()),
                        ],
                    });

                    if *needs_unwrap {
                        expression = sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::from(sway::MemberAccess {
                                expression,
                                member: "unwrap".into(),
                            }),
                            generic_parameters: None,
                            parameters: vec![],
                        });
                    }
                }
                
                sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::from(sway::MemberAccess {
                        expression,
                        member: "read".into(),
                    }),
                    generic_parameters: None,
                    parameters: vec![],
                })
            } else {
                todo!("Handle getter function for non-storage variables")
            }),
        });

        // Add the toplevel function
        translated_definition.functions.push(sway_function.clone());

        // Create the body for the contract impl's function wrapper
        sway_function.body = Some(sway::Block {
            statements: vec![],
            // TODO: change for StorageMap getter functions
            final_expr: Some(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::Identifier(format!("::{}", sway_function.name)),
                generic_parameters: None,
                parameters: vec![],
            })),
        });

        // Create the function wrapper item for the contract impl block
        let impl_item = sway::ImplItem::Function(sway_function);

        if let Some(contract_impl) = translated_definition.find_contract_impl_mut() {
            // Only add the function wrapper to the contract impl if it doesn't already exist
            if !contract_impl.items.contains(&impl_item) {
                contract_impl.items.push(impl_item);
            }
        } else {
            // Add the function wrapper to the contract impl
            translated_definition.get_contract_impl().items.push(impl_item);
        }

        Ok(())
    }

    #[inline]
    fn translate_function_declaration(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        function_definition: &solidity::FunctionDefinition,
    ) -> Result<TranslatedFunction, Error> {
        // Collect information about the function from its type
        let is_constructor = matches!(function_definition.ty, solidity::FunctionTy::Constructor);
        let is_fallback = matches!(function_definition.ty, solidity::FunctionTy::Fallback);
        let is_receive = matches!(function_definition.ty, solidity::FunctionTy::Receive);

        let (old_name, new_name) = if is_constructor {
            (String::new(), "constructor".to_string())
        } else if is_fallback {
            (String::new(), "fallback".to_string())
        } else if is_receive {
            (String::new(), "receive".to_string())
        } else {
            let old_name = function_definition.name.as_ref().unwrap().name.clone();
            let new_name = self.translate_function_name(translated_definition, function_definition);
            (old_name, new_name)
        };

        // Create a scope for modifier invocation translations
        let mut scope = TranslationScope {
            parent: Some(Box::new(translated_definition.toplevel_scope.clone())),
            variables: vec![],
            functions: vec![],
        };

        // Add the function parameters to the scope
        scope.variables.extend(
            function_definition.params.iter().map(|(_, p)| {
                let old_name = p.as_ref().unwrap().name.as_ref().unwrap().name.clone();
                let new_name = self.translate_naming_convention(old_name.as_str(), Case::Snake);
                let type_name = self.translate_type_name(translated_definition, &p.as_ref().unwrap().ty, false);

                TranslatedVariable {
                    old_name,
                    new_name,
                    type_name,
                    ..Default::default()
                }
            })
        );

        let mut modifiers = vec![];
        
        // Translate the function's modifier invocations
        for attr in function_definition.attributes.iter() {
            let solidity::FunctionAttribute::BaseOrModifier(_, base) = attr else { continue };

            let old_name = base.name.identifiers.iter().map(|i| i.name.clone()).collect::<Vec<_>>().join(".");
            let new_name = self.translate_naming_convention(old_name.as_str(), Case::Snake);

            modifiers.push(sway::FunctionCall {
                function: sway::Expression::Identifier(new_name),
                generic_parameters: None,
                parameters: base.args.as_ref()
                    .map(|args| args.iter().map(|a| self.translate_expression(translated_definition, &mut scope, a)).collect::<Result<Vec<_>, _>>())
                    .unwrap_or_else(|| Ok(vec![]))?,
            });
        }

        // Translate the function
        let translated_function = TranslatedFunction {
            old_name,
            new_name,
            parameters: sway::ParameterList {
                entries: function_definition.params.iter().map(|(_, p)| {
                    sway::Parameter {
                        name: self.translate_naming_convention(p.as_ref().unwrap().name.as_ref().unwrap().name.as_str(), Case::Snake),
                        type_name: Some(self.translate_type_name(translated_definition, &p.as_ref().unwrap().ty, false)),
                        ..Default::default()
                    }
                }).collect(),
            },
            modifiers,
            return_type: if function_definition.returns.is_empty() {
                None
            } else {
                Some(if function_definition.returns.len() == 1 {
                    self.translate_type_name(translated_definition, &function_definition.returns[0].1.as_ref().unwrap().ty, false)
                } else {
                    sway::TypeName::Tuple {
                        type_names: function_definition.returns.iter().map(|(_, p)| {
                            self.translate_type_name(translated_definition, &p.as_ref().unwrap().ty, false)
                        }).collect(),
                    }
                })
            },
        };

        // Unbox the inner scope
        translated_definition.toplevel_scope = *scope.parent.as_ref().unwrap().clone();

        Ok(translated_function)
    }

    #[inline]
    fn translate_modifier_definition(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        function_definition: &solidity::FunctionDefinition,
    ) -> Result<(), Error> {
        let old_name = function_definition.name.as_ref().unwrap().name.clone();
        let new_name = self.translate_naming_convention(old_name.as_str(), Case::Snake);

        let mut modifier = TranslatedModifier {
            old_name,
            new_name,
            parameters: sway::ParameterList::default(),
            has_underscore: false,
            pre_body: None,
            post_body: None,
        };

        let mut scope = TranslationScope {
            parent: Some(Box::new(translated_definition.toplevel_scope.clone())),
            variables: vec![],
            functions: vec![],
        };

        for (_, p) in function_definition.params.iter() {
            let old_name = p.as_ref().unwrap().name.as_ref().unwrap().name.clone();
            let new_name = self.translate_naming_convention(old_name.as_str(), Case::Snake);
            let type_name = self.translate_type_name(translated_definition, &p.as_ref().unwrap().ty, false);

            modifier.parameters.entries.push(sway::Parameter {
                name: new_name.clone(),
                type_name: Some(type_name.clone()),
                ..Default::default()
            });

            scope.variables.push(TranslatedVariable {
                old_name,
                new_name,
                type_name,
                ..Default::default()
            });
        }

        let solidity::Statement::Block { statements, .. } = function_definition.body.as_ref().unwrap() else {
            panic!("Invalid modifier body, expected block, found: {:#?}", function_definition.body);
        };

        let mut current_body: &mut Option<sway::Block> = &mut modifier.pre_body;
        let mut current_scope = scope.clone();

        let mut has_pre_storage_read = false;
        let mut has_pre_storage_write = false;

        let mut has_post_storage_read = false;
        let mut has_post_storage_write = false;

        let mut has_storage_read = &mut has_pre_storage_read;
        let mut has_storage_write = &mut has_pre_storage_write;

        for statement in statements.iter() {
            // If we encounter the underscore statement, every following statement goes into the modifier's post_body block.
            if let solidity::Statement::Expression(_, solidity::Expression::Variable(solidity::Identifier { name, .. })) = statement {
                if name == "_" {
                    modifier.has_underscore = true;
                    
                    if let Some(block) = current_body.as_mut() {
                        for variable in current_scope.variables.iter() {
                            if variable.is_storage {
                                if variable.read_count != 0 {
                                    *has_storage_read = true;
                                }

                                if variable.mutation_count != 0 {
                                    *has_storage_write = true;
                                }
                            }
                        }

                        self.finalize_block_translation(&current_scope, block)?;
                    }

                    current_body = &mut modifier.post_body;
                    current_scope = scope.clone();

                    has_storage_read = &mut has_post_storage_read;
                    has_storage_write = &mut has_post_storage_write;

                    continue;
                }
            }

            // Create the current body block if it hasn't already been.
            if current_body.is_none() {
                *current_body = Some(sway::Block::default());
            }

            let block = current_body.as_mut().unwrap();

            // Translate the statement
            let sway_statement = self.translate_statement(translated_definition, &mut current_scope, statement)?;

            // Store the index of the sway statement
            let statement_index = block.statements.len();

            // Add the sway statement to the sway block
            block.statements.push(sway_statement);

            // If the sway statement is a variable declaration, keep track of its statement index
            if let Some(sway::Statement::Let(sway_variable)) = block.statements.last() {
                let mut store_variable_statement_index = |id: &sway::LetIdentifier| {
                    let scope_entry = scope.variables.iter_mut().rev().find(|v| v.new_name == id.name).unwrap();
                    scope_entry.statement_index = Some(statement_index);
                };

                match &sway_variable.pattern {
                    sway::LetPattern::Identifier(id) => store_variable_statement_index(id),
                    sway::LetPattern::Tuple(ids) => ids.iter().for_each(store_variable_statement_index),
                }
            }
        }

        for variable in current_scope.variables.iter() {
            if variable.is_storage {
                if variable.read_count != 0 {
                    *has_storage_read = true;
                }

                if variable.mutation_count != 0 {
                    *has_storage_write = true;
                }
            }
        }

        if let Some(block) = current_body.as_mut() {
            self.finalize_block_translation(&current_scope, block)?;
        }

        let create_attributes = |has_storage_read: bool, has_storage_write: bool| -> Option<sway::AttributeList> {
            let mut parameters = vec![];

            if has_storage_read {
                parameters.push("read".to_string());
            }

            if has_storage_write {
                parameters.push("write".to_string());
            }
            
            if parameters.is_empty() {
                None
            } else {
                Some(sway::AttributeList {
                    attributes: vec![
                        sway::Attribute {
                            name: "storage".into(),
                            parameters: Some(parameters),
                        },
                    ],
                })
            }
        };

        // Ensure that an underscore statement was encountered while translating the modifier
        if !modifier.has_underscore {
            panic!("Malformed modifier missing underscore statement: {}", modifier.old_name);
        }

        // Generate toplevel modifier functions
        match (modifier.pre_body.as_ref(), modifier.post_body.as_ref()) {
            (Some(pre_body), Some(post_body)) => {
                translated_definition.functions.push(sway::Function {
                    attributes: create_attributes(has_pre_storage_read, has_pre_storage_write),
                    is_public: false,
                    name: format!("{}_pre", modifier.new_name),
                    generic_parameters: None,
                    parameters: modifier.parameters.clone(),
                    return_type: None,
                    body: Some(pre_body.clone()),
                });

                translated_definition.functions.push(sway::Function {
                    attributes: create_attributes(has_post_storage_read, has_post_storage_write),
                    is_public: false,
                    name: format!("{}_post", modifier.new_name),
                    generic_parameters: None,
                    parameters: modifier.parameters.clone(),
                    return_type: None,
                    body: Some(post_body.clone()),
                });
            }

            (Some(pre_body), None) => {
                translated_definition.functions.push(sway::Function {
                    attributes: create_attributes(has_pre_storage_read, has_pre_storage_write),
                    is_public: false,
                    name: modifier.new_name.clone(),
                    generic_parameters: None,
                    parameters: modifier.parameters.clone(),
                    return_type: None,
                    body: Some(pre_body.clone()),
                });
            }

            (None, Some(post_body)) => {
                translated_definition.functions.push(sway::Function {
                    attributes: create_attributes(has_post_storage_read, has_post_storage_write),
                    is_public: false,
                    name: modifier.new_name.clone(),
                    generic_parameters: None,
                    parameters: modifier.parameters.clone(),
                    return_type: None,
                    body: Some(post_body.clone()),
                });
            }

            (None, None) => {
                panic!("Malformed modifier missing pre and post bodies");
            }
        }
        
        // Add the translated modifier to the translated definition
        translated_definition.modifiers.push(modifier);

        // Unbox the inner scope
        translated_definition.toplevel_scope = *scope.parent.as_ref().unwrap().clone();

        Ok(())
    }

    #[inline]
    fn translate_function_definition(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        function_definition: &solidity::FunctionDefinition,
    ) -> Result<(), Error> {
        // Collect information about the function from its type
        let is_constructor = matches!(function_definition.ty, solidity::FunctionTy::Constructor);
        let is_fallback = matches!(function_definition.ty, solidity::FunctionTy::Fallback);
        let is_receive = matches!(function_definition.ty, solidity::FunctionTy::Receive);

        // Collect information about the function from its attributes
        let mut is_public = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Visibility(solidity::Visibility::External(_) | solidity::Visibility::Public(_))));
        let is_constant = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Mutability(solidity::Mutability::Constant(_))));
        let is_pure = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Mutability(solidity::Mutability::Pure(_))));
        let is_view = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Mutability(solidity::Mutability::View(_))));
        let is_payable = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Mutability(solidity::Mutability::Payable(_))));
        let _is_virtual = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Virtual(_)));
        let _is_override = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Override(_, _)));

        // If the function is a constructor, we consider it public and add an initializer requirement
        if is_constructor {
            is_public = true;
        }

        //
        // TODO:
        // Handle virtual functions
        // Handle function overrides
        //

        let (old_name, new_name) = if is_constructor {
            (String::new(), "constructor".to_string())
        } else if is_fallback {
            (String::new(), "fallback".to_string())
        } else if is_receive {
            (String::new(), "receive".to_string())
        } else {
            let old_name = function_definition.name.as_ref().unwrap().name.clone();
            let new_name = self.translate_function_name(translated_definition, function_definition);
            (old_name, new_name)
        };

        // Create the function declaration
        let mut sway_function = sway::Function {
            attributes: if is_constant || is_pure {
                None
            } else {
                let mut attributes = vec![];
                
                attributes.push(sway::Attribute {
                    name: "storage".into(),
                    parameters: Some(
                        if is_view {
                            vec!["read".into()]
                        } else {
                            vec!["read".into(), "write".into()]
                        }
                    ),
                });

                if is_payable {
                    attributes.push(sway::Attribute {
                        name: "payable".into(),
                        parameters: None,
                    });
                }

                Some(sway::AttributeList { attributes })
            },

            is_public: false,
            name: new_name.clone(),
            generic_parameters: None,

            parameters: sway::ParameterList {
                entries: function_definition.params.iter().map(|(_, p)| {
                    sway::Parameter {
                        name: self.translate_naming_convention(p.as_ref().unwrap().name.as_ref().unwrap().name.as_str(), Case::Snake),
                        type_name: Some(self.translate_type_name(translated_definition, &p.as_ref().unwrap().ty, false)),
                        ..Default::default()
                    }
                }).collect(),
            },

            return_type: if function_definition.returns.is_empty() {
                None
            } else {
                Some(if function_definition.returns.len() == 1 {
                    self.translate_type_name(translated_definition, &function_definition.returns[0].1.as_ref().unwrap().ty, false)
                } else {
                    sway::TypeName::Tuple {
                        type_names: function_definition.returns.iter().map(|(_, p)| {
                            self.translate_type_name(translated_definition, &p.as_ref().unwrap().ty, false)
                        }).collect(),
                    }
                })
            },

            body: None,
        };

        if is_public {
            if let Some(abi) = translated_definition.abi.as_mut() {
                // Only add the function to the abi if it doesn't already exist
                if !abi.functions.contains(&sway_function) {
                    abi.functions.push(sway_function.clone());
                }
            } else {
                // Add the function to the abi
                translated_definition.get_abi().functions.push(sway_function.clone());
            }
        }

        // Convert the statements in the function's body (if any)
        let Some(solidity::Statement::Block { statements, .. }) = function_definition.body.as_ref() else { return Ok(()) };

        // Create the scope for the body of the toplevel function
        let mut scope = TranslationScope {
            parent: Some(Box::new(translated_definition.toplevel_scope.clone())),
            variables: vec![],
            functions: vec![],
        };

        // Add the function parameters to the scope
        let mut parameters = vec![];

        for (_, p) in function_definition.params.iter() {
            let old_name = p.as_ref().unwrap().name.as_ref().unwrap().name.clone();
            let new_name = self.translate_naming_convention(old_name.as_str(), Case::Snake);
            let type_name = self.translate_type_name(translated_definition, &p.as_ref().unwrap().ty, false);

            let translated_variable = TranslatedVariable {
                old_name,
                new_name,
                type_name,
                ..Default::default()
            };

            parameters.push(translated_variable.clone());
            scope.variables.push(translated_variable);
        }

        // Add the function's named return parameters to the scope
        let mut return_parameters = vec![];

        for (_, return_parameter) in function_definition.returns.iter() {
            let Some(return_parameter) = return_parameter else { continue };
            let Some(old_name) = return_parameter.name.as_ref().map(|n| n.name.clone()) else { continue };
            let new_name = self.translate_naming_convention(old_name.as_str(), Case::Snake);
            let type_name = self.translate_type_name(translated_definition, &return_parameter.ty, false);

            let translated_variable = TranslatedVariable {
                old_name,
                new_name,
                type_name,
                ..Default::default()
            };

            return_parameters.push(translated_variable.clone());
            scope.variables.push(translated_variable);
        }

        // Translate the body for the toplevel function
        let mut function_body = self.translate_block(translated_definition, &mut scope, statements.as_slice())?;

        if is_constructor {
            let name =  self.translate_storage_name(translated_definition, "initialized");
            let type_name = sway::TypeName::Identifier {
                name: "bool".into(),
                generic_parameters: None,
            };

            // Add the `initialized` field to the storage block
            translated_definition.get_storage().fields.push(sway::StorageField {
                name: name.clone(),
                type_name: type_name.clone(),
                value: sway::Expression::create_value_expression(
                    &type_name,
                    Some(&sway::Expression::from(sway::Literal::Bool(false))),
                ),
            });

            // Add the `initialized` requirement to the beginning of the function
            // require(!storage.initialized.read(), "Contract is already initialized");
            function_body.statements.insert(0, sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::from(sway::Expression::Identifier("require".into())),
                generic_parameters: None,
                parameters: vec![
                    sway::Expression::from(sway::UnaryExpression {
                        operator: "!".into(),
                        expression: sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::from(sway::MemberAccess {
                                expression: sway::Expression::from(sway::MemberAccess {
                                    expression: sway::Expression::Identifier("storage".into()),
                                    member: name.clone(),
                                }),
                                member: "read".into(),
                            }),
                            generic_parameters: None,
                            parameters: vec![],
                        })
                    }),
                    sway::Expression::from(sway::Literal::String("Contract is already initialized".into())),
                ],
            })));

            // Set the `initialized` storage field to `true` at the end of the function
            // storage.initialized.write(true);
            function_body.statements.push(sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::from(sway::MemberAccess {
                    expression: sway::Expression::from(sway::MemberAccess {
                        expression: sway::Expression::Identifier("storage".into()),
                        member: name.clone(),
                    }),
                    member: "write".into(),
                }),
                generic_parameters: None,
                parameters: vec![
                    sway::Expression::from(sway::Literal::Bool(true)),
                ],
            })));
        }

        // Check for parameters that were mutated and make them local variables
        for parameter in parameters.iter().rev() {
            let variable = match scope.get_variable_from_new_name(&parameter.new_name) {
                Ok(variable) => variable,
                Err(e) => panic!("{e}"),
            };

            if variable.mutation_count > 0 {
                function_body.statements.insert(0, sway::Statement::Let(sway::Let {
                    pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                        is_mutable: true,
                        name: parameter.new_name.clone(),
                    }),
                    type_name: Some(parameter.type_name.clone()),
                    value: sway::Expression::Identifier(parameter.new_name.clone()),
                }));
            }
        }

        // Propagate the return variable declarations
        for return_parameter in return_parameters.iter().rev() {
            function_body.statements.insert(0, sway::Statement::Let(sway::Let {
                pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                    is_mutable: true,
                    name: return_parameter.new_name.clone(),
                }),
                type_name: Some(return_parameter.type_name.clone()),
                value: sway::Expression::create_value_expression(&return_parameter.type_name, None),
            }));
        }

        // If the function returns values but doesn't end in a return statement, propagate the return variables
        if !return_parameters.is_empty() && !matches!(function_body.statements.last(), Some(sway::Statement::Expression(sway::Expression::Return(_)))) {
            function_body.statements.push(sway::Statement::from(sway::Expression::Return(Some(Box::new(
                if return_parameters.len() == 1 {
                    sway::Expression::Identifier(
                        return_parameters[0].new_name.clone()
                    )
                } else {
                    sway::Expression::Tuple(
                        return_parameters.iter().map(|p| sway::Expression::Identifier(p.new_name.clone())).collect()
                    )
                }
            )))));
        }

        // Check if the final statement returns a value and change it to be the final expression of the block
        if let Some(sway::Statement::Expression(sway::Expression::Return(Some(value)))) = function_body.statements.last().cloned() {
            function_body.statements.pop();
            function_body.final_expr = Some(*value);
        }

        // Get the function from the scope
        let function = scope.get_function_from_old_name(old_name.as_str())?;

        // Propagate modifier pre and post functions into the function's body
        let mut modifier_pre_calls = vec![];
        let mut modifier_post_calls = vec![];

        for modifier_invocation in function.modifiers.iter() {
            let sway::Expression::Identifier(new_name) = &modifier_invocation.function else {
                panic!("Malformed modifier invocation: {modifier_invocation:#?}");
            };
            
            let Some(modifier) = translated_definition.modifiers.iter().find(|v| v.new_name == *new_name) else {
                panic!("Failed to find modifier: {new_name}");
            };

            if modifier.pre_body.is_some() && modifier.post_body.is_some() {
                modifier_pre_calls.push(sway::FunctionCall {
                    function: sway::Expression::Identifier(format!("{}_pre", modifier.new_name)),
                    generic_parameters: None,
                    parameters: modifier_invocation.parameters.clone(),
                });

                modifier_post_calls.push(sway::FunctionCall {
                    function: sway::Expression::Identifier(format!("{}_post", modifier.new_name)),
                    generic_parameters: None,
                    parameters: modifier_invocation.parameters.clone(),
                });
            } else if modifier.pre_body.is_some() {
                modifier_pre_calls.push(sway::FunctionCall {
                    function: sway::Expression::Identifier(modifier.new_name.clone()),
                    generic_parameters: None,
                    parameters: modifier_invocation.parameters.clone(),
                });
            } else if modifier.post_body.is_some() {
                modifier_post_calls.push(sway::FunctionCall {
                    function: sway::Expression::Identifier(modifier.new_name.clone()),
                    generic_parameters: None,
                    parameters: modifier_invocation.parameters.clone(),
                });
            }
        }

        for modifier_pre_call in modifier_pre_calls.iter().rev() {
            function_body.statements.insert(0, sway::Statement::from(sway::Expression::from(modifier_pre_call.clone())));
        }

        for modifier_post_call in modifier_post_calls.iter().rev() {
            function_body.statements.push(sway::Statement::from(sway::Expression::from(modifier_post_call.clone())));
        }

        // Create the body for the toplevel function
        sway_function.body = Some(function_body);

        // Add the toplevel function
        translated_definition.functions.push(sway_function.clone());

        if is_public {
            // Create the body for the contract impl's function wrapper
            sway_function.body = Some(sway::Block {
                statements: vec![],
                final_expr: Some(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::Identifier(format!("::{}", sway_function.name)),
                    generic_parameters: None,
                    parameters: sway_function.parameters.entries.iter().map(|p| sway::Expression::Identifier(p.name.clone())).collect(),
                })),
            });

            // Create the function wrapper item for the contract impl block
            let impl_item = sway::ImplItem::Function(sway_function);

            if let Some(contract_impl) = translated_definition.find_contract_impl_mut() {
                // Only add the function wrapper to the contract impl if it doesn't already exist
                if !contract_impl.items.contains(&impl_item) {
                    contract_impl.items.push(impl_item);
                }
            } else {
                // Add the function wrapper to the contract impl
                translated_definition.get_contract_impl().items.push(impl_item);
            }
        }

        // Unbox the inner scope
        translated_definition.toplevel_scope = *scope.parent.as_ref().unwrap().clone();

        Ok(())
    }

    fn translate_block(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        statements: &[solidity::Statement]
    ) -> Result<sway::Block, Error> {
        let mut block = sway::Block::default();

        // Translate each of the statements in the block
        for statement in statements {
            // Translate the statement
            let sway_statement = self.translate_statement(translated_definition, scope, statement)?;

            // Store the index of the sway statement
            let statement_index = block.statements.len();

            // Add the sway statement to the sway block
            block.statements.push(sway_statement);

            // If the sway statement is a variable declaration, keep track of its statement index
            if let Some(sway::Statement::Let(sway_variable)) = block.statements.last() {
                let mut store_variable_statement_index = |id: &sway::LetIdentifier| {
                    if id.name == "_" {
                        return;
                    }
                    let scope_entry = scope.variables.iter_mut().rev().find(|v| v.new_name == id.name).unwrap();
                    scope_entry.statement_index = Some(statement_index);
                };

                match &sway_variable.pattern {
                    sway::LetPattern::Identifier(id) => store_variable_statement_index(id),
                    sway::LetPattern::Tuple(ids) => ids.iter().for_each(store_variable_statement_index),
                }
            }
        }

        self.finalize_block_translation(scope, &mut block)?;

        Ok(block)
    }

    fn finalize_block_translation(
        &mut self,
        scope: &TranslationScope,
        block: &mut sway::Block,
    ) -> Result<(), Error> {
        // Check the block for variable declarations that need to be marked mutable
        for variable in scope.variables.iter() {
            // Only check variables that are declared as statements
            let Some(statement_index) = variable.statement_index else { continue };

            // If the variable has any mutations, mark it as mutable
            if variable.mutation_count > 0 {
                let let_statement = match &mut block.statements[statement_index] {
                    sway::Statement::Let(let_statement) => let_statement,
                    statement => panic!("Expected let statement, found: {statement:?}"),
                };

                let mark_let_identifier_mutable = |id: &mut sway::LetIdentifier| {
                    if id.name == variable.new_name {
                        id.is_mutable = true;
                    }
                };

                match &mut let_statement.pattern {
                    sway::LetPattern::Identifier(id) => mark_let_identifier_mutable(id),
                    sway::LetPattern::Tuple(ids) => ids.iter_mut().for_each(mark_let_identifier_mutable),
                }
            }
        }

        // Check block for sub-blocks that don't contain shadowing variable declarations and flatten them
        for i in (0..block.statements.len()).rev() {
            let mut statements = None;

            {
                let sway::Statement::Expression(sway::Expression::Block(sub_block)) = &block.statements[i] else { continue };
                
                let mut var_count = 0;

                for statement in sub_block.statements.iter() {
                    let sway::Statement::Let(sway::Let { pattern, .. }) = statement else { continue };

                    let mut check_let_identifier = |identifier: &sway::LetIdentifier| {
                        if let Some(scope) = scope.parent.as_ref() {
                            if scope.get_variable_from_new_name(&identifier.name).is_ok() {
                                var_count += 1;
                            }
                        }
                    };

                    match pattern {
                        sway::LetPattern::Identifier(identifier) => {
                            check_let_identifier(identifier);
                        }

                        sway::LetPattern::Tuple(identifiers) => {
                            for identifier in identifiers.iter() {
                                check_let_identifier(identifier);
                            }
                        }
                    }
                }

                if var_count == 0 {
                    statements = Some(sub_block.statements.clone());
                }
            }

            if let Some(statements) = statements {
                block.statements.remove(i);

                for statement in statements.into_iter().rev() {
                    block.statements.insert(i, statement);
                }
            }
        }

        // If the last statement is a block, flatten it
        if let Some(sway::Statement::Expression(sway::Expression::Block(inner_block))) = block.statements.last().cloned() {
            block.statements.pop();
            block.statements.extend(inner_block.statements);
        }

        Ok(())
    }

    // --------------------------------------------------------------------------------
    // STATEMENT TRANSLATION CODE
    // --------------------------------------------------------------------------------

    fn translate_statement(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        statement: &solidity::Statement
    ) -> Result<sway::Statement, Error> {
        match statement {
            solidity::Statement::Block { statements, .. } => self.translate_block_statement(translated_definition, scope, statements),
            solidity::Statement::Assembly { dialect, flags, block, .. } => self.translate_assembly_statement(translated_definition, scope, dialect, flags, block),
            solidity::Statement::Args(_, named_arguments) => self.translate_args_statement(translated_definition, scope, named_arguments),
            solidity::Statement::If(_, condition, then_body, else_if) => self.translate_if_statement(translated_definition, scope, condition, then_body, else_if),
            solidity::Statement::While(_, condition, body) => self.translate_while_statement(translated_definition, scope, condition, body),
            solidity::Statement::Expression(_, expression) => self.translate_expression_statement(translated_definition, scope, expression),
            solidity::Statement::VariableDefinition(_, variable_declaration, initializer) => self.translate_variable_definition_statement(translated_definition, scope, variable_declaration, initializer),
            solidity::Statement::For(_, initialization, condition, update, body) => self.translate_for_statement(translated_definition, scope, initialization, condition, update, body),
            solidity::Statement::DoWhile(_, _, _) => todo!("translate do while statement: {statement:#?}"),
            solidity::Statement::Continue(_) => Ok(sway::Statement::from(sway::Expression::Continue)),
            solidity::Statement::Break(_) => Ok(sway::Statement::from(sway::Expression::Break)),
            solidity::Statement::Return(_, expression) => self.translate_return_statement(translated_definition, scope, expression),
            solidity::Statement::Revert(_, error_type, parameters) => self.translate_revert_statement(translated_definition, scope, error_type, parameters),
            solidity::Statement::RevertNamedArgs(_, _, _) => todo!("translate revert named args statement: {statement:#?}"),
            solidity::Statement::Emit(_, expression) => self.translate_emit_statement(translated_definition, scope, expression),
            solidity::Statement::Try(_, _, _, _) => todo!("translate try statement: {statement:#?}"),
            solidity::Statement::Error(_) => panic!("Encountered a statement that was not parsed correctly"),
        }
    }
    
    #[inline]
    fn translate_block_statement(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        statements: &[solidity::Statement],
    ) -> Result<sway::Statement, Error> {
        let mut inner_scope = TranslationScope {
            parent: Some(Box::new(scope.clone())),
            variables: vec![],
            functions: vec![],
        };

        // Translate the block
        let translated_block = sway::Statement::from(sway::Expression::from(
            self.translate_block(translated_definition, &mut inner_scope, statements)?
        ));

        // Unbox the inner scope
        *scope = *inner_scope.parent.as_ref().unwrap().clone();

        Ok(translated_block)
    }
    
    #[inline]
    fn translate_args_statement(
        &mut self,
        _translated_definition: &mut TranslatedDefinition,
        _scope: &mut TranslationScope,
        _named_arguments: &[solidity::NamedArgument],
    ) -> Result<sway::Statement, Error> {
        todo!("translate args statement")
    }
    
    #[inline]
    fn translate_if_statement(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        condition: &solidity::Expression,
        then_body: &solidity::Statement,
        else_if: &Option<Box<solidity::Statement>>,
    ) -> Result<sway::Statement, Error> {
        let condition = self.translate_expression(translated_definition, scope, condition)?;
        
        let then_body = match self.translate_statement(translated_definition, scope, then_body)? {
            sway::Statement::Expression(sway::Expression::Block(block)) => *block,
            
            statement => sway::Block {
                statements: vec![statement],
                final_expr: None,
            }
        };

        let else_if = if let Some(else_if) = else_if.as_ref() {
            match self.translate_statement(translated_definition, scope, else_if.as_ref())? {
                sway::Statement::Expression(sway::Expression::If(else_if)) => Some(else_if.clone()),
                sway::Statement::Expression(sway::Expression::Block(block)) => Some(Box::new(sway::If {
                    condition: None,
                    then_body: *block,
                    else_if: None,
                })),
                statement => Some(Box::new(sway::If {
                    condition: None,
                    then_body: sway::Block {
                        statements: vec![statement],
                        final_expr: None,
                    },
                    else_if: None,
                })),
            }
        } else {
            None
        };

        Ok(sway::Statement::from(sway::Expression::from(sway::If {
            condition: Some(condition),
            then_body,
            else_if,
        })))
    }
    
    #[inline]
    fn translate_while_statement(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        condition: &solidity::Expression,
        body: &solidity::Statement,
    ) -> Result<sway::Statement, Error> {
        Ok(sway::Statement::from(sway::Expression::from(sway::While {
            condition: self.translate_expression(translated_definition, scope, condition)?,
            body: match self.translate_statement(translated_definition, scope, body)? {
                sway::Statement::Expression(sway::Expression::Block(block)) => *block,
                statement => sway::Block {
                    statements: vec![statement],
                    final_expr: None,
                }
            },
        })))
    }
    
    #[inline]
    fn translate_expression_statement(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        expression: &solidity::Expression,
    ) -> Result<sway::Statement, Error> {
        match expression {
            // Check for an assignment expression where lhs is a list expression
            solidity::Expression::Assign(_, lhs, rhs) => {
                if let solidity::Expression::List(_, parameters) = lhs.as_ref() {
                    // Collect variable translations for the scope
                    let mut variables = vec![];
    
                    for (_, p) in parameters.iter() {
                        let Some(p) = p.as_ref() else { continue };
                        let Some(name) = p.name.as_ref() else { continue };
    
                        variables.push(TranslatedVariable {
                            old_name: name.name.clone(),
                            new_name: self.translate_naming_convention(name.name.as_str(), Case::Snake),
                            type_name: self.translate_type_name(translated_definition, &p.ty, false),
                            ..Default::default()
                        });
                    }
    
                    scope.variables.extend(variables);
    
                    // Create the variable declaration statement
                    return Ok(sway::Statement::from(sway::Let {
                        pattern: sway::LetPattern::Tuple(
                            parameters.iter()
                                .map(|(_, p)| sway::LetIdentifier {
                                    is_mutable: false,
                                    name: if let Some(p) = p.as_ref() {
                                        if let Some(name) = p.name.as_ref() {
                                            self.translate_naming_convention(name.name.as_str(), Case::Snake)
                                        } else {
                                            "_".into()
                                        }
                                    } else {
                                        "_".into()
                                    },
                                })
                                .collect()
                        ),
    
                        type_name: Some(sway::TypeName::Tuple {
                            type_names: parameters.iter()
                                .map(|(_, p)| {
                                    if let Some(p) = p.as_ref() {
                                        self.translate_type_name(translated_definition, &p.ty, false)
                                    } else {
                                        sway::TypeName::Identifier {
                                            name: "_".into(),
                                            generic_parameters: None,
                                        }
                                    }
                                })
                                .collect(),
                        }),
                        
                        value: self.translate_expression(translated_definition, scope, rhs.as_ref())?,
                    }));
                }
            }

            // Check for standalone pre/post decrement statements
            solidity::Expression::PreDecrement(loc, x)
            | solidity::Expression::PostDecrement(loc, x) => return Ok(sway::Statement::from(
                self.translate_assignment_expression(
                    translated_definition,
                    scope,
                    "-=",
                    x,
                    &solidity::Expression::NumberLiteral(*loc, "1".into(), "".into(), None),
                )?
            )),

            // Check for standalone pre/post increment statements
            solidity::Expression::PreIncrement(loc, x)
            | solidity::Expression::PostIncrement(loc, x) => return Ok(sway::Statement::from(
                self.translate_assignment_expression(
                    translated_definition,
                    scope,
                    "+=",
                    x,
                    &solidity::Expression::NumberLiteral(*loc, "1".into(), "".into(), None),
                )?
            )),

            _ => {}
        }
        
        Ok(sway::Statement::from(
            self.translate_expression(translated_definition, scope, expression)?
        ))
    }
    
    #[inline]
    fn translate_variable_definition_statement(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        variable_declaration: &solidity::VariableDeclaration,
        initializer: &Option<solidity::Expression>,
    ) -> Result<sway::Statement, Error> {
        let old_name = variable_declaration.name.as_ref().unwrap().name.clone();
        let new_name = self.translate_naming_convention(old_name.as_str(), Case::Snake);
        let type_name = self.translate_type_name(translated_definition, &variable_declaration.ty, false);
        let mut value = None;

        if let Some(solidity::Expression::New(_, new_expression)) = initializer.as_ref() {
            let solidity::Expression::FunctionCall(_, ty, args) = new_expression.as_ref() else {
                panic!("Unexpected new expression: {} - {new_expression:#?}", new_expression.to_string());
            };

            let new_type_name = self.translate_type_name(translated_definition, ty, false);

            if type_name != new_type_name {
                panic!("Invalid new expression type name: expected `{type_name}`, found `{new_type_name}`");
            }

            match &type_name {
                sway::TypeName::Identifier { name, generic_parameters: Some(generic_parameters) } if name == "Vec" => {
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
                        panic!("Invalid new array expression: expected 1 argument, found {}", args.len());
                    }

                    let element_type_name = &generic_parameters.entries.first().unwrap().type_name;
                    let length = self.translate_expression(translated_definition, scope, &args[0])?;

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
                                    function: sway::Expression::Identifier("Vec::with_capacity".into()),
                                    generic_parameters: None,
                                    parameters: vec![
                                        length.clone(),
                                    ],
                                }),
                            }),

                            // let mut i = 0;
                            sway::Statement::from(sway::Let {
                                pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                                    is_mutable: true,
                                    name: "i".into(),
                                }),
                                type_name: None,
                                value: sway::Expression::from(sway::Literal::DecInt(0)),
                            }),

                            // while i < length {
                            //     v.push(0);
                            //     i += 1;
                            // }
                            sway::Statement::from(sway::Expression::from(sway::While {
                                // i < length
                                condition: sway::Expression::from(sway::BinaryExpression {
                                    operator: "<".into(),
                                    lhs: sway::Expression::Identifier("i".into()),
                                    rhs: length.clone(),
                                }),

                                body: sway::Block {
                                    statements: vec![
                                        // v.push(0);
                                        sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::from(sway::MemberAccess {
                                                expression: sway::Expression::Identifier("v".into()),
                                                member: "push".into(),
                                            }),
                                            generic_parameters: None,
                                            parameters: vec![
                                                sway::Expression::create_value_expression(element_type_name, None),
                                            ],
                                        })),

                                        // i += 1;
                                        sway::Statement::from(sway::Expression::from(sway::BinaryExpression {
                                            operator: "+=".into(),
                                            lhs: sway::Expression::Identifier("i".into()),
                                            rhs: sway::Expression::from(sway::Literal::DecInt(1)),
                                        })),
                                    ],
                                    final_expr: None,
                                }
                            }))
                        ],

                        // v
                        final_expr: Some(sway::Expression::Identifier("v".into())),
                    }));
                }

                _ => {}
            }
        }

        let statement = sway::Statement::from(sway::Let {
            pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                is_mutable: false,
                name: new_name.clone(),
            }),

            type_name: None,

            value: if let Some(value) = value {
                value
            } else if let Some(x) = initializer.as_ref() {
                self.translate_pre_or_post_operator_value_expression(translated_definition, scope, x)?
            } else {
                sway::Expression::create_value_expression(&type_name, None)
            },
        });

        scope.variables.push(TranslatedVariable {
            old_name,
            new_name,
            type_name,
            ..Default::default()
        });

        Ok(statement)
    }
    
    #[inline]
    fn translate_for_statement(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        initialization: &Option<Box<solidity::Statement>>,
        condition: &Option<Box<solidity::Expression>>,
        update: &Option<Box<solidity::Expression>>,
        body: &Option<Box<solidity::Statement>>,
    ) -> Result<sway::Statement, Error> {
        // {
        //     initialization;
        //     while condition {
        //         body;
        //         update;
        //     }                    
        // }

        let mut statements = vec![];

        if let Some(initialization) = initialization.as_ref() {
            let mut statement = self.translate_statement(translated_definition, scope, initialization.as_ref())?;

            // HACK: Mark identifiers as mutable since they aren't being updated for some reason
            if let sway::Statement::Let(sway::Let { pattern, .. }) = &mut statement {
                let set_let_identifier_as_mutable = |id: &mut sway::LetIdentifier| {
                    id.is_mutable = true;
                };

                match pattern {
                    sway::LetPattern::Identifier(id) => set_let_identifier_as_mutable(id),
                    sway::LetPattern::Tuple(ids) => ids.iter_mut().for_each(set_let_identifier_as_mutable),
                }
            }

            statements.push(statement);
        }

        let condition = if let Some(condition) = condition.as_ref() {
            self.translate_expression(translated_definition, scope, condition.as_ref())?
        } else {
            sway::Expression::from(sway::Literal::Bool(true))
        };

        let mut body = match body.as_ref() {
            None => sway::Block::default(),
            Some(body) => match self.translate_statement(translated_definition, scope, body.as_ref())? {
                sway::Statement::Expression(sway::Expression::Block(block)) => *block,
                statement => sway::Block {
                    statements: vec![statement],
                    final_expr: None,
                }
            }
        };

        if let Some(update) = update.as_ref() {
            body.statements.push(sway::Statement::from(
                match update.as_ref() {
                    // Check for standalone pre/post decrement statements
                    solidity::Expression::PreDecrement(loc, x)
                    | solidity::Expression::PostDecrement(loc, x) => self.translate_assignment_expression(
                        translated_definition,
                        scope,
                        "-=",
                        x,
                        &solidity::Expression::NumberLiteral(*loc, "1".into(), "".into(), None),
                    )?,
        
                    // Check for standalone pre/post increment statements
                    solidity::Expression::PreIncrement(loc, x)
                    | solidity::Expression::PostIncrement(loc, x) => self.translate_assignment_expression(
                        translated_definition,
                        scope,
                        "+=",
                        x,
                        &solidity::Expression::NumberLiteral(*loc, "1".into(), "".into(), None),
                    )?,
        
                    _ => self.translate_expression(translated_definition, scope, update.as_ref())?
                }
            ));
        }

        statements.push(
            sway::Statement::from(sway::Expression::from(sway::While {
                condition,
                body,
            }))
        );

        Ok(sway::Statement::from(sway::Expression::from(sway::Block {
            statements,
            final_expr: None,
        })))
    }
    
    #[inline]
    fn translate_return_statement(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        expression: &Option<solidity::Expression>,
    ) -> Result<sway::Statement, Error> {
        Ok(sway::Statement::from(sway::Expression::Return(
            if let Some(x) = expression.as_ref() {
                Some(Box::new(
                    self.translate_expression(translated_definition, scope, x)?
                ))
            } else {
                None
            }
        )))
    }
    
    #[inline]
    fn translate_revert_statement(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        error_type: &Option<solidity::IdentifierPath>,
        parameters: &Vec<solidity::Expression>,
    ) -> Result<sway::Statement, Error> {
        if let Some(error_type) = error_type.as_ref() {
            //
            // TODO: we need to keep the error enum in the scope and look it up there instead of assuming it will be ContractError
            //
            
            return Ok(sway::Statement::from(sway::Expression::from(sway::Block {
                statements: vec![
                    // 1. log(data)
                    sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("log".into()),
                        generic_parameters: None,
                        parameters: vec![
                            if parameters.is_empty() {
                                sway::Expression::Identifier(format!(
                                    "{}Error::{}",
                                    translated_definition.name,
                                    error_type.identifiers.first().unwrap().name,
                                ))
                            } else {
                                sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::Identifier(format!(
                                        "{}Error::{}",
                                        translated_definition.name,
                                        error_type.identifiers.first().unwrap().name,
                                    )),
                                    generic_parameters: None,
                                    parameters: vec![
                                        if parameters.len() == 1 {
                                            self.translate_expression(translated_definition, scope, &parameters[0])?
                                        } else {
                                            sway::Expression::Tuple(
                                                parameters.iter()
                                                    .map(|p| self.translate_expression(translated_definition, scope, p))
                                                    .collect::<Result<Vec<_>, _>>()?
                                            )
                                        },
                                    ]
                                })
                            },
                        ]
                    })),
                    // 2. revert(0)
                    sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("revert".into()),
                        generic_parameters: None,
                        parameters: vec![
                            sway::Expression::from(sway::Literal::DecInt(0)),
                        ],
                    }))
                ],
                final_expr: None,
            })));
        }

        if parameters.is_empty() {
            return Ok(sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::Identifier("revert".into()),
                generic_parameters: None,
                parameters: vec![
                    sway::Expression::from(sway::Literal::DecInt(0)),
                ],
            })))
        }

        if let Some(solidity::Expression::StringLiteral(reason)) = parameters.first().as_ref() {
            return Ok(sway::Statement::from(sway::Expression::from(sway::Block {
                statements: vec![
                    // 1. log(reason)
                    sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("log".into()),
                        generic_parameters: None,
                        parameters: vec![
                            sway::Expression::from(sway::Literal::String(
                                reason.iter().map(|s| s.string.clone()).collect::<Vec<_>>().join("")
                            )),
                        ]
                    })),
                    // 2. revert(0)
                    sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("revert".into()),
                        generic_parameters: None,
                        parameters: vec![
                            sway::Expression::from(sway::Literal::DecInt(0)),
                        ],
                    }))
                ],
                final_expr: None,
            })));
        }

        todo!("translate revert statement")
    }
    
    #[inline]
    fn translate_emit_statement(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        expression: &solidity::Expression,
    ) -> Result<sway::Statement, Error> {
        //
        // TODO: we need to keep the event enum in the scope and look it up there instead of assuming it will be ContractEvent
        //
        
        match expression {
            solidity::Expression::FunctionCall(_, x, parameters) => match x.as_ref() {
                solidity::Expression::Variable(solidity::Identifier { name, .. }) => {
                    return Ok(sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("log".into()),
                        generic_parameters: None,
                        parameters: vec![
                            if parameters.is_empty() {
                                sway::Expression::Identifier(format!(
                                    "{}Event::{}",
                                    translated_definition.name,
                                    name,
                                ))
                            } else {
                                sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::Identifier(format!(
                                        "{}Event::{}",
                                        translated_definition.name,
                                        name,
                                    )),
                                    generic_parameters: None,
                                    parameters: vec![
                                        if parameters.len() == 1 {
                                            self.translate_expression(translated_definition, scope, &parameters[0])?
                                        } else {
                                            sway::Expression::Tuple(
                                                parameters.iter()
                                                    .map(|p| self.translate_expression(translated_definition, scope, p))
                                                    .collect::<Result<Vec<_>, _>>()?
                                            )
                                        },
                                    ]
                                })
                            },
                        ]
                    })))
                }
                
                _ => {}
            }

            _ => {}
        }

        todo!("translate emit statement")
    }

    // --------------------------------------------------------------------------------
    // EXPRESSION TRANSLATION CODE
    // --------------------------------------------------------------------------------

    fn translate_expression(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        expression: &solidity::Expression,
    ) -> Result<sway::Expression, Error> {
        match expression {
            solidity::Expression::BoolLiteral(_, _)
            | solidity::Expression::NumberLiteral(_, _, _, _)
            | solidity::Expression::RationalNumberLiteral(_, _, _, _, _)
            | solidity::Expression::HexNumberLiteral(_, _, _)
            | solidity::Expression::AddressLiteral(_, _)
            | solidity::Expression::HexLiteral(_)
            | solidity::Expression::StringLiteral(_) => self.translate_literal_expression(expression),
            
            solidity::Expression::Type(_, _) => self.translate_type_expression(translated_definition, scope, expression),
            solidity::Expression::Variable(_) => self.translate_variable_expression(translated_definition, scope, expression),
            
            solidity::Expression::ArrayLiteral(_, expressions) => self.translate_array_literal_expression(translated_definition, scope, expressions.as_slice()),
            solidity::Expression::ArraySubscript(_, _, _) => self.translate_array_subscript_expression(translated_definition, scope, expression),
            solidity::Expression::ArraySlice(_, _, _, _) => self.translate_array_slice_expression(translated_definition, scope, expression),
            solidity::Expression::List(_, parameters) => self.translate_list_expression(translated_definition, scope, parameters.as_slice()),
            solidity::Expression::Parenthesis(_, expression) => self.translate_parenthesis_expression(translated_definition, scope, expression),
            
            solidity::Expression::MemberAccess(_, container, member) => self.translate_member_access_expression(translated_definition, scope, expression, container, member),
            
            solidity::Expression::FunctionCall(_, function, arguments) => self.translate_function_call_expression(translated_definition, scope, expression, function, arguments),
            solidity::Expression::FunctionCallBlock(_, function, block) => self.translate_function_call_block_expression(translated_definition, scope, function, block),
            solidity::Expression::NamedFunctionCall(_, function, named_arguments) => self.translate_named_function_call_expression(translated_definition, scope, function, named_arguments.as_slice()),
            
            solidity::Expression::Not(_, x) => self.translate_unary_expression(translated_definition, scope, "!", x),
            solidity::Expression::BitwiseNot(_, x) => self.translate_unary_expression(translated_definition, scope, "!", x),
            solidity::Expression::UnaryPlus(_, x) => self.translate_expression(translated_definition, scope, x),
            solidity::Expression::Negate(_, x) => self.translate_unary_expression(translated_definition, scope, "-", x),
            
            solidity::Expression::Power(_, lhs, rhs) => self.translate_power_expression(translated_definition, scope, lhs, rhs),
            solidity::Expression::Multiply(_, lhs, rhs) => self.translate_binary_expression(translated_definition, scope, "*", lhs, rhs),
            solidity::Expression::Divide(_, lhs, rhs) => self.translate_binary_expression(translated_definition, scope, "/", lhs, rhs),
            solidity::Expression::Modulo(_, lhs, rhs) => self.translate_binary_expression(translated_definition, scope, "%", lhs, rhs),
            solidity::Expression::Add(_, lhs, rhs) => self.translate_binary_expression(translated_definition, scope, "+", lhs, rhs),
            solidity::Expression::Subtract(_, lhs, rhs) => self.translate_binary_expression(translated_definition, scope, "-", lhs, rhs),
            solidity::Expression::ShiftLeft(_, lhs, rhs) => self.translate_binary_expression(translated_definition, scope, "<<", lhs, rhs),
            solidity::Expression::ShiftRight(_, lhs, rhs) => self.translate_binary_expression(translated_definition, scope, ">>", lhs, rhs),
            solidity::Expression::BitwiseAnd(_, lhs, rhs) => self.translate_binary_expression(translated_definition, scope, "&", lhs, rhs),
            solidity::Expression::BitwiseXor(_, lhs, rhs) => self.translate_binary_expression(translated_definition, scope, "^", lhs, rhs),
            solidity::Expression::BitwiseOr(_, lhs, rhs) => self.translate_binary_expression(translated_definition, scope, "|", lhs, rhs),
            solidity::Expression::Less(_, lhs, rhs) => self.translate_binary_expression(translated_definition, scope, "<", lhs, rhs),
            solidity::Expression::More(_, lhs, rhs) => self.translate_binary_expression(translated_definition, scope, ">", lhs, rhs),
            solidity::Expression::LessEqual(_, lhs, rhs) => self.translate_binary_expression(translated_definition, scope, "<=", lhs, rhs),
            solidity::Expression::MoreEqual(_, lhs, rhs) => self.translate_binary_expression(translated_definition, scope, ">=", lhs, rhs),
            solidity::Expression::Equal(_, lhs, rhs) => self.translate_binary_expression(translated_definition, scope, "==", lhs, rhs),
            solidity::Expression::NotEqual(_, lhs, rhs) => self.translate_binary_expression(translated_definition, scope, "!=", lhs, rhs),
            solidity::Expression::And(_, lhs, rhs) => self.translate_binary_expression(translated_definition, scope, "&&", lhs, rhs),
            solidity::Expression::Or(_, lhs, rhs) => self.translate_binary_expression(translated_definition, scope, "||", lhs, rhs),
            
            solidity::Expression::ConditionalOperator(_, condition, then_value, else_value) => self.translate_conditional_operator_expression(translated_definition, scope, condition, then_value, else_value),
            
            solidity::Expression::Assign(_, lhs, rhs) => self.translate_assignment_expression(translated_definition, scope, "=", lhs.as_ref(), rhs.as_ref()),
            solidity::Expression::AssignOr(_, lhs, rhs) => self.translate_assignment_expression(translated_definition, scope, "|=", lhs.as_ref(), rhs.as_ref()),
            solidity::Expression::AssignAnd(_, lhs, rhs) => self.translate_assignment_expression(translated_definition, scope, "&=", lhs.as_ref(), rhs.as_ref()),
            solidity::Expression::AssignXor(_, lhs, rhs) => self.translate_assignment_expression(translated_definition, scope, "^=", lhs.as_ref(), rhs.as_ref()),
            solidity::Expression::AssignShiftLeft(_, lhs, rhs) => self.translate_assignment_expression(translated_definition, scope, "<<=", lhs.as_ref(), rhs.as_ref()),
            solidity::Expression::AssignShiftRight(_, lhs, rhs) => self.translate_assignment_expression(translated_definition, scope, ">>=", lhs.as_ref(), rhs.as_ref()),
            solidity::Expression::AssignAdd(_, lhs, rhs) => self.translate_assignment_expression(translated_definition, scope, "+=", lhs.as_ref(), rhs.as_ref()),
            solidity::Expression::AssignSubtract(_, lhs, rhs) => self.translate_assignment_expression(translated_definition, scope, "-=", lhs.as_ref(), rhs.as_ref()),
            solidity::Expression::AssignMultiply(_, lhs, rhs) => self.translate_assignment_expression(translated_definition, scope, "*=", lhs.as_ref(), rhs.as_ref()),
            solidity::Expression::AssignDivide(_, lhs, rhs) => self.translate_assignment_expression(translated_definition, scope, "/=", lhs.as_ref(), rhs.as_ref()),
            solidity::Expression::AssignModulo(_, lhs, rhs) => self.translate_assignment_expression(translated_definition, scope, "%=", lhs.as_ref(), rhs.as_ref()),
            
            solidity::Expression::PreIncrement(_, _)
            | solidity::Expression::PostIncrement(_, _)
            | solidity::Expression::PreDecrement(_, _)
            | solidity::Expression::PostDecrement(_, _) => self.translate_pre_or_post_operator_value_expression(translated_definition, scope, expression),

            solidity::Expression::New(_, expression) => self.translate_new_expression(translated_definition, scope, expression),
            solidity::Expression::Delete(_, expression) => self.translate_delete_expression(translated_definition, scope, expression),
        }
    }

    #[inline]
    fn translate_literal_expression(
        &mut self,
        expression: &solidity::Expression,
    ) -> Result<sway::Expression, Error> {
        match expression {
            solidity::Expression::BoolLiteral(_, value) => {
                Ok(sway::Expression::from(sway::Literal::Bool(*value)))
            }
            
            solidity::Expression::NumberLiteral(_, value, _, _) => {
                Ok(sway::Expression::from(sway::Literal::DecInt(value.parse().unwrap())))
            }

            solidity::Expression::RationalNumberLiteral(_, _, _, _, _) => todo!("translate rational number literal expression: {} - {expression:#?}", expression.to_string()),

            solidity::Expression::HexNumberLiteral(_, value, _) | solidity::Expression::AddressLiteral(_, value) => {
                Ok(sway::Expression::from(sway::Literal::HexInt(
                    u64::from_str_radix(value.trim_start_matches("0x"), 16)
                        .map_err(|e| Error::Wrapped(Box::new(e)))?
                )))
            }

            solidity::Expression::HexLiteral(_) => todo!("translate hex literal expression: {} - {expression:#?}", expression.to_string()),
            
            solidity::Expression::StringLiteral(value) => {
                Ok(sway::Expression::from(sway::Literal::String(
                    value.iter().map(|s| s.string.clone()).collect::<Vec<_>>().join("")
                )))
            }

            _ => panic!("Expected literal expression"),
        }
    }

    #[inline]
    fn translate_type_expression(
        &mut self,
        _translated_definition: &mut TranslatedDefinition,
        _scope: &mut TranslationScope,
        expression: &solidity::Expression,
    ) -> Result<sway::Expression, Error> {
        //
        // NOTE:
        // Type expressions should never be encountered on their own.
        // They should be handled in a higher level expression.
        //

        unimplemented!("type expression: {} - {expression:#?}", expression.to_string())
    }

    #[inline]
    fn translate_variable_expression(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        expression: &solidity::Expression,
    ) -> Result<sway::Expression, Error> {
        //
        // NOTE:
        // Variable expressions should only ever be encountered for reading the value.
        // Writes are handled when translating assignment expressions.
        //

        let (variable, expression) = self.translate_variable_access_expression(translated_definition, scope, expression)?;
        variable.read_count += 1;
        
        if variable.is_storage {
            Ok(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::from(sway::MemberAccess {
                    expression,
                    member: "read".into(),
                }),
                generic_parameters: None,
                parameters: vec![],
            }))
        } else {
            Ok(expression)
        }
    }

    #[inline]
    fn translate_array_literal_expression(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        expressions: &[solidity::Expression],
    ) -> Result<sway::Expression, Error> {
        Ok(sway::Expression::Array(sway::Array {
            elements: expressions.iter()
                .map(|x| self.translate_expression(translated_definition, scope, x))
                .collect::<Result<Vec<_>, _>>()?,
        }))
    }

    #[inline]
    fn translate_array_subscript_expression(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        expression: &solidity::Expression,
    ) -> Result<sway::Expression, Error> {
        //
        // NOTE:
        // Array subscript expressions should only ever be encountered for reading the value.
        // Writes are handled when translating assignment expressions.
        //

        let (variable, expression) = self.translate_variable_access_expression(translated_definition, scope, expression)?;
        variable.read_count += 1;

        if variable.is_storage {
            Ok(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::from(sway::MemberAccess {
                    expression,
                    member: "read".into(),
                }),
                generic_parameters: None,
                parameters: vec![],
            }))
        } else {
            match &variable.type_name {
                sway::TypeName::Identifier { name, .. } if name == "Vec" => {
                    let sway::Expression::ArrayAccess(array_access) = expression else {
                        panic!("Expected array access expression, found {expression:#?}");
                    };

                    Ok(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::from(sway::MemberAccess {
                            expression: sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::from(sway::MemberAccess {
                                    expression: array_access.expression.clone(),
                                    member: "get".into(),
                                }),
                                generic_parameters: None,
                                parameters: vec![
                                    array_access.index.clone(),
                                ],
                            }),
                            member: "unwrap".into(),
                        }),
                        generic_parameters: None,
                        parameters: vec![],
                    }))
                }

                _ => Ok(expression),
            }
        }
    }

    #[inline]
    fn translate_array_slice_expression(
        &mut self,
        _translated_definition: &mut TranslatedDefinition,
        _scope: &mut TranslationScope,
        expression: &solidity::Expression,
    ) -> Result<sway::Expression, Error> {
        todo!("translate array slice expression: {} - {expression:#?}", expression.to_string())
    }

    #[inline]
    fn translate_list_expression(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        parameters: &[(solidity::Loc, Option<solidity::Parameter>)],
    ) -> Result<sway::Expression, Error> {
        //
        // NOTE:
        // Assignments are handled at the statement level, since it's an assignment to a list of variable declarations.
        //

        // Ensure all elements of the list have no name (value-only tuple)
        if !parameters.iter().all(|(_, p)| p.as_ref().unwrap().name.is_none()) {
            unimplemented!("non-value list expression")
        }

        // Create a tuple expression
        Ok(sway::Expression::Tuple(
            parameters.iter()
                .map(|(_, p)| self.translate_expression(translated_definition, scope, &p.as_ref().unwrap().ty))
                .collect::<Result<Vec<_>, _>>()?
        ))
    }

    #[inline]
    fn translate_parenthesis_expression(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        expression: &solidity::Expression,
    ) -> Result<sway::Expression, Error> {
        // (x)
        Ok(sway::Expression::Tuple(vec![
            self.translate_expression(translated_definition, scope, expression)?,
        ]))
    }

    #[inline]
    fn translate_member_access_expression(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        expression: &solidity::Expression,
        container: &solidity::Expression,
        member: &solidity::Identifier,
    ) -> Result<sway::Expression, Error> {
        match container {
            solidity::Expression::FunctionCall(_, x, args) => match x.as_ref() {
                solidity::Expression::Type(_, ty) => {
                    if args.len() != 1 {
                        panic!("Invalid type cast expression, expected 1 parameter, found {}: {}", args.len(), expression.to_string());
                    }

                    match ty {
                        solidity::Type::Address => {
                            //
                            // TODO: handle address casting that isn't `this`
                            //

                            let solidity::Expression::Variable(solidity::Identifier { name, .. }) = &args[0] else {
                                todo!("translate address cast member `{member}`: {}", expression.to_string());
                            };

                            if name != "this" {
                                todo!("translate address cast member `{member}`: {}", expression.to_string());
                            }

                            match member.name.as_str() {
                                // address(this).balance => std::context::this_balance(AssetId::default())
                                "balance" => return Ok(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::Identifier("std::context::this_balance".into()),
                                    generic_parameters: None,
                                    parameters: vec![
                                        sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::Identifier("AssetId::default".into()),
                                            generic_parameters: None,
                                            parameters: vec![],
                                        }),
                                    ],
                                })),

                                member => todo!("translate address cast member `{member}`: {}", expression.to_string()),
                            }
                        }

                        solidity::Type::AddressPayable => todo!("translate address payable cast member access: {}", expression.to_string()),
                        solidity::Type::Payable => todo!("translate payable cast member access: {}", expression.to_string()),
                        solidity::Type::Bool => todo!("translate bool cast member access: {}", expression.to_string()),
                        solidity::Type::String => todo!("translate string cast member access: {}", expression.to_string()),
                        solidity::Type::Int(_) => todo!("translate int cast member access: {}", expression.to_string()),
                        solidity::Type::Uint(_) => todo!("translate uint cast member access: {}", expression.to_string()),
                        solidity::Type::Bytes(_) => todo!("translate bytes cast member access: {}", expression.to_string()),
                        solidity::Type::Rational => todo!("translate rational cast member access: {}", expression.to_string()),
                        solidity::Type::DynamicBytes => todo!("translate dynamic bytes cast member access: {}", expression.to_string()),
                        solidity::Type::Mapping { .. } => todo!("translate mapping cast member access: {}", expression.to_string()),
                        solidity::Type::Function { .. } => todo!("translate function cast member access: {}", expression.to_string()),
                    }
                }

                solidity::Expression::Variable(solidity::Identifier { name, .. }) => match name.as_str() {
                    "type" => {
                        if args.len() != 1 {
                            panic!("Invalid type name expression, expected 1 parameter, found {}: {}", args.len(), expression.to_string());
                        }

                        let type_name = self.translate_type_name(translated_definition, &args[0], false);

                        match type_name {
                            sway::TypeName::Undefined => panic!("Undefined type name"),

                            sway::TypeName::Identifier { name, .. } => match (name.as_str(), member.name.as_str()) {
                                ("u8" | "u16" | "u32" | "u64" | "u256", "min") => return Ok(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::Identifier(format!("{name}::min")),
                                    generic_parameters: None,
                                    parameters: vec![],
                                })),

                                ("u8" | "u16" | "u32" | "u64" | "u256", "max") => return Ok(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::Identifier(format!("{name}::max")),
                                    generic_parameters: None,
                                    parameters: vec![],
                                })),

                                _ => todo!("translate type member access: {} - {expression:#?}", expression.to_string()),
                            }

                            sway::TypeName::Array { .. } => todo!("translate type member access: {} - {expression:#?}", expression.to_string()),
                            sway::TypeName::Tuple { .. } => todo!("translate type member access: {} - {expression:#?}", expression.to_string()),
                            sway::TypeName::String { .. } => todo!("translate type member access: {} - {expression:#?}", expression.to_string()),
                        }
                    }

                    _ => {}
                }

                _ => {}
            }

            solidity::Expression::Variable(solidity::Identifier { name, .. }) => match (name.as_str(), member.name.as_str()) {
                // TODO: find out the appropriate sway version of `block.basefee`
                ("block", "basefee") => {
                    // todo!("block.basefee")
                    return Ok(sway::Expression::create_todo(Some("block.basefee".into())))
                }

                // TODO: find out the appropriate sway version of `block.chainid`
                ("block", "chainid") => {
                    // todo!("block.chainid")
                    return Ok(sway::Expression::create_todo(Some("block.chainid".into())))
                }

                // TODO: find out the appropriate sway version of `block.coinbase`
                ("block", "coinbase") => {
                    // todo!("block.coinbase")
                    return Ok(sway::Expression::create_todo(Some("block.coinbase".into())))
                }

                // TODO: find out the appropriate sway version of `block.difficulty`
                ("block", "difficulty") => {
                    // todo!("block.difficulty")
                    return Ok(sway::Expression::create_todo(Some("block.difficulty".into())))
                }

                // TODO: find out the appropriate sway version of `block.gaslimit`
                ("block", "gaslimit") => {
                    // todo!("block.gaslimit")
                    return Ok(sway::Expression::create_todo(Some("block.gaslimit".into())))
                }

                ("block", "number") => {
                    // std::block::height()
                    return Ok(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("std::block::height".into()),
                        generic_parameters: None,
                        parameters: vec![],
                    }))
                }

                // TODO: find out the appropriate sway version of `block.prevrandao`
                ("block", "prevrandao") => {
                    // todo!("block.prevrandao")
                    return Ok(sway::Expression::create_todo(Some("block.prevrandao".into())))
                }

                ("block", "timestamp") => {
                    // std::block::timestamp()
                    return Ok(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("std::block::timestamp".into()),
                        generic_parameters: None,
                        parameters: vec![],
                    }))
                }

                ("msg", "data") => {
                    // std::inputs::input_message_data(0, 0)
                    return Ok(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("std::inputs::input_message_data".into()),
                        generic_parameters: None,
                        parameters: vec![
                            sway::Expression::from(sway::Literal::DecInt(0)),
                            sway::Expression::from(sway::Literal::DecInt(0)),
                        ],
                    }))
                }

                ("msg", "sender") => {
                    // msg_sender().unwrap()
                    return Ok(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::from(sway::MemberAccess {
                            expression: sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier("msg_sender".into()),
                                generic_parameters: None,
                                parameters: vec![],
                            }),
                            member: "unwrap".into(),
                        }),
                        generic_parameters: None,
                        parameters: vec![],
                    }))
                }

                // TODO: find out the appropriate sway version of `msg.sig`
                ("msg", "sig") => {
                    // todo!("msg.sig")
                    return Ok(sway::Expression::create_todo(Some("msg.sig".into())))
                }

                ("msg", "value") => {
                    // std::context::msg_amount()
                    return Ok(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("std::context::msg_amount".into()),
                        generic_parameters: None,
                        parameters: vec![],
                    }))
                }

                ("tx", "gasprice") => {
                    // std::tx::tx_gas_price().unwrap_or(0)
                    return Ok(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::from(sway::MemberAccess {
                            expression: sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier("std::tx::tx_gas_price".to_string()),
                                generic_parameters: None,
                                parameters: vec![],
                            }),
                            member: "unwrap_or".into(),
                        }),
                        generic_parameters: None,
                        parameters: vec![
                            sway::Expression::from(sway::Literal::DecInt(0)),
                        ],
                    }))
                }

                // TODO: find out the appropriate sway version of `tx.origin`
                ("tx", "origin") => {
                    // todo!("tx.origin")
                    return Ok(sway::Expression::create_todo(Some("tx.origin".into())))
                }

                (name, member) => {
                    // Check to see if the variable is an enum
                    if let Some(translated_enum) = translated_definition.enums.iter().find(|e| match &e.type_definition.name {
                        sway::TypeName::Identifier { name: enum_name, .. } => enum_name == name,
                        _ => false
                    }) {
                        let new_name = self.translate_naming_convention(member, Case::ScreamingSnake);

                        // Check to see if member is part of translated enum
                        if let Some(sway::ImplItem::Constant(c)) = translated_enum.variants_impl.items.iter().find(|i| match i {
                            sway::ImplItem::Constant(c) => c.name == new_name,
                            _ => false,
                        }) {
                            return Ok(sway::Expression::Identifier(format!("{}::{}", name, c.name)));
                        }
                    }

                    let variable = match scope.get_variable_from_old_name(name) {
                        Ok(variable) => variable,
                        Err(e) => panic!("{e}"),
                    };

                    match &variable.type_name {
                        sway::TypeName::Undefined => panic!("Undefined type name"),

                        sway::TypeName::Identifier { name, .. } => match (name.as_str(), member) {
                            ("Vec" | "std::bytes::Bytes", "length") => return Ok(sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::from(sway::MemberAccess {
                                    expression: sway::Expression::Identifier(variable.new_name.clone()),
                                    member: "len".into(),
                                }),
                                generic_parameters: None,
                                parameters: vec![],
                            })),

                            _ => todo!("translate {} variable member: {} - {expression:#?}", variable.type_name, expression.to_string())
                        }
                        
                        sway::TypeName::Array { .. } => todo!("translate {} variable member: {} - {expression:#?}", variable.type_name, expression.to_string()),
                        sway::TypeName::Tuple { .. } => todo!("translate {} variable member: {} - {expression:#?}", variable.type_name, expression.to_string()),
                        sway::TypeName::String { .. } => todo!("translate {} variable member: {} - {expression:#?}", variable.type_name, expression.to_string()),
                    }
                }
            }

            _ => {}
        }

        todo!("translate member access expression: {} - {expression:#?}", expression.to_string())
    }

    #[inline]
    fn translate_function_call_expression(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        expression: &solidity::Expression,
        function: &solidity::Expression,
        arguments: &[solidity::Expression],
    ) -> Result<sway::Expression, Error> {
        match function {
            solidity::Expression::Type(_, ty) => {
                // Type casting

                if arguments.len() != 1 {
                    panic!("Invalid type cast: {expression:#?}");
                }

                match ty {
                    solidity::Type::Address => match &arguments[0] {
                        solidity::Expression::NumberLiteral(_, value, _, _) if value == "0" => {
                            // Ensure std::constants::ZERO_B256 is imported
                            translated_definition.ensure_use_declared("std::constants::ZERO_B256");

                            // Create a zero address expression
                            // Identity::Address(Address::from(ZERO_B256))
                            Ok(sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier("Identity::Address".into()),
                                generic_parameters: None,
                                parameters: vec![
                                    sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::Identifier("Address::from".into()),
                                        generic_parameters: None,
                                        parameters: vec![
                                            sway::Expression::Identifier("ZERO_B256".into()), // TODO: ensure `ZERO_B256` is imported
                                        ],
                                    }),
                                ],
                            }))
                        }

                        solidity::Expression::Variable(solidity::Identifier { name, .. }) if name == "this" => {
                            // address(this) => Identity::from(ContractId::this())
                            Ok(sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier("Identity::ContractId".into()),
                                generic_parameters: None,
                                parameters: vec![
                                    sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::Identifier("ContractId::this".into()),
                                        generic_parameters: None,
                                        parameters: vec![],
                                    }),
                                ],
                            }))
                        }

                        _ => todo!("translate address cast: {expression:#?}"),
                    }

                    solidity::Type::Uint(bits) => {
                        let value = self.translate_expression(translated_definition, scope, &arguments[0])?;
                        let type_name = translated_definition.get_underlying_type(&scope.get_expression_type(&value)?);

                        let create_uint_try_from_unwrap_expression = |from_bits: usize, to_bits: usize, value: sway::Expression| -> Result<sway::Expression, Error> {
                            if from_bits == to_bits {
                                return Ok(value);
                            }

                            Ok(sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::from(sway::MemberAccess {
                                    expression: sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::Identifier(format!("u{to_bits}::try_from")),
                                        generic_parameters: None,
                                        parameters: vec![value],
                                    }),
                                    member: "unwrap".into(),
                                }),
                                generic_parameters: None,
                                parameters: vec![],
                            }))
                        };

                        match type_name {
                            sway::TypeName::Identifier { name, .. } => match (name.as_str(), bits) {
                                ("u8", 8 | 16 | 32 | 64 | 256) => create_uint_try_from_unwrap_expression(8, *bits as usize, value),
                                ("u16", 8 | 16 | 32 | 64 | 256) => create_uint_try_from_unwrap_expression(16, *bits as usize, value),
                                ("u32", 8 | 16 | 32 | 64 | 256) => create_uint_try_from_unwrap_expression(32, *bits as usize, value),
                                ("u64", 8 | 16 | 32 | 64 | 256) => create_uint_try_from_unwrap_expression(64, *bits as usize, value),
                                ("u256", 8 | 16 | 32 | 64 | 256) => create_uint_try_from_unwrap_expression(256, *bits as usize, value),
                                _ => todo!("translate {name} type cast: {} - {expression:#?}", expression.to_string())
                            }

                            _ => todo!("translate type cast: {} - {expression:#?}", expression.to_string()),
                        }
                    }

                    _ => todo!("translate type cast: {} - {expression:#?}", expression.to_string()),
                }
            }

            solidity::Expression::Variable(solidity::Identifier { name, .. }) => {
                let parameters = arguments.iter()
                    .map(|a| self.translate_expression(translated_definition, scope, a))
                    .collect::<Result<Vec<_>, _>>()?;

                match name.as_str() {
                    "blockhash" => {
                        // blockhash(block_number) => std::block::block_header_hash(block_height).unwrap_or(0)

                        if parameters.len() != 1 {
                            panic!("Invalid blockhash call: {expression:#?}");
                        }

                        Ok(sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::from(sway::MemberAccess {
                                expression: sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::Identifier("std::block::block_header_hash".into()),
                                    generic_parameters: None,
                                    parameters,
                                }),
                                member: "unwrap_or".into(),
                            }),
                            generic_parameters: None,
                            parameters: vec![
                                sway::Expression::from(sway::Literal::DecInt(0)),
                            ],
                        }))
                    }

                    "gasleft" => {
                        // gasleft() => std::registers::global_gas()

                        if !parameters.is_empty() {
                            panic!("Invalid gasleft call: {expression:#?}");
                        }

                        Ok(sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier("std::registers::global_gas".into()),
                            generic_parameters: None,
                            parameters,
                        }))
                    }

                    "addmod" => {
                        // addmod(x, y, k) => (x + y) % k

                        if parameters.len() != 3 {
                            panic!("Invalid addmod call: {expression:#?}");
                        }

                        Ok(sway::Expression::from(sway::BinaryExpression {
                            operator: "%".into(),
                            lhs: sway::Expression::Tuple(vec![
                                sway::Expression::from(sway::BinaryExpression {
                                    operator: "+".into(),
                                    lhs: parameters[0].clone(),
                                    rhs: parameters[1].clone(),
                                }),
                            ]),
                            rhs: parameters[2].clone(),
                        }))
                    }

                    "mulmod" => {
                        // mulmod(x, y, k) => (x * y) % k

                        if parameters.len() != 3 {
                            panic!("Invalid mulmod call: {expression:#?}");
                        }

                        Ok(sway::Expression::from(sway::BinaryExpression {
                            operator: "%".into(),
                            lhs: sway::Expression::Tuple(vec![
                                sway::Expression::from(sway::BinaryExpression {
                                    operator: "*".into(),
                                    lhs: parameters[0].clone(),
                                    rhs: parameters[1].clone(),
                                }),
                            ]),
                            rhs: parameters[2].clone(),
                        }))
                    }

                    "keccak256" => {
                        // keccak256(value) => std::hash::keccak256(value)

                        if parameters.len() != 1 {
                            panic!("Invalid keccak256 call: {expression:#?}");
                        }

                        Ok(sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier("std::hash::keccak256".into()),
                            generic_parameters: None,
                            parameters,
                        }))
                    }

                    "sha256" => {
                        // sha256(value) => std::hash::sha256(value)

                        if parameters.len() != 1 {
                            panic!("Invalid sha256 call: {expression:#?}");
                        }

                        Ok(sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier("std::hash::sha256".into()),
                            generic_parameters: None,
                            parameters,
                        }))
                    }

                    "ripemd160" => {
                        Ok(sway::Expression::create_unimplemented(Some("ripemd160 is not supported in sway".into())))
                    }

                    "ecrecover" => {
                        // ecrecover(hash, v, r, s) => std::ecr::ec_recover(sig, msg_hash)

                        //
                        // TODO: how should we generate the sig value from v,r,s?
                        //

                        if parameters.len() != 4 {
                            panic!("Invalid ecrecover call: {expression:#?}");
                        }

                        Ok(sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier("std::ecr::ec_recover".into()),
                            generic_parameters: None,
                            parameters: vec![
                                sway::Expression::create_todo(Some("ecrecover: how should we generate the sig value from v,r,s?".into())),
                                parameters[0].clone(),
                            ],
                        }))
                    }

                    "selfdestruct" => {
                        //
                        // TODO: how should we handle this?
                        //

                        Ok(sway::Expression::create_unimplemented(Some("selfdestruct is not supported in sway".into())))
                    }

                    "assert" => {
                        // assert(x) => assert(x)

                        if parameters.len() != 1 {
                            panic!("Invalid assert call: {expression:#?}");
                        }

                        Ok(sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier("assert".into()),
                            generic_parameters: None,
                            parameters,
                        }))
                    }

                    "require" => {
                        // require(x, "msg") => require(x, "msg")

                        if parameters.len() != 2 {
                            panic!("Invalid require call: {expression:#?}");
                        }

                        Ok(sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier("require".into()),
                            generic_parameters: None,
                            parameters,
                        }))
                    }

                    old_name => {
                        // Ensure the function exists in scope
                        let Some(function) = scope.find_function(|f| {
                            // Ensure the function's old name matches the function call we're translating
                            if f.old_name != old_name {
                                return false;
                            }
                            
                            // Ensure the supplied function call args match the function's parameters
                            if parameters.len() != f.parameters.entries.len() {
                                return false;
                            }

                            for (i, parameter) in parameters.iter().enumerate() {
                                if let Some(type_name) = f.parameters.entries[i].type_name.as_ref() {
                                    if scope.get_expression_type(parameter).unwrap() != *type_name {
                                        return false;
                                    }
                                }
                            }

                            true
                        }) else {
                            panic!("Failed to find function `{old_name}` in scope");
                        };

                        // Increase the call count of the function
                        *translated_definition.function_call_counts.entry(function.new_name.clone()).or_insert(0) += 1;

                        // Translate the function call
                        Ok(sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier(function.new_name.clone()),
                            generic_parameters: None,
                            parameters,
                        }))
                    }
                }
            }

            solidity::Expression::MemberAccess(_, expression, member) => {
                match expression.as_ref() {
                    solidity::Expression::Type(_, ty) => match ty {
                        solidity::Type::Address => todo!("handle address member access function `{member:#?}`"),
                        solidity::Type::AddressPayable => todo!("handle address payable member access function `{member:#?}`"),
                        solidity::Type::Payable => todo!("handle payable member access function `{member:#?}`"),
                        solidity::Type::Bool => todo!("handle bool member access function `{member:#?}`"),

                        solidity::Type::String => match member.name.as_str() {
                            "concat" => {
                                // string.concat(x) => ???

                                //
                                // TODO: how should this be handled?
                                //

                                return Ok(sway::Expression::create_todo(Some(format!("string.concat({arguments:?})"))));
                            }
                            
                            member => todo!("handle `string.{member} translation")
                        }

                        solidity::Type::Int(_) => todo!("handle int member access function `{member:#?}`"),
                        solidity::Type::Uint(_) => todo!("handle uint member access function `{member:#?}`"),
                        solidity::Type::Bytes(_) => todo!("handle bytes member access function `{member:#?}`"),
                        solidity::Type::Rational => todo!("handle rational member access function `{member:#?}`"),
                        
                        solidity::Type::DynamicBytes => match member.name.as_str() {
                            "concat" => {
                                // bytes.concat(x) => ???

                                //
                                // TODO: how should this be handled?
                                //

                                return Ok(sway::Expression::create_todo(Some(format!("bytes.concat({arguments:?})"))));
                            }
                            
                            member => todo!("handle `bytes.{member} translation")
                        }

                        solidity::Type::Mapping { .. } => todo!("handle mapping member access function `{member:#?}`"),
                        solidity::Type::Function { .. } => todo!("handle function member access function `{member:#?}`"),
                    }

                    solidity::Expression::Variable(solidity::Identifier { name, .. }) => match name.as_str() {
                        "abi" => match member.name.as_str() {
                            "decode" => {
                                // abi.decode(x) => ???

                                //
                                // TODO: how should this be handled?
                                //

                                return Ok(sway::Expression::create_todo(Some(format!("abi.decode({arguments:?})"))));
                            }

                            "encode" => {
                                // abi.encode(x) => ???

                                //
                                // TODO: how should this be handled?
                                //

                                return Ok(sway::Expression::create_todo(Some(format!("abi.encode({arguments:?})"))))
                            }

                            "encodePacked" => {
                                // abi.encodePacked(x) => ???

                                //
                                // TODO: how should this be handled?
                                //

                                return Ok(sway::Expression::create_todo(Some(format!("abi.encodePacked({arguments:?})"))))
                            }

                            "encodeWithSignature" => {
                                // abi.encodeWithSignature(x) => ???

                                //
                                // TODO: how should this be handled?
                                //

                                return Ok(sway::Expression::create_todo(Some(format!("abi.encodeWithSignature({arguments:?})"))))
                            }
                            
                            member => todo!("handle `abi.{member}` translation"),
                        }

                        name => {
                            let old_name = format!("{}_{}", expression.to_string(), member.to_string());
                            let new_name = self.translate_naming_convention(old_name.as_str(), Case::Snake);
    
                            // Check if function is contained in an external definition
                            if let Some(external_definition) = self.translated_definitions.iter().find(|x| x.name == name) {
                                // Check if the member is a function defined in the toplevel scope
                                // TODO: we should really check if the functions argument types are correct
                                if let Some(external_function_declaration) = external_definition.toplevel_scope.find_function(|f| f.old_name == member.name && f.parameters.entries.len() == arguments.len()) {
                                    // Import the function if we haven't already
                                    if translated_definition.toplevel_scope.get_function_from_old_name(old_name.as_str()).is_err() {
                                        // Get the external function definition
                                        let Some(external_function_definition) = external_definition.functions.iter().find(|f| {
                                            f.name == external_function_declaration.new_name && f.parameters.entries.len() == external_function_declaration.parameters.entries.len()
                                        }) else {
                                            panic!("Failed to find external function definition");
                                        };

                                        // Create the local function definition
                                        let mut local_function_definition = external_function_definition.clone();
                                        local_function_definition.name = new_name.clone();
                                        
                                        // Add the local function definition to the beginning of the list
                                        translated_definition.functions.insert(0, local_function_definition);

                                        // Create the local function declaration for the toplevel scope
                                        let mut local_function_declaration = external_function_declaration.clone();
                                        local_function_declaration.old_name = old_name.clone();
                                        local_function_declaration.new_name = new_name.clone();

                                        // Add the local function to the beginning of the toplevel scope
                                        translated_definition.toplevel_scope.functions.insert(0, local_function_declaration.clone());

                                        // Create the function call
                                        let function_call = sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::Identifier(new_name),
                                            generic_parameters: None,
                                            parameters: arguments.iter().map(|a| self.translate_expression(translated_definition, scope, a)).collect::<Result<Vec<_>, _>>()?,
                                        });

                                        return Ok(function_call);
                                    }
                                }
                            }
                        }
                    }

                    _ => {}
                }

                let expression = self.translate_expression(translated_definition, scope, expression)?;
                let type_name = scope.get_expression_type(&expression)?;

                match type_name {
                    sway::TypeName::Undefined => panic!("Undefined type name"),
                    
                    sway::TypeName::Identifier { name, .. } => match name.as_str() {
                        "Identity" => match member.name.as_str() {
                            "transfer" => {
                                // to.transfer(amount) => std::asset::transfer(to, asset_id, amount)

                                if arguments.len() != 1 {
                                    panic!("Malformed address.transfer call, expected 1 argument, found {}", arguments.len());
                                }

                                //
                                // TODO: we need to determine what the asset id being transferred is
                                //

                                Ok(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::Identifier("std::asset::transfer".into()),
                                    generic_parameters: None,
                                    parameters: vec![
                                        expression,
                                        sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::Identifier("AssetId::default".into()),
                                            generic_parameters: None,
                                            parameters: vec![],
                                        }),
                                        self.translate_expression(translated_definition, scope, &arguments[0])?,
                                    ],
                                }))
                            }

                            "send" => {
                                // to.send(amount) => std::asset::transfer(to, asset_id, amount)

                                if arguments.len() != 1 {
                                    panic!("Malformed address.send call, expected 1 argument, found {}", arguments.len());
                                }

                                //
                                // TODO:
                                // The `address.send` function in solidity returns a bool, but the `std::asset::transfer` in sway has no return type.
                                // We need to determine what the asset id being transferred is.
                                //

                                Ok(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::Identifier("std::asset::transfer".into()),
                                    generic_parameters: None,
                                    parameters: vec![
                                        expression,
                                        sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::Identifier("AssetId::default".into()),
                                            generic_parameters: None,
                                            parameters: vec![],
                                        }),
                                        self.translate_expression(translated_definition, scope, &arguments[0])?,
                                    ],
                                }))
                            }

                            "delegatecall" => {
                                //
                                // TODO: is delegatecall possible?
                                //

                                Ok(sway::Expression::create_todo(Some(format!("{expression:?}"))))
                            }

                            "staticcall" => {
                                //
                                // TODO: is staticcall possible?
                                //

                                Ok(sway::Expression::create_todo(Some(format!("{expression:?}"))))
                            }

                            member => todo!("translate Identity member function call `{member}`: {} - {expression:#?}", sway::TabbedDisplayer(&expression))
                        }
                        
                        _ => todo!("translate {name} member function call: {} - {expression:#?}", sway::TabbedDisplayer(&expression))
                    }

                    sway::TypeName::Array { .. } => todo!("translate array member function call: {} - {expression:#?}", sway::TabbedDisplayer(&expression)),
                    sway::TypeName::Tuple { .. } => todo!("translate tuple member function call: {} - {expression:#?}", sway::TabbedDisplayer(&expression)),
                    sway::TypeName::String { .. } => todo!("translate string member function call: {} - {expression:#?}", sway::TabbedDisplayer(&expression)),
                }
            }

            solidity::Expression::FunctionCallBlock(_, function, block) => match function.as_ref() {
                solidity::Expression::MemberAccess(_, expression, member) => {
                    let expression = self.translate_expression(translated_definition, scope, expression)?;
                    let type_name = scope.get_expression_type(&expression)?;

                    match type_name {
                        sway::TypeName::Undefined => panic!("Undefined type name"),
                        
                        sway::TypeName::Identifier { name, .. } => match name.as_str() {
                            "Identity" => match member.name.as_str() {
                                "call" => {
                                    // address.call{value: x}("") => std::asset::transfer(to, asset_id, amount)

                                    if arguments.len() != 1 {
                                        panic!("Malformed address.call call, expected 1 argument, found {}", arguments.len());
                                    }

                                    let solidity::Statement::Args(_, block_args) = block.as_ref() else {
                                        panic!("Malformed address.call call, expected args block, found: {block:#?}");
                                    };

                                    let Some(value) = block_args.iter().find(|a| a.name.name == "value") else {
                                        panic!("Malformed address.call call, missing `value` argument in block: {block:#?}");
                                    };

                                    let value = self.translate_expression(translated_definition, scope, &value.expr)?;

                                    //
                                    // TODO:
                                    // The `address.call` function in solidity returns a bool and a return data pointer, but the `std::asset::transfer` in sway has no return type.
                                    // We need to determine what the asset id being transferred is.
                                    //

                                    Ok(sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::Identifier("std::asset::transfer".into()),
                                        generic_parameters: None,
                                        parameters: vec![
                                            expression,
                                            sway::Expression::from(sway::FunctionCall {
                                                function: sway::Expression::Identifier("AssetId::default".into()),
                                                generic_parameters: None,
                                                parameters: vec![],
                                            }),
                                            value,
                                        ],
                                    }))
                                }

                                _ => todo!("translate Identity member function call block `{member}`: {} - {expression:#?}", sway::TabbedDisplayer(&expression))
                            }
                            _ => todo!("translate {name} member function call block: {} - {expression:#?}", sway::TabbedDisplayer(&expression))
                        }

                        sway::TypeName::Array { .. } => todo!(),
                        sway::TypeName::Tuple { .. } => todo!(),
                        sway::TypeName::String { .. } => todo!(),
                    } 
                }

                _ => todo!("translate function call block expression: {} - {expression:#?}", expression.to_string())
            }

            _ => todo!("translate function call expression: {} - {expression:#?}", expression.to_string()),
        }
    }

    #[inline]
    fn translate_function_call_block_expression(
        &mut self,
        _translated_definition: &mut TranslatedDefinition,
        _scope: &mut TranslationScope,
        _function: &solidity::Expression,
        _block: &solidity::Statement,
    ) -> Result<sway::Expression, Error> {
        todo!("translate function call block expression")
    }

    #[inline]
    fn translate_named_function_call_expression(
        &mut self,
        _translated_definition: &mut TranslatedDefinition,
        _scope: &mut TranslationScope,
        _function: &solidity::Expression,
        _named_arguments: &[solidity::NamedArgument],
    ) -> Result<sway::Expression, Error> {
        todo!("translate named function call expression")
    }

    #[inline]
    fn translate_unary_expression(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        operator: &str,
        expression: &solidity::Expression,
    ) -> Result<sway::Expression, Error> {
        Ok(sway::Expression::from(sway::UnaryExpression {
            operator: operator.into(),
            expression: self.translate_expression(translated_definition, scope, expression)?,
        }))
    }

    #[inline]
    fn translate_power_expression(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        lhs: &solidity::Expression,
        rhs: &solidity::Expression,
    ) -> Result<sway::Expression, Error> {
        // lhs ** rhs => lhs.pow(rhs)

        // Ensure std::math::Power is imported for the pow function
        translated_definition.ensure_use_declared("std::math::Power");

        let lhs = self.translate_expression(translated_definition, scope, lhs)?;
        let rhs = self.translate_expression(translated_definition, scope, rhs)?;

        Ok(sway::Expression::from(sway::FunctionCall {
            function: sway::Expression::from(sway::MemberAccess {
                expression: lhs,
                member: "pow".into(),
            }),
            generic_parameters: None,
            parameters: vec![
                rhs,
            ],
        }))
    }
    
    #[inline]
    fn translate_binary_expression(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        operator: &str,
        lhs: &solidity::Expression,
        rhs: &solidity::Expression,
    ) -> Result<sway::Expression, Error> {
        // Hack: x.code.length == 0 => x.as_contract_id().is_none()
        if let solidity::Expression::MemberAccess(_, x, member2) = lhs {
            if let solidity::Expression::MemberAccess(_, x, member1) = x.as_ref() {
                if let solidity::Expression::Variable(solidity::Identifier { name, .. }) = x.as_ref() {
                    let variable = match scope.get_variable_from_old_name(name) {
                        Ok(variable) => variable,
                        Err(e) => panic!("{e}"),
                    };

                    match &variable.type_name {
                        sway::TypeName::Identifier { name, generic_parameters: None } if name == "Identity" => {
                            if member1.name == "code" && member2.name == "length" {
                                if let solidity::Expression::NumberLiteral(_, value, _, _) = rhs {
                                    if value == "0" {
                                        return Ok(sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::from(sway::MemberAccess {
                                                expression: sway::Expression::from(sway::FunctionCall {
                                                    function: sway::Expression::from(sway::MemberAccess {
                                                        expression: sway::Expression::Identifier(variable.new_name.clone()),
                                                        member: "as_contract_id".into(),
                                                    }),
                                                    generic_parameters: None,
                                                    parameters: vec![],
                                                }),
                                                member: "is_none".into(),
                                            }),
                                            generic_parameters: None,
                                            parameters: vec![],
                                        }));
                                    }
                                }
                            }
                        }

                        _ => {}
                    }
                }
            }
        }

        Ok(sway::Expression::from(sway::BinaryExpression {
            operator: operator.into(),
            lhs: self.translate_expression(translated_definition, scope, lhs)?,
            rhs: self.translate_expression(translated_definition, scope, rhs)?,
        }))
    }

    fn translate_variable_access_expression<'a>(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &'a mut TranslationScope,
        expression: &solidity::Expression,
    ) -> Result<(&'a mut TranslatedVariable, sway::Expression), Error> {
        match expression {
            solidity::Expression::Variable(solidity::Identifier { name, .. }) => {
                let variable = match scope.get_variable_from_old_name_mut(name.as_str()) {
                    Ok(variable) => variable,
                    Err(e) => panic!("{e}"),
                };

                let variable_name = variable.new_name.clone();
                let is_storage = variable.is_storage;

                Ok((
                    variable,
                    if is_storage {
                        sway::Expression::from(sway::MemberAccess {
                            expression: sway::Expression::Identifier("storage".into()),
                            member: variable_name,
                        })
                    } else {
                        sway::Expression::Identifier(variable_name)
                    }
                ))
            }

            solidity::Expression::ArraySubscript(_, expression, Some(index)) => {
                let index = self.translate_expression(translated_definition, scope, index.as_ref())?;
                let (variable, expression) = self.translate_variable_access_expression(translated_definition, scope, expression)?;
                let is_storage = variable.is_storage;

                Ok((
                    variable,
                    if is_storage {
                        sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::from(sway::MemberAccess {
                                expression,
                                member: "get".into(),
                            }),
                            generic_parameters: None,
                            parameters: vec![index],
                        })
                    } else {
                        sway::Expression::from(sway::ArrayAccess {
                            expression,
                            index,
                        })
                    }
                ))
            }

            _ => todo!("translate variable access expression: {expression:#?}")
        }
    }

    #[inline]
    fn translate_conditional_operator_expression(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        condition: &solidity::Expression,
        then_value: &solidity::Expression,
        else_value: &solidity::Expression,
    ) -> Result<sway::Expression, Error> {
        // if condition { then_value } else { else_value }
        Ok(sway::Expression::from(sway::If {
            condition: Some(self.translate_expression(translated_definition, scope, condition)?),
            then_body: sway::Block {
                statements: vec![],
                final_expr: Some(
                    self.translate_expression(translated_definition, scope, then_value)?
                ),
            },
            else_if: Some(Box::new(sway::If {
                condition: None,
                then_body: sway::Block {
                    statements: vec![],
                    final_expr: Some(
                        self.translate_expression(translated_definition, scope, else_value)?
                    ),
                },
                else_if: None,
            })),
        }))
    }

    #[inline]
    fn translate_assignment_expression(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        operator: &str,
        lhs: &solidity::Expression,
        rhs: &solidity::Expression,
    ) -> Result<sway::Expression, Error> {
        let (variable, expression) = self.translate_variable_access_expression(translated_definition, scope, lhs)?;

        variable.mutation_count += 1;

        if variable.is_storage {
            Ok(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::from(sway::MemberAccess {
                    expression: expression.clone(),
                    member: "write".into(),
                }),
                generic_parameters: None,
                parameters: vec![
                    match operator {
                        "=" => self.translate_pre_or_post_operator_value_expression(translated_definition, scope, rhs)?,

                        _ => {
                            variable.read_count += 1;
                            
                            sway::Expression::from(sway::BinaryExpression {
                                operator: operator.trim_end_matches('=').into(),

                                lhs: sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::from(sway::MemberAccess {
                                        expression: expression.clone(),
                                        member: "read".into(),
                                    }),
                                    generic_parameters: None,
                                    parameters: vec![],
                                }),

                                rhs: self.translate_expression(translated_definition, scope, rhs)?,
                            })
                        }
                    },
                ],
            }))
        } else {
            match &variable.type_name {
                sway::TypeName::Identifier { name, .. } if name == "Vec" => {
                    let sway::Expression::ArrayAccess(array_access) = expression else {
                        panic!("Expected array access expression, found {expression:#?}");
                    };

                    Ok(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::from(sway::MemberAccess {
                            expression: array_access.expression.clone(),
                            member: "set".into(),
                        }),
                        generic_parameters: None,
                        parameters: vec![
                            array_access.index.clone(),
                            match operator {
                                "=" => self.translate_pre_or_post_operator_value_expression(translated_definition, scope, rhs)?,
                                
                                _ => {
                                    variable.read_count += 1;
                                    
                                    sway::Expression::from(sway::BinaryExpression {
                                        operator: operator.trim_end_matches('=').into(),
        
                                        lhs: sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::from(sway::MemberAccess {
                                                expression: sway::Expression::from(sway::FunctionCall {
                                                    function: sway::Expression::from(sway::MemberAccess {
                                                        expression: array_access.expression.clone(),
                                                        member: "get".into(),
                                                    }),
                                                    generic_parameters: None,
                                                    parameters: vec![
                                                        array_access.index.clone(),
                                                    ],
                                                }),
                                                member: "unwrap".into(),
                                            }),
                                            generic_parameters: None,
                                            parameters: vec![],
                                        }),
        
                                        rhs: self.translate_expression(translated_definition, scope, rhs)?,
                                    })
                                }
                            }
                        ],
                    }))
                }

                _ => Ok(sway::Expression::from(sway::BinaryExpression {
                    operator: operator.into(),
                    lhs: self.translate_expression(translated_definition, scope, lhs)?,
                    rhs: self.translate_pre_or_post_operator_value_expression(translated_definition, scope, rhs)?,
                })),
            }
        }
    }

    #[inline]
    fn translate_pre_or_post_operator_value_expression(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        expression: &solidity::Expression,
    ) -> Result<sway::Expression, Error> {
        match expression {
            solidity::Expression::PreIncrement(loc, x) => self.translate_pre_operator_expression(translated_definition, scope, loc, x, "+="),
            solidity::Expression::PreDecrement(loc, x) => self.translate_pre_operator_expression(translated_definition, scope, loc, x, "-="),
            solidity::Expression::PostIncrement(loc, x) => self.translate_post_operator_expression(translated_definition, scope, loc, x, "+="),
            solidity::Expression::PostDecrement(loc, x) => self.translate_post_operator_expression(translated_definition, scope, loc, x, "-="),
            _ => self.translate_expression(translated_definition, scope, expression),
        }
    }

    #[inline]
    fn translate_pre_operator_expression(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        loc: &solidity::Loc,
        x: &solidity::Expression,
        operator: &str,
    ) -> Result<sway::Expression, Error> {
        let assignment = sway::Statement::from(
            self.translate_assignment_expression(
                translated_definition,
                scope,
                operator,
                x,
                &solidity::Expression::NumberLiteral(*loc, "1".into(), "".into(), None),
            )?
        );

        let (variable, expression) = self.translate_variable_access_expression(translated_definition, scope, x)?;
        variable.read_count += 1;

        Ok(sway::Expression::from(sway::Block {
            statements: vec![assignment],
            final_expr: Some(
                if variable.is_storage {
                    sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::from(sway::MemberAccess {
                            expression,
                            member: "read".into(),
                        }),
                        generic_parameters: None,
                        parameters: vec![],
                    })
                } else {
                    expression
                }
            ),
        }))
    }

    #[inline]
    fn translate_post_operator_expression(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        loc: &solidity::Loc,
        x: &solidity::Expression,
        operator: &str,
    ) -> Result<sway::Expression, Error> {
        let assignment = sway::Statement::from(
            self.translate_assignment_expression(
                translated_definition,
                scope,
                operator,
                x,
                &solidity::Expression::NumberLiteral(*loc, "1".into(), "".into(), None),
            )?
        );

        let (variable, expression) = self.translate_variable_access_expression(translated_definition, scope, x)?;
        variable.read_count += 1;

        let variable_name = if variable.is_storage {
            variable.new_name.clone()
        } else {
            format!("_{}", variable.new_name)
        };

        Ok(sway::Expression::from(sway::Block {
            statements: vec![
                sway::Statement::from(sway::Let {
                    pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                        is_mutable: false,
                        name: variable_name.clone(),
                    }),
                    type_name: None,
                    value: if variable.is_storage {
                        sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::from(sway::MemberAccess {
                                expression,
                                member: "read".into(),
                            }),
                            generic_parameters: None,
                            parameters: vec![],
                        })
                    } else {
                        expression
                    },
                }),
                assignment,
            ],
            final_expr: Some(sway::Expression::Identifier(variable_name)),
        }))
    }

    #[inline]
    fn translate_new_expression(
        &mut self,
        _translated_definition: &mut TranslatedDefinition,
        _scope: &mut TranslationScope,
        expression: &solidity::Expression,
    ) -> Result<sway::Expression, Error> {
        todo!("translate new expression: {} - {expression:#?}", expression.to_string())
    }
    
    #[inline]
    fn translate_delete_expression(
        &mut self,
        _translated_definition: &mut TranslatedDefinition,
        _scope: &mut TranslationScope,
        expression: &solidity::Expression,
    ) -> Result<sway::Expression, Error> {
        todo!("translate delete expression: {} - {expression:#?}", expression.to_string())
    }

    // --------------------------------------------------------------------------------
    // INLINE ASSEMBLY TRANSLATION CODE
    // --------------------------------------------------------------------------------

    #[inline]
    fn translate_assembly_statement(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        _dialect: &Option<solidity::StringLiteral>,
        _flags: &Option<Vec<solidity::StringLiteral>>,
        yul_block: &solidity::YulBlock,
    ) -> Result<sway::Statement, Error> {
        let mut block = sway::Block::default();

        for yul_statement in yul_block.statements.iter() {
            match yul_statement {
                solidity::YulStatement::Assign(_, identifiers, value) => {
                    let identifiers = identifiers.iter()
                        .map(|i| self.translate_yul_expression(translated_definition, scope, i))
                        .collect::<Result<Vec<_>, _>>()?;

                    for identifier in identifiers.iter() {
                        let sway::Expression::Identifier(name) = identifier else {
                            panic!("Expected identifier, found: {identifier:#?}")
                        };

                        let variable = match scope.get_variable_from_new_name_mut(name) {
                            Ok(variable) => variable,
                            Err(e) => panic!("{e}"),
                        };

                        variable.mutation_count += 1;
                    }

                    let value = self.translate_yul_expression(translated_definition, scope, value)?;
                    
                    block.statements.push(sway::Statement::from(sway::Expression::from(sway::BinaryExpression {
                        operator: "=".into(),
                        lhs: if identifiers.len() == 1 {
                            identifiers[0].clone()
                        } else {
                            sway::Expression::Tuple(identifiers)
                        },
                        rhs: value,
                    })));
                }
                
                solidity::YulStatement::VariableDeclaration(_, identifiers, value) => {
                    // Collect variable translations for the scope
                    let mut variables = vec![];
    
                    for p in identifiers.iter() {
                        variables.push(TranslatedVariable {
                            old_name: p.id.name.clone(),
                            new_name: self.translate_naming_convention(p.id.name.as_str(), Case::Snake),
                            type_name: sway::TypeName::Identifier {
                                name: "u256".into(),
                                generic_parameters: None,
                            },
                            ..Default::default()
                        });
                    }
    
                    scope.variables.extend(variables.clone());
    
                    // Create the variable declaration statement
                    block.statements.push(sway::Statement::from(sway::Let {
                        pattern: if variables.len() == 1 {
                            sway::LetPattern::Identifier(sway::LetIdentifier {
                                is_mutable: false,
                                name: variables[0].new_name.clone(),
                            })
                        } else {
                            sway::LetPattern::Tuple(
                                variables.iter()
                                    .map(|p| sway::LetIdentifier {
                                        is_mutable: false,
                                        name: p.new_name.clone(),
                                    })
                                    .collect()
                            )
                        },
    
                        type_name: None,
                        
                        value: if let Some(value) = value.as_ref() {
                            self.translate_yul_expression(translated_definition, scope, value)?
                        } else {
                            sway::Expression::create_value_expression(
                                &sway::TypeName::Identifier {
                                    name: "u256".into(),
                                    generic_parameters: None,
                                },
                                None,
                            )
                        },
                    }));
                }

                solidity::YulStatement::If(_, _, _) => todo!("yul if statement: {yul_statement:#?}"),
                solidity::YulStatement::For(_) => todo!("yul for statement: {yul_statement:#?}"),
                solidity::YulStatement::Switch(_) => todo!("yul switch statement: {yul_statement:#?}"),
                solidity::YulStatement::Leave(_) => todo!("yul leave statement: {yul_statement:#?}"),
                solidity::YulStatement::Break(_) => todo!("yul break statement: {yul_statement:#?}"),
                solidity::YulStatement::Continue(_) => todo!("yul continue statement: {yul_statement:#?}"),
                solidity::YulStatement::Block(_) => todo!("yul block statement: {yul_statement:#?}"),
                solidity::YulStatement::FunctionDefinition(_) => todo!("yul function definition statement: {yul_statement:#?}"),
                
                solidity::YulStatement::FunctionCall(_) => {
                    block.statements.push(sway::Statement::from(sway::Expression::create_todo(Some(yul_statement.to_string()))));
                }

                solidity::YulStatement::Error(_) => todo!("yul error statement: {yul_statement:#?}"),
            }
        }
        
        Ok(sway::Statement::from(sway::Expression::from(block)))
    }

    fn translate_yul_expression(
        &mut self,
        translated_definition: &mut TranslatedDefinition,
        scope: &mut TranslationScope,
        expression: &solidity::YulExpression,
    ) -> Result<sway::Expression, Error> {
        match expression {
            solidity::YulExpression::BoolLiteral(_, value, _) => Ok(sway::Expression::from(sway::Literal::Bool(*value))),
            solidity::YulExpression::NumberLiteral(_, value, _, _) => Ok(sway::Expression::from(sway::Literal::DecInt(value.parse().unwrap()))),
            solidity::YulExpression::HexNumberLiteral(_, value, _) => Ok(sway::Expression::from(sway::Literal::HexInt(u64::from_str_radix(value.trim_start_matches("0x"), 16).unwrap()))),
            solidity::YulExpression::HexStringLiteral(_, _) => todo!("yul hex string literal expression: {expression:#?}"),
            solidity::YulExpression::StringLiteral(string_literal, _) => Ok(sway::Expression::from(sway::Literal::String(string_literal.string.clone()))),
            
            solidity::YulExpression::Variable(solidity::Identifier { name, .. }) => {
                let variable = match scope.get_variable_from_old_name(name.as_str()) {
                    Ok(variable) => variable,
                    Err(e) => panic!("{e}"),
                };

                if variable.is_storage {
                    Ok(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::from(sway::MemberAccess {
                            expression: sway::Expression::from(sway::MemberAccess {
                                expression: sway::Expression::Identifier("storage".into()),
                                member: variable.new_name.clone(),
                            }),
                            member: "read".into(),
                        }),
                        generic_parameters: None,
                        parameters: vec![],
                    }))
                } else {
                    Ok(sway::Expression::Identifier(variable.new_name.clone()))
                }
            }

            solidity::YulExpression::FunctionCall(function_call) => {
                let parameters = function_call.arguments.iter()
                    .map(|a| self.translate_yul_expression(translated_definition, scope, a))
                    .collect::<Result<Vec<_>, _>>()?;

                match function_call.id.name.as_str() {
                    "add" => {
                        if parameters.len() != 2 {
                            panic!("Invalid yul add function call, expected 2 parameters, found {}", parameters.len());
                        }

                        Ok(sway::Expression::from(sway::BinaryExpression {
                            operator: "+".into(),
                            lhs: parameters[0].clone(),
                            rhs: parameters[1].clone(),
                        }))
                    }

                    "sub" => {
                        if parameters.len() != 2 {
                            panic!("Invalid yul sub function call, expected 2 parameters, found {}", parameters.len());
                        }

                        Ok(sway::Expression::from(sway::BinaryExpression {
                            operator: "-".into(),
                            lhs: parameters[0].clone(),
                            rhs: parameters[1].clone(),
                        }))
                    }

                    "mul" => {
                        if parameters.len() != 2 {
                            panic!("Invalid yul mul function call, expected 2 parameters, found {}", parameters.len());
                        }

                        Ok(sway::Expression::from(sway::BinaryExpression {
                            operator: "*".into(),
                            lhs: parameters[0].clone(),
                            rhs: parameters[1].clone(),
                        }))
                    }

                    "div" => {
                        if parameters.len() != 2 {
                            panic!("Invalid yul div function call, expected 2 parameters, found {}", parameters.len());
                        }

                        Ok(sway::Expression::from(sway::BinaryExpression {
                            operator: "/".into(),
                            lhs: parameters[0].clone(),
                            rhs: parameters[1].clone(),
                        }))
                    }

                    "lt" => {
                        if parameters.len() != 2 {
                            panic!("Invalid yul lt function call, expected 2 parameters, found {}", parameters.len());
                        }

                        Ok(sway::Expression::from(sway::BinaryExpression {
                            operator: "<".into(),
                            lhs: parameters[0].clone(),
                            rhs: parameters[1].clone(),
                        }))
                    }

                    "gt" => {
                        if parameters.len() != 2 {
                            panic!("Invalid yul gt function call, expected 2 parameters, found {}", parameters.len());
                        }

                        Ok(sway::Expression::from(sway::BinaryExpression {
                            operator: ">".into(),
                            lhs: parameters[0].clone(),
                            rhs: parameters[1].clone(),
                        }))
                    }

                    "not" => {
                        if parameters.len() != 1 {
                            panic!("Invalid yul not function call, expected 1 parameter, found {}", parameters.len());
                        }

                        Ok(sway::Expression::from(sway::BinaryExpression {
                            operator: "!=".into(),
                            lhs: parameters[0].clone(),
                            rhs: sway::Expression::from(sway::Literal::DecInt(0)),
                        }))
                    }

                    "addmod" => {
                        if parameters.len() != 3 {
                            panic!("Invalid yul addmod function call, expected 3 parameters, found {}", parameters.len());
                        }

                        Ok(sway::Expression::from(sway::BinaryExpression {
                            operator: "%".into(),
                            lhs: sway::Expression::Tuple(vec![
                                sway::Expression::from(sway::BinaryExpression {
                                    operator: "+".into(),
                                    lhs: parameters[0].clone(),
                                    rhs: parameters[1].clone(),
                                }),
                            ]),
                            rhs: parameters[3].clone(),
                        }))
                    }

                    "mulmod" => {
                        if parameters.len() != 3 {
                            panic!("Invalid yul addmod function call, expected 3 parameters, found {}", parameters.len());
                        }

                        Ok(sway::Expression::from(sway::BinaryExpression {
                            operator: "%".into(),
                            lhs: sway::Expression::Tuple(vec![
                                sway::Expression::from(sway::BinaryExpression {
                                    operator: "*".into(),
                                    lhs: parameters[0].clone(),
                                    rhs: parameters[1].clone(),
                                }),
                            ]),
                            rhs: parameters[2].clone(),
                        }))
                    }

                    "mload" => {
                        Ok(sway::Expression::create_todo(Some(expression.to_string())))
                    }

                    "mstore" => {
                        Ok(sway::Expression::create_todo(Some(expression.to_string())))
                    }

                    "keccak256" => {
                        Ok(sway::Expression::create_todo(Some(expression.to_string())))
                    }

                    "byte" => {
                        Ok(sway::Expression::create_todo(Some(expression.to_string())))
                    }

                    "create2" => {
                        Ok(sway::Expression::create_todo(Some(expression.to_string())))
                    }

                    name => todo!("look up yul function in scope: \"{name}\"")
                }
            }

            solidity::YulExpression::SuffixAccess(_, _, _) => todo!("yul suffix access expression: {expression:#?}"),
        }
    }
}
