use crate::{
    errors::Error,
    sway,
    translate::{TranslatedDefinition, TranslationScope, TranslatedVariable},
    Options,
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

impl TryFrom<&Options> for Project {
    type Error = Error;

    fn try_from(options: &Options) -> Result<Self, Self::Error> {
        let mut project = Project::default();

        for path in options.contract_files.iter() {
            project.parse_solidity_source_unit(path)?;
        }

        Ok(project)
    }
}

impl Project {
    /// Attempts to parse the file from the supplied `path`.
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

    /// Loads line ranges in a specfic file `path` from the provided `source` text.
    fn load_line_ranges(&mut self, path: PathBuf, source: &str) {
        let mut line_range = (0usize, 0usize);

        for (i, c) in source.chars().enumerate() {
            if c == '\n' {
                line_range.1 = i;
                self.line_ranges.entry(path.clone()).or_insert(vec![]).push(line_range);
                line_range = (i + 1, 0);
            }
        }

        if line_range.1 > line_range.0 {
            self.line_ranges.entry(path.clone()).or_insert(vec![]).push(line_range);
        }
    }

    pub fn translate(&mut self) -> Result<(), Error> {
        let solidity_source_units = self.solidity_source_units.clone();
        let translation_queue = self.create_translation_queue()?;

        // Translate source units through translation queue
        for source_unit_path in translation_queue.iter() {
            // Get the parsed source unit
            let source_unit = solidity_source_units.borrow().get(source_unit_path).unwrap().clone();

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
                        self.translate_contract_definition(&source_unit_path, contract_definition)?;
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
        }

        //
        // TODO:
        // Flatten final contract
        // Create forc project on disk
        //

        Ok(())
    }

    fn create_translation_queue(&mut self) -> Result<Vec<PathBuf>, Error> {
        let mut translation_queue: Vec<PathBuf> = vec![];
        let mut source_unit_paths = self.solidity_source_units.borrow().keys().cloned().collect::<Vec<_>>();

        while let Some(source_unit_path) = source_unit_paths.pop() {
            let source_unit_directory = source_unit_path.parent().unwrap();

            let mut queue_import_path = |import_path: &solidity::ImportPath| -> Result<(), Error> {
                match import_path {
                    solidity::ImportPath::Filename(filename) => {
                        if filename.string.starts_with("@") {
                            todo!("handle global import paths (i.e: node_modules)")
                        }

                        // Get the canonical path of the imported source unit
                        let import_path = source_unit_directory.join(filename.string.clone()).canonicalize().map_err(|e| Error::Wrapped(Box::new(e)))?;
                        source_unit_paths.push(import_path.clone());
                        
                        // If a source unit is already queued, move it to the top of the queue
                        if let Some((index, _)) = translation_queue.iter().enumerate().find(|(_, p)| import_path.to_string_lossy() == p.to_string_lossy()) {
                            translation_queue.remove(index);
                            translation_queue.insert(0, import_path);
                        }
                        // If a source unit is not queued, add it to the end of the queue
                        else {
                            translation_queue.push(import_path);
                        }
                    }

                    solidity::ImportPath::Path(path) => todo!("Experimental solidity import path: {path}"),
                }

                Ok(())
            };
            
            // Parse the source unit if it has not been parsed already
            if !self.solidity_source_units.borrow().contains_key(&source_unit_path) {
                self.parse_solidity_source_unit(&source_unit_path)?;
            }

            // Get the parsed source unit
            let source_unit = self.solidity_source_units.borrow().get(&source_unit_path).unwrap().clone();

            for source_unit_part in source_unit.0.iter() {
                // Only check import directives
                let solidity::SourceUnitPart::ImportDirective(import_directive) = source_unit_part else { continue };

                // Queue the imported source unit for translation
                match import_directive {
                    solidity::Import::Plain(import_path, _) => queue_import_path(import_path)?,
                    solidity::Import::GlobalSymbol(import_path, _, _) => queue_import_path(import_path)?,
                    solidity::Import::Rename(import_path, _, _) => queue_import_path(import_path)?,
                }
            }

            // Add the source unit path to the end of the translation queue
            if !translation_queue.contains(&source_unit_path) {
                translation_queue.push(source_unit_path);
            }
        }

        Ok(translation_queue)
    }

    fn translate_naming_convention(&mut self, name: &str, case: Case) -> String {
        let prefix = name.chars().take_while(|c| *c == '_').collect::<String>();
        let postfix = name.chars().rev().take_while(|c| *c == '_').collect::<String>();
        format!("{prefix}{}{postfix}", name.to_case(case))
    }

    fn translate_type_name(&mut self, source_unit_path: &Path, type_name: &solidity::Expression) -> sway::TypeName {
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

                solidity::Type::Int(bits) => todo!("signed integer types"),

                solidity::Type::Uint(bits) => sway::TypeName::Identifier {
                    name: match *bits {
                        8 => "u8".into(),
                        16 => "u16".into(),
                        32 => "u32".into(),
                        x if x >= 64 => "u64".into(), // TODO: is this really ok?
                        _ => todo!("unsigned integers of non-standard bit sizes")
                    },
                    generic_parameters: None,
                },

                solidity::Type::Bytes(length) => sway::TypeName::Array {
                    type_name: Box::new(sway::TypeName::Identifier {
                        name: "u8".into(),
                        generic_parameters: None,
                    }),
                    length: *length as usize,
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
                                name: self.translate_type_name(source_unit_path, key.as_ref()),
                                implements: vec![],
                            },
                            sway::GenericParameter {
                                name: self.translate_type_name(source_unit_path, value.as_ref()),
                                implements: vec![],
                            },
                        ],
                    }),
                },

                solidity::Type::Function { params, attributes, returns } => todo!("function types"),
            }

            _ => unimplemented!("type name expression: {type_name:?}"),
        }
    }

    fn translate_literal_expression(&mut self, source_unit_path: &Path, value: &solidity::Expression) -> sway::Expression {
        match value {
            solidity::Expression::BoolLiteral(_, value) => sway::Expression::Literal(sway::Literal::Bool(*value)),
            solidity::Expression::NumberLiteral(_, value, _, _) => sway::Expression::Literal(sway::Literal::DecInt(value.parse().unwrap())),
            solidity::Expression::HexNumberLiteral(_, value, _) => sway::Expression::Literal(sway::Literal::HexInt(u64::from_str_radix(value.trim_start_matches("0x"), 16).unwrap())),
            solidity::Expression::StringLiteral(values) => sway::Expression::Literal(sway::Literal::String(values.iter().map(|v| v.string.clone()).collect::<Vec<_>>().join(""))),
            solidity::Expression::ArrayLiteral(_, values) => sway::Expression::Array(sway::Array {
                elements: values.iter().map(|x| self.translate_literal_expression(source_unit_path, x)).collect(),
            }),
            _ => panic!("Invalid literal expression: {value:?}"),
        }
    }
    
    fn translate_contract_definition(
        &mut self,
        source_unit_path: &Path,
        solidity_definition: &solidity::ContractDefinition,
    ) -> Result<(), Error> {
        let definition_name = solidity_definition.name.as_ref().unwrap().name.clone();
        let inherits: Vec<String> = solidity_definition.base.iter().map(|b| b.name.identifiers.iter().map(|i| i.name.clone()).collect::<Vec<_>>().join(".")).collect();

        // Create a new translation container
        let mut sway_definition = TranslatedDefinition::new(
            source_unit_path,
            solidity_definition.ty.clone(),
            definition_name.clone(),
            inherits.clone()
        );

        // Collect each type definition ahead of time for contextual reasons
        for part in solidity_definition.parts.iter() {
            let solidity::ContractPart::TypeDefinition(type_definition) = part else { continue };
            
            sway_definition.type_definitions.push(sway::TypeDefinition {
                is_public: true,
                name: sway::TypeName::Identifier {
                    name: type_definition.name.name.clone(),
                    generic_parameters: None,
                },
                underlying_type: Some(self.translate_type_name(source_unit_path, &type_definition.ty)),
            });
        }

        // Collect each struct ahead of time for contextual reasons
        for part in solidity_definition.parts.iter() {
            let solidity::ContractPart::StructDefinition(struct_definition) = part else { continue };

            sway_definition.structs.push(sway::Struct {
                attributes: None,
                is_public: true,
                name: struct_definition.name.as_ref().unwrap().name.clone(),
                generic_parameters: None,
                fields: struct_definition.fields.iter().map(|f| {
                    sway::StructField {
                        is_public: true,
                        name: self.translate_naming_convention(f.name.as_ref().unwrap().name.as_str(), Case::Snake), // TODO: keep track of original name
                        type_name: self.translate_type_name(source_unit_path, &f.ty),
                    }
                }).collect(),
            });
        }

        // Collect each enum ahead of time for contextual reasons
        for part in solidity_definition.parts.iter() {
            let solidity::ContractPart::EnumDefinition(enum_definition) = part else { continue };

            todo!("enum definitions")
        }

        // Collect each event and error ahead of time for contextual reasons
        for part in solidity_definition.parts.iter() {
            match part {
                solidity::ContractPart::EventDefinition(event_definition) => {
                    sway_definition.get_events_enum().variants.push(sway::EnumVariant {
                        name: event_definition.name.as_ref().unwrap().name.clone(),
                        type_name: if event_definition.fields.len() == 1 {
                            self.translate_type_name(source_unit_path, &event_definition.fields[0].ty)
                        } else {
                            sway::TypeName::Tuple {
                                type_names: event_definition.fields.iter().map(|f| {
                                    self.translate_type_name(source_unit_path, &f.ty)
                                }).collect(),
                            }
                        },
                    });
                }

                solidity::ContractPart::ErrorDefinition(error_definition) => {
                    sway_definition.get_errors_enum().variants.push(sway::EnumVariant {
                        name: error_definition.name.as_ref().unwrap().name.clone(),
                        type_name: if error_definition.fields.len() == 1 {
                            self.translate_type_name(source_unit_path, &error_definition.fields[0].ty)
                        } else {
                            sway::TypeName::Tuple {
                                type_names: error_definition.fields.iter().map(|f| {
                                    self.translate_type_name(source_unit_path, &f.ty)
                                }).collect(),
                            }
                        },
                    });
                }
                
                _ => {}
            }
        }

        // Resolve all using-for statements ahead of time for contextual reasons
        for part in solidity_definition.parts.iter() {
            let solidity::ContractPart::Using(using) = part else { continue };

            todo!("using-for statements")
        }

        // Keep track of storage variables for function scopes
        let mut storage_variables = vec![];

        // Collect each storage field ahead of time for contextual reasons
        for part in solidity_definition.parts.iter() {
            let solidity::ContractPart::VariableDefinition(variable_definition) = part else { continue };

            // Translate the variable's type name
            let variable_type_name = self.translate_type_name(source_unit_path, &variable_definition.ty);

            // Collect information about the variable from its attributes
            let is_public = variable_definition.attrs.iter().any(|x| matches!(x, solidity::VariableAttribute::Visibility(solidity::Visibility::External(_) | solidity::Visibility::Public(_))));
            let is_constant = variable_definition.attrs.iter().any(|x| matches!(x, solidity::VariableAttribute::Constant(_)));
            let is_immutable = variable_definition.attrs.iter().any(|x| matches!(x, solidity::VariableAttribute::Immutable(_)));

            // Handle constant variable definitions
            if is_constant {
                let variable_name = self.translate_naming_convention(variable_definition.name.as_ref().unwrap().name.as_str(), Case::ScreamingSnake); // TODO: keep track of original name

                todo!("contract constants");
                continue;
            }
            
            // Handle immutable variable definitions
            if is_immutable {
                let variable_name = self.translate_naming_convention(variable_definition.name.as_ref().unwrap().name.as_str(), Case::Snake); // TODO: keep track of original name

                todo!("immutable variables");
                continue;
            }
            
            // Handle all other variable definitions
            let old_name = variable_definition.name.as_ref().unwrap().name.clone();
            let new_name = self.translate_naming_convention(old_name.as_str(), Case::Snake); // TODO: keep track of original name

            // Add the storage variable for function scopes
            storage_variables.push(TranslatedVariable {
                old_name,
                new_name: new_name.clone(),
                type_name: variable_type_name.clone(),
                is_storage: true,
                statement_index: None,
                mutation_count: 0,
            });

            // Add the storage field to the storage block
            sway_definition.get_storage().fields.push(sway::StorageField {
                name: new_name.clone(), // TODO: keep track of original name
                type_name: variable_type_name.clone(),
                value: sway::Expression::create_value_expression(
                    &variable_type_name,
                    variable_definition.initializer.as_ref().map(|x| {
                        self.translate_literal_expression(source_unit_path, x)
                    }).as_ref(),
                ),
            });

            // Generate a getter function if the storage field is public
            if !is_public {
                continue;
            }
        
            // Create the function declaration for the abi
            let mut sway_function = sway::Function {
                attributes: Some(sway::AttributeList {
                    attributes: vec![
                        sway::Attribute {
                            name: "storage".into(),
                            parameters: Some(vec![
                                "read".into(),
                            ]),
                        },
                    ],
                }),
                is_public: false,
                name: new_name.clone(), // TODO: keep track of original name
                generic_parameters: None,
                parameters: sway::ParameterList::default(), // TODO: create parameters for StorageMap getter functions
                return_type: Some(variable_type_name.clone()), // TODO: get proper return type for StorageMap getter functions
                body: None,
            };

            // Add the function to the abi
            sway_definition.get_abi().functions.push(sway_function.clone());

            // Create the body for the toplevel function
            sway_function.body = Some(sway::Block {
                statements: vec![],
                // TODO: change for StorageMap getter functions
                final_expr: Some(sway::Expression::FunctionCall(Box::new(sway::FunctionCall {
                    function: sway::Expression::MemberAccess(Box::new(sway::MemberAccess {
                        expression: sway::Expression::MemberAccess(Box::new(sway::MemberAccess {
                            expression: sway::Expression::Identifier("storage".into()),
                            member: new_name.clone(),
                        })),
                        member: "read".into(),
                    })),
                    generic_parameters: None,
                    parameters: vec![],
                }))),
            });

            // Add the toplevel function
            sway_definition.functions.push(sway_function.clone());

            // Create the body for the contract impl's function wrapper
            sway_function.body = Some(sway::Block {
                statements: vec![],
                // TODO: change for StorageMap getter functions
                final_expr: Some(sway::Expression::FunctionCall(Box::new(sway::FunctionCall {
                    function: sway::Expression::Identifier(format!("::{}", sway_function.name)),
                    generic_parameters: None,
                    parameters: vec![],
                }))),
            });

            // Add the function wrapper to the contract impl
            sway_definition.get_contract_impl().items.push(sway::ImplItem::Function(sway_function));
        }
        
        //
        // TODO:
        // We may need to do a first pass scan to collect information about all available functions before
        // translating each of their bodies so that we know what functions are availble in the toplevel scope
        //

        // Translate each function
        for part in solidity_definition.parts.iter() {
            let solidity::ContractPart::FunctionDefinition(function_definition) = part else { continue };

            // Collect information about the function from its type
            let is_constructor = matches!(function_definition.ty, solidity::FunctionTy::Constructor);
            let is_fallback = matches!(function_definition.ty, solidity::FunctionTy::Fallback);
            let is_receive = matches!(function_definition.ty, solidity::FunctionTy::Receive);
            let is_modifier = matches!(function_definition.ty, solidity::FunctionTy::Modifier);

            // Collect information about the function from its attributes
            let is_public = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Visibility(solidity::Visibility::External(_) | solidity::Visibility::Public(_))));
            let is_constant = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Mutability(solidity::Mutability::Constant(_))));
            let is_pure = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Mutability(solidity::Mutability::Pure(_))));
            let is_view = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Mutability(solidity::Mutability::View(_))));
            let is_payable = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Mutability(solidity::Mutability::Payable(_))));
            let is_virtual = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Virtual(_)));
            let is_override = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Override(_, _)));

            //
            // TODO:
            // Handle virtual functions
            // Handle function overrides
            //

            let function_name = if is_constructor {
                "constructor".to_string()
            } else if is_fallback {
                "fallback".to_string()
            } else if is_receive {
                "receive".to_string()
            } else {
                self.translate_naming_convention(function_definition.name.as_ref().unwrap().name.as_str(), Case::Snake)
            };

            if is_modifier {
                println!("WARNING: modifiers not yet implemented");
                continue;
            }
            
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
                name: function_name.clone(), // TODO: keep track of original name
                generic_parameters: None,

                parameters: sway::ParameterList {
                    entries: function_definition.params.iter().map(|(_, p)| {
                        sway::Parameter {
                            name: self.translate_naming_convention(p.as_ref().unwrap().name.as_ref().unwrap().name.as_str(), Case::Snake), // TODO: keep track of original name
                            type_name: self.translate_type_name(source_unit_path, &p.as_ref().unwrap().ty),
                        }
                    }).collect(),
                },

                return_type: if function_definition.returns.is_empty() {
                    None
                } else {
                    Some(if function_definition.returns.len() == 1 {
                        self.translate_type_name(source_unit_path, &function_definition.returns[0].1.as_ref().unwrap().ty)
                    } else {
                        sway::TypeName::Tuple {
                            type_names: function_definition.returns.iter().map(|(_, p)| {
                                self.translate_type_name(source_unit_path, &p.as_ref().unwrap().ty)
                            }).collect(),
                        }
                    })
                },

                body: None,
            };

            // Add the function declaration to the abi if it's public
            if is_public {
                sway_definition.get_abi().functions.push(sway_function.clone());
            }

            // Convert the statements in the function's body (if any)
            let Some(solidity::Statement::Block { statements, .. }) = function_definition.body.as_ref() else { continue };

            // Create the scope for the body of the toplevel function
            let mut scope = TranslationScope {
                parent: None,
                variables: storage_variables.clone(),
            };

            // Add the function parameters to the scope
            scope.variables.extend(
                function_definition.params.iter().map(|(_, p)| {
                    let old_name = p.as_ref().unwrap().name.as_ref().unwrap().name.clone();
                    let new_name = self.translate_naming_convention(old_name.as_str(), Case::Snake);
                    let type_name = self.translate_type_name(source_unit_path, &p.as_ref().unwrap().ty);

                    TranslatedVariable {
                        old_name,
                        new_name,
                        type_name,
                        is_storage: false,
                        statement_index: None,
                        mutation_count: 0,
                    }
                })
            );

            // Translate the body for the toplevel function
            let mut function_body = self.translate_block(source_unit_path, scope, statements.as_slice())?;

            // Check if the final statement returns a value and change it to be the final expression of the block
            if let Some(sway::Statement::Expression(sway::Expression::Return(Some(value)))) = function_body.statements.last().cloned() {
                function_body.statements.pop();
                function_body.final_expr = Some(*value);
            }

            // Create the body for the toplevel function
            sway_function.body = Some(function_body);

            //
            // TODO:
            // Propogate modifier pre and post functions into the function's body
            //

            // Add the toplevel function
            sway_definition.functions.push(sway_function.clone());

            if is_public {
                // Create the body for the contract impl's function wrapper
                sway_function.body = Some(sway::Block {
                    statements: vec![],
                    final_expr: Some(sway::Expression::FunctionCall(Box::new(sway::FunctionCall {
                        function: sway::Expression::Identifier(format!("::{}", sway_function.name)),
                        generic_parameters: None,
                        parameters: sway_function.parameters.entries.iter().map(|p| sway::Expression::Identifier(p.name.clone())).collect(),
                    }))),
                });

                // Add the function wrapper to the contract impl
                sway_definition.get_contract_impl().items.push(sway::ImplItem::Function(sway_function));
            }
        }

        println!("First translation pass of \"{}\" in \"{}\":", sway_definition.name, source_unit_path.to_string_lossy());
        println!("{sway_definition}");

        self.translated_definitions.push(sway_definition);
        
        Ok(())
    }

    fn translate_block(&mut self, source_unit_path: &Path, mut scope: TranslationScope, statements: &[solidity::Statement]) -> Result<sway::Block, Error> {
        let mut block = sway::Block::default();

        // Translate each of the statements in the block
        for statement in statements {
            // Translate the statement
            let sway_statement = self.translate_statement(source_unit_path, &mut scope, statement)?;

            // Store the index of the sway statement
            let statement_index = block.statements.len();

            // Add the sway statement to the sway block
            block.statements.push(sway_statement);

            // If the sway statement is a variable declaration, keep track of its statement index
            if let Some(sway::Statement::Let(sway_variable)) = block.statements.last() {
                let scope_entry = scope.variables.iter_mut().find(|v| v.new_name == sway_variable.name).unwrap();
                scope_entry.statement_index = Some(statement_index);
            }
        }

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

                let_statement.is_mutable = true;
            }
        }

        Ok(block)
    }

    fn translate_statement(&mut self, source_unit_path: &Path, scope: &mut TranslationScope, statement: &solidity::Statement) -> Result<sway::Statement, Error> {
        match statement {
            solidity::Statement::Block { statements, .. } => {
                let scope = TranslationScope {
                    parent: Some(Box::new(scope.clone())),
                    variables: vec![],
                };

                Ok(sway::Statement::Expression(sway::Expression::Block(Box::new(
                    self.translate_block(source_unit_path, scope, statements)?
                ))))
            }

            solidity::Statement::Assembly { loc, dialect, flags, block } => Ok(sway::Statement::Expression(sway::Expression::create_todo(Some(format!("translate assembly blocks"))))),
            solidity::Statement::Args(_, _) => Ok(sway::Statement::Expression(sway::Expression::create_todo(Some(format!("translate args statement: {statement:?}"))))),

            solidity::Statement::If(_, condition, then_body, else_if) => {
                let condition = self.translate_expression(source_unit_path, scope, condition)?;
                
                let then_body = match self.translate_statement(source_unit_path, scope, then_body.as_ref())? {
                    sway::Statement::Expression(sway::Expression::Block(block)) => *block,
                    
                    statement => sway::Block {
                        statements: vec![statement],
                        final_expr: None,
                    }
                };

                let else_if = if let Some(else_if) = else_if.as_ref() {
                    match self.translate_statement(source_unit_path, scope, else_if.as_ref())? {
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

                Ok(sway::Statement::Expression(sway::Expression::If(Box::new(sway::If {
                    condition: Some(condition),
                    then_body,
                    else_if,
                }))))
            }

            solidity::Statement::While(_, condition, body) => {
                Ok(sway::Statement::Expression(sway::Expression::While(Box::new(sway::While {
                    condition: self.translate_expression(source_unit_path, scope, condition)?,
                    body: match self.translate_statement(source_unit_path, scope, body)? {
                        sway::Statement::Expression(sway::Expression::Block(block)) => *block,
                        statement => sway::Block {
                            statements: vec![statement],
                            final_expr: None,
                        }
                    },
                }))))
            }
            
            solidity::Statement::Expression(_, x) => {
                Ok(sway::Statement::Expression(
                    self.translate_expression(source_unit_path, scope, x)?
                ))
            }

            solidity::Statement::VariableDefinition(_, variable_declaration, initializer) => {
                let old_name = variable_declaration.name.as_ref().unwrap().name.clone();
                let new_name = self.translate_naming_convention(old_name.as_str(), Case::Snake);
                let type_name = self.translate_type_name(source_unit_path, &variable_declaration.ty);

                let statement = sway::Statement::Let(sway::Let {
                    is_mutable: false,
                    name: new_name.clone(),
                    type_name: Some(type_name.clone()),
                    value: if let Some(x) = initializer.as_ref() {
                        Some(self.translate_expression(source_unit_path, scope, x)?)
                    } else {
                        None
                    },
                });

                scope.variables.push(TranslatedVariable {
                    old_name,
                    new_name,
                    type_name,
                    is_storage: false,
                    statement_index: None,
                    mutation_count: 0,
                });

                Ok(statement)
            }

            solidity::Statement::For(_, initialization, condition, update, body) => {
                // {
                //     initialization;
                //     while condition {
                //         body;
                //         update;
                //     }                    
                // }

                let mut statements = vec![];

                if let Some(initialization) = initialization.as_ref() {
                    statements.push(
                        self.translate_statement(source_unit_path, scope, initialization.as_ref())?
                    );
                }

                let condition = if let Some(condition) = condition.as_ref() {
                    self.translate_expression(source_unit_path, scope, condition.as_ref())?
                } else {
                    sway::Expression::Literal(sway::Literal::Bool(true))
                };

                let mut body = match body.as_ref() {
                    None => sway::Block::default(),
                    Some(body) => match self.translate_statement(source_unit_path, scope, body.as_ref())? {
                        sway::Statement::Expression(sway::Expression::Block(block)) => *block,
                        statement => sway::Block {
                            statements: vec![statement],
                            final_expr: None,
                        }
                    }
                };

                if let Some(update) = update.as_ref() {
                    body.statements.push(sway::Statement::Expression(
                        self.translate_expression(source_unit_path, scope, update.as_ref())?
                    ));
                }

                statements.push(
                    sway::Statement::Expression(sway::Expression::While(Box::new(sway::While {
                        condition,
                        body,
                    })))
                );

                Ok(sway::Statement::Expression(sway::Expression::Block(Box::new(sway::Block {
                    statements,
                    final_expr: None,
                }))))
            }

            solidity::Statement::DoWhile(_, body, condition) => Ok(sway::Statement::Expression(sway::Expression::create_todo(Some(format!("translate do while statements"))))),
            
            solidity::Statement::Continue(_) => {
                Ok(sway::Statement::Expression(sway::Expression::Continue))
            }

            solidity::Statement::Break(_) => {
                Ok(sway::Statement::Expression(sway::Expression::Break))
            }

            solidity::Statement::Return(_, x) => {
                Ok(sway::Statement::Expression(sway::Expression::Return(
                    if let Some(x) = x.as_ref() {
                        Some(Box::new(
                            self.translate_expression(source_unit_path, scope, x)?
                        ))
                    } else {
                        None
                    }
                )))
            }
            
            solidity::Statement::Revert(_, _, _) => Ok(sway::Statement::Expression(sway::Expression::create_todo(Some(format!("translate revert statements: {statement:?}"))))),
            solidity::Statement::RevertNamedArgs(_, _, _) => Ok(sway::Statement::Expression(sway::Expression::create_todo(Some(format!("translate revert named args statements"))))),
            solidity::Statement::Emit(_, _) => Ok(sway::Statement::Expression(sway::Expression::create_todo(Some(format!("translate emit statements: {statement:?}"))))),
            solidity::Statement::Try(_, _, _, _) => Ok(sway::Statement::Expression(sway::Expression::create_todo(Some(format!("translate try statements"))))),

            solidity::Statement::Error(_) => panic!("Encountered a statement that was not parsed correctly"),
        }
    }

    fn translate_expression(&mut self, source_unit_path: &Path, scope: &mut TranslationScope, expression: &solidity::Expression) -> Result<sway::Expression, Error> {
        match expression {
            solidity::Expression::PostIncrement(_, x) => {
                let x = self.translate_expression(source_unit_path, scope, x)?;

                // { x += 1; x }
                Ok(sway::Expression::Block(Box::new(sway::Block {
                    statements: vec![
                        sway::Statement::Expression(sway::Expression::BinaryExpression(Box::new(sway::BinaryExpression {
                            operator: "+=".into(),
                            lhs: x.clone(),
                            rhs: sway::Expression::Literal(sway::Literal::DecInt(1)),
                        }))),
                    ],
                    final_expr: Some(x),
                })))
            }

            solidity::Expression::PostDecrement(_, x) => {
                let x = self.translate_expression(source_unit_path, scope, x)?;

                // { x -= 1; x }
                Ok(sway::Expression::Block(Box::new(sway::Block {
                    statements: vec![
                        sway::Statement::Expression(sway::Expression::BinaryExpression(Box::new(sway::BinaryExpression {
                            operator: "-=".into(),
                            lhs: x.clone(),
                            rhs: sway::Expression::Literal(sway::Literal::DecInt(1)),
                        }))),
                    ],
                    final_expr: Some(x),
                })))
            }

            solidity::Expression::New(_, _) => Ok(sway::Expression::create_todo(Some(format!("translate new expression: {expression:?}")))),
            solidity::Expression::ArraySubscript(_, _, _) => Ok(sway::Expression::create_todo(Some(format!("translate array subscript expression: {expression:?}")))),
            solidity::Expression::ArraySlice(_, _, _, _) => Ok(sway::Expression::create_todo(Some(format!("translate array slice expression: {expression:?}")))),
            
            solidity::Expression::Parenthesis(_, x) => {
                // (x)
                Ok(sway::Expression::Tuple(vec![
                    self.translate_expression(source_unit_path, scope, x.as_ref())?,
                ]))
            }

            solidity::Expression::MemberAccess(_, x, member) => {
                if let solidity::Expression::Variable(solidity::Identifier { name, .. }) = x.as_ref() {
                    match (name.as_str(), member.name.as_str()) {
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

                        // TODO: find out the appropriate sway version of `block.number`
                        ("block", "number") => {
                            // todo!("block.number")
                            return Ok(sway::Expression::create_todo(Some("block.number".into())))
                        }

                        // TODO: find out the appropriate sway version of `block.prevrandao`
                        ("block", "prevrandao") => {
                            // todo!("block.prevrandao")
                            return Ok(sway::Expression::create_todo(Some("block.prevrandao".into())))
                        }

                        // TODO: find out the appropriate sway version of `block.timestamp`
                        ("block", "timestamp") => {
                            // todo!("block.timestamp")
                            return Ok(sway::Expression::create_todo(Some("block.timestamp".into())))
                        }

                        // TODO: find out the appropriate sway version of `msg.data`
                        ("msg", "data") => {
                            // todo!("msg.data")
                            return Ok(sway::Expression::create_todo(Some("msg.data".into())))
                        }

                        ("msg", "sender") => {
                            // msg_sender().unwrap()
                            return Ok(sway::Expression::FunctionCall(Box::new(sway::FunctionCall {
                                function: sway::Expression::MemberAccess(Box::new(sway::MemberAccess {
                                    expression: sway::Expression::FunctionCall(Box::new(sway::FunctionCall {
                                        function: sway::Expression::Identifier("msg_sender".into()),
                                        generic_parameters: None,
                                        parameters: vec![],
                                    })),
                                    member: "unwrap".into(),
                                })),
                                generic_parameters: None,
                                parameters: vec![],
                            })))
                        }

                        // TODO: find out the appropriate sway version of `msg.sig`
                        ("msg", "sig") => {
                            // todo!("msg.sig")
                            return Ok(sway::Expression::create_todo(Some("msg.sig".into())))
                        }

                        // TODO: find out the appropriate sway version of `msg.value`
                        ("msg", "value") => {
                            // todo!("msg.value")
                            return Ok(sway::Expression::create_todo(Some("msg.value".into())))
                        }

                        // TODO: find out the appropriate sway version of `tx.gasprice`
                        ("tx", "gasprice") => {
                            // todo!("tx.gasprice")
                            return Ok(sway::Expression::create_todo(Some("tx.gasprice".into())))
                        }

                        // TODO: find out the appropriate sway version of `tx.origin`
                        ("tx", "origin") => {
                            // todo!("tx.origin")
                            return Ok(sway::Expression::create_todo(Some("tx.origin".into())))
                        }

                        _ => {}
                    }
                }

                Ok(sway::Expression::create_todo(Some(format!("translate member access expression: {expression:?}"))))
            }

            solidity::Expression::FunctionCall(_, _, _) => Ok(sway::Expression::create_todo(Some(format!("translate function call expression: {expression:?}")))),
            solidity::Expression::FunctionCallBlock(_, _, _) => Ok(sway::Expression::create_todo(Some(format!("translate function call block expression: {expression:?}")))),
            solidity::Expression::NamedFunctionCall(_, _, _) => Ok(sway::Expression::create_todo(Some(format!("translate named function call expression: {expression:?}")))),

            solidity::Expression::Not(_, x) => {
                // !x
                Ok(sway::Expression::UnaryExpression(Box::new(sway::UnaryExpression {
                    operator: "!".into(),
                    expression: self.translate_expression(source_unit_path, scope, x)?,
                })))
            }

            solidity::Expression::BitwiseNot(_, x) => {
                // TODO: rust uses the ! operator instead of ~, I believe sway does too, but we should verify this
                // !x
                Ok(sway::Expression::UnaryExpression(Box::new(sway::UnaryExpression {
                    operator: "!".into(),
                    expression: self.translate_expression(source_unit_path, scope, x)?,
                })))
            }

            solidity::Expression::Delete(_, _) => Ok(sway::Expression::create_todo(Some(format!("translate delete expression: {expression:?}")))),
            solidity::Expression::PreIncrement(_, _) => Ok(sway::Expression::create_todo(Some(format!("translate pre increment expression: {expression:?}")))),
            solidity::Expression::PreDecrement(_, _) => Ok(sway::Expression::create_todo(Some(format!("translate pre decrement expression: {expression:?}")))),

            solidity::Expression::UnaryPlus(_, x) => {
                // x
                self.translate_expression(source_unit_path, scope, x)
            }

            solidity::Expression::Negate(_, x) => {
                // -x
                Ok(sway::Expression::UnaryExpression(Box::new(sway::UnaryExpression {
                    operator: "-".into(),
                    expression: self.translate_expression(source_unit_path, scope, x)?,
                })))
            }

            solidity::Expression::Power(_, _, _) => Ok(sway::Expression::create_todo(Some(format!("translate power expression: {expression:?}")))),

            solidity::Expression::Multiply(_, lhs, rhs) => {
                // lhs * rhs
                Ok(sway::Expression::BinaryExpression(Box::new(sway::BinaryExpression {
                    operator: "*".into(),
                    lhs: self.translate_expression(source_unit_path, scope, lhs)?,
                    rhs: self.translate_expression(source_unit_path, scope, rhs)?,
                })))
            }

            solidity::Expression::Divide(_, lhs, rhs) => {
                // lhs / rhs
                Ok(sway::Expression::BinaryExpression(Box::new(sway::BinaryExpression {
                    operator: "/".into(),
                    lhs: self.translate_expression(source_unit_path, scope, lhs)?,
                    rhs: self.translate_expression(source_unit_path, scope, rhs)?,
                })))
            }

            solidity::Expression::Modulo(_, lhs, rhs) => {
                // lhs % rhs
                Ok(sway::Expression::BinaryExpression(Box::new(sway::BinaryExpression {
                    operator: "%".into(),
                    lhs: self.translate_expression(source_unit_path, scope, lhs)?,
                    rhs: self.translate_expression(source_unit_path, scope, rhs)?,
                })))
            }

            solidity::Expression::Add(_, lhs, rhs) => {
                // lhs + rhs
                Ok(sway::Expression::BinaryExpression(Box::new(sway::BinaryExpression {
                    operator: "+".into(),
                    lhs: self.translate_expression(source_unit_path, scope, lhs)?,
                    rhs: self.translate_expression(source_unit_path, scope, rhs)?,
                })))
            }

            solidity::Expression::Subtract(_, lhs, rhs) => {
                // lhs - rhs
                Ok(sway::Expression::BinaryExpression(Box::new(sway::BinaryExpression {
                    operator: "-".into(),
                    lhs: self.translate_expression(source_unit_path, scope, lhs)?,
                    rhs: self.translate_expression(source_unit_path, scope, rhs)?,
                })))
            }

            solidity::Expression::ShiftLeft(_, lhs, rhs) => {
                // lhs << rhs
                Ok(sway::Expression::BinaryExpression(Box::new(sway::BinaryExpression {
                    operator: "<<".into(),
                    lhs: self.translate_expression(source_unit_path, scope, lhs)?,
                    rhs: self.translate_expression(source_unit_path, scope, rhs)?,
                })))
            }

            solidity::Expression::ShiftRight(_, lhs, rhs) => {
                // lhs >> rhs
                Ok(sway::Expression::BinaryExpression(Box::new(sway::BinaryExpression {
                    operator: ">>".into(),
                    lhs: self.translate_expression(source_unit_path, scope, lhs)?,
                    rhs: self.translate_expression(source_unit_path, scope, rhs)?,
                })))
            }

            solidity::Expression::BitwiseAnd(_, lhs, rhs) => {
                // lhs & rhs
                Ok(sway::Expression::BinaryExpression(Box::new(sway::BinaryExpression {
                    operator: "&".into(),
                    lhs: self.translate_expression(source_unit_path, scope, lhs)?,
                    rhs: self.translate_expression(source_unit_path, scope, rhs)?,
                })))
            }

            solidity::Expression::BitwiseXor(_, lhs, rhs) => {
                // lhs ^ rhs
                Ok(sway::Expression::BinaryExpression(Box::new(sway::BinaryExpression {
                    operator: "^".into(),
                    lhs: self.translate_expression(source_unit_path, scope, lhs)?,
                    rhs: self.translate_expression(source_unit_path, scope, rhs)?,
                })))
            }

            solidity::Expression::BitwiseOr(_, lhs, rhs) => {
                // lhs | rhs
                Ok(sway::Expression::BinaryExpression(Box::new(sway::BinaryExpression {
                    operator: "|".into(),
                    lhs: self.translate_expression(source_unit_path, scope, lhs)?,
                    rhs: self.translate_expression(source_unit_path, scope, rhs)?,
                })))
            }

            solidity::Expression::Less(_, lhs, rhs) => {
                // lhs < rhs
                Ok(sway::Expression::BinaryExpression(Box::new(sway::BinaryExpression {
                    operator: "<".into(),
                    lhs: self.translate_expression(source_unit_path, scope, lhs)?,
                    rhs: self.translate_expression(source_unit_path, scope, rhs)?,
                })))
            }

            solidity::Expression::More(_, lhs, rhs) => {
                // lhs > rhs
                Ok(sway::Expression::BinaryExpression(Box::new(sway::BinaryExpression {
                    operator: ">".into(),
                    lhs: self.translate_expression(source_unit_path, scope, lhs)?,
                    rhs: self.translate_expression(source_unit_path, scope, rhs)?,
                })))
            }

            solidity::Expression::LessEqual(_, lhs, rhs) => {
                // lhs <= rhs
                Ok(sway::Expression::BinaryExpression(Box::new(sway::BinaryExpression {
                    operator: "<=".into(),
                    lhs: self.translate_expression(source_unit_path, scope, lhs)?,
                    rhs: self.translate_expression(source_unit_path, scope, rhs)?,
                })))
            }

            solidity::Expression::MoreEqual(_, lhs, rhs) => {
                // lhs >= rhs
                Ok(sway::Expression::BinaryExpression(Box::new(sway::BinaryExpression {
                    operator: ">=".into(),
                    lhs: self.translate_expression(source_unit_path, scope, lhs)?,
                    rhs: self.translate_expression(source_unit_path, scope, rhs)?,
                })))
            }

            solidity::Expression::Equal(_, lhs, rhs) => {
                // lhs == rhs
                Ok(sway::Expression::BinaryExpression(Box::new(sway::BinaryExpression {
                    operator: "==".into(),
                    lhs: self.translate_expression(source_unit_path, scope, lhs)?,
                    rhs: self.translate_expression(source_unit_path, scope, rhs)?,
                })))
            }

            solidity::Expression::NotEqual(_, lhs, rhs) => {
                // lhs != rhs
                Ok(sway::Expression::BinaryExpression(Box::new(sway::BinaryExpression {
                    operator: "!=".into(),
                    lhs: self.translate_expression(source_unit_path, scope, lhs)?,
                    rhs: self.translate_expression(source_unit_path, scope, rhs)?,
                })))
            }

            solidity::Expression::And(_, lhs, rhs) => {
                // lhs && rhs
                Ok(sway::Expression::BinaryExpression(Box::new(sway::BinaryExpression {
                    operator: "&&".into(),
                    lhs: self.translate_expression(source_unit_path, scope, lhs)?,
                    rhs: self.translate_expression(source_unit_path, scope, rhs)?,
                })))
            }

            solidity::Expression::Or(_, lhs, rhs) => {
                // lhs || rhs
                Ok(sway::Expression::BinaryExpression(Box::new(sway::BinaryExpression {
                    operator: "||".into(),
                    lhs: self.translate_expression(source_unit_path, scope, lhs)?,
                    rhs: self.translate_expression(source_unit_path, scope, rhs)?,
                })))
            }

            solidity::Expression::ConditionalOperator(_, condition, then_value, else_value) => {
                // if condition { then_value } else { else_value }
                Ok(sway::Expression::If(Box::new(sway::If {
                    condition: Some(self.translate_expression(source_unit_path, scope, condition.as_ref())?),
                    then_body: sway::Block {
                        statements: vec![],
                        final_expr: Some(
                            self.translate_expression(source_unit_path, scope, then_value.as_ref())?
                        ),
                    },
                    else_if: Some(Box::new(sway::If {
                        condition: None,
                        then_body: sway::Block {
                            statements: vec![],
                            final_expr: Some(
                                self.translate_expression(source_unit_path, scope, else_value.as_ref())?
                            ),
                        },
                        else_if: None,
                    })),
                })))
            }
            
            solidity::Expression::Assign(_, lhs, rhs) => {
                //
                // TODO:
                // Look up `lhs` to see if it is a regular variable or a storage variable
                // Check if lhs is a regular identifier or a compound expression (mix of array access and member access, etc)
                // Find root variable of lhs and mark it as mutated
                //

                match lhs.as_ref() {
                    solidity::Expression::Variable(solidity::Identifier { name, .. }) => {
                        let Some(variable) = scope.find_variable_mut(name.as_str()) else {
                            panic!("Failed to find variable in scope: {name}")
                        };

                        // Create a storage write expression
                        if variable.is_storage {
                            // TODO: special handling for StorageMap and StorageVec
                            return Ok(sway::Expression::FunctionCall(Box::new(sway::FunctionCall {
                                function: sway::Expression::MemberAccess(Box::new(sway::MemberAccess {
                                    expression: sway::Expression::MemberAccess(Box::new(sway::MemberAccess {
                                        expression: sway::Expression::Identifier("storage".into()),
                                        member: variable.new_name.clone(),
                                    })),
                                    member: "write".into(),
                                })),
                                generic_parameters: None,
                                parameters: vec![
                                    self.translate_expression(source_unit_path, scope, rhs.as_ref())?,
                                ],
                            })));
                        }

                        // Increment the variable's mutation count
                        variable.mutation_count += 1;

                        // Create a regular assignment expression
                        return Ok(sway::Expression::BinaryExpression(Box::new(sway::BinaryExpression {
                            operator: "=".into(),
                            lhs: self.translate_expression(source_unit_path, scope, lhs.as_ref())?,
                            rhs: self.translate_expression(source_unit_path, scope, rhs.as_ref())?,
                        })));
                    }

                    _ => {}
                }

                Ok(sway::Expression::create_todo(Some(format!("handle assignment to lhs: {lhs:?}"))))
            }

            solidity::Expression::AssignOr(_, _, _) => Ok(sway::Expression::create_todo(Some(format!("translate assign or expression: {expression:?}")))),
            solidity::Expression::AssignAnd(_, _, _) => Ok(sway::Expression::create_todo(Some(format!("translate assign and expression: {expression:?}")))),
            solidity::Expression::AssignXor(_, _, _) => Ok(sway::Expression::create_todo(Some(format!("translate assign xor expression: {expression:?}")))),
            solidity::Expression::AssignShiftLeft(_, _, _) => Ok(sway::Expression::create_todo(Some(format!("translate assign shift left expression: {expression:?}")))),
            solidity::Expression::AssignShiftRight(_, _, _) => Ok(sway::Expression::create_todo(Some(format!("translate assign shift right expression: {expression:?}")))),
            solidity::Expression::AssignAdd(_, _, _) => Ok(sway::Expression::create_todo(Some(format!("translate assign add expression: {expression:?}")))),
            solidity::Expression::AssignSubtract(_, _, _) => Ok(sway::Expression::create_todo(Some(format!("translate assign subtract expression: {expression:?}")))),
            solidity::Expression::AssignMultiply(_, _, _) => Ok(sway::Expression::create_todo(Some(format!("translate assign multiply expression: {expression:?}")))),
            solidity::Expression::AssignDivide(_, _, _) => Ok(sway::Expression::create_todo(Some(format!("translate assign divide expression: {expression:?}")))),
            solidity::Expression::AssignModulo(_, _, _) => Ok(sway::Expression::create_todo(Some(format!("translate assign modulo expression: {expression:?}")))),
            
            solidity::Expression::BoolLiteral(_, value) => {
                Ok(sway::Expression::Literal(sway::Literal::Bool(*value)))
            }
            
            solidity::Expression::NumberLiteral(_, value, _, _) => {
                Ok(sway::Expression::Literal(sway::Literal::DecInt(value.parse().unwrap())))
            }

            solidity::Expression::RationalNumberLiteral(_, _, _, _, _) => Ok(sway::Expression::create_todo(Some(format!("translate rational number literal expression: {expression:?}")))),

            solidity::Expression::HexNumberLiteral(_, value, _) => {
                Ok(sway::Expression::Literal(sway::Literal::HexInt(
                    u64::from_str_radix(value.as_str(), 16)
                        .map_err(|e| Error::Wrapped(Box::new(e)))?
                )))
            }

            solidity::Expression::StringLiteral(value) => {
                Ok(sway::Expression::Literal(sway::Literal::String(
                    value.iter().map(|s| s.string.clone()).collect::<Vec<_>>().join("")
                )))
            }
            
            solidity::Expression::Type(_, _) => Ok(sway::Expression::create_todo(Some(format!("translate type expression: {expression:?}")))),
            solidity::Expression::HexLiteral(_) => Ok(sway::Expression::create_todo(Some(format!("translate hex literal expression: {expression:?}")))),
            solidity::Expression::AddressLiteral(_, _) => Ok(sway::Expression::create_todo(Some(format!("translate address literal expression: {expression:?}")))),
            
            solidity::Expression::Variable(variable) => {
                if let Some(variable) = scope.find_variable(variable.name.as_str()) {
                    return Ok(sway::Expression::Identifier(variable.new_name.clone()));
                }

                todo!("find variable in storage block: {}", variable.name)
            }

            solidity::Expression::List(_, _) => Ok(sway::Expression::create_todo(Some(format!("translate list expression: {expression:?}")))),

            solidity::Expression::ArrayLiteral(_, xs) => {
                let mut elements = vec![];

                for x in xs.iter() {
                    elements.push(self.translate_expression(source_unit_path, scope, x)?);
                }

                Ok(sway::Expression::Array(sway::Array {
                    elements,
                }))
            }
        }
    }
}
