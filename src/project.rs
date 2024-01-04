use crate::{errors::Error, Options, sway};
use convert_case::{Case, Casing};
use solang_parser::pt as solidity;
use std::{
    cell::RefCell,
    collections::HashMap,
    path::{Path, PathBuf},
    rc::Rc,
};

pub struct TranslatedDefinition {
    /// The path to the file that the original definition is located in.
    pub path: PathBuf,

    pub kind: solidity::ContractTy,
    pub name: String,
    pub inherits: Vec<String>,
    pub type_definitions: Vec<sway::TypeDefinition>,
    pub structs: Vec<sway::Struct>,
    pub events_enum: Option<sway::Enum>,
    pub errors_enum: Option<sway::Enum>,
    pub abi: Option<sway::Abi>,
    pub storage: Option<sway::Storage>,
    pub functions: Vec<sway::Function>,
    pub impls: Vec<sway::Impl>,
}

impl TranslatedDefinition {
    /// Gets the events enum for the translated definition. If it doesn't exist, it gets created.
    pub fn get_events_enum(&mut self) -> &mut sway::Enum {
        if self.events_enum.is_none() {
            self.events_enum = Some(sway::Enum {
                attributes: None,
                is_public: false,
                name: format!("{}Event", self.name),
                generic_parameters: sway::GenericParameterList::default(),
                variants: vec![],
            });
        }

        self.events_enum.as_mut().unwrap()
    }

    /// Gets the errors enum for the translated definition. If it doesn't exist, it gets created.
    pub fn get_errors_enum(&mut self) -> &mut sway::Enum {
        if self.errors_enum.is_none() {
            self.errors_enum = Some(sway::Enum {
                attributes: None,
                is_public: false,
                name: format!("{}Error", self.name),
                generic_parameters: sway::GenericParameterList::default(),
                variants: vec![],
            });
        }

        self.errors_enum.as_mut().unwrap()
    }

    /// Gets the abi for the translated definition. If it doesn't exist, it gets created.
    pub fn get_abi(&mut self) -> &mut sway::Abi {
        if self.abi.is_none() {
            self.abi = Some(sway::Abi {
                name: self.name.clone(),
                inherits: self.inherits.clone(),
                functions: vec![],
            });
        }

        self.abi.as_mut().unwrap()
    }

    /// Gets the storage block for the translated definition. If it doesn't exist, it gets created.
    pub fn get_storage(&mut self) -> &mut sway::Storage {
        if self.storage.is_none() {
            self.storage = Some(sway::Storage {
                fields: vec![],
            });
        }

        self.storage.as_mut().unwrap()
    }

    /// Gets the translated definition's implementation for `Contract`. If it doesn't exist, it gets created.
    pub fn get_contract_impl(&mut self) -> &mut sway::Impl {
        let find_contract_impl = |i: &sway::Impl| {
            let sway::TypeName::Identifier { name: type_name, .. } = &i.type_name else { return false };
            let Some(sway::TypeName::Identifier { name: for_type_name, .. }) = i.for_type_name.as_ref() else { return false };
            *type_name == self.name && for_type_name == "Contract"
        };

        if !self.impls.iter_mut().any(|i| find_contract_impl(i)) {
            self.impls.push(sway::Impl {
                generic_parameters: sway::GenericParameterList::default(),
                type_name: sway::TypeName::Identifier {
                    name: self.name.clone(),
                    generic_parameters: sway::GenericParameterList::default(),
                },
                for_type_name: Some(sway::TypeName::Identifier {
                    name: "Contract".into(),
                    generic_parameters: sway::GenericParameterList::default(),
                }),
                items: vec![],
            });
        }

        self.impls.iter_mut().find(|i| find_contract_impl(*i)).unwrap()
    }
}

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
                    generic_parameters: sway::GenericParameterList::default(),
                },

                // TODO: should we note that this address was marked payable?
                solidity::Type::AddressPayable => sway::TypeName::Identifier {
                    name: "Identity".into(),
                    generic_parameters: sway::GenericParameterList::default(),
                },

                solidity::Type::Payable => todo!("payable types (used for casting)"),
                
                solidity::Type::Bool => sway::TypeName::Identifier {
                    name: "bool".into(),
                    generic_parameters: sway::GenericParameterList::default(),
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
                    generic_parameters: sway::GenericParameterList::default(),
                },

                solidity::Type::Bytes(length) => sway::TypeName::Array {
                    type_name: Box::new(sway::TypeName::Identifier {
                        name: "u8".into(),
                        generic_parameters: sway::GenericParameterList::default(),
                    }),
                    length: *length as usize,
                },

                solidity::Type::Rational => todo!("rational types"),

                solidity::Type::DynamicBytes => sway::TypeName::Identifier {
                    name: "std::bytes::Bytes".into(), // TODO: is this ok?
                    generic_parameters: sway::GenericParameterList::default(),
                },

                solidity::Type::Mapping { key, value, .. } => sway::TypeName::Identifier {
                    name: "StorageMap".into(),
                    generic_parameters: sway::GenericParameterList {
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
                    },
                },

                solidity::Type::Function { params, attributes, returns } => todo!("function types"),
            }

            _ => unimplemented!("type name expression: {type_name:#?}"),
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
            _ => panic!("Invalid literal expression: {value:#?}"),
        }
    }
    
    fn translate_contract_definition(
        &mut self,
        source_unit_path: &Path,
        solidity_definition: &solidity::ContractDefinition,
    ) -> Result<(), Error> {
        let definition_name = solidity_definition.name.as_ref().unwrap().name.clone();
        let inherits: Vec<String> = solidity_definition.base.iter().map(|b| b.name.identifiers.iter().map(|i| i.name.clone()).collect::<Vec<_>>().join(".")).collect();

        let mut sway_definition = TranslatedDefinition {
            path: source_unit_path.into(),
            kind: solidity_definition.ty.clone(),
            name: definition_name.clone(),
            inherits: inherits.clone(),
            type_definitions: vec![],
            structs: vec![],
            events_enum: None,
            errors_enum: None,
            abi: None,
            storage: None,
            functions: vec![],
            impls: vec![],
        };

        // Collect each type definition ahead of time for contextual reasons
        for part in solidity_definition.parts.iter() {
            let solidity::ContractPart::TypeDefinition(type_definition) = part else { continue };
            
            sway_definition.type_definitions.push(sway::TypeDefinition {
                is_public: true,
                name: sway::TypeName::Identifier {
                    name: type_definition.name.name.clone(),
                    generic_parameters: sway::GenericParameterList::default(),
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
                generic_parameters: sway::GenericParameterList::default(),
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

        // Collect each storage field ahead of time for contextual reasons
        for part in solidity_definition.parts.iter() {
            let solidity::ContractPart::VariableDefinition(variable_definition) = part else { continue };

            let variable_type_name = self.translate_type_name(source_unit_path, &variable_definition.ty);
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
            let variable_name = self.translate_naming_convention(variable_definition.name.as_ref().unwrap().name.as_str(), Case::Snake); // TODO: keep track of original name

            // Add the storage field to the storage block
            sway_definition.get_storage().fields.push(sway::StorageField {
                name: variable_name.clone(), // TODO: keep track of original name
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
                name: variable_name.clone(), // TODO: keep track of original name
                generic_parameters: sway::GenericParameterList::default(),
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
                            member: variable_name.clone(),
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
        
        // Resolve all using-for statements ahead of time for contextual reasons
        for part in solidity_definition.parts.iter() {
            let solidity::ContractPart::Using(using) = part else { continue };

            todo!("using-for statements")
        }

        //
        // TODO:
        // We may need to do a first pass scan to collect information about all available functions before
        // translating each of their bodies so that we know what functions are availble in the toplevel scope
        //

        // Translate each function
        for part in solidity_definition.parts.iter() {
            let solidity::ContractPart::FunctionDefinition(function_definition) = part else { continue };

            // Collect information about the function from its attributes
            let is_public = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Visibility(solidity::Visibility::External(_) | solidity::Visibility::Public(_))));
            let is_constructor = matches!(function_definition.ty, solidity::FunctionTy::Constructor);
            let is_function = matches!(function_definition.ty, solidity::FunctionTy::Function);
            let is_fallback = matches!(function_definition.ty, solidity::FunctionTy::Fallback);
            let is_receive = matches!(function_definition.ty, solidity::FunctionTy::Receive);
            let is_modifier = matches!(function_definition.ty, solidity::FunctionTy::Modifier);
            let is_constant = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Mutability(solidity::Mutability::Constant(_))));
            let is_pure = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Mutability(solidity::Mutability::Pure(_))));
            let is_view = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Mutability(solidity::Mutability::View(_))));
            let is_payable = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Mutability(solidity::Mutability::Payable(_))));

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
                generic_parameters: sway::GenericParameterList::default(),

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

            if function_definition.body.is_none() {
                continue;
            }

            // Create the body for the toplevel function
            sway_function.body = Some(sway::Block { // TODO: actually translate function body
                statements: vec![],
                final_expr: Some(sway::Expression::FunctionCall(Box::new(sway::FunctionCall {
                    function: sway::Expression::Identifier("todo!".into()),
                    generic_parameters: None,
                    parameters: vec![],
                }))),
            });

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

        for x in sway_definition.type_definitions.iter() {
            println!("{}", sway::TabbedDisplayer(x));
        }

        for x in sway_definition.structs.iter() {
            println!("{}", sway::TabbedDisplayer(x));
        }
    
        if let Some(x) = sway_definition.events_enum.as_ref() {
            println!("{}", sway::TabbedDisplayer(x));
        }

        if let Some(x) = sway_definition.errors_enum.as_ref() {
            println!("{}", sway::TabbedDisplayer(x));
        }
        
        if let Some(x) = sway_definition.abi.as_ref() {
            println!("{}", sway::TabbedDisplayer(x));
        }
        
        if let Some(x) = sway_definition.storage.as_ref() {
            println!("{}", sway::TabbedDisplayer(x));
        }
        
        for x in sway_definition.functions.iter() {
            println!("{}", sway::TabbedDisplayer(x));
        }
        
        for x in sway_definition.impls.iter() {
            println!("{}", sway::TabbedDisplayer(x));
        }

        self.translated_definitions.push(sway_definition);
        
        Ok(())
    }
}
