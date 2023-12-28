use crate::{
    errors::Error,
    sway::{self, GenericParameterList},
    Options,
};
use convert_case::{Case, Casing};
use solang_parser::pt::{
    ContractDefinition, ContractPart, ContractTy, FunctionAttribute, FunctionTy, Import,
    ImportPath, SourceUnit, SourceUnitPart, VariableAttribute, Visibility,
};
use std::{
    cell::RefCell,
    collections::HashMap,
    path::{Path, PathBuf},
    rc::Rc,
};

pub struct TranslatedDefinition {
    /// The path to the file that the original definition is located in.
    pub path: PathBuf,

    /// The data of the translated definition.
    pub data: TranslatedDefinitionData,
}

pub struct TranslatedIdentifier {
    pub old: String,
    pub new: String,
}

pub enum TranslatedDefinitionData {
    Contract {
        is_abstract: bool,
        name: String,
        inherits: Vec<(String, PathBuf)>,
        functions: Vec<sway::Function>,
    }
}

pub struct TranslatedFunction {
    pub name: String,
}

#[derive(Default)]
pub struct Project {
    line_ranges: HashMap<PathBuf, Vec<(usize, usize)>>,
    solidity_source_units: Rc<RefCell<HashMap<PathBuf, SourceUnit>>>,
    sway_modules: HashMap<String, sway::Module>,
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

    fn create_conversion_queue(&self) -> Result<Vec<PathBuf>, Error> {
        let mut conversion_queue: Vec<PathBuf> = vec![];

        // Create conversion queue from import directives
        for (source_unit_path, source_unit) in self.solidity_source_units.borrow().iter() {
            let source_unit_directory = source_unit_path.parent().unwrap();

            let mut queue_import_path = |import_path: &ImportPath| -> Result<(), Error> {
                match import_path {
                    ImportPath::Filename(filename) => {
                        // Get the canonical path of the imported source unit
                        let import_path = source_unit_directory.join(filename.string.clone()).canonicalize().map_err(|e| Error::Wrapped(Box::new(e)))?;
                        
                        // If a source unit is already queued, move it to the top of the queue
                        if let Some((index, _)) = conversion_queue.iter().enumerate().find(|(_, p)| import_path.to_string_lossy() == p.to_string_lossy()) {
                            conversion_queue.remove(index);
                            conversion_queue.insert(0, import_path);
                        }
                        // If a source unit is not queued, add it to the end of the queue
                        else {
                            conversion_queue.push(import_path);
                        }
                    }

                    ImportPath::Path(path) => todo!("Experimental solidity import path: {path}"),
                }

                Ok(())
            };
            
            for source_unit_part in source_unit.0.iter() {
                // Only check import directives
                let SourceUnitPart::ImportDirective(import_directive) = source_unit_part else { continue };

                // Queue the imported source unit for conversion
                match import_directive {
                    Import::Plain(import_path, _) => queue_import_path(import_path)?,
                    Import::GlobalSymbol(import_path, _, _) => queue_import_path(import_path)?,
                    Import::Rename(import_path, _, _) => queue_import_path(import_path)?,
                }
            }
        }

        Ok(conversion_queue)
    }

    pub fn translate(&mut self) -> Result<(), Error> {
        let solidity_source_units = self.solidity_source_units.clone();
        let conversion_queue = self.create_conversion_queue()?;

        // Translate source units through conversion queue
        for source_unit_path in conversion_queue.iter() {
            // Parse the source unit if it has not been parsed already
            if !self.solidity_source_units.borrow().contains_key(source_unit_path) {
                println!("Parsing \"{}\"", source_unit_path.to_string_lossy());
                self.parse_solidity_source_unit(source_unit_path)?;
            }

            // Get the parsed source unit
            let source_unit = solidity_source_units.borrow().get(source_unit_path).unwrap().clone();
            println!("Translating \"{}\"...", source_unit_path.to_string_lossy());

            // Handle the first translation pass
            for source_unit_part in source_unit.0.iter() {
                match source_unit_part {
                    SourceUnitPart::PragmaDirective(_, _, _) => {
                        // TODO: check if any are actually important
                    }
        
                    SourceUnitPart::ImportDirective(_) => {
                        // TODO: import and translate the file
                    }
        
                    SourceUnitPart::ContractDefinition(contract_definition) => {
                        self.translate_contract_definition(contract_definition)?;
                    }
        
                    SourceUnitPart::EnumDefinition(_) => {
                        // TODO: create the enum
                    }
        
                    SourceUnitPart::StructDefinition(_) => {
                        // TODO: create the struct
                    }
        
                    SourceUnitPart::EventDefinition(_) => {
                        // TODO: determine how to handle custom events
                    }
        
                    SourceUnitPart::ErrorDefinition(_) => {
                        // TODO: determine how to handle custom errors
                    }
        
                    SourceUnitPart::FunctionDefinition(_) => {
                        // TODO: translate the function
                    }
        
                    SourceUnitPart::VariableDefinition(_) => {
                        // TODO: is this actually used?
                    }
        
                    SourceUnitPart::TypeDefinition(_) => {
                        // TODO: create the type definition (?)
                    }
        
                    SourceUnitPart::Annotation(_) => {
                        // TODO: is this actually used?
                    }
        
                    SourceUnitPart::Using(_) => {
                        // TODO: determine if this needs special handling
                    }
        
                    SourceUnitPart::StraySemicolon(_) => {}
                }
            }

            println!("Done.");
        }

        Ok(())
    }

    fn translate_contract_definition(&mut self, contract_definition: &ContractDefinition) -> Result<(), Error> {
        let mut module = sway::Module::new(match &contract_definition.ty {
            ContractTy::Abstract(_) => todo!("Determine how to handle abstract contract generation"),
            ContractTy::Contract(_) => sway::ModuleKind::Contract,
            ContractTy::Interface(_) => todo!("Create ABI for interface"),
            ContractTy::Library(_) => sway::ModuleKind::Library,
        });

        let contract_name = contract_definition.name.as_ref().unwrap().name.clone();

        fn canonicalize_type_name(type_name: &str) -> sway::TypeName {
            //
            // TODO: check mapping for previously canonicalized user type names?
            //

            sway::TypeName {
                name: match type_name {
                    "uint" | "uint256" => "u64".into(),
                    "address" | "address payable" => "Address".into(),
                    _ => type_name.into(),
                },
                generic_parameters: GenericParameterList::default(),
            }
        }

        for part in contract_definition.parts.iter() {
            match part {
                ContractPart::StructDefinition(struct_definition) => {
                    let mut struct_item = sway::Struct {
                        is_public: true,
                        name: struct_definition.name.as_ref().unwrap().name.clone(),
                        generic_parameters: GenericParameterList::default(),
                        fields: vec![],
                    };

                    for field in struct_definition.fields.iter() {
                        //
                        // TODO:
                        // * make note of original name vs snake case name
                        // * generate canonicalized type name
                        // * make note of original type vs canonicalized type
                        //

                        struct_item.fields.push(sway::StructField {
                            is_public: true,
                            name: field.name.as_ref().unwrap().name.to_case(Case::Snake),
                            type_name: canonicalize_type_name(field.ty.to_string().as_str()),
                        });
                    }

                    module.items.push(sway::ModuleItem::Struct(struct_item));
                }

                ContractPart::EventDefinition(_) => {
                    // TODO: track the event type in order to create proper `log` calls
                }

                ContractPart::EnumDefinition(_) => {
                    // TODO: determine the best way to handle the conversion, since solidity and sway enums are different from each other
                }

                ContractPart::ErrorDefinition(_) => {
                    // TODO: determine the best way to handle these
                }
                
                ContractPart::VariableDefinition(variable_definition) => {
                    //
                    // TODO:
                    // * make note of original name vs snake case name
                    // * generate canonicalized type name
                    // * make note of original type vs canonicalized type
                    // * create proper constructor expressions
                    // * generate getter functions for public variables
                    //

                    let is_public = variable_definition.attrs.iter().any(|x| matches!(x, VariableAttribute::Visibility(Visibility::External(_) | Visibility::Public(_))));

                    // Handle constant variable definitions
                    if variable_definition.attrs.iter().any(|x| matches!(x, VariableAttribute::Constant(_))) {
                        module.items.push(sway::ModuleItem::Constant(sway::Constant {
                            is_public,
                            name: variable_definition.name.as_ref().unwrap().name.to_case(Case::UpperSnake),
                            type_name: canonicalize_type_name(variable_definition.ty.to_string().as_str()),
    
                            // TODO: proper value constructors
                            value: Some(sway::Expression::FunctionCall(Box::new(sway::FunctionCall {
                                function: sway::Expression::Identifier("todo!".into()),
                                generic_parameters: None,
                                parameters: vec![],
                            }))),
                        }));
                    }
                    // Handle immutable variable definitions
                    else if variable_definition.attrs.iter().any(|x| matches!(x, VariableAttribute::Immutable(_))) {
                        todo!("Determine how to handle immutable variables (should it be a configurable?)")
                    }
                    // Handle all other variable definitions
                    else {
                        let storage = module.get_or_create_storage();
    
                        storage.fields.push(sway::StorageField {
                            name: variable_definition.name.as_ref().unwrap().name.to_case(Case::Snake),
                            type_name: canonicalize_type_name(variable_definition.ty.to_string().as_str()),
    
                            // TODO: proper value constructors
                            value: sway::Expression::FunctionCall(Box::new(sway::FunctionCall {
                                function: sway::Expression::Identifier("todo!".into()),
                                generic_parameters: None,
                                parameters: vec![],
                            })),
                        });
                    }
                }

                ContractPart::FunctionDefinition(function_definition) => {
                    //
                    // TODO:
                    // * differentiate between `constructor`, `function`, `fallback`, `receive`, `modifier`
                    // * make note of original name vs snake case name
                    // * translate function
                    // * determine if function reads from storage
                    // * determine if function writes to storage
                    // * determine if function is payable
                    //

                    let is_public = function_definition.attributes.iter().any(|x| matches!(x, FunctionAttribute::Visibility(Visibility::External(_) | Visibility::Public(_))));
                    let is_constructor = matches!(function_definition.ty, FunctionTy::Constructor);
                    let is_function = matches!(function_definition.ty, FunctionTy::Function);
                    let is_fallback = matches!(function_definition.ty, FunctionTy::Fallback);
                    let is_receive = matches!(function_definition.ty, FunctionTy::Receive);
                    let is_modifier = matches!(function_definition.ty, FunctionTy::Modifier);

                    if is_modifier {
                        //
                        // TODO:
                        // * translate the modifier code 
                        // * generate functions for modifier pre and post code
                        // * keep track of pre and post code functions for inserting into functions that use the modifier
                        //
                    } else if is_public || is_constructor {
                        let abi = module.get_or_create_abi(contract_name.as_str());
                        
                        let mut function = sway::Function {
                            is_public: false,
                            name: if is_constructor {
                                "constructor".into() // TODO: multiple constructors?
                            } else {
                                function_definition.name.as_ref().unwrap().name.to_case(Case::Snake)
                            },
                            generic_parameters: sway::GenericParameterList::default(),
                            parameters: sway::ParameterList {
                                entries: vec![
                                    // TODO
                                ],
                            },
                            return_type: None, // TODO
                            body: None,
                        };

                        // Create the function declaration in the contract's ABI
                        abi.functions.push(function.clone());

                        //
                        // TODO:
                        // * convert the function's body code
                        //

                        function.body = Some(sway::Block {
                            statements: vec![],
                            final_expr: Some(sway::Expression::FunctionCall(Box::new(sway::FunctionCall {
                                function: sway::Expression::Identifier("todo!".into()),
                                generic_parameters: None,
                                parameters: vec![],
                            }))),
                        });

                        // Add the function to its ABI impl block
                        let impl_for = module.get_or_create_impl_for(contract_name.as_str(), "Contract");
                        impl_for.items.push(sway::ImplItem::Function(function));
                    } else {
                        //
                        // TODO:
                        // * create toplevel function (?)
                        //
                    }
                }

                ContractPart::TypeDefinition(type_definition) => {
                    // TODO: check if this is OK
                    module.items.push(sway::ModuleItem::TypeDefinition(sway::TypeDefinition {
                        is_public: true,
                        name: sway::TypeName {
                            name: type_definition.name.to_string(),
                            generic_parameters: GenericParameterList::default(),
                        },
                        underlying_type: Some(canonicalize_type_name(type_definition.ty.to_string().as_str())),
                    }));
                }

                ContractPart::Annotation(_) => {}

                ContractPart::Using(_) => {
                    // TODO
                }

                ContractPart::StraySemicolon(_) => {}
            }
        }

        println!("{}", sway::TabbedDisplayer(&module));
        self.sway_modules.insert(contract_name, module);

        Ok(())
    }
}
