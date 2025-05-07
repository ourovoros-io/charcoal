use crate::{
    errors::Error,
    sway::{self, AttributeList}, translate::{contracts::{translate_contract_definition, translate_using_directive}, enums::{translate_enum_definition, translate_error_definition, translate_event_definition}, functions::{translate_function_declaration, translate_function_definition}, import_directives::translate_import_directives, storage::translate_storage_name, structs::translate_struct_definition, type_definitions::translate_type_definition, TranslatedDefinition},
};
use convert_case::Case;
use num_bigint::BigUint;
use num_traits::One;
use solang_parser::pt as solidity;
use std::{
    cell::RefCell,
    collections::HashMap,
    path::{Component, Path, PathBuf},
    rc::Rc,
};

/// Represents the type of project as [ProjectKind] default is [ProjectKind::Unknown]
#[derive(Clone, Debug, Default)]
pub enum ProjectKind {
    Foundry {
        remappings: HashMap<String, String>,
    },
    Hardhat,
    Brownie {
        remappings: HashMap<String, String>,
    },
    Truffle,
    Dapp,
    #[default]
    Unknown,
}

impl ProjectKind {
    pub const FOUNDRY_CONFIG_FILE: &'static str = "foundry.toml";
    pub const HARDHAT_CONFIG_FILE: &'static str = "hardhat.config.js";
    pub const HARDHAT_CONFIG_FILE_TS: &'static str = "hardhat.config.ts";
    pub const BROWNIE_CONFIG_FILE: &'static str = "brownie-config.yaml";
    pub const TRUFFLE_CONFIG_FILE: &'static str = "truffle-config.js";
    pub const DAPP_CONFIG_FILE: &'static str = "Dappfile";
}

#[derive(Default)]
pub struct Project {
    pub kind: ProjectKind,
    pub line_ranges: HashMap<PathBuf, Vec<(usize, usize)>>,
    pub solidity_source_units: Rc<RefCell<HashMap<PathBuf, solidity::SourceUnit>>>,
    pub translated_definitions: Vec<TranslatedDefinition>,
}

impl Project {
    /// Attempts to parse the file from the supplied `path`.
    #[inline]
    pub fn parse_solidity_source_unit<P: AsRef<Path>>(&mut self, path: P) -> Result<(), Error> {
        if !path.as_ref().exists() {
            return Err(Error::Wrapped(Box::new(
                std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!("File not found: {}", path.as_ref().to_string_lossy()),
                )
            )));
        }

        let path = crate::get_canonical_path(path, false, false)
            .map_err(|e| Error::Wrapped(Box::new(e)))?;
        
        let source = std::fs::read_to_string(path.clone())
            .map_err(|e| Error::Wrapped(Box::new(e)))?;
        
        self.load_line_ranges(&path, source.as_str());

        let line_ranges = self.line_ranges.get(&path).unwrap();

        let (source_unit, _comments) = solang_parser::parse(source.as_str(), 0)
            .map_err(|e| Error::SolangDiagnostics(path.clone(), line_ranges.clone(), e))?;

        // TODO: do we need the comments for anything?

        self.solidity_source_units.borrow_mut().insert(path, source_unit);

        Ok(())
    }

    /// Loads line ranges in a specific file `path` from the provided `source` text.
    #[inline]
    fn load_line_ranges(&mut self, path: &PathBuf, source: &str) {
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

    #[inline]
    pub fn loc_to_line_and_column<P: AsRef<Path>>(&self, path: P, loc: &solidity::Loc) -> Option<(usize, usize)> {
        let line_ranges = self.line_ranges.get(path.as_ref())?;

        let start = match loc {
            solidity::Loc::Builtin
            | solidity::Loc::CommandLine
            | solidity::Loc::Implicit
            | solidity::Loc::Codegen => return None,

            solidity::Loc::File(_, start, _) => start,
        };

        for (i, (line_start, line_end)) in line_ranges.iter().enumerate() {
            if start >= line_start && start < line_end {
                return Some((i + 1, (start - line_start) + 1));
            }
        }
        
        None
    }

    pub fn collect_translated_definitions<P: AsRef<Path>>(&self, definition_name: Option<&String>, source_unit_path: P) -> Vec<TranslatedDefinition> {
        let mut result = vec![];
        
        for translated_definition in self.translated_definitions.iter() {
            if let Some(definition_name) = definition_name {
                if translated_definition.name != *definition_name {
                    continue;
                }
            }

            if translated_definition.path == source_unit_path.as_ref() {
                result.push(translated_definition.clone());
            }
        }

        result
    }

    pub fn find_definition_with_abi(&self, name: &str) -> Option<&TranslatedDefinition> {
        if let Some(external_definition) = self.translated_definitions.iter().find(|d| d.name == name && d.abi.is_some()) {
            return Some(external_definition);
        }

        None
    }

    pub fn canonicalize_import_path(&self, source_unit_directory: &Path, path_string: &str) -> Result<PathBuf, Error> {
        let mut import_path = PathBuf::from(path_string);

        if !import_path.to_string_lossy().starts_with('.') {
            import_path = self.get_project_type_path(source_unit_directory, path_string)?;
        } else {
            import_path = source_unit_directory.join(import_path);
        }
        
        import_path = crate::get_canonical_path(import_path, false, false)
            .map_err(|e| Error::Wrapped(Box::new(e))).unwrap();
        
        if !import_path.exists() {
            return Err(Error::Wrapped(Box::new(
                std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!("File not found: {}", import_path.to_string_lossy()),
                )
            )));
        }

        Ok(import_path)
    }

    pub fn translate(&mut self, definition_name: Option<&String>, source_unit_path: &Path) -> Result<(), Error> {
        let solidity_source_units = self.solidity_source_units.clone();

        // Ensure the source unit has been parsed
        if !solidity_source_units.borrow().contains_key(source_unit_path) {
            self.parse_solidity_source_unit(source_unit_path)?;
        }
        
        // Get the parsed source unit
        let source_unit = solidity_source_units.borrow().get(source_unit_path).unwrap().clone();

        // Collect toplevel items ahead of time for contextual reasons
        let mut import_directives = vec![];
        let mut toplevel_using_directives = vec![];
        let mut toplevel_type_definitions = vec![];
        let mut toplevel_enums = vec![];
        let mut toplevel_structs = vec![];
        let mut toplevel_events = vec![];
        let mut toplevel_errors = vec![];
        let mut toplevel_functions = vec![];
        let mut toplevel_variables = vec![];
        let mut contract_names = vec![];

        for source_unit_part in source_unit.0.iter() {
            match source_unit_part {
                solidity::SourceUnitPart::PragmaDirective(_) => {
                    // NOTE: we don't need to do anything with pragma directives
                }

                solidity::SourceUnitPart::ImportDirective(import_directive) => {
                    import_directives.push(import_directive.clone());
                }

                solidity::SourceUnitPart::ContractDefinition(contract_definition) => {
                    contract_names.push(contract_definition.name.as_ref().unwrap().name.clone());
                }

                solidity::SourceUnitPart::EnumDefinition(enum_definition) => {
                    toplevel_enums.push(enum_definition.as_ref().clone());
                }

                solidity::SourceUnitPart::StructDefinition(struct_definition) => {
                    toplevel_structs.push(struct_definition.as_ref().clone());
                }

                solidity::SourceUnitPart::EventDefinition(event_definition) => {
                    toplevel_events.push(event_definition.as_ref().clone());
                }

                solidity::SourceUnitPart::ErrorDefinition(error_definition) => {
                    toplevel_errors.push(error_definition.as_ref().clone());
                }

                solidity::SourceUnitPart::FunctionDefinition(function_definition) => {
                    toplevel_functions.push(function_definition.as_ref().clone());
                }

                solidity::SourceUnitPart::VariableDefinition(variable_definition) => {
                    toplevel_variables.push(variable_definition.as_ref().clone());
                }

                solidity::SourceUnitPart::TypeDefinition(type_definition) => {
                    toplevel_type_definitions.push(type_definition.as_ref().clone());
                }

                solidity::SourceUnitPart::Annotation(_) => {
                    // NOTE: we don't need to do anything with annotations
                }
                
                solidity::SourceUnitPart::Using(using_directive) => {
                    toplevel_using_directives.push(using_directive.as_ref().clone());
                }

                solidity::SourceUnitPart::StraySemicolon(_) => {
                    // NOTE: we don't need to do anything with stray semicolons
                }
            }
        }

        //
        // TODO: Build the symbol list by flattening the imported symbols and listing everything in
        //       the source file (contracts, structs, enums, functions, etc)
        //

        let mut translated_definition = TranslatedDefinition {
            path: source_unit_path.into(),
            ..Default::default()
        };

        // Translate import directives
        translate_import_directives(self, &mut translated_definition, import_directives.as_slice())?;

        // Translate toplevel using directives
        for using_directive in toplevel_using_directives.iter() {
            translate_using_directive(self, &mut translated_definition, using_directive)?;
        }

        // Translate toplevel type definitions
        for type_definition in toplevel_type_definitions.iter() {
            translate_type_definition(self, &mut translated_definition, type_definition)?;
        }

        // Translate toplevel enum definitions
        for enum_definition in toplevel_enums.iter() {
            translate_enum_definition(self, &mut translated_definition, enum_definition)?;
        }

        // Collect toplevel struct names ahead of time for contextual reasons
        for struct_definition in toplevel_structs.iter() {
            let struct_name = struct_definition.name.as_ref().unwrap().name.clone();

            if !translated_definition.struct_names.contains(&struct_name) {
                translated_definition.struct_names.push(struct_name);
            }
        }

        // Translate toplevel struct definitions
        for struct_definition in toplevel_structs.iter() {
            translate_struct_definition(self, &mut translated_definition, struct_definition)?;
        }

        // Translate toplevel event definitions
        for event_definition in toplevel_events.iter() {
            translate_event_definition(self, &mut translated_definition, event_definition)?;
        }

        // Translate toplevel error definitions
        for error_definition in toplevel_errors.iter() {
            translate_error_definition(self, &mut translated_definition, error_definition)?;
        }

        // Collect each toplevel function ahead of time for contextual reasons
        for function_definition in toplevel_functions.iter() {

            let is_modifier = matches!(function_definition.ty, solidity::FunctionTy::Modifier);

            if is_modifier {
                continue;
            }

            // Add the toplevel function to the list of toplevel functions for the toplevel scope
            let function = translate_function_declaration(self, &mut translated_definition, function_definition)?;
            
            let sway::TypeName::Function { parameters: function_parameters, return_type: function_return_type, .. } = &function.type_name else {
                panic!("Invalid function type name: {:#?}", function.type_name)
            };
            
            let mut function_exists = false;
            
            for f in translated_definition.toplevel_scope.borrow().functions.iter() {
                let mut f = f.borrow_mut();

                let sway::TypeName::Function { parameters: f_parameters, return_type: f_return_type, .. } = &f.type_name else {
                    panic!("Invalid function type name: {:#?}", f.type_name)
                };
                
                if ((!f.old_name.is_empty() && (f.old_name == function.old_name)) || (f.new_name == function.new_name))
                    && f_parameters == function_parameters
                    && f_return_type == function_return_type
                {
                    f.new_name.clone_from(&function.new_name);
                    function_exists = true;
                    break;
                }
            }

            if !function_exists {
                translated_definition.toplevel_scope.borrow_mut().functions.push(Rc::new(RefCell::new(function)));
            }
        }

        // Translate toplevel function definitions
        for function_definition in toplevel_functions.iter() {
            translate_function_definition(self, &mut translated_definition, function_definition)?;
        }
        
        let finalize_mappings = |project: &mut Project, translated_definition: &mut TranslatedDefinition| {
            if translated_definition.mapping_names.is_empty() {
                return;
            }
            
            let mut constructor_body = translated_definition.get_constructor_fn().body.clone().unwrap();

            // Insert constructor call guard if necessary
            if constructor_body.statements.is_empty() {
                let prefix = crate::translate::translate_naming_convention(translated_definition.name.as_str(), Case::Snake);
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
                constructor_body.statements.insert(0, sway::Statement::from(sway::Expression::create_function_calls(None, &[
                    ("require", Some((None, vec![
                        sway::Expression::from(sway::UnaryExpression {
                            operator: "!".into(),
                            expression: sway::Expression::create_function_calls(None, &[
                                ("storage", None),
                                (constructor_called_variable_name.as_str(), None),
                                ("read", Some((None, vec![]))),
                            ])
                        }),
                        sway::Expression::from(sway::Literal::String(
                            format!("The {} constructor has already been called", translated_definition.name)
                        )),
                    ]))),
                ])));

                // Set the `constructor_called` storage field to `true` at the end of the function
                // storage.initialized.write(true);
                constructor_body.statements.push(sway::Statement::from(sway::Expression::create_function_calls(None, &[
                    ("storage", None),
                    (constructor_called_variable_name.as_str(), None),
                    ("write", Some((None, vec![
                        sway::Expression::from(sway::Literal::Bool(true)),
                    ]))),
                ])));
            }

            // Insert statements into the constructor to initialize each struct mapping
            for (mapping_name, field_names) in translated_definition.mapping_names.iter() {
                let mut i = 0;

                // Skip `require` statements in the constructor
                while i < constructor_body.statements.len() {
                    let sway::Statement::Expression(sway::Expression::FunctionCall(f)) = &constructor_body.statements[i] else { break };
                    let Some(id) = f.function.as_identifier() else { break }; 
                    if id != "require" {
                        break;
                    }
                    i += 1;
                }

                // storage.x_instance_count.write(i + 1);
                constructor_body.statements.insert(i, sway::Statement::from(sway::Expression::create_function_calls(None, &[
                    ("storage", None),
                    (format!("{mapping_name}_instance_count").as_str(), None),
                    ("write", Some((None, vec![sway::Expression::from(sway::BinaryExpression { 
                        operator: "+".into(),
                        lhs: sway::Expression::create_identifier("i".to_string()),
                        rhs: sway::Expression::from(sway::Literal::DecInt(BigUint::one(), None))
                    })])))
                ])));   

                // let i = storage.x_instance_count.read();
                constructor_body.statements.insert(i, sway::Statement::from(sway::Let { 
                    pattern: sway::LetPattern::Identifier(sway::LetIdentifier { 
                        is_mutable: false,
                        name: "i".to_string()
                    }),
                    type_name: None,
                    value: sway::Expression::create_function_calls(None, &[
                        ("storage", None),
                        (format!("{mapping_name}_instance_count").as_str(), None),
                        ("read", Some((None, vec![])))
                    ])
                }));

                // HACK: Skip forward two statements
                i += 2;

                for field_name in field_names.iter().rev() {
                    // storage.my_struct.my_mapping.write(Some(my_mapping));
                    constructor_body.statements.insert(i, sway::Statement::from(sway::Expression::create_function_calls(None, &[
                        ("storage", None),
                        (mapping_name.as_str(), None),
                        (field_name.as_str(), None),
                        ("write", Some((None, vec![
                            sway::Expression::create_function_calls(None, &[
                                ("Some", Some((None, vec![
                                    sway::Expression::create_identifier(field_name.to_string()),
                                ]))),
                            ]),
                        ]))),
                    ])));

                    // let my_mapping = storage.my_struct_mappings.get(i);
                    constructor_body.statements.insert(i, sway::Statement::from(
                        sway::Let { 
                            pattern: sway::LetPattern::Identifier(sway::LetIdentifier { 
                                is_mutable: false,
                                name: field_name.clone()
                            }),
                            type_name: None,
                            value: sway::Expression::create_function_calls(None, &[
                                ("storage", None),
                                (format!("{mapping_name}_{field_name}s").as_str(), None),
                                ("get", Some((None, vec![sway::Expression::create_identifier("i".to_string())])))
                            ])
                        }
                    ));
                }
            }
            
            // Set up constructor attributes
            let constructor_attrs = {
                let constructor_fn = translated_definition.get_constructor_fn();

                *constructor_fn.body.as_mut().unwrap() = constructor_body;

                if constructor_fn.attributes.is_none() {
                    constructor_fn.attributes = Some(AttributeList::default());
                }

                let Some(attributes) = constructor_fn.attributes.as_mut() else { unreachable!() };

                if !attributes.attributes.iter().any(|a| a.name == "storage") {
                    attributes.attributes.push(sway::Attribute { 
                        name: "storage".to_string(),
                        parameters: None
                    });
                }

                let Some(storage_attr) = attributes.attributes.iter_mut().find(|a| a.name == "storage") else { unreachable!() };
                if storage_attr.parameters.is_none() {
                    storage_attr.parameters = Some(Vec::default());
                }

                let Some(parameters) = storage_attr.parameters.as_mut() else { unreachable!() };
                
                if !parameters.iter().any(|p| p == "read") {
                    parameters.push("read".to_string());
                }

                if !parameters.iter().any(|p| p == "write") {
                    parameters.push("write".to_string());
                }

                Some(attributes.clone())
            };

            let abi = translated_definition.get_abi();

            if let Some(abi_constructor) = abi.functions.iter_mut().find(|f| f.name == "constructor") {
                abi_constructor.attributes = constructor_attrs;
            }
        };

        // Translate any contract definitions in the file
        for source_unit_part in source_unit.0.iter() {
            let solidity::SourceUnitPart::ContractDefinition(contract_definition) = source_unit_part else { continue };

            if let Some(definition_name) = definition_name {
                if contract_definition.name.as_ref().unwrap().name != *definition_name {
                    continue;
                }
            }

            let mut translated_definition = TranslatedDefinition {
                kind: Some(contract_definition.ty.clone()),
                name: contract_definition.name.as_ref().unwrap().name.clone(),
                inherits: contract_definition.base.iter()
                    .map(|b| {
                        b.name.identifiers.iter().map(|i| i.name.clone())
                            .collect::<Vec<_>>()
                            .join(".")
                    })
                    .collect(),
                contract_names: contract_names.clone(),
                ..translated_definition.clone()
            };
            
            translate_contract_definition(
                self,
                &mut translated_definition,
                import_directives.as_slice(),
                contract_definition,
            )?;

            finalize_mappings(self, &mut translated_definition);

            self.translated_definitions.push(translated_definition);
        }
        
        finalize_mappings(self, &mut translated_definition);

        // HACK: if we don't have any contracts, add a standalone translated definition
        if contract_names.is_empty() {
            self.translated_definitions.push(translated_definition);
        }

        Ok(())
    }

    /// Get the project type from the [PathBuf] and return a [ProjectKind]
    pub fn detect_project_type<P: AsRef<Path>>(&mut self, path: P) -> Result<(), Error> {
        let path = path.as_ref();
        if path.join(ProjectKind::FOUNDRY_CONFIG_FILE).exists() {
            self.kind = ProjectKind::Foundry {
                remappings: HashMap::new(),
            };

            let remappings = self.get_remappings(path).map_err(|e| Error::Wrapped(
                format!("Failed to get remappings for Foundry project: {e:#?}").into()
            ))?;

            self.kind = ProjectKind::Foundry { remappings };
        } else if path.join(ProjectKind::HARDHAT_CONFIG_FILE).exists() || path.join(ProjectKind::HARDHAT_CONFIG_FILE_TS).exists() {
            self.kind = ProjectKind::Hardhat;
        } else if path.join(ProjectKind::BROWNIE_CONFIG_FILE).exists() {
            self.kind = ProjectKind::Brownie { remappings: HashMap::new() };

            self.kind = ProjectKind::Brownie { remappings: self.get_remappings(path).map_err(|e| Error::Wrapped(
                format!("Failed to get remappings for Brownie project: {e:#?}").into()
            ))? };
        } else if path.join(ProjectKind::TRUFFLE_CONFIG_FILE).exists() {
            self.kind = ProjectKind::Truffle;
        } else if path.join(ProjectKind::DAPP_CONFIG_FILE).exists() {
            self.kind = ProjectKind::Dapp;
        } else {
            self.kind = ProjectKind::Unknown;
        }

        Ok(())
    }

    /// Get the project type path from the [ProjectKind] and return a [PathBuf]
    pub fn get_project_type_path(&self, source_unit_directory: &Path, filename: &str) -> Result<PathBuf, Error> {
        let project_root_folder = find_project_root_folder(source_unit_directory);

        let Some(project_root_folder) = project_root_folder else {
            // If we cant find a project root folder we return the filename as is
            return Ok(PathBuf::from(filename));
        };

        match &self.kind {
            // Remappings in foundry and brownie are handled using the same pattern
            ProjectKind::Foundry { remappings } | ProjectKind::Brownie { remappings } => {
                for (k, v) in remappings {
                    if filename.starts_with(k) {
                        let project_full_path = project_root_folder.join(v);
                        return Ok(PathBuf::from(filename.replace(k, project_full_path.to_string_lossy().as_ref())))
                    }
                }

                Ok(source_unit_directory.join(filename))
            }

            // Remappings in hardhat and truffle are done using the @ symbol and the node_modules folder
            ProjectKind::Hardhat | ProjectKind::Truffle => {
                if filename.starts_with('.') {
                    Ok(project_root_folder.join(filename))
                } else {
                    Ok(project_root_folder.join("node_modules").join(filename))
                }
            }

            ProjectKind::Dapp => {
                let filename = PathBuf::from(filename);
                let mut components: Vec<_> = filename.components().collect();
                
                if components.len() <= 1 {
                    panic!("Dapp filename should have more than one component")
                }

                match &components[0] {
                    Component::Normal(_) => {
                        components.insert(1, Component::Normal("src".as_ref()));
                        let component = components.iter().map(|c| c.as_os_str()).collect::<PathBuf>();
                        Ok(project_root_folder.join("lib").join(component))
                    }

                    _ => {
                        Ok(project_root_folder.join(filename))
                    }
                }
            }

            // If we find that the project type is unknown we return the filename as is
            ProjectKind::Unknown => {
                println!("Charcoal was unable to detect the project type.");
                println!("Please make sure you are targeting the root folder of the project (do not target the contracts folder itself).");
                Ok(PathBuf::from(filename))
            },
        }
    }

    /// Get the re mappings from the re mappings file on the root folder of the project represented by the [PathBuf]
    fn get_remappings(&self, root_folder_path: &Path) -> Result<HashMap<String, String>, Error> {
        match &self.kind {
            ProjectKind::Foundry { .. } => {
                let remappings_filename = "remappings.txt";
                
                let lines: Vec<String> = if root_folder_path.join(remappings_filename).exists() {
                    // Get the remappings.txt file from the root of the project folder
                    let remappings_content = std::fs::read_to_string(root_folder_path.join(remappings_filename))
                        .map_err(|e| Error::Wrapped(e.into()))?;

                    remappings_content.lines().map(str::to_string).collect()
                } else {
                    // Get foundry toml file from the root of the project folder
                    let remappings_from_toml_str = std::fs::read_to_string(root_folder_path.join(ProjectKind::FOUNDRY_CONFIG_FILE))
                        .map_err(|e| Error::Wrapped(e.into()))?;
        
                    let remappings_from_toml: toml::Value = toml::from_str(&remappings_from_toml_str)
                        .map_err(|e| Error::Wrapped(e.into()))?;

                    let value = find_in_toml_value(&remappings_from_toml, remappings_filename.strip_suffix(".txt").unwrap());

                    let Some(value) = value.as_ref() else { return Ok(HashMap::new()) };

                    let toml::Value::Array(arr) = value else {
                        panic!("remappings key in foundry.toml should be an array")
                    };

                    arr.iter().map(|x| x.as_str().unwrap().to_string()).collect::<Vec<_>>()
                };

                let mut remappings = HashMap::new();

                for line in lines {
                    let mut split = line.split('=');
                    let from = split.next().unwrap().to_string();
                    let to = split.next().unwrap().to_string();
                    remappings.insert(from, to);
                }
                
                Ok(remappings)
            }

            ProjectKind::Brownie { .. } => {
                let remappings_from_yaml_str = std::fs::read_to_string(root_folder_path.join(ProjectKind::BROWNIE_CONFIG_FILE))
                    .map_err(|e| Error::Wrapped(e.into()))?;

                let remappings_from_yaml: serde_yaml::Value = serde_yaml::from_str(&remappings_from_yaml_str)
                    .map_err(|e| Error::Wrapped(e.into()))?;
                
                let Some(compiler) = remappings_from_yaml.get("compiler") else {
                    return Err(Error::Wrapped("compiler key not found in brownie-config.yaml".into()))
                };

                let Some(solc) = compiler.get("solc") else {
                    return Err(Error::Wrapped("solc key not found in brownie-config.yaml".into()))
                };

                let Some(remappings) = solc.get("solidity.remappings").or_else(|| solc.get("remappings")) else {
                    return Err(Error::Wrapped("solidity.remappings key not found in brownie-config.yaml".into()));
                };

                let serde_yaml::Value::Sequence(seq) = remappings else {
                    return Err(Error::Wrapped("solidity.remappings should be a sequence".into()));
                };
                    
                let mut remappings = HashMap::new();
                
                for v in seq {
                    let mut split = v.as_str().unwrap().split('=');
                    let from = split.next().unwrap().to_string();
                    let to = split.next().unwrap().to_string();
                    remappings.insert(from, to);
                }

                Ok(remappings)
            }

            _ => Ok(HashMap::new())
        }
    }
}

/// Recursively check to find the root folder of the project and return a [PathBuf]
pub fn find_project_root_folder<P: AsRef<Path>>(path: P) -> Option<PathBuf> {
    let path = path.as_ref();

    if path.join(ProjectKind::FOUNDRY_CONFIG_FILE).exists()
    || path.join(ProjectKind::HARDHAT_CONFIG_FILE).exists()
    || path.join(ProjectKind::HARDHAT_CONFIG_FILE_TS).exists()
    || path.join(ProjectKind::BROWNIE_CONFIG_FILE).exists()
    || path.join(ProjectKind::TRUFFLE_CONFIG_FILE).exists()
    || path.join(ProjectKind::DAPP_CONFIG_FILE).exists() {
        return Some(path.to_path_buf());
    }

    if let Some(parent) = path.parent() {
        return find_project_root_folder(parent);
    }

    None
}

/// Find a key in a [toml::Table] and return the [toml::Value]
fn find_in_toml_value(value: &toml::Value, key: &str) -> Option<toml::Value> {
    match value {
        toml::Value::Table(table) => {
            for (k, v) in table {
                if k == key {
                    return Some(v.clone());
                }
                if let Some(val) = find_in_toml_value(v, key) {
                    return Some(val);
                }
            }
            None
        },
        _ => None,
    }
}
