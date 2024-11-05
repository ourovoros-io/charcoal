use crate::{
    errors::Error,
    translate::{translate_contract_definition, TranslatedDefinition},
};
use solang_parser::pt as solidity;
use std::{
    cell::RefCell,
    collections::HashMap,
    path::{Component, Path, PathBuf},
    rc::Rc,
};

/// Represents the type of project as [ProjectType] default is [ProjectType::Unknown]
#[derive(Clone, Debug, Default)]
pub enum ProjectType {
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

impl ProjectType {
    pub const FOUNDRY_CONFIG_FILE: &'static str = "foundry.toml";
    pub const HARDHAT_CONFIG_FILE: &'static str = "hardhat.config.js";
    pub const BROWNIE_CONFIG_FILE: &'static str = "brownie-config.yaml";
    pub const TRUFFLE_CONFIG_FILE: &'static str = "truffle-config.js";
    pub const DAPP_CONFIG_FILE: &'static str = "Dappfile";
}

#[derive(Default)]
pub struct Project {
    pub line_ranges: HashMap<PathBuf, Vec<(usize, usize)>>,
    pub solidity_source_units: Rc<RefCell<HashMap<PathBuf, solidity::SourceUnit>>>,
    pub translated_definitions: Vec<TranslatedDefinition>,
    pub import_directives: HashMap<PathBuf, HashMap<PathBuf, Option<Vec<String>>>>,
    pub project_type: ProjectType,
}

impl Project {
    /// Attempts to parse the file from the supplied `path`.
    #[inline]
    fn parse_solidity_source_unit<P: AsRef<Path>>(&mut self, path: P) -> Result<(), Error> {
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

    pub fn translate(&mut self, definition_name: Option<&String>, source_unit_path: &Path) -> Result<(), Error> {
        let source_unit_directory = source_unit_path.parent().map(PathBuf::from).unwrap();
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
                },
                solidity::SourceUnitPart::VariableDefinition(_) => todo!("toplevel variable definition"),

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

        // Extend the import directive tree
        for import_directive in import_directives.iter() {
            let mut translate_import_directive = |definition_name: Option<&String>, filename: &solidity::StringLiteral| -> Result<(), Error> {
                let mut import_path = PathBuf::from(filename.string.clone());

                if !import_path.to_string_lossy().starts_with('.') {
                    import_path = self.get_project_type_path(source_unit_directory.as_path(), filename.string.as_str())?;
                } else {
                    import_path = source_unit_directory.join(import_path);
                }
                
                import_path = crate::get_canonical_path(import_path.clone(), false, false)
                    .map_err(|e| Error::Wrapped(format!("{}: {}", e, import_path.to_string_lossy()).into()))?;
                
                let import_directives = self.import_directives.entry(source_unit_path.into()).or_default();
                let definition_names = import_directives.entry(import_path).or_default();

                if let Some(definition_name) = definition_name {
                    if definition_names.is_none() {
                        *definition_names = Some(vec![]);
                    }

                    let definition_names = definition_names.as_mut().unwrap();

                    if !definition_names.contains(definition_name) {
                        definition_names.push(definition_name.clone());
                    }
                }

                Ok(())
            };

            match import_directive {
                solidity::Import::Plain(solidity::ImportPath::Filename(filename), _) => {
                    translate_import_directive(None, filename)?;
                }

                solidity::Import::Rename(solidity::ImportPath::Filename(filename), identifiers, _) => {
                    for (identifier, _) in identifiers.iter() {
                        translate_import_directive(Some(&identifier.name), filename)?;
                    }
                }

                _ => panic!("Unsupported import directive: {import_directive:#?}"),
            }
        }

        // Translate any contract definitions in the file
        for source_unit_part in source_unit.0.iter() {
            let solidity::SourceUnitPart::ContractDefinition(contract_definition) = source_unit_part else { continue };

            if let Some(definition_name) = definition_name {
                if contract_definition.name.as_ref().unwrap().name != *definition_name {
                    continue;
                }
            }

            translate_contract_definition(
                self,
                source_unit_path,
                import_directives.as_slice(),
                toplevel_using_directives.as_slice(),
                toplevel_type_definitions.as_slice(),
                toplevel_enums.as_slice(),
                toplevel_structs.as_slice(),
                toplevel_events.as_slice(),
                toplevel_errors.as_slice(),
                toplevel_functions.as_slice(),
                contract_names.as_slice(),
                contract_definition,
            )?;
        }

        Ok(())
    }

    /// Get the project type from the [PathBuf] and return a [ProjectType]
    pub fn detect_project_type<P: AsRef<Path>>(&mut self, path: P) -> Result<(), Error> {
        let path = path.as_ref();
        if path.join(ProjectType::FOUNDRY_CONFIG_FILE).exists() {
            self.project_type = ProjectType::Foundry {
                remappings: HashMap::new(),
            };

            let remappings = self.get_remappings(path).map_err(|e| Error::Wrapped(
                format!("Failed to get remappings for Foundry project: {:#?}", e).into()
            ))?;

            self.project_type = ProjectType::Foundry { remappings };
        } else if path.join(ProjectType::HARDHAT_CONFIG_FILE).exists() {
            self.project_type = ProjectType::Hardhat;
        } else if path.join(ProjectType::BROWNIE_CONFIG_FILE).exists() {
            self.project_type = ProjectType::Brownie { remappings: HashMap::new() };

            self.project_type = ProjectType::Brownie { remappings: self.get_remappings(path).map_err(|e| Error::Wrapped(
                format!("Failed to get remappings for Brownie project: {:#?}", e).into()
            ))? };
        } else if path.join(ProjectType::TRUFFLE_CONFIG_FILE).exists() {
            self.project_type = ProjectType::Truffle;
        } else if path.join(ProjectType::DAPP_CONFIG_FILE).exists() {
            self.project_type = ProjectType::Dapp;
        } else {
            self.project_type = ProjectType::Unknown;
        }

        Ok(())
    }

    /// Get the project type path from the [ProjectType] and return a [PathBuf]
    pub fn get_project_type_path(&self, source_unit_directory: &Path, filename: &str) -> Result<PathBuf, Error> {
        let project_root_folder = find_project_root_folder(source_unit_directory);

        let Some(project_root_folder) = project_root_folder else {
            // If we cant find a project root folder we return the filename as is
            return Ok(PathBuf::from(filename));
        };

        match &self.project_type {
            // Remappings in foundry and brownie are handled using the same pattern
            ProjectType::Foundry { remappings } | ProjectType::Brownie { remappings } => {
                for (k, v) in remappings {
                    if filename.starts_with(k) {
                        let project_full_path = project_root_folder.join(v);
                        return Ok(PathBuf::from(filename.replace(k, project_full_path.to_string_lossy().as_ref())))
                    }
                }

                Ok(PathBuf::from(source_unit_directory.join(filename)))
            }

            // Remappings in hardhat and truffle are done using the @ symbol and the node_modules folder
            ProjectType::Hardhat | ProjectType::Truffle => {
                if filename.starts_with('.') {
                    Ok(project_root_folder.join(filename))
                } else {
                    Ok(project_root_folder.join("node_modules").join(filename))
                }
            }

            ProjectType::Dapp => {
                let filename = PathBuf::from(filename);
                let mut components: Vec<_> = filename.components().collect();
                
                if components.len() <= 1 {
                    panic!("Dapp filename should have more than one component")
                }

                match &components[0] {
                    Component::Normal(_) => {
                        components.insert(1, Component::Normal("src".as_ref()));
                        let component = PathBuf::from(components.iter().map(|c| c.as_os_str()).collect::<PathBuf>());
                        Ok(project_root_folder.join("lib").join(component))
                    }

                    _ => {
                        Ok(project_root_folder.join(filename))
                    }
                }
            }

            // If we find that the project type is unknown we return the filename as is
            ProjectType::Unknown => Ok(PathBuf::from(filename)),
        }
    }

    /// Get the re mappings from the re mappings file on the root folder of the project represented by the [PathBuf]
    fn get_remappings(&self, root_folder_path: &Path) -> Result<HashMap<String, String>, Error> {
        match &self.project_type {
            ProjectType::Foundry { .. } => {
                let remappings_filename = "remappings.txt";
                
                let lines: Vec<String> = if root_folder_path.join(remappings_filename).exists() {
                    // Get the remappings.txt file from the root of the project folder
                    let remappings_content = std::fs::read_to_string(root_folder_path.join(remappings_filename))
                        .map_err(|e| Error::Wrapped(e.into()))?;

                    remappings_content.lines().map(str::to_string).collect()
                } else {
                    // Get foundry toml file from the root of the project folder
                    let remappings_from_toml_str = std::fs::read_to_string(root_folder_path.join(ProjectType::FOUNDRY_CONFIG_FILE))
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

            ProjectType::Brownie { .. } => {
                let remappings_from_yaml_str = std::fs::read_to_string(root_folder_path.join(ProjectType::BROWNIE_CONFIG_FILE))
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

    if path.join(ProjectType::FOUNDRY_CONFIG_FILE).exists() || path.join(ProjectType::HARDHAT_CONFIG_FILE).exists() 
    || path.join(ProjectType::BROWNIE_CONFIG_FILE).exists() || path.join(ProjectType::TRUFFLE_CONFIG_FILE).exists()  
    || path.join(ProjectType::DAPP_CONFIG_FILE).exists() {
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
