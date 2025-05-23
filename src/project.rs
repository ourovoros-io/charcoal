use convert_case::{Case, Casing};
use solang_parser::pt as solidity;
use std::{
    cell::RefCell,
    collections::HashMap,
    path::{Component, Path, PathBuf},
    rc::Rc,
};

use crate::{
    cli::Args,
    error::Error,
    translate::{self, TranslatedModule},
    wrapped_err,
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
    pub root_folder: Option<PathBuf>,
    pub output_directory: Option<PathBuf>,
    pub kind: ProjectKind,
    pub queue: Vec<PathBuf>,
    pub line_ranges: HashMap<PathBuf, Vec<(usize, usize)>>,
    pub solidity_source_units: HashMap<PathBuf, solidity::SourceUnit>,
    pub translated_modules: Vec<Rc<RefCell<TranslatedModule>>>,
}

impl Project {
    pub fn new(options: &Args) -> Result<Self, Error> {
        let (kind, root_folder) = match options.root_folder.as_ref() {
            Some(root_folder) => {
                let kind = Self::detect_project_type(&root_folder)?;
                let root_folder = wrapped_err!(std::fs::canonicalize(root_folder.clone()))?;
                (kind, Some(root_folder))
            }
            None => {
                if let Some(root_path) = Self::find_project_root_folder(options.target.as_path()) {
                    let kind = Self::detect_project_type(&root_path)?;
                    (kind, Some(root_path))
                } else {
                    (ProjectKind::Unknown, None)
                }
            }
        };

        let mut result = Self {
            root_folder,
            output_directory: options.output_directory.clone(),
            kind,
            ..Default::default()
        };

        result.queue = result.create_usage_queue()?;

        Ok(result)
    }

    /// Attempts to parse the file from the supplied `path`.
    #[inline]
    fn parse_solidity_source_unit<P: AsRef<Path>>(&mut self, path: P) -> Result<(), Error> {
        if !path.as_ref().exists() {
            return Err(Error::Wrapped(
                format!("File not found: {}", path.as_ref().to_string_lossy()).into(),
            ));
        }

        let path = wrapped_err!(path.as_ref().canonicalize())?;

        let source = wrapped_err!(std::fs::read_to_string(path.clone()))?;

        self.load_line_ranges(&path, source.as_str());
        let line_ranges = self.line_ranges.get(&path).unwrap();

        let (source_unit, _comments) = solang_parser::parse(source.as_str(), 0)
            .map_err(|e| Error::SolangDiagnostics(path.clone(), line_ranges.clone(), e))?;

        // TODO: do we need the comments for anything?

        self.solidity_source_units.insert(path, source_unit);

        Ok(())
    }

    /// Loads line ranges in a specific file `path` from the provided `source` text.
    #[inline]
    fn load_line_ranges(&mut self, path: &PathBuf, source: &str) {
        let mut line_range = (0usize, 0usize);

        for (i, c) in source.chars().enumerate() {
            if c == '\n' {
                line_range.1 = i;
                self.line_ranges
                    .entry(path.clone())
                    .or_default()
                    .push(line_range);
                line_range = (i + 1, 0);
            }
        }

        if line_range.1 > line_range.0 {
            self.line_ranges
                .entry(path.clone())
                .or_default()
                .push(line_range);
        }
    }

    #[inline]
    pub fn loc_to_line_and_column<P: AsRef<Path>>(
        &self,
        path: P,
        loc: &solidity::Loc,
    ) -> Option<(usize, usize)> {
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

    /// Get the project type from the [PathBuf] and return a [ProjectKind]
    fn detect_project_type<P: AsRef<Path>>(path: P) -> Result<ProjectKind, Error> {
        let path = path.as_ref();
        let mut kind = ProjectKind::Unknown;
        if path.join(ProjectKind::FOUNDRY_CONFIG_FILE).exists() {
            kind = ProjectKind::Foundry {
                remappings: HashMap::new(),
            };

            let remappings = wrapped_err!(Self::get_remappings(&kind, path))?;

            kind = ProjectKind::Foundry { remappings };
        } else if path.join(ProjectKind::HARDHAT_CONFIG_FILE).exists()
            || path.join(ProjectKind::HARDHAT_CONFIG_FILE_TS).exists()
        {
            kind = ProjectKind::Hardhat;
        } else if path.join(ProjectKind::BROWNIE_CONFIG_FILE).exists() {
            kind = ProjectKind::Brownie {
                remappings: HashMap::new(),
            };

            kind = ProjectKind::Brownie {
                remappings: wrapped_err!(Self::get_remappings(&kind, path))?,
            };
        } else if path.join(ProjectKind::TRUFFLE_CONFIG_FILE).exists() {
            kind = ProjectKind::Truffle;
        } else if path.join(ProjectKind::DAPP_CONFIG_FILE).exists() {
            kind = ProjectKind::Dapp;
        }

        Ok(kind)
    }

    /// Find a key in a [toml::Table] and return the [toml::Value]
    fn find_in_toml_value(value: &toml::Value, key: &str) -> Option<toml::Value> {
        match value {
            toml::Value::Table(table) => {
                for (k, v) in table {
                    if k == key {
                        return Some(v.clone());
                    }
                    if let Some(val) = Self::find_in_toml_value(v, key) {
                        return Some(val);
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Get the project type path from the [ProjectKind] and return a [PathBuf]
    fn get_project_type_path(
        &self,
        source_unit_directory: &Path,
        filename: &str,
    ) -> Result<PathBuf, Error> {
        let Some(project_root_folder) = self.root_folder.as_ref() else {
            // If we cant find a project root folder we return the filename as is
            return Ok(PathBuf::from(filename));
        };

        match &self.kind {
            // Remappings in foundry and brownie are handled using the same pattern
            ProjectKind::Foundry { remappings } | ProjectKind::Brownie { remappings } => {
                for (k, v) in remappings {
                    if filename.starts_with(k) {
                        let project_full_path = project_root_folder.join(v);
                        return Ok(PathBuf::from(
                            filename.replace(k, project_full_path.to_string_lossy().as_ref()),
                        ));
                    }
                }

                Ok(source_unit_directory.join(filename))
            }

            // Remappings in hardhat and truffle are done using the @ symbol and the node_modules folder
            ProjectKind::Hardhat | ProjectKind::Truffle => {
                if filename.starts_with('.') {
                    Ok(source_unit_directory.join(filename))
                } else if filename.starts_with('@') {
                    Ok(project_root_folder.join("node_modules").join(filename))
                } else {
                    Ok(project_root_folder.join(filename))
                }
            }

            ProjectKind::Dapp => {
                if filename.starts_with('.') {
                    let result = source_unit_directory.join(filename);
                    return Ok(result);
                }

                let filename = PathBuf::from(filename);
                let mut components: Vec<_> = filename.components().collect();

                if components.len() <= 1 {
                    panic!("Dapp filename should have more than one component")
                }

                match &components[0] {
                    Component::Normal(_) => {
                        components.insert(1, Component::Normal("src".as_ref()));
                        let component = components
                            .iter()
                            .map(|c| c.as_os_str())
                            .collect::<PathBuf>();
                        Ok(project_root_folder.join("lib").join(component))
                    }

                    _ => Ok(project_root_folder.join(filename)),
                }
            }

            // If we find that the project type is unknown we return the filename as is
            ProjectKind::Unknown => {
                println!("Charcoal was unable to detect the project type.");
                println!(
                    "Please make sure you are targeting the root folder of the project (do not target the contracts folder itself)."
                );
                Ok(PathBuf::from(filename))
            }
        }
    }

    /// Get the re mappings from the re mappings file on the root folder of the project represented by the [PathBuf]
    fn get_remappings(
        kind: &ProjectKind,
        root_folder_path: &Path,
    ) -> Result<HashMap<String, String>, Error> {
        match kind {
            ProjectKind::Foundry { .. } => {
                let remappings_filename = "remappings.txt";

                let lines: Vec<String> = if root_folder_path.join(remappings_filename).exists() {
                    // Get the remappings.txt file from the root of the project folder
                    let remappings_content = wrapped_err!(std::fs::read_to_string(
                        root_folder_path.join(remappings_filename)
                    ))?;

                    remappings_content.lines().map(str::to_string).collect()
                } else {
                    // Get foundry toml file from the root of the project folder
                    let remappings_from_toml_str = wrapped_err!(std::fs::read_to_string(
                        root_folder_path.join(ProjectKind::FOUNDRY_CONFIG_FILE)
                    ))?;

                    let remappings_from_toml: toml::Value =
                        wrapped_err!(toml::from_str(&remappings_from_toml_str))?;

                    let value = Self::find_in_toml_value(
                        &remappings_from_toml,
                        remappings_filename.strip_suffix(".txt").unwrap(),
                    );

                    let Some(value) = value.as_ref() else {
                        return Ok(HashMap::new());
                    };

                    let toml::Value::Array(arr) = value else {
                        panic!("remappings key in foundry.toml should be an array")
                    };

                    arr.iter()
                        .map(|x| x.as_str().unwrap().to_string())
                        .collect::<Vec<_>>()
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
                let remappings_from_yaml_str = wrapped_err!(std::fs::read_to_string(
                    root_folder_path.join(ProjectKind::BROWNIE_CONFIG_FILE)
                ))?;

                let remappings_from_yaml: serde_yaml::Value =
                    wrapped_err!(serde_yaml::from_str(&remappings_from_yaml_str))?;

                let Some(compiler) = remappings_from_yaml.get("compiler") else {
                    return Err(Error::Wrapped(
                        "compiler key not found in brownie-config.yaml".into(),
                    ));
                };

                let Some(solc) = compiler.get("solc") else {
                    return Err(Error::Wrapped(
                        "solc key not found in brownie-config.yaml".into(),
                    ));
                };

                let Some(remappings) = solc
                    .get("solidity.remappings")
                    .or_else(|| solc.get("remappings"))
                else {
                    return Err(Error::Wrapped(
                        "solidity.remappings key not found in brownie-config.yaml".into(),
                    ));
                };

                let serde_yaml::Value::Sequence(seq) = remappings else {
                    return Err(Error::Wrapped(
                        "solidity.remappings should be a sequence".into(),
                    ));
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

            _ => Ok(HashMap::new()),
        }
    }

    /// Recursively check to find the root folder of the project and return a [PathBuf]
    fn find_project_root_folder<P: AsRef<Path>>(path: P) -> Option<PathBuf> {
        let path = path.as_ref();

        if path.join(ProjectKind::FOUNDRY_CONFIG_FILE).exists()
            || path.join(ProjectKind::HARDHAT_CONFIG_FILE).exists()
            || path.join(ProjectKind::HARDHAT_CONFIG_FILE_TS).exists()
            || path.join(ProjectKind::BROWNIE_CONFIG_FILE).exists()
            || path.join(ProjectKind::TRUFFLE_CONFIG_FILE).exists()
        {
            return Some(path.to_path_buf());
        }

        if let Some(parent) = path.parent() {
            return Self::find_project_root_folder(parent);
        }

        None
    }

    /// Recursively search for .sol files in the given directory
    fn collect_source_unit_paths(
        path: &Path,
        project_kind: &ProjectKind,
    ) -> Result<Vec<PathBuf>, Error> {
        let mut source_unit_paths = vec![];

        if let ProjectKind::Hardhat | ProjectKind::Truffle = project_kind {
            // Skip the node_modules folder. Only translate things that are imported explicitly.
            if path
                .to_string_lossy()
                .replace('\\', "/")
                .contains("/node_modules/")
            {
                return Ok(vec![]);
            }
        }

        if !path.is_dir() {
            assert!(
                path.extension().unwrap() == "sol",
                "Only solidity files are supported."
            );

            if !path.exists() {
                return Err(Error::Wrapped(Box::new(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!("File not found: {}", path.to_string_lossy()),
                ))));
            }

            source_unit_paths.push(wrapped_err!(path.canonicalize())?);
        } else {
            for entry in wrapped_err!(std::fs::read_dir(path))? {
                let entry = wrapped_err!(entry)?;
                let path = entry.path();

                if path.is_dir() {
                    source_unit_paths.extend(Self::collect_source_unit_paths(&path, project_kind)?);
                    continue;
                }

                if let Some(extension) = path.extension() {
                    if extension == "sol" {
                        source_unit_paths.push(wrapped_err!(path.canonicalize())?);
                    }
                }
            }
        }

        Ok(source_unit_paths)
    }

    fn create_usage_queue(&mut self) -> Result<Vec<PathBuf>, Error> {
        let paths =
            Self::collect_source_unit_paths(self.root_folder.as_ref().unwrap(), &self.kind)?;

        // Build a mapping of file paths to import paths, and file paths to the number of times that that file is imported from another file
        let mut import_paths = HashMap::new();
        let mut import_counts = HashMap::new();

        fn collect_imports(
            project: &mut Project,
            path: &Path,
            import_paths: &mut HashMap<PathBuf, Vec<PathBuf>>,
            import_counts: &mut HashMap<PathBuf, i32>,
        ) {
            import_counts.entry(path.into()).or_insert(0);

            project.parse_solidity_source_unit(path).unwrap();
            let ast = project.solidity_source_units.get(path).cloned().unwrap();

            if !import_paths.contains_key(path) {
                let paths = project.get_import_paths(&ast, path).unwrap();

                for import_path in paths.iter() {
                    collect_imports(project, import_path, import_paths, import_counts);
                    *import_counts.entry(import_path.clone()).or_insert(0) += 1;
                }

                import_paths.insert(path.into(), paths);
            }
        }

        for path in paths.iter() {
            collect_imports(self, path, &mut import_paths, &mut import_counts);
        }

        // Start with all the files that have an import count of 0 (as in they are never imported by another file)
        // When visiting a file you visit each of its imports first, inserting them higher in the queue before inserting the current file
        // After visiting each import path, add the path of the current file
        fn queue_imports(
            project: &mut Project,
            import_paths: &HashMap<PathBuf, Vec<PathBuf>>,
            path: &Path,
            queue: &mut Vec<PathBuf>,
        ) {
            let current_import_paths = import_paths.get(path).unwrap();

            for import_path in current_import_paths.iter() {
                queue_imports(project, import_paths, import_path, queue);
            }

            if !queue.iter().any(|p| p == path) {
                queue.push(path.into());
            }
        }

        let mut queue = vec![];

        for (path, _) in import_counts.iter().filter(|&(_, x)| *x == 0) {
            queue_imports(self, &import_paths, path, &mut queue);
        }

        Ok(queue)
    }

    /// Returns all the contract imports
    fn get_contract_imports(source_unit: &solidity::SourceUnit) -> Option<Vec<solidity::Import>> {
        let imports: Vec<solidity::Import> = source_unit
            .0
            .iter()
            .filter_map(|part| match part {
                solidity::SourceUnitPart::ImportDirective(import) => Some(import),
                _ => None,
            })
            .cloned()
            .collect();

        (!imports.is_empty()).then_some(imports)
    }

    fn get_import_paths(
        &mut self,
        ast: &solidity::SourceUnit,
        source_unit_path: &Path,
    ) -> Result<Vec<PathBuf>, Error> {
        let mut import_paths = Vec::new();
        if let Some(import_directives) = Self::get_contract_imports(&ast) {
            for import_directive in &import_directives {
                let import_path = match import_directive {
                    solang_parser::pt::Import::Plain(import_path, _) => import_path,
                    solang_parser::pt::Import::GlobalSymbol(import_path, _, _) => import_path,
                    solang_parser::pt::Import::Rename(import_path, _, _) => import_path,
                };

                let import_path = match import_path {
                    solang_parser::pt::ImportPath::Filename(path) => {
                        std::path::PathBuf::from(path.to_string())
                    }
                    solang_parser::pt::ImportPath::Path(path) => {
                        std::path::PathBuf::from(path.to_string())
                    }
                };

                // Clean the import path and remove quotes
                let mut import_path = import_path.to_str().unwrap().replace('\"', "");

                if let ProjectKind::Unknown = self.kind {
                    // Join the import path with the source unit path
                    import_path = source_unit_path
                        .parent()
                        .unwrap()
                        .join(import_path)
                        .to_str()
                        .unwrap()
                        .to_string();
                } else {
                    // If we have detected a framework we need to resolve the path based on the remappings if found
                    import_path = self
                        .get_project_type_path(source_unit_path.parent().unwrap(), &import_path)?
                        .to_str()
                        .unwrap()
                        .to_string();
                }

                // Normalize the import path
                let import_path = wrapped_err!(std::fs::canonicalize(import_path))?;

                import_paths.push(import_path);
            }
        }
        Ok(import_paths)
    }

    pub fn canonicalize_import_path(
        &self,
        source_unit_directory: &Path,
        path_string: &str,
    ) -> Result<PathBuf, Error> {
        let mut import_path = PathBuf::from(path_string);

        if !import_path.to_string_lossy().starts_with('.') {
            import_path = self.get_project_type_path(source_unit_directory, path_string)?;
        } else {
            import_path = source_unit_directory.join(import_path);
        }

        import_path = wrapped_err!(import_path.canonicalize())?;

        if !import_path.exists() {
            return Err(Error::Wrapped(Box::new(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("File not found: {}", import_path.to_string_lossy()),
            ))));
        }

        Ok(import_path)
    }

    pub fn find_module(&mut self, path: &Path) -> Option<Rc<RefCell<TranslatedModule>>> {
        let path = PathBuf::from(path).with_extension("");
        let mut parent_module: Option<Rc<RefCell<TranslatedModule>>> = None;

        for comp in path.components() {
            if let Component::RootDir = comp {
                continue;
            }

            let comp = comp
                .as_os_str()
                .to_string_lossy()
                .to_string()
                .replace(".", "_")
                .to_case(Case::Snake);
            match parent_module.clone() {
                Some(parent) => {
                    if let Some(module) = parent
                        .borrow()
                        .submodules
                        .iter()
                        .find(|s| s.borrow().name == comp)
                    {
                        parent_module = Some(module.clone());
                    } else {
                        return None;
                    }
                }
                None => {
                    if let Some(module) = self
                        .translated_modules
                        .iter()
                        .find(|t| t.borrow().name == comp)
                    {
                        parent_module = Some(module.clone());
                    } else {
                        return None;
                    }
                }
            }
        }

        parent_module
    }

    pub fn find_or_create_module(&mut self, path: &Path) -> Rc<RefCell<TranslatedModule>> {
        let mut current_path = PathBuf::new();

        let components = path
            .components()
            .filter(|c| !matches!(c, Component::RootDir))
            .collect::<Vec<_>>();
        current_path.push(components[0].clone());

        let component = components[0]
            .as_os_str()
            .to_string_lossy()
            .to_string()
            .replace(".", "_")
            .to_case(Case::Snake);

        let mut parent_module = match self
            .translated_modules
            .iter()
            .find(|t| t.borrow().name == component)
        {
            Some(result) => result.clone(),
            None => {
                self.translated_modules
                    .push(Rc::new(RefCell::new(TranslatedModule {
                        name: component,
                        path: current_path.clone(),
                        ..Default::default()
                    })));

                self.translated_modules.last().cloned().unwrap()
            }
        };

        for component in &components[1..] {
            current_path.push(component.clone());
            let component = component
                .as_os_str()
                .to_string_lossy()
                .to_string()
                .replace(".", "_")
                .to_case(Case::Snake);

            let found = parent_module
                .borrow()
                .submodules
                .iter()
                .find(|t| t.borrow().name == component)
                .cloned();

            match found {
                Some(result) => parent_module = result,
                None => {
                    parent_module
                        .borrow_mut()
                        .submodules
                        .push(Rc::new(RefCell::new(TranslatedModule {
                            name: component,
                            path: current_path.clone(),
                            ..Default::default()
                        })));

                    let child = parent_module.borrow().submodules.last().cloned().unwrap();
                    parent_module = child;
                }
            }
        }

        parent_module
    }

    pub fn translate(&mut self) -> Result<(), Error> {
        for source_unit_path in self.queue.clone() {
            self.translate_file(&source_unit_path)?;
        }

        match self.output_directory.clone() {
            Some(output_directory) => {
                todo!("{:#?}", output_directory);
                // self.generate_forc_project(output_directory, source_unit_path)?;
            }

            None => {
                println!("{:#?}", self.translated_modules);
                // for module in project.collect_translated_definitions(options.definition_name.as_ref(), source_unit_path) {
                //     println!("// Translated from {}", module.path.to_string_lossy());

                //     let module: sway::Module = module.into();
                //     println!("{}", sway::TabbedDisplayer(&module));
                // }
            }
        }

        Ok(())
    }

    fn translate_file(&mut self, source_unit_path: &Path) -> Result<(), Error> {
        // Ensure the source unit has been parsed
        if !self.solidity_source_units.contains_key(source_unit_path) {
            self.parse_solidity_source_unit(source_unit_path)?;
        }
        // Get the parsed source unit
        let source_unit = self
            .solidity_source_units
            .get(source_unit_path)
            .unwrap()
            .clone();

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

        let relative_path = PathBuf::from(
            source_unit_path.to_string_lossy().trim_start_matches(
                &self
                    .root_folder
                    .as_ref()
                    .unwrap()
                    .to_string_lossy()
                    .to_string(),
            ),
        )
        .with_extension("");

        let translated_module = self.find_or_create_module(&relative_path);

        translate::translate_import_directives(
            self,
            translated_module.clone(),
            &import_directives,
        )?;

        for type_definition in toplevel_type_definitions {
            translate::translate_type_definition(
                self,
                translated_module.clone(),
                &type_definition,
            )?;
        }

        for toplevel_enum in toplevel_enums {
            translate::translate_enum_definition(self, translated_module.clone(), &toplevel_enum)?;
        }

        for toplevel_struct in toplevel_structs {
            translate::translate_struct_definition(
                self,
                translated_module.clone(),
                &toplevel_struct,
            )?;
        }

        for toplevel_event in toplevel_events {
            translate::translate_event_definition(
                self,
                translated_module.clone(),
                &toplevel_event,
            )?;
        }

        for toplevel_error in toplevel_errors {
            translate::translate_error_definition(
                self,
                translated_module.clone(),
                &toplevel_error,
            )?;
        }

        for toplevel_function in toplevel_functions {}

        Ok(())
    }

    pub fn generate_forc_project<P1: AsRef<Path>, P2: AsRef<Path>>(
        &mut self,
        output_directory: P1,
        source_unit_path: P2,
    ) -> Result<(), Error> {
        todo!()
    }
}
