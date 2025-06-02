use crate::{cli::Args, error::Error, sway, translate::*, wrapped_err};
use convert_case::{Case, Casing};
use solang_parser::pt as solidity;
use std::{
    cell::RefCell,
    collections::HashMap,
    path::{Component, Path, PathBuf},
    rc::Rc,
};

/// Represents the type of project as [Framework] default is [Framework::Unknown]
#[derive(Clone, Debug, Default)]
pub enum Framework {
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

impl Framework {
    pub const FOUNDRY_CONFIG_FILE: &'static str = "foundry.toml";
    pub const HARDHAT_CONFIG_FILE: &'static str = "hardhat.config.js";
    pub const HARDHAT_CONFIG_FILE_TS: &'static str = "hardhat.config.ts";
    pub const BROWNIE_CONFIG_FILE: &'static str = "brownie-config.yaml";
    pub const TRUFFLE_CONFIG_FILE: &'static str = "truffle-config.js";
    pub const DAPP_CONFIG_FILE: &'static str = "Dappfile";
}

#[derive(Default)]
pub struct Project {
    pub options: Args,
    pub framework: Framework,
    pub queue: Vec<PathBuf>,
    pub line_ranges: HashMap<PathBuf, Vec<(usize, usize)>>,
    pub solidity_source_units: HashMap<PathBuf, solidity::SourceUnit>,
    pub translated_modules: Vec<Rc<RefCell<TranslatedModule>>>,
}

impl Project {
    pub fn new(options: &Args) -> Result<Self, Error> {
        let framework = Self::detect_project_type(&options.input)?;

        let mut result = Self {
            options: options.clone(),
            framework,
            ..Default::default()
        };

        result.queue = result.create_usage_queue()?;

        Ok(result)
    }

    /// Get the project type from the [PathBuf] and return a [Framework]
    fn detect_project_type<P: AsRef<Path>>(path: P) -> Result<Framework, Error> {
        let path = path.as_ref();
        let mut framework = Framework::Unknown;
        if path.join(Framework::FOUNDRY_CONFIG_FILE).exists() {
            framework = Framework::Foundry {
                remappings: HashMap::new(),
            };

            let remappings = wrapped_err!(Self::get_remappings(&framework, path))?;

            framework = Framework::Foundry { remappings };
        } else if path.join(Framework::HARDHAT_CONFIG_FILE).exists()
            || path.join(Framework::HARDHAT_CONFIG_FILE_TS).exists()
        {
            framework = Framework::Hardhat;
        } else if path.join(Framework::BROWNIE_CONFIG_FILE).exists() {
            framework = Framework::Brownie {
                remappings: HashMap::new(),
            };

            framework = Framework::Brownie {
                remappings: wrapped_err!(Self::get_remappings(&framework, path))?,
            };
        } else if path.join(Framework::TRUFFLE_CONFIG_FILE).exists() {
            framework = Framework::Truffle;
        } else if path.join(Framework::DAPP_CONFIG_FILE).exists() {
            framework = Framework::Dapp;
        }

        Ok(framework)
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

    /// Get the project type path from the [Framework] and return a [PathBuf]
    fn get_project_type_path(
        &self,
        source_unit_directory: &Path,
        filename: &str,
    ) -> Result<PathBuf, Error> {
        match &self.framework {
            // Remappings in foundry and brownie are handled using the same pattern
            Framework::Foundry { remappings } | Framework::Brownie { remappings } => {
                for (k, v) in remappings {
                    if filename.starts_with(k) {
                        let project_full_path = self.options.input.join(v);
                        return Ok(PathBuf::from(
                            filename.replace(k, project_full_path.to_string_lossy().as_ref()),
                        ));
                    }
                }

                Ok(source_unit_directory.join(filename))
            }

            // Remappings in hardhat and truffle are done using the @ symbol and the node_modules folder
            Framework::Hardhat | Framework::Truffle => {
                if filename.starts_with('.') {
                    Ok(source_unit_directory.join(filename))
                } else if filename.starts_with('@') {
                    Ok(self.options.input.join("node_modules").join(filename))
                } else {
                    Ok(self.options.input.join(filename))
                }
            }

            Framework::Dapp => {
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
                        Ok(self.options.input.join("lib").join(component))
                    }

                    _ => Ok(self.options.input.join(filename)),
                }
            }

            // If we find that the project type is unknown we return the filename as is
            Framework::Unknown => {
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
        framework: &Framework,
        root_folder_path: &Path,
    ) -> Result<HashMap<String, String>, Error> {
        match framework {
            Framework::Foundry { .. } => {
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
                        root_folder_path.join(Framework::FOUNDRY_CONFIG_FILE)
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

            Framework::Brownie { .. } => {
                let remappings_from_yaml_str = wrapped_err!(std::fs::read_to_string(
                    root_folder_path.join(Framework::BROWNIE_CONFIG_FILE)
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

        if path.join(Framework::FOUNDRY_CONFIG_FILE).exists()
            || path.join(Framework::HARDHAT_CONFIG_FILE).exists()
            || path.join(Framework::HARDHAT_CONFIG_FILE_TS).exists()
            || path.join(Framework::BROWNIE_CONFIG_FILE).exists()
            || path.join(Framework::TRUFFLE_CONFIG_FILE).exists()
            || path.join(Framework::DAPP_CONFIG_FILE).exists()
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
        framework: &Framework,
    ) -> Result<Vec<PathBuf>, Error> {
        let mut source_unit_paths = vec![];

        if let Framework::Hardhat | Framework::Truffle = framework {
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
                    source_unit_paths.extend(Self::collect_source_unit_paths(&path, framework)?);
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
        let paths = Self::collect_source_unit_paths(&self.options.input, &self.framework)?;

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

            let ast = project.parse_solidity_source_unit(path).cloned().unwrap();

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

        let mut paths = import_counts
            .iter()
            .filter(|&(_, x)| *x == 0)
            .map(|(x, _)| x.clone())
            .collect::<Vec<_>>();

        paths.sort();

        let mut queue = vec![];

        for path in paths {
            queue_imports(self, &import_paths, &path, &mut queue);
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

                if let Framework::Unknown = self.framework {
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
            import_path = self
                .get_project_type_path(source_unit_directory, path_string)
                .unwrap();
        } else {
            import_path = source_unit_directory.join(import_path);
        }

        import_path = wrapped_err!(import_path.canonicalize()).unwrap();

        if !import_path.exists() {
            return Err(Error::Wrapped(Box::new(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("File not found: {}", import_path.to_string_lossy()),
            ))));
        }

        Ok(import_path)
    }

    /// Attempts to parse the file from the supplied `path`.
    #[inline]
    fn parse_solidity_source_unit<P: AsRef<Path>>(
        &mut self,
        path: P,
    ) -> Result<&solidity::SourceUnit, Error> {
        if self.solidity_source_units.contains_key(path.as_ref()) {
            return Ok(self.solidity_source_units.get(path.as_ref()).unwrap());
        }

        let path = wrapped_err!(path.as_ref().canonicalize())?;
        let source = wrapped_err!(std::fs::read_to_string(path.clone()))?;
        let line_ranges = self.load_line_ranges(&path, source.as_str());

        let (source_unit, _comments) = solang_parser::parse(source.as_str(), 0)
            .map_err(|e| Error::SolangDiagnostics(path.clone(), line_ranges.clone(), e))?;

        // TODO: do we need the comments for anything?

        self.solidity_source_units.insert(path.clone(), source_unit);

        Ok(self.solidity_source_units.get(&path).unwrap())
    }

    /// Loads line ranges in a specific file `path` from the provided `source` text.
    #[inline]
    fn load_line_ranges(&mut self, path: &PathBuf, source: &str) -> &Vec<(usize, usize)> {
        if self.line_ranges.contains_key(path) {
            return self.line_ranges.get(path).unwrap();
        }

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

        self.line_ranges.get(path).unwrap()
    }

    #[inline]
    pub fn loc_to_line_and_column(
        &self,
        module: Rc<RefCell<TranslatedModule>>,
        loc: &solidity::Loc,
    ) -> Option<(usize, usize)> {
        let path = self
            .options
            .input
            .clone()
            .join(module.borrow().path.clone())
            .with_extension("sol");

        let line_ranges = self.line_ranges.get(&path)?;

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

    pub fn find_module_with_contract(
        &mut self,
        contract_name: &str,
    ) -> Option<Rc<RefCell<TranslatedModule>>> {
        fn check_module(
            module: Rc<RefCell<TranslatedModule>>,
            contract_name: &str,
        ) -> Option<Rc<RefCell<TranslatedModule>>> {
            if module
                .borrow()
                .contracts
                .iter()
                .any(|c| c.signature.to_string() == contract_name)
            {
                return Some(module.clone());
            }

            for module in module.borrow().submodules.iter() {
                if let Some(module) = check_module(module.clone(), contract_name) {
                    return Some(module.clone());
                }
            }

            None
        }

        for module in self.translated_modules.iter() {
            if let Some(module) = check_module(module.clone(), contract_name) {
                return Some(module.clone());
            }
        }

        None
    }
    pub fn translate(&mut self) -> Result<(), Error> {
        for source_unit_path in self.queue.clone() {
            self.translate_file(&source_unit_path)?;
        }

        match self.options.output_directory.clone() {
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
        // Parse the source unit
        let source_unit = self.parse_solidity_source_unit(source_unit_path)?;

        // Collect toplevel items ahead of time for contextual reasons
        let mut import_directives = vec![];
        let mut using_directives = vec![];
        let mut type_definitions = vec![];
        let mut enum_definitions = vec![];
        let mut struct_definitions = vec![];
        let mut event_definitions = vec![];
        let mut error_definitions = vec![];
        let mut function_definitions = vec![];
        let mut variable_definitions = vec![];
        let mut contract_definitions = vec![];

        for source_unit_part in source_unit.0.iter() {
            match source_unit_part {
                solidity::SourceUnitPart::PragmaDirective(_) => {
                    // NOTE: we don't need to do anything with pragma directives
                }

                solidity::SourceUnitPart::ImportDirective(import_directive) => {
                    import_directives.push(import_directive.clone());
                }

                solidity::SourceUnitPart::ContractDefinition(contract_definition) => {
                    contract_definitions.push(contract_definition.clone());
                }

                solidity::SourceUnitPart::EnumDefinition(enum_definition) => {
                    enum_definitions.push(enum_definition.as_ref().clone());
                }

                solidity::SourceUnitPart::StructDefinition(struct_definition) => {
                    struct_definitions.push(struct_definition.as_ref().clone());
                }

                solidity::SourceUnitPart::EventDefinition(event_definition) => {
                    event_definitions.push(event_definition.as_ref().clone());
                }

                solidity::SourceUnitPart::ErrorDefinition(error_definition) => {
                    error_definitions.push(error_definition.as_ref().clone());
                }

                solidity::SourceUnitPart::FunctionDefinition(function_definition) => {
                    function_definitions.push(function_definition.as_ref().clone());
                }

                solidity::SourceUnitPart::VariableDefinition(variable_definition) => {
                    variable_definitions.push(variable_definition.as_ref().clone());
                }

                solidity::SourceUnitPart::TypeDefinition(type_definition) => {
                    type_definitions.push(type_definition.as_ref().clone());
                }

                solidity::SourceUnitPart::Annotation(_) => {
                    // NOTE: we don't need to do anything with annotations
                }

                solidity::SourceUnitPart::Using(using_directive) => {
                    using_directives.push(using_directive.as_ref().clone());
                }

                solidity::SourceUnitPart::StraySemicolon(_) => {
                    // NOTE: we don't need to do anything with stray semicolons
                }
            }
        }

        // Create a new module to store the translated items in
        let module = self.find_or_create_module(
            &PathBuf::from(
                source_unit_path
                    .to_string_lossy()
                    .trim_start_matches(&self.options.input.to_string_lossy().to_string()),
            )
            .with_extension(""),
        );

        //
        // Translate each source unit part
        //

        translate_import_directives(self, module.clone(), &import_directives)?;

        // Collect the type definition signatures ahead of time
        let type_definitions_index = module.borrow().type_definitions.len();

        for type_definition in type_definitions.iter() {
            module.borrow_mut().type_definitions.push(TranslatedItem {
                signature: sway::TypeName::Identifier {
                    name: type_definition.name.name.clone(),
                    generic_parameters: None,
                },
                implementation: None,
            });
        }

        // Collect the enum signatures ahead of time
        let enums_index = module.borrow().enums.len();

        for enum_definition in enum_definitions.iter() {
            module.borrow_mut().enums.push(TranslatedItem {
                signature: sway::TypeName::Identifier {
                    name: enum_definition.name.as_ref().unwrap().name.clone(),
                    generic_parameters: None,
                },
                implementation: None,
            });
        }

        // Collect the struct signatures ahead of time
        let structs_index = module.borrow().structs.len();

        for struct_definition in struct_definitions.iter() {
            module.borrow_mut().structs.push(TranslatedItem {
                signature: sway::TypeName::Identifier {
                    name: struct_definition.name.as_ref().unwrap().name.clone(),
                    generic_parameters: None,
                },
                implementation: None,
            })
        }

        // Collect the function signatures ahead of time
        let functions_index = module.borrow().functions.len();

        for function_definition in function_definitions.iter() {
            if matches!(function_definition.ty, solidity::FunctionTy::Modifier) {
                continue;
            }

            let declaration =
                translate_function_declaration(self, module.clone(), function_definition)?;

            module.borrow_mut().functions.push(TranslatedItem {
                signature: declaration.type_name,
                implementation: None,
            });
        }

        // Collect the contract signatures ahead of time
        let contracts_index = module.borrow().contracts.len();

        for contract_definition in contract_definitions.iter() {
            module.borrow_mut().contracts.push(TranslatedItem {
                signature: sway::TypeName::Identifier {
                    name: contract_definition
                        .name
                        .as_ref()
                        .map(|name| name.name.clone())
                        .unwrap(),
                    generic_parameters: None,
                },
                implementation: None,
            });
        }

        for (i, type_definition) in type_definitions.into_iter().enumerate() {
            module.borrow_mut().type_definitions[type_definitions_index + i].implementation = Some(
                translate_type_definition(self, module.clone(), &type_definition)?,
            );
        }

        for (i, enum_definition) in enum_definitions.into_iter().enumerate() {
            module.borrow_mut().enums[enums_index + i].implementation = Some(
                translate_enum_definition(self, module.clone(), &enum_definition)?,
            );
        }

        for (i, struct_definition) in struct_definitions.into_iter().enumerate() {
            module.borrow_mut().structs[structs_index + i].implementation = Some(
                translate_struct_definition(self, module.clone(), &struct_definition)?,
            );
        }

        for event_definition in event_definitions {
            translate_event_definition(self, module.clone(), &event_definition)?;
        }

        for error_definition in error_definitions {
            translate_error_definition(self, module.clone(), &error_definition)?;
        }

        for variable_definition in variable_definitions {
            let (deferred_initializations, mapping_names) =
                translate_state_variable(self, module.clone(), &variable_definition)?;
            assert!(deferred_initializations.is_empty());
            assert!(mapping_names.is_empty());
        }

        for (i, function_definition) in function_definitions.into_iter().enumerate() {
            if matches!(function_definition.ty, solidity::FunctionTy::Modifier) {
                continue;
            }

            let (function, abi_function, impl_item) =
                translate_function_definition(self, module.clone(), &function_definition)?;

            assert!(abi_function.is_none());
            assert!(impl_item.is_none());

            module.borrow_mut().functions[functions_index + i].implementation = Some(function);
        }

        for (i, contract_definition) in contract_definitions.into_iter().enumerate() {
            module.borrow_mut().contracts[contracts_index + i].implementation = Some(
                translate_contract_definition(self, module.clone(), &contract_definition)?,
            );
        }

        Ok(())
    }

    fn generate_forc_project<P1: AsRef<Path>, P2: AsRef<Path>>(
        &mut self,
        output_directory: P1,
        source_unit_path: P2,
    ) -> Result<(), Error> {
        todo!()
    }
}
