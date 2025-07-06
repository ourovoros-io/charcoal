use crate::{cli::Args, error::Error, framework::Framework, ir, sway, translate::*, wrapped_err};
use convert_case::{Case, Casing};
use solang_parser::pt as solidity;
use std::{
    cell::RefCell,
    collections::HashMap,
    path::{Component, Path, PathBuf},
    rc::Rc,
};

pub struct Project {
    pub options: Args,
    pub contracts_path: PathBuf,
    pub framework: Framework,
    pub queue: Vec<PathBuf>,
    pub line_ranges: HashMap<PathBuf, Vec<(usize, usize)>>,
    pub solidity_source_units: HashMap<PathBuf, solidity::SourceUnit>,
    pub translated_modules: Vec<Rc<RefCell<ir::Module>>>,
}

impl Project {
    pub fn new(options: Args, framework: Framework) -> Result<Self, Error> {
        let contracts_path = Self::resolve_input_path(&options, &framework)?;

        let mut result = Self {
            options,
            contracts_path,
            framework,
            queue: vec![],
            line_ranges: HashMap::new(),
            solidity_source_units: HashMap::new(),
            translated_modules: vec![],
        };

        result.queue = result.create_usage_queue()?;

        Ok(result)
    }

    /// Resolve the input path based on the remappings of the [Framework]
    pub fn resolve_input_path(options: &Args, framework: &Framework) -> Result<PathBuf, Error> {
        let mut contracts_path = options.input.clone();

        match framework {
            Framework::Foundry { src_path, .. } => match src_path.as_ref() {
                Some(src_path) => contracts_path.extend(src_path),
                None => contracts_path.push("src"),
            },

            _ => contracts_path.push("contracts"),
        }

        if !contracts_path.exists() || !contracts_path.is_dir() {
            return Err(Error::Wrapped(Box::new(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                format!(
                    "Contracts folder should be a directory: {}",
                    contracts_path.to_string_lossy()
                ),
            ))));
        }

        Ok(contracts_path)
    }

    /// Resolve the import path based on the remappings of the [Framework]
    fn resolve_import_path(
        &self,
        source_unit_directory: &Path,
        filename: &str,
    ) -> Result<PathBuf, Error> {
        match &self.framework {
            // Remappings in foundry are handled using the same pattern
            Framework::Foundry { remappings, .. } => {
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

            // Remappings in hardhat are done using the @ symbol and the node_modules folder
            Framework::Hardhat => {
                if filename.starts_with('.') {
                    Ok(source_unit_directory.join(filename))
                } else if filename.starts_with('@') {
                    Ok(self.options.input.join("node_modules").join(filename))
                } else {
                    Ok(self.options.input.join(filename))
                }
            }
        }
    }

    /// Recursively search for .sol files in the given directory
    fn collect_source_unit_paths(
        path: &Path,
        framework: &Framework,
    ) -> Result<Vec<PathBuf>, Error> {
        let mut source_unit_paths = vec![];

        if let Framework::Hardhat = framework {
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
        let paths = Self::collect_source_unit_paths(&self.contracts_path, &self.framework)?;

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

                import_paths.insert(path.into(), paths.clone());

                for import_path in paths.iter() {
                    collect_imports(project, import_path, import_paths, import_counts);
                    *import_counts.entry(import_path.clone()).or_insert(0) += 1;
                }
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

                // If we have detected a framework we need to resolve the path based on the remappings if found
                import_path = self
                    .resolve_import_path(source_unit_path.parent().unwrap(), &import_path)?
                    .to_str()
                    .unwrap()
                    .to_string();

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
                .resolve_import_path(source_unit_directory, path_string)
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
    fn loc_to_line_and_column(
        &self,
        module: Rc<RefCell<ir::Module>>,
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

    #[inline]
    pub fn loc_to_file_location_string(
        &self,
        module: Rc<RefCell<ir::Module>>,
        loc: &solidity::Loc,
    ) -> String {
        match self.loc_to_line_and_column(module.clone(), loc) {
            Some((line, col)) => format!(
                "{}:{}:{}",
                self.options
                    .input
                    .join(module.borrow().path.clone())
                    .with_extension("sol")
                    .to_string_lossy(),
                line,
                col
            ),
            None => format!(
                "{}",
                self.options
                    .input
                    .join(module.borrow().path.clone())
                    .with_extension("sol")
                    .to_string_lossy()
            ),
        }
    }

    pub fn resolve_use(&mut self, use_expr: &sway::Use) -> Option<Rc<RefCell<ir::Module>>> {
        let sway::UseTree::Path { prefix, suffix } = &use_expr.tree else {
            return None;
        };

        // Only check crate-local imports
        if !prefix.is_empty() {
            return None;
        }

        let mut use_tree = suffix.as_ref().clone();
        let mut path = PathBuf::new();

        while let sway::UseTree::Path { prefix, suffix } = &use_tree {
            path.push(prefix);

            if let sway::UseTree::Glob = suffix.as_ref() {
                break;
            }

            use_tree = suffix.as_ref().clone();
        }

        path = PathBuf::from(path).with_extension("");
        let mut parent_module: Option<Rc<RefCell<ir::Module>>> = None;
        let mut first = true;

        for comp in path.components() {
            if let Component::RootDir = comp {
                continue;
            }

            let mut comp = comp
                .as_os_str()
                .to_string_lossy()
                .to_string()
                .replace(".", "_")
                .to_case(Case::Snake);

            if first {
                first = false;

                if let "lib" | "src" | "main" = comp.as_str() {
                    comp = format!("_{comp}");
                }
            }

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

    pub fn find_module(&mut self, path: &Path) -> Option<Rc<RefCell<ir::Module>>> {
        let path = PathBuf::from(path).with_extension("");
        let mut parent_module: Option<Rc<RefCell<ir::Module>>> = None;

        for comp in path.components() {
            if let Component::RootDir = comp {
                continue;
            }

            let mut comp = comp
                .as_os_str()
                .to_string_lossy()
                .to_string()
                .replace(".", "_")
                .to_case(Case::Snake);

            if let "lib" | "src" | "main" = comp.as_str() {
                comp = format!("_{comp}");
            }

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

    pub fn find_or_create_module(&mut self, path: &Path) -> Rc<RefCell<ir::Module>> {
        let mut current_path = PathBuf::new();

        let components = path
            .components()
            .filter(|c| !matches!(c, Component::RootDir))
            .collect::<Vec<_>>();
        current_path.push(components[0].clone());

        let mut component = components[0]
            .as_os_str()
            .to_string_lossy()
            .to_string()
            .replace(".", "_")
            .to_case(Case::Snake);

        if let "lib" | "src" | "main" = component.as_str() {
            component = format!("_{component}");
        }

        let mut parent_module = match self
            .translated_modules
            .iter()
            .find(|t| t.borrow().name == component)
        {
            Some(result) => result.clone(),
            None => {
                self.translated_modules
                    .push(Rc::new(RefCell::new(ir::Module {
                        name: component,
                        path: current_path.clone(),
                        ..Default::default()
                    })));

                self.translated_modules.last().cloned().unwrap()
            }
        };

        for component in &components[1..] {
            current_path.push(component.clone());
            let mut component = component
                .as_os_str()
                .to_string_lossy()
                .to_string()
                .replace(".", "_")
                .to_case(Case::Snake);

            if let "lib" | "src" | "main" = component.as_str() {
                component = format!("_{component}");
            }

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
                        .push(Rc::new(RefCell::new(ir::Module {
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

    pub fn is_contract_declared(
        &mut self,
        module: Rc<RefCell<ir::Module>>,
        contract_name: &str,
    ) -> bool {
        // Check to see if the contract was declared in the current file
        if module
            .borrow()
            .contracts
            .iter()
            .any(|c| c.signature.to_string() == contract_name)
        {
            return true;
        }

        // Check all of the module's `use` statements for crate-local imports,
        // find the module being imported, then check if the contract lives there.
        for use_item in module.borrow().uses.iter() {
            if let Some(found_module) = self.resolve_use(use_item)
                && found_module
                    .borrow()
                    .contracts
                    .iter()
                    .any(|c| c.signature.to_string() == contract_name)
            {
                return true;
            }
        }

        false
    }

    pub fn find_contract(
        &mut self,
        module: Rc<RefCell<ir::Module>>,
        contract_name: &str,
    ) -> Option<Rc<RefCell<ir::Contract>>> {
        // Check to see if the contract was defined in the current file
        if let Some(contract) = module
            .borrow()
            .contracts
            .iter()
            .find(|c| c.signature.to_string() == contract_name)
        {
            return contract.implementation.clone();
        }

        // Check all of the module's `use` statements for crate-local imports,
        // find the module being imported, then check if the contract lives there.
        for use_item in module.borrow().uses.iter() {
            if let Some(found_module) = self.resolve_use(use_item)
                && let Some(contract) = self.find_contract(found_module, contract_name)
            {
                return Some(contract.clone());
            }
        }

        None
    }

    pub fn find_module_and_contract(
        &mut self,
        module: Rc<RefCell<ir::Module>>,
        contract_name: &str,
    ) -> Option<(Rc<RefCell<ir::Module>>, Rc<RefCell<ir::Contract>>)> {
        // Check to see if the contract was defined in the current file
        if let Some(contract) = module
            .borrow()
            .contracts
            .iter()
            .find(|c| c.signature.to_string() == contract_name)
        {
            return Some((module.clone(), contract.implementation.clone().unwrap()));
        }

        // Check all of the module's `use` statements for crate-local imports,
        // find the module being imported, then check if the contract lives there.
        for use_item in module.borrow().uses.iter() {
            if let Some(found_module) = self.resolve_use(use_item)
                && let Some(contract) = found_module
                    .borrow()
                    .contracts
                    .iter()
                    .find(|c| c.signature.to_string() == contract_name)
            {
                return Some((
                    found_module.clone(),
                    contract.implementation.clone().unwrap(),
                ));
            }
        }

        None
    }

    pub fn find_module_containing_contract(
        &mut self,
        module: Rc<RefCell<ir::Module>>,
        contract_name: &str,
    ) -> Option<Rc<RefCell<ir::Module>>> {
        // Check to see if the contract was defined in the current file
        if module
            .borrow()
            .contracts
            .iter()
            .any(|c| c.signature.to_string() == contract_name)
        {
            return Some(module.clone());
        }

        // Check all of the module's `use` statements for crate-local imports,
        // find the module being imported, then check if the contract lives there.
        for use_item in module.borrow().uses.iter() {
            if let Some(found_module) = self.resolve_use(use_item)
                && found_module
                    .borrow()
                    .contracts
                    .iter()
                    .any(|c| c.signature.to_string() == contract_name)
            {
                return Some(found_module.clone());
            }
        }

        None
    }

    pub fn is_type_definition_declared(
        &mut self,
        module: Rc<RefCell<ir::Module>>,
        name: &str,
    ) -> bool {
        // Check to see if the type definition was defined in the current file
        if module
            .borrow()
            .type_definitions
            .iter()
            .any(|x| x.signature.to_string() == name)
        {
            return true;
        }

        // Check all of the module's `use` statements for crate-local imports,
        // find the module being imported, then check if the type definition lives there.
        for use_item in module.borrow().uses.iter() {
            if let Some(found_module) = self.resolve_use(use_item)
                && found_module
                    .borrow()
                    .type_definitions
                    .iter()
                    .any(|x| x.signature.to_string() == name)
            {
                return true;
            }
        }

        false
    }

    pub fn find_type_definition(
        &mut self,
        module: Rc<RefCell<ir::Module>>,
        name: &str,
    ) -> Option<sway::TypeDefinition> {
        // Check to see if the type definition was defined in the current file
        if let Some(x) = module
            .borrow()
            .type_definitions
            .iter()
            .find(|x| x.signature.to_string() == name)
        {
            return x.implementation.clone();
        }

        // Check all of the module's `use` statements for crate-local imports,
        // find the module being imported, then check if the type definition lives there.
        for use_item in module.borrow().uses.iter() {
            if let Some(found_module) = self.resolve_use(use_item)
                && let Some(x) = found_module
                    .borrow()
                    .type_definitions
                    .iter()
                    .find(|x| x.signature.to_string() == name)
            {
                return x.implementation.clone();
            }
        }

        None
    }

    pub fn is_enum_declared(&mut self, module: Rc<RefCell<ir::Module>>, name: &str) -> bool {
        // Check to see if the enum was defined in the current file
        if module
            .borrow()
            .enums
            .iter()
            .any(|x| x.signature.to_string() == name)
        {
            return true;
        }

        // Check all of the module's `use` statements for crate-local imports,
        // find the module being imported, then check if the enum lives there.
        for use_item in module.borrow().uses.iter() {
            if let Some(found_module) = self.resolve_use(use_item)
                && found_module
                    .borrow()
                    .enums
                    .iter()
                    .any(|x| x.signature.to_string() == name)
            {
                return true;
            }
        }

        false
    }

    pub fn find_enum(&mut self, module: Rc<RefCell<ir::Module>>, name: &str) -> Option<ir::Enum> {
        // Check to see if the enum was defined in the current file
        if let Some(x) = module
            .borrow()
            .enums
            .iter()
            .find(|x| x.signature.to_string() == name)
        {
            return x.implementation.clone();
        }

        // Check all of the module's `use` statements for crate-local imports,
        // find the module being imported, then check if the enum lives there.
        for use_item in module.borrow().uses.iter() {
            if let Some(found_module) = self.resolve_use(use_item)
                && let Some(x) = found_module
                    .borrow()
                    .enums
                    .iter()
                    .find(|x| x.signature.to_string() == name)
            {
                return x.implementation.clone();
            }
        }

        None
    }

    pub fn is_struct_declared(&mut self, module: Rc<RefCell<ir::Module>>, name: &str) -> bool {
        // Check to see if the struct was defined in the current file
        if module
            .borrow()
            .structs
            .iter()
            .any(|x| x.signature.to_string() == name)
        {
            return true;
        }

        // Check all of the module's `use` statements for crate-local imports,
        // find the module being imported, then check if the struct lives there.
        for use_item in module.borrow().uses.iter() {
            if let Some(found_module) = self.resolve_use(use_item)
                && found_module
                    .borrow()
                    .structs
                    .iter()
                    .any(|x| x.signature.to_string() == name)
            {
                return true;
            }
        }

        false
    }

    pub fn find_struct(
        &mut self,
        module: Rc<RefCell<ir::Module>>,
        scope: Rc<RefCell<ir::Scope>>,
        name: &str,
    ) -> Option<Rc<RefCell<sway::Struct>>> {
        // Check to see if the struct was defined in the current file
        if let Some(x) = module
            .borrow()
            .structs
            .iter()
            .find(|x| x.signature.to_string() == name)
        {
            return x.implementation.clone();
        }

        // Check to see if the struct is a storage struct
        if let Some(contract_name) = scope.borrow().get_contract_name()
            && let Some(contract) = self.find_contract(module.clone(), &contract_name)
            && let Some(x) = contract.borrow().storage_struct.as_ref()
            && x.borrow().name == name
        {
            return Some(x.clone());
        }

        // Check all of the module's `use` statements for crate-local imports,
        // find the module being imported, then check if the struct lives there.
        for use_item in module.borrow().uses.iter() {
            if let Some(found_module) = self.resolve_use(use_item)
                && let Some(x) = found_module
                    .borrow()
                    .structs
                    .iter()
                    .find(|x| x.signature.to_string() == name)
            {
                return x.implementation.clone();
            }
        }

        None
    }

    pub fn translate(&mut self) -> Result<(), Error> {
        for source_unit_path in self.queue.clone() {
            self.translate_file(&source_unit_path)?;
        }

        match self.options.output_directory.clone() {
            Some(_) => {
                self.generate_forc_project()?;
            }

            None => {
                for module in self.translated_modules.iter() {
                    println!(
                        "// Translated from {}",
                        module.borrow().path.to_string_lossy()
                    );

                    let module: sway::Module = module.borrow().clone().into();
                    println!("{}", sway::TabbedDisplayer(&module));
                }
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
            module.borrow_mut().type_definitions.push(ir::Item {
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
            module.borrow_mut().enums.push(ir::Item {
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
            module.borrow_mut().structs.push(ir::Item {
                signature: sway::TypeName::Identifier {
                    name: struct_definition.name.as_ref().unwrap().name.clone(),
                    generic_parameters: None,
                },
                implementation: None,
            })
        }

        // Collect the function signatures ahead of time
        for function_definition in function_definitions.iter() {
            if matches!(function_definition.ty, solidity::FunctionTy::Modifier) {
                continue;
            }

            let declaration =
                translate_function_declaration(self, module.clone(), None, function_definition)?;

            let sway::TypeName::Function { new_name, .. } = &declaration.type_name else {
                unreachable!()
            };

            for constructor_call in declaration.constructor_calls.iter() {
                module
                    .borrow_mut()
                    .function_constructor_calls
                    .entry(new_name.clone())
                    .or_default()
                    .push(constructor_call.clone());
            }

            for modifier in declaration.modifiers.iter() {
                module
                    .borrow_mut()
                    .function_modifiers
                    .entry(new_name.clone())
                    .or_default()
                    .push(modifier.clone());
            }

            module.borrow_mut().functions.push(ir::Item {
                signature: declaration.type_name,
                implementation: None,
            });
        }

        // Collect the contract signatures ahead of time
        let contracts_index = module.borrow().contracts.len();

        for contract_definition in contract_definitions.iter() {
            let contract_name = contract_definition.name.as_ref().unwrap().name.as_str();

            module.borrow_mut().contracts.push(ir::Item {
                signature: sway::TypeName::Identifier {
                    name: contract_name.into(),
                    generic_parameters: None,
                },
                implementation: None,
            });
        }

        // Translate toplevel variable definitions
        for variable_definition in variable_definitions {
            let state_variable_info =
                translate_state_variable(self, module.clone(), None, &variable_definition)?;
            assert!(state_variable_info.deferred_initializations.is_empty());
            assert!(state_variable_info.mapping_names.is_empty());
        }

        // Collect the contract's toplevel item signatures ahead of time
        for (i, contract_definition) in contract_definitions.iter().enumerate() {
            let contract_name = contract_definition.name.as_ref().unwrap().name.as_str();
            let mut inherits = vec![];

            for base in contract_definition.base.iter() {
                if base.args.as_ref().is_some() {
                    //
                    // TODO:
                    // set up a base constructor call
                    //

                    // print!("WARNING: Unused contract base constructor args: `{base}`; ");
                    // println!("This is for inherits and base constructor calls.")
                }

                assert!(base.name.identifiers.len() == 1);

                inherits.push(sway::TypeName::Identifier {
                    name: base.name.identifiers[0].name.clone(),
                    generic_parameters: None,
                });
            }

            let contract = Rc::new(RefCell::new(ir::Contract::new(
                contract_name,
                contract_definition.ty.clone(),
                inherits.as_slice(),
            )));

            let mut type_definitions = vec![];
            let mut enum_definitions = vec![];
            let mut struct_definitions = vec![];
            let mut function_definitions = vec![];
            let mut variable_definitions = vec![];

            for part in contract_definition.parts.iter() {
                match part {
                    solidity::ContractPart::TypeDefinition(type_definition) => {
                        type_definitions.push(type_definition.clone());
                    }

                    solidity::ContractPart::StructDefinition(struct_definition) => {
                        struct_definitions.push(struct_definition.clone())
                    }

                    solidity::ContractPart::EnumDefinition(enum_definition) => {
                        enum_definitions.push(enum_definition.clone())
                    }

                    solidity::ContractPart::FunctionDefinition(function_definition) => {
                        function_definitions.push(function_definition.clone());
                    }

                    solidity::ContractPart::VariableDefinition(variable_definition) => {
                        let is_constant = variable_definition
                            .attrs
                            .iter()
                            .any(|x| matches!(x, solidity::VariableAttribute::Constant(_)));

                        let is_immutable = variable_definition
                            .attrs
                            .iter()
                            .any(|x| matches!(x, solidity::VariableAttribute::Immutable(_)));

                        let is_configurable = is_immutable && !is_constant;

                        // Only translate constant and configurable variables ahead of time.
                        // Regular state variables are translated later.
                        if is_constant || is_configurable {
                            variable_definitions.push(variable_definition);
                        }
                    }

                    _ => {}
                }
            }

            // Collect the signatures of the contract type definitions
            let type_definitions_index = module.borrow().type_definitions.len();

            for type_definition in type_definitions.iter() {
                module.borrow_mut().type_definitions.push(ir::Item {
                    signature: sway::TypeName::Identifier {
                        name: type_definition.name.name.clone(),
                        generic_parameters: None,
                    },
                    implementation: None,
                });
            }

            // Collect the signatures of the contract enum definitions
            let enums_index = module.borrow().enums.len();

            for enum_definition in enum_definitions.iter() {
                module.borrow_mut().enums.push(ir::Item {
                    signature: sway::TypeName::Identifier {
                        name: enum_definition.name.as_ref().unwrap().name.clone(),
                        generic_parameters: None,
                    },
                    implementation: None,
                });
            }

            // Collect the signatures of the contract struct definitions
            let structs_index = module.borrow().structs.len();

            for struct_definition in struct_definitions.iter() {
                module.borrow_mut().structs.push(ir::Item {
                    signature: sway::TypeName::Identifier {
                        name: struct_definition.name.as_ref().unwrap().name.clone(),
                        generic_parameters: None,
                    },
                    implementation: None,
                });
            }

            // Translate contract constants and immutables
            let mut state_variable_infos = vec![];
            for variable_definition in variable_definitions.iter() {
                let state_variable_info = translate_state_variable(
                    self,
                    module.clone(),
                    Some(contract_name),
                    variable_definition,
                )?;

                assert!(state_variable_info.deferred_initializations.is_empty());
                assert!(state_variable_info.mapping_names.is_empty());

                state_variable_infos.push(state_variable_info);
            }

            let scope = Rc::new(RefCell::new(ir::Scope::new(
                Some(contract_name),
                None,
                None,
            )));

            for state_variable_info in state_variable_infos {
                let Some((abi_fn, toplevel_fn, impl_fn)) =
                    generate_state_variable_getter_functions(
                        self,
                        module.clone(),
                        scope.clone(),
                        Some(contract_name),
                        &state_variable_info,
                    )?
                else {
                    continue;
                };

                contract.borrow_mut().abi.functions.push(abi_fn);

                if let Some(function) = module
                    .borrow_mut()
                    .functions
                    .iter_mut()
                    .find(|f| f.signature == toplevel_fn.get_type_name())
                {
                    assert!(function.implementation.is_none());
                    function.implementation = Some(toplevel_fn);
                } else {
                    module.borrow_mut().functions.push(ir::Item {
                        signature: toplevel_fn.get_type_name(),
                        implementation: Some(toplevel_fn),
                    });
                }

                contract
                    .borrow_mut()
                    .abi_impl
                    .items
                    .push(sway::ImplItem::Function(impl_fn));
            }

            // Translate contract type definitions
            for (i, type_definition) in type_definitions.into_iter().enumerate() {
                module.borrow_mut().type_definitions[type_definitions_index + i].implementation =
                    Some(translate_type_definition(
                        self,
                        module.clone(),
                        Some(&contract_name),
                        type_definition.as_ref(),
                    )?);
            }

            // Translate contract enum definitions
            for (i, enum_definition) in enum_definitions.into_iter().enumerate() {
                module.borrow_mut().enums[enums_index + i].implementation = Some(
                    translate_enum_definition(self, module.clone(), &enum_definition)?,
                );
            }

            // Translate contract struct definitions
            for (i, struct_definition) in struct_definitions.into_iter().enumerate() {
                module.borrow_mut().structs[structs_index + i].implementation =
                    Some(translate_struct_definition(
                        self,
                        module.clone(),
                        Some(&contract_name),
                        struct_definition.as_ref(),
                    )?);
            }

            // Translate abi functions
            for function_definition in function_definitions {
                if matches!(function_definition.ty, solidity::FunctionTy::Modifier) {
                    continue;
                }

                if let Some(abi_fn) = translate_abi_function(
                    self,
                    module.clone(),
                    contract_name,
                    &function_definition,
                ) {
                    contract.borrow_mut().abi.functions.push(abi_fn);
                }

                // Only create a toplevel function if the function has a body
                if function_definition.body.is_none() {
                    continue;
                }

                let declaration = translate_function_declaration(
                    self,
                    module.clone(),
                    Some(&contract_name),
                    &function_definition,
                )?;

                let sway::TypeName::Function { new_name, .. } = &declaration.type_name else {
                    unreachable!()
                };

                for constructor_call in declaration.constructor_calls.iter() {
                    module
                        .borrow_mut()
                        .function_constructor_calls
                        .entry(new_name.clone())
                        .or_default()
                        .push(constructor_call.clone());
                }

                for modifier in declaration.modifiers.iter() {
                    module
                        .borrow_mut()
                        .function_modifiers
                        .entry(new_name.clone())
                        .or_default()
                        .push(modifier.clone());
                }

                module.borrow_mut().functions.push(ir::Item {
                    signature: declaration.type_name,
                    implementation: None,
                });
            }

            module.borrow_mut().contracts[contracts_index + i].implementation =
                Some(contract.clone());
        }

        for (i, type_definition) in type_definitions.into_iter().enumerate() {
            module.borrow_mut().type_definitions[type_definitions_index + i].implementation = Some(
                translate_type_definition(self, module.clone(), None, &type_definition)?,
            );
        }

        for (i, enum_definition) in enum_definitions.into_iter().enumerate() {
            module.borrow_mut().enums[enums_index + i].implementation = Some(
                translate_enum_definition(self, module.clone(), &enum_definition)?,
            );
        }

        for (i, struct_definition) in struct_definitions.into_iter().enumerate() {
            module.borrow_mut().structs[structs_index + i].implementation = Some(
                translate_struct_definition(self, module.clone(), None, &struct_definition)?,
            );
        }

        for event_definition in event_definitions {
            translate_event_definition(self, module.clone(), None, &event_definition)?;
        }

        for error_definition in error_definitions {
            translate_error_definition(self, module.clone(), None, &error_definition)?;
        }

        // Translate modifiers before functions
        for function_definition in function_definitions.iter() {
            if !matches!(function_definition.ty, solidity::FunctionTy::Modifier) {
                continue;
            }

            translate_modifier_definition(self, module.clone(), None, function_definition)?;
        }

        for function_definition in function_definitions {
            if matches!(function_definition.ty, solidity::FunctionTy::Modifier) {
                continue;
            }

            let (Some(function), impl_item) =
                translate_function_definition(self, module.clone(), None, &function_definition)?
            else {
                continue;
            };

            assert!(impl_item.is_none());

            let mut module = module.borrow_mut();

            let function_signature = sway::TypeName::Function {
                old_name: function.old_name.clone(),
                new_name: function.new_name.clone(),
                generic_parameters: function.generic_parameters.clone(),
                parameters: function.parameters.clone(),
                storage_struct_parameter: function.storage_struct_parameter.clone().map(Box::new),
                return_type: function.return_type.clone().map(Box::new),
            };

            let Some(function_entry) = module.functions.iter_mut().find(|f| {
                let sway::TypeName::Function { new_name, .. } = &f.signature else {
                    unreachable!()
                };

                *new_name == function.new_name
                    && f.signature.is_compatible_with(&function_signature)
            }) else {
                panic!(
                    "Failed to find function {} - {} - in list:\n{}",
                    function.new_name,
                    function_signature,
                    module
                        .functions
                        .iter()
                        .map(|f| {
                            let sway::TypeName::Function { new_name, .. } = &f.signature else {
                                unreachable!()
                            };

                            format!("    {} - {}", new_name, f.signature)
                        })
                        .collect::<Vec<_>>()
                        .join("\n"),
                );
            };

            function_entry.implementation = Some(function);
        }

        for (i, contract_definition) in contract_definitions.into_iter().enumerate() {
            let contract = module.borrow().contracts[contracts_index + i].clone();

            translate_contract_definition(
                self,
                module.clone(),
                &contract_definition,
                contract.implementation.clone().unwrap(),
            )?;
        }

        Ok(())
    }

    fn generate_forc_project(&mut self) -> Result<(), Error> {
        let mut modules: Vec<(PathBuf, sway::Module)> = vec![];
        let mut dependencies = vec![];

        fn process_submodules(
            dependencies: &mut Vec<String>,
            modules: &mut Vec<(PathBuf, sway::Module)>,
            module: Rc<RefCell<ir::Module>>,
        ) {
            let dirty_module_path = module.borrow().path.clone();

            let mut module_path = PathBuf::new();
            let mut first = true;

            for component in dirty_module_path.components() {
                let mut component = component
                    .as_os_str()
                    .to_string_lossy()
                    .to_string()
                    .replace(".", "_")
                    .to_case(Case::Snake);

                if first {
                    first = false;

                    if let "lib" | "src" | "main" = component.as_str() {
                        component = format!("_{component}");
                    }
                }

                module_path.push(component);
            }

            dependencies.extend(module.borrow().dependencies.clone());

            modules.push((
                module_path.with_extension("sw"),
                module.borrow().clone().into(),
            ));

            for submodule in module.borrow().submodules.iter() {
                process_submodules(dependencies, modules, submodule.clone());
            }
        }

        let mut lib_module = sway::Module {
            kind: sway::ModuleKind::Library,
            items: vec![],
        };

        for module in self.translated_modules.iter() {
            lib_module
                .items
                .push(sway::ModuleItem::Submodule(sway::Submodule {
                    is_public: true,
                    name: module.borrow().name.clone(),
                }));

            dependencies.extend(module.borrow().dependencies.clone());

            process_submodules(&mut dependencies, &mut modules, module.clone());
        }

        modules.push(("lib.sw".into(), lib_module));

        let output_directory = self
            .options
            .output_directory
            .clone()
            .unwrap()
            .join(self.options.name.clone().unwrap());

        wrapped_err!(std::fs::create_dir_all(&output_directory))?;

        let src_dir_path = output_directory.join("src");
        wrapped_err!(std::fs::create_dir_all(&src_dir_path))?;

        for (module_path, module) in modules {
            let module_path = src_dir_path.join(&module_path);
            wrapped_err!(std::fs::create_dir_all(&module_path.parent().unwrap()))?;

            std::fs::write(module_path, sway::TabbedDisplayer(&module).to_string())
                .map_err(|e| Error::Wrapped(Box::new(e)))?;
        }

        std::fs::write(
            output_directory.join(".gitignore"),
            "out\ntarget\nForc.lock\n",
        )
        .map_err(|e| Error::Wrapped(Box::new(e)))?;

        std::fs::write(
            output_directory.join("Forc.toml"),
            format!(
                "[project]\n\
                authors = [\"\"]\n\
                entry = \"main.sw\"\n\
                license = \"Apache-2.0\"\n\
                name = \"{}\"\n\
                \n\
                [dependencies]\n\
                {}\
                \n\
                ",
                self.options.name.as_ref().unwrap(),
                dependencies.join("\n"),
            ),
        )
        .map_err(|e| Error::Wrapped(Box::new(e)))?;

        Ok(())
    }
}
