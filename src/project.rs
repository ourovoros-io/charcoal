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
    pub standards: Rc<RefCell<HashMap<PathBuf, HashMap<String, Vec<Standard>>>>>,
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
            standards: Rc::new(RefCell::new(HashMap::new())),
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
                format!("Contracts folder should be a directory: {}", contracts_path.display()),
            ))));
        }

        Ok(contracts_path)
    }

    /// Resolve the import path based on the remappings of the [Framework]
    fn resolve_import_path(&self, source_unit_directory: &Path, filename: &str) -> Result<PathBuf, Error> {
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
    fn collect_source_unit_paths(path: &Path, framework: &Framework) -> Result<Vec<PathBuf>, Error> {
        let mut source_unit_paths = vec![];

        if let Framework::Hardhat = framework {
            // Skip the node_modules folder. Only translate things that are imported explicitly.
            if path.to_string_lossy().replace('\\', "/").contains("/node_modules/") {
                return Ok(vec![]);
            }
        }

        if !path.is_dir() {
            assert!(path.extension().unwrap() == "sol", "Only solidity files are supported.");

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
                    for source_unit_path in Self::collect_source_unit_paths(&path, framework)? {
                        if !source_unit_paths.contains(&source_unit_path) {
                            source_unit_paths.push(source_unit_path);
                        }
                    }
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

    fn get_import_paths(&mut self, ast: &solidity::SourceUnit, source_unit_path: &Path) -> Result<Vec<PathBuf>, Error> {
        let mut import_paths = Vec::new();
        if let Some(import_directives) = Self::get_contract_imports(ast) {
            for import_directive in &import_directives {
                let import_path = match import_directive {
                    solang_parser::pt::Import::Plain(import_path, _) => import_path,
                    solang_parser::pt::Import::GlobalSymbol(import_path, _, _) => import_path,
                    solang_parser::pt::Import::Rename(import_path, _, _) => import_path,
                };

                let import_path = match import_path {
                    solang_parser::pt::ImportPath::Filename(path) => std::path::PathBuf::from(path.to_string()),
                    solang_parser::pt::ImportPath::Path(path) => std::path::PathBuf::from(path.to_string()),
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

    pub fn canonicalize_import_path(&self, source_unit_directory: &Path, path_string: &str) -> Result<PathBuf, Error> {
        let mut import_path = PathBuf::from(path_string);

        if !import_path.to_string_lossy().starts_with('.') {
            import_path = self.resolve_import_path(source_unit_directory, path_string).unwrap();
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
    fn parse_solidity_source_unit<P: AsRef<Path>>(&mut self, path: P) -> Result<&solidity::SourceUnit, Error> {
        if self.solidity_source_units.contains_key(path.as_ref()) {
            return Ok(self.solidity_source_units.get(path.as_ref()).unwrap());
        }

        let path = wrapped_err!(path.as_ref().canonicalize())?;
        let source = wrapped_err!(std::fs::read_to_string(path.clone()))?;
        let line_ranges = self.load_line_ranges(&path, source.as_str());

        let (source_unit, _comments) = solang_parser::parse(source.as_str(), 0)
            .map_err(|e| Error::SolangDiagnostics(path.clone(), line_ranges.clone(), e))?;

        for part in source_unit.0.iter() {
            let solidity::SourceUnitPart::ContractDefinition(contract) = part else {
                continue;
            };

            let Some(contract_name) = contract.name.as_ref().map(|c| c.name.clone()) else {
                continue;
            };

            'standard_loop: for standard in STANDARDS.iter() {
                for standard_part in standard.parts.iter() {
                    let mut part_satisfied = false;
                    match standard_part {
                        StandardDefinitionPart::Function {
                            name,
                            arguments,
                            returns,
                        } => {
                            'contract_part_loop: for contract_part in contract.parts.iter() {
                                let solidity::ContractPart::FunctionDefinition(function) = contract_part else {
                                    continue;
                                };

                                let Some(function_name) = function.name.as_ref() else {
                                    continue;
                                };

                                if function_name.name != *name
                                    || function.params.len() != arguments.len()
                                    || function.returns.len() != returns.len()
                                {
                                    continue;
                                }

                                for (parameter, expected_type) in function.params.iter().zip(arguments.iter()) {
                                    let Some(parameter) = parameter.1.as_ref() else {
                                        continue 'contract_part_loop;
                                    };
                                    let solidity::Expression::Type(_, ty) = &parameter.ty else {
                                        panic!("unexpected parameter type expression type {:#?}", parameter.ty);
                                    };

                                    if ty != expected_type {
                                        continue 'contract_part_loop;
                                    }
                                }

                                for (parameter, expected_type) in function.returns.iter().zip(returns.iter()) {
                                    let Some(parameter) = parameter.1.as_ref() else {
                                        continue 'contract_part_loop;
                                    };
                                    let solidity::Expression::Type(_, ty) = &parameter.ty else {
                                        panic!("unexpected parameter type expression type {:#?}", parameter.ty);
                                    };

                                    if ty != expected_type {
                                        continue 'contract_part_loop;
                                    }
                                }

                                part_satisfied = true;
                                break;
                            }
                        }
                        StandardDefinitionPart::Event { name, arguments } => {
                            'contract_part_loop: for contract_part in contract.parts.iter() {
                                let solidity::ContractPart::EventDefinition(event) = contract_part else {
                                    continue;
                                };

                                let Some(event_name) = event.name.as_ref() else {
                                    continue;
                                };

                                if event_name.name != *name || event.fields.len() != arguments.len() {
                                    continue;
                                }

                                for (parameter, expected_type) in event.fields.iter().zip(arguments.iter()) {
                                    let solidity::Expression::Type(_, ty) = &parameter.ty else {
                                        panic!("unexpected parameter type expression type {:#?}", parameter.ty);
                                    };

                                    if ty != expected_type {
                                        continue 'contract_part_loop;
                                    }
                                }

                                part_satisfied = true;
                                break;
                            }
                        }
                    }
                    if !part_satisfied {
                        continue 'standard_loop;
                    }
                }

                self.standards
                    .borrow_mut()
                    .entry(path.clone())
                    .or_default()
                    .entry(contract_name.clone())
                    .or_default()
                    .push(standard.name);
            }
        }

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
                self.line_ranges.entry(path.clone()).or_default().push(line_range);
                line_range = (i + 1, 0);
            }
        }

        if line_range.1 > line_range.0 {
            self.line_ranges.entry(path.clone()).or_default().push(line_range);
        }

        self.line_ranges.get(path).unwrap()
    }

    #[inline]
    fn loc_to_line_and_column(&self, module: Rc<RefCell<ir::Module>>, loc: &solidity::Loc) -> Option<(usize, usize)> {
        let path = self
            .options
            .input
            .clone()
            .join(module.borrow().path.clone())
            .with_extension("sol");

        let line_ranges = self.line_ranges.get(&path)?;

        let start = match loc {
            solidity::Loc::Builtin | solidity::Loc::CommandLine | solidity::Loc::Implicit | solidity::Loc::Codegen => {
                return None;
            }

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
    pub fn loc_to_file_location_string(&self, module: Rc<RefCell<ir::Module>>, loc: &solidity::Loc) -> String {
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

        path = path.with_extension("");
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

            comp = check_for_reserved_keywords(&comp);

            match parent_module.clone() {
                Some(parent) => {
                    if let Some(module) = parent.borrow().submodules.iter().find(|s| s.borrow().name == comp) {
                        parent_module = Some(module.clone());
                    } else {
                        return None;
                    }
                }
                None => {
                    if let Some(module) = self.translated_modules.iter().find(|t| t.borrow().name == comp) {
                        parent_module = Some(module.clone());
                    } else {
                        return None;
                    }
                }
            }
        }

        parent_module
    }

    pub fn find_module(&self, path: &Path) -> Option<Rc<RefCell<ir::Module>>> {
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

            comp = check_for_reserved_keywords(&comp);

            match parent_module.clone() {
                Some(parent) => {
                    if let Some(module) = parent.borrow().submodules.iter().find(|s| s.borrow().name == comp) {
                        parent_module = Some(module.clone());
                    } else {
                        return None;
                    }
                }
                None => {
                    if let Some(module) = self.translated_modules.iter().find(|t| t.borrow().name == comp) {
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
        current_path.push(components[0]);

        let mut component = components[0]
            .as_os_str()
            .to_string_lossy()
            .to_string()
            .replace(".", "_")
            .to_case(Case::Snake);

        component = check_for_reserved_keywords(&component);

        let mut parent_module = match self.translated_modules.iter().find(|t| t.borrow().name == component) {
            Some(result) => result.clone(),
            None => {
                self.translated_modules.push(Rc::new(RefCell::new(ir::Module {
                    name: component,
                    path: current_path.clone(),
                    ..Default::default()
                })));

                self.translated_modules.last().cloned().unwrap()
            }
        };

        for component in &components[1..] {
            current_path.push(*component);
            let mut component = component
                .as_os_str()
                .to_string_lossy()
                .to_string()
                .replace(".", "_")
                .to_case(Case::Snake);

            component = check_for_reserved_keywords(&component);

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

    pub fn is_contract_declared(&mut self, module: Rc<RefCell<ir::Module>>, contract_name: &str) -> bool {
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
                && self.is_contract_declared(found_module.clone(), contract_name)
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
                && let Some(result) = self.find_module_and_contract(found_module, contract_name)
            {
                return Some(result);
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
                && let Some(result) = self.find_module_containing_contract(found_module, contract_name)
            {
                return Some(result);
            }
        }

        None
    }

    pub fn is_type_definition_declared(&mut self, module: Rc<RefCell<ir::Module>>, name: &str) -> bool {
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
                && self.is_type_definition_declared(found_module, name)
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
                && let Some(result) = self.find_type_definition(found_module, name)
            {
                return Some(result);
            }
        }

        None
    }

    pub fn is_enum_declared(&mut self, module: Rc<RefCell<ir::Module>>, name: &str) -> bool {
        // Check to see if the enum was defined in the current file
        if module.borrow().enums.iter().any(|x| x.signature.to_string() == name) {
            return true;
        }

        // Check all of the module's `use` statements for crate-local imports,
        // find the module being imported, then check if the enum lives there.
        for use_item in module.borrow().uses.iter() {
            if let Some(found_module) = self.resolve_use(use_item)
                && self.is_enum_declared(found_module.clone(), name)
            {
                return true;
            }
        }

        false
    }

    pub fn find_enum(&mut self, module: Rc<RefCell<ir::Module>>, name: &str) -> Option<ir::Enum> {
        // Check to see if the enum was defined in the current file
        if let Some(x) = module.borrow().enums.iter().find(|x| x.signature.to_string() == name) {
            return x.implementation.clone();
        }

        // Check all of the module's `use` statements for crate-local imports,
        // find the module being imported, then check if the enum lives there.
        for use_item in module.borrow().uses.iter() {
            if let Some(found_module) = self.resolve_use(use_item)
                && let Some(result) = self.find_enum(found_module, name)
            {
                return Some(result);
            }
        }

        None
    }

    pub fn is_struct_declared(
        &mut self,
        module: Rc<RefCell<ir::Module>>,
        scope: Rc<RefCell<ir::Scope>>,
        name: &str,
    ) -> bool {
        // Check to see if the struct was defined in the current file
        if module.borrow().structs.iter().any(|x| {
            let sway::TypeName::Identifier {
                name: struct_name,
                generic_parameters: None,
            } = &x.signature
            else {
                return false;
            };

            let storage_name = format!("Storage{}", struct_name);

            struct_name == name || storage_name == name
        }) {
            return true;
        }

        // Check to see if the struct is a storage struct
        let contract_name = scope.borrow().get_current_contract_name();
        if let Some(contract_name) = contract_name
            && let Some(contract) = self.find_contract(module.clone(), &contract_name)
            && let Some(x) = contract.borrow().storage_struct.as_ref()
            && x.borrow().name == name
        {
            return true;
        }

        // Check all of the module's `use` statements for crate-local imports,
        // find the module being imported, then check if the struct lives there.
        for use_item in module.borrow().uses.iter() {
            if let Some(found_module) = self.resolve_use(use_item)
                && self.is_struct_declared(found_module.clone(), scope.clone(), name)
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
    ) -> Option<Rc<RefCell<ir::Struct>>> {
        // Check to see if the struct was defined in the current file
        if let Some(x) = module.borrow().structs.iter().find(|x| {
            let Some(implementation) = x.implementation.as_ref() else {
                return false;
            };

            implementation.borrow().memory.name == name || implementation.borrow().storage.name == name
        }) {
            return x.implementation.clone();
        }

        // Check to see if the struct is a storage struct
        for contract in module.borrow().contracts.iter() {
            if let Some(contract) = contract.implementation.as_ref()
                && let Some(storage_struct) = contract.borrow().storage_struct.as_ref()
                && storage_struct.borrow().name == name
            {
                return Some(storage_struct.clone());
            }
        }

        // Check all of the module's `use` statements for crate-local imports,
        // find the module being imported, then check if the struct lives there.
        for use_item in module.borrow().uses.iter() {
            if let Some(found_module) = self.resolve_use(use_item)
                && let Some(struct_definition) = self.find_struct(found_module.clone(), scope.clone(), name)
            {
                return Some(struct_definition.clone());
            }
        }

        None
    }

    pub fn translate(&mut self) -> Result<(), Error> {
        for source_unit_path in self.queue.clone() {
            self.translate_file(&source_unit_path)?;
        }

        self.modify_contracts_for_sway_standards()?;

        match self.options.output_directory.clone() {
            Some(_) => {
                self.generate_forc_project()?;
            }

            None => {
                for module in self.translated_modules.iter() {
                    println!("// Translated from {}", module.borrow().path.to_string_lossy());

                    let module: sway::Module = module.borrow().clone().into();
                    println!("{}", sway::TabbedDisplayer(&module));
                }
            }
        }

        Ok(())
    }

    fn translate_file(&mut self, source_unit_path: &Path) -> Result<(), Error> {
        // println!("Translating \"{}\"", source_unit_path.display());

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
                solidity::SourceUnitPart::PragmaDirective(_, _, _) => {
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

        let mut sorted_contract_definitions = vec![];
        let mut sorted_inherited_definitions = vec![];

        for contract_definition in contract_definitions.iter() {
            for base in contract_definition.base.iter() {
                let inherited_name = base
                    .name
                    .identifiers
                    .iter()
                    .map(|i| i.name.clone())
                    .collect::<Vec<_>>()
                    .join(".");

                if let Some(contract) = contract_definitions
                    .iter()
                    .find(|c| c.name.as_ref().unwrap().name == inherited_name)
                {
                    sorted_inherited_definitions.push(contract.clone());
                }
            }
            if !sorted_inherited_definitions.contains(contract_definition) {
                sorted_contract_definitions.push(contract_definition.clone());
            }
        }

        contract_definitions.clear();

        for definition in sorted_inherited_definitions {
            if !contract_definitions.contains(&definition) {
                contract_definitions.push(definition);
            }
        }

        for definition in sorted_contract_definitions {
            if !contract_definitions.contains(&definition) {
                contract_definitions.push(definition);
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
                signature: sway::TypeName::create_identifier(type_definition.name.name.as_str()),
                implementation: None,
            });
        }

        // Collect the enum signatures ahead of time
        let enums_index = module.borrow().enums.len();

        for enum_definition in enum_definitions.iter() {
            module.borrow_mut().enums.push(ir::Item {
                signature: sway::TypeName::create_identifier(enum_definition.name.as_ref().unwrap().name.as_str()),
                implementation: None,
            });
        }

        // Collect the struct signatures ahead of time
        let structs_index = module.borrow().structs.len();

        for struct_definition in struct_definitions.iter() {
            module.borrow_mut().structs.push(ir::Item {
                signature: sway::TypeName::create_identifier(struct_definition.name.as_ref().unwrap().name.as_str()),
                implementation: None,
            });
        }

        // Collect the modifier signatures ahead of time
        for function_definition in function_definitions.iter() {
            if !matches!(function_definition.ty, solidity::FunctionTy::Modifier) {
                continue;
            }
            let function_name = translate_function_name(
                self,
                module.clone(),
                None,
                function_definition.name.as_ref().map(|n| n.name.as_str()),
                &function_definition.params,
                &function_definition.ty,
            );

            let parameters = translate_function_parameters(self, module.clone(), None, function_definition);

            module.borrow_mut().modifiers.push(ir::Item {
                signature: sway::TypeName::Function {
                    old_name: function_name.old_name,
                    new_name: function_name.abi_fn_name,
                    generic_parameters: None,
                    parameters,
                    storage_struct_parameter: None,
                    return_type: translate_return_type_name(
                        self,
                        module.clone(),
                        Rc::new(RefCell::new(ir::Scope::new(
                            Some(module.borrow().path.clone()),
                            None,
                            None,
                            None,
                        ))),
                        function_definition,
                    )
                    .map(Box::new),
                },
                implementation: None,
            })
        }

        // Collect the function signatures ahead of time
        for function_definition in function_definitions.iter() {
            if matches!(function_definition.ty, solidity::FunctionTy::Modifier) {
                continue;
            }

            let declaration = translate_function_declaration(self, module.clone(), None, None, function_definition)?;

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
                signature: sway::TypeName::create_identifier(contract_name),
                implementation: None,
            });
        }

        // Translate toplevel variable definitions
        for variable_definition in variable_definitions {
            let state_variable_info = translate_state_variable(self, module.clone(), None, &variable_definition)?;
            assert!(state_variable_info.deferred_initializations.is_empty());
            assert!(state_variable_info.mapping_names.is_empty());
        }

        for (i, type_definition) in type_definitions.into_iter().enumerate() {
            module.borrow_mut().type_definitions[type_definitions_index + i].implementation =
                Some(translate_type_definition(self, module.clone(), None, &type_definition)?);
        }

        for (i, enum_definition) in enum_definitions.into_iter().enumerate() {
            module.borrow_mut().enums[enums_index + i].implementation =
                Some(translate_enum_definition(self, module.clone(), &enum_definition)?);
        }

        for (i, struct_definition) in struct_definitions.into_iter().enumerate() {
            module.borrow_mut().structs[structs_index + i].implementation = Some(translate_struct_definition(
                self,
                module.clone(),
                None,
                &struct_definition,
            )?);
        }

        for event_definition in event_definitions {
            translate_event_definition(self, module.clone(), None, &event_definition)?;
        }

        for error_definition in error_definitions {
            translate_error_definition(self, module.clone(), None, &error_definition)?;
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

                inherits.push(sway::TypeName::create_identifier(
                    base.name.identifiers[0].name.as_str(),
                ));
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

            // Translate all contract enum definitions
            for enum_definition in enum_definitions.iter() {
                module.borrow_mut().enums.push(ir::Item {
                    signature: sway::TypeName::create_identifier(enum_definition.name.as_ref().unwrap().name.as_str()),
                    implementation: Some(translate_enum_definition(self, module.clone(), &enum_definition)?),
                });
            }

            // Collect the signatures of the contract type definitions
            let type_definitions_index = module.borrow().type_definitions.len();

            for type_definition in type_definitions.iter() {
                module.borrow_mut().type_definitions.push(ir::Item {
                    signature: sway::TypeName::create_identifier(type_definition.name.name.as_str()),
                    implementation: None,
                });
            }

            // Collect the signatures of the contract struct definitions
            let structs_index = module.borrow().structs.len();

            for struct_definition in struct_definitions.iter() {
                module.borrow_mut().structs.push(ir::Item {
                    signature: sway::TypeName::create_identifier(
                        struct_definition.name.as_ref().unwrap().name.as_str(),
                    ),
                    implementation: None,
                });
            }

            // Translate contract constants and immutables
            let mut state_variable_infos = vec![];

            for variable_definition in variable_definitions.iter() {
                let state_variable_info =
                    translate_state_variable(self, module.clone(), Some(contract_name), variable_definition)?;

                assert!(state_variable_info.deferred_initializations.is_empty());
                assert!(state_variable_info.mapping_names.is_empty());

                state_variable_infos.push(state_variable_info);
            }

            let scope = Rc::new(RefCell::new(ir::Scope::new(
                Some(module.borrow().path.clone()),
                Some(contract_name),
                None,
                None,
            )));

            for state_variable_info in state_variable_infos {
                let Some((abi_fn, toplevel_fn, impl_fn)) = generate_state_variable_getter_functions(
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
                    if let Some(function_implementation) = function.implementation.as_ref() {
                        assert!(*function_implementation == toplevel_fn);
                    } else {
                        assert!(function.implementation.is_none());
                        function.implementation = Some(toplevel_fn);
                    }
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
                module.borrow_mut().type_definitions[type_definitions_index + i].implementation = Some(
                    translate_type_definition(self, module.clone(), Some(contract_name), type_definition.as_ref())?,
                );
            }

            // Translate contract struct definitions
            for (i, struct_definition) in struct_definitions.into_iter().enumerate() {
                module.borrow_mut().structs[structs_index + i].implementation = Some(translate_struct_definition(
                    self,
                    module.clone(),
                    Some(contract_name),
                    struct_definition.as_ref(),
                )?);
            }

            // Translate abi modifiers
            for function_definition in function_definitions.iter() {
                if !matches!(function_definition.ty, solidity::FunctionTy::Modifier) {
                    continue;
                }

                let function_names = translate_function_name(
                    self,
                    module.clone(),
                    Some(contract_name),
                    function_definition.name.as_ref().map(|n| n.name.as_str()),
                    &function_definition.params,
                    &function_definition.ty,
                );

                let parameters =
                    translate_function_parameters(self, module.clone(), Some(contract_name), function_definition);

                module.borrow_mut().modifiers.push(ir::Item {
                    signature: sway::TypeName::Function {
                        old_name: function_names.old_name,
                        new_name: function_names.top_level_fn_name,
                        generic_parameters: None,
                        parameters,
                        storage_struct_parameter: None,
                        return_type: translate_return_type_name(
                            self,
                            module.clone(),
                            scope.clone(),
                            function_definition,
                        )
                        .map(Box::new),
                    },
                    implementation: None,
                })
            }

            // Translate abi functions
            for function_definition in function_definitions {
                if matches!(function_definition.ty, solidity::FunctionTy::Modifier) {
                    continue;
                }

                if let Some(abi_fn) = translate_abi_function(self, module.clone(), contract_name, &function_definition)
                {
                    contract.borrow_mut().abi.functions.push(abi_fn);
                }

                // Only create a toplevel function if the function has a body
                // if function_definition.body.is_none() {
                //     continue;
                // }

                let declaration = translate_function_declaration(
                    self,
                    module.clone(),
                    Some(contract_name),
                    Some(&contract_definition.ty),
                    &function_definition,
                )?;

                module.borrow_mut().functions.push(ir::Item {
                    signature: declaration.type_name,
                    implementation: None,
                });
            }

            module.borrow_mut().contracts[contracts_index + i].implementation = Some(contract.clone());
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

                *new_name == function.new_name && f.signature.is_compatible_with(&function_signature)
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

    fn modify_contracts_for_sway_standards(&mut self) -> Result<(), Error> {
        if self.standards.borrow().is_empty() {
            return Ok(());
        }

        for (solidity_path, contract_standards) in self.standards.borrow().iter() {
            for (contract_name, contract_standards) in contract_standards.iter() {
                for contract_standard in contract_standards.iter() {
                    let Some(standard_definition) = STANDARDS.iter().find(|s| s.name == *contract_standard) else {
                        panic!("Definition for {:?} not found", contract_standard)
                    };

                    match contract_standard {
                        Standard::ERC20 => {
                            let path = PathBuf::from(
                                solidity_path
                                    .to_string_lossy()
                                    .trim_start_matches(&self.options.input.to_string_lossy().to_string()),
                            )
                            .with_extension("");

                            let Some(module) = self.find_module(&path) else {
                                panic!("Module not found: {}", path.display(),);
                            };

                            let mut module = module.borrow_mut();

                            // ERC20 to SRC20
                            // 1. add dependencies to Forc.toml
                            // 2. take a look at the uses and add what we need to add to it.
                            // 3. implement the SRC20 implementation block for functions and events.

                            // src20 = "0.8.1"
                            module.ensure_dependency_declared("src20 = \"0.8.1\"");

                            // use src20::{SetDecimalsEvent, SetNameEvent, SetSymbolEvent, SRC20, TotalSupplyEvent};
                            module.ensure_use_declared("src20::SetDecimalsEvent");
                            module.ensure_use_declared("src20::SetNameEvent");
                            module.ensure_use_declared("src20::SetSymbolEvent");
                            module.ensure_use_declared("src20::TotalSupplyEvent");
                            module.ensure_use_declared("src20::SRC20");

                            let Some(contract) = module
                                .contracts
                                .iter_mut()
                                .find(|c| c.signature.to_string() == *contract_name)
                                .map(|c| c.implementation.as_ref().unwrap().clone())
                            else {
                                panic!("Contract not found: {}", contract_name);
                            };

                            // Remove from abi function signatures
                            let abi_fn_count = {
                                let contract = contract.borrow();
                                contract.abi.functions.len()
                            };

                            for i in (0..abi_fn_count).rev() {
                                let function = {
                                    let contract = contract.borrow();
                                    contract.abi.functions[i].clone()
                                };

                                let mut found = false;

                                for part in standard_definition.parts.iter() {
                                    let StandardDefinitionPart::Function { name, .. } = part else {
                                        continue;
                                    };

                                    if function.old_name == *name {
                                        found = true;
                                        break;
                                    }
                                }

                                if found {
                                    contract.borrow_mut().abi.functions.remove(i);
                                }
                            }

                            // Remove from abi implementation functions
                            let mut function_bodies = HashMap::new();

                            let abi_impl_item_count = {
                                let contract = contract.borrow();
                                contract.abi_impl.items.len()
                            };

                            for i in (0..abi_impl_item_count).rev() {
                                let item = {
                                    let contract = contract.borrow();
                                    contract.abi_impl.items[i].clone()
                                };

                                let sway::ImplItem::Function(function) = item else {
                                    continue;
                                };

                                let mut found = false;

                                for part in standard_definition.parts.iter() {
                                    let StandardDefinitionPart::Function { name, .. } = part else {
                                        continue;
                                    };

                                    if function.old_name == *name {
                                        found = true;
                                        break;
                                    }
                                }

                                if found {
                                    function_bodies
                                        .insert(function.old_name.clone(), function.body.as_ref().unwrap().clone());
                                    contract.borrow_mut().abi_impl.items.remove(i);
                                }
                            }

                            // Remove allowances
                            let Some(allowance_body) = function_bodies.get("allowance") else {
                                panic!("Allowance function body not found");
                            };

                            let Some(allowance_expr) = &allowance_body.final_expr else {
                                panic!("Failed to get allowance expression from final expression");
                            };

                            let sway::Expression::FunctionCall(f) = allowance_expr else {
                                panic!("Failed to get function call from allowance expression");
                            };

                            let Some(function_name) = f.function.as_identifier() else {
                                panic!("Failed to get the function name for allowance");
                            };

                            let Some((i, toplevel_fn)) = module
                                .functions
                                .iter()
                                .enumerate()
                                .find(|(_, f)| {
                                    let sway::TypeName::Function { new_name, .. } = &f.signature else {
                                        return false;
                                    };
                                    new_name == function_name
                                })
                                .map(|(i, f)| (i, f.clone()))
                            else {
                                panic!("Failed to get top level function");
                            };

                            let Some(allowance_expr) = toplevel_fn
                                .implementation
                                .as_ref()
                                .unwrap()
                                .body
                                .as_ref()
                                .unwrap()
                                .final_expr
                                .as_ref()
                            else {
                                panic!("Failed to get allowance expression from final expression")
                            };

                            let Some(expr) = allowance_expr.to_read_call_parts() else {
                                panic!(
                                    "Failed to get expression from read call parts of: {}",
                                    allowance_expr.display()
                                )
                            };

                            let Some((expr, _)) = expr.to_get_call_parts() else {
                                panic!("Failed to get expression from get call parts")
                            };

                            let Some((expr, _)) = expr.to_get_call_parts() else {
                                panic!("Failed to get expression from get call parts")
                            };

                            let sway::Expression::MemberAccess(member_access) = expr else {
                                panic!("Failed to get member access");
                            };

                            module.functions.remove(i);

                            let storage = contract.borrow().storage.as_ref().unwrap().clone();
                            let storage_struct = contract.borrow().storage_struct.as_ref().unwrap().clone();

                            let namespace_name = contract.borrow().name.to_case(Case::Snake);

                            let Some(storage_namespace) = storage
                                .borrow()
                                .namespaces
                                .iter()
                                .find(|n| n.borrow().name == namespace_name)
                                .cloned()
                            else {
                                panic!("Failed to get storage namespace")
                            };

                            let Some((i, _)) = storage_namespace
                                .borrow()
                                .fields
                                .iter()
                                .enumerate()
                                .find(|(_, f)| f.name == member_access.member)
                            else {
                                panic!("Failed to find storage field")
                            };

                            storage_namespace.borrow_mut().fields.remove(i);

                            let Some((i, _)) = storage_struct
                                .borrow()
                                .storage
                                .fields
                                .iter()
                                .enumerate()
                                .find(|(_, f)| f.new_name == member_access.member)
                            else {
                                panic!("Failed to find storage struct field")
                            };

                            storage_struct.borrow_mut().storage.fields.remove(i);

                            // impl SRC20 for Contract
                            contract.borrow_mut().impls.push(sway::Impl {
                                generic_parameters: None,
                                type_name: sway::TypeName::create_identifier("SRC20"),
                                for_type_name: Some(sway::TypeName::create_identifier("Contract")),
                                items: vec![
                                    // #[storage(read)]
                                    // fn total_assets() -> u64 {
                                    //     1
                                    // }
                                    sway::ImplItem::Function(sway::Function {
                                        attributes: Some(sway::AttributeList {
                                            attributes: vec![sway::Attribute {
                                                name: "storage".to_string(),
                                                parameters: Some(vec!["read".to_string()]),
                                            }],
                                        }),
                                        is_public: false,
                                        old_name: String::new(),
                                        new_name: "total_assets".to_string(),
                                        generic_parameters: None,
                                        parameters: sway::ParameterList { entries: vec![] },
                                        storage_struct_parameter: None,
                                        return_type: Some(sway::TypeName::create_identifier("u64")),
                                        body: Some(sway::Block {
                                            statements: vec![],
                                            final_expr: Some(sway::Expression::create_dec_int_literal(
                                                1_u8.into(),
                                                None,
                                            )),
                                        }),
                                    }),
                                    // #[storage(read)]
                                    // fn total_supply(asset: AssetId) -> Option<u64> {
                                    //     if asset == AssetId::default() {
                                    //         Some(storage.total_supply.read())
                                    //     } else {
                                    //         None
                                    //     }
                                    // }
                                    sway::ImplItem::Function(sway::Function {
                                        attributes: Some(sway::AttributeList {
                                            attributes: vec![sway::Attribute {
                                                name: "storage".to_string(),
                                                parameters: Some(vec!["read".to_string()]),
                                            }],
                                        }),
                                        is_public: false,
                                        old_name: String::new(),
                                        new_name: "total_supply".to_string(),
                                        generic_parameters: None,
                                        parameters: sway::ParameterList {
                                            entries: vec![sway::Parameter {
                                                is_ref: false,
                                                is_mut: false,
                                                name: "asset".to_string(),
                                                type_name: Some(sway::TypeName::create_identifier("AssetId")),
                                            }],
                                        },
                                        storage_struct_parameter: None,
                                        return_type: Some(sway::TypeName::create_identifier("u64").to_option()),
                                        body: Some(sway::Block {
                                            statements: vec![],
                                            final_expr: Some(sway::Expression::from(sway::If {
                                                condition: Some(sway::Expression::from(sway::BinaryExpression {
                                                    operator: "==".to_string(),
                                                    lhs: sway::Expression::create_identifier("asset"),
                                                    rhs: sway::Expression::create_function_call(
                                                        "AssetId::default",
                                                        None,
                                                        vec![],
                                                    ),
                                                })),
                                                then_body: sway::Block {
                                                    statements: vec![],
                                                    final_expr: Some(sway::Expression::create_function_call(
                                                        "Some",
                                                        None,
                                                        vec![
                                                            sway::Expression::create_function_call(
                                                                "u64::try_from",
                                                                None,
                                                                vec![
                                                                    function_bodies
                                                                        .get("totalSupply")
                                                                        .unwrap()
                                                                        .clone()
                                                                        .into(),
                                                                ],
                                                            )
                                                            .with_unwrap_call(),
                                                        ],
                                                    )),
                                                },
                                                else_if: Some(Box::new(sway::If {
                                                    condition: None,
                                                    then_body: sway::Block {
                                                        statements: vec![],
                                                        final_expr: Some(sway::Expression::create_identifier("None")),
                                                    },
                                                    else_if: None,
                                                })),
                                            })),
                                        }),
                                    }),
                                    // #[storage(read)]
                                    // fn name(asset: AssetId) -> Option<String> {
                                    //     if asset == AssetId::default() {
                                    //         Some(storage.name.read_slice().unwrap())
                                    //     } else {
                                    //         None
                                    //     }
                                    // }
                                    sway::ImplItem::Function(sway::Function {
                                        attributes: Some(sway::AttributeList {
                                            attributes: vec![sway::Attribute {
                                                name: "storage".to_string(),
                                                parameters: Some(vec!["read".to_string()]),
                                            }],
                                        }),
                                        is_public: false,
                                        old_name: String::new(),
                                        new_name: "name".to_string(),
                                        generic_parameters: None,
                                        parameters: sway::ParameterList {
                                            entries: vec![sway::Parameter {
                                                is_ref: false,
                                                is_mut: false,
                                                name: "asset".to_string(),
                                                type_name: Some(sway::TypeName::create_identifier("AssetId")),
                                            }],
                                        },
                                        storage_struct_parameter: None,
                                        return_type: Some(sway::TypeName::create_identifier("String").to_option()),
                                        body: Some(sway::Block {
                                            statements: vec![],
                                            final_expr: Some(sway::Expression::from(sway::If {
                                                condition: Some(sway::Expression::from(sway::BinaryExpression {
                                                    operator: "==".to_string(),
                                                    lhs: sway::Expression::create_identifier("asset"),
                                                    rhs: sway::Expression::create_function_call(
                                                        "AssetId::default",
                                                        None,
                                                        vec![],
                                                    ),
                                                })),
                                                then_body: sway::Block {
                                                    statements: vec![],
                                                    final_expr: Some(sway::Expression::create_function_call(
                                                        "Some",
                                                        None,
                                                        vec![function_bodies.get("name").unwrap().clone().into()],
                                                    )),
                                                },
                                                else_if: Some(Box::new(sway::If {
                                                    condition: None,
                                                    then_body: sway::Block {
                                                        statements: vec![],
                                                        final_expr: Some(sway::Expression::create_identifier("None")),
                                                    },
                                                    else_if: None,
                                                })),
                                            })),
                                        }),
                                    }),
                                    // #[storage(read)]
                                    // fn symbol(asset: AssetId) -> Option<String> {
                                    //     if asset == AssetId::default() {
                                    //         Some(storage.symbol.read_slice().unwrap())
                                    //     } else {
                                    //         None
                                    //     }
                                    // }
                                    sway::ImplItem::Function(sway::Function {
                                        attributes: Some(sway::AttributeList {
                                            attributes: vec![sway::Attribute {
                                                name: "storage".to_string(),
                                                parameters: Some(vec!["read".to_string()]),
                                            }],
                                        }),
                                        is_public: false,
                                        old_name: String::new(),
                                        new_name: "symbol".to_string(),
                                        generic_parameters: None,
                                        parameters: sway::ParameterList {
                                            entries: vec![sway::Parameter {
                                                is_ref: false,
                                                is_mut: false,
                                                name: "asset".to_string(),
                                                type_name: Some(sway::TypeName::create_identifier("AssetId")),
                                            }],
                                        },
                                        storage_struct_parameter: None,
                                        return_type: Some(sway::TypeName::create_identifier("String").to_option()),
                                        body: Some(sway::Block {
                                            statements: vec![],
                                            final_expr: Some(sway::Expression::from(sway::If {
                                                condition: Some(sway::Expression::from(sway::BinaryExpression {
                                                    operator: "==".to_string(),
                                                    lhs: sway::Expression::create_identifier("asset"),
                                                    rhs: sway::Expression::create_function_call(
                                                        "AssetId::default",
                                                        None,
                                                        vec![],
                                                    ),
                                                })),
                                                then_body: sway::Block {
                                                    statements: vec![],
                                                    final_expr: Some(sway::Expression::create_function_call(
                                                        "Some",
                                                        None,
                                                        vec![function_bodies.get("symbol").unwrap().clone().into()],
                                                    )),
                                                },
                                                else_if: Some(Box::new(sway::If {
                                                    condition: None,
                                                    then_body: sway::Block {
                                                        statements: vec![],
                                                        final_expr: Some(sway::Expression::create_identifier("None")),
                                                    },
                                                    else_if: None,
                                                })),
                                            })),
                                        }),
                                    }),
                                    // #[storage(read)]
                                    // fn decimals(asset: AssetId) -> Option<u8> {
                                    //     if asset == AssetId::default() {
                                    //         Some(storage.decimals.read())
                                    //     } else {
                                    //         None
                                    //     }
                                    // }
                                    sway::ImplItem::Function(sway::Function {
                                        attributes: Some(sway::AttributeList {
                                            attributes: vec![sway::Attribute {
                                                name: "storage".to_string(),
                                                parameters: Some(vec!["read".to_string()]),
                                            }],
                                        }),
                                        is_public: false,
                                        old_name: String::new(),
                                        new_name: "decimals".to_string(),
                                        generic_parameters: None,
                                        parameters: sway::ParameterList {
                                            entries: vec![sway::Parameter {
                                                is_ref: false,
                                                is_mut: false,
                                                name: "asset".to_string(),
                                                type_name: Some(sway::TypeName::create_identifier("AssetId")),
                                            }],
                                        },
                                        storage_struct_parameter: None,
                                        return_type: Some(sway::TypeName::create_identifier("u8").to_option()),
                                        body: Some(sway::Block {
                                            statements: vec![],
                                            final_expr: Some(sway::Expression::from(sway::If {
                                                condition: Some(sway::Expression::from(sway::BinaryExpression {
                                                    operator: "==".to_string(),
                                                    lhs: sway::Expression::create_identifier("asset"),
                                                    rhs: sway::Expression::create_function_call(
                                                        "AssetId::default",
                                                        None,
                                                        vec![],
                                                    ),
                                                })),
                                                then_body: sway::Block {
                                                    statements: vec![],
                                                    final_expr: Some(sway::Expression::create_function_call(
                                                        "Some",
                                                        None,
                                                        vec![function_bodies.get("decimals").unwrap().clone().into()],
                                                    )),
                                                },
                                                else_if: Some(Box::new(sway::If {
                                                    condition: None,
                                                    then_body: sway::Block {
                                                        statements: vec![],
                                                        final_expr: Some(sway::Expression::create_identifier("None")),
                                                    },
                                                    else_if: None,
                                                })),
                                            })),
                                        }),
                                    }),
                                ],
                            });
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn generate_forc_project(&mut self) -> Result<(), Error> {
        fn process_submodules(
            options: &Args,
            dependencies: &mut Vec<String>,
            modules: &mut Vec<(PathBuf, sway::Module)>,
            contract_modules: &mut Vec<ir::Module>,
            module: Rc<RefCell<ir::Module>>,
        ) {
            let mut module = module.borrow_mut();

            // Construct the module path
            let dirty_module_path = module.path.clone();

            let mut module_path = PathBuf::new();

            for component in dirty_module_path.components() {
                let mut component = component
                    .as_os_str()
                    .to_string_lossy()
                    .to_string()
                    .replace(".", "_")
                    .to_case(Case::Snake);

                component = check_for_reserved_keywords(&component);

                module_path.push(component);
            }

            // Extend the shared library project's dependencies
            for dependency in module.dependencies.iter() {
                if !dependencies.contains(dependency) {
                    dependencies.push(dependency.clone());
                }
            }

            // Construct the contract's shared library use tree
            let mut use_tree = sway::UseTree::Glob;

            for component in module.path.components().rev() {
                match component {
                    std::path::Component::Prefix(_) => continue,
                    std::path::Component::RootDir => continue,
                    std::path::Component::CurDir => continue,

                    std::path::Component::ParentDir => {
                        // Discard the prefix of the use tree
                        let sway::UseTree::Path { suffix, .. } = use_tree else {
                            panic!("Malformed import path: {:#?}", module.path)
                        };

                        use_tree = *suffix;
                    }

                    std::path::Component::Normal(name) => {
                        let mut name = name
                            .to_string_lossy()
                            .to_string()
                            .replace(".", "_")
                            .to_case(Case::Snake);

                        name = check_for_reserved_keywords(&name);

                        use_tree = sway::UseTree::Path {
                            prefix: name,
                            suffix: Box::new(use_tree),
                        };
                    }
                }
            }

            assert!(
                use_tree != sway::UseTree::Glob,
                "Invalid module path: {:#?}",
                module.path
            );

            use_tree = sway::UseTree::Path {
                prefix: options.name.clone().unwrap(),
                suffix: Box::new(use_tree),
            };

            // Construct the contract project's dependencies
            let mut contract_dependencies = module.dependencies.clone();

            let shared_dependency = format!(
                "{} = {{ path = \"../../{}\" }}",
                options.name.clone().unwrap(),
                options.name.clone().unwrap()
            );

            if !contract_dependencies.contains(&shared_dependency) {
                contract_dependencies.push(shared_dependency);
            }

            // Construct the contract main module's use statements
            let mut uses = module.uses.iter().filter(|u| !u.is_public).cloned().collect::<Vec<_>>();

            uses.push(sway::Use {
                is_public: true,
                tree: use_tree.clone(),
            });

            let configurable = module.configurable.take();

            // Move the storage and abi impls out of the contract IR and into the main modules of the new contract projects
            for contract in module.contracts.iter_mut() {
                let contract_signature = contract.signature.clone();
                let mut contract = contract.implementation.as_mut().unwrap().borrow_mut();

                // Don't create projects for libraries
                if let solidity::ContractTy::Library(_) = contract.kind {
                    continue;
                }

                let contract_project_name = contract.name.to_case(Case::Snake);

                let output_directory = options
                    .output_directory
                    .clone()
                    .unwrap()
                    .join("contracts")
                    .join(contract_project_name.clone());

                let src_dir_path = output_directory.join("src");
                let module_path = src_dir_path.join("main.sw");

                let storage = contract.storage.take();

                let abi_impl = contract.abi_impl.clone();

                let impls = contract.impls.clone();

                contract_modules.push(ir::Module {
                    name: contract_project_name,
                    path: module_path.clone(),
                    dependencies: contract_dependencies.clone(),
                    uses: uses.clone(),
                    configurable: configurable.clone(),
                    contracts: vec![ir::Item {
                        signature: contract_signature,
                        implementation: Some(Rc::new(RefCell::new(ir::Contract {
                            storage,
                            abi_impl,
                            impls,
                            storage_struct: None,
                            ..ir::Contract::new(
                                contract.name.as_str(),
                                contract.kind.clone(),
                                contract.abi.inherits.as_slice(),
                            )
                        }))),
                    }],
                    functions: contract
                        .storage_struct_constructor_fn
                        .as_ref()
                        .map(|s| {
                            vec![ir::Item {
                                signature: sway::TypeName::Function {
                                    old_name: s.old_name.clone(),
                                    new_name: s.new_name.clone(),
                                    generic_parameters: s.generic_parameters.clone(),
                                    parameters: s.parameters.clone(),
                                    storage_struct_parameter: s.storage_struct_parameter.clone().map(Box::new),
                                    return_type: s.return_type.clone().map(Box::new),
                                },
                                implementation: Some(s.clone()),
                            }]
                        })
                        .unwrap_or_default(),
                    ..Default::default()
                });
            }

            for contract_module in contract_modules.iter_mut() {
                // If the contract has a fallback function remove fallback attribute from the shared code
                // and add a fallback function on the contract and call the shared fallback function
                let fallback_function_name = format!("{}_fallback", contract_module.name).to_case(Case::Snake);

                if let Some(fallback_function) = module.functions.iter_mut().find(|f| {
                    let sway::TypeName::Function { new_name, .. } = &f.signature else {
                        unreachable!()
                    };

                    *new_name == fallback_function_name
                }) {
                    let fallback_function_signature = fallback_function.signature.clone();
                    let fallback_function = fallback_function.implementation.as_mut().unwrap();
                    let fallback_function_attributes = fallback_function.attributes.clone();

                    let Some(attributes) = fallback_function.attributes.as_mut() else {
                        continue;
                    };

                    let Some((index, _)) = attributes
                        .attributes
                        .iter()
                        .enumerate()
                        .find(|(_, a)| a.name == "fallback")
                    else {
                        continue;
                    };

                    attributes.attributes.remove(index);

                    let mut statements = vec![];
                    let mut parameters = vec![];

                    if contract_module
                        .contracts
                        .last()
                        .unwrap()
                        .implementation
                        .as_ref()
                        .unwrap()
                        .borrow()
                        .storage
                        .is_some()
                    {
                        statements.push(sway::Statement::from(sway::Let {
                            pattern: sway::LetPattern::from(sway::LetIdentifier {
                                is_mutable: false,
                                name: "storage_struct".to_string(),
                            }),
                            type_name: None,
                            value: sway::Expression::create_function_call(
                                format!("create_{}_storage_struct", contract_module.name).as_str(),
                                None,
                                vec![],
                            ),
                        }));

                        parameters.push(sway::Expression::create_identifier("storage_struct"));
                    }

                    contract_module.functions.push(ir::Item {
                        signature: fallback_function_signature,
                        implementation: Some(sway::Function {
                            attributes: fallback_function_attributes,
                            is_public: false,
                            old_name: String::new(),
                            new_name: "fallback".to_string(),
                            generic_parameters: None,
                            parameters: sway::ParameterList::default(),
                            storage_struct_parameter: None,
                            return_type: None,
                            body: Some(sway::Block {
                                statements,
                                final_expr: Some(sway::Expression::create_function_call(
                                    fallback_function_name.as_str(),
                                    None,
                                    parameters,
                                )),
                            }),
                        }),
                    })
                }
            }

            let submodules = module.submodules.clone();
            let mut module = sway::Module::from(module.clone());
            module.kind = sway::ModuleKind::Library;

            let mut remove_indices = vec![];

            for (i, item) in module.items.iter().enumerate() {
                if let sway::ModuleItem::Impl(abi_impl) = item
                    && let Some(for_type_name) = abi_impl.for_type_name.as_ref()
                    && for_type_name.to_string() == "Contract"
                {
                    remove_indices.push(i);
                }
            }

            for i in remove_indices.iter().rev() {
                module.items.remove(*i);
            }

            modules.push((module_path.with_extension("sw"), module));

            for submodule in submodules.iter() {
                process_submodules(options, dependencies, modules, contract_modules, submodule.clone());
            }
        }

        // Process all of the modules in the project from the top down
        let mut lib_module = sway::Module {
            kind: sway::ModuleKind::Library,
            items: vec![],
        };

        let mut shared_dependencies = vec![];
        let mut modules = vec![];
        let mut contract_modules = vec![];

        for module in self.translated_modules.iter() {
            lib_module.items.push(sway::ModuleItem::Submodule(sway::Submodule {
                is_public: true,
                name: module.borrow().name.clone(),
            }));

            for dependency in module.borrow().dependencies.iter() {
                if !shared_dependencies.contains(dependency) {
                    shared_dependencies.push(dependency.clone());
                }
            }

            process_submodules(
                &self.options,
                &mut shared_dependencies,
                &mut modules,
                &mut contract_modules,
                module.clone(),
            );
        }

        modules.push(("lib.sw".into(), lib_module));

        // Generate the shared library project
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
            wrapped_err!(std::fs::create_dir_all(module_path.parent().unwrap()))?;

            std::fs::write(module_path, sway::TabbedDisplayer(&module).to_string())
                .map_err(|e| Error::Wrapped(Box::new(e)))?;
        }

        std::fs::write(output_directory.join(".gitignore"), "out\ntarget\nForc.lock\n")
            .map_err(|e| Error::Wrapped(Box::new(e)))?;

        std::fs::write(
            output_directory.join("Forc.toml"),
            format!(
                "[project]\n\
                authors = [\"\"]\n\
                entry = \"lib.sw\"\n\
                license = \"Apache-2.0\"\n\
                name = \"{}\"\n\
                \n\
                [dependencies]\n\
                {}\
                \n\
                ",
                self.options.name.as_ref().unwrap(),
                shared_dependencies.join("\n"),
            ),
        )
        .map_err(|e| Error::Wrapped(Box::new(e)))?;

        // Generate each of the contract projects
        for module in contract_modules {
            let contract_project_name = module.name.clone();

            // Ensure the output directories exist
            let output_directory = self
                .options
                .output_directory
                .clone()
                .unwrap()
                .join("contracts")
                .join(contract_project_name.clone());

            std::fs::create_dir_all(&output_directory).unwrap();
            std::fs::create_dir_all(module.path.parent().unwrap()).unwrap();

            // Write the project's `.gitignore` file
            std::fs::write(output_directory.join(".gitignore"), "out\ntarget\nForc.lock\n")
                .map_err(|e| Error::Wrapped(Box::new(e)))
                .unwrap();

            // Write the project's `Forc.toml` file
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
                    contract_project_name,
                    module.dependencies.join("\n"),
                ),
            )
            .map_err(|e| Error::Wrapped(Box::new(e)))
            .unwrap();

            // Write the project's `main.sw` file
            let module_path = module.path.clone();
            let mut module = sway::Module::from(module);
            module.kind = sway::ModuleKind::Contract;
            let mut remove_indices = vec![];

            for (i, item) in module.items.iter().enumerate() {
                if let sway::ModuleItem::Abi(_) = item {
                    remove_indices.push(i);
                }
            }

            for i in remove_indices.iter().rev() {
                module.items.remove(*i);
            }

            std::fs::write(module_path, sway::TabbedDisplayer(&{ module }).to_string())
                .map_err(|e| Error::Wrapped(Box::new(e)))
                .unwrap();
        }

        Ok(())
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Standard {
    ERC20,
}

#[derive(Debug)]
pub struct StandardDefinition {
    pub name: Standard,
    pub parts: &'static [StandardDefinitionPart],
}

#[derive(Debug)]
pub enum StandardDefinitionPart {
    Function {
        name: &'static str,
        arguments: &'static [solidity::Type],
        returns: &'static [solidity::Type],
    },
    Event {
        name: &'static str,
        arguments: &'static [solidity::Type],
    },
}

pub const STANDARDS: &[StandardDefinition] = &[
    // ERC20
    StandardDefinition {
        name: Standard::ERC20,
        parts: &[
            // Functions
            // function name() public view returns (string);
            StandardDefinitionPart::Function {
                name: "name",
                arguments: &[],
                returns: &[solidity::Type::String],
            },
            // function symbol() public view returns (string);
            StandardDefinitionPart::Function {
                name: "symbol",
                arguments: &[],
                returns: &[solidity::Type::String],
            },
            // function decimals() public view returns (uint8);
            StandardDefinitionPart::Function {
                name: "decimals",
                arguments: &[],
                returns: &[solidity::Type::Uint(8)],
            },
            // function totalSupply() public view returns (uint256);
            StandardDefinitionPart::Function {
                name: "totalSupply",
                arguments: &[],
                returns: &[solidity::Type::Uint(256)],
            },
            // function balanceOf(address _owner) public view returns (uint256 balance);
            StandardDefinitionPart::Function {
                name: "balanceOf",
                arguments: &[solidity::Type::Address],
                returns: &[solidity::Type::Uint(256)],
            },
            // function transfer(address _to, uint256 _value) public returns (bool success);
            StandardDefinitionPart::Function {
                name: "transfer",
                arguments: &[solidity::Type::Address, solidity::Type::Uint(256)],
                returns: &[solidity::Type::Bool],
            },
            // function transferFrom(address _from, address _to, uint256 _value) public returns (bool success);
            StandardDefinitionPart::Function {
                name: "transferFrom",
                arguments: &[
                    solidity::Type::Address,
                    solidity::Type::Address,
                    solidity::Type::Uint(256),
                ],
                returns: &[solidity::Type::Bool],
            },
            // function approve(address _spender, uint256 _value) public returns (bool success);
            StandardDefinitionPart::Function {
                name: "approve",
                arguments: &[solidity::Type::Address, solidity::Type::Uint(256)],
                returns: &[solidity::Type::Bool],
            },
            // function allowance(address _owner, address _spender) public view returns (uint256 remaining);
            StandardDefinitionPart::Function {
                name: "allowance",
                arguments: &[solidity::Type::Address, solidity::Type::Address],
                returns: &[solidity::Type::Uint(256)],
            },
            // Events
            // event Transfer(address indexed _from, address indexed _to, uint256 _value);
            StandardDefinitionPart::Event {
                name: "Transfer",
                arguments: &[
                    solidity::Type::Address,
                    solidity::Type::Address,
                    solidity::Type::Uint(256),
                ],
            },
            // event Approval(address indexed _owner, address indexed _spender, uint256 _value);
            StandardDefinitionPart::Event {
                name: "Approval",
                arguments: &[
                    solidity::Type::Address,
                    solidity::Type::Address,
                    solidity::Type::Uint(256),
                ],
            },
        ],
    },
];
