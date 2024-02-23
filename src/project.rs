use crate::{
    errors::Error,
    translate::{translate_contract_definition, TranslatedDefinition},
};
use solang_parser::pt as solidity;
use std::{
    cell::RefCell,
    collections::HashMap,
    path::{Path, PathBuf},
    rc::Rc,
};

#[derive(Default)]
pub struct Project {
    pub line_ranges: HashMap<PathBuf, Vec<(usize, usize)>>,
    pub solidity_source_units: Rc<RefCell<HashMap<PathBuf, solidity::SourceUnit>>>,
    pub translated_definitions: Vec<TranslatedDefinition>,
    pub import_directives: HashMap<PathBuf, HashMap<PathBuf, Option<Vec<String>>>>,
}

impl Project {
    /// Attempts to parse the file from the supplied `path`.
    #[inline]
    fn parse_solidity_source_unit<P: AsRef<Path>>(&mut self, path: P) -> Result<(), Error> {
        if !path.as_ref().exists() {
            return Err(
                Error::Wrapped(Box::new(std::io::Error::new(std::io::ErrorKind::NotFound, format!("File not found: {}", path.as_ref().to_string_lossy()))))
            );
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

        for source_unit_part in source_unit.0.iter() {
            match source_unit_part {
                solidity::SourceUnitPart::PragmaDirective(_, _, _) => {
                    // NOTE: we don't need to do anything with pragma directives
                }

                solidity::SourceUnitPart::ImportDirective(import_directive) => {
                    import_directives.push(import_directive.clone());
                }

                solidity::SourceUnitPart::ContractDefinition(_) => {
                    // NOTE: contracts are handled below
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

                solidity::SourceUnitPart::FunctionDefinition(_) => todo!("toplevel function definition"),
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
                if !filename.string.starts_with('.') {
                    todo!("handle global import paths (i.e: node_modules)")
                }
                
                let import_path = crate::get_canonical_path(source_unit_directory.join(filename.string.clone()), false, false)
                    .map_err(|e| Error::Wrapped(Box::new(e)))?;

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
                contract_definition,
            )?;
        }

        Ok(())
    }
}
