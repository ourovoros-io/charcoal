use crate::sway;
use solang_parser::pt as solidity;
use std::{
    collections::HashMap,
    fmt::Display,
    path::{Path, PathBuf},
};

#[derive(Clone, Debug, PartialEq)]
pub struct TranslatedVariable {
    pub old_name: String,
    pub new_name: String,
    pub type_name: sway::TypeName,
    pub is_storage: bool,
    pub statement_index: Option<usize>,
    pub read_count: usize,
    pub mutation_count: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TranslatedFunction {
    pub old_name: String,
    pub new_name: String,
    pub parameters: sway::ParameterList,
    pub modifiers: Vec<sway::FunctionCall>,
    pub return_type: Option<sway::TypeName>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TranslatedModifier {
    pub old_name: String,
    pub new_name: String,
    pub parameters: sway::ParameterList,
    pub has_underscore: bool,
    pub pre_body: Option<sway::Block>,
    pub post_body: Option<sway::Block>,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct TranslationScope {
    pub parent: Option<Box<TranslationScope>>,
    pub variables: Vec<TranslatedVariable>,
    pub functions: Vec<TranslatedFunction>,
}

impl TranslationScope {
    /// Attempts to get a reference to a variable using its old name
    pub fn find_variable_from_old_name(&self, old_name: &str) -> Option<&TranslatedVariable> {
        if let Some(variable) = self.variables.iter().rev().find(|v| v.old_name == old_name) {
            return Some(variable);
        }

        if let Some(parent) = self.parent.as_ref() {
            if let Some(variable) = parent.find_variable_from_old_name(old_name) {
                return Some(variable);
            }
        }

        None
    }

    /// Attempts to get a mutable reference to a variable using its old name
    pub fn find_variable_from_old_name_mut(&mut self, old_name: &str) -> Option<&mut TranslatedVariable> {
        if let Some(variable) = self.variables.iter_mut().rev().find(|v| v.old_name == old_name) {
            return Some(variable);
        }

        if let Some(parent) = self.parent.as_mut() {
            if let Some(variable) = parent.find_variable_from_old_name_mut(old_name) {
                return Some(variable);
            }
        }

        None
    }

    /// Attempts to get a reference to a variable using its new name
    pub fn find_variable_from_new_name(&self, new_name: &str) -> Option<&TranslatedVariable> {
        if let Some(variable) = self.variables.iter().rev().find(|v| v.new_name == new_name) {
            return Some(variable);
        }

        if let Some(parent) = self.parent.as_ref() {
            if let Some(variable) = parent.find_variable_from_new_name(new_name) {
                return Some(variable);
            }
        }

        None
    }

    /// Attempts to get a mutable reference to a variable using its new name
    pub fn find_variable_from_new_name_mut(&mut self, new_name: &str) -> Option<&mut TranslatedVariable> {
        if let Some(variable) = self.variables.iter_mut().rev().find(|v| v.new_name == new_name) {
            return Some(variable);
        }

        if let Some(parent) = self.parent.as_mut() {
            if let Some(variable) = parent.find_variable_from_new_name_mut(new_name) {
                return Some(variable);
            }
        }

        None
    }

    pub fn find_function<F: Copy + FnMut(&&TranslatedFunction) -> bool>(&self, f: F) -> Option<&TranslatedFunction> {
        if let Some(function) = self.functions.iter().find(f) {
            return Some(function);
        }

        if let Some(parent) = self.parent.as_ref() {
            if let Some(function) = parent.find_function(f) {
                return Some(function);
            }
        }

        None
    }

    /// Attempts to get a reference to a function using its old name
    pub fn find_function_from_old_name(&self, old_name: &str) -> Option<&TranslatedFunction> {
        if let Some(function) = self.functions.iter().rev().find(|v| v.old_name == old_name) {
            return Some(function);
        }

        if let Some(parent) = self.parent.as_ref() {
            if let Some(function) = parent.find_function_from_old_name(old_name) {
                return Some(function);
            }
        }

        None
    }

    /// Attempts to get a mutable reference to a function using its old name
    pub fn find_function_from_old_name_mut(&mut self, old_name: &str) -> Option<&mut TranslatedFunction> {
        if let Some(function) = self.functions.iter_mut().rev().find(|v| v.old_name == old_name) {
            return Some(function);
        }

        if let Some(parent) = self.parent.as_mut() {
            if let Some(function) = parent.find_function_from_old_name_mut(old_name) {
                return Some(function);
            }
        }

        None
    }

    /// Attempts to get a reference to a function using its new name
    pub fn find_function_from_new_name(&self, new_name: &str) -> Option<&TranslatedFunction> {
        if let Some(function) = self.functions.iter().rev().find(|v| v.new_name == new_name) {
            return Some(function);
        }

        if let Some(parent) = self.parent.as_ref() {
            if let Some(function) = parent.find_function_from_new_name(new_name) {
                return Some(function);
            }
        }

        None
    }

    /// Attempts to get a mutable reference to a function using its new name
    pub fn find_function_from_new_name_mut(&mut self, new_name: &str) -> Option<&mut TranslatedFunction> {
        if let Some(function) = self.functions.iter_mut().rev().find(|v| v.new_name == new_name) {
            return Some(function);
        }

        if let Some(parent) = self.parent.as_mut() {
            if let Some(function) = parent.find_function_from_new_name_mut(new_name) {
                return Some(function);
            }
        }

        None
    }
}

pub struct TranslatedDefinition {
    pub path: PathBuf,
    pub toplevel_scope: TranslationScope,
    pub kind: solidity::ContractTy,
    pub uses: Vec<sway::Use>,
    pub name: String,
    pub inherits: Vec<String>,
    pub type_definitions: Vec<sway::TypeDefinition>,
    pub structs: Vec<sway::Struct>,
    pub events_enum: Option<sway::Enum>,
    pub errors_enum: Option<sway::Enum>,
    pub abi: Option<sway::Abi>,
    pub configurable: Option<sway::Configurable>,
    pub storage: Option<sway::Storage>,
    pub modifiers: Vec<TranslatedModifier>,
    pub functions: Vec<sway::Function>,
    pub function_name_counts: HashMap<String, usize>,
    pub function_names: HashMap<String, String>,
    pub function_call_counts: HashMap<String, usize>,
    pub impls: Vec<sway::Impl>,
}

impl Display for TranslatedDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut written = 0usize;

        for use_entry in self.uses.iter() {
            writeln!(f, "{}", sway::TabbedDisplayer(use_entry))?;
            written += 1;
        }

        for (i, x) in self.type_definitions.iter().enumerate() {
            if i == 0 && written > 0 {
                writeln!(f)?;
            }

            writeln!(f, "{}", sway::TabbedDisplayer(x))?;
            written += 1;
        }

        for (i, x) in self.structs.iter().enumerate() {
            if i == 0 && written > 0 {
                writeln!(f)?;
            } else if i > 0 {
                writeln!(f)?;
            }

            writeln!(f, "{}", sway::TabbedDisplayer(x))?;
            written += 1;
        }
    
        if let Some(x) = self.events_enum.as_ref() {
            if written > 0 {
                writeln!(f)?;
            }

            writeln!(f, "{}", sway::TabbedDisplayer(x))?;
            written += 1;
        }

        if let Some(x) = self.errors_enum.as_ref() {
            if written > 0 {
                writeln!(f)?;
            }

            writeln!(f, "{}", sway::TabbedDisplayer(x))?;
            written += 1;
        }
        
        if let Some(x) = self.abi.as_ref() {
            if written > 0 {
                writeln!(f)?;
            }

            writeln!(f, "{}", sway::TabbedDisplayer(x))?;
            written += 1;
        }
        
        if let Some(x) = self.storage.as_ref() {
            if written > 0 {
                writeln!(f)?;
            }

            writeln!(f, "{}", sway::TabbedDisplayer(x))?;
            written += 1;
        }
        
        for (i, x) in self.functions.iter().enumerate() {
            if i == 0 && written > 0 {
                writeln!(f)?;
            } else if i > 0 {
                writeln!(f)?;
            }

            writeln!(f, "{}", sway::TabbedDisplayer(x))?;
            written += 1;
        }
        
        for (i, x) in self.impls.iter().enumerate() {
            if i == 0 && written > 0 {
                writeln!(f)?;
            } else if i > 0 {
                writeln!(f)?;
            }

            writeln!(f, "{}", sway::TabbedDisplayer(x))?;
            written += 1;
        }

        Ok(())
    }
}

impl TranslatedDefinition {
    pub fn new<P: AsRef<Path>, S: ToString>(path: P, kind: solidity::ContractTy, name: S, inherits: Vec<S>) -> Self {
        Self {
            path: path.as_ref().into(),
            toplevel_scope: TranslationScope::default(),
            kind,
            uses: vec![],
            name: name.to_string(),
            inherits: inherits.iter().map(|i| i.to_string()).collect(),
            type_definitions: vec![],
            structs: vec![],
            events_enum: None,
            errors_enum: None,
            abi: None,
            configurable: None,
            storage: None,
            modifiers: vec![],
            functions: vec![],
            function_name_counts: HashMap::new(),
            function_names: HashMap::new(),
            function_call_counts: HashMap::new(),
            impls: vec![],
        }
    }

    /// Gets the events enum for the translated definition. If it doesn't exist, it gets created.
    pub fn get_events_enum(&mut self) -> &mut sway::Enum {
        if self.events_enum.is_none() {
            self.events_enum = Some(sway::Enum {
                attributes: None,
                is_public: false,
                name: format!("{}Event", self.name),
                generic_parameters: None,
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
                generic_parameters: None,
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
                inherits: vec![],
                functions: vec![],
            });
        }

        self.abi.as_mut().unwrap()
    }

    /// Gets the configurable block for the translated definition. If it doesn't exist, it gets created.
    pub fn get_configurable(&mut self) -> &mut sway::Configurable {
        if self.configurable.is_none() {
            self.configurable = Some(sway::Configurable {
                fields: vec![],
            });
        }

        self.configurable.as_mut().unwrap()
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

    pub fn find_contract_impl(&self) -> Option<&sway::Impl> {
        self.impls.iter().find(|i| {
            let sway::TypeName::Identifier { name: type_name, .. } = &i.type_name else { return false };
            let Some(sway::TypeName::Identifier { name: for_type_name, .. }) = i.for_type_name.as_ref() else { return false };
            *type_name == self.name && for_type_name == "Contract"
        })
    }

    pub fn find_contract_impl_mut(&mut self) -> Option<&mut sway::Impl> {
        self.impls.iter_mut().find(|i| {
            let sway::TypeName::Identifier { name: type_name, .. } = &i.type_name else { return false };
            let Some(sway::TypeName::Identifier { name: for_type_name, .. }) = i.for_type_name.as_ref() else { return false };
            *type_name == self.name && for_type_name == "Contract"
        })
    }

    /// Gets the translated definition's implementation for `Contract`. If it doesn't exist, it gets created.
    pub fn get_contract_impl(&mut self) -> &mut sway::Impl {
        if self.find_contract_impl().is_none() {
            self.impls.push(sway::Impl {
                generic_parameters: None,
                type_name: sway::TypeName::Identifier {
                    name: self.name.clone(),
                    generic_parameters: None,
                },
                for_type_name: Some(sway::TypeName::Identifier {
                    name: "Contract".into(),
                    generic_parameters: None,
                }),
                items: vec![],
            });
        }

        self.find_contract_impl_mut().unwrap()
    }
}
