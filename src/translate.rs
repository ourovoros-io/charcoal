use crate::sway;
use solang_parser::pt as solidity;
use std::{
    fmt::Display,
    path::{Path, PathBuf},
};

#[derive(Clone, Debug, Default)]
pub struct TranslationScope {
    pub parent: Option<Box<TranslationScope>>,
    pub parameters: Vec<sway::Parameter>,
}

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
    pub configurable: Option<sway::Configurable>,
    pub storage: Option<sway::Storage>,
    pub functions: Vec<sway::Function>,
    pub impls: Vec<sway::Impl>,
}

impl Display for TranslatedDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut written = 0usize;

        for x in self.type_definitions.iter() {
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
            kind,
            name: name.to_string(),
            inherits: inherits.iter().map(|i| i.to_string()).collect(),
            type_definitions: vec![],
            structs: vec![],
            events_enum: None,
            errors_enum: None,
            abi: None,
            configurable: None,
            storage: None,
            functions: vec![],
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
