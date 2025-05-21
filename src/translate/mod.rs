use std::{cell::RefCell, path::PathBuf, rc::Rc};
use crate::sway;
use convert_case::{Case, Casing};


mod import_directives;
mod type_definitions;
mod type_names;
mod enums;
mod structs;
pub use import_directives::*;
pub use type_definitions::*;
pub use type_names::*;
pub use enums::*;
pub use structs::*;


#[derive(Clone, Debug, PartialEq)]
pub struct TranslatedUsingDirective {
    pub library_name: String,
    pub for_type: Option<sway::TypeName>,
    pub functions: Vec<TranslatedFunction>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TranslatedEnum {
    pub type_definition: sway::TypeDefinition,
    pub variants_impl: sway::Impl,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct TranslatedVariable {
    pub old_name: String,
    pub new_name: String,
    pub type_name: sway::TypeName,
    pub abi_type_name: Option<sway::TypeName>,
    pub storage_namespace: Option<String>,
    pub is_configurable: bool,
    pub is_constant: bool,
    pub statement_index: Option<usize>,
    pub read_count: usize,
    pub mutation_count: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TranslatedFunction {
    pub old_name: String,
    pub new_name: String,
    pub attributes: Option<sway::AttributeList>,
    pub constructor_calls: Vec<sway::FunctionCall>,
    pub modifiers: Vec<sway::FunctionCall>,
    pub type_name: sway::TypeName,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TranslatedModifier {
    pub old_name: String,
    pub new_name: String,
    pub parameters: sway::ParameterList,
    pub attributes: Option<sway::AttributeList>,
    pub has_underscore: bool,
    pub pre_body: Option<sway::Block>,
    pub post_body: Option<sway::Block>,
}

#[derive(Default, Debug)]
pub struct TranslatedModule {
    pub name: String,
    pub path: PathBuf,
    pub submodules: Vec<Rc<RefCell<TranslatedModule>>>,
    pub dependencies: Vec<String>,
    pub uses: Vec<sway::Use>,
    pub using_directives: Vec<TranslatedUsingDirective>,
    pub type_definitions: Vec<sway::TypeDefinition>,
    pub structs: Vec<Rc<RefCell<sway::Struct>>>,
    pub enums: Vec<TranslatedEnum>,
    pub events_enums: Vec<(Rc<RefCell<sway::Enum>>, Rc<RefCell<sway::Impl>>)>,
    pub errors_enums: Vec<(Rc<RefCell<sway::Enum>>, Rc<RefCell<sway::Impl>>)>,
    pub constants: Vec<sway::Constant>,
    pub abis: Vec<sway::Abi>,
    pub abi: Option<sway::Abi>,
    pub configurable: Option<sway::Configurable>,
    pub storage: Option<sway::Storage>,
    pub modifiers: Vec<TranslatedModifier>,
    pub functions: Vec<sway::Function>,
    pub impls: Vec<sway::Impl>,
}

impl TranslatedModule {
    /// Ensures that the provided `use` statement is declared in the translated definition.
    #[inline]
    pub fn ensure_use_declared(&mut self, name: &str) {
        let mut tree: Option<sway::UseTree> = None;

        for part in name.split("::").collect::<Vec<_>>().into_iter().rev() {
            match part {
                "*" => tree = Some(sway::UseTree::Glob),

                _ => {
                    tree = Some(if let Some(use_tree) = tree.clone() {
                        sway::UseTree::Path {
                            prefix: part.into(),
                            suffix: Box::new(use_tree),
                        }
                    } else {
                        sway::UseTree::Name { name: part.into() }
                    })
                }
            }
        }

        let tree = tree.unwrap();

        if !self.uses.iter().any(|u| u.tree == tree) {
            self.uses.push(sway::Use {
                is_public: false,
                tree,
            });
        }
    }

    /// Ensures that the provided dependency is declared in the generated `Forc.toml` file.
    #[inline]
    pub fn ensure_dependency_declared(&mut self, dependency: &str) {
        let dependency = dependency.to_string();

        if !self.dependencies.contains(&dependency) {
            self.dependencies.push(dependency);
            self.dependencies.sort();
        }
    }

     /// Gets the base underlying type of the supplied type name
    pub fn get_underlying_type(&self, type_name: &sway::TypeName) -> sway::TypeName {
        // Check to see if the expression's type is a type definition and get the underlying type
        for type_definition in self.type_definitions.iter() {
            if &type_definition.name == type_name {
                return self.get_underlying_type(type_definition.underlying_type.as_ref().unwrap());
            }
        }

        // If we didn't find a type definition, check to see if an enum exists and get its underlying type
        for translated_enum in self.enums.iter() {
            if &translated_enum.type_definition.name == type_name {
                return self.get_underlying_type(
                    translated_enum
                        .type_definition
                        .underlying_type
                        .as_ref()
                        .unwrap(),
                );
            }
        }

        type_name.clone()
    }
}

#[inline]
pub fn translate_naming_convention(name: &str, case: Case) -> String {
    // HACK: do not allow dollar signs
    let mut name = name.replace('$', "dollar_sign").to_case(case).to_string();
    
    // HACK: do not allow name to start with double underscore
    while name.starts_with("__") {
        name = name[2..].to_string();
    }

    let name = if name.chars().all(|c| c == '_') {
        name.to_string()
    } else {
        let prefix = name.chars().take_while(|c| *c == '_').collect::<String>();
        let postfix = name.chars().rev().take_while(|c| *c == '_').collect::<String>();
        format!("{prefix}{}{postfix}", name.to_case(case))
    };

    match name.as_str() {
        "self" => "this".into(),
        _ => name,
    }
}