mod assembly;
mod contracts;
mod enums;
mod expressions;
mod functions;
mod import_directives;
mod statements;
mod storage;
mod structs;
mod type_definitions;
mod type_names;

#[allow(ambiguous_glob_reexports)]
pub use self::{
    assembly::*, contracts::*, enums::*, expressions::*, functions::*, import_directives::*,
    statements::*, storage::*, structs::*, type_definitions::*, type_names::*,
};

use crate::{errors::Error, project::Project, sway};
use solang_parser::pt as solidity;
use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::Display,
    path::{Path, PathBuf},
    rc::Rc,
};

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
    pub is_storage: bool,
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

#[derive(Clone, Debug, Default, PartialEq)]
pub struct TranslationScope {
    pub parent: Option<Rc<RefCell<TranslationScope>>>,
    pub variables: Vec<Rc<RefCell<TranslatedVariable>>>,
    pub functions: Vec<Rc<RefCell<TranslatedFunction>>>,
}

impl TranslationScope {
    pub fn dump(&self, variables: bool, functions: bool) {
        if let Some(parent) = self.parent.as_ref() {
            parent.borrow().dump(variables, functions);
        }

        if variables {
            println!("variables:");

            for v in self.variables.iter() {
                println!("{v:#?}");
            }
        }

        if functions {
            println!("functions:");

            for f in self.functions.iter() {
                println!("{f:#?}");
            }
        }
    }

    #[inline]
    pub fn generate_unique_variable_name(&self, name: &str) -> String {
        let mut result = name.to_string();

        while self.get_variable_from_new_name(result.as_str()).is_some() {
            result = format!("_{result}");
        }

        result
    }

    /// Attempts to get a reference to a translated variable using its old name
    pub fn get_variable_from_old_name(
        &self,
        old_name: &str,
    ) -> Option<Rc<RefCell<TranslatedVariable>>> {
        if let Some(variable) = self
            .variables
            .iter()
            .rev()
            .find(|v| v.borrow().old_name == old_name)
        {
            return Some(variable.clone());
        }

        if let Some(parent) = self.parent.as_ref() {
            if let Some(variable) = parent.borrow().get_variable_from_old_name(old_name) {
                return Some(variable);
            }
        }

        None
    }

    /// Attempts to get a reference to a translated variable using its new name
    pub fn get_variable_from_new_name(
        &self,
        new_name: &str,
    ) -> Option<Rc<RefCell<TranslatedVariable>>> {
        if let Some(variable) = self
            .variables
            .iter()
            .rev()
            .find(|v| v.borrow().new_name == new_name)
        {
            return Some(variable.clone());
        }

        if let Some(parent) = self.parent.as_ref() {
            if let Some(variable) = parent.borrow().get_variable_from_new_name(new_name) {
                return Some(variable);
            }
        }

        None
    }

    /// Attempts to find a translated variable using a custom function
    pub fn find_variable<F: Copy + FnMut(&&Rc<RefCell<TranslatedVariable>>) -> bool>(
        &self,
        f: F,
    ) -> Option<Rc<RefCell<TranslatedVariable>>> {
        if let Some(variable) = self.variables.iter().find(f) {
            return Some(variable.clone());
        }

        if let Some(parent) = self.parent.as_ref() {
            if let Some(variable) = parent.borrow().find_variable(f) {
                return Some(variable);
            }
        }

        None
    }

    /// Atempts to find a translated function using a custom function
    pub fn find_function<F: Copy + FnMut(&&Rc<RefCell<TranslatedFunction>>) -> bool>(
        &self,
        f: F,
    ) -> Option<Rc<RefCell<TranslatedFunction>>> {
        if let Some(function) = self.functions.iter().find(f) {
            return Some(function.clone());
        }

        if let Some(parent) = self.parent.as_ref() {
            if let Some(function) = parent.borrow().find_function(f) {
                return Some(function);
            }
        }

        None
    }
}

#[derive(Clone, Debug)]
pub struct DeferredInitialization {
    pub name: String,
    pub is_storage: bool,
    pub is_constant: bool,
    pub is_configurable: bool,
    pub value: sway::Expression,
}

#[derive(Clone, Debug, Default)]
pub struct TranslatedDefinition {
    pub path: PathBuf,
    pub toplevel_scope: Rc<RefCell<TranslationScope>>,
    pub kind: Option<solidity::ContractTy>,
    pub dependencies: Vec<String>,
    pub deferred_initializations: Vec<DeferredInitialization>,

    pub uses: Vec<sway::Use>,
    pub name: String,
    pub inherits: Vec<String>,
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

    pub struct_names: Vec<String>,
    pub contract_names: Vec<String>,

    pub function_name_counts: HashMap<String, usize>,
    pub function_names: HashMap<String, String>,
    pub function_call_counts: HashMap<String, usize>,
    pub functions_called: HashMap<String, Vec<String>>,
    pub current_functions: Vec<String>,

    pub storage_fields_name_counts: HashMap<String, usize>,
    pub storage_fields_names: HashMap<String, String>,

    pub mapping_names: Vec<(String, Vec<String>)>,
}

impl Display for TranslatedDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut written = 0usize;

        for use_entry in self.uses.iter() {
            writeln!(f, "{}", sway::TabbedDisplayer(use_entry))?;
            written += 1;
        }

        for (i, x) in self.constants.iter().enumerate() {
            if i == 0 && written > 0 {
                writeln!(f)?;
            }

            writeln!(f, "{}", sway::TabbedDisplayer(x))?;
            written += 1;
        }

        for (i, x) in self.type_definitions.iter().enumerate() {
            if i == 0 && written > 0 {
                writeln!(f)?;
            }

            writeln!(f, "{}", sway::TabbedDisplayer(x))?;
            written += 1;
        }

        for (i, x) in self.enums.iter().enumerate() {
            if (i == 0 && written > 0) || i > 0 {
                writeln!(f)?;
            }

            writeln!(f, "{}", sway::TabbedDisplayer(&x.type_definition))?;
            writeln!(f)?;
            writeln!(f, "{}", sway::TabbedDisplayer(&x.variants_impl))?;
            written += 1;
        }

        for (i, x) in self.structs.iter().enumerate() {
            if (i == 0 && written > 0) || i > 0 {
                writeln!(f)?;
            }

            writeln!(f, "{}", sway::TabbedDisplayer(&*x.borrow()))?;
            written += 1;
        }

        for (i, (events_enum, abi_encode_impl)) in self.events_enums.iter().enumerate() {
            if (i == 0 && written > 0) || i > 0 {
                writeln!(f)?;
            }

            writeln!(
                f,
                "{}",
                sway::TabbedDisplayer(&events_enum.borrow().clone())
            )?;
            writeln!(f)?;
            writeln!(
                f,
                "{}",
                sway::TabbedDisplayer(&abi_encode_impl.borrow().clone())
            )?;
            written += 1;
        }

        for (i, (errors_enum, abi_encode_impl)) in self.errors_enums.iter().enumerate() {
            if (i == 0 && written > 0) || i > 0 {
                writeln!(f)?;
            }

            writeln!(
                f,
                "{}",
                sway::TabbedDisplayer(&errors_enum.borrow().clone())
            )?;
            writeln!(f)?;
            writeln!(
                f,
                "{}",
                sway::TabbedDisplayer(&abi_encode_impl.borrow().clone())
            )?;
            written += 1;
        }

        for (i, x) in self.abis.iter().enumerate() {
            if (i == 0 && written > 0) || i > 0 {
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

        if let Some(x) = self.configurable.as_ref() {
            if written > 0 {
                writeln!(f)?;
            }

            writeln!(f, "{}", sway::TabbedDisplayer(x))?;
            written += 1;
        }

        for (i, x) in self.functions.iter().enumerate() {
            if (i == 0 && written > 0) || i > 0 {
                writeln!(f)?;
            }
            writeln!(f, "{}", sway::TabbedDisplayer(x))?;
            written += 1;
        }

        for (i, x) in self.impls.iter().enumerate() {
            if (i == 0 && written > 0) || i > 0 {
                writeln!(f)?;
            }
            writeln!(f, "{}", sway::TabbedDisplayer(x))?;
            written += 1;
        }

        Ok(())
    }
}

impl From<TranslatedDefinition> for sway::Module {
    fn from(val: TranslatedDefinition) -> Self {
        let mut result = sway::Module {
            kind: match val.kind.as_ref() {
                Some(solidity::ContractTy::Abstract(_))
                | Some(solidity::ContractTy::Contract(_))
                | Some(solidity::ContractTy::Interface(_)) => sway::ModuleKind::Contract,

                _ => sway::ModuleKind::Library,
            },
            items: vec![],
        };

        for x in val.uses.iter() {
            result.items.push(sway::ModuleItem::Use(x.clone()));
        }

        for x in val.constants.iter() {
            result.items.push(sway::ModuleItem::Constant(x.clone()));
        }

        for x in val.type_definitions.iter() {
            result
                .items
                .push(sway::ModuleItem::TypeDefinition(x.clone()));
        }

        for x in val.enums.iter() {
            result
                .items
                .push(sway::ModuleItem::TypeDefinition(x.type_definition.clone()));
            result
                .items
                .push(sway::ModuleItem::Impl(x.variants_impl.clone()));
        }

        for x in val.structs.iter() {
            result
                .items
                .push(sway::ModuleItem::Struct(x.borrow().clone()));
        }

        for (events_enum, abi_encode_impl) in val.events_enums.iter() {
            result
                .items
                .push(sway::ModuleItem::Enum(events_enum.borrow().clone()));
            result
                .items
                .push(sway::ModuleItem::Impl(abi_encode_impl.borrow().clone()));
        }

        for (errors_enum, abi_encode_impl) in val.errors_enums.iter() {
            result
                .items
                .push(sway::ModuleItem::Enum(errors_enum.borrow().clone()));
            result
                .items
                .push(sway::ModuleItem::Impl(abi_encode_impl.borrow().clone()));
        }

        for x in val.abis.iter() {
            result.items.push(sway::ModuleItem::Abi(x.clone()));
        }

        if let Some(x) = val.abi.as_ref() {
            result.items.push(sway::ModuleItem::Abi(x.clone()));
        }

        if let Some(x) = val.storage.as_ref() {
            result.items.push(sway::ModuleItem::Storage(x.clone()));
        }

        if let Some(x) = val.configurable.as_ref() {
            result.items.push(sway::ModuleItem::Configurable(x.clone()));
        }

        for x in val.functions.iter() {
            if let Some(0) = val.function_call_counts.get(&x.name) {
                continue;
            }

            result.items.push(sway::ModuleItem::Function(x.clone()));
        }

        for x in val.impls.iter() {
            result.items.push(sway::ModuleItem::Impl(x.clone()));
        }

        result
    }
}

impl TranslatedDefinition {
    pub fn new<P: AsRef<Path>, S1: ToString, S2: ToString>(
        path: P,
        kind: solidity::ContractTy,
        name: S1,
        inherits: Vec<S2>,
    ) -> Self {
        Self {
            path: path.as_ref().into(),
            toplevel_scope: Rc::new(RefCell::new(TranslationScope::default())),
            kind: Some(kind),
            dependencies: vec![],
            deferred_initializations: vec![],

            uses: vec![],
            name: name.to_string(),
            inherits: inherits.iter().map(|i| i.to_string()).collect(),
            using_directives: vec![],
            type_definitions: vec![],
            enums: vec![],
            structs: vec![],
            events_enums: vec![],
            errors_enums: vec![],
            constants: vec![],
            abis: vec![],
            abi: None,
            configurable: None,
            storage: None,
            modifiers: vec![],
            functions: vec![],
            impls: vec![],

            struct_names: vec![],
            contract_names: vec![],

            function_name_counts: HashMap::new(),
            function_names: HashMap::new(),
            function_call_counts: HashMap::new(),
            functions_called: HashMap::new(),
            current_functions: vec![],

            storage_fields_name_counts: HashMap::new(),
            storage_fields_names: HashMap::new(),

            mapping_names: Vec::new(),
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

    /// Ensures that the provided struct is declared in the translated definition, as well as all of its field types.
    #[inline]
    pub fn ensure_struct_included(
        &mut self,
        project: &Project,
        struct_definition: Rc<RefCell<sway::Struct>>,
    ) {
        if !self.struct_names.contains(&struct_definition.borrow().name) {
            self.struct_names
                .push(struct_definition.borrow().name.clone());
        }

        if self.structs.contains(&struct_definition) {
            return;
        }

        for field in struct_definition.borrow().fields.iter() {
            if let sway::TypeName::Identifier {
                name,
                generic_parameters: None,
            } = &field.type_name
            {
                if !self.structs.iter().any(|s| s.borrow().name == *name) {
                    'lookup: for external_definition in project.translated_definitions.iter() {
                        for external_struct in external_definition.structs.iter() {
                            if external_struct.borrow().name == *name {
                                self.structs.push(external_struct.clone());
                                break 'lookup;
                            }
                        }
                    }
                }
            }
        }

        self.structs.push(struct_definition.clone());
    }

    #[inline]
    pub fn add_enum(&mut self, translated_enum: &TranslatedEnum) {
        if self.enums.contains(translated_enum) {
            return;
        }

        let sway::TypeName::Identifier {
            name,
            generic_parameters: None,
        } = &translated_enum.type_definition.name
        else {
            panic!(
                "Expected Identifier type name, found {:#?}",
                translated_enum.type_definition.name
            );
        };

        for item in translated_enum.variants_impl.items.iter() {
            let sway::ImplItem::Constant(c) = item else {
                continue;
            };

            self.toplevel_scope
                .borrow_mut()
                .variables
                .push(Rc::new(RefCell::new(TranslatedVariable {
                    old_name: String::new(), // TODO: is this ok?
                    new_name: format!("{}::{}", name, c.name),
                    type_name: translated_enum.type_definition.name.clone(),
                    ..Default::default()
                })));
        }

        self.enums.push(translated_enum.clone());
    }

    /// Gets the abi for the translated definition. If it doesn't exist, it gets created.
    #[inline]
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

    #[inline]
    pub fn find_abi<F: Copy + FnMut(&&sway::Abi) -> bool>(&self, mut f: F) -> Option<&sway::Abi> {
        if let Some(abi) = self.abi.as_ref() {
            if f(&abi) {
                return Some(abi);
            }
        }

        if let Some(abi) = self.abis.iter().find(f) {
            return Some(abi);
        }

        None
    }

    /// Gets the configurable block for the translated definition. If it doesn't exist, it gets created.
    #[inline]
    pub fn get_configurable(&mut self) -> &mut sway::Configurable {
        if self.configurable.is_none() {
            self.configurable = Some(sway::Configurable { fields: vec![] });
        }

        self.configurable.as_mut().unwrap()
    }

    /// Gets the storage block for the translated definition. If it doesn't exist, it gets created.
    #[inline]
    pub fn get_storage(&mut self) -> &mut sway::Storage {
        if self.storage.is_none() {
            self.storage = Some(sway::Storage { fields: vec![] });
        }

        self.storage.as_mut().unwrap()
    }


    #[inline]
    pub fn get_constructor_fn(&mut self) -> &mut sway::Function {
        let abi = self.get_abi();
        if !abi.functions.iter().any(|f| f.name == "constructor") {
            abi.functions.push(sway::Function{ 
                attributes: None, 
                is_public: false, 
                old_name: String::new(), 
                name: "constructor".into(), 
                generic_parameters: None, 
                parameters: sway::ParameterList{ entries: vec![] }, 
                return_type: None, 
                body: None,
            });
        }

        let contract_impl = self.get_contract_impl();
        if !contract_impl.items.iter().any(|f| {
            match f {
                sway::ImplItem::Function(function) => function.name == "constructor",
                _ => false
            }
        }) {
            contract_impl.items.push(sway::ImplItem::Function(sway::Function{ 
                attributes: None, 
                is_public: false, 
                old_name: String::new(), 
                name: "constructor".into(), 
                generic_parameters: None, 
                parameters: sway::ParameterList{ entries: vec![] }, 
                return_type: None, 
                body: Some(sway::Block { 
                    statements: vec![], 
                    final_expr: None, 
                }),
            }));
        }

        let Some(sway::ImplItem::Function(item)) = contract_impl.items.iter_mut().find(|f| {
            match f {
                sway::ImplItem::Function(function) => function.name == "constructor",
                _ => false,
            }
        }) else { 
            unreachable!() 
        };

        item
    }

    /// Attempts to find the translated definition's `impl` block, if available.
    #[inline]
    pub fn find_contract_impl(&self) -> Option<&sway::Impl> {
        self.impls.iter().find(|i| {
            let sway::TypeName::Identifier {
                name: type_name, ..
            } = &i.type_name
            else {
                return false;
            };
            let Some(sway::TypeName::Identifier {
                name: for_type_name,
                ..
            }) = i.for_type_name.as_ref()
            else {
                return false;
            };
            *type_name == self.name && for_type_name == "Contract"
        })
    }

    /// Attempts to find the translated definitions `impl` block mutability, if available.
    #[inline]
    pub fn find_contract_impl_mut(&mut self) -> Option<&mut sway::Impl> {
        self.impls.iter_mut().find(|i| {
            let sway::TypeName::Identifier {
                name: type_name, ..
            } = &i.type_name
            else {
                return false;
            };
            let Some(sway::TypeName::Identifier {
                name: for_type_name,
                ..
            }) = i.for_type_name.as_ref()
            else {
                return false;
            };
            *type_name == self.name && for_type_name == "Contract"
        })
    }

    /// Gets the translated definition's implementation for `Contract`. If it doesn't exist, it gets created.
    #[inline]
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

    /// Attempts to get the type of the supplied expression.
    pub fn get_expression_type(
        &mut self,
        scope: Rc<RefCell<TranslationScope>>,
        expression: &sway::Expression,
    ) -> Result<sway::TypeName, Error> {
        match expression {
            sway::Expression::Literal(literal) => match literal {
                sway::Literal::Bool(_) => Ok(sway::TypeName::Identifier {
                    name: "bool".into(),
                    generic_parameters: None,
                }),
                sway::Literal::DecInt(value, suffix) => Ok(sway::TypeName::Identifier {
                    name: if let Some(suffix) = suffix.as_ref() {
                        suffix.clone()
                    } else {
                        let mut bits = value.bits();
                        let remainder = bits % 8;

                        if remainder != 0 {
                            bits = bits + 8 - remainder;
                        }

                        if bits < 64 {
                            bits = 64;
                        } else if bits > 64 && bits < 256 {
                            bits = 256;
                        } else if bits > 256 {
                            panic!("integer has too many bits: {bits}")
                        }

                        format!("u{}", bits)
                    },
                    generic_parameters: None,
                }),
                sway::Literal::HexInt(value, suffix) => Ok(sway::TypeName::Identifier {
                    name: if let Some(suffix) = suffix.as_ref() {
                        suffix.clone()
                    } else {
                        let mut bits = value.bits();
                        let remainder = bits % 8;

                        if remainder != 0 {
                            bits = bits + 8 - remainder;
                        }

                        if bits < 64 {
                            bits = 64;
                        } else if bits > 64 && bits < 256 {
                            bits = 256;
                        } else if bits > 256 {
                            panic!("integer has too many bits: {bits}")
                        }

                        format!("u{}", bits)
                    },
                    generic_parameters: None,
                }),
                sway::Literal::String(_) => Ok(sway::TypeName::StringSlice),
            },

            sway::Expression::Identifier(name) => {
                // HACK: Check if the identifier is a translated enum variant
                if name.contains("::") {
                    let parts = name.split("::").collect::<Vec<_>>();

                    if parts.len() == 2 {
                        let enum_name = parts[0];
                        let variant_name = parts[1];

                        if self.enums.iter().any(|e| {
                            let sway::TypeName::Identifier {
                                name,
                                generic_parameters: None,
                            } = &e.type_definition.name
                            else {
                                return false;
                            };

                            if !e.variants_impl.items.iter().any(|i| {
                                let sway::ImplItem::Constant(variant) = i else {
                                    return false;
                                };
                                variant.name == variant_name
                            }) {
                                return false;
                            }

                            name == enum_name
                        }) {
                            return Ok(sway::TypeName::Identifier {
                                name: enum_name.into(),
                                generic_parameters: None,
                            });
                        }
                    }
                }

                if let Some(variable) = scope.borrow().get_variable_from_new_name(name) {
                    let variable = variable.borrow();

                    // Variable should not be a storage field
                    if variable.is_storage {
                        panic!("error: Variable not found in scope: \"{name}\"");
                    }

                    return Ok(variable.type_name.clone());
                }

                if let Some(function) = scope
                    .borrow()
                    .find_function(|f| f.borrow().new_name == *name)
                {
                    return Ok(function.borrow().type_name.clone());
                }

                panic!("error: Variable not found in scope: \"{name}\"");
            }

            sway::Expression::FunctionCall(_) | sway::Expression::FunctionCallBlock(_) => {
                let (function, function_generic_parameters, parameters) = match expression {
                    sway::Expression::FunctionCall(f) => {
                        (&f.function, f.generic_parameters.as_ref(), &f.parameters)
                    }
                    sway::Expression::FunctionCallBlock(f) => {
                        (&f.function, f.generic_parameters.as_ref(), &f.parameters)
                    }
                    _ => unreachable!(),
                };

                match function {
                    sway::Expression::Identifier(name) => match name.as_str() {
                        "__size_of" => Ok(sway::TypeName::Identifier {
                            name: "u64".into(),
                            generic_parameters: None,
                        }),

                        "todo!" => Ok(sway::TypeName::Identifier {
                            name: "todo!".into(),
                            generic_parameters: None,
                        }),

                        "abi" => {
                            if parameters.len() != 2 {
                                panic!(
                                    "Malformed abi cast, expected 2 parameters, found {}",
                                    parameters.len()
                                );
                            }

                            let sway::Expression::Identifier(definition_name) = &parameters[0]
                            else {
                                panic!(
                                    "Malformed abi cast, expected identifier, found {:#?}",
                                    parameters[0]
                                );
                            };

                            Ok(sway::TypeName::Identifier {
                                name: definition_name.clone(),
                                generic_parameters: None,
                            })
                        }

                        "AssetId::default" => Ok(sway::TypeName::Identifier {
                            name: "AssetId".into(),
                            generic_parameters: None,
                        }),

                        "b256::from" => Ok(sway::TypeName::Identifier {
                            name: "b256".into(),
                            generic_parameters: None,
                        }),

                        "b256::from_be_bytes" | "b256::from_le_bytes" => {
                            Ok(sway::TypeName::Identifier {
                                name: "b256".into(),
                                generic_parameters: None,
                            })
                        }

                        "Bytes::new" | "Bytes::from" | "Bytes::with_capacity" => {
                            Ok(sway::TypeName::Identifier {
                                name: "Bytes".into(),
                                generic_parameters: None,
                            })
                        }

                        "I8::from" | "I8::from_uint" | "I8::max" | "I8::min" => {
                            Ok(sway::TypeName::Identifier {
                                name: "I8".into(),
                                generic_parameters: None,
                            })
                        }

                        "I16::from" | "I16::from_uint" | "I16::max" | "I16::min" => {
                            Ok(sway::TypeName::Identifier {
                                name: "I16".into(),
                                generic_parameters: None,
                            })
                        }

                        "I32::from" | "I32::from_uint" | "I32::max" | "I32::min" => {
                            Ok(sway::TypeName::Identifier {
                                name: "I32".into(),
                                generic_parameters: None,
                            })
                        }

                        "I64::from" | "I64::from_uint" | "I64::max" | "I64::min" => {
                            Ok(sway::TypeName::Identifier {
                                name: "I64".into(),
                                generic_parameters: None,
                            })
                        }

                        "I128::from" | "I128::from_uint" | "I128::max" | "I128::min" => {
                            Ok(sway::TypeName::Identifier {
                                name: "I128".into(),
                                generic_parameters: None,
                            })
                        }

                        "I256::from" | "I256::from_uint" | "I256::max" | "I256::min" => {
                            Ok(sway::TypeName::Identifier {
                                name: "I256".into(),
                                generic_parameters: None,
                            })
                        }

                        "Identity::Address" | "Identity::ContractId" | "Identity::from" => {
                            Ok(sway::TypeName::Identifier {
                                name: "Identity".into(),
                                generic_parameters: None,
                            })
                        }

                        "msg_sender" => Ok(sway::TypeName::Identifier {
                            name: "Option".into(),
                            generic_parameters: Some(sway::GenericParameterList {
                                entries: vec![sway::GenericParameter {
                                    type_name: sway::TypeName::Identifier {
                                        name: "Identity".into(),
                                        generic_parameters: None,
                                    },
                                    implements: None,
                                }],
                            }),
                        }),

                        "raw_slice::from_parts" => Ok(sway::TypeName::Identifier {
                            name: "raw_slice".into(),
                            generic_parameters: None,
                        }),

                        "std::alloc::alloc" => Ok(sway::TypeName::Identifier {
                            name: "raw_ptr".into(),
                            generic_parameters: None,
                        }),

                        "std::block::height" => Ok(sway::TypeName::Identifier {
                            name: "u32".into(),
                            generic_parameters: None,
                        }),

                        "std::block::timestamp" => Ok(sway::TypeName::Identifier {
                            name: "u64".into(),
                            generic_parameters: None,
                        }),

                        "std::context::balance_of" => Ok(sway::TypeName::Identifier {
                            name: "u64".into(),
                            generic_parameters: None,
                        }),

                        "std::context::msg_amount" => Ok(sway::TypeName::Identifier {
                            name: "u64".into(),
                            generic_parameters: None,
                        }),

                        "std::context::this_balance" => Ok(sway::TypeName::Identifier {
                            name: "u64".into(),
                            generic_parameters: None,
                        }),

                        "std::hash::keccak256" => Ok(sway::TypeName::Identifier {
                            name: "b256".into(),
                            generic_parameters: None,
                        }),

                        "std::hash::sha256" => Ok(sway::TypeName::Identifier {
                            name: "b256".into(),
                            generic_parameters: None,
                        }),

                        "std::inputs::input_message_data" => Ok(sway::TypeName::Identifier {
                            name: "Option".into(),
                            generic_parameters: Some(sway::GenericParameterList {
                                entries: vec![sway::GenericParameter {
                                    type_name: sway::TypeName::Identifier {
                                        name: "Bytes".into(),
                                        generic_parameters: None,
                                    },
                                    implements: None,
                                }],
                            }),
                        }),

                        "std::registers::return_length" => Ok(sway::TypeName::Identifier {
                            name: "u64".into(),
                            generic_parameters: None,
                        }),

                        "String::from_ascii" => Ok(sway::TypeName::Identifier {
                            name: "String".into(),
                            generic_parameters: None,
                        }),

                        "u8::from" | "u8::max" | "u8::min" | "u8::from_be_bytes"
                        | "u8::from_le_bytes" => Ok(sway::TypeName::Identifier {
                            name: "u8".into(),
                            generic_parameters: None,
                        }),

                        "u8::try_from" => Ok(sway::TypeName::Identifier {
                            name: "Option".into(),
                            generic_parameters: Some(sway::GenericParameterList {
                                entries: vec![sway::GenericParameter {
                                    type_name: sway::TypeName::Identifier {
                                        name: "u8".into(),
                                        generic_parameters: None,
                                    },
                                    implements: None,
                                }],
                            }),
                        }),

                        "u16::from" | "u16::max" | "u16::min" | "u16::from_be_bytes"
                        | "u16::from_le_bytes" => Ok(sway::TypeName::Identifier {
                            name: "u16".into(),
                            generic_parameters: None,
                        }),

                        "u16::try_from" => Ok(sway::TypeName::Identifier {
                            name: "Option".into(),
                            generic_parameters: Some(sway::GenericParameterList {
                                entries: vec![sway::GenericParameter {
                                    type_name: sway::TypeName::Identifier {
                                        name: "u16".into(),
                                        generic_parameters: None,
                                    },
                                    implements: None,
                                }],
                            }),
                        }),

                        "u32::from" | "u32::max" | "u32::min" | "u32::from_be_bytes"
                        | "u32::from_le_bytes" => Ok(sway::TypeName::Identifier {
                            name: "u32".into(),
                            generic_parameters: None,
                        }),

                        "u32::try_from" => Ok(sway::TypeName::Identifier {
                            name: "Option".into(),
                            generic_parameters: Some(sway::GenericParameterList {
                                entries: vec![sway::GenericParameter {
                                    type_name: sway::TypeName::Identifier {
                                        name: "u32".into(),
                                        generic_parameters: None,
                                    },
                                    implements: None,
                                }],
                            }),
                        }),

                        "u64::from" | "u64::max" | "u64::min" | "u64::from_be_bytes"
                        | "u64::from_le_bytes" => Ok(sway::TypeName::Identifier {
                            name: "u64".into(),
                            generic_parameters: None,
                        }),

                        "u64::try_from" => Ok(sway::TypeName::Identifier {
                            name: "Option".into(),
                            generic_parameters: Some(sway::GenericParameterList {
                                entries: vec![sway::GenericParameter {
                                    type_name: sway::TypeName::Identifier {
                                        name: "u64".into(),
                                        generic_parameters: None,
                                    },
                                    implements: None,
                                }],
                            }),
                        }),

                        "u256::from"
                        | "u256::max"
                        | "u256::min"
                        | "u256::from_be_bytes"
                        | "u256::from_le_bytes" => Ok(sway::TypeName::Identifier {
                            name: "u256".into(),
                            generic_parameters: None,
                        }),

                        "u256::try_from" => Ok(sway::TypeName::Identifier {
                            name: "Option".into(),
                            generic_parameters: Some(sway::GenericParameterList {
                                entries: vec![sway::GenericParameter {
                                    type_name: sway::TypeName::Identifier {
                                        name: "u256".into(),
                                        generic_parameters: None,
                                    },
                                    implements: None,
                                }],
                            }),
                        }),

                        "U128::from" | "U128::max" | "U128::min" => {
                            Ok(sway::TypeName::Identifier {
                                name: "U128".into(),
                                generic_parameters: None,
                            })
                        }

                        "U128::try_from" => Ok(sway::TypeName::Identifier {
                            name: "Option".into(),
                            generic_parameters: Some(sway::GenericParameterList {
                                entries: vec![sway::GenericParameter {
                                    type_name: sway::TypeName::Identifier {
                                        name: "U128".into(),
                                        generic_parameters: None,
                                    },
                                    implements: None,
                                }],
                            }),
                        }),

                        "U256::from" | "U256::max" | "U256::min" => {
                            Ok(sway::TypeName::Identifier {
                                name: "U256".into(),
                                generic_parameters: None,
                            })
                        }

                        "U256::try_from" => Ok(sway::TypeName::Identifier {
                            name: "Option".into(),
                            generic_parameters: Some(sway::GenericParameterList {
                                entries: vec![sway::GenericParameter {
                                    type_name: sway::TypeName::Identifier {
                                        name: "U256".into(),
                                        generic_parameters: None,
                                    },
                                    implements: None,
                                }],
                            }),
                        }),

                        "Vec::with_capacity" => Ok(sway::TypeName::Identifier {
                            name: "Vec".into(),
                            generic_parameters: Some(sway::GenericParameterList {
                                entries: vec![sway::GenericParameter {
                                    type_name: sway::TypeName::Identifier {
                                        name: "_".into(),
                                        generic_parameters: None,
                                    },
                                    implements: None,
                                }],
                            }),
                        }),

                        new_name => {
                            let parameter_types = parameters
                                .iter()
                                .map(|p| self.get_expression_type(scope.clone(), p))
                                .collect::<Result<Vec<_>, _>>()?;

                            // Ensure the function exists in scope
                            if let Some(function) = scope.borrow().find_function(|f| {
                                let f = f.borrow();

                                let sway::TypeName::Function {
                                    parameters: fn_parameters,
                                    ..
                                } = &f.type_name
                                else {
                                    panic!("Invalid function type name: {:#?}", f.type_name)
                                };

                                // Ensure the function's new name matches the function call we're translating
                                if f.new_name != new_name {
                                    return false;
                                }

                                // println!("found function \"{new_name}\"");

                                // Ensure the supplied function call args match the function's parameters
                                if parameters.len() != fn_parameters.entries.len() {
                                    // println!(
                                    //     "parameter count mismatch: expected {}, got {}; skipping...",
                                    //     f.parameters.entries.len(),
                                    //     parameters.len(),
                                    // );
                                    return false;
                                }

                                for (i, value_type_name) in parameter_types.iter().enumerate() {
                                    let Some(parameter_type_name) =
                                        fn_parameters.entries[i].type_name.as_ref()
                                    else {
                                        continue;
                                    };

                                    if !value_type_name.is_compatible_with(parameter_type_name) {
                                        // println!(
                                        //     "incompatible parameter type: expected {}, got {}; skipping...",
                                        //     sway::TabbedDisplayer(parameter_type_name),
                                        //     sway::TabbedDisplayer(value_type_name),
                                        // );
                                        return false;
                                    }
                                }

                                true
                            }) {
                                let function = function.borrow();

                                let sway::TypeName::Function { return_type, .. } =
                                    &function.type_name
                                else {
                                    panic!("Invalid function type name: {:#?}", function.type_name)
                                };

                                if let Some(return_type) = return_type.as_ref() {
                                    return Ok(return_type.as_ref().clone());
                                } else {
                                    return Ok(sway::TypeName::Tuple { type_names: vec![] });
                                }
                            } else if let Some(variable) = scope.borrow().find_variable(|v| {
                                let sway::TypeName::Function {
                                    parameters: fn_parameters,
                                    ..
                                } = &v.borrow().type_name
                                else {
                                    return false;
                                };

                                // Ensure the function's new name matches the function call we're translating
                                if v.borrow().new_name != *name {
                                    return false;
                                }

                                // Ensure the supplied function call args match the function's parameters
                                if parameters.len() != fn_parameters.entries.len() {
                                    return false;
                                }

                                for (i, value_type_name) in parameter_types.iter().enumerate() {
                                    let Some(parameter_type_name) =
                                        fn_parameters.entries[i].type_name.as_ref()
                                    else {
                                        continue;
                                    };

                                    if !value_type_name.is_compatible_with(parameter_type_name) {
                                        return false;
                                    }
                                }

                                true
                            }) {
                                let variable = variable.borrow();
                                let sway::TypeName::Function { return_type, .. } =
                                    &variable.type_name
                                else {
                                    unreachable!()
                                };

                                if let Some(return_type) = return_type.as_ref() {
                                    return Ok(return_type.as_ref().clone());
                                } else {
                                    return Ok(sway::TypeName::Tuple { type_names: vec![] });
                                }
                            }

                            panic!(
                                "Failed to find function or variable `{new_name}({})` in scope",
                                parameter_types
                                    .iter()
                                    .map(|t| t.to_string())
                                    .collect::<Vec<_>>()
                                    .join(", "),
                            );
                        }
                    },

                    sway::Expression::MemberAccess(member_access) => {
                        let mut container_type = self.get_expression_type(scope.clone(), &member_access.expression)?;

                        // Check to see if the container's type is a translated enum and switch to its underlying type
                        for enum_definition in self.enums.iter() {
                            if enum_definition.type_definition.name == container_type {
                                if let Some(underlying_type) = enum_definition.type_definition.underlying_type.as_ref() {
                                    container_type = underlying_type.clone();
                                    break;
                                }
                            }
                        }

                        // Check to see if the container's type is a UDT and switch to its underlying type
                        for type_definition in self.type_definitions.iter() {
                            if type_definition.name == container_type {
                                if let Some(underlying_type) = type_definition.underlying_type.as_ref() {
                                    container_type = underlying_type.clone();
                                    break;
                                }
                            }
                        }

                        match &container_type {
                            sway::TypeName::Undefined => panic!("Undefined type name"),

                            sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
                                ("b256", None) => match member_access.member.as_str() {
                                    "as_u256" => Ok(sway::TypeName::Identifier {
                                        name: "u256".into(),
                                        generic_parameters: None,
                                    }),

                                    "to_be_bytes" => Ok(sway::TypeName::Array {
                                        type_name: Box::new(sway::TypeName::Identifier {
                                            name: "u8".into(),
                                            generic_parameters: None,
                                        }),
                                        length: 32,
                                    }),

                                    "to_le_bytes" => Ok(sway::TypeName::Array {
                                        type_name: Box::new(sway::TypeName::Identifier {
                                            name: "u8".into(),
                                            generic_parameters: None,
                                        }),
                                        length: 32,
                                    }),

                                    _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                                }

                                ("Bytes", None) => match member_access.member.as_str() {
                                    "as_raw_slice" => Ok(sway::TypeName::Identifier {
                                        name: "raw_slice".into(),
                                        generic_parameters: None,
                                    }),

                                    "get" => Ok(sway::TypeName::Identifier {
                                        name: "Option".into(),
                                        generic_parameters: Some(sway::GenericParameterList {
                                            entries: vec![
                                                sway::GenericParameter {
                                                    type_name: sway::TypeName::Identifier {
                                                        name: "u8".into(),
                                                        generic_parameters: None,
                                                    },
                                                    implements: None,
                                                },
                                            ],
                                        }),
                                    }),

                                    "len" => Ok(sway::TypeName::Identifier {
                                        name: "u64".into(),
                                        generic_parameters: None,
                                    }),

                                    "ptr" => Ok(sway::TypeName::Identifier {
                                        name: "raw_ptr".into(),
                                        generic_parameters: None,
                                    }),

                                    "split_at" => Ok(sway::TypeName::Tuple {
                                        type_names: vec![
                                            sway::TypeName::Identifier {
                                                name: "Bytes".into(),
                                                generic_parameters: None,
                                            },
                                            sway::TypeName::Identifier {
                                                name: "Bytes".into(),
                                                generic_parameters: None,
                                            },
                                        ],
                                    }),

                                    _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                                }

                                ("I8", None) => match member_access.member.as_str() {
                                    "wrapping_neg" => Ok(sway::TypeName::Identifier {
                                        name: "I8".into(),
                                        generic_parameters: None,
                                    }),

                                    "underlying" => Ok(sway::TypeName::Identifier {
                                        name: "u8".into(),
                                        generic_parameters: None,
                                    }),

                                    _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                                }

                                ("I16", None) => match member_access.member.as_str() {
                                    "wrapping_neg" => Ok(sway::TypeName::Identifier {
                                        name: "I16".into(),
                                        generic_parameters: None,
                                    }),

                                    "underlying" => Ok(sway::TypeName::Identifier {
                                        name: "u16".into(),
                                        generic_parameters: None,
                                    }),

                                    _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                                }

                                ("I32", None) => match member_access.member.as_str() {
                                    "wrapping_neg" => Ok(sway::TypeName::Identifier {
                                        name: "I32".into(),
                                        generic_parameters: None,
                                    }),

                                    "underlying" => Ok(sway::TypeName::Identifier {
                                        name: "u32".into(),
                                        generic_parameters: None,
                                    }),

                                    _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                                }

                                ("I64", None) => match member_access.member.as_str() {
                                    "wrapping_neg" => Ok(sway::TypeName::Identifier {
                                        name: "I64".into(),
                                        generic_parameters: None,
                                    }),

                                    "underlying" => Ok(sway::TypeName::Identifier {
                                        name: "u64".into(),
                                        generic_parameters: None,
                                    }),

                                    _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                                }

                                ("I128", None) => match member_access.member.as_str() {
                                    "wrapping_neg" => Ok(sway::TypeName::Identifier {
                                        name: "I128".into(),
                                        generic_parameters: None,
                                    }),

                                    "underlying" => Ok(sway::TypeName::Identifier {
                                        name: "U128".into(),
                                        generic_parameters: None,
                                    }),

                                    _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                                }

                                ("I256", None) => match member_access.member.as_str() {
                                    "wrapping_neg" => Ok(sway::TypeName::Identifier {
                                        name: "I256".into(),
                                        generic_parameters: None,
                                    }),

                                    "underlying" => Ok(sway::TypeName::Identifier {
                                        name: "u256".into(),
                                        generic_parameters: None,
                                    }),

                                    _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                                }

                                ("Identity", None) => match member_access.member.as_str() {
                                    "as_address" => Ok(sway::TypeName::Identifier {
                                        name: "Option".into(),
                                        generic_parameters: Some(sway::GenericParameterList {
                                            entries: vec![
                                                sway::GenericParameter {
                                                    type_name: sway::TypeName::Identifier {
                                                        name: "Address".into(),
                                                        generic_parameters: None,
                                                    },
                                                    implements: None,
                                                },
                                            ],
                                        }),
                                    }),

                                    "as_contract_id" => Ok(sway::TypeName::Identifier {
                                        name: "Option".into(),
                                        generic_parameters: Some(sway::GenericParameterList {
                                            entries: vec![
                                                sway::GenericParameter {
                                                    type_name: sway::TypeName::Identifier {
                                                        name: "ContractId".into(),
                                                        generic_parameters: None,
                                                    },
                                                    implements: None,
                                                },
                                            ],
                                        }),
                                    }),

                                    "is_address" => Ok(sway::TypeName::Identifier {
                                        name: "bool".into(),
                                        generic_parameters: None,
                                    }),

                                    "is_contract_id" => Ok(sway::TypeName::Identifier {
                                        name: "bool".into(),
                                        generic_parameters: None,
                                    }),

                                    _ => Err(Error::Wrapped(Box::new(
                                        std::io::Error::new(
                                            std::io::ErrorKind::NotFound,
                                            format!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                                        )
                                    ))),
                                }

                                ("Option", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => match member_access.member.as_str() {
                                    "is_none" | "is_some" if parameters.len() == 0 => Ok(sway::TypeName::Identifier {
                                        name: "bool".into(),
                                        generic_parameters: None,
                                    }),

                                    "unwrap" => Ok(generic_parameters.entries[0].type_name.clone()),
                                    "unwrap_or" => Ok(generic_parameters.entries[0].type_name.clone()),

                                    _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                                }

                                ("Result", Some(generic_parameters)) if generic_parameters.entries.len() == 2 => match member_access.member.as_str() {
                                    "unwrap" => Ok(generic_parameters.entries[0].type_name.clone()),

                                    _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                                }

                                ("raw_ptr", None) => match member_access.member.as_str() {
                                    "add" => Ok(sway::TypeName::Identifier {
                                        name: "raw_ptr".into(),
                                        generic_parameters: None,
                                    }),

                                    "read" => Ok(function_generic_parameters.unwrap().entries[0].type_name.clone()),

                                    _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                                }

                                ("raw_slice", None) => match member_access.member.as_str() {
                                    "ptr" => Ok(sway::TypeName::Identifier {
                                        name: "raw_ptr".into(),
                                        generic_parameters: None,
                                    }),

                                    _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                                }

                                ("StorageKey", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => match member_access.member.as_str() {
                                    "clear" => Ok(sway::TypeName::Identifier {
                                        name: "bool".into(),
                                        generic_parameters: None,
                                    }),

                                    "read" => Ok(generic_parameters.entries[0].type_name.clone()),

                                    "try_read" => Ok(sway::TypeName::Identifier {
                                        name: "Option".into(),
                                        generic_parameters: Some(sway::GenericParameterList {
                                            entries: vec![
                                                sway::GenericParameter {
                                                    type_name: generic_parameters.entries[0].type_name.clone(),
                                                    implements: None,
                                                },
                                            ],
                                        }),
                                    }),

                                    "write" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                    _ => match &generic_parameters.entries[0].type_name {
                                        sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
                                            ("StorageBytes", None) => match member_access.member.as_str() {
                                                "clear" => Ok(sway::TypeName::Identifier {
                                                    name: "bool".into(),
                                                    generic_parameters: None,
                                                }),

                                                "len" => Ok(sway::TypeName::Identifier {
                                                    name: "u64".into(),
                                                    generic_parameters: None,
                                                }),

                                                "read_slice" => Ok(sway::TypeName::Identifier {
                                                    name: "Option".into(),
                                                    generic_parameters: Some(sway::GenericParameterList {
                                                        entries: vec![
                                                            sway::GenericParameter {
                                                                type_name: sway::TypeName::Identifier {
                                                                    name: "Bytes".into(),
                                                                    generic_parameters: None,
                                                                },
                                                                implements: None,
                                                            },
                                                        ],
                                                    }),
                                                }),

                                                "write_slice" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                                _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                                            }

                                            ("StorageMap", Some(generic_parameters)) if generic_parameters.entries.len() == 2 => match member_access.member.as_str() {
                                                "get" => Ok(sway::TypeName::Identifier {
                                                    name: "StorageKey".into(),
                                                    generic_parameters: Some(sway::GenericParameterList {
                                                        entries: vec![
                                                            sway::GenericParameter {
                                                                type_name: generic_parameters.entries[1].type_name.clone(),
                                                                implements: None,
                                                            },
                                                        ],
                                                    }),
                                                }),

                                                "insert" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                                "remove" => Ok(sway::TypeName::Identifier {
                                                    name: "bool".into(),
                                                    generic_parameters: None,
                                                }),

                                                "try_insert" => Ok(sway::TypeName::Identifier {
                                                    name: "Result".into(),
                                                    generic_parameters: Some(sway::GenericParameterList {
                                                        entries: vec![
                                                            sway::GenericParameter {
                                                                type_name: generic_parameters.entries[0].type_name.clone(),
                                                                implements: None,
                                                            },
                                                            sway::GenericParameter {
                                                                type_name: sway::TypeName::Identifier {
                                                                    name: "StorageMapError".into(),
                                                                    generic_parameters: Some(sway::GenericParameterList {
                                                                        entries: vec![
                                                                            sway::GenericParameter {
                                                                                type_name: generic_parameters.entries[0].type_name.clone(),
                                                                                implements: None,
                                                                            },
                                                                        ],
                                                                    }),
                                                                },
                                                                implements: None,
                                                            },
                                                        ],
                                                    }),
                                                }),

                                                _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                                            }

                                            ("StorageString", None) => match member_access.member.as_str() {
                                                "clear" => Ok(sway::TypeName::Identifier {
                                                    name: "bool".into(),
                                                    generic_parameters: None,
                                                }),

                                                "len" => Ok(sway::TypeName::Identifier {
                                                    name: "u64".into(),
                                                    generic_parameters: None,
                                                }),

                                                "read_slice" => Ok(sway::TypeName::Identifier {
                                                    name: "Option".into(),
                                                    generic_parameters: Some(sway::GenericParameterList {
                                                        entries: vec![
                                                            sway::GenericParameter {
                                                                type_name: sway::TypeName::Identifier {
                                                                    name: "String".into(),
                                                                    generic_parameters: None,
                                                                },
                                                                implements: None,
                                                            },
                                                        ],
                                                    }),
                                                }),

                                                "write_slice" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                                _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                                            }

                                            ("StorageVec", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => match member_access.member.as_str() {
                                                "fill" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                                "first" => Ok(sway::TypeName::Identifier {
                                                    name: "Option".into(),
                                                    generic_parameters: Some(sway::GenericParameterList {
                                                        entries: vec![
                                                            sway::GenericParameter {
                                                                type_name: sway::TypeName::Identifier {
                                                                    name: "StorageKey".into(),
                                                                    generic_parameters: Some(sway::GenericParameterList {
                                                                        entries: vec![
                                                                            sway::GenericParameter {
                                                                                type_name: generic_parameters.entries[0].type_name.clone(),
                                                                                implements: None,
                                                                            },
                                                                        ],
                                                                    }),
                                                                },
                                                                implements: None,
                                                            },
                                                        ],
                                                    }),
                                                }),

                                                "get" => Ok(sway::TypeName::Identifier {
                                                    name: "Option".into(),
                                                    generic_parameters: Some(sway::GenericParameterList {
                                                        entries: vec![
                                                            sway::GenericParameter {
                                                                type_name: sway::TypeName::Identifier {
                                                                    name: "StorageKey".into(),
                                                                    generic_parameters: Some(sway::GenericParameterList {
                                                                        entries: vec![
                                                                            sway::GenericParameter {
                                                                                type_name: generic_parameters.entries[0].type_name.clone(),
                                                                                implements: None,
                                                                            },
                                                                        ],
                                                                    }),
                                                                },
                                                                implements: None,
                                                            },
                                                        ],
                                                    }),
                                                }),

                                                "insert" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                                "is_empty" => Ok(sway::TypeName::Identifier {
                                                    name: "bool".into(),
                                                    generic_parameters: None,
                                                }),

                                                "last" => Ok(sway::TypeName::Identifier {
                                                    name: "Option".into(),
                                                    generic_parameters: Some(sway::GenericParameterList {
                                                        entries: vec![
                                                            sway::GenericParameter {
                                                                type_name: sway::TypeName::Identifier {
                                                                    name: "StorageKey".into(),
                                                                    generic_parameters: Some(sway::GenericParameterList {
                                                                        entries: vec![
                                                                            sway::GenericParameter {
                                                                                type_name: generic_parameters.entries[0].type_name.clone(),
                                                                                implements: None,
                                                                            },
                                                                        ],
                                                                    }),
                                                                },
                                                                implements: None,
                                                            },
                                                        ],
                                                    }),
                                                }),

                                                "len" => Ok(sway::TypeName::Identifier {
                                                    name: "u64".into(),
                                                    generic_parameters: None,
                                                }),

                                                "load_vec" => Ok(sway::TypeName::Identifier {
                                                    name: "Vec".into(),
                                                    generic_parameters: Some(sway::GenericParameterList {
                                                        entries: vec![
                                                            sway::GenericParameter {
                                                                type_name: generic_parameters.entries[0].type_name.clone(),
                                                                implements: None,
                                                            },
                                                        ],
                                                    }),
                                                }),

                                                "pop" => Ok(sway::TypeName::Identifier {
                                                    name: "Option".into(),
                                                    generic_parameters: Some(sway::GenericParameterList {
                                                        entries: vec![
                                                            sway::GenericParameter {
                                                                type_name: generic_parameters.entries[0].type_name.clone(),
                                                                implements: None,
                                                            },
                                                        ],
                                                    }),
                                                }),

                                                "push" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                                "remove" => Ok(generic_parameters.entries[0].type_name.clone()),

                                                "resize" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                                "reverse" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                                "set" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                                "store_vec" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                                "swap_remove" => Ok(generic_parameters.entries[0].type_name.clone()),

                                                "swap" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                                _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                                            }

                                            (name, _) => todo!("get type of {name}::{} function call expression: {} - {expression:#?}", member_access.member, sway::TabbedDisplayer(expression)),
                                        }

                                        _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                                    }
                                }

                                ("StorageMap", Some(generic_parameters)) if generic_parameters.entries.len() == 2 => match member_access.member.as_str() {
                                    "get" if parameters.len() == 1 => Ok(sway::TypeName::Identifier {
                                        name: "StorageKey".to_string(),
                                        generic_parameters: Some(sway::GenericParameterList {
                                            entries : vec![generic_parameters.entries[1].clone()]
                                        }),
                                    }),
                                    _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression))
                                }

                                ("String", None) => match member_access.member.as_str() {
                                    "len" => Ok(sway::TypeName::Identifier {
                                        name: "u64".into(),
                                        generic_parameters: None,
                                    }),

                                    _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                                }

                                ("u8", None) => match member_access.member.as_str() {
                                    "as_u16" => Ok(sway::TypeName::Identifier {
                                        name: "u16".into(),
                                        generic_parameters: None,
                                    }),

                                    "as_u32" => Ok(sway::TypeName::Identifier {
                                        name: "u32".into(),
                                        generic_parameters: None,
                                    }),

                                    "as_u64" => Ok(sway::TypeName::Identifier {
                                        name: "u64".into(),
                                        generic_parameters: None,
                                    }),

                                    "as_u256" => Ok(sway::TypeName::Identifier {
                                        name: "u256".into(),
                                        generic_parameters: None,
                                    }),

                                    "pow" => Ok(sway::TypeName::Identifier {
                                        name: "u8".into(),
                                        generic_parameters: None,
                                    }),

                                    _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                                }

                                ("u16", None) => match member_access.member.as_str() {
                                    "as_u32" => Ok(sway::TypeName::Identifier {
                                        name: "u32".into(),
                                        generic_parameters: None,
                                    }),

                                    "as_u64" => Ok(sway::TypeName::Identifier {
                                        name: "u64".into(),
                                        generic_parameters: None,
                                    }),

                                    "as_u256" => Ok(sway::TypeName::Identifier {
                                        name: "u256".into(),
                                        generic_parameters: None,
                                    }),

                                    "pow" => Ok(sway::TypeName::Identifier {
                                        name: "u16".into(),
                                        generic_parameters: None,
                                    }),

                                    "to_be_bytes" => Ok(sway::TypeName::Array {
                                        type_name: Box::new(sway::TypeName::Identifier {
                                            name: "u8".into(),
                                            generic_parameters: None,
                                        }),
                                        length: 2,
                                    }),

                                    "to_le_bytes" => Ok(sway::TypeName::Array {
                                        type_name: Box::new(sway::TypeName::Identifier {
                                            name: "u8".into(),
                                            generic_parameters: None,
                                        }),
                                        length: 2,
                                    }),

                                    _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                                }

                                ("u32", None) => match member_access.member.as_str() {
                                    "as_u64" => Ok(sway::TypeName::Identifier {
                                        name: "u64".into(),
                                        generic_parameters: None,
                                    }),

                                    "as_u256" => Ok(sway::TypeName::Identifier {
                                        name: "u256".into(),
                                        generic_parameters: None,
                                    }),

                                    "pow" => Ok(sway::TypeName::Identifier {
                                        name: "u32".into(),
                                        generic_parameters: None,
                                    }),

                                    "to_be_bytes" => Ok(sway::TypeName::Array {
                                        type_name: Box::new(sway::TypeName::Identifier {
                                            name: "u8".into(),
                                            generic_parameters: None,
                                        }),
                                        length: 4,
                                    }),

                                    "to_le_bytes" => Ok(sway::TypeName::Array {
                                        type_name: Box::new(sway::TypeName::Identifier {
                                            name: "u8".into(),
                                            generic_parameters: None,
                                        }),
                                        length: 4,
                                    }),

                                    _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                                }

                                ("u64", None) => match member_access.member.as_str() {
                                    "as_u256" => Ok(sway::TypeName::Identifier {
                                        name: "u256".into(),
                                        generic_parameters: None,
                                    }),

                                    "pow" => Ok(sway::TypeName::Identifier {
                                        name: "u64".into(),
                                        generic_parameters: None,
                                    }),

                                    "to_be_bytes" => Ok(sway::TypeName::Array {
                                        type_name: Box::new(sway::TypeName::Identifier {
                                            name: "u8".into(),
                                            generic_parameters: None,
                                        }),
                                        length: 8,
                                    }),

                                    "to_le_bytes" => Ok(sway::TypeName::Array {
                                        type_name: Box::new(sway::TypeName::Identifier {
                                            name: "u8".into(),
                                            generic_parameters: None,
                                        }),
                                        length: 8,
                                    }),

                                    "wrapping_neg" => {
                                        self.ensure_dependency_declared(
                                            "sway_libs = { git = \"https://github.com/FuelLabs/sway-libs\", tag = \"v0.25.0\" }"
                                        );

                                        Ok(sway::TypeName::Identifier {
                                            name: {
                                                self.ensure_use_declared("sway_libs::signed_integers::i64::*");
                                                "I64".into()
                                            },
                                            generic_parameters: None,
                                        })
                                    }

                                    _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                                }

                                ("u256", None) => match member_access.member.as_str() {
                                    "as_b256" => Ok(sway::TypeName::Identifier {
                                        name: "b256".into(),
                                        generic_parameters: None,
                                    }),

                                    "pow" => Ok(sway::TypeName::Identifier {
                                        name: "u256".into(),
                                        generic_parameters: None,
                                    }),

                                    "to_be_bytes" => Ok(sway::TypeName::Array {
                                        type_name: Box::new(sway::TypeName::Identifier {
                                            name: "u8".into(),
                                            generic_parameters: None,
                                        }),
                                        length: 32,
                                    }),

                                    "to_le_bytes" => Ok(sway::TypeName::Array {
                                        type_name: Box::new(sway::TypeName::Identifier {
                                            name: "u8".into(),
                                            generic_parameters: None,
                                        }),
                                        length: 32,
                                    }),

                                    _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                                }

                                ("Vec", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => match member_access.member.as_str() {
                                    "get" => Ok(sway::TypeName::Identifier {
                                        name: "Option".into(),
                                        generic_parameters: Some(sway::GenericParameterList {
                                            entries: vec![
                                                generic_parameters.entries.first().unwrap().clone(),
                                            ],
                                        }),
                                    }),

                                    "len" => Ok(sway::TypeName::Identifier {
                                        name: "u64".into(),
                                        generic_parameters: None,
                                    }),

                                    _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                                }

                                (name, None) => {
                                    if let Some(abi) = self.abi.as_ref() {
                                        if abi.name == name {
                                            if let Some(function_definition) = abi.functions.iter().find(|f| f.name == member_access.member) {
                                                return Ok(function_definition.return_type.clone().unwrap_or_else(|| sway::TypeName::Tuple { type_names: vec![] }))
                                            }
                                        }
                                    }

                                    for abi in self.abis.iter() {
                                        if abi.name == name {
                                            if let Some(function_definition) = abi.functions.iter().find(|f| f.name == member_access.member) {
                                                return Ok(function_definition.return_type.clone().unwrap_or_else(|| sway::TypeName::Tuple { type_names: vec![] }))
                                            }
                                        }
                                    }

                                    todo!("get type of {name} function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression))
                                }

                                _ => todo!("get type of {name} function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                            }

                            sway::TypeName::StringSlice => match member_access.member.as_str() {
                                "len" => Ok(sway::TypeName::Identifier {
                                    name: "u64".into(),
                                    generic_parameters: None,
                                }),

                                _ => todo!("get type of function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                            }

                            _ => todo!("get type of {container_type} function call expression: {} - {expression:#?}", sway::TabbedDisplayer(expression)),
                        }
                    }

                    _ => todo!(
                        "get type of function call expression: {} - {expression:#?}",
                        sway::TabbedDisplayer(expression)
                    ),
                }
            }

            sway::Expression::Block(block) => {
                let Some(expression) = block.final_expr.as_ref() else {
                    return Ok(sway::TypeName::Tuple { type_names: vec![] });
                };

                let inner_scope = Rc::new(RefCell::new(TranslationScope {
                    parent: Some(scope.clone()),
                    ..Default::default()
                }));

                for statement in block.statements.iter() {
                    let sway::Statement::Let(sway::Let {
                        pattern,
                        type_name,
                        value,
                    }) = statement
                    else {
                        continue;
                    };

                    let type_name = match type_name.as_ref() {
                        Some(type_name) => type_name.clone(),
                        None => self.get_expression_type(inner_scope.clone(), value)?,
                    };

                    let add_variable = |id: &sway::LetIdentifier, type_name: &sway::TypeName| {
                        inner_scope
                            .borrow_mut()
                            .variables
                            .push(Rc::new(RefCell::new(TranslatedVariable {
                                old_name: String::new(),
                                new_name: id.name.clone(),
                                type_name: type_name.clone(),
                                ..Default::default()
                            })));
                    };

                    match pattern {
                        sway::LetPattern::Identifier(id) => add_variable(id, &type_name),
                        sway::LetPattern::Tuple(ids) => {
                            let sway::TypeName::Tuple { type_names } = &type_name else {
                                panic!("Expected tuple type, found {type_name}");
                            };

                            for (id, type_name) in ids.iter().zip(type_names.iter()) {
                                add_variable(id, type_name);
                            }
                        }
                    }
                }

                self.get_expression_type(inner_scope, expression)
            }

            sway::Expression::Return(value) => {
                if let Some(value) = value.as_ref() {
                    self.get_expression_type(scope.clone(), value)
                } else {
                    Ok(sway::TypeName::Tuple { type_names: vec![] })
                }
            }

            sway::Expression::Array(array) => Ok(sway::TypeName::Array {
                type_name: Box::new(if let Some(expression) = array.elements.first() {
                    self.get_expression_type(scope.clone(), expression)?
                } else {
                    sway::TypeName::Tuple { type_names: vec![] }
                }),
                length: array.elements.len(),
            }),

            sway::Expression::ArrayAccess(array_access) => {
                let element_type_name =
                    self.get_expression_type(scope.clone(), &array_access.expression)?;

                let type_name = match &element_type_name {
                    sway::TypeName::Identifier {
                        name,
                        generic_parameters: Some(generic_parameters),
                    } if name == "Vec" => &generic_parameters.entries.first().unwrap().type_name,

                    sway::TypeName::Array { type_name, .. } => type_name.as_ref(),

                    _ => todo!("array access for type {element_type_name}"),
                };

                Ok(type_name.clone())
            }

            sway::Expression::MemberAccess(member_access) => match &member_access.expression {
                sway::Expression::Identifier(name) => match name.as_str() {
                    "storage" => {
                        let Some(variable) = scope.borrow().find_variable(|v| {
                            v.borrow().is_storage && v.borrow().new_name == member_access.member
                        }) else {
                            panic!(
                                "Failed to find storage variable in scope: `{}`",
                                member_access.member
                            );
                        };

                        let variable = variable.borrow();

                        Ok(sway::TypeName::Identifier {
                            name: "StorageKey".into(),
                            generic_parameters: Some(sway::GenericParameterList {
                                entries: vec![sway::GenericParameter {
                                    type_name: variable.type_name.clone(),
                                    implements: None,
                                }],
                            }),
                        })
                    }

                    _ => {
                        let container_type =
                            self.get_expression_type(scope.clone(), &member_access.expression)?;

                        match &container_type {
                            sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
                                ("I8", None) => match member_access.member.as_str() {
                                    "underlying" => Ok(sway::TypeName::Identifier {
                                        name: "u8".into(),
                                        generic_parameters: None,
                                    }),

                                    _ => todo!("get type of {container_type} member access expression: {expression:#?}"),
                                }

                                ("I16", None) => match member_access.member.as_str() {
                                    "underlying" => Ok(sway::TypeName::Identifier {
                                        name: "u16".into(),
                                        generic_parameters: None,
                                    }),

                                    _ => todo!("get type of {container_type} member access expression: {expression:#?}"),
                                }

                                ("I32", None) => match member_access.member.as_str() {
                                    "underlying" => Ok(sway::TypeName::Identifier {
                                        name: "u32".into(),
                                        generic_parameters: None,
                                    }),

                                    _ => todo!("get type of {container_type} member access expression: {expression:#?}"),
                                }

                                ("I64", None) => match member_access.member.as_str() {
                                    "underlying" => Ok(sway::TypeName::Identifier {
                                        name: "u64".into(),
                                        generic_parameters: None,
                                    }),

                                    _ => todo!("get type of {container_type} member access expression: {expression:#?}"),
                                }

                                ("I128", None) => match member_access.member.as_str() {
                                    "underlying" => Ok(sway::TypeName::Identifier {
                                        name: "U128".into(),
                                        generic_parameters: None,
                                    }),

                                    _ => todo!("get type of {container_type} member access expression: {expression:#?}"),
                                }

                                ("I256", None) => match member_access.member.as_str() {
                                    "underlying" => Ok(sway::TypeName::Identifier {
                                        name: "u256".into(),
                                        generic_parameters: None,
                                    }),

                                    _ => todo!("get type of {container_type} member access expression: {expression:#?}"),
                                }

                                _ => {
                                    // Check if container is a struct
                                    if let Some(struct_definition) = self.structs.iter().find(|s| s.borrow().name == *name) {
                                        if let Some(field) = struct_definition.borrow().fields.iter().find(|f| f.name == member_access.member) {
                                            return Ok(field.type_name.clone());
                                        }
                                    }

                                    todo!("get type of {container_type} member access expression: {expression:#?}")
                                }
                            }

                            _ => todo!("get type of {container_type} member access expression: {expression:#?}"),
                        }
                    }
                },

                _ => {
                    let type_name =
                        self.get_expression_type(scope.clone(), &member_access.expression)?;
                    let type_name_string = type_name.to_string();

                    // Check to see if container is a built-in type
                    match &type_name {
                        sway::TypeName::Identifier {
                            name,
                            generic_parameters,
                        } => match (name.as_str(), generic_parameters.as_ref()) {
                            ("I256", None) => match member_access.member.as_str() {
                                "underlying" => {
                                    return Ok(sway::TypeName::Identifier {
                                        name: "u256".into(),
                                        generic_parameters: None,
                                    })
                                }

                                _ => {}
                            },

                            _ => {}
                        },

                        _ => {}
                    }

                    // Check to see if container is a struct
                    if let Some(struct_definition) = self
                        .structs
                        .iter()
                        .find(|s| s.borrow().name == type_name_string)
                    {
                        let struct_definition = struct_definition.borrow();

                        let Some(field) = struct_definition
                            .fields
                            .iter()
                            .find(|f| f.name == member_access.member)
                        else {
                            panic!("{type_name} does not contain a field named \"{}\" - member access expression: {:#?}", member_access.member, member_access.expression)
                        };

                        return Ok(field.type_name.clone());
                    }

                    todo!(
                        "get type of {type_name} member access expression: {:#?}",
                        expression
                    )
                }
            },

            sway::Expression::Tuple(tuple) => {
                if tuple.len() == 1 {
                    self.get_expression_type(scope.clone(), tuple.first().unwrap())
                } else {
                    Ok(sway::TypeName::Tuple {
                        type_names: tuple
                            .iter()
                            .map(|x| self.get_expression_type(scope.clone(), x))
                            .collect::<Result<Vec<_>, _>>()?,
                    })
                }
            }

            sway::Expression::If(if_expr) => {
                if let Some(expression) = if_expr.then_body.final_expr.as_ref() {
                    self.get_expression_type(scope.clone(), expression)
                } else {
                    Ok(sway::TypeName::Tuple { type_names: vec![] })
                }
            }

            sway::Expression::Match(match_expr) => {
                if let Some(branch) = match_expr.branches.first() {
                    self.get_expression_type(scope.clone(), &branch.value)
                } else {
                    Ok(sway::TypeName::Tuple { type_names: vec![] })
                }
            }

            sway::Expression::While(_) => Ok(sway::TypeName::Tuple { type_names: vec![] }),
            sway::Expression::UnaryExpression(unary_expression) => {
                self.get_expression_type(scope.clone(), &unary_expression.expression)
            }

            sway::Expression::BinaryExpression(binary_expression) => {
                match binary_expression.operator.as_str() {
                    "==" | "!=" | ">" | "<" | ">=" | "<=" | "&&" | "||" => {
                        Ok(sway::TypeName::Identifier {
                            name: "bool".into(),
                            generic_parameters: None,
                        })
                    }

                    _ => self.get_expression_type(scope.clone(), &binary_expression.lhs),
                }
            }

            sway::Expression::Constructor(constructor) => Ok(constructor.type_name.clone()),
            sway::Expression::Continue => Ok(sway::TypeName::Tuple { type_names: vec![] }),
            sway::Expression::Break => Ok(sway::TypeName::Tuple { type_names: vec![] }),

            sway::Expression::AsmBlock(asm_block) => match asm_block.final_expression.as_ref() {
                Some(expression) => match expression.type_name.as_ref() {
                    Some(type_name) => Ok(type_name.clone()),
                    None => todo!(),
                },
                None => todo!(),
            },

            sway::Expression::Commented(_, x) => self.get_expression_type(scope.clone(), x),
        }
    }
}
