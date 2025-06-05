use crate::{error::Error, project::Project, sway};
use convert_case::{Case, Casing};
use num_bigint::BigUint;
use num_traits::{One, Zero};
use solang_parser::pt as solidity;
use std::{cell::RefCell, collections::HashMap, path::PathBuf, rc::Rc};

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
pub use assembly::*;
pub use contracts::*;
pub use enums::*;
pub use expressions::*;
pub use functions::*;
pub use import_directives::*;
pub use statements::*;
pub use storage::*;
pub use structs::*;
pub use type_definitions::*;
pub use type_names::*;

#[derive(Clone, Debug, PartialEq)]
pub struct TranslatedUsingDirective {
    pub library_name: String,
    pub for_type: Option<sway::TypeName>,
    pub functions: Vec<String>,
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
    pub statement_index: Option<usize>,
    pub read_count: usize,
    pub mutation_count: usize,
}

#[derive(Clone, Debug)]
pub struct TranslatedVariableAccess {
    pub variable: Option<Rc<RefCell<TranslatedVariable>>>,
    pub expression: sway::Expression,
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

#[derive(Default, Debug, Clone)]
pub struct TranslationScope {
    pub parent: Option<Rc<RefCell<TranslationScope>>>,
    pub variables: Vec<Rc<RefCell<TranslatedVariable>>>,
}

impl TranslationScope {
    pub fn dump(&self) {
        if let Some(parent) = self.parent.as_ref() {
            parent.borrow().dump();
        }

        // println!("variables:");

        for v in self.variables.iter() {
            // println!("{v:#?}");
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
}

#[derive(Clone, Debug)]
pub struct TranslatedItem<T> {
    pub signature: sway::TypeName,
    pub implementation: Option<T>,
}

#[derive(Clone, Debug)]
pub struct DeferredInitialization {
    pub name: String,
    pub is_storage: bool,
    pub is_constant: bool,
    pub is_configurable: bool,
    pub value: sway::Expression,
}

#[derive(Clone, Debug)]
pub struct TranslatedContract {
    pub name: String,
    pub kind: solidity::ContractTy,
    pub abi: sway::Abi,
    pub abi_impl: sway::Impl,
}

impl TranslatedContract {
    pub fn new(name: &str, kind: solidity::ContractTy, inherits: &[sway::TypeName]) -> Self {
        Self {
            name: name.to_string(),
            kind,
            abi: sway::Abi {
                name: name.to_string(),
                inherits: inherits.to_vec(),
                functions: vec![],
            },
            abi_impl: sway::Impl {
                generic_parameters: None,
                type_name: sway::TypeName::Identifier {
                    name: name.to_string(),
                    generic_parameters: None,
                },
                for_type_name: Some(sway::TypeName::Identifier {
                    name: "Contract".to_string(),
                    generic_parameters: None,
                }),
                items: vec![],
            },
        }
    }
}

pub enum Symbol {
    TypeDefinition(String),
    Event(String),
    Enum(String),
    Error(String),
    Struct(String),
    Function(String),
    Abi(String),
}

#[derive(Default, Debug, Clone)]
pub struct TranslatedModule {
    pub name: String,
    pub path: PathBuf,
    pub submodules: Vec<Rc<RefCell<TranslatedModule>>>,
    pub dependencies: Vec<String>,

    pub uses: Vec<sway::Use>,
    pub using_directives: Vec<TranslatedUsingDirective>,
    pub type_definitions: Vec<TranslatedItem<sway::TypeDefinition>>,
    pub structs: Vec<TranslatedItem<Rc<RefCell<sway::Struct>>>>,
    pub enums: Vec<TranslatedItem<TranslatedEnum>>,
    pub events_enums: Vec<(Rc<RefCell<sway::Enum>>, Rc<RefCell<sway::Impl>>)>,
    pub errors_enums: Vec<(Rc<RefCell<sway::Enum>>, Rc<RefCell<sway::Impl>>)>,
    pub constants: Vec<sway::Constant>,
    pub configurable: Option<sway::Configurable>,
    pub storage: Option<sway::Storage>,
    pub modifiers: Vec<TranslatedModifier>,
    pub functions: Vec<TranslatedItem<sway::Function>>,
    pub contracts: Vec<TranslatedItem<TranslatedContract>>,
    pub impls: Vec<sway::Impl>,

    pub function_name_counts: HashMap<String, usize>,
    pub function_names: HashMap<String, String>,
    pub function_constructor_calls: HashMap<String, Vec<sway::FunctionCall>>,
    pub function_modifiers: HashMap<String, Vec<sway::FunctionCall>>,

    pub storage_fields_name_counts: HashMap<String, usize>,
    pub storage_fields_names: HashMap<String, String>,
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
            self.storage = Some(sway::Storage::default());
        }

        self.storage.as_mut().unwrap()
    }

    /// Attempt to get storage namespace by name from the translated definition. If it doesn't exist, it gets created.
    #[inline]
    pub fn get_storage_namespace(&mut self) -> &mut sway::StorageNamespace {
        let namespace_name = self.get_storage_namespace_name();
        let storage = self.get_storage();

        if !storage.namespaces.iter().any(|s| s.name == namespace_name) {
            storage.namespaces.push(sway::StorageNamespace {
                name: namespace_name.clone(),
                fields: vec![],
                namespaces: vec![],
            });
        }

        storage
            .namespaces
            .iter_mut()
            .find(|s| s.name == namespace_name)
            .unwrap()
    }

    /// Gets the name of the storage namespace from the translated definition.
    #[inline]
    pub fn get_storage_namespace_name(&self) -> String {
        translate_naming_convention(&self.name, Case::Snake)
    }

    /// Attempts to find the translated definitions `impl` block mutability, if available.
    #[inline]
    pub fn find_contract_impl_mut(&mut self, definition_name: &str) -> Option<&mut sway::Impl> {
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
            *type_name == definition_name && for_type_name == "Contract"
        })
    }

    /// Attempts to find the translated definition's `impl` block, if available.
    #[inline]
    pub fn find_contract_impl(&self, definition_name: &str) -> Option<&sway::Impl> {
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
            *type_name == definition_name && for_type_name == "Contract"
        })
    }

    #[inline]
    pub fn get_module_name(&self) -> String {
        translate_naming_convention(&self.name, Case::Snake)
    }

    /// Gets the base underlying type of the supplied type name
    pub fn get_underlying_type(&self, type_name: &sway::TypeName) -> sway::TypeName {
        // Check to see if the expression's type is a type definition and get the underlying type
        for type_definition in self.type_definitions.iter() {
            if &type_definition.implementation.as_ref().unwrap().name == type_name {
                return self.get_underlying_type(
                    type_definition
                        .implementation
                        .as_ref()
                        .unwrap()
                        .underlying_type
                        .as_ref()
                        .unwrap(),
                );
            }
        }

        // If we didn't find a type definition, check to see if an enum exists and get its underlying type
        for translated_enum in self.enums.iter() {
            if &translated_enum
                .implementation
                .as_ref()
                .unwrap()
                .type_definition
                .name
                == type_name
            {
                return self.get_underlying_type(
                    translated_enum
                        .implementation
                        .as_ref()
                        .unwrap()
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
        project: &mut Project,
        scope: Rc<RefCell<TranslationScope>>,
        expression: &sway::Expression,
    ) -> Result<sway::TypeName, Error> {
        match expression {
            sway::Expression::Literal(literal) => Ok(self.get_literal_type(literal)),
            sway::Expression::PathExpr(path_expr) => {
                Ok(self.get_path_expr_type(scope.clone(), path_expr))
            }
            sway::Expression::FunctionCall(_) | sway::Expression::FunctionCallBlock(_) => {
                self.get_function_call_type(project, scope.clone(), expression)
            }
            sway::Expression::Block(block) => self.get_block_type(project, scope.clone(), block),
            sway::Expression::Return(value) => {
                self.get_return_type(project, scope.clone(), value.as_deref())
            }
            sway::Expression::Array(array) => self.get_array_type(project, scope.clone(), array),
            sway::Expression::ArrayAccess(array_access) => {
                self.get_array_access_type(project, scope.clone(), array_access)
            }
            sway::Expression::MemberAccess(member_access) => {
                self.get_member_access_type(project, scope.clone(), member_access, expression)
            }
            sway::Expression::Tuple(tuple) => self.get_tuple_type(project, scope.clone(), tuple),
            sway::Expression::If(if_expr) => self.get_if_type(project, scope.clone(), if_expr),
            sway::Expression::Match(match_expr) => {
                self.get_match_type(project, scope.clone(), match_expr)
            }
            sway::Expression::While(_) => Ok(sway::TypeName::Tuple { type_names: vec![] }),
            sway::Expression::UnaryExpression(unary_expression) => {
                self.get_unary_expression_type(project, scope.clone(), unary_expression)
            }
            sway::Expression::BinaryExpression(binary_expression) => {
                self.get_binary_expression_type(project, scope.clone(), binary_expression)
            }
            sway::Expression::Constructor(constructor) => Ok(constructor.type_name.clone()),
            sway::Expression::Continue => Ok(sway::TypeName::Tuple { type_names: vec![] }),
            sway::Expression::Break => Ok(sway::TypeName::Tuple { type_names: vec![] }),
            sway::Expression::AsmBlock(asm_block) => self.get_asm_block_type(asm_block),
            sway::Expression::Commented(_, x) => {
                self.get_expression_type(project, scope.clone(), x)
            }
        }
    }

    #[inline(always)]
    fn get_literal_type(&mut self, literal: &sway::Literal) -> sway::TypeName {
        match literal {
            sway::Literal::Bool(_) => sway::TypeName::Identifier {
                name: "bool".into(),
                generic_parameters: None,
            },

            sway::Literal::DecInt(value, suffix) => sway::TypeName::Identifier {
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

                    format!("u{bits}")
                },
                generic_parameters: None,
            },

            sway::Literal::HexInt(value, suffix) => sway::TypeName::Identifier {
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

                    format!("u{bits}")
                },
                generic_parameters: None,
            },

            sway::Literal::String(_) => sway::TypeName::StringSlice,
        }
    }

    #[inline(always)]
    fn get_path_expr_type(
        &mut self,
        scope: Rc<RefCell<TranslationScope>>,
        path_expr: &sway::PathExpr,
    ) -> sway::TypeName {
        let Some(name) = path_expr.as_identifier() else {
            todo!("get type of non-identifier path expressions: {path_expr} - {path_expr:#?}")
        };

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
                    } = &e.implementation.as_ref().unwrap().type_definition.name
                    else {
                        return false;
                    };

                    if !e
                        .implementation
                        .as_ref()
                        .unwrap()
                        .variants_impl
                        .items
                        .iter()
                        .any(|i| {
                            let sway::ImplItem::Constant(variant) = i else {
                                return false;
                            };

                            variant.name == variant_name
                        })
                    {
                        return false;
                    }

                    name == enum_name
                }) {
                    return sway::TypeName::Identifier {
                        name: enum_name.into(),
                        generic_parameters: None,
                    };
                }
            }
        }

        if let Some(variable) = scope.borrow().get_variable_from_new_name(name) {
            let variable = variable.borrow();

            return variable.type_name.clone();
        }

        if let Some(function) = self.functions.iter().find(|f| {
            let sway::TypeName::Function { new_name, .. } = &f.signature else {
                unreachable!()
            };
            new_name == name
        }) {
            return function.signature.clone();
        }

        if let Some(constant) = self.constants.iter().find(|c| c.name == name) {
            return constant.type_name.clone();
        }

        if let Some(configurable) = self.configurable.as_ref() {
            if let Some(field) = configurable.fields.iter().find(|c| c.name == name) {
                return field.type_name.clone();
            }
        }

        panic!("error: Variable not found in scope: \"{name}\"");
    }

    #[inline(always)]
    fn get_block_type(
        &mut self,
        project: &mut Project,
        scope: Rc<RefCell<TranslationScope>>,
        block: &sway::Block,
    ) -> Result<sway::TypeName, Error> {
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
                None => self.get_expression_type(project, inner_scope.clone(), value)?,
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

        self.get_expression_type(project, inner_scope.clone(), expression)
    }

    #[inline(always)]
    fn get_return_type(
        &mut self,
        project: &mut Project,
        scope: Rc<RefCell<TranslationScope>>,
        value: Option<&sway::Expression>,
    ) -> Result<sway::TypeName, Error> {
        if let Some(value) = value.as_ref() {
            self.get_expression_type(project, scope.clone(), value)
        } else {
            Ok(sway::TypeName::Tuple { type_names: vec![] })
        }
    }

    #[inline(always)]
    fn get_array_type(
        &mut self,
        project: &mut Project,
        scope: Rc<RefCell<TranslationScope>>,
        array: &sway::Array,
    ) -> Result<sway::TypeName, Error> {
        Ok(sway::TypeName::Array {
            type_name: Box::new(if let Some(expression) = array.elements.first() {
                self.get_expression_type(project, scope.clone(), expression)?
            } else {
                sway::TypeName::Tuple { type_names: vec![] }
            }),
            length: array.elements.len(),
        })
    }

    #[inline(always)]
    fn get_array_access_type(
        &mut self,
        project: &mut Project,
        scope: Rc<RefCell<TranslationScope>>,
        array_access: &sway::ArrayAccess,
    ) -> Result<sway::TypeName, Error> {
        let element_type_name =
            self.get_expression_type(project, scope.clone(), &array_access.expression)?;

        let type_name = match &element_type_name {
            sway::TypeName::Identifier {
                name,
                generic_parameters: Some(generic_parameters),
            } if name == "Vec" => &generic_parameters.entries.first().unwrap().type_name,

            sway::TypeName::Array { type_name, .. } => type_name.as_ref(),

            _ => todo!(
                "array access for type {element_type_name}: {}",
                sway::TabbedDisplayer(array_access)
            ),
        };

        Ok(type_name.clone())
    }

    #[inline(always)]
    fn get_member_access_type(
        &mut self,
        project: &mut Project,
        scope: Rc<RefCell<TranslationScope>>,
        member_access: &sway::MemberAccess,
        expression: &sway::Expression,
    ) -> Result<sway::TypeName, Error> {
        if let sway::Expression::PathExpr(path) = &member_access.expression {
            let name = path.to_string();

            if name.starts_with("storage::") {
                let parts = name.split("::").collect::<Vec<_>>();

                assert!(parts.len() == 2);

                let Some(namespace) = self
                    .storage
                    .as_ref()
                    .map(|s| s.namespaces.iter().find(|n| n.name == parts[1]))
                    .flatten()
                else {
                    panic!(
                        "Failed to find storage namespace {}. Available namespaces : {}",
                        parts[1],
                        self.storage
                            .as_ref()
                            .map(|s| s
                                .namespaces
                                .iter()
                                .map(|n| n.name.clone())
                                .collect::<Vec<_>>()
                                .join(", "))
                            .unwrap_or("<none>".into())
                    );
                };

                let Some(storage_field) = namespace
                    .fields
                    .iter()
                    .find(|f| f.name == member_access.member)
                else {
                    panic!(
                        "Failed to find storage variable in scope: `{}`",
                        member_access.member
                    )
                };

                return Ok(sway::TypeName::Identifier {
                    name: "StorageKey".into(),
                    generic_parameters: Some(sway::GenericParameterList {
                        entries: vec![sway::GenericParameter {
                            type_name: storage_field.type_name.clone(),
                            implements: None,
                        }],
                    }),
                });
            }
        }

        let container_type =
            self.get_expression_type(project, scope.clone(), &member_access.expression)?;

        // Check if field is a signed integer
        if let Some(bits) = container_type.int_bits() {
            match member_access.member.as_str() {
                "underlying" => {
                    return Ok(sway::TypeName::Identifier {
                        name: match bits {
                            8 => "u8",
                            16 => "u16",
                            32 => "u32",
                            64 => "u64",
                            128 => "U128",
                            256 => "u256",
                            _ => unimplemented!("I{bits}"),
                        }
                        .into(),
                        generic_parameters: None,
                    });
                }

                _ => {}
            }
        }

        // Check if container is a struct
        if let sway::TypeName::Identifier {
            name,
            generic_parameters: None,
        } = &container_type
        {
            if let Some(struct_definition) = self
                .structs
                .iter()
                .find(|s| s.implementation.as_ref().unwrap().borrow().name == *name)
            {
                if let Some(field) = struct_definition
                    .implementation
                    .as_ref()
                    .unwrap()
                    .borrow()
                    .fields
                    .iter()
                    .find(|f| f.name == member_access.member)
                {
                    return Ok(field.type_name.clone());
                }
            }
        }

        todo!("get type of {container_type} member access expression: {expression:#?}")
    }

    #[inline(always)]
    fn get_tuple_type(
        &mut self,
        project: &mut Project,
        scope: Rc<RefCell<TranslationScope>>,
        tuple: &[sway::Expression],
    ) -> Result<sway::TypeName, Error> {
        if tuple.len() == 1 {
            self.get_expression_type(project, scope.clone(), tuple.first().unwrap())
        } else {
            Ok(sway::TypeName::Tuple {
                type_names: tuple
                    .iter()
                    .map(|x| self.get_expression_type(project, scope.clone(), x))
                    .collect::<Result<Vec<_>, _>>()?,
            })
        }
    }

    #[inline(always)]
    fn get_if_type(
        &mut self,
        project: &mut Project,
        scope: Rc<RefCell<TranslationScope>>,
        if_expr: &sway::If,
    ) -> Result<sway::TypeName, Error> {
        if let Some(expression) = if_expr.then_body.final_expr.as_ref() {
            self.get_expression_type(project, scope.clone(), expression)
        } else {
            Ok(sway::TypeName::Tuple { type_names: vec![] })
        }
    }

    #[inline(always)]
    fn get_match_type(
        &mut self,
        project: &mut Project,
        scope: Rc<RefCell<TranslationScope>>,
        match_expr: &sway::Match,
    ) -> Result<sway::TypeName, Error> {
        if let Some(branch) = match_expr.branches.first() {
            self.get_expression_type(project, scope.clone(), &branch.value)
        } else {
            Ok(sway::TypeName::Tuple { type_names: vec![] })
        }
    }

    #[inline(always)]
    fn get_unary_expression_type(
        &mut self,
        project: &mut Project,
        scope: Rc<RefCell<TranslationScope>>,
        unary_expression: &sway::UnaryExpression,
    ) -> Result<sway::TypeName, Error> {
        self.get_expression_type(project, scope.clone(), &unary_expression.expression)
    }

    #[inline(always)]
    fn get_binary_expression_type(
        &mut self,
        project: &mut Project,
        scope: Rc<RefCell<TranslationScope>>,
        binary_expression: &sway::BinaryExpression,
    ) -> Result<sway::TypeName, Error> {
        match binary_expression.operator.as_str() {
            "==" | "!=" | ">" | "<" | ">=" | "<=" | "&&" | "||" => Ok(sway::TypeName::Identifier {
                name: "bool".into(),
                generic_parameters: None,
            }),

            _ => self.get_expression_type(project, scope.clone(), &binary_expression.lhs),
        }
    }

    #[inline(always)]
    fn get_asm_block_type(&mut self, asm_block: &sway::AsmBlock) -> Result<sway::TypeName, Error> {
        match asm_block.final_expression.as_ref() {
            Some(expression) => match expression.type_name.as_ref() {
                Some(type_name) => Ok(type_name.clone()),
                None => todo!(),
            },
            None => todo!(),
        }
    }

    #[inline(always)]
    fn get_function_call_type(
        &mut self,
        project: &mut Project,
        scope: Rc<RefCell<TranslationScope>>,
        expression: &sway::Expression,
    ) -> Result<sway::TypeName, Error> {
        let (function, function_generic_parameters, parameters) = match expression {
            sway::Expression::FunctionCall(f) => {
                (&f.function, f.generic_parameters.as_ref(), &f.parameters)
            }
            sway::Expression::FunctionCallBlock(f) => {
                (&f.function, f.generic_parameters.as_ref(), &f.parameters)
            }
            _ => unimplemented!(),
        };

        match function {
            sway::Expression::PathExpr(path_expr) => Ok(self
                .get_path_expr_function_call_type(
                    project,
                    scope.clone(),
                    path_expr,
                    function_generic_parameters,
                    parameters.as_slice(),
                )?
                .unwrap()),

            sway::Expression::MemberAccess(member_access) => self
                .get_member_access_function_call_type(
                    project,
                    scope.clone(),
                    member_access,
                    function_generic_parameters,
                    parameters.as_slice(),
                ),

            _ => todo!(
                "get type of function call expression: {} - {expression:#?}",
                sway::TabbedDisplayer(expression)
            ),
        }
    }

    #[inline(always)]
    fn get_path_expr_function_call_type(
        &mut self,
        project: &mut Project,
        scope: Rc<RefCell<TranslationScope>>,
        path_expr: &sway::PathExpr,
        generic_parameters: Option<&sway::GenericParameterList>,
        parameters: &[sway::Expression],
    ) -> Result<Option<sway::TypeName>, Error> {
        //
        // TODO: check generic parameters!
        //

        let name = path_expr.to_string();

        match name.as_str() {
            "todo!" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "todo!".into(),
                    generic_parameters: None,
                }));
            }

            "abi" => {
                assert!(
                    parameters.len() == 2,
                    "Malformed abi cast, expected 2 parameters, found {}",
                    parameters.len()
                );

                let Some(definition_name) = parameters[0].as_identifier() else {
                    panic!(
                        "Malformed abi cast, expected identifier, found {:#?}",
                        parameters[0]
                    );
                };

                return Ok(Some(sway::TypeName::Identifier {
                    name: definition_name.into(),
                    generic_parameters: None,
                }));
            }

            _ => {}
        }

        let parameter_types = parameters
            .iter()
            .map(|p| self.get_expression_type(project, scope.clone(), p))
            .collect::<Result<Vec<_>, _>>()?;

        match name.as_str() {
            "__size_of" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "u64".into(),
                    generic_parameters: None,
                }));
            }

            "Address::from" => {
                assert!(
                    parameters.len() == 1,
                    "Malformed `Address::from` call, expected 1 parameter, found {}",
                    parameters.len()
                );

                return Ok(Some(sway::TypeName::Identifier {
                    name: "Address".into(),
                    generic_parameters: None,
                }));
            }

            "AssetId::default" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "AssetId".into(),
                    generic_parameters: None,
                }));
            }

            "b256::from" | "b256::from_be_bytes" | "b256::from_le_bytes" | "b256::zero" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "b256".into(),
                    generic_parameters: None,
                }));
            }

            "Bytes::new" | "Bytes::from" | "Bytes::with_capacity" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "Bytes".into(),
                    generic_parameters: None,
                }));
            }

            "ContractId::this" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "ContractId".into(),
                    generic_parameters: None,
                }));
            }

            "I8::from" | "I8::from_uint" | "I8::max" | "I8::min" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "I8".into(),
                    generic_parameters: None,
                }));
            }

            "I16::from" | "I16::from_uint" | "I16::max" | "I16::min" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "I16".into(),
                    generic_parameters: None,
                }));
            }

            "I32::from" | "I32::from_uint" | "I32::max" | "I32::min" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "I32".into(),
                    generic_parameters: None,
                }));
            }

            "I64::from" | "I64::from_uint" | "I64::max" | "I64::min" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "I64".into(),
                    generic_parameters: None,
                }));
            }

            "I128::from" | "I128::from_uint" | "I128::max" | "I128::min" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "I128".into(),
                    generic_parameters: None,
                }));
            }

            "I256::from" | "I256::from_uint" | "I256::max" | "I256::min" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "I256".into(),
                    generic_parameters: None,
                }));
            }

            "Identity::Address" | "Identity::ContractId" | "Identity::from" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "Identity".into(),
                    generic_parameters: None,
                }));
            }

            "msg_sender" => {
                return Ok(Some(sway::TypeName::Identifier {
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
                }));
            }

            "raw_slice::from_parts" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "raw_slice".into(),
                    generic_parameters: None,
                }));
            }

            "std::alloc::alloc" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "raw_ptr".into(),
                    generic_parameters: None,
                }));
            }

            "std::block::height" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "u32".into(),
                    generic_parameters: None,
                }));
            }

            "std::block::timestamp" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "u64".into(),
                    generic_parameters: None,
                }));
            }

            "std::context::balance_of" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "u64".into(),
                    generic_parameters: None,
                }));
            }

            "std::context::msg_amount" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "u64".into(),
                    generic_parameters: None,
                }));
            }

            "std::context::this_balance" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "u64".into(),
                    generic_parameters: None,
                }));
            }

            "std::hash::keccak256" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "b256".into(),
                    generic_parameters: None,
                }));
            }

            "std::hash::sha256" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "b256".into(),
                    generic_parameters: None,
                }));
            }

            "std::inputs::input_message_data" => {
                return Ok(Some(sway::TypeName::Identifier {
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
                }));
            }

            "std::registers::balance" => {
                assert!(
                    parameters.is_empty(),
                    "Malformed `std::registers::balance` call, expected 0 parameters, found {}",
                    parameters.len()
                );

                return Ok(Some(sway::TypeName::Identifier {
                    name: "u64".into(),
                    generic_parameters: None,
                }));
            }

            "std::registers::context_gas" => {
                assert!(
                    parameters.is_empty(),
                    "Malformed `std::registers::context_gas` call, expected 0 parameters, found {}",
                    parameters.len()
                );

                return Ok(Some(sway::TypeName::Identifier {
                    name: "u64".into(),
                    generic_parameters: None,
                }));
            }

            "std::registers::error" => {
                assert!(
                    parameters.is_empty(),
                    "Malformed `std::registers::error` call, expected 0 parameters, found {}",
                    parameters.len()
                );

                return Ok(Some(sway::TypeName::Identifier {
                    name: "u64".into(),
                    generic_parameters: None,
                }));
            }

            "std::registers::flags" => {
                assert!(
                    parameters.is_empty(),
                    "Malformed `std::registers::flags` call, expected 0 parameters, found {}",
                    parameters.len()
                );

                return Ok(Some(sway::TypeName::Identifier {
                    name: "u64".into(),
                    generic_parameters: None,
                }));
            }

            "std::registers::frame_ptr" => {
                assert!(
                    parameters.is_empty(),
                    "Malformed `std::registers::frame_ptr` call, expected 0 parameters, found {}",
                    parameters.len()
                );

                return Ok(Some(sway::TypeName::Identifier {
                    name: "raw_ptr".into(),
                    generic_parameters: None,
                }));
            }

            "std::registers::global_gas" => {
                assert!(
                    parameters.is_empty(),
                    "Malformed `std::registers::global_gas` call, expected 0 parameters, found {}",
                    parameters.len()
                );

                return Ok(Some(sway::TypeName::Identifier {
                    name: "u64".into(),
                    generic_parameters: None,
                }));
            }

            "std::registers::heap_ptr" => {
                assert!(
                    parameters.is_empty(),
                    "Malformed `std::registers::heap_ptr` call, expected 0 parameters, found {}",
                    parameters.len()
                );

                return Ok(Some(sway::TypeName::Identifier {
                    name: "raw_ptr".into(),
                    generic_parameters: None,
                }));
            }

            "std::registers::instrs_start" => {
                assert!(
                    parameters.is_empty(),
                    "Malformed `std::registers::instrs_start` call, expected 0 parameters, found {}",
                    parameters.len()
                );

                return Ok(Some(sway::TypeName::Identifier {
                    name: "raw_ptr".into(),
                    generic_parameters: None,
                }));
            }

            "std::registers::overflow" => {
                assert!(
                    parameters.is_empty(),
                    "Malformed `std::registers::overflow` call, expected 0 parameters, found {}",
                    parameters.len()
                );

                return Ok(Some(sway::TypeName::Identifier {
                    name: "u64".into(),
                    generic_parameters: None,
                }));
            }

            "std::registers::program_counter" => {
                assert!(
                    parameters.is_empty(),
                    "Malformed `std::registers::program_counter` call, expected 0 parameters, found {}",
                    parameters.len()
                );

                return Ok(Some(sway::TypeName::Identifier {
                    name: "raw_ptr".into(),
                    generic_parameters: None,
                }));
            }

            "std::registers::return_value" => {
                assert!(
                    parameters.is_empty(),
                    "Malformed `std::registers::return_value` call, expected 0 parameters, found {}",
                    parameters.len()
                );

                return Ok(Some(sway::TypeName::Identifier {
                    name: "u64".into(),
                    generic_parameters: None,
                }));
            }

            "std::registers::return_length" => {
                assert!(
                    parameters.is_empty(),
                    "Malformed `std::registers::return_length` call, expected 0 parameters, found {}",
                    parameters.len()
                );

                return Ok(Some(sway::TypeName::Identifier {
                    name: "u64".into(),
                    generic_parameters: None,
                }));
            }

            "std::registers::stack_ptr" => {
                assert!(
                    parameters.is_empty(),
                    "Malformed `std::registers::stack_ptr` call, expected 0 parameters, found {}",
                    parameters.len()
                );

                return Ok(Some(sway::TypeName::Identifier {
                    name: "raw_ptr".into(),
                    generic_parameters: None,
                }));
            }

            "std::registers::stack_start_ptr" => {
                assert!(
                    parameters.is_empty(),
                    "Malformed `std::registers::stack_start_ptr` call, expected 0 parameters, found {}",
                    parameters.len()
                );

                return Ok(Some(sway::TypeName::Identifier {
                    name: "raw_ptr".into(),
                    generic_parameters: None,
                }));
            }

            "String::from_ascii" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "String".into(),
                    generic_parameters: None,
                }));
            }

            "u8::from" | "u8::max" | "u8::min" | "u8::from_be_bytes" | "u8::from_le_bytes" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "u8".into(),
                    generic_parameters: None,
                }));
            }

            "u8::try_from" => {
                return Ok(Some(sway::TypeName::Identifier {
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
                }));
            }

            "u16::from" | "u16::max" | "u16::min" | "u16::from_be_bytes" | "u16::from_le_bytes" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "u16".into(),
                    generic_parameters: None,
                }));
            }

            "u16::try_from" => {
                return Ok(Some(sway::TypeName::Identifier {
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
                }));
            }

            "u32::from" | "u32::max" | "u32::min" | "u32::from_be_bytes" | "u32::from_le_bytes" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "u32".into(),
                    generic_parameters: None,
                }));
            }

            "u32::try_from" => {
                return Ok(Some(sway::TypeName::Identifier {
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
                }));
            }

            "u64::from" | "u64::max" | "u64::min" | "u64::from_be_bytes" | "u64::from_le_bytes" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "u64".into(),
                    generic_parameters: None,
                }));
            }

            "u64::try_from" => {
                return Ok(Some(sway::TypeName::Identifier {
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
                }));
            }

            "u256::from"
            | "u256::max"
            | "u256::min"
            | "u256::from_be_bytes"
            | "u256::from_le_bytes" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "u256".into(),
                    generic_parameters: None,
                }));
            }

            "u256::try_from" => {
                return Ok(Some(sway::TypeName::Identifier {
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
                }));
            }

            "U128::from" | "U128::max" | "U128::min" | "U128::zero" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "U128".into(),
                    generic_parameters: None,
                }));
            }

            "U128::try_from" => {
                return Ok(Some(sway::TypeName::Identifier {
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
                }));
            }

            "U256::from" | "U256::max" | "U256::min" => {
                return Ok(Some(sway::TypeName::Identifier {
                    name: "U256".into(),
                    generic_parameters: None,
                }));
            }

            "U256::try_from" => {
                return Ok(Some(sway::TypeName::Identifier {
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
                }));
            }

            "Vec::with_capacity" => {
                return Ok(Some(sway::TypeName::Identifier {
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
                }));
            }

            _ => {}
        }

        fn check_function(
            module: &mut TranslatedModule,
            scope: Rc<RefCell<TranslationScope>>,
            name: &str,
            generic_parameters: Option<&sway::GenericParameterList>,
            parameters: &[sway::Expression],
            parameter_types: &[sway::TypeName],
        ) -> Result<Option<sway::TypeName>, Error> {
            // Attempt to find a function in scope
            if let Some(function) = module.functions.iter().find(|f| {
                let sway::TypeName::Function {
                    new_name: fn_name,
                    parameters: fn_parameters,
                    ..
                } = &f.signature
                else {
                    unreachable!()
                };

                // Ensure the function's new name matches the function call we're translating
                if *fn_name != name {
                    return false;
                }

                // Ensure the supplied function call args match the function's parameters
                if parameters.len() != fn_parameters.entries.len() {
                    return false;
                }

                for (i, value_type_name) in parameter_types.iter().enumerate() {
                    let Some(parameter_type_name) = fn_parameters.entries[i].type_name.as_ref()
                    else {
                        continue;
                    };

                    // HACK: allow numeric literals for any uint types
                    if value_type_name.is_uint() && parameter_type_name.is_uint() {
                        match &parameters[i] {
                            sway::Expression::Literal(
                                sway::Literal::DecInt(_, None) | sway::Literal::HexInt(_, None),
                            ) => continue,

                            sway::Expression::Commented(_, expression) => match expression.as_ref()
                            {
                                sway::Expression::Literal(
                                    sway::Literal::DecInt(_, None) | sway::Literal::HexInt(_, None),
                                ) => continue,

                                _ => {}
                            },

                            _ => {}
                        }
                    }

                    // HACK: allow array literals of uint types containing only literals if the lengths match
                    if let (
                        sway::TypeName::Array {
                            type_name: value_type_name,
                            length: value_length,
                        },
                        sway::TypeName::Array {
                            type_name: parameter_type_name,
                            length: parameter_length,
                        },
                    ) = (value_type_name, parameter_type_name)
                    {
                        if value_length != parameter_length {
                            return false;
                        }

                        if value_type_name.is_uint() && parameter_type_name.is_uint() {
                            match &parameters[i] {
                                sway::Expression::Array(array) => {
                                    if array.elements.iter().all(|e| {
                                        matches!(
                                            e,
                                            sway::Expression::Literal(
                                                sway::Literal::DecInt(_, None)
                                                    | sway::Literal::HexInt(_, None)
                                            )
                                        )
                                    }) {
                                        continue;
                                    }
                                }

                                sway::Expression::Commented(_, expression) => {
                                    match expression.as_ref() {
                                        sway::Expression::Array(array) => {
                                            if array.elements.iter().all(|e| {
                                                matches!(
                                                    e,
                                                    sway::Expression::Literal(
                                                        sway::Literal::DecInt(_, None)
                                                            | sway::Literal::HexInt(_, None)
                                                    )
                                                )
                                            }) {
                                                continue;
                                            }
                                        }

                                        _ => {}
                                    }
                                }

                                _ => {}
                            }
                        }
                    }

                    if !value_type_name.is_compatible_with(parameter_type_name) {
                        return false;
                    }
                }

                true
            }) {
                let sway::TypeName::Function { return_type, .. } = &function.signature else {
                    unreachable!()
                };

                if let Some(return_type) = return_type.as_ref() {
                    return Ok(Some(return_type.as_ref().clone()));
                }

                return Ok(Some(sway::TypeName::Tuple { type_names: vec![] }));
            }

            // Attempt to find a function pointer variable in scope
            if let Some(variable) = scope.borrow().find_variable(|v| {
                let v = v.borrow();

                let sway::TypeName::Function {
                    parameters: fn_parameters,
                    ..
                } = &v.type_name
                else {
                    return false;
                };

                // Ensure the function's new name matches the function call we're translating
                if v.new_name != *name {
                    return false;
                }

                // Ensure the supplied function call args match the function's parameters
                if parameters.len() != fn_parameters.entries.len() {
                    return false;
                }

                for (i, value_type_name) in parameter_types.iter().enumerate() {
                    let Some(parameter_type_name) = fn_parameters.entries[i].type_name.as_ref()
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
                let sway::TypeName::Function { return_type, .. } = &variable.type_name else {
                    unreachable!()
                };

                if let Some(return_type) = return_type.as_ref() {
                    return Ok(Some(return_type.as_ref().clone()));
                } else {
                    return Ok(Some(sway::TypeName::Tuple { type_names: vec![] }));
                }
            }
            Ok(None)
        }

        if let Some(type_name) = check_function(
            self,
            scope.clone(),
            &name,
            generic_parameters,
            parameters,
            &parameter_types,
        )? {
            return Ok(Some(type_name));
        }

        for use_item in self.uses.iter() {
            if let Some(found_module) = project.resolve_use(use_item) {
                if let Some(type_name) = check_function(
                    &mut found_module.borrow_mut(),
                    scope.clone(),
                    &name,
                    generic_parameters,
                    parameters,
                    &parameter_types,
                )? {
                    return Ok(Some(type_name));
                }
            }
        }

        panic!(
            "Failed to find function or variable `{}({})` in scope",
            path_expr.to_string(),
            parameter_types
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(", "),
        )
    }

    #[inline(always)]
    fn get_member_access_function_call_type(
        &mut self,
        project: &mut Project,
        scope: Rc<RefCell<TranslationScope>>,
        member_access: &sway::MemberAccess,
        function_generic_parameters: Option<&sway::GenericParameterList>,
        parameters: &[sway::Expression],
    ) -> Result<sway::TypeName, Error> {
        //
        // TODO: check generic parameters!
        //

        let mut container_type =
            self.get_expression_type(project, scope.clone(), &member_access.expression)?;

        // Check to see if the container's type is a translated enum and switch to its underlying type
        for enum_definition in self.enums.iter() {
            if enum_definition
                .implementation
                .as_ref()
                .unwrap()
                .type_definition
                .name
                == container_type
            {
                if let Some(underlying_type) = enum_definition
                    .implementation
                    .as_ref()
                    .unwrap()
                    .type_definition
                    .underlying_type
                    .as_ref()
                {
                    container_type = underlying_type.clone();
                    break;
                }
            }
        }

        // Check to see if the container's type is a UDT and switch to its underlying type
        for type_definition in self.type_definitions.iter() {
            if type_definition.implementation.as_ref().unwrap().name == container_type {
                if let Some(underlying_type) = type_definition
                    .implementation
                    .as_ref()
                    .unwrap()
                    .underlying_type
                    .as_ref()
                {
                    container_type = underlying_type.clone();
                    break;
                }
            }
        }

        match &container_type {
            sway::TypeName::Undefined => panic!("Undefined type name"),

            sway::TypeName::Identifier {
                name,
                generic_parameters,
            } => match (name.as_str(), generic_parameters.as_ref()) {
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

                    _ => todo!(
                        "get type of function call expression: {} - {member_access:#?}",
                        sway::TabbedDisplayer(member_access)
                    ),
                },

                ("Bytes", None) => match member_access.member.as_str() {
                    "as_raw_slice" => Ok(sway::TypeName::Identifier {
                        name: "raw_slice".into(),
                        generic_parameters: None,
                    }),

                    "get" => Ok(sway::TypeName::Identifier {
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

                    _ => todo!(
                        "get type of function call expression: {} - {member_access:#?}",
                        sway::TabbedDisplayer(member_access)
                    ),
                },

                ("I8", None) => match member_access.member.as_str() {
                    "wrapping_neg" => Ok(sway::TypeName::Identifier {
                        name: "I8".into(),
                        generic_parameters: None,
                    }),

                    "underlying" => Ok(sway::TypeName::Identifier {
                        name: "u8".into(),
                        generic_parameters: None,
                    }),

                    _ => todo!(
                        "get type of function call expression: {} - {member_access:#?}",
                        sway::TabbedDisplayer(member_access)
                    ),
                },

                ("I16", None) => match member_access.member.as_str() {
                    "wrapping_neg" => Ok(sway::TypeName::Identifier {
                        name: "I16".into(),
                        generic_parameters: None,
                    }),

                    "underlying" => Ok(sway::TypeName::Identifier {
                        name: "u16".into(),
                        generic_parameters: None,
                    }),

                    _ => todo!(
                        "get type of function call expression: {} - {member_access:#?}",
                        sway::TabbedDisplayer(member_access)
                    ),
                },

                ("I32", None) => match member_access.member.as_str() {
                    "wrapping_neg" => Ok(sway::TypeName::Identifier {
                        name: "I32".into(),
                        generic_parameters: None,
                    }),

                    "underlying" => Ok(sway::TypeName::Identifier {
                        name: "u32".into(),
                        generic_parameters: None,
                    }),

                    _ => todo!(
                        "get type of function call expression: {} - {member_access:#?}",
                        sway::TabbedDisplayer(member_access)
                    ),
                },

                ("I64", None) => match member_access.member.as_str() {
                    "wrapping_neg" => Ok(sway::TypeName::Identifier {
                        name: "I64".into(),
                        generic_parameters: None,
                    }),

                    "underlying" => Ok(sway::TypeName::Identifier {
                        name: "u64".into(),
                        generic_parameters: None,
                    }),

                    _ => todo!(
                        "get type of function call expression: {} - {member_access:#?}",
                        sway::TabbedDisplayer(member_access)
                    ),
                },

                ("I128", None) => match member_access.member.as_str() {
                    "wrapping_neg" => Ok(sway::TypeName::Identifier {
                        name: "I128".into(),
                        generic_parameters: None,
                    }),

                    "underlying" => Ok(sway::TypeName::Identifier {
                        name: "U128".into(),
                        generic_parameters: None,
                    }),

                    _ => todo!(
                        "get type of function call expression: {} - {member_access:#?}",
                        sway::TabbedDisplayer(member_access)
                    ),
                },

                ("I256", None) => match member_access.member.as_str() {
                    "wrapping_neg" => Ok(sway::TypeName::Identifier {
                        name: "I256".into(),
                        generic_parameters: None,
                    }),

                    "underlying" => Ok(sway::TypeName::Identifier {
                        name: "u256".into(),
                        generic_parameters: None,
                    }),

                    _ => todo!(
                        "get type of function call expression: {} - {member_access:#?}",
                        sway::TabbedDisplayer(member_access)
                    ),
                },

                ("Identity", None) => match member_access.member.as_str() {
                    "as_address" => Ok(sway::TypeName::Identifier {
                        name: "Option".into(),
                        generic_parameters: Some(sway::GenericParameterList {
                            entries: vec![sway::GenericParameter {
                                type_name: sway::TypeName::Identifier {
                                    name: "Address".into(),
                                    generic_parameters: None,
                                },
                                implements: None,
                            }],
                        }),
                    }),

                    "as_contract_id" => Ok(sway::TypeName::Identifier {
                        name: "Option".into(),
                        generic_parameters: Some(sway::GenericParameterList {
                            entries: vec![sway::GenericParameter {
                                type_name: sway::TypeName::Identifier {
                                    name: "ContractId".into(),
                                    generic_parameters: None,
                                },
                                implements: None,
                            }],
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

                    _ => Err(Error::Wrapped(Box::new(std::io::Error::new(
                        std::io::ErrorKind::NotFound,
                        format!(
                            "get type of function call member_access: {} - {member_access:#?}",
                            sway::TabbedDisplayer(member_access)
                        ),
                    )))),
                },

                ("Option", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => {
                    match member_access.member.as_str() {
                        "is_none" | "is_some" if parameters.is_empty() => {
                            Ok(sway::TypeName::Identifier {
                                name: "bool".into(),
                                generic_parameters: None,
                            })
                        }

                        "unwrap" => Ok(generic_parameters.entries[0].type_name.clone()),
                        "unwrap_or" => Ok(generic_parameters.entries[0].type_name.clone()),

                        _ => todo!(
                            "get type of function call expression: {} - {member_access:#?}",
                            sway::TabbedDisplayer(member_access)
                        ),
                    }
                }

                ("Result", Some(generic_parameters)) if generic_parameters.entries.len() == 2 => {
                    match member_access.member.as_str() {
                        "unwrap" => Ok(generic_parameters.entries[0].type_name.clone()),

                        _ => todo!(
                            "get type of function call expression: {} - {member_access:#?}",
                            sway::TabbedDisplayer(member_access)
                        ),
                    }
                }

                ("raw_ptr", None) => match member_access.member.as_str() {
                    "add" => Ok(sway::TypeName::Identifier {
                        name: "raw_ptr".into(),
                        generic_parameters: None,
                    }),

                    "read" => Ok(function_generic_parameters.unwrap().entries[0]
                        .type_name
                        .clone()),

                    _ => todo!(
                        "get type of function call expression: {} - {member_access:#?}",
                        sway::TabbedDisplayer(member_access)
                    ),
                },

                ("raw_slice", None) => match member_access.member.as_str() {
                    "ptr" => Ok(sway::TypeName::Identifier {
                        name: "raw_ptr".into(),
                        generic_parameters: None,
                    }),

                    _ => todo!(
                        "get type of function call expression: {} - {member_access:#?}",
                        sway::TabbedDisplayer(member_access)
                    ),
                },

                ("StorageKey", Some(generic_parameters))
                    if generic_parameters.entries.len() == 1 =>
                {
                    match member_access.member.as_str() {
                        "clear" => Ok(sway::TypeName::Identifier {
                            name: "bool".into(),
                            generic_parameters: None,
                        }),

                        "read" => Ok(generic_parameters.entries[0].type_name.clone()),

                        "try_read" => Ok(sway::TypeName::Identifier {
                            name: "Option".into(),
                            generic_parameters: Some(sway::GenericParameterList {
                                entries: vec![sway::GenericParameter {
                                    type_name: generic_parameters.entries[0].type_name.clone(),
                                    implements: None,
                                }],
                            }),
                        }),

                        "write" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                        _ => match &generic_parameters.entries[0].type_name {
                            sway::TypeName::Identifier {
                                name,
                                generic_parameters,
                            } => match (name.as_str(), generic_parameters.as_ref()) {
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
                                            entries: vec![sway::GenericParameter {
                                                type_name: sway::TypeName::Identifier {
                                                    name: "Bytes".into(),
                                                    generic_parameters: None,
                                                },
                                                implements: None,
                                            }],
                                        }),
                                    }),

                                    "write_slice" => {
                                        Ok(sway::TypeName::Tuple { type_names: vec![] })
                                    }

                                    _ => todo!(
                                        "get type of function call expression: {} - {member_access:#?}",
                                        sway::TabbedDisplayer(member_access)
                                    ),
                                },

                                ("StorageMap", Some(generic_parameters))
                                    if generic_parameters.entries.len() == 2 =>
                                {
                                    match member_access.member.as_str() {
                                        "get" => Ok(sway::TypeName::Identifier {
                                            name: "StorageKey".into(),
                                            generic_parameters: Some(sway::GenericParameterList {
                                                entries: vec![sway::GenericParameter {
                                                    type_name: generic_parameters.entries[1]
                                                        .type_name
                                                        .clone(),
                                                    implements: None,
                                                }],
                                            }),
                                        }),

                                        "insert" => {
                                            Ok(sway::TypeName::Tuple { type_names: vec![] })
                                        }

                                        "remove" => Ok(sway::TypeName::Identifier {
                                            name: "bool".into(),
                                            generic_parameters: None,
                                        }),

                                        "try_insert" => Ok(sway::TypeName::Identifier {
                                            name: "Result".into(),
                                            generic_parameters: Some(sway::GenericParameterList {
                                                entries: vec![
                                                    sway::GenericParameter {
                                                        type_name: generic_parameters.entries[0]
                                                            .type_name
                                                            .clone(),
                                                        implements: None,
                                                    },
                                                    sway::GenericParameter {
                                                        type_name: sway::TypeName::Identifier {
                                                            name: "StorageMapError".into(),
                                                            generic_parameters: Some(
                                                                sway::GenericParameterList {
                                                                    entries: vec![
                                                                        sway::GenericParameter {
                                                                            type_name:
                                                                                generic_parameters
                                                                                    .entries[0]
                                                                                    .type_name
                                                                                    .clone(),
                                                                            implements: None,
                                                                        },
                                                                    ],
                                                                },
                                                            ),
                                                        },
                                                        implements: None,
                                                    },
                                                ],
                                            }),
                                        }),

                                        _ => todo!(
                                            "get type of function call expression: {} - {member_access:#?}",
                                            sway::TabbedDisplayer(member_access)
                                        ),
                                    }
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
                                            entries: vec![sway::GenericParameter {
                                                type_name: sway::TypeName::Identifier {
                                                    name: "String".into(),
                                                    generic_parameters: None,
                                                },
                                                implements: None,
                                            }],
                                        }),
                                    }),

                                    "write_slice" => {
                                        Ok(sway::TypeName::Tuple { type_names: vec![] })
                                    }

                                    _ => todo!(
                                        "get type of function call expression: {} - {member_access:#?}",
                                        sway::TabbedDisplayer(member_access)
                                    ),
                                },

                                ("StorageVec", Some(generic_parameters))
                                    if generic_parameters.entries.len() == 1 =>
                                {
                                    match member_access.member.as_str() {
                                        "fill" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                        "first" => Ok(sway::TypeName::Identifier {
                                            name: "Option".into(),
                                            generic_parameters: Some(sway::GenericParameterList {
                                                entries: vec![sway::GenericParameter {
                                                    type_name: sway::TypeName::Identifier {
                                                        name: "StorageKey".into(),
                                                        generic_parameters: Some(
                                                            sway::GenericParameterList {
                                                                entries: vec![
                                                                    sway::GenericParameter {
                                                                        type_name:
                                                                            generic_parameters
                                                                                .entries[0]
                                                                                .type_name
                                                                                .clone(),
                                                                        implements: None,
                                                                    },
                                                                ],
                                                            },
                                                        ),
                                                    },
                                                    implements: None,
                                                }],
                                            }),
                                        }),

                                        "get" => Ok(sway::TypeName::Identifier {
                                            name: "Option".into(),
                                            generic_parameters: Some(sway::GenericParameterList {
                                                entries: vec![sway::GenericParameter {
                                                    type_name: sway::TypeName::Identifier {
                                                        name: "StorageKey".into(),
                                                        generic_parameters: Some(
                                                            sway::GenericParameterList {
                                                                entries: vec![
                                                                    sway::GenericParameter {
                                                                        type_name:
                                                                            generic_parameters
                                                                                .entries[0]
                                                                                .type_name
                                                                                .clone(),
                                                                        implements: None,
                                                                    },
                                                                ],
                                                            },
                                                        ),
                                                    },
                                                    implements: None,
                                                }],
                                            }),
                                        }),

                                        "insert" => {
                                            Ok(sway::TypeName::Tuple { type_names: vec![] })
                                        }

                                        "is_empty" => Ok(sway::TypeName::Identifier {
                                            name: "bool".into(),
                                            generic_parameters: None,
                                        }),

                                        "last" => Ok(sway::TypeName::Identifier {
                                            name: "Option".into(),
                                            generic_parameters: Some(sway::GenericParameterList {
                                                entries: vec![sway::GenericParameter {
                                                    type_name: sway::TypeName::Identifier {
                                                        name: "StorageKey".into(),
                                                        generic_parameters: Some(
                                                            sway::GenericParameterList {
                                                                entries: vec![
                                                                    sway::GenericParameter {
                                                                        type_name:
                                                                            generic_parameters
                                                                                .entries[0]
                                                                                .type_name
                                                                                .clone(),
                                                                        implements: None,
                                                                    },
                                                                ],
                                                            },
                                                        ),
                                                    },
                                                    implements: None,
                                                }],
                                            }),
                                        }),

                                        "len" => Ok(sway::TypeName::Identifier {
                                            name: "u64".into(),
                                            generic_parameters: None,
                                        }),

                                        "load_vec" => Ok(sway::TypeName::Identifier {
                                            name: "Vec".into(),
                                            generic_parameters: Some(sway::GenericParameterList {
                                                entries: vec![sway::GenericParameter {
                                                    type_name: generic_parameters.entries[0]
                                                        .type_name
                                                        .clone(),
                                                    implements: None,
                                                }],
                                            }),
                                        }),

                                        "pop" => Ok(sway::TypeName::Identifier {
                                            name: "Option".into(),
                                            generic_parameters: Some(sway::GenericParameterList {
                                                entries: vec![sway::GenericParameter {
                                                    type_name: generic_parameters.entries[0]
                                                        .type_name
                                                        .clone(),
                                                    implements: None,
                                                }],
                                            }),
                                        }),

                                        "push" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                        "remove" => {
                                            Ok(generic_parameters.entries[0].type_name.clone())
                                        }

                                        "resize" => {
                                            Ok(sway::TypeName::Tuple { type_names: vec![] })
                                        }

                                        "reverse" => {
                                            Ok(sway::TypeName::Tuple { type_names: vec![] })
                                        }

                                        "set" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                        "store_vec" => {
                                            Ok(sway::TypeName::Tuple { type_names: vec![] })
                                        }

                                        "swap_remove" => {
                                            Ok(generic_parameters.entries[0].type_name.clone())
                                        }

                                        "swap" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                        _ => todo!(
                                            "get type of function call expression: {} - {member_access:#?}",
                                            sway::TabbedDisplayer(member_access)
                                        ),
                                    }
                                }

                                (name, _) => todo!(
                                    "get type of {name}::{} function call member_access: {} - {member_access:#?}",
                                    member_access.member,
                                    sway::TabbedDisplayer(member_access)
                                ),
                            },

                            _ => todo!(
                                "get type of function call expression: {} - {member_access:#?}",
                                sway::TabbedDisplayer(member_access)
                            ),
                        },
                    }
                }

                ("StorageMap", Some(generic_parameters))
                    if generic_parameters.entries.len() == 2 =>
                {
                    match member_access.member.as_str() {
                        "get" if parameters.len() == 1 => Ok(sway::TypeName::Identifier {
                            name: "StorageKey".to_string(),
                            generic_parameters: Some(sway::GenericParameterList {
                                entries: vec![generic_parameters.entries[1].clone()],
                            }),
                        }),
                        _ => todo!(
                            "get type of function call expression: {} - {member_access:#?}",
                            sway::TabbedDisplayer(member_access)
                        ),
                    }
                }

                ("String", None) => match member_access.member.as_str() {
                    "len" => Ok(sway::TypeName::Identifier {
                        name: "u64".into(),
                        generic_parameters: None,
                    }),

                    _ => todo!(
                        "get type of function call expression: {} - {member_access:#?}",
                        sway::TabbedDisplayer(member_access)
                    ),
                },

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

                    _ => todo!(
                        "get type of function call expression: {} - {member_access:#?}",
                        sway::TabbedDisplayer(member_access)
                    ),
                },

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

                    _ => todo!(
                        "get type of function call expression: {} - {member_access:#?}",
                        sway::TabbedDisplayer(member_access)
                    ),
                },

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

                    _ => todo!(
                        "get type of function call expression: {} - {member_access:#?}",
                        sway::TabbedDisplayer(member_access)
                    ),
                },

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
                            "sway_libs = { git = \"https://github.com/FuelLabs/sway-libs\", tag = \"v0.25.2\" }"
                        );

                        Ok(sway::TypeName::Identifier {
                            name: {
                                self.ensure_use_declared("sway_libs::signed_integers::i64::*");
                                "I64".into()
                            },
                            generic_parameters: None,
                        })
                    }

                    _ => todo!(
                        "get type of function call expression: {} - {member_access:#?}",
                        sway::TabbedDisplayer(member_access)
                    ),
                },

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

                    name => {
                        // Check to see if we are using a function from the using library
                        if self
                            .using_directives
                            .iter()
                            .any(|using| using.functions.iter().any(|fnc| fnc == name))
                        {
                            return Ok(sway::TypeName::Identifier {
                                name: "u256".into(),
                                generic_parameters: None,
                            });
                        }

                        todo!(
                            "get type of function call expression: {} - {member_access:#?}",
                            sway::TabbedDisplayer(member_access)
                        )
                    }
                },

                ("Vec", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => {
                    match member_access.member.as_str() {
                        "get" => Ok(sway::TypeName::Identifier {
                            name: "Option".into(),
                            generic_parameters: Some(sway::GenericParameterList {
                                entries: vec![generic_parameters.entries.first().unwrap().clone()],
                            }),
                        }),

                        "len" => Ok(sway::TypeName::Identifier {
                            name: "u64".into(),
                            generic_parameters: None,
                        }),

                        _ => todo!(
                            "get type of function call expression: {} - {member_access:#?}",
                            sway::TabbedDisplayer(member_access)
                        ),
                    }
                }

                (name, None) => {
                    for contract in self.contracts.iter() {
                        let contract = contract.implementation.as_ref().unwrap();

                        if contract.abi.name == name {
                            if let Some(function_definition) = contract
                                .abi
                                .functions
                                .iter()
                                .find(|f| f.name == member_access.member)
                            {
                                return Ok(function_definition.return_type.clone().unwrap_or_else(
                                    || sway::TypeName::Tuple { type_names: vec![] },
                                ));
                            }
                        }
                    }

                    todo!(
                        "get type of {name} function call member_access: {} - {member_access:#?}",
                        sway::TabbedDisplayer(member_access)
                    )
                }

                _ => todo!(
                    "get type of {name} function call member_access: {} - {member_access:#?}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            sway::TypeName::StringSlice => match member_access.member.as_str() {
                "len" => Ok(sway::TypeName::Identifier {
                    name: "u64".into(),
                    generic_parameters: None,
                }),

                _ => todo!(
                    "get type of function call expression: {} - {member_access:#?}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            _ => todo!(
                "get type of {container_type} function call member_access: {} - {member_access:#?}",
                sway::TabbedDisplayer(member_access)
            ),
        }
    }
}

impl From<TranslatedModule> for sway::Module {
    fn from(module: TranslatedModule) -> Self {
        let kind = if module.contracts.is_empty() {
            sway::ModuleKind::Library
        } else {
            sway::ModuleKind::Contract
        };

        let mut items = vec![];

        for x in module.submodules {
            items.push(sway::ModuleItem::Submodule(sway::Submodule {
                is_public: true,
                name: x.borrow().name.clone(),
            }));
        }

        for x in module.uses {
            items.push(sway::ModuleItem::Use(x));
        }

        for x in module.type_definitions {
            items.push(sway::ModuleItem::TypeDefinition(x.implementation.unwrap()));
        }

        for x in module.structs {
            items.push(sway::ModuleItem::Struct(
                x.implementation.unwrap().borrow().clone(),
            ));
        }

        for x in module.enums {
            items.push(sway::ModuleItem::TypeDefinition(
                x.implementation.as_ref().unwrap().type_definition.clone(),
            ));

            items.push(sway::ModuleItem::Impl(
                x.implementation.unwrap().variants_impl,
            ))
        }

        for (events_enum, abi_encode_impl) in module.events_enums {
            items.push(sway::ModuleItem::Enum(events_enum.borrow().clone()));
            items.push(sway::ModuleItem::Impl(abi_encode_impl.borrow().clone()));
        }

        for (errors_enum, abi_encode_impl) in module.errors_enums.iter() {
            items.push(sway::ModuleItem::Enum(errors_enum.borrow().clone()));
            items.push(sway::ModuleItem::Impl(abi_encode_impl.borrow().clone()));
        }

        for x in module.constants {
            items.push(sway::ModuleItem::Constant(x));
        }

        if let Some(x) = module.configurable {
            items.push(sway::ModuleItem::Configurable(x));
        }

        if let Some(x) = module.storage {
            items.push(sway::ModuleItem::Storage(x));
        }

        for x in module.functions {
            items.push(sway::ModuleItem::Function(x.implementation.unwrap()));
        }

        for x in module.contracts {
            items.push(sway::ModuleItem::Abi(
                x.implementation.as_ref().unwrap().abi.clone(),
            ));

            items.push(sway::ModuleItem::Impl(x.implementation.unwrap().abi_impl));
        }

        for x in module.impls {
            items.push(sway::ModuleItem::Impl(x));
        }

        sway::Module { kind, items }
    }
}

#[inline]
pub fn translate_naming_convention(name: &str, case: Case) -> String {
    // HACK: do not allow dollar signs
    let mut name = name.replace('$', "dollar_sign").to_string();

    // HACK: do not allow name to start with double underscore
    while name.starts_with("__") {
        name = name[2..].to_string();
    }

    let name = if name.chars().all(|c| c == '_') {
        name.to_string()
    } else {
        let prefix = name.chars().take_while(|c| *c == '_').collect::<String>();
        let postfix = name
            .chars()
            .rev()
            .take_while(|c| *c == '_')
            .collect::<String>();
        format!("{prefix}{}{postfix}", name.to_case(case))
    };

    match name.as_str() {
        "self" => "this".into(),
        _ => name,
    }
}

/// Coerces a argument type to a parameter type
pub fn coerce_expression(
    expression: &sway::Expression,
    from_type_name: &sway::TypeName,
    to_type_name: &sway::TypeName,
) -> Option<sway::Expression> {
    // println!(
    //     "Coercing from `{from_type_name}` to `{to_type_name}`: {}",
    //     sway::TabbedDisplayer(expression)
    // );

    if from_type_name.is_compatible_with(to_type_name) {
        return Some(expression.clone());
    }

    let is_uint = from_type_name.is_uint();
    let is_int = from_type_name.is_int();

    let mut expression = expression.clone();

    //
    // If `to_type_name` is `Identity`, but `expression` is an abi cast expression,
    // then we need to de-cast it, so `expression` turns into the 2nd parameter of the abi cast,
    // and `from_type_name` turns into `Identity`.
    //
    if let (
        _,
        sway::TypeName::Identifier {
            name,
            generic_parameters,
        },
    ) = (from_type_name, to_type_name)
    {
        match (name.as_str(), generic_parameters.as_ref()) {
            ("Identity", None) => match &expression {
                sway::Expression::FunctionCall(f) => {
                    if let Some(ident) = f.function.as_identifier() {
                        if ident == "abi" && f.parameters.len() == 2 {
                            let rhs = f.parameters[1].clone();
                            if let sway::Expression::FunctionCall(f) = &rhs {
                                if let sway::Expression::MemberAccess(e) = &f.function {
                                    if e.member == "into" {
                                        if let sway::Expression::FunctionCall(f) = &e.expression {
                                            if let sway::Expression::MemberAccess(e) = &f.function {
                                                if e.member == "unwrap" {
                                                    if let sway::Expression::FunctionCall(f) =
                                                        &e.expression
                                                    {
                                                        if let sway::Expression::MemberAccess(e) =
                                                            &f.function
                                                        {
                                                            if e.member == "as_contract_id" {
                                                                return Some(e.expression.clone());
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                _ => {}
            },

            _ => {}
        }
    }

    match (from_type_name, to_type_name) {
        (sway::TypeName::Undefined, sway::TypeName::Undefined) => {}

        (
            sway::TypeName::Identifier {
                name: lhs_name,
                generic_parameters: lhs_generic_parameters,
            },
            sway::TypeName::Identifier {
                name: rhs_name,
                generic_parameters: rhs_generic_parameters,
            },
        ) => {
            if lhs_generic_parameters.is_some() != rhs_generic_parameters.is_some() {
                // From `StorageKey<T>` to `T`
                if let Some(storage_key_type) = from_type_name.storage_key_type() {
                    if !storage_key_type.is_compatible_with(to_type_name) {
                        return None;
                    }

                    return Some(sway::Expression::create_function_calls(
                        Some(expression.clone()),
                        &[("read", Some((None, vec![])))],
                    ));
                }

                return None;
            }

            if let (Some(lhs_generic_parameters), Some(rhs_generic_parameters)) = (
                lhs_generic_parameters.as_ref(),
                rhs_generic_parameters.as_ref(),
            ) {
                if lhs_generic_parameters.entries.len() != rhs_generic_parameters.entries.len() {
                    return None;
                }
            }

            if lhs_name == rhs_name {
                return Some(expression.clone());
            }

            // {
            //     let mut v = Vec::new();
            //     let len = storage.storageVec.len();
            //     let mut i = 0;
            //     while i < len {
            //         v.push(storage.storageVec.get(i).unwrap().read())
            //         i += 1;
            //     }
            //     v
            // }
            if let Some(storage_key_type) = from_type_name.storage_key_type() {
                if let Some(storage_vec_type) = storage_key_type.storage_vec_type() {
                    if let Some(vec_type) = to_type_name.vec_type() {
                        // let unique_variable_name =
                        let get_expression = sway::Expression::create_function_calls(
                            Some(expression.clone()),
                            &[
                                (
                                    "get",
                                    Some((
                                        None,
                                        vec![sway::Expression::create_identifier("i".to_string())],
                                    )),
                                ),
                                ("unwrap", Some((None, vec![]))),
                                ("read", Some((None, vec![]))),
                            ],
                        );
                        let element_expression =
                            coerce_expression(&get_expression, &storage_vec_type, &vec_type)
                                .unwrap();

                        return Some(sway::Expression::from(sway::Block {
                            statements: vec![
                                sway::Statement::from(sway::Let {
                                    pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                                        is_mutable: false,
                                        name: "len".to_string(),
                                    }),
                                    type_name: None,
                                    value: sway::Expression::create_function_calls(
                                        Some(expression.clone()),
                                        &[("len", Some((None, vec![])))],
                                    ),
                                }),
                                sway::Statement::from(sway::Let {
                                    pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                                        is_mutable: true,
                                        name: "v".to_string(),
                                    }),
                                    type_name: None,
                                    value: sway::Expression::create_function_calls(
                                        None,
                                        &[(
                                            "Vec::with_capacity",
                                            Some((
                                                None,
                                                vec![sway::Expression::create_identifier(
                                                    "len".to_string(),
                                                )],
                                            )),
                                        )],
                                    ),
                                }),
                                sway::Statement::from(sway::Let {
                                    pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                                        is_mutable: true,
                                        name: "i".to_string(),
                                    }),
                                    type_name: None,
                                    value: sway::Expression::from(sway::Literal::DecInt(
                                        BigUint::zero(),
                                        None,
                                    )),
                                }),
                                sway::Statement::from(sway::Expression::from(sway::While {
                                    condition: sway::Expression::from(sway::BinaryExpression {
                                        operator: "<".to_string(),
                                        lhs: sway::Expression::create_identifier("i".to_string()),
                                        rhs: sway::Expression::create_identifier("len".to_string()),
                                    }),
                                    body: sway::Block {
                                        statements: vec![
                                            sway::Statement::from(
                                                sway::Expression::create_function_calls(
                                                    None,
                                                    &[
                                                        ("v", None),
                                                        (
                                                            "push",
                                                            Some((
                                                                None,
                                                                vec![element_expression.clone()],
                                                            )),
                                                        ),
                                                    ],
                                                ),
                                            ),
                                            sway::Statement::from(sway::Expression::from(
                                                sway::BinaryExpression {
                                                    operator: "+=".to_string(),
                                                    lhs: sway::Expression::create_identifier(
                                                        "i".to_string(),
                                                    ),
                                                    rhs: sway::Expression::from(
                                                        sway::Literal::DecInt(BigUint::one(), None),
                                                    ),
                                                },
                                            )),
                                        ],
                                        final_expr: None,
                                    },
                                })),
                            ],
                            final_expr: Some(sway::Expression::create_identifier("v".to_string())),
                        }));
                    }
                }
            }

            // From uint to int
            if from_type_name.is_uint() && !to_type_name.is_uint() {
                if to_type_name.is_int() {
                    let lhs_bits: usize = lhs_name
                        .trim_start_matches('u')
                        .trim_start_matches('U')
                        .trim_start_matches('I')
                        .parse()
                        .unwrap();
                    let rhs_bits: usize = rhs_name
                        .trim_start_matches('u')
                        .trim_start_matches('U')
                        .trim_start_matches('I')
                        .parse()
                        .unwrap();

                    if lhs_bits > rhs_bits {
                        expression = sway::Expression::create_function_calls(
                            None,
                            &[
                                (
                                    format!("u{rhs_bits}::try_from").as_str(),
                                    Some((None, vec![expression.clone()])),
                                ),
                                ("unwrap", Some((None, vec![]))),
                            ],
                        );
                    } else if lhs_bits < rhs_bits {
                        expression = sway::Expression::create_function_calls(
                            Some(expression.clone()),
                            &[(format!("as_u{rhs_bits}").as_str(), Some((None, vec![])))],
                        );
                    }

                    expression = sway::Expression::create_function_calls(
                        None,
                        &[(
                            format!("I{rhs_bits}::from_uint").as_str(),
                            Some((None, vec![expression.clone()])),
                        )],
                    );
                } else {
                    return None;
                }
            }
            // From int to uint
            else if is_int && !to_type_name.is_int() {
                if to_type_name.is_uint() {
                    let lhs_bits: usize = lhs_name
                        .trim_start_matches('u')
                        .trim_start_matches('U')
                        .trim_start_matches('I')
                        .parse()
                        .unwrap();
                    let rhs_bits: usize = rhs_name
                        .trim_start_matches('u')
                        .trim_start_matches('U')
                        .trim_start_matches('I')
                        .parse()
                        .unwrap();

                    if lhs_bits > rhs_bits {
                        expression = sway::Expression::create_function_calls(
                            None,
                            &[
                                (
                                    format!("u{rhs_bits}::try_from").as_str(),
                                    Some((
                                        None,
                                        vec![sway::Expression::from(sway::MemberAccess {
                                            expression: expression.clone(),
                                            member: "underlying".to_string(),
                                        })],
                                    )),
                                ),
                                ("unwrap", Some((None, vec![]))),
                            ],
                        );
                    } else if lhs_bits < rhs_bits {
                        expression = sway::Expression::create_function_calls(
                            Some(expression.clone()),
                            &[
                                ("underlying", None),
                                (format!("as_u{rhs_bits}").as_str(), Some((None, vec![]))),
                            ],
                        );
                    }

                    expression = sway::Expression::create_function_calls(
                        None,
                        &[(
                            format!("I{rhs_bits}::from_uint").as_str(),
                            Some((None, vec![expression.clone()])),
                        )],
                    );
                } else {
                    return None;
                }
            }
            // From uint/int of different bit lengths
            else if (is_uint && to_type_name.is_uint()) || (is_int && to_type_name.is_int()) {
                let lhs_bits: usize = lhs_name
                    .trim_start_matches('u')
                    .trim_start_matches('U')
                    .trim_start_matches('I')
                    .parse()
                    .unwrap();
                let rhs_bits: usize = rhs_name
                    .trim_start_matches('u')
                    .trim_start_matches('U')
                    .trim_start_matches('I')
                    .parse()
                    .unwrap();

                match &expression {
                    sway::Expression::Literal(sway::Literal::DecInt(i, suffix)) => {
                        if suffix.is_none() {
                            expression = sway::Expression::Literal(sway::Literal::DecInt(
                                i.clone(),
                                Some(format!("{}{}", rhs_name.chars().nth(0).unwrap(), rhs_bits)),
                            ));
                        } else {
                            if lhs_bits > rhs_bits {
                                // x.as_u256()
                                // u64::try_from(x).unwrap()
                                expression = sway::Expression::create_function_calls(
                                    None,
                                    &[
                                        (
                                            format!("{to_type_name}::try_from").as_str(),
                                            Some((None, vec![expression.clone()])),
                                        ),
                                        ("unwrap", Some((None, vec![]))),
                                    ],
                                );
                            } else if lhs_bits < rhs_bits {
                                expression = sway::Expression::create_function_calls(
                                    Some(expression.clone()),
                                    &[(
                                        format!("as_{to_type_name}").as_str(),
                                        Some((None, vec![])),
                                    )],
                                );
                            }
                        }
                    }
                    _ => {
                        if lhs_bits > rhs_bits {
                            // x.as_u256()
                            // u64::try_from(x).unwrap()
                            expression = sway::Expression::create_function_calls(
                                None,
                                &[
                                    (
                                        format!("{to_type_name}::try_from").as_str(),
                                        Some((None, vec![expression.clone()])),
                                    ),
                                    ("unwrap", Some((None, vec![]))),
                                ],
                            );
                        } else if lhs_bits < rhs_bits {
                            expression = sway::Expression::create_function_calls(
                                Some(expression.clone()),
                                &[(format!("as_{to_type_name}").as_str(), Some((None, vec![])))],
                            );
                        }
                    }
                }
            }
            // From StorageString to String
            else if lhs_name == "StorageString" && rhs_name == "String" {
                if let sway::Expression::FunctionCall(f) = expression {
                    if let sway::Expression::MemberAccess(member_access) = f.function {
                        if member_access.member == "read" {
                            expression = sway::Expression::create_function_calls(
                                Some(member_access.expression),
                                &[
                                    ("read_slice", Some((None, vec![]))),
                                    ("unwrap", Some((None, vec![]))),
                                ],
                            )
                        } else {
                            return None;
                        }
                    } else {
                        return None;
                    }
                } else {
                    return None;
                }
            }
            // Do not allow incompatible types
            else if !from_type_name.is_compatible_with(to_type_name) {
                return None;
            }
        }

        (
            sway::TypeName::Array {
                type_name: lhs_type_name,
                length: lhs_len,
            },
            sway::TypeName::Array {
                type_name: rhs_type_name,
                length: rhs_len,
            },
        ) => {
            if lhs_len != rhs_len || !lhs_type_name.is_compatible_with(rhs_type_name) {
                todo!("Handle conversion from {from_type_name} to {to_type_name}")
            }

            match expression {
                sway::Expression::Array(array) => {
                    expression = sway::Expression::from(sway::Array {
                        elements: array
                            .elements
                            .iter()
                            .map(|e| coerce_expression(e, lhs_type_name, rhs_type_name).unwrap())
                            .collect(),
                    });
                }

                _ => {
                    expression = sway::Expression::from(sway::Array {
                        elements: (0..*rhs_len)
                            .map(|i| {
                                coerce_expression(
                                    &sway::Expression::from(sway::ArrayAccess {
                                        expression: expression.clone(),
                                        index: sway::Expression::from(sway::Literal::DecInt(
                                            i.into(),
                                            None,
                                        )),
                                    }),
                                    lhs_type_name,
                                    rhs_type_name,
                                )
                                .unwrap()
                            })
                            .collect(),
                    });
                }
            }
        }

        (
            sway::TypeName::Tuple {
                type_names: lhs_type_names,
            },
            sway::TypeName::Tuple {
                type_names: rhs_type_names,
            },
        ) => match &expression {
            sway::Expression::PathExpr(path_expr) if path_expr.is_identifier() => {
                let component_names = ('a'..='z')
                    .enumerate()
                    .take_while(|(i, _)| *i < lhs_type_names.len())
                    .map(|(_, c)| sway::LetIdentifier {
                        is_mutable: false,
                        name: c.to_string(),
                    })
                    .collect::<Vec<_>>();

                let let_stmt = sway::Statement::from(sway::Let {
                    pattern: sway::LetPattern::Tuple(component_names.clone()),
                    type_name: None,
                    value: expression.clone(),
                });

                let exprs = component_names
                    .iter()
                    .enumerate()
                    .map(|(i, c)| {
                        let expr = sway::Expression::create_identifier(c.name.clone());
                        coerce_expression(&expr, &lhs_type_names[i], &rhs_type_names[i])
                    })
                    .collect::<Vec<_>>();

                if exprs.iter().any(|x| x.is_none()) {
                    return None;
                }

                expression = sway::Expression::from(sway::Block {
                    statements: vec![let_stmt],
                    final_expr: Some(sway::Expression::Tuple(
                        exprs.iter().flatten().cloned().collect(),
                    )),
                });
            }

            sway::Expression::Tuple(expressions) => {
                let mut expressions = expressions.clone();

                if expressions.len() != rhs_type_names.len() {
                    return None;
                }

                for (i, (lhs, rhs)) in lhs_type_names.iter().zip(rhs_type_names).enumerate() {
                    match coerce_expression(&expressions[i], lhs, rhs) {
                        Some(expr) => expressions[i] = expr,
                        None => return None,
                    }
                }

                expression = sway::Expression::Tuple(expressions);
            }

            _ => {
                return None;
            }
        },

        (sway::TypeName::StringSlice, sway::TypeName::StringSlice) => {}

        (
            sway::TypeName::StringArray { length: lhs_len },
            sway::TypeName::StringArray { length: rhs_len },
        ) => {
            if lhs_len != rhs_len {
                todo!("Handle coersion from str[{lhs_len}] to str[{rhs_len}]")
            }

            // otherwise it's the same length and we don't need to do anything
        }

        (
            sway::TypeName::Function {
                generic_parameters: _lhs_generic_parameters,
                parameters: _lhs_parameters_list,
                return_type: _lhs_return_type,
                ..
            },
            sway::TypeName::Function {
                generic_parameters: _rhs_generic_parameters,
                parameters: _rhs_parameters_list,
                return_type: _rhs_return_type,
                ..
            },
        ) => todo!(),

        (
            _,
            sway::TypeName::Identifier {
                name,
                generic_parameters,
            },
        ) => match (name.as_str(), generic_parameters.as_ref()) {
            ("Bytes", None) => match from_type_name {
                sway::TypeName::StringSlice => {
                    // Bytes::from(raw_slice::from_parts::<u8>((s.as_ptr(), s.len())))
                    expression = sway::Expression::create_function_calls(
                        None,
                        &[(
                            "Bytes::from",
                            Some((
                                None,
                                vec![sway::Expression::create_function_calls(
                                    None,
                                    &[(
                                        "raw_slice::from_parts",
                                        Some((
                                            Some(sway::GenericParameterList {
                                                entries: vec![sway::GenericParameter {
                                                    type_name: sway::TypeName::Identifier {
                                                        name: "u8".into(),
                                                        generic_parameters: None,
                                                    },
                                                    implements: None,
                                                }],
                                            }),
                                            vec![
                                                sway::Expression::create_function_calls(
                                                    Some(expression.clone()),
                                                    &[("as_ptr", Some((None, vec![])))],
                                                ),
                                                sway::Expression::create_function_calls(
                                                    Some(expression.clone()),
                                                    &[("len", Some((None, vec![])))],
                                                ),
                                            ],
                                        )),
                                    )],
                                )],
                            )),
                        )],
                    );
                }

                _ => return None,
            },

            ("String", None) => match from_type_name {
                sway::TypeName::Identifier {
                    name,
                    generic_parameters,
                } => match (name.as_str(), generic_parameters.as_ref()) {
                    ("todo!", None) => {}
                    ("String", None) => {}
                    _ => todo!(),
                },

                // String::from_ascii_str(x)
                sway::TypeName::StringSlice => {
                    expression = sway::Expression::create_function_calls(
                        None,
                        &[(
                            "String::from_ascii_str",
                            Some((None, vec![expression.clone()])),
                        )],
                    );
                }

                // String::from_ascii_str(from_str_array(x))
                sway::TypeName::StringArray { .. } => {
                    expression = sway::Expression::create_function_calls(
                        None,
                        &[(
                            "String::from_ascii_str",
                            Some((
                                None,
                                vec![sway::Expression::create_function_calls(
                                    None,
                                    &[("from_str_array", Some((None, vec![expression.clone()])))],
                                )],
                            )),
                        )],
                    );
                }

                _ => todo!("{}", sway::TabbedDisplayer(&to_type_name)),
            },

            _ => return None,
        },

        _ => return None,
    }

    Some(expression)
}

pub fn resolve_symbol(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    symbol: Symbol,
) -> Option<Box<dyn std::any::Any>> {
    match &symbol {
        Symbol::TypeDefinition(name) => todo!(),
        Symbol::Event(name) => {
            if let Some(event_enum) = module
                .borrow()
                .events_enums
                .iter()
                .find(|e| e.0.borrow().variants.iter().any(|v| v.name == *name))
            {
                let variant = event_enum
                    .0
                    .borrow()
                    .variants
                    .iter()
                    .find(|v| v.name == *name)
                    .cloned()
                    .unwrap();

                return Some(Box::new((event_enum.0.borrow().name.clone(), variant)));
            }
        }
        Symbol::Enum(name) => todo!(),
        Symbol::Error(name) => todo!(),
        Symbol::Struct(name) => todo!(),
        Symbol::Function(name) => todo!(),
        Symbol::Abi(name) => todo!(),
    }

    for use_expr in module.borrow().uses.iter() {
        if let Some(imported_module) = project.resolve_use(use_expr) {
            return resolve_symbol(project, imported_module.clone(), symbol);
        }
    }

    None
}
