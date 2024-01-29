use crate::{sway, errors::Error};
use solang_parser::pt as solidity;
use std::{
    collections::HashMap,
    fmt::Display,
    path::{Path, PathBuf},
};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct TranslatedVariable {
    pub old_name: String,
    pub new_name: String,
    pub type_name: sway::TypeName,
    pub abi_type_name: Option<sway::TypeName>,
    pub is_storage: bool,
    pub is_configurable: bool,
    pub statement_index: Option<usize>,
    pub read_count: usize,
    pub mutation_count: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TranslatedFunction {
    pub old_name: String,
    pub new_name: String,
    pub parameters: sway::ParameterList,
    pub constructor_calls: Vec<sway::FunctionCall>,
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

#[derive(Clone, Debug, PartialEq)]
pub struct TranslatedEnum {
    pub type_definition: sway::TypeDefinition,
    pub variants_impl: sway::Impl,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct TranslationScope {
    pub parent: Option<Box<TranslationScope>>,
    pub variables: Vec<TranslatedVariable>,
    pub functions: Vec<TranslatedFunction>,
}

impl TranslationScope {
    /// Attempts to get a reference to a translated variable using its old name
    pub fn get_variable_from_old_name(&self, old_name: &str) -> Result<&TranslatedVariable, Error> {
        if let Some(variable) = self.variables.iter().rev().find(|v| v.old_name == old_name) {
            return Ok(variable);
        }

        if let Some(parent) = self.parent.as_ref() {
            if let Ok(variable) = parent.get_variable_from_old_name(old_name) {
                return Ok(variable);
            }
        }

        Err(Error::VariableNotInScope(old_name.into()))
    }

    /// Attempts to get a mutable reference to a translated variable using its old name
    pub fn get_variable_from_old_name_mut(&mut self, old_name: &str) -> Result<&mut TranslatedVariable, Error> {
        if let Some(variable) = self.variables.iter_mut().rev().find(|v| v.old_name == old_name) {
            return Ok(variable);
        }

        if let Some(parent) = self.parent.as_mut() {
            if let Ok(variable) = parent.get_variable_from_old_name_mut(old_name) {
                return Ok(variable);
            }
        }

        Err(Error::VariableNotInScope(old_name.into()))
    }

    /// Attempts to get a reference to a translated variable using its new name
    pub fn get_variable_from_new_name(&self, new_name: &str) -> Result<&TranslatedVariable, Error> {
        if let Some(variable) = self.variables.iter().rev().find(|v| v.new_name == new_name) {
            return Ok(variable);
        }

        if let Some(parent) = self.parent.as_ref() {
            if let Ok(variable) = parent.get_variable_from_new_name(new_name) {
                return Ok(variable);
            }
        }

        Err(Error::VariableNotInScope(new_name.into()))
    }

    /// Attempts to get a mutable reference to a translated variable using its new name
    pub fn get_variable_from_new_name_mut(&mut self, new_name: &str) -> Result<&mut TranslatedVariable, Error> {
        if let Some(variable) = self.variables.iter_mut().rev().find(|v| v.new_name == new_name) {
            return Ok(variable);
        }

        if let Some(parent) = self.parent.as_mut() {
            if let Ok(variable) = parent.get_variable_from_new_name_mut(new_name) {
                return Ok(variable);
            }
        }

        Err(Error::VariableNotInScope(new_name.into()))
    }

    /// Attempts to find a translated variable using a custom function
    pub fn find_variable<F: Copy + FnMut(&&TranslatedVariable) -> bool>(&self, f: F) -> Option<&TranslatedVariable> {
        if let Some(variable) = self.variables.iter().find(f) {
            return Some(variable);
        }

        if let Some(parent) = self.parent.as_ref() {
            if let Some(variable) = parent.find_variable(f) {
                return Some(variable);
            }
        }

        None
    }

    /// Atempts to find a translated function using a custom function
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

    /// Attempts to get a reference to a translated function using its old name
    pub fn get_function_from_old_name(&self, old_name: &str) -> Result<&TranslatedFunction, Error> {
        if let Some(function) = self.functions.iter().rev().find(|v| v.old_name == old_name) {
            return Ok(function);
        }

        if let Some(parent) = self.parent.as_ref() {
            if let Ok(function) = parent.get_function_from_old_name(old_name) {
                return Ok(function);
            }
        }

        Err(Error::FunctionNotInScope(old_name.into()))
    }

    pub fn get_expression_type(
        &self,
        expression: &sway::Expression,
    ) -> Result<sway::TypeName, Error> {
        match expression {
            sway::Expression::Literal(literal) => match literal {
                sway::Literal::Bool(_) => Ok(sway::TypeName::Identifier {
                    name: "bool".into(),
                    generic_parameters: None,
                }),
                sway::Literal::DecInt(_) => Ok(sway::TypeName::Identifier {
                    name: "u64".into(), // TODO: is this ok?
                    generic_parameters: None,
                }),
                sway::Literal::HexInt(_) => Ok(sway::TypeName::Identifier {
                    name: "u64".into(), // TODO: is this ok?
                    generic_parameters: None,
                }),
                sway::Literal::String(_) => Ok(sway::TypeName::StringSlice),
            }

            sway::Expression::Identifier(name) => {
                let variable = self.get_variable_from_new_name(name)?;

                // Variable should not be a storage field
                if variable.is_storage {
                    return Err(Error::VariableNotInScope(name.clone()));
                }

                Ok(variable.type_name.clone())
            }

            sway::Expression::FunctionCall(function_call) => match &function_call.function {
                sway::Expression::Identifier(name) => match name.as_str() {
                    "todo!" => Ok(sway::TypeName::Identifier {
                        name: "todo!".into(),
                        generic_parameters: None,
                    }),

                    "Identity::Address" | "Identity::ContractId" | "Identity::from" => Ok(sway::TypeName::Identifier {
                        name: "Identity".into(),
                        generic_parameters: None,
                    }),

                    "msg_sender" => Ok(sway::TypeName::Identifier {
                        name: "Option".into(),
                        generic_parameters: Some(sway::GenericParameterList {
                            entries: vec![
                                sway::GenericParameter {
                                    type_name: sway::TypeName::Identifier {
                                        name: "Identity".into(),
                                        generic_parameters: None,
                                    },
                                    implements: None,
                                },
                            ],
                        }),
                    }),

                    new_name => {
                        // Ensure the function exists in scope
                        let Some(function) = self.find_function(|f| {
                            // Ensure the function's new name matches the function call we're translating
                            if f.new_name != new_name {
                                return false;
                            }
                            
                            // Ensure the supplied function call args match the function's parameters
                            if function_call.parameters.len() != f.parameters.entries.len() {
                                return false;
                            }

                            for (i, parameter) in function_call.parameters.iter().enumerate() {
                                let Some(parameter_type_name) = f.parameters.entries[i].type_name.as_ref() else { continue };

                                // Don't check literal integer value types
                                if let sway::Expression::Literal(sway::Literal::DecInt(_) | sway::Literal::HexInt(_)) = parameter {
                                    if let sway::TypeName::Identifier { name, generic_parameters: None } = parameter_type_name {
                                        if let "u8" | "u16" | "u32" | "u64" | "u256" = name.as_str() {
                                            continue;
                                        }
                                    }
                                }

                                let value_type_name = self.get_expression_type(parameter).unwrap();

                                // HACK: Don't check todo! value types
                                if let sway::TypeName::Identifier { name, generic_parameters: None } = &value_type_name {
                                    if name == "todo!" {
                                        continue;
                                    }
                                }

                                if value_type_name != *parameter_type_name {
                                    return false;
                                }
                            }

                            true
                        }) else {
                            panic!("Failed to find function `{new_name}` in scope");
                        };

                        if let Some(return_type) = function.return_type.as_ref() {
                            Ok(return_type.clone())
                        } else {
                            Ok(sway::TypeName::Tuple { type_names: vec![] })
                        }
                    }
                }

                sway::Expression::MemberAccess(member_access) => match member_access.member.as_str() {
                    "unwrap" => match self.get_expression_type(&member_access.expression)? {
                        sway::TypeName::Identifier { name, generic_parameters: Some(generic_parameters) } => match name.as_str() {
                            "Option" => Ok(generic_parameters.entries.first().unwrap().type_name.clone()),
                            "Result" => Ok(generic_parameters.entries.first().unwrap().type_name.clone()),
                            _ => todo!("get type of function call expression: {expression:#?}"),
                        }

                        _ => todo!("get type of function call expression: {expression:#?}"),
                    }
                    
                    "get" => match self.get_expression_type(&member_access.expression)? {
                        sway::TypeName::Identifier { name, generic_parameters: Some(generic_parameters) } => match name.as_str() {
                            "Vec" => Ok(sway::TypeName::Identifier {
                                name: "Option".into(),
                                generic_parameters: Some(sway::GenericParameterList {
                                    entries: vec![
                                        generic_parameters.entries.first().unwrap().clone(),
                                    ],
                                }),
                            }),

                            _ => todo!("get type of function call expression: {expression:#?}"),
                        }
                        
                        _ => todo!("get type of function call expression: {expression:#?}"),
                    }

                    "pow" => match self.get_expression_type(&member_access.expression)? {
                        t if t.is_uint() => Ok(t),
                        _ => todo!("get type of `pow` function call expression: {expression:#?}"),
                    }

                    "read" => match self.get_expression_type(&member_access.expression)? {
                        sway::TypeName::Identifier { name, generic_parameters: Some(generic_parameters) } => match name.as_str() {
                            "StorageKey" => Ok(generic_parameters.entries.first().unwrap().type_name.clone()),
                            _ => todo!("get type of function call expression: {expression:#?}"),
                        }

                        _ => todo!("get type of function call expression: {expression:#?}"),
                    }

                    _ => todo!("get type of function call expression: {expression:#?}"),
                }

                _ => todo!("get type of function call expression: {expression:#?}"),
            }

            sway::Expression::Block(block) => {
                if let Some(expression) = block.final_expr.as_ref() {
                    self.get_expression_type(expression)
                } else {
                    Ok(sway::TypeName::Tuple { type_names: vec![] })
                }
            }

            sway::Expression::Return(value) => {
                if let Some(value) = value.as_ref() {
                    self.get_expression_type(value)
                } else {
                    Ok(sway::TypeName::Tuple { type_names: vec![] })
                }
            }

            sway::Expression::Array(array) => Ok(sway::TypeName::Array {
                type_name: Box::new(
                    if let Some(expression) = array.elements.first() {
                        self.get_expression_type(expression)?
                    } else {
                        sway::TypeName::Tuple { type_names: vec![] }
                    }
                ),
                length: array.elements.len(),
            }),

            sway::Expression::ArrayAccess(array_access) => {
                let element_type_name = self.get_expression_type(&array_access.expression)?;
                
                let type_name = match &element_type_name {
                    sway::TypeName::Identifier {
                        name,
                        generic_parameters: Some(generic_parameters),
                    } if name == "Vec" => {
                        &generic_parameters.entries.first().unwrap().type_name
                    }

                    sway::TypeName::Array { type_name, .. } => type_name.as_ref(),

                    _ => todo!("array access for type {element_type_name}"),
                };

                Ok(type_name.clone())
            }

            sway::Expression::MemberAccess(member_access) => match &member_access.expression {
                sway::Expression::Identifier(name) => match name.as_str() {
                    "storage" => {
                        let Some(variable) = self.find_variable(|v| v.is_storage && v.new_name == member_access.member) else {
                            panic!("Failed to find storage variable in scope: `{}`", member_access.member);
                        };

                        Ok(sway::TypeName::Identifier {
                            name: "StorageKey".into(),
                            generic_parameters: Some(sway::GenericParameterList {
                                entries: vec![
                                    sway::GenericParameter {
                                        type_name: variable.type_name.clone(),
                                        implements: None,
                                    },
                                ],
                            }),
                        })
                    }

                    _ => todo!("get type of member access expression: {expression:#?}"),
                }

                _ => todo!("get type of member access expression: {expression:#?}"),
            }
            
            sway::Expression::Tuple(tuple) => Ok(sway::TypeName::Tuple {
                type_names: tuple.iter().map(|x| self.get_expression_type(x)).collect::<Result<Vec<_>, _>>()?,
            }),
            
            sway::Expression::If(if_expr) => {
                if let Some(expression) = if_expr.then_body.final_expr.as_ref() {
                    self.get_expression_type(expression)
                } else {
                    Ok(sway::TypeName::Tuple { type_names: vec![] })
                }
            }

            sway::Expression::Match(match_expr) => {
                if let Some(branch) = match_expr.branches.first() {
                    self.get_expression_type(&branch.value)
                } else {
                    Ok(sway::TypeName::Tuple { type_names: vec![] })
                }
            }
            
            sway::Expression::While(_) => Ok(sway::TypeName::Tuple { type_names: vec![] }),
            sway::Expression::UnaryExpression(unary_expression) => self.get_expression_type(&unary_expression.expression),
            sway::Expression::BinaryExpression(binary_expression) => self.get_expression_type(&binary_expression.lhs),
            sway::Expression::Constructor(constructor) => Ok(constructor.type_name.clone()),
            sway::Expression::Continue => Ok(sway::TypeName::Tuple { type_names: vec![] }),
            sway::Expression::Break => Ok(sway::TypeName::Tuple { type_names: vec![] }),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TranslatedUsingDirective {
    pub library_name: String,
    pub for_type: Option<sway::TypeName>,
    pub functions: Vec<TranslatedFunction>,
}

#[derive(Clone, Debug)]
pub struct TranslatedDefinition {
    pub path: PathBuf,
    pub toplevel_scope: TranslationScope,
    pub kind: solidity::ContractTy,
    pub uses: Vec<sway::Use>,
    pub name: String,
    pub inherits: Vec<String>,
    pub using_directives: Vec<TranslatedUsingDirective>,
    pub type_definitions: Vec<sway::TypeDefinition>,
    pub structs: Vec<sway::Struct>,
    pub enums: Vec<TranslatedEnum>,
    pub events_enums: Vec<(sway::Enum, sway::Impl)>,
    pub errors_enums: Vec<(sway::Enum, sway::Impl)>,
    pub constants: Vec<sway::Constant>,
    pub abis: Vec<sway::Abi>,
    pub abi: Option<sway::Abi>,
    pub configurable: Option<sway::Configurable>,
    pub storage: Option<sway::Storage>,
    pub modifiers: Vec<TranslatedModifier>,
    pub functions: Vec<sway::Function>,
    pub impls: Vec<sway::Impl>,
    
    pub function_name_counts: HashMap<String, usize>,
    pub function_names: HashMap<String, String>,
    pub function_call_counts: HashMap<String, usize>,

    pub storage_fields_name_counts: HashMap<String, usize>,
    pub storage_fields_names: HashMap<String, String>,
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
            if i == 0 && written > 0 {
                writeln!(f)?;
            } else if i > 0 {
                writeln!(f)?;
            }

            writeln!(f, "{}", sway::TabbedDisplayer(&x.type_definition))?;
            writeln!(f)?;
            writeln!(f, "{}", sway::TabbedDisplayer(&x.variants_impl))?;
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
        
        for (i, (events_enum, abi_encode_impl)) in self.events_enums.iter().enumerate() {
            if i == 0 && written > 0 {
                writeln!(f)?;
            } else if i > 0 {
                writeln!(f)?;
            }

            writeln!(f, "{}", sway::TabbedDisplayer(events_enum))?;
            writeln!(f)?;
            writeln!(f, "{}", sway::TabbedDisplayer(abi_encode_impl))?;
            written += 1;
        }

        for (i, (errors_enum, abi_encode_impl)) in self.errors_enums.iter().enumerate() {
            if i == 0 && written > 0 {
                writeln!(f)?;
            } else if i > 0 {
                writeln!(f)?;
            }

            writeln!(f, "{}", sway::TabbedDisplayer(errors_enum))?;
            writeln!(f)?;
            writeln!(f, "{}", sway::TabbedDisplayer(abi_encode_impl))?;
            written += 1;
        }
        
        for (i, x) in self.abis.iter().enumerate() {
            if i == 0 && written > 0 {
                writeln!(f)?;
            } else if i > 0 {
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
            function_name_counts: HashMap::new(),
            function_names: HashMap::new(),
            function_call_counts: HashMap::new(),
            storage_fields_name_counts: HashMap::new(),
            storage_fields_names: HashMap::new(),
        }
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

    // Gets the base underlying type of the supplied type name
    pub fn get_underlying_type(&self, type_name: &sway::TypeName) -> sway::TypeName {
        // Check to see if the expression's type is a type definition and get the underlying type
        for type_definition in self.type_definitions.iter() {
            if &type_definition.name == type_name {
                return self.get_underlying_type(
                    type_definition.underlying_type.as_ref().unwrap(),
                );
            }
        }

        // If we didn't find a type definition, check to see if an enum exists and get its underlying type
        for translated_enum in self.enums.iter() {
            if &translated_enum.type_definition.name == type_name {
                return self.get_underlying_type(
                    translated_enum.type_definition.underlying_type.as_ref().unwrap(),
                );
            }
        }

        type_name.clone()
    }

    pub fn ensure_use_declared(&mut self, name: &str) {
        let mut tree: Option<sway::UseTree> = None;
        
        for part in name.split("::").collect::<Vec<_>>().into_iter().rev() {
            match part {
                "*" => tree = Some(sway::UseTree::Glob),
                
                _ => tree = Some(if let Some(use_tree) = tree.clone() {
                    sway::UseTree::Path {
                        prefix: part.into(),
                        suffix: Box::new(use_tree),
                    }
                } else {
                    sway::UseTree::Name {
                        name: part.into(),
                    }
                }),
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
}
