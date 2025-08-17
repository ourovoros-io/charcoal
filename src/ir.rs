use crate::{project::Project, sway, translate::*};
use convert_case::Case;
use solang_parser::pt as solidity;
use std::{cell::RefCell, collections::HashMap, path::PathBuf, rc::Rc};

#[derive(Clone, Debug, PartialEq)]
pub struct UsingDirective {
    pub library_name: String,
    pub for_type: Option<sway::TypeName>,
    pub functions: Vec<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Enum {
    pub type_definition: sway::TypeDefinition,
    pub variants_impl: sway::Impl,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
    pub name: String,
    pub memory: sway::Struct,
    pub storage: sway::Struct,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Variable {
    pub old_name: String,
    pub new_name: String,
    pub type_name: sway::TypeName,
    pub statement_index: Option<usize>,
    pub read_count: usize,
    pub mutation_count: usize,
}

#[derive(Clone, Debug)]
pub struct VariableAccess {
    pub variable: Option<Rc<RefCell<Variable>>>,
    pub expression: sway::Expression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub old_name: String,
    pub new_name: String,
    pub attributes: Option<sway::AttributeList>,
    pub type_name: sway::TypeName,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionNames {
    pub old_name: String,
    pub top_level_fn_name: String,
    pub abi_fn_name: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Modifier {
    pub old_name: String,
    pub new_name: String,
    pub parameters: sway::ParameterList,
    pub storage_struct_parameter: Option<sway::Parameter>,
    pub attributes: Option<sway::AttributeList>,
    pub has_underscore: bool,
    pub inline_body: Option<sway::Block>,
    pub pre_body: Option<sway::Block>,
    pub post_body: Option<sway::Block>,
}

#[derive(Debug, Clone)]
pub struct Scope {
    contract_name: Option<String>,
    function_name: Option<String>,
    parent: Option<Rc<RefCell<Scope>>>,
    variables: Vec<Rc<RefCell<Variable>>>,
}

impl Scope {
    pub fn new(
        contract_name: Option<&str>,
        function_name: Option<&str>,
        parent: Option<Rc<RefCell<Scope>>>,
    ) -> Self {
        Self {
            contract_name: contract_name.map(str::to_string),
            function_name: function_name.map(str::to_string),
            parent,
            variables: vec![],
        }
    }

    pub fn get_contract_name(&self) -> Option<String> {
        if let Some(contract_name) = self.contract_name.as_ref() {
            return Some(contract_name.clone());
        }

        if let Some(parent) = self.parent.as_ref()
            && let Some(contract_name) = parent.borrow().get_contract_name()
        {
            return Some(contract_name.clone());
        }

        None
    }

    pub fn get_function_name(&self) -> Option<String> {
        if let Some(function_name) = self.function_name.as_ref() {
            return Some(function_name.clone());
        }

        if let Some(parent) = self.parent.as_ref()
            && let Some(function_name) = parent.borrow().get_function_name()
        {
            return Some(function_name.clone());
        }

        None
    }

    pub fn set_function_name(&mut self, function_name: &str) {
        self.function_name = Some(function_name.to_string());
    }

    #[inline]
    pub fn get_parent(&self) -> Option<Rc<RefCell<Scope>>> {
        self.parent.clone()
    }

    #[inline]
    pub fn get_variables(&self) -> Vec<Rc<RefCell<Variable>>> {
        self.variables.clone()
    }

    #[inline]
    pub fn add_variable(&mut self, variable: Rc<RefCell<Variable>>) {
        self.variables.push(variable);
    }

    pub fn dump(&self) {
        if let Some(parent) = self.parent.as_ref() {
            parent.borrow().dump();
        }

        println!("variables:");

        for v in self.variables.iter() {
            println!("{v:#?}");
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
    pub fn get_variable_from_old_name(&self, old_name: &str) -> Option<Rc<RefCell<Variable>>> {
        if let Some(variable) = self
            .variables
            .iter()
            .rev()
            .find(|v| v.borrow().old_name == old_name)
        {
            return Some(variable.clone());
        }

        if let Some(parent) = self.parent.as_ref()
            && let Some(variable) = parent.borrow().get_variable_from_old_name(old_name)
        {
            return Some(variable);
        }

        None
    }

    /// Attempts to get a reference to a translated variable using its new name
    pub fn get_variable_from_new_name(&self, new_name: &str) -> Option<Rc<RefCell<Variable>>> {
        if let Some(variable) = self
            .variables
            .iter()
            .rev()
            .find(|v| v.borrow().new_name == new_name)
        {
            return Some(variable.clone());
        }

        if let Some(parent) = self.parent.as_ref()
            && let Some(variable) = parent.borrow().get_variable_from_new_name(new_name)
        {
            return Some(variable);
        }

        None
    }

    /// Attempts to find a translated variable using a custom function
    pub fn find_variable<F: Copy + FnMut(&&Rc<RefCell<Variable>>) -> bool>(
        &self,
        f: F,
    ) -> Option<Rc<RefCell<Variable>>> {
        if let Some(variable) = self.variables.iter().find(f) {
            return Some(variable.clone());
        }

        if let Some(parent) = self.parent.as_ref()
            && let Some(variable) = parent.borrow().find_variable(f)
        {
            return Some(variable);
        }

        None
    }

    pub fn set_function_storage_accesses(
        &self,
        module: Rc<RefCell<Module>>,
        storage_read: bool,
        storage_write: bool,
    ) {
        if let Some(function_name) = self.get_function_name() {
            if storage_read {
                module
                    .borrow_mut()
                    .function_storage_accesses
                    .entry(function_name.clone())
                    .or_default()
                    .0 = true;
            }

            if storage_write {
                module
                    .borrow_mut()
                    .function_storage_accesses
                    .entry(function_name)
                    .or_default()
                    .1 = true;
            }
        }
    }

    pub fn add_function_parameters(
        &mut self,
        project: &mut Project,
        module: Rc<RefCell<Module>>,
        function_definition: solidity::FunctionDefinition,
    ) -> Vec<crate::ir::Variable> {
        let mut parameters = vec![];

        for (_, p) in function_definition.params.iter() {
            let old_name = p
                .as_ref()
                .unwrap()
                .name
                .as_ref()
                .map_or("_".into(), |n| n.name.clone());
            let new_name = translate_naming_convention(old_name.as_str(), Case::Snake);

            let type_name = translate_type_name(
                project,
                module.clone(),
                Rc::new(RefCell::new(self.clone())),
                &p.as_ref().unwrap().ty,
                p.as_ref().and_then(|p| p.storage.as_ref()),
            );

            let translated_variable = crate::ir::Variable {
                old_name,
                new_name,
                type_name,
                ..Default::default()
            };

            parameters.push(translated_variable.clone());

            self.add_variable(Rc::new(RefCell::new(translated_variable)));
        }

        parameters
    }

    pub fn add_function_return_parameters(
        &mut self,
        project: &mut Project,
        module: Rc<RefCell<Module>>,
        function_definition: solidity::FunctionDefinition,
    ) -> Vec<crate::ir::Variable> {
        let mut return_parameters = vec![];
        for (_, return_parameter) in function_definition.returns.iter() {
            let Some(return_parameter) = return_parameter else {
                continue;
            };

            let Some(old_name) = return_parameter.name.as_ref().map(|n| n.name.clone()) else {
                continue;
            };

            let new_name = translate_naming_convention(old_name.as_str(), Case::Snake);

            let type_name = translate_type_name(
                project,
                module.clone(),
                Rc::new(RefCell::new(self.clone())),
                &return_parameter.ty,
                return_parameter.storage.as_ref(),
            );

            let translated_variable = crate::ir::Variable {
                old_name,
                new_name,
                type_name,
                ..Default::default()
            };

            return_parameters.push(translated_variable.clone());

            self.add_variable(Rc::new(RefCell::new(translated_variable)));
        }
        return_parameters
    }
}

#[derive(Clone, Debug)]
pub struct Item<T> {
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
pub struct Contract {
    pub name: String,
    pub kind: solidity::ContractTy,
    pub abi: sway::Abi,
    pub abi_impl: sway::Impl,
    pub storage: Option<Rc<RefCell<sway::Storage>>>,
    pub storage_struct: Option<Rc<RefCell<Struct>>>,
    pub storage_struct_constructor_fn: Option<sway::Function>,
}

impl Contract {
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
                type_name: sway::TypeName::create_identifier(name),
                for_type_name: Some(sway::TypeName::create_identifier("Contract")),
                items: vec![],
            },
            storage: None,
            storage_struct: None,
            storage_struct_constructor_fn: None,
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct Module {
    pub name: String,
    pub path: PathBuf,
    pub submodules: Vec<Rc<RefCell<Module>>>,
    pub dependencies: Vec<String>,

    pub uses: Vec<sway::Use>,
    pub using_directives: Vec<UsingDirective>,
    pub type_definitions: Vec<Item<sway::TypeDefinition>>,
    pub structs: Vec<Item<Rc<RefCell<Struct>>>>,
    pub enums: Vec<Item<Enum>>,
    pub events_enums: Vec<(Rc<RefCell<sway::Enum>>, Rc<RefCell<sway::Impl>>)>,
    pub errors_enums: Vec<(Rc<RefCell<sway::Enum>>, Rc<RefCell<sway::Impl>>)>,
    pub constants: Vec<sway::Constant>,
    pub configurable: Option<sway::Configurable>,
    pub modifiers: Vec<Item<Rc<RefCell<Modifier>>>>,
    pub functions: Vec<Item<sway::Function>>,
    pub contracts: Vec<Item<Rc<RefCell<Contract>>>>,
    pub impls: Vec<sway::Impl>,

    pub contract_function_name_counts: HashMap<String, Rc<RefCell<HashMap<String, usize>>>>,
    pub contract_function_names: HashMap<String, Rc<RefCell<HashMap<String, String>>>>,

    pub function_name_counts: Rc<RefCell<HashMap<String, usize>>>,
    pub function_names: Rc<RefCell<HashMap<String, String>>>,
    pub function_constructor_calls: HashMap<String, Vec<sway::FunctionCall>>,
    pub function_modifiers: HashMap<String, Vec<sway::FunctionCall>>,
    pub function_storage_accesses: HashMap<String, (bool, bool)>,

    pub constant_name_counts: HashMap<String, usize>,
}

impl Module {
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
    pub fn get_storage(&mut self, scope: Rc<RefCell<Scope>>) -> Rc<RefCell<sway::Storage>> {
        let contract_name = scope.borrow().get_contract_name().unwrap();

        let contract_item = self
            .contracts
            .iter()
            .find(|c| c.signature.to_string() == contract_name)
            .unwrap();

        let contract = contract_item.implementation.clone().unwrap();

        if contract.borrow().storage.is_none() {
            contract.borrow_mut().storage = Some(Rc::new(RefCell::new(sway::Storage::default())));
        }

        contract.borrow().storage.clone().unwrap()
    }

    /// Gets the storage struct for the translated definition. If it doesn't exist, it gets created.
    #[inline]
    pub fn get_storage_struct(&mut self, scope: Rc<RefCell<Scope>>) -> Rc<RefCell<Struct>> {
        let contract_name = scope.borrow().get_contract_name().unwrap();

        let contract_item = self
            .contracts
            .iter()
            .find(|c| c.signature.to_string() == contract_name)
            .unwrap();

        let contract = contract_item.implementation.clone().unwrap();

        if contract.borrow().storage_struct.is_none() {
            contract.borrow_mut().storage_struct = Some(Rc::new(RefCell::new(Struct {
                name: format!("{contract_name}Storage"),
                memory: sway::Struct {
                    attributes: None,
                    is_public: true,
                    name: String::new(),
                    generic_parameters: None,
                    fields: vec![],
                },
                storage: sway::Struct {
                    attributes: None,
                    is_public: true,
                    name: format!("{contract_name}Storage"),
                    generic_parameters: None,
                    fields: vec![],
                },
            })));
        }

        contract.borrow().storage_struct.clone().unwrap()
    }

    /// Attempt to get storage namespace by name from the translated definition. If it doesn't exist, it gets created.
    #[inline]
    pub fn get_storage_namespace(
        &mut self,
        scope: Rc<RefCell<Scope>>,
    ) -> Option<Rc<RefCell<sway::StorageNamespace>>> {
        let namespace_name = self.get_storage_namespace_name(scope.clone())?;
        let storage = self.get_storage(scope.clone());

        if !storage
            .borrow()
            .namespaces
            .iter()
            .any(|s| s.borrow().name == namespace_name)
        {
            storage
                .borrow_mut()
                .namespaces
                .push(Rc::new(RefCell::new(sway::StorageNamespace {
                    name: namespace_name.clone(),
                    fields: vec![],
                    namespaces: vec![],
                })));
        }

        storage
            .borrow()
            .namespaces
            .iter()
            .find(|s| s.borrow().name == namespace_name)
            .cloned()
    }

    /// Ensure the supplied storage field exists in both the current contract's storage namespace and storage struct.
    #[inline]
    pub fn create_storage_field(
        &mut self,
        scope: Rc<RefCell<Scope>>,
        name: &str,
        type_name: &sway::TypeName,
        value: &sway::Expression,
    ) {
        // Ensure the field exists in the storage namespace
        let storage = self.get_storage_namespace(scope.clone()).unwrap();

        if let Some(field) = storage.borrow().fields.iter().find(|f| f.name == name) {
            assert!(
                field.type_name == *type_name,
                "{name} field already exists: {field:#?}"
            );
        } else {
            storage.borrow_mut().fields.push(sway::StorageField {
                old_name: String::new(),
                name: name.to_string(),
                type_name: type_name.clone(),
                value: value.clone(),
            });
        }

        // Ensure the field exists in the storage struct
        let storage_struct = self.get_storage_struct(scope.clone());

        if let Some(field) = storage_struct
            .borrow()
            .storage
            .fields
            .iter()
            .find(|f| f.new_name == name)
        {
            let mut valid = false;

            if let Some(storage_key_type) = field.type_name.storage_key_type()
                && storage_key_type == *type_name
            {
                valid = true;
            }

            assert!(valid, "{name} field already exists: {field:#?}");
        } else {
            storage_struct
                .borrow_mut()
                .storage
                .fields
                .push(sway::StructField {
                    is_public: true,
                    new_name: name.to_string(),
                    old_name: String::new(),
                    type_name: type_name.to_storage_key(),
                });
        }
    }

    /// Gets the name of the storage namespace from the translated definition.
    #[inline]
    pub fn get_storage_namespace_name(&self, scope: Rc<RefCell<Scope>>) -> Option<String> {
        let contract_name = scope.borrow().get_contract_name()?;

        Some(translate_naming_convention(&contract_name, Case::Snake))
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

    /// Returns the translated module name in snake case
    #[inline]
    pub fn get_module_name(&self) -> String {
        translate_naming_convention(&self.name, Case::Snake)
    }
}

impl From<Module> for sway::Module {
    fn from(module: Module) -> Self {
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
                x.implementation.as_ref().unwrap().borrow().clone().memory,
            ));
            items.push(sway::ModuleItem::Struct(
                x.implementation.unwrap().borrow().clone().storage,
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

        let mut module_storage: Option<Rc<RefCell<sway::Storage>>> = None;

        for contract_item in module.contracts.iter() {
            let contract = contract_item.implementation.clone().unwrap();
            let contract = contract.borrow();

            if let Some(storage_struct) = contract.storage_struct.as_ref() {
                items.push(sway::ModuleItem::Struct(
                    storage_struct.borrow().storage.clone(),
                ));
                assert!(storage_struct.borrow().memory.fields.is_empty());
            }

            let Some(storage) = contract.storage.as_ref() else {
                continue;
            };

            let Some(module_storage) = module_storage.as_mut() else {
                module_storage = Some(storage.clone());
                continue;
            };

            for field in storage.borrow().fields.iter() {
                if !module_storage.borrow().fields.contains(field) {
                    module_storage.borrow_mut().fields.push(field.clone());
                }
            }

            for namespace in storage.borrow().namespaces.iter() {
                let mut module_storage = module_storage.borrow_mut();

                let Some(module_namespace) = module_storage
                    .namespaces
                    .iter()
                    .find(|n| n.borrow().name == namespace.borrow().name)
                else {
                    module_storage.namespaces.push(namespace.clone());
                    continue;
                };

                fn expand_storage_namespace(
                    input: Rc<RefCell<sway::StorageNamespace>>,
                    output: Rc<RefCell<sway::StorageNamespace>>,
                ) {
                    let input = input.borrow();
                    let mut output = output.borrow_mut();

                    for input_field in input.fields.iter() {
                        if !output.fields.contains(input_field) {
                            output.fields.push(input_field.clone());
                        }
                    }

                    for input_namespace in input.namespaces.iter() {
                        let Some(output_namespace) = output
                            .namespaces
                            .iter_mut()
                            .find(|n| n.borrow().name == input_namespace.borrow().name)
                        else {
                            output.namespaces.push(input_namespace.clone());
                            continue;
                        };

                        expand_storage_namespace(input_namespace.clone(), output_namespace.clone());
                    }
                }

                expand_storage_namespace(namespace.clone(), module_namespace.clone());
            }
        }

        if let Some(x) = module_storage {
            items.push(sway::ModuleItem::Storage(x.borrow().clone()));
        }

        for contract_item in module.contracts {
            let contract = contract_item.implementation.as_ref().unwrap().borrow();

            if contract.abi.functions.is_empty() == contract.abi_impl.items.is_empty()
                || !contract.abi.functions.is_empty()
            {
                items.push(sway::ModuleItem::Abi(contract.abi.clone()));
            }

            if contract.abi.functions.is_empty() == contract.abi_impl.items.is_empty()
                || !contract.abi_impl.items.is_empty()
            {
                items.push(sway::ModuleItem::Impl(contract.abi_impl.clone()));
            }
        }

        for x in module.impls {
            items.push(sway::ModuleItem::Impl(x));
        }

        for x in module.functions {
            let mut function = x.implementation.unwrap();

            if let Some(storage_struct_parameter) = function.storage_struct_parameter.as_ref() {
                function
                    .parameters
                    .entries
                    .push(storage_struct_parameter.clone());
            }

            items.push(sway::ModuleItem::Function(function));
        }

        sway::Module { kind, items }
    }
}
