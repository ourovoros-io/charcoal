use num_bigint::BigUint;
use std::fmt::Display;

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

pub trait TabbedDisplay {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
}

impl<T: Display> TabbedDisplay for T {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (0..depth).map(|_| "    ").collect::<String>().fmt(f)?;
        self.fmt(f)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

pub struct TabbedDisplayer<'a, T: TabbedDisplay>(pub &'a T);

impl<T: TabbedDisplay> Display for TabbedDisplayer<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.tabbed_fmt(0, f)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub enum ModuleKind {
    Contract,
    Library,
    Script,
    Predicate,
}

impl Display for ModuleKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModuleKind::Contract => write!(f, "contract"),
            ModuleKind::Library => write!(f, "library"),
            ModuleKind::Script => write!(f, "script"),
            ModuleKind::Predicate => write!(f, "predicate"),
        }
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub kind: ModuleKind,
    pub items: Vec<ModuleItem>,
}

impl Module {
    pub fn new(kind: ModuleKind) -> Self {
        Self {
            kind,
            items: vec![],
        }
    }

    /// Retrieves the `abi` item with the specified name from the module, creating it if it doesn't exist.
    pub fn get_or_create_abi(&mut self, abi_name: &str) -> &mut Abi {
        if !self.items.iter().any(|x| {
            let ModuleItem::Abi(abi) = x else {
                return false;
            };
            abi.name == abi_name
        }) {
            self.items.push(ModuleItem::Abi(Abi {
                name: abi_name.into(),
                inherits: vec![],
                functions: vec![],
            }));
        }

        let Some(ModuleItem::Abi(result)) = self.items.iter_mut().find(|x| {
            let ModuleItem::Abi(abi) = x else {
                return false;
            };
            abi.name == abi_name
        }) else {
            panic!("Failed to find ABI item in module")
        };

        result
    }

    /// Retrieves the `impl _ for _` with the specified types from the module, creating it if it doesn't exist.
    pub fn get_or_create_impl_for(&mut self, impl_name: &str, for_name: &str) -> &mut Impl {
        if !self.items.iter().any(|x| {
            let ModuleItem::Impl(x) = x else { return false };
            let TypeName::Identifier {
                name: impl_type_name,
                ..
            } = &x.type_name
            else {
                return false;
            };
            let Some(TypeName::Identifier {
                name: for_type_name,
                ..
            }) = x.for_type_name.as_ref()
            else {
                return false;
            };
            impl_type_name == impl_name && for_type_name == for_name
        }) {
            self.items.push(ModuleItem::Impl(Impl {
                generic_parameters: None,
                type_name: TypeName::Identifier {
                    name: impl_name.into(),
                    generic_parameters: None,
                },
                for_type_name: Some(TypeName::Identifier {
                    name: for_name.into(),
                    generic_parameters: None,
                }),
                items: vec![],
            }));
        }

        let Some(ModuleItem::Impl(result)) = self.items.iter_mut().find(|x| {
            let ModuleItem::Impl(x) = x else { return false };
            let TypeName::Identifier {
                name: impl_type_name,
                ..
            } = &x.type_name
            else {
                return false;
            };
            let Some(TypeName::Identifier {
                name: for_type_name,
                ..
            }) = x.for_type_name.as_ref()
            else {
                return false;
            };
            impl_type_name == impl_name && for_type_name == for_name
        }) else {
            panic!("Failed to find impl item in module");
        };

        result
    }

    /// Retrieves the `storage` item from the module, creating it if it doesn't exist.
    pub fn get_or_create_storage(&mut self) -> &mut Storage {
        if !self
            .items
            .iter()
            .any(|x| matches!(x, ModuleItem::Storage(_)))
        {
            self.items.push(ModuleItem::Storage(Storage::default()));
        }

        let Some(ModuleItem::Storage(result)) = self
            .items
            .iter_mut()
            .find(|x| matches!(x, ModuleItem::Storage(_)))
        else {
            panic!("Failed to find storage item in module")
        };

        result
    }

    /// Retrieves the `configurable` item from the module, creating it if it doesn't exist.
    pub fn get_or_create_configurable(&mut self) -> &mut Configurable {
        if !self
            .items
            .iter()
            .any(|x| matches!(x, ModuleItem::Configurable(_)))
        {
            self.items
                .push(ModuleItem::Configurable(Configurable::default()));
        }

        let Some(ModuleItem::Configurable(result)) = self
            .items
            .iter_mut()
            .find(|x| matches!(x, ModuleItem::Configurable(_)))
        else {
            panic!("Failed to find configurable item in module")
        };

        result
    }
}

impl TabbedDisplay for Module {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{};", self.kind)?;
        writeln!(f)?;

        let mut prev_item: Option<&ModuleItem> = None;

        for (i, item) in self.items.iter().enumerate() {
            if let Some(prev_item) = prev_item {
                if !(matches!(prev_item, ModuleItem::Use(_)) && matches!(item, ModuleItem::Use(_))
                    || matches!(prev_item, ModuleItem::Constant(_))
                        && matches!(item, ModuleItem::Constant(_))
                    || matches!(prev_item, ModuleItem::TypeDefinition(_))
                        && matches!(item, ModuleItem::TypeDefinition(_)))
                {
                    writeln!(f)?;
                }
            } else if i > 0 {
                writeln!(f)?;
            }

            item.tabbed_fmt(depth, f)?;
            writeln!(f)?;

            prev_item = Some(item);
        }

        Ok(())
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct Submodule {
    pub name: String,
    pub items: Vec<ModuleItem>,
}

impl TabbedDisplay for Submodule {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "mod {} {{", self.name)?;

        let mut prev_item: Option<&ModuleItem> = None;

        for (i, item) in self.items.iter().enumerate() {
            if let Some(prev_item) = prev_item {
                if !(matches!(prev_item, ModuleItem::Use(_)) && matches!(item, ModuleItem::Use(_))
                    || matches!(prev_item, ModuleItem::Constant(_))
                        && matches!(item, ModuleItem::Constant(_))
                    || matches!(prev_item, ModuleItem::TypeDefinition(_))
                        && matches!(item, ModuleItem::TypeDefinition(_)))
                {
                    writeln!(f)?;
                }
            } else if i > 0 {
                writeln!(f)?;
            }

            item.tabbed_fmt(depth + 1, f)?;
            writeln!(f)?;

            prev_item = Some(item);
        }

        write!(f, "}}")
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub enum ModuleItem {
    Use(Use),
    TypeDefinition(TypeDefinition),
    Constant(Constant),
    Struct(Struct),
    Enum(Enum),
    Abi(Abi),
    Trait(Trait),
    Storage(Storage),
    Configurable(Configurable),
    Function(Function),
    Impl(Impl),
    Submodule(Submodule),
}

impl TabbedDisplay for ModuleItem {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModuleItem::Use(x) => x.tabbed_fmt(depth, f),
            ModuleItem::TypeDefinition(x) => x.tabbed_fmt(depth, f),
            ModuleItem::Constant(x) => x.tabbed_fmt(depth, f),
            ModuleItem::Struct(x) => x.tabbed_fmt(depth, f),
            ModuleItem::Enum(x) => x.tabbed_fmt(depth, f),
            ModuleItem::Abi(x) => x.tabbed_fmt(depth, f),
            ModuleItem::Trait(x) => x.tabbed_fmt(depth, f),
            ModuleItem::Storage(x) => x.tabbed_fmt(depth, f),
            ModuleItem::Configurable(x) => x.tabbed_fmt(depth, f),
            ModuleItem::Function(x) => x.tabbed_fmt(depth, f),
            ModuleItem::Impl(x) => x.tabbed_fmt(depth, f),
            ModuleItem::Submodule(x) => x.tabbed_fmt(depth, f),
        }
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct Use {
    pub is_public: bool,
    pub tree: UseTree,
}

impl Display for Use {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_public {
            write!(f, "pub ")?;
        }

        write!(f, "use {};", self.tree)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub enum UseTree {
    Path {
        prefix: String,
        suffix: Box<UseTree>,
    },
    Group {
        imports: Vec<UseTree>,
    },
    Name {
        name: String,
    },
    Rename {
        name: String,
        alias: String,
    },
    Glob,
}

impl Display for UseTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UseTree::Path { prefix, suffix } => write!(f, "{prefix}::{suffix}"),
            UseTree::Group { imports } => write!(
                f,
                "{{{}}}",
                imports
                    .iter()
                    .map(|x| format!("{x}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            UseTree::Name { name } => write!(f, "{name}"),
            UseTree::Rename { name, alias } => write!(f, "{name} as {alias}"),
            UseTree::Glob => write!(f, "*"),
        }
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct GenericParameter {
    pub type_name: TypeName,
    pub implements: Option<Vec<TypeName>>,
}

impl Display for GenericParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_name)?;

        if let Some(implements) = self.implements.as_ref() {
            write!(
                f,
                ": {}",
                implements
                    .iter()
                    .map(|x| format!("{x}"))
                    .collect::<Vec<_>>()
                    .join(" + ")
            )?;
        }

        Ok(())
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, Default, PartialEq)]
pub struct GenericParameterList {
    pub entries: Vec<GenericParameter>,
}

impl Display for GenericParameterList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<{}>",
            self.entries
                .iter()
                .map(|x| format!("{x}"))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct Attribute {
    pub name: String,
    pub parameters: Option<Vec<String>>,
}

impl Display for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;

        if let Some(parameters) = self.parameters.as_ref() {
            write!(f, "({})", parameters.join(", "))?;
        }

        Ok(())
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, Default, PartialEq)]
pub struct AttributeList {
    pub attributes: Vec<Attribute>,
}

impl Display for AttributeList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "#[{}]",
            self.attributes
                .iter()
                .map(|a| format!("{a}"))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, Default, PartialEq)]
pub enum TypeName {
    #[default]
    Undefined,

    Identifier {
        name: String,
        generic_parameters: Option<GenericParameterList>,
    },
    Array {
        type_name: Box<TypeName>,
        length: usize,
    },
    Tuple {
        type_names: Vec<TypeName>,
    },
    StringSlice,
    StringArray {
        length: usize,
    },
    Function {
        generic_parameters: Option<GenericParameterList>,
        parameters: ParameterList,
        return_type: Option<Box<TypeName>>,
    },
}

impl Display for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeName::Undefined => panic!("Undefined type name"),
            TypeName::Identifier {
                name,
                generic_parameters,
            } => write!(
                f,
                "{name}{}",
                if let Some(p) = generic_parameters.as_ref() {
                    format!("{p}")
                } else {
                    String::new()
                }
            ),
            TypeName::Array { type_name, length } => write!(f, "[{type_name}; {length}]"),
            TypeName::Tuple { type_names } => write!(
                f,
                "({})",
                type_names
                    .iter()
                    .map(|t| format!("{t}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            TypeName::StringSlice => write!(f, "str"),
            TypeName::StringArray { length } => write!(f, "str[{length}]"),
            TypeName::Function {
                generic_parameters,
                parameters,
                return_type,
            } => {
                write!(
                    f,
                    "fn{}{}{}",
                    match generic_parameters.as_ref() {
                        Some(g) => g.to_string(),
                        None => String::new(),
                    },
                    parameters,
                    match return_type.as_ref() {
                        Some(t) => format!(" -> {t}"),
                        None => String::new(),
                    },
                )
            }
        }
    }
}

impl TypeName {
    /// Checks if the type name is an unsigned integer type
    pub fn is_uint(&self) -> bool {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => {
                matches!(
                    name.as_str(),
                    "u8" | "u16" | "u32" | "u64" | "u256" | "U128" | "U256"
                )
            }

            _ => false,
        }
    }

    pub fn uint_bits(&self) -> Option<usize> {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => match name.as_str() {
                "u8" | "u16" | "u32" | "u64" | "u256" | "U128" | "U256" => Some(
                    name.trim_start_matches("u")
                        .trim_start_matches("U")
                        .parse()
                        .unwrap(),
                ),

                _ => None,
            },

            _ => None,
        }
    }

    /// Checks if the type name is a signed integer type
    pub fn is_int(&self) -> bool {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => {
                matches!(
                    name.as_str(),
                    "I8" | "I16" | "I32" | "I64" | "I128" | "I256"
                )
            }

            _ => false,
        }
    }

    pub fn int_bits(&self) -> Option<usize> {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => match name.as_str() {
                "I8" | "I16" | "I32" | "I64" | "I128" | "I256" => {
                    Some(name.trim_start_matches("I").parse().unwrap())
                }

                _ => None,
            },

            _ => None,
        }
    }

    pub fn is_b256(&self) -> bool {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => name == "b256",
            _ => false,
        }
    }

    pub fn is_u64(&self) -> bool {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => name == "u64",
            _ => false,
        }
    }

    /// Checks if the type name is `Identity`
    pub fn is_identity(&self) -> bool {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => name == "Identity",
            _ => false,
        }
    }

    pub fn is_string_slice(&self) -> bool {
        matches!(self, TypeName::StringSlice)
    }

    pub fn is_string(&self) -> bool {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => name == "String",
            TypeName::StringSlice => true,
            TypeName::StringArray { .. } => true,
            _ => false,
        }
    }

    pub fn u8_array_length(&self) -> Option<usize> {
        match self {
            TypeName::Array { type_name, length } => match type_name.as_ref() {
                TypeName::Identifier {
                    name,
                    generic_parameters: None,
                } if name == "u8" => Some(*length),
                _ => None,
            },

            _ => None,
        }
    }

    pub fn array_info(&self) -> Option<(TypeName, usize)> {
        match self {
            TypeName::Array { type_name, length } => Some((type_name.as_ref().clone(), *length)),
            _ => None,
        }
    }

    #[inline]
    pub fn is_array(&self) -> bool {
        self.array_info().is_some()
    }

    #[inline]
    pub fn is_u8_array(&self) -> bool {
        self.u8_array_length().is_some()
    }

    pub fn option_type(&self) -> Option<TypeName> {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: Some(generic_parameters),
            } => {
                if name == "Option" && generic_parameters.entries.len() == 1 {
                    Some(generic_parameters.entries[0].type_name.clone())
                } else {
                    None
                }
            }

            _ => None,
        }
    }

    #[inline]
    pub fn is_option(&self) -> bool {
        self.option_type().is_some()
    }

    pub fn storage_key_type(&self) -> Option<TypeName> {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: Some(generic_parameters),
            } => {
                if name == "StorageKey" && generic_parameters.entries.len() == 1 {
                    Some(generic_parameters.entries[0].type_name.clone())
                } else {
                    None
                }
            }

            _ => None,
        }
    }

    #[inline]
    pub fn is_storage_key(&self) -> bool {
        self.storage_key_type().is_some()
    }

    pub fn storage_map_type(&self) -> Option<(TypeName, TypeName)> {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: Some(generic_parameters),
            } => {
                if name == "StorageMap" && generic_parameters.entries.len() == 2 {
                    Some((
                        generic_parameters.entries[0].type_name.clone(),
                        generic_parameters.entries[1].type_name.clone(),
                    ))
                } else {
                    None
                }
            }

            _ => None,
        }
    }

    #[inline]
    pub fn is_storage_vec(&self) -> bool {
        self.storage_vec_type().is_some()
    }

    pub fn storage_vec_type(&self) -> Option<TypeName> {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: Some(generic_parameters),
            } => {
                if name == "StorageVec" && generic_parameters.entries.len() == 1 {
                    Some(generic_parameters.entries[0].type_name.clone())
                } else {
                    None
                }
            }

            _ => None,
        }
    }

    pub fn is_storage_string(&self) -> bool {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => name == "StorageString",
            _ => false,
        }
    }

    #[inline]
    pub fn is_vec(&self) -> bool {
        self.vec_type().is_some()
    }

    pub fn vec_type(&self) -> Option<TypeName> {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: Some(generic_parameters),
            } => {
                if name == "Vec" && generic_parameters.entries.len() == 1 {
                    Some(generic_parameters.entries[0].type_name.clone())
                } else {
                    None
                }
            }

            _ => None,
        }
    }

    #[inline]
    pub fn is_storage_map(&self) -> bool {
        self.storage_map_type().is_some()
    }

    /// Checks to see if the type name is compatible with another type name
    pub fn is_compatible_with(&self, other: &TypeName) -> bool {
        match self {
            TypeName::Array {
                type_name: lhs_type_name,
                length: lhs_length,
            } => match other {
                TypeName::Array {
                    type_name: rhs_type_name,
                    length: rhs_length,
                } => {
                    if !lhs_type_name.is_compatible_with(rhs_type_name) {
                        return false;
                    }

                    if *lhs_length == *rhs_length {
                        return true;
                    }
                }

                _ => {}
            },

            _ => {}
        }

        // HACK: Don't check todo! value types
        if let TypeName::Identifier {
            name,
            generic_parameters: None,
        } = self
        {
            if name == "todo!" {
                return true;
            }
        }

        // HACK: Don't check parameter names of function types
        if let (
            TypeName::Function {
                parameters: lhs_parameters,
                return_type: lhs_return_type,
                ..
            },
            TypeName::Function {
                parameters: rhs_parameters,
                return_type: rhs_return_type,
                ..
            },
        ) = (self, other)
        {
            if lhs_parameters.entries.len() == rhs_parameters.entries.len() {
                if lhs_parameters
                    .entries
                    .iter()
                    .zip(rhs_parameters.entries.iter())
                    .all(|(lhs, rhs)| {
                        lhs.type_name.is_some() == rhs.type_name.is_some()
                            && lhs.type_name.as_ref().map_or(true, |x| {
                                x.is_compatible_with(rhs.type_name.as_ref().unwrap())
                            })
                    })
                {
                    match (lhs_return_type.as_ref(), rhs_return_type.as_ref()) {
                        (Some(lhs_return_type), Some(rhs_return_type)) => {
                            if lhs_return_type.is_compatible_with(rhs_return_type) {
                                return true;
                            }
                        }

                        (None, None) => {
                            return true;
                        }

                        _ => {}
                    }
                }
            }
        }

        self == other
    }

    /// Gets the parameters and return type name for the getter function of the type name
    pub fn getter_function_parameters_and_return_type(
        &self,
    ) -> Option<(Vec<(Parameter, bool)>, TypeName)> {
        match self {
            TypeName::Undefined => panic!("Undefined type name"),

            TypeName::Identifier {
                name,
                generic_parameters: Some(generic_parameters),
            } => match name.as_str() {
                "StorageMap" => {
                    let mut parameters = vec![(
                        Parameter {
                            name: "_".into(),
                            type_name: Some(generic_parameters.entries[0].type_name.clone()),
                            ..Default::default()
                        },
                        false,
                    )];

                    let mut return_type = generic_parameters.entries[1].type_name.clone();

                    if let Some((inner_parameters, inner_return_type)) = generic_parameters.entries
                        [1]
                    .type_name
                    .getter_function_parameters_and_return_type()
                    {
                        parameters.extend(inner_parameters);
                        return_type = inner_return_type;
                    }

                    let parameter_names: Vec<String> = ('a'..='z')
                        .enumerate()
                        .take_while(|(i, _)| *i < parameters.len())
                        .map(|(_, c)| c.into())
                        .collect();

                    for (i, name) in parameter_names.into_iter().enumerate() {
                        parameters[i].0.name = name;
                    }

                    Some((parameters, return_type))
                }

                "StorageVec" => {
                    let mut parameters = vec![(
                        Parameter {
                            name: "_".into(),
                            type_name: Some(TypeName::Identifier {
                                name: "u64".into(),
                                generic_parameters: None,
                            }),
                            ..Default::default()
                        },
                        true,
                    )];

                    let mut return_type = generic_parameters.entries[0].type_name.clone();

                    if let Some((inner_parameters, inner_return_type)) = generic_parameters.entries
                        [0]
                    .type_name
                    .getter_function_parameters_and_return_type()
                    {
                        parameters.extend(inner_parameters);
                        return_type = inner_return_type;
                    }

                    let parameter_names: Vec<String> = ('a'..='z')
                        .enumerate()
                        .take_while(|(i, _)| *i < parameters.len())
                        .map(|(_, c)| c.into())
                        .collect();

                    for (i, name) in parameter_names.into_iter().enumerate() {
                        parameters[i].0.name = name;
                    }

                    Some((parameters, return_type))
                }

                _ => None,
            },

            _ => None,
        }
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct TypeDefinition {
    pub is_public: bool,
    pub name: TypeName,
    pub underlying_type: Option<TypeName>,
}

impl Display for TypeDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_public {
            write!(f, "pub ")?;
        }

        write!(f, "type {}", self.name)?;

        if let Some(underlying_type) = self.underlying_type.as_ref() {
            write!(f, " = {underlying_type}")?;
        }

        write!(f, ";")
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct Constant {
    pub is_public: bool,
    pub name: String,
    pub type_name: TypeName,
    pub value: Option<Expression>,
}

impl TabbedDisplay for Constant {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_public {
            write!(f, "pub ")?;
        }

        write!(f, "const {}: {}", self.name, self.type_name)?;

        if let Some(value) = self.value.as_ref() {
            write!(f, " = ")?;
            value.tabbed_fmt(depth, f)?;
        }

        write!(f, ";")
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Bool(bool),
    DecInt(BigUint, Option<String>),
    HexInt(BigUint, Option<String>),
    String(String),
}

impl TabbedDisplay for Literal {
    fn tabbed_fmt(&self, _depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Bool(x) => write!(f, "{x}"),

            Literal::DecInt(x, suffix) => write!(
                f,
                "{}{}",
                if suffix.as_ref().is_some_and(|s| s == "u256" || s == "b256") {
                    format!("0x{x:064X}")
                } else {
                    format!("{x}")
                },
                if let Some(suffix) = suffix.as_ref() {
                    suffix.as_str()
                } else {
                    ""
                },
            ),

            Literal::HexInt(x, suffix) => write!(
                f,
                "0x{x:X}{}",
                if let Some(suffix) = suffix.as_ref() {
                    suffix.as_str()
                } else {
                    ""
                },
            ),

            Literal::String(x) => write!(f, "\"{x}\""),
        }
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
    pub attributes: Option<AttributeList>,
    pub is_public: bool,
    pub name: String,
    pub generic_parameters: Option<GenericParameterList>,
    pub fields: Vec<StructField>,
}

impl TabbedDisplay for Struct {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(attributes) = self.attributes.as_ref() {
            writeln!(f, "{attributes}")?;
            "".tabbed_fmt(depth, f)?;
        }

        if self.is_public {
            write!(f, "pub ")?;
        }

        writeln!(
            f,
            "struct {}{} {{",
            self.name,
            if let Some(p) = self.generic_parameters.as_ref() {
                format!("{p}")
            } else {
                String::new()
            },
        )?;

        for field in self.fields.iter() {
            field.tabbed_fmt(depth + 1, f)?;
            writeln!(f, ",")?;
        }

        "}".tabbed_fmt(depth, f)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct StructField {
    pub is_public: bool,
    pub name: String,
    pub type_name: TypeName,
}

impl Display for StructField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_public {
            write!(f, "pub ")?;
        }

        write!(f, "{}: {}", self.name, self.type_name)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Enum {
    pub attributes: Option<AttributeList>,
    pub is_public: bool,
    pub name: String,
    pub generic_parameters: Option<GenericParameterList>,
    pub variants: Vec<EnumVariant>,
}

impl TabbedDisplay for Enum {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(attributes) = self.attributes.as_ref() {
            writeln!(f, "{attributes}")?;
            "".tabbed_fmt(depth, f)?;
        }

        if self.is_public {
            write!(f, "pub ")?;
        }

        writeln!(
            f,
            "enum {}{} {{",
            self.name,
            if let Some(p) = self.generic_parameters.as_ref() {
                format!("{p}")
            } else {
                String::new()
            },
        )?;

        for field in self.variants.iter() {
            field.tabbed_fmt(depth + 1, f)?;
            writeln!(f, ",")?;
        }

        "}".tabbed_fmt(depth, f)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct EnumVariant {
    pub name: String,
    pub type_name: TypeName,
}

impl Display for EnumVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.type_name)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct Abi {
    pub name: String,
    pub inherits: Vec<TypeName>,
    pub functions: Vec<Function>,
}

impl TabbedDisplay for Abi {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "abi {}", self.name)?;

        if !self.inherits.is_empty() {
            write!(f, ": {}", self.inherits.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" + "))?;
        }

        writeln!(f, " {{")?;

        for (i, function) in self.functions.iter().enumerate() {
            if i > 0 {
                writeln!(f)?;
            }

            "".tabbed_fmt(depth + 1, f)?;
            function.tabbed_fmt(depth + 1, f)?;
            writeln!(f)?;
        }

        "}".tabbed_fmt(depth, f)
    }
}

impl Abi {
    /// Atempts to find a function using a custom function
    #[inline]
    pub fn find_function<F: Copy + FnMut(&&Function) -> bool>(&self, f: F) -> Option<&Function> {
        self.functions.iter().find(f)
    }
}
// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct Trait {
    pub attributes: Option<AttributeList>,
    pub is_public: bool,
    pub name: String,
    pub generic_parameters: Option<GenericParameterList>,
    pub items: Vec<TraitItem>,
}

impl TabbedDisplay for Trait {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(attributes) = self.attributes.as_ref() {
            writeln!(f, "{attributes}")?;
            "".tabbed_fmt(depth, f)?;
        }

        if self.is_public {
            write!(f, "pub ")?;
        }

        writeln!(
            f,
            "trait {}{} {{",
            self.name,
            if let Some(p) = self.generic_parameters.as_ref() {
                format!("{p}")
            } else {
                String::new()
            },
        )?;

        for item in self.items.iter() {
            "".tabbed_fmt(depth + 1, f)?;
            item.tabbed_fmt(depth + 1, f)?;
        }

        "}".tabbed_fmt(depth, f)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub enum TraitItem {
    Constant(Constant),
    TypeName(GenericParameter),
    Function(Function),
}

impl TabbedDisplay for TraitItem {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TraitItem::Constant(x) => x.tabbed_fmt(depth, f),
            TraitItem::TypeName(x) => x.tabbed_fmt(depth, f),
            TraitItem::Function(x) => x.tabbed_fmt(depth, f),
        }
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Storage {
    pub fields: Vec<StorageField>,
    pub namespaces: Vec<StorageNamespace>,
}

impl TabbedDisplay for Storage {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "storage {{")?;

        for field in self.fields.iter() {
            "".tabbed_fmt(depth + 1, f)?;
            field.tabbed_fmt(depth + 1, f)?;
            writeln!(f, ",")?;
        }

        for namespace in self.namespaces.iter() {
            "".tabbed_fmt(depth + 1, f)?;
            namespace.tabbed_fmt(depth + 1, f)?;
            writeln!(f, ",")?;
        }

        "}".tabbed_fmt(depth, f)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, Default, PartialEq)]
pub struct StorageNamespace {
    pub name: String,
    pub fields: Vec<StorageField>,
    pub namespaces: Vec<StorageNamespace>,
}

impl TabbedDisplay for StorageNamespace {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} {{", self.name)?;

        for field in self.fields.iter() {
            "".tabbed_fmt(depth + 1, f)?;
            field.tabbed_fmt(depth + 1, f)?;
            writeln!(f, ",")?;
        }

        for namespace in self.namespaces.iter() {
            "".tabbed_fmt(depth + 1, f)?;
            namespace.tabbed_fmt(depth + 1, f)?;
            writeln!(f, ",")?;
        }

        "}".tabbed_fmt(depth, f)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct StorageField {
    pub name: String,
    pub type_name: TypeName,
    pub value: Expression,
}

impl TabbedDisplay for StorageField {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {} = ", self.name, self.type_name)?;
        self.value.tabbed_fmt(depth, f)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Configurable {
    pub fields: Vec<ConfigurableField>,
}

impl TabbedDisplay for Configurable {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "configurable {{")?;

        for field in self.fields.iter() {
            "".tabbed_fmt(depth + 1, f)?;
            field.tabbed_fmt(depth + 1, f)?;
            writeln!(f, ",")?;
        }

        "}".tabbed_fmt(depth, f)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct ConfigurableField {
    pub name: String,
    pub type_name: TypeName,
    pub value: Expression,
}

impl TabbedDisplay for ConfigurableField {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {} = ", self.name, self.type_name)?;
        self.value.tabbed_fmt(depth, f)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub attributes: Option<AttributeList>,
    pub is_public: bool,
    pub old_name: String,
    pub name: String,
    pub generic_parameters: Option<GenericParameterList>,
    pub parameters: ParameterList,
    pub return_type: Option<TypeName>,
    pub body: Option<Block>,
}

impl TabbedDisplay for Function {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(attributes) = self.attributes.as_ref() {
            writeln!(f, "{attributes}")?;
            "".tabbed_fmt(depth, f)?;
        }

        if self.is_public {
            write!(f, "pub ")?;
        }

        write!(
            f,
            "fn {}{}{}",
            self.name,
            if let Some(p) = self.generic_parameters.as_ref() {
                format!("{p}")
            } else {
                String::new()
            },
            self.parameters,
        )?;

        if let Some(return_type) = self.return_type.as_ref() {
            write!(f, " -> {return_type}")?;
        }

        if let Some(body) = self.body.as_ref() {
            write!(f, " ")?;
            body.tabbed_fmt(depth, f)?;
        } else {
            write!(f, ";")?;
        }

        Ok(())
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Parameter {
    pub is_ref: bool,
    pub is_mut: bool,
    pub name: String,
    pub type_name: Option<TypeName>,
}

impl Display for Parameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_ref {
            write!(f, "ref ")?;
        }

        if self.is_mut {
            write!(f, "mut ")?;
        }

        write!(f, "{}", self.name)?;

        if let Some(type_name) = self.type_name.as_ref() {
            write!(f, ": {type_name}")?;
        }

        Ok(())
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, Default, PartialEq)]
pub struct ParameterList {
    pub entries: Vec<Parameter>,
}

impl Display for ParameterList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({})",
            self.entries
                .iter()
                .map(|x| format!("{x}"))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Impl {
    pub generic_parameters: Option<GenericParameterList>,
    pub type_name: TypeName,
    pub for_type_name: Option<TypeName>,
    pub items: Vec<ImplItem>,
}

impl TabbedDisplay for Impl {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "impl{} {}",
            if let Some(p) = self.generic_parameters.as_ref() {
                format!("{p}")
            } else {
                String::new()
            },
            self.type_name,
        )?;

        if let Some(for_type_name) = self.for_type_name.as_ref() {
            write!(f, " for {for_type_name}")?;
        }

        writeln!(f, " {{")?;

        let mut was_constant = false;

        for (i, item) in self.items.iter().enumerate() {
            if i > 0 && !(was_constant && matches!(item, ImplItem::Constant(_))) {
                writeln!(f)?;
            }

            "".tabbed_fmt(depth + 1, f)?;
            item.tabbed_fmt(depth + 1, f)?;
            writeln!(f)?;

            was_constant = matches!(item, ImplItem::Constant(_));
        }

        "}".tabbed_fmt(depth, f)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub enum ImplItem {
    Constant(Constant),
    TypeDefinition(TypeDefinition),
    Function(Function),
}

impl TabbedDisplay for ImplItem {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ImplItem::Constant(x) => x.tabbed_fmt(depth, f),
            ImplItem::TypeDefinition(x) => x.tabbed_fmt(depth, f),
            ImplItem::Function(x) => x.tabbed_fmt(depth, f),
        }
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub final_expr: Option<Expression>,
}

impl TabbedDisplay for Block {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{{")?;

        for statement in self.statements.iter() {
            "".tabbed_fmt(depth + 1, f)?;
            statement.tabbed_fmt(depth + 1, f)?;
            writeln!(f)?;
        }

        if let Some(final_expr) = self.final_expr.as_ref() {
            "".tabbed_fmt(depth + 1, f)?;
            final_expr.tabbed_fmt(depth + 1, f)?;
            writeln!(f)?;
        }

        "}".tabbed_fmt(depth, f)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Let(Let),
    Expression(Expression),
    Commented(String, Option<Box<Statement>>), // TODO: finish
}

impl TabbedDisplay for Statement {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let(x) => {
                x.tabbed_fmt(depth, f)?;
                write!(f, ";")
            }

            Statement::Expression(x) => {
                x.tabbed_fmt(depth, f)?;

                if !matches!(
                    x,
                    Expression::Block(_)
                        | Expression::If(_)
                        | Expression::Match(_)
                        | Expression::While(_)
                        | Expression::AsmBlock(_)
                ) {
                    write!(f, ";")?;
                }

                Ok(())
            }
            Statement::Commented(comment, statement) => match statement.as_ref() {
                Some(statement) => {
                    writeln!(f, "/* {comment} */")?;
                    statement.tabbed_fmt(depth, f)
                }
                None => write!(f, "/* {comment} */"),
            },
        }
    }
}

macro_rules! impl_stmt_from {
    ($t: ident) => {
        impl From<$t> for Statement {
            fn from(x: $t) -> Self {
                Self::$t(x)
            }
        }
    };
}

impl_stmt_from!(Let);
impl_stmt_from!(Expression);

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct Let {
    pub pattern: LetPattern,
    pub type_name: Option<TypeName>,
    pub value: Expression,
}

impl TabbedDisplay for Let {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {}", self.pattern)?;

        if let Some(type_name) = self.type_name.as_ref() {
            write!(f, ": {type_name}")?;
        }

        write!(f, " = ")?;
        self.value.tabbed_fmt(depth, f)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub enum LetPattern {
    Identifier(LetIdentifier),
    Tuple(Vec<LetIdentifier>),
}

impl Display for LetPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LetPattern::Identifier(id) => write!(f, "{id}"),
            LetPattern::Tuple(ids) => write!(
                f,
                "({})",
                ids.iter()
                    .map(|id| format!("{id}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl From<LetIdentifier> for LetPattern {
    fn from(value: LetIdentifier) -> Self {
        LetPattern::Identifier(value)
    }
}

impl From<Vec<LetIdentifier>> for LetPattern {
    fn from(value: Vec<LetIdentifier>) -> Self {
        LetPattern::Tuple(value)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct LetIdentifier {
    pub is_mutable: bool,
    pub name: String,
}

impl Display for LetIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_mutable {
            write!(f, "mut ")?;
        }

        write!(f, "{}", self.name)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub enum PathExprRoot {
    Identifier(String),
    Cast {
        from_type: TypeName,
        to_type: TypeName,
    },
}

impl Display for PathExprRoot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PathExprRoot::Identifier(name) => write!(f, "{name}"),
            PathExprRoot::Cast { from_type, to_type } => write!(f, "<{from_type} as {to_type}>"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PathExprSegment {
    pub name: String,
    pub generic_parameters: Option<GenericParameterList>,
}

impl Display for PathExprSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;

        if let Some(generic_parameters) = self.generic_parameters.as_ref() {
            write!(f, "::{generic_parameters}")?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PathExpr {
    pub root: PathExprRoot,
    pub segments: Vec<PathExprSegment>,
}

impl Display for PathExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.root)?;

        for segment in self.segments.iter() {
            write!(f, "::{segment}")?;
        }

        Ok(())
    }
}

impl PathExpr {
    #[inline(always)]
    pub fn as_identifier(&self) -> Option<&str> {
        let PathExprRoot::Identifier(name) = &self.root else {
            return None;
        };

        if !self.segments.is_empty() {
            return None;
        }

        Some(name.as_str())
    }

    #[inline(always)]
    pub fn is_identifier(&self) -> bool {
        self.as_identifier().is_some()
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Literal(Literal),
    PathExpr(PathExpr),
    FunctionCall(Box<FunctionCall>),
    FunctionCallBlock(Box<FunctionCallBlock>),
    Block(Box<Block>),
    Return(Option<Box<Expression>>),
    Array(Array),
    ArrayAccess(Box<ArrayAccess>),
    MemberAccess(Box<MemberAccess>),
    Tuple(Vec<Expression>),
    If(Box<If>),
    Match(Box<Match>),
    While(Box<While>),
    UnaryExpression(Box<UnaryExpression>),
    BinaryExpression(Box<BinaryExpression>),
    Constructor(Box<Constructor>),
    Continue,
    Break,
    AsmBlock(Box<AsmBlock>),
    Commented(String, Box<Expression>),
    // TODO: finish
}

impl TabbedDisplay for Expression {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Literal(x) => x.tabbed_fmt(depth, f),
            Expression::PathExpr(x) => write!(f, "{x}"),
            Expression::FunctionCall(x) => x.tabbed_fmt(depth, f),
            Expression::FunctionCallBlock(x) => x.tabbed_fmt(depth, f),
            Expression::Block(x) => x.tabbed_fmt(depth, f),
            Expression::Return(x) => {
                write!(f, "return")?;
                if let Some(x) = x.as_ref() {
                    write!(f, " ")?;
                    x.tabbed_fmt(depth, f)?;
                }
                Ok(())
            }
            Expression::Array(x) => x.tabbed_fmt(depth, f),
            Expression::ArrayAccess(x) => x.tabbed_fmt(depth, f),
            Expression::MemberAccess(x) => x.tabbed_fmt(depth, f),
            Expression::If(x) => x.tabbed_fmt(depth, f),
            Expression::Match(x) => x.tabbed_fmt(depth, f),
            Expression::While(x) => x.tabbed_fmt(depth, f),
            Expression::Tuple(x) => {
                write!(f, "(")?;
                for (i, expr) in x.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    expr.tabbed_fmt(depth, f)?;
                }
                write!(f, ")")
            }
            Expression::UnaryExpression(x) => x.tabbed_fmt(depth, f),
            Expression::BinaryExpression(x) => x.tabbed_fmt(depth, f),
            Expression::Constructor(x) => x.tabbed_fmt(depth, f),
            Expression::Continue => write!(f, "continue"),
            Expression::Break => write!(f, "break"),
            Expression::AsmBlock(x) => x.tabbed_fmt(depth, f),
            Expression::Commented(comment, x) => {
                write!(f, "/*{comment}*/ ")?;
                x.tabbed_fmt(depth, f)
            }
        }
    }
}

macro_rules! impl_expr_from {
    ($t: ident) => {
        impl From<$t> for Expression {
            fn from(x: $t) -> Self {
                Self::$t(x)
            }
        }
    };
}

macro_rules! impl_expr_box_from {
    ($t: ident) => {
        impl From<$t> for Expression {
            fn from(x: $t) -> Self {
                Self::$t(Box::new(x))
            }
        }
    };
}

impl_expr_from!(Literal);
impl_expr_from!(PathExpr);
impl_expr_box_from!(FunctionCall);
impl_expr_box_from!(FunctionCallBlock);
impl_expr_box_from!(Block);
impl_expr_from!(Array);
impl_expr_box_from!(ArrayAccess);
impl_expr_box_from!(MemberAccess);
impl_expr_box_from!(If);
impl_expr_box_from!(Match);
impl_expr_box_from!(While);
impl_expr_box_from!(UnaryExpression);
impl_expr_box_from!(BinaryExpression);
impl_expr_box_from!(Constructor);
impl_expr_box_from!(AsmBlock);

impl Expression {
    #[inline(always)]
    pub fn create_todo(msg: Option<String>) -> Expression {
        Expression::create_function_calls(
            None,
            &[(
                "todo!",
                Some((
                    None,
                    if let Some(msg) = msg {
                        vec![Expression::Literal(Literal::String(
                            msg.replace('\\', "\\\\").replace('\"', "\\\""),
                        ))]
                    } else {
                        vec![]
                    },
                )),
            )],
        )
    }

    #[inline(always)]
    pub fn create_unimplemented(msg: Option<String>) -> Expression {
        Expression::create_function_calls(
            None,
            &[(
                "unimplemented!",
                Some((
                    None,
                    if let Some(msg) = msg {
                        vec![Expression::Literal(Literal::String(msg))]
                    } else {
                        vec![]
                    },
                )),
            )],
        )
    }

    #[inline(always)]
    pub fn create_identifier(name: String) -> Expression {
        assert!(!name.is_empty());

        Expression::PathExpr(PathExpr {
            root: PathExprRoot::Identifier(name),
            segments: vec![],
        })
    }

    #[inline(always)]
    pub fn as_identifier(&self) -> Option<&str> {
        let Expression::PathExpr(path_expr) = self else {
            return None;
        };

        path_expr.as_identifier()
    }

    #[inline(always)]
    pub fn is_identifier(&self) -> bool {
        self.as_identifier().is_some()
    }

    pub fn create_member_access(container: Expression, members: &[&str]) -> Expression {
        assert!(!members.is_empty());

        let mut result = container;

        for member in members {
            result = Expression::from(MemberAccess {
                expression: result,
                member: member.to_string(),
            });
        }

        result
    }

    pub fn create_function_calls(
        container: Option<Expression>,
        member_calls: &[(
            &str,
            Option<(Option<GenericParameterList>, Vec<Expression>)>,
        )],
    ) -> Expression {
        if container.is_none() {
            assert!(!member_calls.is_empty());
        }

        let mut result = container;

        for (member, call_data) in member_calls {
            if member.contains("<") {
                todo!("Handle path strings with generic parameters");
            }

            let mut member_parts = member.split("::").collect::<Vec<_>>();

            assert!(member.len() >= 1);

            let root = PathExprRoot::Identifier(member_parts[0].to_string());
            member_parts.remove(0);

            let path = PathExpr {
                root,
                segments: member_parts
                    .iter()
                    .map(|p| PathExprSegment {
                        name: p.to_string(),
                        generic_parameters: None,
                    })
                    .collect(),
            };

            if let Some(container) = result {
                assert!(member_parts.is_empty());
                result = Some(Expression::from(MemberAccess {
                    expression: container,
                    member: member.to_string(),
                }));
            } else {
                result = Some(Expression::from(path));
            }

            if let Some((generic_parameters, parameters)) = call_data {
                result = Some(Expression::from(FunctionCall {
                    function: result.unwrap(),
                    generic_parameters: generic_parameters.clone(),
                    parameters: parameters.clone(),
                }));
            }
        }

        result.unwrap()
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCall {
    pub function: Expression,
    pub generic_parameters: Option<GenericParameterList>,
    pub parameters: Vec<Expression>,
}

impl TabbedDisplay for FunctionCall {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.function.tabbed_fmt(depth, f)?;

        if let Some(generic_parameters) = self.generic_parameters.as_ref() {
            write!(f, "::{generic_parameters}")?;
        }

        write!(f, "(")?;

        for (i, parameter) in self.parameters.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }

            parameter.tabbed_fmt(depth, f)?;
        }

        write!(f, ")")
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCallBlock {
    pub function: Expression,
    pub generic_parameters: Option<GenericParameterList>,
    pub fields: Vec<ConstructorField>,
    pub parameters: Vec<Expression>,
}

impl TabbedDisplay for FunctionCallBlock {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.function.tabbed_fmt(depth, f)?;

        if let Some(generic_parameters) = self.generic_parameters.as_ref() {
            write!(f, "::{generic_parameters}")?;
        }

        writeln!(f, " {{")?;

        for field in self.fields.iter() {
            "".tabbed_fmt(depth + 1, f)?;
            field.tabbed_fmt(depth + 1, f)?;
            writeln!(f)?;
        }

        "}".tabbed_fmt(depth, f)?;
        writeln!(f)?;

        "(".tabbed_fmt(depth, f)?;

        for (i, parameter) in self.parameters.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }

            parameter.tabbed_fmt(depth, f)?;
        }

        write!(f, ")")
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Array {
    pub elements: Vec<Expression>,
}

impl TabbedDisplay for Array {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;

        for (i, element) in self.elements.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }

            element.tabbed_fmt(depth, f)?;
        }

        write!(f, "]")
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct ArrayAccess {
    pub expression: Expression,
    pub index: Expression,
}

impl TabbedDisplay for ArrayAccess {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.expression.tabbed_fmt(depth, f)?;
        write!(f, "[")?;
        self.index.tabbed_fmt(depth, f)?;
        write!(f, "]")
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct MemberAccess {
    pub expression: Expression,
    pub member: String,
}

impl TabbedDisplay for MemberAccess {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.expression.tabbed_fmt(depth, f)?;
        write!(f, ".{}", self.member)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct If {
    pub condition: Option<Expression>,
    pub then_body: Block,
    pub else_if: Option<Box<If>>,
}

impl TabbedDisplay for If {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(condition) = self.condition.as_ref() {
            write!(f, "if ")?;
            condition.tabbed_fmt(depth, f)?;
            write!(f, " ")?;
        }

        self.then_body.tabbed_fmt(depth, f)?;

        if let Some(else_if) = self.else_if.as_ref() {
            write!(f, " else ")?;
            else_if.tabbed_fmt(depth, f)?;
        }

        Ok(())
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct Match {
    pub expression: Expression,
    pub branches: Vec<MatchBranch>,
}

impl TabbedDisplay for Match {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "match ")?;
        self.expression.tabbed_fmt(depth, f)?;
        writeln!(f, " {{")?;

        for branch in self.branches.iter() {
            "".tabbed_fmt(depth + 1, f)?;
            branch.tabbed_fmt(depth + 1, f)?;
            writeln!(f)?;
        }

        "}".tabbed_fmt(depth, f)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchBranch {
    pub pattern: Expression,
    pub value: Expression,
}

impl TabbedDisplay for MatchBranch {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pattern.tabbed_fmt(depth, f)?;
        write!(f, " => ")?;
        self.value.tabbed_fmt(depth, f)?;
        write!(f, ",")
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct While {
    pub condition: Expression,
    pub body: Block,
}

impl TabbedDisplay for While {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "while ")?;
        self.condition.tabbed_fmt(depth, f)?;
        write!(f, " ")?;
        self.body.tabbed_fmt(depth, f)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct UnaryExpression {
    pub operator: String,
    pub expression: Expression,
}

impl TabbedDisplay for UnaryExpression {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.operator)?;
        self.expression.tabbed_fmt(depth, f)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct BinaryExpression {
    pub operator: String,
    pub lhs: Expression,
    pub rhs: Expression,
}

impl TabbedDisplay for BinaryExpression {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.lhs.tabbed_fmt(depth, f)?;
        write!(f, " {} ", self.operator)?;
        self.rhs.tabbed_fmt(depth, f)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct Constructor {
    pub type_name: TypeName,
    pub fields: Vec<ConstructorField>,
}

impl TabbedDisplay for Constructor {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {{", self.type_name)?;

        if !self.fields.is_empty() {
            writeln!(f)?;
        }

        for field in self.fields.iter() {
            "".tabbed_fmt(depth + 1, f)?;
            field.tabbed_fmt(depth + 1, f)?;
            writeln!(f)?;
        }
        if self.fields.is_empty() {
            write!(f, "}}")
        } else {
            "}".tabbed_fmt(depth, f)
        }
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct ConstructorField {
    pub name: String,
    pub value: Expression,
}

impl TabbedDisplay for ConstructorField {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: ", self.name)?;
        self.value.tabbed_fmt(depth, f)?;
        write!(f, ",")
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, Default, PartialEq)]
pub struct AsmBlock {
    pub registers: Vec<AsmRegister>,
    pub instructions: Vec<AsmInstruction>,
    pub final_expression: Option<AsmFinalExpression>,
}

impl TabbedDisplay for AsmBlock {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "asm (")?;

        for (i, register) in self.registers.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }

            write!(f, "{register}")?;
        }

        writeln!(f, ") {{")?;

        for instruction in self.instructions.iter() {
            instruction.tabbed_fmt(depth + 1, f)?;
            writeln!(f)?;
        }

        if let Some(final_expression) = self.final_expression.as_ref() {
            final_expression.tabbed_fmt(depth + 1, f)?;
            writeln!(f)?;
        }

        "}".tabbed_fmt(depth, f)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct AsmRegister {
    pub name: String,
    pub value: Option<Expression>,
}

impl Display for AsmRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;

        if let Some(value) = self.value.as_ref() {
            write!(f, ": ")?;
            value.tabbed_fmt(0, f)?;
        }

        Ok(())
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct AsmInstruction {
    pub op_code: String,
    pub args: Vec<String>,
}

impl Display for AsmInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.op_code)?;

        for arg in self.args.iter() {
            write!(f, " {arg}")?;
        }

        write!(f, ";")
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct AsmFinalExpression {
    pub register: String,
    pub type_name: Option<TypeName>,
}

impl Display for AsmFinalExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.register)?;

        if let Some(type_name) = self.type_name.as_ref() {
            write!(f, ": {type_name}")?;
        }

        Ok(())
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        // Create a new contract module
        let mut module = Module {
            kind: ModuleKind::Contract,
            items: vec![],
        };

        // Add a `use` statement:
        // use std::storage::storage_vec::*;
        module.items.push(ModuleItem::Use(Use {
            is_public: false,
            tree: UseTree::Path {
                prefix: "std".into(),
                suffix: Box::new(UseTree::Path {
                    prefix: "storage".into(),
                    suffix: Box::new(UseTree::Path {
                        prefix: "storage_vec".into(),
                        suffix: Box::new(UseTree::Glob),
                    }),
                }),
            },
        }));

        // Add a test function:
        // fn test() {
        //     return;
        // }
        module.items.push(ModuleItem::Function(Function {
            attributes: None,
            is_public: true,
            old_name: "test".into(),
            name: "test".into(),
            generic_parameters: None,
            parameters: ParameterList::default(),
            return_type: None,
            body: Some(Block {
                statements: vec![Statement::Expression(Expression::Return(None))],
                final_expr: None,
            }),
        }));

        // Display the generated contract module
        println!("{}", TabbedDisplayer(&module));
    }
}
