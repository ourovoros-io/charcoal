use num_bigint::BigUint;
use std::{cell::RefCell, fmt::Display, rc::Rc};

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

impl TabbedDisplay for Module {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{};", self.kind)?;
        writeln!(f)?;

        let mut prev_item: Option<&ModuleItem> = None;

        for (i, item) in self.items.iter().enumerate() {
            if let Some(prev_item) = prev_item {
                if !(matches!(prev_item, ModuleItem::Use(_)) && matches!(item, ModuleItem::Use(_))
                    || matches!(prev_item, ModuleItem::Constant(_)) && matches!(item, ModuleItem::Constant(_))
                    || matches!(prev_item, ModuleItem::TypeDefinition(_))
                        && matches!(item, ModuleItem::TypeDefinition(_))
                    || matches!(prev_item, ModuleItem::Submodule(_)) && matches!(item, ModuleItem::Submodule(_)))
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
    pub is_public: bool,
    pub name: String,
}

impl TabbedDisplay for Submodule {
    fn tabbed_fmt(&self, _depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_public {
            write!(f, "pub ")?;
        }

        write!(f, "mod {};", self.name)
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

impl TabbedDisplay for Use {
    fn tabbed_fmt(&self, _depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_public {
            write!(f, "pub ")?;
        }

        write!(f, "use {};", self.tree)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub enum UseTree {
    Path { prefix: String, suffix: Box<UseTree> },
    Group { imports: Vec<UseTree> },
    Name { name: String },
    Rename { name: String, alias: String },
    Glob,
}

impl Display for UseTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UseTree::Path { prefix, suffix } => write!(f, "{prefix}::{suffix}"),
            UseTree::Group { imports } => write!(
                f,
                "{{{}}}",
                imports.iter().map(|x| format!("{x}")).collect::<Vec<_>>().join(", ")
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

impl std::ops::Deref for GenericParameterList {
    type Target = Vec<GenericParameter>;

    fn deref(&self) -> &Self::Target {
        &self.entries
    }
}

impl std::ops::DerefMut for GenericParameterList {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.entries
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
        old_name: String,
        new_name: String,
        generic_parameters: Option<GenericParameterList>,
        parameters: ParameterList,
        storage_struct_parameter: Option<Box<Parameter>>,
        return_type: Option<Box<TypeName>>,
    },
    Abi {
        type_name: Box<TypeName>,
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
                type_names.iter().map(|t| format!("{t}")).collect::<Vec<_>>().join(", ")
            ),
            TypeName::StringSlice => write!(f, "str"),
            TypeName::StringArray { length } => write!(f, "str[{length}]"),
            TypeName::Function {
                generic_parameters,
                parameters,
                return_type,
                ..
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
            TypeName::Abi { .. } => write!(f, "Identity"),
        }
    }
}

impl TypeName {
    #[inline(always)]
    pub fn create_identifier(name: &str) -> Self {
        Self::Identifier {
            name: name.to_string(),
            generic_parameters: None,
        }
    }

    #[inline(always)]
    pub fn create_generic(name: &str, generic_parameters: Vec<TypeName>) -> Self {
        Self::Identifier {
            name: name.to_string(),
            generic_parameters: Some(GenericParameterList {
                entries: generic_parameters
                    .iter()
                    .map(|g| GenericParameter {
                        type_name: g.clone(),
                        implements: None,
                    })
                    .collect(),
            }),
        }
    }

    #[inline(always)]
    pub fn create_array(element_type: TypeName, length: usize) -> Self {
        Self::Array {
            type_name: Box::new(element_type),
            length,
        }
    }

    #[inline(always)]
    pub fn create_tuple(type_names: Vec<TypeName>) -> Self {
        Self::Tuple { type_names }
    }

    #[inline(always)]
    pub fn is_todo(&self) -> bool {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => name == "todo!",

            _ => false,
        }
    }

    #[inline(always)]
    pub fn is_bool(&self) -> bool {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => name == "bool",

            _ => false,
        }
    }

    /// Checks if the type name is an unsigned integer type
    #[inline(always)]
    pub fn is_uint(&self) -> bool {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => {
                matches!(name.as_str(), "u8" | "u16" | "u32" | "u64" | "u256" | "U128" | "U256")
            }

            _ => false,
        }
    }

    #[inline(always)]
    pub fn uint_bits(&self) -> Option<usize> {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => match name.as_str() {
                "u8" | "u16" | "u32" | "u64" | "u256" | "U128" | "U256" => {
                    Some(name.trim_start_matches("u").trim_start_matches("U").parse().unwrap())
                }

                _ => None,
            },

            _ => None,
        }
    }

    /// Checks if the type name is a signed integer type
    #[inline(always)]
    pub fn is_int(&self) -> bool {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => {
                matches!(name.as_str(), "I8" | "I16" | "I32" | "I64" | "I128" | "I256")
            }

            _ => false,
        }
    }

    #[inline(always)]
    pub fn int_bits(&self) -> Option<usize> {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => match name.as_str() {
                "I8" | "I16" | "I32" | "I64" | "I128" | "I256" => Some(name.trim_start_matches("I").parse().unwrap()),

                _ => None,
            },

            _ => None,
        }
    }

    #[inline(always)]
    pub fn is_b256(&self) -> bool {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => name == "b256",
            _ => false,
        }
    }

    #[inline(always)]
    pub fn is_u8(&self) -> bool {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => name == "u8",
            _ => false,
        }
    }

    #[inline(always)]
    pub fn is_u16(&self) -> bool {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => name == "u16",
            _ => false,
        }
    }

    #[inline(always)]
    pub fn is_u32(&self) -> bool {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => name == "u32",
            _ => false,
        }
    }

    #[inline(always)]
    pub fn is_u64(&self) -> bool {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => name == "u64",
            _ => false,
        }
    }

    #[inline(always)]
    pub fn is_u256(&self) -> bool {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => name == "u256",
            _ => false,
        }
    }

    /// Checks if the type name is `Identity`
    #[inline(always)]
    pub fn is_identity(&self) -> bool {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => name == "Identity",

            TypeName::Abi { .. } => true,

            _ => false,
        }
    }

    /// Checks if the type name is `ContractId`
    #[inline(always)]
    pub fn is_contract_id(&self) -> bool {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => name == "ContractId",
            _ => false,
        }
    }

    /// Checks if the type name is `Address`
    #[inline(always)]
    pub fn is_address(&self) -> bool {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => name == "Address",
            _ => false,
        }
    }

    #[inline(always)]
    pub fn is_string_slice(&self) -> bool {
        matches!(self, TypeName::StringSlice)
    }

    #[inline(always)]
    pub fn is_string_array(&self) -> bool {
        matches!(self, TypeName::StringArray { .. })
    }

    #[inline(always)]
    pub fn is_string(&self) -> bool {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => name == "String",

            _ => false,
        }
    }

    #[inline(always)]
    pub fn is_raw_slice(&self) -> bool {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => name == "raw_slice",

            _ => false,
        }
    }

    #[inline(always)]
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

    #[inline(always)]
    pub fn array_info(&self) -> Option<(TypeName, usize)> {
        match self {
            TypeName::Array { type_name, length } => Some((type_name.as_ref().clone(), *length)),
            _ => None,
        }
    }

    #[inline(always)]
    pub fn is_array(&self) -> bool {
        self.array_info().is_some()
    }

    #[inline(always)]
    pub fn tuple_type_names(&self) -> Option<Vec<TypeName>> {
        match self {
            TypeName::Tuple { type_names } => Some(type_names.clone()),
            _ => None,
        }
    }

    #[inline(always)]
    pub fn is_tuple(&self) -> bool {
        self.tuple_type_names().is_some()
    }

    #[inline(always)]
    pub fn is_u8_array(&self) -> bool {
        self.u8_array_length().is_some()
    }

    #[inline(always)]
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

    #[inline(always)]
    pub fn is_option(&self) -> bool {
        self.option_type().is_some()
    }

    #[inline(always)]
    pub fn to_option(&self) -> TypeName {
        TypeName::Identifier {
            name: "Option".to_string(),
            generic_parameters: Some(GenericParameterList {
                entries: vec![GenericParameter {
                    type_name: self.clone(),
                    implements: None,
                }],
            }),
        }
    }

    #[inline(always)]
    pub fn create_result_type(ok_type: TypeName, error_type: TypeName) -> TypeName {
        TypeName::Identifier {
            name: "Result".to_string(),
            generic_parameters: Some(GenericParameterList {
                entries: vec![
                    GenericParameter {
                        type_name: ok_type,
                        implements: None,
                    },
                    GenericParameter {
                        type_name: error_type,
                        implements: None,
                    },
                ],
            }),
        }
    }

    #[inline(always)]
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

    #[inline(always)]
    pub fn is_storage_key(&self) -> bool {
        self.storage_key_type().is_some()
    }

    #[inline(always)]
    pub fn to_storage_key(&self) -> TypeName {
        if self.is_storage_key() {
            return self.clone();
        }

        TypeName::Identifier {
            name: "StorageKey".to_string(),
            generic_parameters: Some(GenericParameterList {
                entries: vec![GenericParameter {
                    type_name: if self.is_string_slice() {
                        TypeName::create_identifier("StorageString")
                    } else if self.is_bytes() {
                        TypeName::create_identifier("StorageBytes")
                    } else if let Some(vec_type) = self.vec_type() {
                        vec_type.to_storage_vec()
                    } else if let Some(storage_key_type) = self.storage_key_type() {
                        storage_key_type
                    } else {
                        self.clone()
                    },
                    implements: None,
                }],
            }),
        }
    }

    #[inline(always)]
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

    #[inline(always)]
    pub fn is_storage_map(&self) -> bool {
        self.storage_map_type().is_some()
    }

    #[inline(always)]
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

    #[inline(always)]
    pub fn is_storage_vec(&self) -> bool {
        self.storage_vec_type().is_some()
    }

    #[inline(always)]
    pub fn to_storage_vec(&self) -> TypeName {
        TypeName::Identifier {
            name: "StorageVec".to_string(),
            generic_parameters: Some(GenericParameterList {
                entries: vec![GenericParameter {
                    type_name: if self.is_string_slice() {
                        TypeName::create_identifier("StorageString")
                    } else if self.is_bytes() {
                        TypeName::create_identifier("StorageBytes")
                    } else if let Some(vec_type) = self.vec_type() {
                        vec_type.to_storage_vec()
                    } else if let Some(storage_key_type) = self.storage_key_type() {
                        storage_key_type
                    } else {
                        self.clone()
                    },
                    implements: None,
                }],
            }),
        }
    }

    #[inline(always)]
    pub fn is_storage_string(&self) -> bool {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => name == "StorageString",
            _ => false,
        }
    }

    #[inline(always)]
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

    #[inline(always)]
    pub fn is_vec(&self) -> bool {
        self.vec_type().is_some()
    }

    #[inline(always)]
    pub fn to_vec(&self) -> TypeName {
        TypeName::Identifier {
            name: "Vec".to_string(),
            generic_parameters: Some(GenericParameterList {
                entries: vec![GenericParameter {
                    type_name: self.clone(),
                    implements: None,
                }],
            }),
        }
    }

    #[inline(always)]
    pub fn is_bytes(&self) -> bool {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => name == "Bytes",

            _ => false,
        }
    }

    #[inline(always)]
    pub fn is_storage_bytes(&self) -> bool {
        match self {
            TypeName::Identifier {
                name,
                generic_parameters: None,
            } => name == "StorageBytes",

            _ => false,
        }
    }

    #[inline(always)]
    pub fn abi_type(&self) -> Option<TypeName> {
        match self {
            TypeName::Abi { type_name } => Some(type_name.as_ref().clone()),
            _ => None,
        }
    }

    #[inline(always)]
    pub fn is_abi_type(&self) -> bool {
        self.abi_type().is_some()
    }

    /// Checks to see if the type name is compatible with another type name
    pub fn is_compatible_with(&self, other: &TypeName) -> bool {
        // Check for todo types
        if self.is_todo() || other.is_todo() {
            return true;
        }

        // Check for abi and Identity types
        if self.is_identity() && other.is_identity() {
            return true;
        }

        // Check generic parameter compatibility
        if let (
            TypeName::Identifier {
                name: lhs_name,
                generic_parameters: Some(lhs_generic_parameters),
            },
            TypeName::Identifier {
                name: rhs_name,
                generic_parameters: Some(rhs_generic_parameters),
            },
        ) = (self, other)
            && lhs_name == rhs_name
            && lhs_generic_parameters.entries.len() == rhs_generic_parameters.entries.len()
        {
            for (lhs_generic_parameter, rhs_generic_parameter) in lhs_generic_parameters
                .entries
                .iter()
                .zip(rhs_generic_parameters.entries.iter())
            {
                if !lhs_generic_parameter
                    .type_name
                    .is_compatible_with(&rhs_generic_parameter.type_name)
                {
                    return false;
                }
            }

            return true;
        }

        // Check array type compatibility
        if let (
            TypeName::Array {
                type_name: lhs_type_name,
                length: lhs_length,
            },
            TypeName::Array {
                type_name: rhs_type_name,
                length: rhs_length,
            },
        ) = (self, other)
        {
            if !lhs_type_name.is_compatible_with(rhs_type_name) {
                return false;
            }

            if *lhs_length == *rhs_length {
                return true;
            }
        }

        // HACK: Don't check `_` value types
        if let TypeName::Identifier {
            name,
            generic_parameters: None,
        } = self
        {
            if name == "_" {
                return true;
            }
        }

        if let TypeName::Identifier {
            name,
            generic_parameters: None,
        } = other
        {
            if name == "_" {
                return true;
            }
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
                            && lhs
                                .type_name
                                .as_ref()
                                .is_none_or(|x| x.is_compatible_with(rhs.type_name.as_ref().unwrap()))
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

        if let (
            TypeName::Tuple {
                type_names: lhs_type_names,
            },
            TypeName::Tuple {
                type_names: rhs_type_names,
            },
        ) = (self, other)
        {
            if lhs_type_names
                .iter()
                .zip(rhs_type_names.iter())
                .all(|(lhs, rhs)| lhs.is_compatible_with(rhs))
            {
                return true;
            }
        }

        self == other
    }

    /// Gets a storage-compatible version of the type name
    pub fn to_storage_compatible_type(self) -> TypeName {
        if let Some(mut vec_type) = self.vec_type() {
            vec_type = vec_type.to_storage_compatible_type();

            return TypeName::Identifier {
                name: "StorageKey".to_string(),
                generic_parameters: Some(GenericParameterList {
                    entries: vec![GenericParameter {
                        type_name: vec_type.to_storage_vec(),
                        implements: None,
                    }],
                }),
            };
        }

        if self.is_bytes() {
            return TypeName::Identifier {
                name: "StorageKey".to_string(),
                generic_parameters: Some(GenericParameterList {
                    entries: vec![GenericParameter {
                        type_name: TypeName::Identifier {
                            name: "StorageBytes".to_string(),
                            generic_parameters: None,
                        },
                        implements: None,
                    }],
                }),
            };
        }

        if self.is_string() {
            return TypeName::Identifier {
                name: "StorageKey".to_string(),
                generic_parameters: Some(GenericParameterList {
                    entries: vec![GenericParameter {
                        type_name: TypeName::Identifier {
                            name: "StorageString".to_string(),
                            generic_parameters: None,
                        },
                        implements: None,
                    }],
                }),
            };
        }

        self
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct TypeDefinition {
    pub is_public: bool,
    pub name: TypeName,
    pub underlying_type: Option<TypeName>,
}

impl TabbedDisplay for TypeDefinition {
    fn tabbed_fmt(&self, _depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
    pub old_name: String,
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
                if suffix.as_ref().is_some_and(|s| s == "b256") {
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
                "0x{}{}",
                if suffix.as_ref().is_some_and(|s| s == "b256") {
                    format!("{x:064X}")
                } else {
                    format!("{x:X}")
                },
                if let Some(suffix) = suffix.as_ref() {
                    if suffix != "b256" { suffix.as_str() } else { "" }
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
    pub new_name: String,
    pub old_name: String,
    pub type_name: TypeName,
}

impl Display for StructField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_public {
            write!(f, "pub ")?;
        }

        write!(f, "{}: {}", self.new_name, self.type_name)
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
            write!(
                f,
                ": {}",
                self.inherits
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" + ")
            )?;
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
    pub namespaces: Vec<Rc<RefCell<StorageNamespace>>>,
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
            namespace.borrow().tabbed_fmt(depth + 1, f)?;
            writeln!(f, ",")?;
        }

        "}".tabbed_fmt(depth, f)
    }
}

impl Storage {
    pub fn extend(&mut self, other: &Self) {
        for member in other.fields.iter() {
            if !self.fields.contains(member) {
                self.fields.push(member.clone());
            }
        }

        for member in other.namespaces.iter() {
            if !self.namespaces.iter().any(|n| n.borrow().name == member.borrow().name) {
                self.namespaces.push(Rc::new(RefCell::new(StorageNamespace {
                    name: member.borrow().name.clone(),
                    ..Default::default()
                })));
            }

            let namespace = self
                .namespaces
                .iter_mut()
                .find(|n| n.borrow().name == member.borrow().name)
                .unwrap();

            namespace.borrow_mut().extend(&member.borrow());
        }
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, Default, PartialEq)]
pub struct StorageNamespace {
    pub name: String,
    pub fields: Vec<StorageField>,
    pub namespaces: Vec<Rc<RefCell<StorageNamespace>>>,
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
            namespace.borrow().tabbed_fmt(depth + 1, f)?;
            writeln!(f, ",")?;
        }

        "}".tabbed_fmt(depth, f)
    }
}

impl StorageNamespace {
    pub fn extend(&mut self, other: &Self) {
        assert!(self.name == other.name);

        for member in other.fields.iter() {
            if !self.fields.contains(member) {
                self.fields.push(member.clone());
            }
        }

        for member in other.namespaces.iter() {
            if !self.namespaces.iter().any(|n| n.borrow().name == member.borrow().name) {
                self.namespaces.push(Rc::new(RefCell::new(Self {
                    name: member.borrow().name.clone(),
                    ..Default::default()
                })));
            }

            let namespace = self
                .namespaces
                .iter_mut()
                .find(|n| n.borrow().name == member.borrow().name)
                .unwrap();

            namespace.borrow_mut().extend(&member.borrow());
        }
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct StorageField {
    pub old_name: String,
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
    pub old_name: String,
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
    pub new_name: String,
    pub generic_parameters: Option<GenericParameterList>,
    pub parameters: ParameterList,
    pub storage_struct_parameter: Option<Parameter>,
    pub return_type: Option<TypeName>,
    pub body: Option<Block>,
}

impl Function {
    pub fn get_type_name(&self) -> TypeName {
        let Function {
            old_name,
            new_name: name,
            generic_parameters,
            parameters,
            storage_struct_parameter,
            return_type,
            ..
        } = self;

        TypeName::Function {
            old_name: old_name.clone(),
            new_name: name.clone(),
            generic_parameters: generic_parameters.clone(),
            parameters: parameters.clone(),
            storage_struct_parameter: storage_struct_parameter.clone().map(Box::new),
            return_type: return_type.clone().map(Box::new),
        }
    }
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
            self.new_name,
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

impl Statement {
    /// Applies a lambda to a statement and its child statements.
    pub fn filter_map<T, F>(&self, f: F) -> Option<T>
    where
        F: Clone + Fn(&&Statement) -> Option<T>,
    {
        if let Some(result) = f(&self) {
            return Some(result);
        }

        let check_expression = |expression: &&_| -> Option<_> {
            match expression {
                Expression::Block(block) => {
                    for statement in block.statements.iter() {
                        if let Some(result) = statement.filter_map(f.clone()) {
                            return Some(result);
                        }
                    }
                }

                Expression::If(if_expr) => {
                    for statement in if_expr.then_body.statements.iter() {
                        if let Some(result) = statement.filter_map(f.clone()) {
                            return Some(result);
                        }
                    }
                }

                Expression::While(while_expr) => {
                    for statement in while_expr.body.statements.iter() {
                        if let Some(result) = statement.filter_map(f.clone()) {
                            return Some(result);
                        }
                    }
                }

                _ => {}
            }

            None
        };

        match self {
            Statement::Let(let_expr) => {
                if let Some(result) = let_expr.value.filter_map(check_expression) {
                    return Some(result);
                }
            }

            Statement::Expression(expression) => {
                if let Some(result) = expression.filter_map(check_expression) {
                    return Some(result);
                }
            }

            Statement::Commented(_, statement) => {
                if let Some(statement) = statement.as_ref()
                    && let Some(result) = statement.filter_map(f.clone())
                {
                    return Some(result);
                }
            }
        }

        None
    }
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
                ids.iter().map(|id| format!("{id}")).collect::<Vec<_>>().join(", ")
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
    Cast { from_type: TypeName, to_type: TypeName },
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
    pub fn display<'a>(&'a self) -> TabbedDisplayer<'a, Self> {
        TabbedDisplayer(self)
    }

    #[inline(always)]
    pub fn create_todo(msg: Option<String>) -> Expression {
        Expression::create_function_call(
            "todo!",
            None,
            if let Some(msg) = msg {
                vec![Expression::Literal(Literal::String(
                    msg.replace('\\', "\\\\").replace('\"', "\\\""),
                ))]
            } else {
                vec![]
            },
        )
    }

    #[inline(always)]
    pub fn create_unimplemented(msg: Option<String>) -> Expression {
        Expression::create_function_call(
            "unimplemented!",
            None,
            if let Some(msg) = msg {
                vec![Expression::Literal(Literal::String(msg))]
            } else {
                vec![]
            },
        )
    }

    #[inline(always)]
    pub fn create_identifier(name: &str) -> Expression {
        assert!(!name.is_empty());

        let mut segments = name.split("::").collect::<Vec<_>>();
        segments.reverse();
        let root = segments.pop().unwrap();
        segments.reverse();

        Expression::PathExpr(PathExpr {
            root: PathExprRoot::Identifier(root.to_string()),
            segments: segments
                .iter()
                .map(|s| PathExprSegment {
                    name: s.to_string(),
                    generic_parameters: None,
                })
                .collect(),
        })
    }

    #[inline(always)]
    pub fn create_dec_int_literal(value: BigUint, suffix: Option<&str>) -> Expression {
        Expression::from(Literal::DecInt(value, suffix.map(str::to_string)))
    }

    #[inline(always)]
    pub fn create_hex_int_literal(value: BigUint, suffix: Option<&str>) -> Expression {
        Expression::from(Literal::DecInt(value, suffix.map(str::to_string)))
    }

    #[inline(always)]
    pub fn create_binary(operator: &str, lhs: Expression, rhs: Expression) -> Expression {
        Expression::from(BinaryExpression {
            operator: operator.to_string(),
            lhs,
            rhs,
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

    #[inline(always)]
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

    #[inline(always)]
    pub fn with_member(&self, member: &str) -> Self {
        Self::from(MemberAccess {
            expression: self.clone(),
            member: member.to_string(),
        })
    }

    #[inline(always)]
    pub fn with_array_access(&self, index: Expression) -> Self {
        Self::from(ArrayAccess {
            expression: self.clone(),
            index,
        })
    }

    pub fn to_array_access_parts(&self) -> Option<(&Expression, &Expression)> {
        if let Expression::ArrayAccess(array_access) = self {
            return Some((&array_access.expression, &array_access.index));
        }

        None
    }

    #[inline(always)]
    pub fn with_function_calls(
        &self,
        member_calls: &[(&str, Option<(Option<GenericParameterList>, Vec<Self>)>)],
    ) -> Self {
        Self::create_function_calls(Some(self.clone()), member_calls)
    }

    #[inline(always)]
    pub fn with_function_call(
        &self,
        name: &str,
        generic_parameters: Option<GenericParameterList>,
        parameters: Vec<Self>,
    ) -> Self {
        self.with_function_calls(&[(name, Some((generic_parameters, parameters)))])
    }

    #[inline(always)]
    pub fn into_some_call(&self) -> Self {
        Self::create_function_call("Some", None, vec![self.clone()])
    }

    #[inline(always)]
    pub fn with_into_call(&self) -> Self {
        self.with_function_calls(&[("into", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn with_len_call(&self) -> Self {
        self.with_function_calls(&[("len", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn with_push_call(&self, value: Expression) -> Self {
        self.with_function_calls(&[("push", Some((None, vec![value])))])
    }

    #[inline(always)]
    pub fn with_pop_call(&self) -> Self {
        self.with_function_calls(&[("pop", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn with_bits_call(&self) -> Self {
        self.with_function_calls(&[("bits", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn to_bits_call_parts(&self) -> Option<&Expression> {
        if let Expression::FunctionCall(f) = self
            && let Expression::MemberAccess(m) = &f.function
            && m.member == "bits"
        {
            return Some(&m.expression);
        }

        None
    }

    #[inline(always)]
    pub fn with_as_contract_id_call(&self) -> Self {
        self.with_function_calls(&[("as_contract_id", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn with_as_address_call(&self) -> Self {
        self.with_function_calls(&[("as_address", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn with_is_some_call(&self) -> Self {
        self.with_function_calls(&[("is_some", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn with_is_none_call(&self) -> Self {
        self.with_function_calls(&[("is_none", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn with_as_bytes_call(&self) -> Self {
        self.with_function_calls(&[("as_bytes", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn with_as_str_call(&self) -> Self {
        self.with_function_calls(&[("as_str", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn to_as_str_call_parts(&self) -> Option<&Self> {
        if let Self::FunctionCall(f) = self
            && let Self::MemberAccess(m) = &f.function
            && m.member == "as_str"
            && f.parameters.is_empty()
        {
            return Some(&m.expression);
        }

        None
    }

    #[inline(always)]
    pub fn with_as_ptr_call(&self) -> Self {
        self.with_function_calls(&[("as_ptr", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn with_ptr_call(&self) -> Self {
        self.with_function_calls(&[("ptr", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn with_as_raw_slice_call(&self) -> Self {
        self.with_function_calls(&[("as_raw_slice", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn with_to_be_bytes_call(&self) -> Self {
        self.with_function_calls(&[("to_be_bytes", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn with_as_u8_call(&self) -> Self {
        self.with_function_calls(&[("as_u8", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn with_as_u16_call(&self) -> Self {
        self.with_function_calls(&[("as_u16", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn with_as_u32_call(&self) -> Self {
        self.with_function_calls(&[("as_u32", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn with_as_u64_call(&self) -> Self {
        self.with_function_calls(&[("as_u64", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn with_as_u256_call(&self) -> Self {
        self.with_function_calls(&[("as_u256", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn with_as_b256_call(&self) -> Self {
        self.with_function_calls(&[("as_b256", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn with_unwrap_call(&self) -> Self {
        self.with_function_calls(&[("unwrap", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn to_unwrap_call_parts(&self) -> Option<&Expression> {
        if let Self::FunctionCall(f) = self
            && let Self::MemberAccess(m) = &f.function
            && m.member == "unwrap"
            && f.parameters.is_empty()
        {
            return Some(&m.expression);
        }

        None
    }

    #[inline(always)]
    pub fn is_unwrap_call(&self) -> bool {
        self.to_unwrap_call_parts().is_some()
    }

    #[inline(always)]
    pub fn with_unwrap_or_call(&self, value: Expression) -> Self {
        self.with_function_calls(&[("unwrap_or", Some((None, vec![value])))])
    }

    #[inline(always)]
    pub fn with_clear_call(&self) -> Self {
        self.with_function_calls(&[("clear", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn with_get_call(&self, value: Expression) -> Self {
        self.with_function_calls(&[("get", Some((None, vec![value])))])
    }

    #[inline(always)]
    pub fn to_get_call_parts(&self) -> Option<(&Expression, &Expression)> {
        if let Expression::FunctionCall(function_call) = self
            && let Expression::MemberAccess(member_access) = &function_call.function
            && member_access.member == "get"
            && function_call.parameters.len() == 1
        {
            return Some((&member_access.expression, &function_call.parameters[0]));
        }

        None
    }

    #[inline(always)]
    pub fn with_set_call(&self, index: Expression, value: Expression) -> Self {
        self.with_function_calls(&[("set", Some((None, vec![index, value])))])
    }

    #[inline(always)]
    pub fn with_insert_call(&self, index: Expression, value: Expression) -> Self {
        self.with_function_calls(&[("insert", Some((None, vec![index, value])))])
    }

    #[inline(always)]
    pub fn with_remove_call(&self, index: Expression) -> Self {
        self.with_function_calls(&[("remove", Some((None, vec![index])))])
    }

    #[inline(always)]
    pub fn with_read_call(&self) -> Self {
        self.with_function_calls(&[("read", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn to_read_call_parts(&self) -> Option<&Expression> {
        if let Self::FunctionCall(f) = self
            && let Self::MemberAccess(m) = &f.function
            && m.member == "read"
            && f.parameters.is_empty()
        {
            return Some(&m.expression);
        }

        None
    }

    #[inline(always)]
    pub fn to_write_call_parts(&self) -> Option<(&Expression, &Expression)> {
        if let Self::FunctionCall(f) = self
            && let Self::MemberAccess(m) = &f.function
            && m.member == "write"
            && f.parameters.len() == 1
        {
            return Some((&m.expression, &f.parameters[0]));
        }

        None
    }

    #[inline(always)]
    pub fn is_read_call(&self) -> bool {
        self.to_read_call_parts().is_some()
    }

    #[inline(always)]
    pub fn with_read_slice_call(&self) -> Self {
        self.with_function_calls(&[("read_slice", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn with_load_vec_call(&self) -> Self {
        self.with_function_calls(&[("load_vec", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn with_write_call(&self, value: Expression) -> Self {
        self.with_function_calls(&[("write", Some((None, vec![value])))])
    }

    #[inline(always)]
    pub fn with_write_slice_call(&self, value: Expression) -> Self {
        self.with_function_calls(&[("write_slice", Some((None, vec![value])))])
    }

    #[inline(always)]
    pub fn with_write_str_call(&self, value: Expression) -> Self {
        self.with_function_calls(&[("write_str", Some((None, vec![value])))])
    }

    #[inline(always)]
    pub fn with_store_vec_call(&self, value: Expression) -> Self {
        self.with_function_calls(&[("store_vec", Some((None, vec![value])))])
    }

    #[inline(always)]
    pub fn with_abi_encode_call(&self, buffer_expression: Expression) -> Self {
        self.with_function_calls(&[("abi_encode", Some((None, vec![buffer_expression])))])
    }

    #[inline(always)]
    pub fn with_keccak256_call(&self) -> Self {
        self.with_function_calls(&[("keccak256", Some((None, vec![])))])
    }

    #[inline(always)]
    pub fn create_function_call(
        name: &str,
        generic_parameters: Option<GenericParameterList>,
        parameters: Vec<Expression>,
    ) -> Expression {
        Expression::create_function_calls(None, &[(name, Some((generic_parameters, parameters)))])
    }

    fn create_function_calls(
        mut container: Option<Expression>,
        member_calls: &[(&str, Option<(Option<GenericParameterList>, Vec<Expression>)>)],
    ) -> Expression {
        if let Some(container) = container.as_mut() {
            if let Expression::BinaryExpression(_) | Expression::UnaryExpression(_) = container {
                *container = Expression::Tuple(vec![container.clone()])
            }
        } else {
            assert!(!member_calls.is_empty());
        }

        let mut result = container;

        for (member, call_data) in member_calls {
            if member.contains("<") {
                todo!("Handle path strings with generic parameters");
            }

            let mut member_parts = member.split("::").collect::<Vec<_>>();

            assert!(!member_parts.is_empty());

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

    /// Applies a lambda to an expression and all of its child expressions.
    pub fn filter_map<T, F>(&self, f: F) -> Option<T>
    where
        F: Clone + Fn(&&Expression) -> Option<T>,
    {
        fn check_statement<T>(statement: &Statement, f: &impl Fn(&&Expression) -> Option<T>) -> Option<T> {
            match statement {
                Statement::Let(let_expr) => {
                    if let Some(result) = check_expression(&let_expr.value, f) {
                        return Some(result);
                    }
                }

                Statement::Expression(expression) => {
                    if let Some(result) = check_expression(expression, f) {
                        return Some(result);
                    }
                }

                Statement::Commented(_, statement) => {
                    if let Some(statement) = statement.as_ref()
                        && let Some(result) = check_statement(statement, f)
                    {
                        return Some(result);
                    }
                }
            }

            None
        }

        fn check_expression<T>(expression: &Expression, f: &impl Fn(&&Expression) -> Option<T>) -> Option<T> {
            if let Some(result) = f(&expression) {
                return Some(result);
            }

            match expression {
                Expression::FunctionCall(function_call) => {
                    if let Some(result) = check_expression(&function_call.function, f) {
                        return Some(result);
                    }

                    for parameter in function_call.parameters.iter() {
                        if let Some(result) = check_expression(parameter, f) {
                            return Some(result);
                        }
                    }
                }

                Expression::FunctionCallBlock(function_call) => {
                    if let Some(result) = check_expression(&function_call.function, f) {
                        return Some(result);
                    }

                    for field in function_call.fields.iter() {
                        if let Some(result) = check_expression(&field.value, f) {
                            return Some(result);
                        }
                    }

                    for parameter in function_call.parameters.iter() {
                        if let Some(result) = check_expression(parameter, f) {
                            return Some(result);
                        }
                    }
                }

                Expression::Block(block) => {
                    for statement in block.statements.iter() {
                        if let Some(result) = check_statement(statement, f) {
                            return Some(result);
                        }
                    }

                    if let Some(final_expr) = block.final_expr.as_ref()
                        && let Some(result) = check_expression(final_expr, f)
                    {
                        return Some(result);
                    }
                }

                Expression::Return(expression) => {
                    if let Some(expression) = expression.as_ref()
                        && let Some(result) = check_expression(expression.as_ref(), f)
                    {
                        return Some(result);
                    }
                }

                Expression::Array(array) => {
                    for element in array.elements.iter() {
                        if let Some(result) = check_expression(element, f) {
                            return Some(result);
                        }
                    }
                }

                Expression::ArrayAccess(array_access) => {
                    if let Some(result) = check_expression(&array_access.expression, f) {
                        return Some(result);
                    }

                    if let Some(result) = check_expression(&array_access.index, f) {
                        return Some(result);
                    }
                }

                Expression::MemberAccess(member_access) => {
                    if let Some(result) = check_expression(&member_access.expression, f) {
                        return Some(result);
                    }
                }

                Expression::Tuple(expressions) => {
                    for expression in expressions.iter() {
                        if let Some(result) = check_expression(expression, f) {
                            return Some(result);
                        }
                    }
                }

                Expression::If(if_expr) => {
                    let mut next_if_expr = Some(if_expr.as_ref().clone());

                    while let Some(if_expr) = next_if_expr.take() {
                        if let Some(condition) = if_expr.condition.as_ref()
                            && let Some(result) = check_expression(condition, f)
                        {
                            return Some(result);
                        }

                        for statement in if_expr.then_body.statements.iter() {
                            if let Some(result) = check_statement(statement, f) {
                                return Some(result);
                            }
                        }

                        if let Some(final_expr) = if_expr.then_body.final_expr.as_ref()
                            && let Some(result) = check_expression(final_expr, f)
                        {
                            return Some(result);
                        }

                        next_if_expr = if_expr.else_if.as_ref().map(|x| x.as_ref().clone());
                    }
                }

                Expression::Match(match_expr) => {
                    if let Some(result) = check_expression(&match_expr.expression, f) {
                        return Some(result);
                    }

                    for match_branch in match_expr.branches.iter() {
                        if let Some(result) = check_expression(&match_branch.pattern, f) {
                            return Some(result);
                        }

                        if let Some(result) = check_expression(&match_branch.value, f) {
                            return Some(result);
                        }
                    }
                }

                Expression::While(while_expr) => {
                    if let Some(result) = check_expression(&while_expr.condition, f) {
                        return Some(result);
                    }

                    for statement in while_expr.body.statements.iter() {
                        if let Some(result) = check_statement(statement, f) {
                            return Some(result);
                        }
                    }

                    if let Some(final_expr) = while_expr.body.final_expr.as_ref()
                        && let Some(result) = check_expression(final_expr, f)
                    {
                        return Some(result);
                    }
                }

                Expression::UnaryExpression(unary_expression) => {
                    if let Some(result) = check_expression(&unary_expression.expression, f) {
                        return Some(result);
                    }
                }

                Expression::BinaryExpression(binary_expression) => {
                    if let Some(result) = check_expression(&binary_expression.lhs, f) {
                        return Some(result);
                    }

                    if let Some(result) = check_expression(&binary_expression.rhs, f) {
                        return Some(result);
                    }
                }

                Expression::Constructor(constructor) => {
                    for field in constructor.fields.iter() {
                        if let Some(result) = check_expression(&field.value, f) {
                            return Some(result);
                        }
                    }
                }

                Expression::AsmBlock(asm_block) => {
                    for register in asm_block.registers.iter() {
                        if let Some(value) = register.value.as_ref()
                            && let Some(result) = check_expression(value, f)
                        {
                            return Some(result);
                        }
                    }
                }

                Expression::Commented(_, expression) => {
                    if let Some(result) = check_expression(expression.as_ref(), f) {
                        return Some(result);
                    }
                }

                _ => {}
            }

            None
        }

        check_expression(self, &f)
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
            new_name: "test".into(),
            generic_parameters: None,
            parameters: ParameterList::default(),
            storage_struct_parameter: None,
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
