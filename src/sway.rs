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
            let ModuleItem::Abi(abi) = x else { return false };
            abi.name == abi_name
        }) {
            self.items.push(ModuleItem::Abi(Abi {
                name: abi_name.into(),
                inherits: vec![],
                functions: vec![],
            }));
        }

        let Some(ModuleItem::Abi(result)) = self.items.iter_mut().find(|x| {
            let ModuleItem::Abi(abi) = x else { return false };
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
            let TypeName::Identifier { name: impl_type_name, .. } = &x.type_name else { return false };
            let Some(TypeName::Identifier { name: for_type_name, .. }) = x.for_type_name.as_ref() else { return false };
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
            let TypeName::Identifier { name: impl_type_name, .. } = &x.type_name else { return false };
            let Some(TypeName::Identifier { name: for_type_name, .. }) = x.for_type_name.as_ref() else { return false };
            impl_type_name == impl_name && for_type_name == for_name
        }) else {
            panic!("Failed to find impl item in module");
        };

        result
    }

    /// Retrieves the `storage` item from the module, creating it if it doesn't exist.
    pub fn get_or_create_storage(&mut self) -> &mut Storage {
        if !self.items.iter().any(|x| matches!(x, ModuleItem::Storage(_))) {
            self.items.push(ModuleItem::Storage(Storage::default()));
        }
        
        let Some(ModuleItem::Storage(result)) = self.items.iter_mut().find(|x| {
            matches!(x, ModuleItem::Storage(_))
        }) else {
            panic!("Failed to find storage item in module")
        };
        
        result
    }

    /// Retrieves the `configurable` item from the module, creating it if it doesn't exist.
    pub fn get_or_create_configurable(&mut self) -> &mut Configurable {
        if !self.items.iter().any(|x| matches!(x, ModuleItem::Configurable(_))) {
            self.items.push(ModuleItem::Configurable(Configurable::default()));
        }
        
        let Some(ModuleItem::Configurable(result)) = self.items.iter_mut().find(|x| {
            matches!(x, ModuleItem::Configurable(_))
        }) else {
            panic!("Failed to find configurable item in module")
        };
        
        result
    }
}

impl TabbedDisplay for Module {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{};", self.kind)?;
        writeln!(f)?;

        for (i, item) in self.items.iter().enumerate() {
            if i > 0 {
                writeln!(f)?;
            }

            item.tabbed_fmt(depth, f)?;
            writeln!(f)?;
        }

        Ok(())
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
            UseTree::Group { imports } => write!(f, "{{{}}}", imports.iter().map(|x| format!("{x}")).collect::<Vec<_>>().join(", ")),
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
            write!(f, ": {}", implements.iter().map(|x| format!("{x}")).collect::<Vec<_>>().join(" + "))?;
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
        write!(f, "<{}>", self.entries.iter().map(|x| format!("{x}")).collect::<Vec<_>>().join(", "))
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
        write!(f, "#[{}]", self.attributes.iter().map(|a| format!("{a}")).collect::<Vec<_>>().join(", "))
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub enum TypeName {
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
    String {
        length: usize,
    },
}

impl Display for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeName::Identifier { name, generic_parameters } => write!(f, "{name}{}", if let Some(p) = generic_parameters.as_ref() { format!("{p}") } else { String::new() }),
            TypeName::Array { type_name, length } => write!(f, "[{type_name}; {length}]"),
            TypeName::Tuple { type_names } => write!(f, "({})", type_names.iter().map(|t| format!("{t}")).collect::<Vec<_>>().join(", ")),
            TypeName::String { length } => write!(f, "str[{length}]"),
        }
    }
}

impl TypeName {
    pub fn has_storage_map(&self) -> bool {
        match self {
            TypeName::Identifier { name, generic_parameters } => {
                if name == "StorageMap" {
                    return true;
                }

                if let Some(generic_parameters) = generic_parameters.as_ref() {
                    for generic_parameter in generic_parameters.entries.iter() {
                        if generic_parameter.type_name.has_storage_map() {
                            return true;
                        }
                    }
                }

                false
            }

            TypeName::Array { type_name, .. } => type_name.has_storage_map(),
            TypeName::Tuple { type_names } => type_names.iter().any(|t| t.has_storage_map()),
            TypeName::String { .. } => false,
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
    DecInt(u64),
    HexInt(u64),
    String(String),
}

impl TabbedDisplay for Literal {
    fn tabbed_fmt(&self, _depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Bool(x) => write!(f, "{x}"),
            Literal::DecInt(x) => write!(f, "{x}"),
            Literal::HexInt(x) => write!(f, "0x{x:X}"),
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

#[derive(Clone, Debug, PartialEq)]
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
    pub inherits: Vec<String>,
    pub functions: Vec<Function>,
}

impl TabbedDisplay for Abi {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "abi {}", self.name)?;

        if !self.inherits.is_empty() {
            write!(f, ": {}", self.inherits.join(" + "))?;
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
}

impl TabbedDisplay for Storage {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "storage {{")?;

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
            writeln!(f)?;
        }

        writeln!(f, "}}")
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

#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub type_name: TypeName,
}

impl Display for Parameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.type_name)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, Default, PartialEq)]
pub struct ParameterList {
    pub entries: Vec<Parameter>,
}

impl Display for ParameterList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})", self.entries.iter().map(|x| format!("{x}")).collect::<Vec<_>>().join(", "))
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
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

        writeln!(f, "}}")
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
    // TODO: finish
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

                match x {
                    Expression::Block(_) | Expression::If(_) | Expression::While(_) => {}

                    _ => write!(f, ";")?,
                }

                Ok(())
            }
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
            LetPattern::Tuple(ids) => write!(f, "({})", ids.iter().map(|id| format!("{id}")).collect::<Vec<_>>().join(", ")),
        }
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
pub enum Expression {
    Literal(Literal),
    Identifier(String),
    FunctionCall(Box<FunctionCall>),
    Block(Box<Block>),
    Return(Option<Box<Expression>>),
    Array(Array),
    ArrayAccess(Box<ArrayAccess>),
    MemberAccess(Box<MemberAccess>),
    Tuple(Vec<Expression>),
    If(Box<If>),
    While(Box<While>),
    UnaryExpression(Box<UnaryExpression>),
    BinaryExpression(Box<BinaryExpression>),
    Constructor(Box<Constructor>),
    Continue,
    Break,
    // TODO: finish
}

impl TabbedDisplay for Expression {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Literal(x) => x.tabbed_fmt(depth, f),
            Expression::Identifier(x) => write!(f, "{x}"),
            Expression::FunctionCall(x) => x.tabbed_fmt(depth, f),
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
impl_expr_box_from!(FunctionCall);
impl_expr_box_from!(Block);
impl_expr_from!(Array);
impl_expr_box_from!(ArrayAccess);
impl_expr_box_from!(MemberAccess);
impl_expr_box_from!(If);
impl_expr_box_from!(While);
impl_expr_box_from!(UnaryExpression);
impl_expr_box_from!(BinaryExpression);
impl_expr_box_from!(Constructor);

impl Expression {
    pub fn create_todo(msg: Option<String>) -> Expression {
        Expression::FunctionCall(Box::new(FunctionCall {
            function: Expression::Identifier("todo!".into()),
            generic_parameters: None,
            parameters: if let Some(msg) = msg {
                vec![
                    Expression::Literal(Literal::String(msg.replace("\\", "\\\\").replace("\"", "\\\""))),
                ]
            } else {
                vec![]
            },
        }))
    }

    pub fn create_unimplemented(msg: Option<String>) -> Expression {
        Expression::FunctionCall(Box::new(FunctionCall {
            function: Expression::Identifier("unimplemented!".into()),
            generic_parameters: None,
            parameters: if let Some(msg) = msg {
                vec![
                    Expression::Literal(Literal::String(msg)),
                ]
            } else {
                vec![]
            },
        }))
    }

    pub fn create_value_expression(type_name: &TypeName, value: Option<&Expression>) -> Expression {
        match type_name {
            TypeName::Identifier { name, .. } => match name.as_str() {
                "bool" => match value {
                    None => Expression::Literal(Literal::Bool(false)),
                    Some(Expression::Literal(Literal::Bool(value))) => Expression::Literal(Literal::Bool(*value)),
                    Some(value) => panic!("Invalid bool value expression: {value:#?}"),
                }

                "u8" | "u16" | "u32" | "u64" | "u256" => match value {
                    None => Expression::Literal(Literal::DecInt(0)),
                    Some(value) if matches!(value, Expression::Literal(Literal::DecInt(_) | Literal::HexInt(_))) => value.clone(),
                    Some(value) => panic!("Invalid {name} value expression: {value:#?}"),
                }

                "StorageMap" => match value {
                    None => Expression::Constructor(Box::new(Constructor {
                        type_name: TypeName::Identifier {
                            name: "StorageMap".into(),
                            generic_parameters: None,
                        },
                        fields: vec![],
                    })),

                    Some(value) => panic!("Invalid StorageMap value expression: {value:#?}"),
                }

                _ => Expression::FunctionCall(Box::new(FunctionCall { // TODO: generate valid value expression
                    function: Expression::Identifier("todo!".into()),
                    generic_parameters: None,
                    parameters: vec![],
                })),
            },

            TypeName::Array { type_name, length } => match value {
                None => Expression::Array(Array {
                    elements: (0..*length).map(|_| Self::create_value_expression(type_name, None)).collect(),
                }),

                Some(Expression::Array(value)) => {
                    if value.elements.len() != *length {
                        panic!("Invalid array value expression, expected {} elements, found {}: {value:#?}", *length, value.elements.len());
                    }

                    Expression::Array(value.clone())
                }

                Some(value) => panic!("Invalid array value expression: {value:#?}"),
            }

            TypeName::Tuple { type_names } => match value {
                None => Expression::Tuple(
                    type_names.iter().map(|type_name| Self::create_value_expression(type_name, None)).collect()
                ),

                Some(Expression::Tuple(value)) if value.len() == type_names.len() => Expression::Tuple(value.clone()),

                Some(value) => panic!("Invalid tuple value expression: {value:#?}"),
            }

            TypeName::String { length } => match value {
                None => Expression::FunctionCall(Box::new(FunctionCall {
                    function: Expression::Identifier("__to_str_array".into()),
                    generic_parameters: None,
                    parameters: vec![
                        Expression::Literal(Literal::String((0..*length).map(|_| " ").collect())),
                    ],
                })),

                Some(Expression::Literal(Literal::String(value))) => {
                    if value.len() > *length {
                        panic!("Invalid string value expression, string is {} characters long, expected {}: {value}", value.len(), *length);
                    }

                    let mut value = value.clone();

                    while value.len() < *length {
                        value.push(' ');
                    }

                    Expression::FunctionCall(Box::new(FunctionCall {
                        function: Expression::Identifier("__to_str_array".into()),
                        generic_parameters: None,
                        parameters: vec![
                            Expression::Literal(Literal::String(value)),
                        ],
                    }))
                }

                Some(Expression::FunctionCall(f)) => {
                    let Expression::Identifier(id) = &f.function else {
                        panic!("Invalid string value expression, expected `__to_str_array` function call, found: {value:#?}");
                    };

                    if id != "__to_str_array" {
                        panic!("Invalid string value expression, expected `__to_str_array` function call, found: {value:#?}");
                    }

                    if f.parameters.len() != 1 {
                        panic!("Invalid string value expression, invalid parameters supplied to `__to_str_array` function call, found: {value:#?}");
                    }

                    let Expression::Literal(Literal::String(value)) = &f.parameters[0] else {
                        panic!("Invalid string value expression, expected string literal to be supplied to `__to_str_array` function call, found: {value:#?}");
                    };

                    if value.len() > *length {
                        panic!("Invalid string value expression, string is {} characters long, expected {}: {value}", value.len(), *length);
                    }

                    let mut value = value.clone();

                    while value.len() < *length {
                        value.push(' ');
                    }

                    Expression::FunctionCall(Box::new(FunctionCall {
                        function: Expression::Identifier("__to_str_array".into()),
                        generic_parameters: None,
                        parameters: vec![
                            Expression::Literal(Literal::String(value)),
                        ],
                    }))
                }

                Some(value) => panic!("Invalid string value expression: {value:#?}"),
            }
        }
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

        write!(f, "}}")
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
        self.value.tabbed_fmt(depth, f)
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
            name: "test".into(),
            generic_parameters: None,
            parameters: ParameterList::default(),
            return_type: None,
            body: Some(Block {
                statements: vec![
                    Statement::Expression(Expression::Return(None)),
                ],
                final_expr: None,
            }),
        }));

        // Display the generated contract module
        println!("{}", TabbedDisplayer(&module));
    }
}
