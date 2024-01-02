use std::fmt::Display;

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

pub trait TabbedDisplay {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
}

impl<T: Display> TabbedDisplay for T {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (0..depth).into_iter().map(|_| "    ").collect::<String>().fmt(f)?;
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

#[derive(Clone)]
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

#[derive(Clone)]
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
                generic_parameters: GenericParameterList::default(),
                type_name: TypeName::Identifier {
                    name: impl_name.into(),
                    generic_parameters: GenericParameterList::default(),
                },
                for_type_name: Some(TypeName::Identifier {
                    name: for_name.into(),
                    generic_parameters: GenericParameterList::default(),
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

#[derive(Clone)]
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

#[derive(Clone)]
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

#[derive(Clone)]
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

#[derive(Clone)]
pub struct GenericParameter {
    pub name: TypeName,
    pub implements: Vec<TypeName>,
}

impl Display for GenericParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;

        if !self.implements.is_empty() {
            write!(f, ": {}", self.implements.iter().map(|x| format!("{x}")).collect::<Vec<_>>().join(" + "))?;
        }

        Ok(())
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Default)]
pub struct GenericParameterList {
    pub entries: Vec<GenericParameter>,
}

impl Display for GenericParameterList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.entries.is_empty() {
            write!(f, "<{}>", self.entries.iter().map(|x| format!("{x}")).collect::<Vec<_>>().join(", "))?;
        }

        Ok(())
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone)]
pub enum TypeName {
    Identifier {
        name: String,
        generic_parameters: GenericParameterList,
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
            TypeName::Identifier { name, generic_parameters } => write!(f, "{name}{generic_parameters}"),
            TypeName::Array { type_name, length } => write!(f, "[{type_name}; {length}]"),
            TypeName::Tuple { type_names } => write!(f, "({})", type_names.iter().map(|t| format!("{t}")).collect::<Vec<_>>().join(", ")),
            TypeName::String { length } => write!(f, "str[{length}]"),
        }
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone)]
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

#[derive(Clone)]
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

#[derive(Clone)]
pub enum Literal {
    Bool(bool),
    DecInt(u64),
    HexInt(u64),
    String(String),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Bool(x) => write!(f, "{x}"),
            Literal::DecInt(x) => write!(f, "{x}"),
            Literal::HexInt(x) => write!(f, "0x{x:X}"),
            Literal::String(x) => write!(f, "\"{x}\""),
        }
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone)]
pub struct Struct {
    pub is_public: bool,
    pub name: String,
    pub generic_parameters: GenericParameterList,
    pub fields: Vec<StructField>,
}

impl TabbedDisplay for Struct {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_public {
            write!(f, "pub ")?;
        }

        writeln!(f, "struct {}{} {{", self.name, self.generic_parameters)?;

        for field in self.fields.iter() {
            field.tabbed_fmt(depth + 1, f)?;
            writeln!(f, ",")?;
        }

        "}".tabbed_fmt(depth, f)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone)]
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

#[derive(Clone)]
pub struct Enum {
    pub is_public: bool,
    pub name: String,
    pub generic_parameters: GenericParameterList,
    pub variants: Vec<EnumVariant>,
}

impl TabbedDisplay for Enum {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_public {
            write!(f, "pub ")?;
        }

        writeln!(f, "enum {}{} {{", self.name, self.generic_parameters)?;

        for field in self.variants.iter() {
            field.tabbed_fmt(depth + 1, f)?;
            writeln!(f, ",")?;
        }

        "}".tabbed_fmt(depth, f)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone)]
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

#[derive(Clone)]
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

        for function in self.functions.iter() {
            "".tabbed_fmt(depth + 1, f)?;
            function.tabbed_fmt(depth + 1, f)?;
            writeln!(f)?;
        }

        "}".tabbed_fmt(depth, f)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone)]
pub struct Trait {
    pub is_public: bool,
    pub name: String,
    pub generic_parameters: GenericParameterList,
    pub items: Vec<TraitItem>,
}

impl TabbedDisplay for Trait {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_public {
            write!(f, "pub ")?;
        }

        write!(f, "trait {}{} {{", self.name, self.generic_parameters)?;

        for item in self.items.iter() {
            item.tabbed_fmt(depth + 1, f)?;
        }

        "}".tabbed_fmt(depth, f)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone)]
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

#[derive(Clone, Default)]
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

#[derive(Clone)]
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

#[derive(Clone, Default)]
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

#[derive(Clone)]
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

#[derive(Clone)]
pub struct Function {
    pub is_public: bool,
    pub name: String,
    pub generic_parameters: GenericParameterList,
    pub parameters: ParameterList,
    pub return_type: Option<TypeName>,
    pub body: Option<Block>,
}

impl TabbedDisplay for Function {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_public {
            write!(f, "pub ")?;
        }

        write!(f, "fn {}{}{}", self.name, self.generic_parameters, self.parameters)?;

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

#[derive(Clone)]
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

#[derive(Clone, Default)]
pub struct ParameterList {
    pub entries: Vec<Parameter>,
}

impl Display for ParameterList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})", self.entries.iter().map(|x| format!("{x}")).collect::<Vec<_>>().join(", "))
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone)]
pub struct Impl {
    pub generic_parameters: GenericParameterList,
    pub type_name: TypeName,
    pub for_type_name: Option<TypeName>,
    pub items: Vec<ImplItem>,
}

impl TabbedDisplay for Impl {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "impl{} {}", self.generic_parameters, self.type_name)?;

        if let Some(for_type_name) = self.for_type_name.as_ref() {
            write!(f, " for {for_type_name}")?;
        }

        writeln!(f, " {{")?;

        for item in self.items.iter() {
            "".tabbed_fmt(depth + 1, f)?;
            item.tabbed_fmt(depth + 1, f)?;
            writeln!(f)?;
        }

        writeln!(f, "}}")
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone)]
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

#[derive(Clone)]
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

#[derive(Clone)]
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
                write!(f, ";")
            }
        }
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone)]
pub struct Let {
    pub is_mutable: bool,
    pub name: String,
    pub type_name: Option<TypeName>,
    pub value: Option<Expression>,
}

impl TabbedDisplay for Let {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let ")?;

        if self.is_mutable {
            write!(f, "mut ")?;
        }

        write!(f, "{}", self.name)?;

        if let Some(type_name) = self.type_name.as_ref() {
            write!(f, "{type_name}")?;
        }

        if let Some(value) = self.value.as_ref() {
            write!(f, " = ")?;
            value.tabbed_fmt(depth, f)?;
        }

        write!(f, ";")
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone)]
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
    UnaryExpression(Box<UnaryExpression>),
    BinaryExpression(Box<BinaryExpression>),
    Constructor(Box<Constructor>),
    // TODO: finish
}

impl TabbedDisplay for Expression {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Literal(x) => write!(f, "{x}"),
            Expression::Identifier(x) => write!(f, "{x}"),
            Expression::FunctionCall(x) => x.tabbed_fmt(depth, f),
            Expression::Block(x) => x.tabbed_fmt(depth, f),
            Expression::Return(x) => {
                "return".tabbed_fmt(depth, f)?;
                if let Some(x) = x.as_ref() {
                    write!(f, " ")?;
                    x.tabbed_fmt(0, f)?;
                }
                Ok(())
            }
            Expression::Array(x) => write!(f, "{x}"),
            Expression::ArrayAccess(x) => x.tabbed_fmt(depth, f),
            Expression::MemberAccess(x) => x.tabbed_fmt(depth, f),
            Expression::Tuple(x) => {
                "(".tabbed_fmt(depth, f)?;
                for (i, expr) in x.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    expr.tabbed_fmt(0, f)?;
                }
                write!(f, ")")
            }
            Expression::UnaryExpression(x) => x.tabbed_fmt(depth, f),
            Expression::BinaryExpression(x) => x.tabbed_fmt(depth, f),
            Expression::Constructor(x) => x.tabbed_fmt(depth, f),
        }
    }
}

impl Expression {
    pub fn create_value_expression(type_name: &TypeName) -> Expression {
        match type_name {
            TypeName::Identifier { name, generic_parameters } => match name.as_str() {
                "u8" | "u16" | "u32" | "u64" => Expression::Literal(Literal::DecInt(0)),

                "StorageMap" => Expression::Constructor(Box::new(Constructor {
                    type_name: TypeName::Identifier {
                        name: "StorageMap".into(),
                        generic_parameters: GenericParameterList::default(),
                    },
                    fields: vec![],
                })),

                _ => Expression::FunctionCall(Box::new(FunctionCall { // TODO: generate valid value expression
                    function: Expression::Identifier("todo!".into()),
                    generic_parameters: None,
                    parameters: vec![],
                })),
            },

            TypeName::Array { type_name, length } => Expression::Array(Array {
                elements: (0..*length).into_iter().map(|_| Self::create_value_expression(type_name)).collect(),
            }),

            TypeName::Tuple { type_names } => Expression::Tuple(
                type_names.iter().map(|type_name| Self::create_value_expression(type_name)).collect()
            ),

            TypeName::String { length } => Expression::FunctionCall(Box::new(FunctionCall {
                function: Expression::Identifier("__to_str_array".into()),
                generic_parameters: None,
                parameters: vec![
                    Expression::Literal(Literal::String((0..*length).into_iter().map(|_| " ").collect())),
                ],
            })),
        }
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone)]
pub struct FunctionCall {
    pub function: Expression,
    pub generic_parameters: Option<GenericParameterList>,
    pub parameters: Vec<Expression>,
}

impl TabbedDisplay for FunctionCall {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.function.tabbed_fmt(0, f)?;
        
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

#[derive(Clone, Default)]
pub struct Array {
    pub elements: Vec<Expression>,
}

impl Display for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;

        for (i, element) in self.elements.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }

            element.tabbed_fmt(0, f)?;
        }

        write!(f, "]")
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone)]
pub struct ArrayAccess {
    pub expression: Expression,
    pub index: Expression,
}

impl Display for ArrayAccess {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.expression.tabbed_fmt(0, f)?;
        write!(f, "[")?;
        self.index.tabbed_fmt(0, f)?;
        write!(f, "]")
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone)]
pub struct MemberAccess {
    pub expression: Expression,
    pub member: String,
}

impl Display for MemberAccess {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.expression.tabbed_fmt(0, f)?;
        write!(f, ".{}", self.member)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone)]
pub struct UnaryExpression {
    pub operator: String,
    pub expression: Expression,
}

impl Display for UnaryExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.operator)?;
        self.expression.tabbed_fmt(0, f)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone)]
pub struct BinaryExpression {
    pub operator: String,
    pub lhs: Expression,
    pub rhs: Expression,
}

impl Display for BinaryExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.lhs.tabbed_fmt(0, f)?;
        write!(f, " {} ", self.operator)?;
        self.rhs.tabbed_fmt(0, f)
    }
}

// -------------------------------------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone)]
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

#[derive(Clone)]
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
            is_public: true,
            name: "test".into(),
            generic_parameters: GenericParameterList::default(),
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
