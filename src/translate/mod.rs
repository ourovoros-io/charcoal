use crate::{error::Error, ir, project::Project, sway};
use convert_case::{Case, Casing};
use num_bigint::BigUint;
use num_traits::{One, Zero};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

mod assembly;
mod coerce;
mod contracts;
mod enums;
mod expressions;
mod functions;
mod import_directives;
mod statements;
mod storage;
mod structs;
mod symbols;
mod types;
pub use assembly::*;
pub use coerce::*;
pub use contracts::*;
pub use enums::*;
pub use expressions::*;
pub use functions::*;
pub use import_directives::*;
pub use statements::*;
pub use storage::*;
pub use structs::*;
pub use symbols::*;
pub use types::*;

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
        let postfix = name.chars().rev().take_while(|c| *c == '_').collect::<String>();
        format!("{prefix}{}{postfix}", name.to_case(case))
    };

    match name.as_str() {
        "self" => "this".into(),
        _ => name,
    }
}

/// Gets the base underlying type of the supplied type name
pub fn get_underlying_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    type_name: &sway::TypeName,
) -> sway::TypeName {
    // Check to see if the expression's type is a type definition and get the underlying type
    if let Some(type_definition) = project.find_type_definition(module.clone(), type_name.to_string().as_str()) {
        return get_underlying_type(
            project,
            module.clone(),
            type_definition.underlying_type.as_ref().unwrap(),
        );
    }

    // If we didn't find a type definition, check to see if an enum exists and get its underlying type
    if let Some(enum_definition) = project.find_enum(module.clone(), type_name.to_string().as_str()) {
        return get_underlying_type(
            project,
            module.clone(),
            enum_definition.type_definition.underlying_type.as_ref().unwrap(),
        );
    }

    type_name.clone()
}

#[inline]
pub fn get_return_type_name(
    _project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    type_name: &sway::TypeName,
) -> sway::TypeName {
    if type_name.is_string_slice() || type_name.is_storage_string() {
        // Ensure `std::string::*` is imported
        module.borrow_mut().ensure_use_declared("std::string::*");

        return sway::TypeName::create_identifier("String");
    }

    type_name.clone()
}

/// Attempts to get the type of the supplied expression.
pub fn get_expression_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    expression: &sway::Expression,
) -> Result<sway::TypeName, Error> {
    match expression {
        sway::Expression::Literal(literal) => Ok(get_literal_type(literal)),
        sway::Expression::PathExpr(path_expr) => {
            Ok(get_path_expr_type(project, module.clone(), scope.clone(), path_expr))
        }
        sway::Expression::FunctionCall(_) | sway::Expression::FunctionCallBlock(_) => {
            get_function_call_type(project, module.clone(), scope.clone(), expression)
        }
        sway::Expression::Block(block) => get_block_type(project, module.clone(), scope.clone(), block),
        sway::Expression::Return(value) => get_return_type(project, module.clone(), scope.clone(), value.as_deref()),
        sway::Expression::Array(array) => get_array_type(project, module.clone(), scope.clone(), array),
        sway::Expression::ArrayAccess(array_access) => {
            get_array_access_type(project, module.clone(), scope.clone(), array_access)
        }
        sway::Expression::MemberAccess(member_access) => {
            get_member_access_type(project, module.clone(), scope.clone(), member_access, expression)
        }
        sway::Expression::Tuple(tuple) => get_tuple_type(project, module.clone(), scope.clone(), tuple),
        sway::Expression::If(if_expr) => get_if_type(project, module.clone(), scope.clone(), if_expr),
        sway::Expression::Match(match_expr) => get_match_type(project, module.clone(), scope.clone(), match_expr),
        sway::Expression::While(_) => Ok(sway::TypeName::create_tuple(vec![])),
        sway::Expression::UnaryExpression(unary_expression) => {
            get_unary_expression_type(project, module.clone(), scope.clone(), unary_expression)
        }
        sway::Expression::BinaryExpression(binary_expression) => {
            get_binary_expression_type(project, module.clone(), scope.clone(), binary_expression)
        }
        sway::Expression::Constructor(constructor) => Ok(constructor.type_name.clone()),
        sway::Expression::Continue => Ok(sway::TypeName::create_tuple(vec![])),
        sway::Expression::Break => Ok(sway::TypeName::create_tuple(vec![])),
        sway::Expression::AsmBlock(asm_block) => get_asm_block_type(asm_block),
        sway::Expression::Commented(_, x) => get_expression_type(project, module.clone(), scope.clone(), x),
    }
}

pub fn get_abi_function_call_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    abi: &sway::Abi,
    is_abi_cast: bool,
    function_name: &str,
    parameter_types: &[sway::TypeName],
) -> Result<Option<sway::TypeName>, Error> {
    let functions = if is_abi_cast {
        abi.functions.clone()
    } else {
        module
            .borrow()
            .functions
            .iter()
            .map(|f| {
                let sway::TypeName::Function {
                    old_name,
                    new_name,
                    generic_parameters,
                    parameters,
                    storage_struct_parameter,
                    return_type,
                } = &f.signature
                else {
                    unreachable!()
                };
                sway::Function {
                    attributes: None,
                    is_public: false,
                    old_name: old_name.clone(),
                    new_name: new_name.clone(),
                    generic_parameters: generic_parameters.clone(),
                    parameters: parameters.clone(),
                    storage_struct_parameter: storage_struct_parameter.as_ref().map(|x| x.as_ref().clone()),
                    return_type: return_type.as_ref().map(|x| x.as_ref().clone()),
                    body: None,
                }
            })
            .collect()
    };

    if let Some(function) = functions.iter().find(|function| {
        let function_parameters = &function.parameters;

        // Ensure the function's new name matches the function call we're trying to find
        if function.new_name != function_name {
            return false;
        }

        // Ensure the supplied function call parameters match the function's parameters
        let mut function_parameter_count = function_parameters.entries.len();

        if !is_abi_cast && function.storage_struct_parameter.is_some() {
            function_parameter_count += 1;
        }

        if parameter_types.len() != function_parameter_count {
            return false;
        }

        for (i, parameter_type_name) in function_parameters
            .entries
            .iter()
            .map(|p| p.type_name.as_ref())
            .enumerate()
        {
            let Some(parameter_type_name) = parameter_type_name else {
                continue;
            };

            if !parameter_type_name.is_compatible_with(&parameter_types[i]) {
                return false;
            }
        }

        true
    }) {
        return Ok(Some(
            function
                .return_type
                .clone()
                .unwrap_or(sway::TypeName::Tuple { type_names: vec![] }),
        ));
    }

    // If we didn't find a function, check inherited functions
    for inherit in abi.inherits.iter() {
        let Some((module, contract)) = project.find_module_and_contract(module.clone(), inherit.to_string().as_str())
        else {
            continue;
        };

        let abi = contract.borrow().abi.clone();

        let scope = Rc::new(RefCell::new(ir::Scope::new(
            Some(inherit.to_string().as_str()),
            None,
            Some(scope.clone()),
        )));

        if let Some(result) = get_abi_function_call_type(
            project,
            module.clone(),
            scope.clone(),
            &abi,
            is_abi_cast,
            function_name,
            parameter_types,
        )? {
            return Ok(Some(result));
        }
    }

    Ok(None)
}

#[inline(always)]
fn get_literal_type(literal: &sway::Literal) -> sway::TypeName {
    match literal {
        sway::Literal::Bool(_) => sway::TypeName::create_identifier("bool"),

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
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    path_expr: &sway::PathExpr,
) -> sway::TypeName {
    fn check_expr(
        project: &mut Project,
        module: Rc<RefCell<ir::Module>>,
        scope: Rc<RefCell<ir::Scope>>,
        path_expr: &sway::PathExpr,
    ) -> Option<sway::TypeName> {
        // Check if the identifier is a translated enum variant
        if let sway::PathExprRoot::Identifier(path_root) = &path_expr.root
            && path_expr.segments.len() == 1
            && path_expr.segments[0].generic_parameters.is_none()
        {
            let enum_name = path_root;
            let variant_name = &path_expr.segments[0].name;

            if let Some(enum_definition) = project.find_enum(module.clone(), &enum_name)
                && enum_definition.variants_impl.items.iter().any(|i| {
                    let sway::ImplItem::Constant(variant) = i else {
                        return false;
                    };

                    variant.name == *variant_name
                })
            {
                return Some(sway::TypeName::create_identifier(enum_name));
            }
        }

        let Some(name) = path_expr.as_identifier() else {
            todo!("get type of non-identifier path expressions: {path_expr} - {path_expr:#?}")
        };

        if let Some(variable) = scope.borrow().get_variable_from_new_name(name) {
            let variable = variable.borrow();

            if let sway::TypeName::Function {
                generic_parameters,
                parameters,
                storage_struct_parameter,
                return_type,
                ..
            } = &variable.type_name
            {
                return Some(sway::TypeName::Function {
                    old_name: variable.old_name.clone(),
                    new_name: variable.new_name.clone(),
                    generic_parameters: generic_parameters.clone(),
                    parameters: parameters.clone(),
                    storage_struct_parameter: storage_struct_parameter.clone(),
                    return_type: return_type.clone(),
                });
            }

            return Some(variable.type_name.clone());
        }

        if let Some(function) = module.borrow().functions.iter().find(|f| {
            let sway::TypeName::Function { new_name, .. } = &f.signature else {
                unreachable!()
            };
            new_name == name
        }) {
            return Some(function.signature.clone());
        }

        if let Some(constant) = module.borrow().constants.iter().find(|c| c.name == name) {
            return Some(constant.type_name.clone());
        }

        if let Some(configurable) = module.borrow().configurable.as_ref()
            && let Some(field) = configurable.fields.iter().find(|c| c.name == name)
        {
            return Some(field.type_name.clone());
        }

        let contract_name = scope.borrow().get_contract_name();
        if let Some(contract_name) = contract_name
            && let Some(module) = project.find_module_containing_contract(module.clone(), &contract_name)
        {
            let function_name = scope.borrow().get_function_name();
            if let Some(function_name) = function_name {
                let module = module.borrow();

                if let Some(function) = module.functions.iter().find(|f| {
                    let sway::TypeName::Function { new_name, .. } = &f.signature else {
                        unreachable!()
                    };
                    *new_name == function_name
                }) {
                    let sway::TypeName::Function {
                        storage_struct_parameter,
                        ..
                    } = &function.signature
                    else {
                        unreachable!()
                    };

                    if let Some(storage_struct_parameter) = storage_struct_parameter.as_ref()
                        && name == storage_struct_parameter.name
                    {
                        return storage_struct_parameter.type_name.clone();
                    }
                }

                if module.modifiers.iter().any(|m| {
                    let sway::TypeName::Function { new_name, .. } = &m.signature else {
                        unreachable!()
                    };
                    *new_name == function_name
                }) {
                    return Some(sway::TypeName::create_identifier(
                        format!("{contract_name}Storage").as_str(),
                    ));
                }
            }

            for use_item in module.borrow().uses.iter() {
                if let Some(module) = project.resolve_use(use_item)
                    && let Some(result) = check_expr(project, module.clone(), scope.clone(), path_expr)
                {
                    return Some(result);
                }
            }
        }

        None
    }

    if let Some(result) = check_expr(project, module.clone(), scope.clone(), path_expr) {
        return result;
    }

    if path_expr.to_string() == "_" {
        return sway::TypeName::create_identifier("_");
    }

    panic!(
        "error: Variable not found in scope: \"{}\"",
        sway::TabbedDisplayer(path_expr)
    );
}

#[inline(always)]
fn get_block_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    block: &sway::Block,
) -> Result<sway::TypeName, Error> {
    let Some(expression) = block.final_expr.as_ref() else {
        return Ok(sway::TypeName::create_tuple(vec![]));
    };

    let inner_scope = Rc::new(RefCell::new(ir::Scope::new(None, None, Some(scope.clone()))));

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
            None => get_expression_type(project, module.clone(), inner_scope.clone(), value)?,
        };

        let add_variable = |id: &sway::LetIdentifier, type_name: &sway::TypeName| {
            inner_scope
                .borrow_mut()
                .add_variable(Rc::new(RefCell::new(ir::Variable {
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

    get_expression_type(project, module.clone(), inner_scope.clone(), expression)
}

#[inline(always)]
fn get_return_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    value: Option<&sway::Expression>,
) -> Result<sway::TypeName, Error> {
    if let Some(value) = value.as_ref() {
        get_expression_type(project, module.clone(), scope.clone(), value)
    } else {
        Ok(sway::TypeName::create_tuple(vec![]))
    }
}

#[inline(always)]
fn get_array_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    array: &sway::Array,
) -> Result<sway::TypeName, Error> {
    Ok(sway::TypeName::Array {
        type_name: Box::new(if let Some(expression) = array.elements.first() {
            get_expression_type(project, module.clone(), scope.clone(), expression)?
        } else {
            sway::TypeName::create_tuple(vec![])
        }),
        length: array.elements.len(),
    })
}

#[inline(always)]
fn get_array_access_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    array_access: &sway::ArrayAccess,
) -> Result<sway::TypeName, Error> {
    let element_type_name = get_expression_type(project, module.clone(), scope.clone(), &array_access.expression)?;

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
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    member_access: &sway::MemberAccess,
    expression: &sway::Expression,
) -> Result<sway::TypeName, Error> {
    if let sway::Expression::PathExpr(path) = &member_access.expression {
        let name = path.to_string();

        if name.starts_with("storage::") {
            let parts = name.split("::").collect::<Vec<_>>();

            assert!(parts.len() == 2);

            let contract_name = scope.borrow().get_contract_name().unwrap();
            let storage_namespace_name = parts[1];

            fn check_contract(
                project: &mut Project,
                module: Rc<RefCell<ir::Module>>,
                contract: Rc<RefCell<ir::Contract>>,
                storage_namespace_name: &str,
                storage_field_name: &str,
            ) -> Result<Option<sway::TypeName>, Error> {
                let contract_storage_namespace_name = contract.borrow().name.to_case(Case::Snake);

                if storage_namespace_name == contract_storage_namespace_name
                    && let Some(storage) = contract.borrow().storage.as_ref()
                    && let Some(storage_namespace) = storage
                        .borrow()
                        .namespaces
                        .iter()
                        .find(|n| n.borrow().name == contract_storage_namespace_name)
                    && let Some(field) = storage_namespace
                        .borrow()
                        .fields
                        .iter()
                        .find(|f| f.name == storage_field_name)
                {
                    return Ok(Some(field.type_name.clone()));
                }

                let inherits = contract.borrow().abi.inherits.clone();

                for inherited_contract_name in inherits {
                    let (module, inherited_contract) = project
                        .find_module_and_contract(module.clone(), inherited_contract_name.to_string().as_str())
                        .unwrap();

                    if let Some(result) = check_contract(
                        project,
                        module.clone(),
                        inherited_contract.clone(),
                        storage_namespace_name,
                        storage_field_name,
                    )? {
                        return Ok(Some(result));
                    }
                }

                Ok(None)
            }

            let contract = project.find_contract(module.clone(), contract_name.as_str()).unwrap();

            if let Some(type_name) = check_contract(
                project,
                module.clone(),
                contract.clone(),
                storage_namespace_name,
                member_access.member.as_str(),
            )? {
                return Ok(type_name.to_storage_key());
            }

            panic!(
                "Failed to find storage variable in scope: `{}`",
                sway::TabbedDisplayer(member_access),
            )
        }
    }

    let container_type = get_expression_type(project, module.clone(), scope.clone(), &member_access.expression)?;

    // Check if field is a signed integer
    if let Some(bits) = container_type.int_bits() {
        if member_access.member.as_str() == "underlying" {
            return Ok(sway::TypeName::create_identifier(match bits {
                8 => "u8",
                16 => "u16",
                32 => "u32",
                64 => "u64",
                128 => "U128",
                256 => "u256",
                _ => unimplemented!("I{bits}"),
            }));
        }
    }

    // Check if container is a struct
    if let sway::TypeName::Identifier {
        name,
        generic_parameters: None,
    } = &container_type
    {
        let mut struct_definition = project.find_struct(module.clone(), scope.clone(), name);

        // HACK: if we couldn't find it in the current module, check them ALL
        if struct_definition.is_none() {
            fn try_to_find_struct(
                project: &mut Project,
                module: Rc<RefCell<ir::Module>>,
                scope: Rc<RefCell<ir::Scope>>,
                name: &str,
            ) -> Option<Rc<RefCell<ir::Struct>>> {
                if let Some(external_struct_definition) = project.find_struct(module.clone(), scope.clone(), name) {
                    return Some(external_struct_definition.clone());
                }

                for submodule in module.borrow().submodules.iter() {
                    if let Some(result) = try_to_find_struct(project, submodule.clone(), scope.clone(), name) {
                        return Some(result);
                    }
                }

                None
            }

            for external_module in project.translated_modules.clone() {
                if let Some(result) = try_to_find_struct(project, external_module.clone(), scope.clone(), name) {
                    struct_definition = Some(result.clone());
                    break;
                }
            }
        }

        if let Some(struct_definition) = struct_definition {
            let struct_definition = struct_definition.borrow();

            let fields = if struct_definition.memory.name == *name {
                struct_definition.memory.fields.as_slice()
            } else if struct_definition.storage.name == *name {
                struct_definition.storage.fields.as_slice()
            } else {
                todo!()
            };

            if let Some(field) = fields.iter().find(|f| f.new_name == member_access.member) {
                return Ok(field.type_name.clone());
            }
        }
    }

    todo!(
        "get type of {container_type} member access expression: {}",
        sway::TabbedDisplayer(expression)
    )
}

#[inline(always)]
fn get_tuple_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    tuple: &[sway::Expression],
) -> Result<sway::TypeName, Error> {
    if tuple.len() == 1 {
        get_expression_type(project, module.clone(), scope.clone(), tuple.first().unwrap())
    } else {
        Ok(sway::TypeName::Tuple {
            type_names: tuple
                .iter()
                .map(|x| get_expression_type(project, module.clone(), scope.clone(), x))
                .collect::<Result<Vec<_>, _>>()?,
        })
    }
}

#[inline(always)]
fn get_if_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    if_expr: &sway::If,
) -> Result<sway::TypeName, Error> {
    if let Some(expression) = if_expr.then_body.final_expr.as_ref() {
        get_expression_type(project, module.clone(), scope.clone(), expression)
    } else {
        Ok(sway::TypeName::create_tuple(vec![]))
    }
}

#[inline(always)]
fn get_match_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    match_expr: &sway::Match,
) -> Result<sway::TypeName, Error> {
    if let Some(branch) = match_expr.branches.first() {
        let branch_scope = Rc::new(RefCell::new(ir::Scope::new(None, None, Some(scope.clone()))));

        // Add branch pattern destructured variables to branch-specific scope
        if let sway::Expression::FunctionCall(f) = &branch.pattern {
            if let sway::Expression::PathExpr(path_expr) = &f.function {
                match path_expr.to_string().as_str() {
                    "Identity::Address" if f.parameters.len() == 1 => {
                        if let Some(ident) = f.parameters[0].as_identifier() {
                            branch_scope
                                .borrow_mut()
                                .add_variable(Rc::new(RefCell::new(ir::Variable {
                                    new_name: ident.into(),
                                    type_name: sway::TypeName::create_identifier("Address"),
                                    ..Default::default()
                                })));
                        }
                    }

                    "Identity::ContractId" if f.parameters.len() == 1 => {
                        if let Some(ident) = f.parameters[0].as_identifier() {
                            branch_scope
                                .borrow_mut()
                                .add_variable(Rc::new(RefCell::new(ir::Variable {
                                    new_name: ident.into(),
                                    type_name: sway::TypeName::create_identifier("ContractId"),
                                    ..Default::default()
                                })));
                        }
                    }

                    _ => {}
                }
            }
        }

        return get_expression_type(project, module.clone(), branch_scope, &branch.value);
    }

    Ok(sway::TypeName::create_tuple(vec![]))
}

#[inline(always)]
fn get_unary_expression_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    unary_expression: &sway::UnaryExpression,
) -> Result<sway::TypeName, Error> {
    get_expression_type(project, module.clone(), scope.clone(), &unary_expression.expression)
}

#[inline(always)]
fn get_binary_expression_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    binary_expression: &sway::BinaryExpression,
) -> Result<sway::TypeName, Error> {
    match binary_expression.operator.as_str() {
        "==" | "!=" | ">" | "<" | ">=" | "<=" | "&&" | "||" => Ok(sway::TypeName::create_identifier("bool")),

        _ => get_expression_type(project, module.clone(), scope.clone(), &binary_expression.lhs),
    }
}

#[inline(always)]
fn get_asm_block_type(asm_block: &sway::AsmBlock) -> Result<sway::TypeName, Error> {
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
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    expression: &sway::Expression,
) -> Result<sway::TypeName, Error> {
    let (function, function_generic_parameters, parameters) = match expression {
        sway::Expression::FunctionCall(f) => (&f.function, f.generic_parameters.as_ref(), &f.parameters),
        sway::Expression::FunctionCallBlock(f) => (&f.function, f.generic_parameters.as_ref(), &f.parameters),
        _ => unimplemented!(),
    };

    match function {
        sway::Expression::PathExpr(path_expr) => Ok(get_path_expr_function_call_type(
            project,
            module.clone(),
            scope.clone(),
            path_expr,
            function_generic_parameters,
            parameters.as_slice(),
        )?
        .unwrap()),

        sway::Expression::MemberAccess(member_access) => get_member_access_function_call_type(
            project,
            module.clone(),
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
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    path_expr: &sway::PathExpr,
    generic_parameters: Option<&sway::GenericParameterList>,
    parameters: &[sway::Expression],
) -> Result<Option<sway::TypeName>, Error> {
    let name = path_expr.to_string();
    assert!(!name.is_empty());

    // Check special forms
    match name.as_str() {
        "todo!" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("todo!")));
        }

        "abi" if generic_parameters.is_none() => {
            assert!(
                parameters.len() == 2,
                "Malformed abi cast, expected 2 parameters, found {}",
                parameters.len()
            );

            let Some(definition_name) = parameters[0].as_identifier() else {
                panic!("Malformed abi cast, expected identifier, found {:#?}", parameters[0]);
            };

            return Ok(Some(sway::TypeName::Abi {
                type_name: Box::new(sway::TypeName::create_identifier(definition_name)),
            }));
        }

        _ => {}
    }

    let parameter_types = parameters
        .iter()
        .map(|p| get_expression_type(project, module.clone(), scope.clone(), p))
        .collect::<Result<Vec<_>, _>>()?;

    // Check built-ins and standard library types
    match name.as_str() {
        "__size_of" => {
            if let Some(generic_parameters) = generic_parameters {
                assert!(generic_parameters.entries.len() == 1);

                return Ok(Some(generic_parameters.entries[0].type_name.clone()));
            }

            return Ok(Some(sway::TypeName::create_identifier("u64")));
        }

        "Address::from" if generic_parameters.is_none() => {
            assert!(
                parameters.len() == 1,
                "Malformed `Address::from` call, expected 1 parameter, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::create_identifier("Address")));
        }

        "AssetId::default" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("AssetId")));
        }

        "b256::from" | "b256::from_be_bytes" | "b256::from_le_bytes" | "b256::zero" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("b256")));
        }

        "Bytes::new" | "Bytes::from" | "Bytes::with_capacity" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("Bytes")));
        }

        "ContractId::from" | "ContractId::this" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("ContractId")));
        }

        "from_str_array" if generic_parameters.is_none() => {
            assert!(
                parameters.len() == 1,
                "Malformed `from_str_array` call, expected 1 parameter, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::StringSlice));
        }

        "I8::from" | "I8::from_uint" | "I8::max" | "I8::min" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("I8")));
        }

        "I8::try_from" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("I8").to_option()));
        }

        "I16::from" | "I16::from_uint" | "I16::max" | "I16::min" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("I16")));
        }

        "I16::try_from" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("I16").to_option()));
        }

        "I32::from" | "I32::from_uint" | "I32::max" | "I32::min" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("I32")));
        }

        "I32::try_from" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("I32").to_option()));
        }

        "I64::from" | "I64::from_uint" | "I64::max" | "I64::min" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("I64")));
        }

        "I64::try_from" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("I64").to_option()));
        }

        "I128::from" | "I128::from_uint" | "I128::max" | "I128::min" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("I128")));
        }

        "I128::try_from" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("I128").to_option()));
        }

        "I256::from" | "I256::from_uint" | "I256::max" | "I256::min" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("I256")));
        }

        "I256::try_from" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("I256").to_option()));
        }

        "Identity::Address" | "Identity::ContractId" | "Identity::from" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("Identity")));
        }

        "msg_sender" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("Identity").to_option()));
        }

        "raw_slice::from_parts" if generic_parameters.is_some() => {
            if let Some(generic_parameters) = generic_parameters
                && generic_parameters.entries.len() == 1
            {
                return Ok(Some(sway::TypeName::create_identifier("raw_slice")));
            }
        }

        "Secp256k1::from" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("Secp256k1")));
        }

        "Some" if generic_parameters.is_none() && parameters.len() == 1 => {
            return Ok(Some(
                get_expression_type(project, module.clone(), scope.clone(), &parameters[0])?.to_option(),
            ));
        }

        "std::alloc::alloc" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("raw_ptr")));
        }

        "std::block::block_header_hash" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_result_type(
                sway::TypeName::create_identifier("b256"),
                sway::TypeName::create_identifier("BlockHashError"),
            )));
        }

        "std::block::height" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("u32")));
        }

        "std::block::timestamp" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("u64")));
        }

        "std::context::balance_of" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("u64")));
        }

        "std::context::msg_amount" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("u64")));
        }

        "std::context::this_balance" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("u64")));
        }

        "std::hash::keccak256" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("b256")));
        }

        "std::hash::sha256" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("b256")));
        }

        "std::inputs::input_message_data" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("Bytes").to_option()));
        }

        "std::registers::balance" if generic_parameters.is_none() => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::balance` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::create_identifier("u64")));
        }

        "std::registers::context_gas" if generic_parameters.is_none() => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::context_gas` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::create_identifier("u64")));
        }

        "std::registers::error" if generic_parameters.is_none() => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::error` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::create_identifier("u64")));
        }

        "std::registers::flags" if generic_parameters.is_none() => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::flags` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::create_identifier("u64")));
        }

        "std::registers::frame_ptr" if generic_parameters.is_none() => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::frame_ptr` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::create_identifier("raw_ptr")));
        }

        "std::registers::global_gas" if generic_parameters.is_none() => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::global_gas` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::create_identifier("u64")));
        }

        "std::registers::heap_ptr" if generic_parameters.is_none() => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::heap_ptr` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::create_identifier("raw_ptr")));
        }

        "std::registers::instrs_start" if generic_parameters.is_none() => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::instrs_start` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::create_identifier("raw_ptr")));
        }

        "std::registers::overflow" if generic_parameters.is_none() => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::overflow` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::create_identifier("u64")));
        }

        "std::registers::program_counter" if generic_parameters.is_none() => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::program_counter` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::create_identifier("raw_ptr")));
        }

        "std::registers::return_value" if generic_parameters.is_none() => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::return_value` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::create_identifier("u64")));
        }

        "std::registers::return_length" if generic_parameters.is_none() => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::return_length` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::create_identifier("u64")));
        }

        "std::registers::stack_ptr" if generic_parameters.is_none() => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::stack_ptr` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::create_identifier("raw_ptr")));
        }

        "std::registers::stack_start_ptr" if generic_parameters.is_none() => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::stack_start_ptr` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::create_identifier("raw_ptr")));
        }

        "String::from_ascii" | "String::from_ascii_str" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("String")));
        }

        "u8::from" | "u8::max" | "u8::min" | "u8::from_be_bytes" | "u8::from_le_bytes"
            if generic_parameters.is_none() =>
        {
            return Ok(Some(sway::TypeName::create_identifier("u8")));
        }

        "u8::try_from" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("u8").to_option()));
        }

        "u16::from" | "u16::max" | "u16::min" | "u16::from_be_bytes" | "u16::from_le_bytes"
            if generic_parameters.is_none() =>
        {
            return Ok(Some(sway::TypeName::create_identifier("u16")));
        }

        "u16::try_from" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("u16").to_option()));
        }

        "u32::from" | "u32::max" | "u32::min" | "u32::from_be_bytes" | "u32::from_le_bytes"
            if generic_parameters.is_none() =>
        {
            return Ok(Some(sway::TypeName::create_identifier("u32")));
        }

        "u32::try_from" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("u32").to_option()));
        }

        "u64::from" | "u64::max" | "u64::min" | "u64::from_be_bytes" | "u64::from_le_bytes"
            if generic_parameters.is_none() =>
        {
            return Ok(Some(sway::TypeName::create_identifier("u64")));
        }

        "u64::try_from" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("u64").to_option()));
        }

        "u256::from" | "u256::max" | "u256::min" | "u256::from_be_bytes" | "u256::from_le_bytes"
            if generic_parameters.is_none() =>
        {
            return Ok(Some(sway::TypeName::create_identifier("u256")));
        }

        "u256::try_from" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("u256").to_option()));
        }

        "U128::from" | "U128::max" | "U128::min" | "U128::zero" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("U128")));
        }

        "U128::try_from" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("U128").to_option()));
        }

        "U256::from" | "U256::max" | "U256::min" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("U256")));
        }

        "U256::try_from" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("U256").to_option()));
        }

        "Vec::with_capacity" if generic_parameters.is_none() => {
            return Ok(Some(sway::TypeName::create_identifier("_").to_vec()));
        }

        _ => {}
    }

    // Check custom event and error types
    if let sway::PathExprRoot::Identifier(path_root) = &path_expr.root
        && path_expr.segments.len() == 1
    {
        // Check custom event types
        let event_contract_name = path_root.trim_end_matches("Event");
        let event_scope = Rc::new(RefCell::new(ir::Scope::new(Some(&event_contract_name), None, None)));

        if let Some(SymbolData::EventVariant { type_name, variant }) = resolve_symbol(
            project,
            module.clone(),
            event_scope.clone(),
            Symbol::Event(path_expr.segments[0].name.clone()),
        ) {
            let mut valid = true;

            if let Some(tuple_types) = variant.type_name.tuple_type_names()
                && !tuple_types.is_empty()
                && parameters.len() == 1
                && let sway::Expression::Tuple(parameters) = &parameters[0]
                && let sway::TypeName::Tuple {
                    type_names: parameter_types,
                } = &parameter_types[0]
            {
                if parameters.len() != tuple_types.len() {
                    valid = false;
                } else {
                    for (parameter_type, tuple_type) in parameter_types.iter().zip(tuple_types.iter()) {
                        if !parameter_type.is_compatible_with(tuple_type) {
                            valid = false;
                            break;
                        }
                    }
                }
            } else {
                if parameters.len() != 1 {
                    valid = false;
                } else {
                    if !parameter_types[0].is_compatible_with(&variant.type_name) {
                        valid = false;
                    }
                }
            }

            if valid {
                return Ok(Some(sway::TypeName::create_identifier(type_name.to_string().as_str())));
            }
        }

        // Check custom error types
        let error_contract_name = path_root.trim_end_matches("Error");
        let error_scope = Rc::new(RefCell::new(ir::Scope::new(Some(&error_contract_name), None, None)));
        if let Some(SymbolData::ErrorVariant { type_name, variant }) = resolve_symbol(
            project,
            module.clone(),
            error_scope.clone(),
            Symbol::Error(path_expr.segments[0].name.clone()),
        ) {
            let mut valid = true;

            if let Some(tuple_types) = variant.type_name.tuple_type_names()
                && !tuple_types.is_empty()
                && parameters.len() == 1
                && let sway::Expression::Tuple(parameters) = &parameters[0]
                && let sway::TypeName::Tuple {
                    type_names: parameter_types,
                } = &parameter_types[0]
            {
                if parameters.len() != tuple_types.len() {
                    valid = false;
                } else {
                    for (parameter_type, tuple_type) in parameter_types.iter().zip(tuple_types.iter()) {
                        if !parameter_type.is_compatible_with(tuple_type) {
                            valid = false;
                            break;
                        }
                    }
                }
            } else {
                if parameters.len() != 1 {
                    valid = false;
                } else {
                    if !parameter_types[0].is_compatible_with(&variant.type_name) {
                        valid = false;
                    }
                }
            }

            if valid {
                return Ok(Some(sway::TypeName::create_identifier(type_name.to_string().as_str())));
            }
        }
    }

    fn check_function(
        project: &mut Project,
        from_module: Option<Rc<RefCell<ir::Module>>>,
        module: Rc<RefCell<ir::Module>>,
        scope: Rc<RefCell<ir::Scope>>,
        path_expr: &sway::PathExpr,
        name: &str,
        generic_parameters: Option<&sway::GenericParameterList>,
        parameters: &[sway::Expression],
        parameter_types: &[sway::TypeName],
    ) -> Result<Option<sway::TypeName>, Error> {
        let project = Rc::new(RefCell::new(project));

        let find_function = |function: &sway::TypeName| {
            let sway::TypeName::Function {
                new_name,
                parameters: function_parameters,
                storage_struct_parameter,
                ..
            } = function
            else {
                return false;
            };

            // Ensure the function's old name matches the function call we're translating
            if new_name != name {
                return false;
            }

            // Ensure the supplied function call args match the function's parameters
            let mut function_parameter_count = function_parameters.entries.len();

            if storage_struct_parameter.is_some() {
                function_parameter_count += 1;
            }

            if parameters.len() != function_parameter_count {
                return false;
            }

            for (i, value_type_name) in parameter_types.iter().enumerate() {
                if i >= function_parameters.entries.len() {
                    break;
                }

                let Some(parameter_type_name) = function_parameters.entries[i].type_name.as_ref() else {
                    continue;
                };

                let value_type_name = get_underlying_type(
                    &mut *project.borrow_mut(),
                    if let Some(from_module) = from_module.as_ref() {
                        from_module.clone()
                    } else {
                        module.clone()
                    },
                    value_type_name,
                );

                let parameter_type_name =
                    get_underlying_type(&mut *project.borrow_mut(), module.clone(), parameter_type_name);

                if !value_type_name.is_compatible_with(&parameter_type_name) {
                    return false;
                }
            }

            true
        };

        // Attempt to find a function in scope
        if let Some(function_signature) = module
            .borrow()
            .functions
            .iter()
            .map(|f| f.signature.clone())
            .find(find_function)
        {
            let sway::TypeName::Function { return_type, .. } = &function_signature else {
                unreachable!()
            };

            if let Some(return_type) = return_type.as_ref() {
                return Ok(Some(return_type.as_ref().clone()));
            }

            return Ok(Some(sway::TypeName::create_tuple(vec![])));
        }

        // Attempt to find a function pointer variable in scope
        if let Some(variable) = scope.borrow().find_variable(|v| {
            let v = v.borrow();

            let sway::TypeName::Function {
                generic_parameters,
                parameters,
                storage_struct_parameter,
                return_type,
                ..
            } = &v.type_name
            else {
                return false;
            };

            find_function(&sway::TypeName::Function {
                old_name: v.old_name.clone(),
                new_name: v.new_name.clone(),
                generic_parameters: generic_parameters.clone(),
                parameters: parameters.clone(),
                storage_struct_parameter: storage_struct_parameter.clone(),
                return_type: return_type.clone(),
            })
        }) {
            let variable = variable.borrow();
            let sway::TypeName::Function { return_type, .. } = &variable.type_name else {
                unreachable!()
            };

            if let Some(return_type) = return_type.as_ref() {
                return Ok(Some(return_type.as_ref().clone()));
            } else {
                return Ok(Some(sway::TypeName::create_tuple(vec![])));
            }
        }

        // Attempt to find a function from an import
        for use_item in module.borrow().uses.iter() {
            let found_module = {
                let mut project = project.borrow_mut();
                project.resolve_use(use_item).clone()
            };

            if let Some(found_module) = found_module {
                if let Some(type_name) = check_function(
                    &mut *project.borrow_mut(),
                    from_module.clone(),
                    found_module.clone(),
                    scope.clone(),
                    path_expr,
                    name,
                    generic_parameters,
                    parameters,
                    parameter_types,
                )? {
                    return Ok(Some(type_name));
                }
            }
        }

        // If we didn't find a function, check inherited functions
        let contract_name = scope.borrow().get_contract_name();

        if let Some(contract_name) = contract_name {
            let contract = {
                let mut project = project.borrow_mut();
                project.find_contract(module.clone(), &contract_name).clone()
            };

            if let Some(contract) = contract {
                for inherit in contract.borrow().abi.inherits.clone() {
                    let module = {
                        let mut project = project.borrow_mut();
                        project
                            .find_module_containing_contract(module.clone(), inherit.to_string().as_str())
                            .clone()
                    };

                    if let Some(module) = module {
                        let scope = Rc::new(RefCell::new(ir::Scope::new(
                            Some(inherit.to_string().as_str()),
                            None,
                            Some(scope.clone()),
                        )));

                        if let Some(result) = get_path_expr_function_call_type(
                            &mut *project.borrow_mut(),
                            module.clone(),
                            scope.clone(),
                            path_expr,
                            generic_parameters,
                            parameters,
                        )? {
                            return Ok(Some(result));
                        }
                    }
                }
            }
        }

        // If we didn't find a function, check using directives
        if !parameters.is_empty() {
            let using_directives = module.borrow().using_directives.clone();

            for using_directive in using_directives {
                // Look up the definition of the using directive
                let external_module = {
                    let mut project = project.borrow_mut();
                    project.find_module_containing_contract(
                        if let Some(from_module) = from_module.as_ref() {
                            from_module.clone()
                        } else {
                            module.clone()
                        },
                        &using_directive.library_name,
                    )
                };

                if let Some(external_module) = external_module {
                    // Attempt to find a function in scope
                    if let Some(function_signature) = external_module
                        .borrow()
                        .functions
                        .iter()
                        .map(|f| f.signature.clone())
                        .find(find_function)
                    {
                        let sway::TypeName::Function { return_type, .. } = &function_signature else {
                            unreachable!()
                        };

                        if let Some(return_type) = return_type.as_ref() {
                            return Ok(Some(return_type.as_ref().clone()));
                        }

                        return Ok(Some(sway::TypeName::create_tuple(vec![])));
                    }

                    // Attempt to find a function pointer variable in scope
                    if let Some(variable) = scope.borrow().find_variable(|v| {
                        let v = v.borrow();
                        find_function(&v.type_name)
                    }) {
                        let variable = variable.borrow();
                        let sway::TypeName::Function { return_type, .. } = &variable.type_name else {
                            unreachable!()
                        };

                        if let Some(return_type) = return_type.as_ref() {
                            return Ok(Some(return_type.as_ref().clone()));
                        } else {
                            return Ok(Some(sway::TypeName::create_tuple(vec![])));
                        }
                    }
                }
            }
        }

        Ok(None)
    }

    if let Some(type_name) = check_function(
        project,
        Some(module.clone()),
        module.clone(),
        scope.clone(),
        path_expr,
        &name,
        generic_parameters,
        parameters,
        &parameter_types,
    )? {
        return Ok(Some(type_name));
    }

    panic!(
        "Failed to find function or variable `{}{}({})` in scope",
        path_expr,
        if let Some(generic_parameters) = generic_parameters {
            format!("{}", sway::TabbedDisplayer(generic_parameters))
        } else {
            String::new()
        },
        parameter_types
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(", "),
    )
}

#[inline(always)]
fn get_member_access_function_call_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    member_access: &sway::MemberAccess,
    function_generic_parameters: Option<&sway::GenericParameterList>,
    parameters: &[sway::Expression],
) -> Result<sway::TypeName, Error> {
    //
    // TODO: check generic parameters!
    //

    let mut container_type = get_expression_type(project, module.clone(), scope.clone(), &member_access.expression)?;

    container_type = get_underlying_type(project, module.clone(), &container_type);

    if container_type.is_identity() {
        match member_access.member.as_str() {
            "as_address" if parameters.is_empty() => {
                return Ok(sway::TypeName::create_identifier("Address").to_option());
            }

            "as_contract_id" if parameters.is_empty() => {
                return Ok(sway::TypeName::create_identifier("ContractId").to_option());
            }

            "bits" if parameters.is_empty() => {
                return Ok(sway::TypeName::create_identifier("b256"));
            }

            "is_address" if parameters.is_empty() => {
                return Ok(sway::TypeName::create_identifier("bool"));
            }

            "is_contract_id" if parameters.is_empty() => {
                return Ok(sway::TypeName::create_identifier("bool"));
            }

            _ => {}
        }
    }

    match &container_type {
        sway::TypeName::Undefined => panic!("Undefined type name"),

        sway::TypeName::Identifier {
            name,
            generic_parameters,
        } => match (name.as_str(), generic_parameters.as_ref()) {
            ("b256", None) => match member_access.member.as_str() {
                "as_u256" => Ok(sway::TypeName::create_identifier("u256")),

                "to_be_bytes" => Ok(sway::TypeName::create_array(
                    sway::TypeName::create_identifier("u8"),
                    32,
                )),

                "to_le_bytes" => Ok(sway::TypeName::create_array(
                    sway::TypeName::create_identifier("u8"),
                    32,
                )),

                _ => todo!(
                    "get type of `b256` member function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("Bytes", None) => match member_access.member.as_str() {
                "as_raw_slice" => Ok(sway::TypeName::create_identifier("raw_slice")),

                "get" => Ok(sway::TypeName::create_identifier("u8").to_option()),

                "len" => Ok(sway::TypeName::create_identifier("u64")),

                "ptr" => Ok(sway::TypeName::create_identifier("raw_ptr")),

                "split_at" => Ok(sway::TypeName::create_tuple(vec![
                    sway::TypeName::create_identifier("Bytes"),
                    sway::TypeName::create_identifier("Bytes"),
                ])),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("I8", None) => match member_access.member.as_str() {
                "wrapping_neg" => Ok(sway::TypeName::create_identifier("I8")),

                "underlying" => Ok(sway::TypeName::create_identifier("u8")),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("I16", None) => match member_access.member.as_str() {
                "wrapping_neg" => Ok(sway::TypeName::create_identifier("I16")),

                "underlying" => Ok(sway::TypeName::create_identifier("U16")),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("I32", None) => match member_access.member.as_str() {
                "wrapping_neg" => Ok(sway::TypeName::create_identifier("I32")),

                "underlying" => Ok(sway::TypeName::create_identifier("u32")),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("I64", None) => match member_access.member.as_str() {
                "wrapping_neg" => Ok(sway::TypeName::create_identifier("I64")),

                "underlying" => Ok(sway::TypeName::create_identifier("u64")),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("I128", None) => match member_access.member.as_str() {
                "wrapping_neg" => Ok(sway::TypeName::create_identifier("I128")),

                "underlying" => Ok(sway::TypeName::create_identifier("U128")),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("I256", None) => match member_access.member.as_str() {
                "wrapping_neg" => Ok(sway::TypeName::create_identifier("I256")),

                "underlying" => Ok(sway::TypeName::create_identifier("u256")),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("Option", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => {
                match member_access.member.as_str() {
                    "is_none" | "is_some" if parameters.is_empty() => Ok(sway::TypeName::create_identifier("bool")),

                    "unwrap" => Ok(generic_parameters.entries[0].type_name.clone()),
                    "unwrap_or" => Ok(generic_parameters.entries[0].type_name.clone()),

                    _ => todo!(
                        "get type of function call expression: {}",
                        sway::TabbedDisplayer(member_access)
                    ),
                }
            }

            ("Result", Some(generic_parameters)) if generic_parameters.entries.len() == 2 => {
                match member_access.member.as_str() {
                    "unwrap" => Ok(generic_parameters.entries[0].type_name.clone()),

                    "unwrap_or" => Ok(generic_parameters.entries[0].type_name.clone()),

                    _ => todo!(
                        "get type of function call expression: {}",
                        sway::TabbedDisplayer(member_access)
                    ),
                }
            }

            ("raw_ptr", None) => match member_access.member.as_str() {
                "add" => Ok(sway::TypeName::create_identifier("raw_ptr")),

                "read" => Ok(function_generic_parameters.unwrap().entries[0].type_name.clone()),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("raw_slice", None) => match member_access.member.as_str() {
                "ptr" => Ok(sway::TypeName::create_identifier("raw_ptr")),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("Secp256k1", None) => match member_access.member.as_str() {
                // Result<Address, SignatureError>
                "address" if parameters.len() == 1 => {
                    module
                        .borrow_mut()
                        .ensure_use_declared("std::crypto::signature_error::SignatureError");

                    Ok(sway::TypeName::create_result_type(
                        sway::TypeName::create_identifier("Address"),
                        sway::TypeName::create_identifier("SignatureError"),
                    ))
                }

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("StorageKey", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => {
                match member_access.member.as_str() {
                    "clear" => Ok(sway::TypeName::create_identifier("bool")),

                    "read" => Ok(generic_parameters.entries[0].type_name.clone()),

                    "try_read" => Ok(generic_parameters.entries[0].type_name.to_option()),

                    "write" => Ok(sway::TypeName::create_tuple(vec![])),

                    _ => match &generic_parameters.entries[0].type_name {
                        sway::TypeName::Identifier {
                            name,
                            generic_parameters,
                        } => match (name.as_str(), generic_parameters.as_ref()) {
                            ("StorageBytes", None) => match member_access.member.as_str() {
                                "clear" => Ok(sway::TypeName::create_identifier("bool")),

                                "len" => Ok(sway::TypeName::create_identifier("u64")),

                                "read_slice" => Ok(sway::TypeName::create_identifier("Bytes").to_option()),

                                "write_slice" => Ok(sway::TypeName::create_tuple(vec![])),

                                _ => todo!(
                                    "get type of function call expression: {}",
                                    sway::TabbedDisplayer(member_access)
                                ),
                            },

                            ("StorageMap", Some(generic_parameters)) if generic_parameters.entries.len() == 2 => {
                                match member_access.member.as_str() {
                                    "get" => Ok(generic_parameters.entries[1].type_name.to_storage_key()),

                                    "insert" => Ok(sway::TypeName::create_tuple(vec![])),

                                    "remove" => Ok(sway::TypeName::create_identifier("bool")),

                                    "try_insert" => Ok(sway::TypeName::create_result_type(
                                        generic_parameters.entries[0].type_name.clone(),
                                        sway::TypeName::create_generic(
                                            "StorageMapError",
                                            vec![generic_parameters.entries[0].type_name.clone()],
                                        ),
                                    )),

                                    _ => todo!(
                                        "get type of function call expression: {}",
                                        sway::TabbedDisplayer(member_access)
                                    ),
                                }
                            }

                            ("StorageString", None) => match member_access.member.as_str() {
                                "clear" => Ok(sway::TypeName::create_identifier("bool")),

                                "len" => Ok(sway::TypeName::create_identifier("u64")),

                                "read_slice" => Ok(sway::TypeName::create_identifier("String").to_option()),

                                "write_slice" => Ok(sway::TypeName::create_tuple(vec![])),

                                _ => todo!(
                                    "get type of function call expression: {}",
                                    sway::TabbedDisplayer(member_access)
                                ),
                            },

                            ("StorageVec", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => {
                                match member_access.member.as_str() {
                                    "fill" => Ok(sway::TypeName::create_tuple(vec![])),

                                    "first" => Ok(generic_parameters.entries[0].type_name.to_storage_key().to_option()),

                                    "get" => Ok(generic_parameters.entries[0].type_name.to_storage_key().to_option()),

                                    "insert" => Ok(sway::TypeName::create_tuple(vec![])),

                                    "is_empty" => Ok(sway::TypeName::create_identifier("bool")),

                                    "last" => Ok(generic_parameters.entries[0].type_name.to_storage_key().to_option()),

                                    "len" => Ok(sway::TypeName::create_identifier("u64")),

                                    "load_vec" => Ok(generic_parameters.entries[0].type_name.to_vec()),

                                    "pop" => Ok(generic_parameters.entries[0].type_name.to_option()),

                                    "push" => Ok(sway::TypeName::create_tuple(vec![])),

                                    "remove" => Ok(generic_parameters.entries[0].type_name.clone()),

                                    "resize" => Ok(sway::TypeName::create_tuple(vec![])),

                                    "reverse" => Ok(sway::TypeName::create_tuple(vec![])),

                                    "set" => Ok(sway::TypeName::create_tuple(vec![])),

                                    "store_vec" => Ok(sway::TypeName::create_tuple(vec![])),

                                    "swap_remove" => Ok(generic_parameters.entries[0].type_name.clone()),

                                    "swap" => Ok(sway::TypeName::create_tuple(vec![])),

                                    _ => todo!(
                                        "get type of function call expression: {}",
                                        sway::TabbedDisplayer(member_access)
                                    ),
                                }
                            }

                            (name, _) => todo!(
                                "get type of {name}::{} function call member_access: {}",
                                member_access.member,
                                sway::TabbedDisplayer(member_access)
                            ),
                        },

                        _ => todo!(
                            "get type of function call expression: {}",
                            sway::TabbedDisplayer(member_access)
                        ),
                    },
                }
            }

            ("StorageMap", Some(generic_parameters)) if generic_parameters.entries.len() == 2 => {
                match member_access.member.as_str() {
                    "get" if parameters.len() == 1 => Ok(generic_parameters.entries[1].type_name.to_storage_key()),

                    _ => todo!(
                        "get type of function call expression: {}",
                        sway::TabbedDisplayer(member_access)
                    ),
                }
            }

            ("String", None) => match member_access.member.as_str() {
                "as_bytes" => Ok(sway::TypeName::create_identifier("Bytes")),

                "capacity" => Ok(sway::TypeName::create_identifier("u64")),

                "clear" => Ok(sway::TypeName::create_tuple(vec![])),

                "from_ascii" => Ok(sway::TypeName::create_identifier("String")),

                "from_ascii_str" => Ok(sway::TypeName::create_identifier("String")),

                "is_empty" => Ok(sway::TypeName::create_identifier("bool")),

                "new" => Ok(sway::TypeName::create_identifier("String")),

                "with_capacity" => Ok(sway::TypeName::create_identifier("String")),

                "ptr" => Ok(sway::TypeName::create_identifier("raw_ptr")),

                "as_str" => Ok(sway::TypeName::StringSlice),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("u8", None) => match member_access.member.as_str() {
                "as_u16" => Ok(sway::TypeName::create_identifier("u16")),

                "as_u32" => Ok(sway::TypeName::create_identifier("u32")),

                "as_u64" => Ok(sway::TypeName::create_identifier("u64")),

                "as_u256" => Ok(sway::TypeName::create_identifier("u256")),

                "pow" => Ok(sway::TypeName::create_identifier("u8")),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("u16", None) => match member_access.member.as_str() {
                "as_u32" => Ok(sway::TypeName::create_identifier("u32")),

                "as_u64" => Ok(sway::TypeName::create_identifier("u64")),

                "as_u256" => Ok(sway::TypeName::create_identifier("u256")),

                "pow" => Ok(sway::TypeName::create_identifier("u16")),

                "to_be_bytes" => Ok(sway::TypeName::create_array(sway::TypeName::create_identifier("u8"), 2)),

                "to_le_bytes" => Ok(sway::TypeName::create_array(sway::TypeName::create_identifier("u8"), 2)),

                "try_as_u8" => Ok(sway::TypeName::create_generic(
                    "Option",
                    vec![sway::TypeName::create_identifier("u8")],
                )),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("u32", None) => match member_access.member.as_str() {
                "as_u64" => Ok(sway::TypeName::create_identifier("u64")),

                "as_u256" => Ok(sway::TypeName::create_identifier("u256")),

                "pow" => Ok(sway::TypeName::create_identifier("u32")),

                "to_be_bytes" => Ok(sway::TypeName::create_array(sway::TypeName::create_identifier("u8"), 4)),

                "to_le_bytes" => Ok(sway::TypeName::create_array(sway::TypeName::create_identifier("u8"), 4)),

                "try_as_u8" => Ok(sway::TypeName::create_generic(
                    "Option",
                    vec![sway::TypeName::create_identifier("u8")],
                )),

                "try_as_u16" => Ok(sway::TypeName::create_generic(
                    "Option",
                    vec![sway::TypeName::create_identifier("u16")],
                )),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("u64", None) => match member_access.member.as_str() {
                "as_u256" => Ok(sway::TypeName::create_identifier("u256")),

                "pow" => Ok(sway::TypeName::create_identifier("u64")),

                "to_be_bytes" => Ok(sway::TypeName::create_array(sway::TypeName::create_identifier("u8"), 8)),

                "to_le_bytes" => Ok(sway::TypeName::create_array(sway::TypeName::create_identifier("u8"), 8)),

                "try_as_u8" => Ok(sway::TypeName::create_identifier("u8").to_option()),

                "try_as_u16" => Ok(sway::TypeName::create_identifier("u16").to_option()),

                "try_as_u32" => Ok(sway::TypeName::create_identifier("u32").to_option()),

                "wrapping_neg" => Ok(sway::TypeName::create_identifier("I64")),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("u256", None) => match member_access.member.as_str() {
                "as_b256" => Ok(sway::TypeName::create_identifier("b256")),

                "pow" => Ok(sway::TypeName::create_identifier("u256")),

                "to_be_bytes" => Ok(sway::TypeName::create_array(
                    sway::TypeName::create_identifier("u8"),
                    32,
                )),

                "to_le_bytes" => Ok(sway::TypeName::create_array(
                    sway::TypeName::create_identifier("u8"),
                    32,
                )),

                "try_as_u8" => Ok(sway::TypeName::create_generic(
                    "Option",
                    vec![sway::TypeName::create_identifier("u8")],
                )),

                "try_as_u16" => Ok(sway::TypeName::create_generic(
                    "Option",
                    vec![sway::TypeName::create_identifier("u16")],
                )),

                "try_as_u32" => Ok(sway::TypeName::create_generic(
                    "Option",
                    vec![sway::TypeName::create_identifier("u32")],
                )),

                "try_as_u64" => Ok(sway::TypeName::create_generic(
                    "Option",
                    vec![sway::TypeName::create_identifier("u64")],
                )),

                name => {
                    // Check to see if we are using a function from the using library
                    if module
                        .borrow()
                        .using_directives
                        .iter()
                        .any(|using| using.functions.iter().any(|fnc| fnc == name))
                    {
                        return Ok(sway::TypeName::create_identifier("u256"));
                    }

                    todo!(
                        "get type of function call expression: {}",
                        sway::TabbedDisplayer(member_access)
                    )
                }
            },

            ("Vec", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => {
                match member_access.member.as_str() {
                    "get" => Ok(generic_parameters.entries[0].type_name.to_option()),

                    "len" => Ok(sway::TypeName::create_identifier("u64")),

                    _ => todo!(
                        "get type of function call expression: {}",
                        sway::TabbedDisplayer(member_access)
                    ),
                }
            }

            (name, None) => {
                todo!(
                    "get type of {name} function call member_access: {}",
                    sway::TabbedDisplayer(member_access)
                )
            }

            _ => todo!(
                "get type of {name} function call member_access: {}",
                sway::TabbedDisplayer(member_access)
            ),
        },

        sway::TypeName::StringSlice => match member_access.member.as_str() {
            "as_ptr" => Ok(sway::TypeName::create_identifier("raw_ptr")),

            "len" => Ok(sway::TypeName::create_identifier("u64")),

            _ => todo!(
                "get type of function call expression: {}",
                sway::TabbedDisplayer(member_access)
            ),
        },

        sway::TypeName::Array { .. } => match member_access.member.as_str() {
            "len" => Ok(sway::TypeName::create_identifier("u64")),

            value => todo!("{value}"),
        },

        sway::TypeName::Abi { type_name } => {
            if let sway::TypeName::Identifier { name, .. } = type_name.as_ref()
                && let Some((external_module, contract)) = project.find_module_and_contract(module.clone(), name)
            {
                let abi = contract.borrow().abi.clone();

                let parameter_types = parameters
                    .iter()
                    .map(|p| get_expression_type(project, module.clone(), scope.clone(), p))
                    .collect::<Result<Vec<_>, _>>()?;

                if let Some(result) = get_abi_function_call_type(
                    project,
                    external_module,
                    scope,
                    &abi,
                    true,
                    member_access.member.as_str(),
                    parameter_types.as_slice(),
                )? {
                    return Ok(result);
                }
            }

            todo!(
                "get type of {type_name} function call member_access: {}",
                sway::TabbedDisplayer(member_access)
            )
        }

        _ => todo!(
            "get type of {container_type} function call member_access: {}",
            sway::TabbedDisplayer(member_access)
        ),
    }
}
