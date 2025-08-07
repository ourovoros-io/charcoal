use crate::{ir, project::Project, sway, translate::*};
use std::{cell::RefCell, rc::Rc};

#[derive(Clone, Debug)]
pub enum Symbol {
    TypeDefinition(String),
    Enum(String),
    Event(String),
    Error(String),
    Struct(String),
    ValueSource(String),
}

#[derive(Clone, Debug)]
pub enum SymbolData {
    TypeDefinition(sway::TypeDefinition),
    Enum(ir::Enum),
    EventVariant {
        type_name: sway::TypeName,
        variant: sway::EnumVariant,
    },
    ErrorVariant {
        type_name: sway::TypeName,
        variant: sway::EnumVariant,
    },
    Struct(Rc<RefCell<sway::Struct>>),
    Variable(Rc<RefCell<ir::Variable>>),
    Constant(sway::Constant),
    ConfigurableField(sway::ConfigurableField),
    StorageField {
        namespace: Option<String>,
        field: sway::StorageField,
    },
    StorageStructField {
        parameter_name: String,
        field: sway::StructField,
    },
}

impl TryInto<sway::Expression> for SymbolData {
    type Error = Error;

    fn try_into(self) -> Result<sway::Expression, Self::Error> {
        match self {
            SymbolData::Variable(variable) => Ok(sway::Expression::create_identifier(
                variable.borrow().new_name.clone(),
            )),

            SymbolData::Constant(constant) => {
                Ok(sway::Expression::create_identifier(constant.name.clone()))
            }

            SymbolData::ConfigurableField(configurable_field) => Ok(
                sway::Expression::create_identifier(configurable_field.name.clone()),
            ),

            SymbolData::StorageField { namespace, field } => {
                let mut namespace_path = sway::PathExpr {
                    root: sway::PathExprRoot::Identifier("storage".into()),
                    segments: vec![],
                };

                if let Some(namespace) = namespace {
                    namespace_path.segments.push(sway::PathExprSegment {
                        name: namespace,
                        generic_parameters: None,
                    });
                }

                Ok(sway::Expression::from(sway::MemberAccess {
                    expression: sway::Expression::from(namespace_path),
                    member: field.name.clone(),
                }))
            }

            SymbolData::StorageStructField {
                parameter_name,
                field,
            } => Ok(sway::Expression::from(sway::MemberAccess {
                expression: sway::Expression::create_identifier(parameter_name),
                member: field.new_name.clone(),
            })),

            _ => panic!("Invalid symbol to expression input: {self:#?}"),
        }
    }
}

pub fn resolve_symbol(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    symbol: Symbol,
) -> Option<SymbolData> {
    match &symbol {
        Symbol::TypeDefinition(name) => {
            if let Some(type_definition) = project.find_type_definition(module.clone(), name) {
                return Some(SymbolData::TypeDefinition(type_definition));
            }
        }

        Symbol::Enum(name) => {
            if let Some(enum_definition) = project.find_enum(module.clone(), name) {
                return Some(SymbolData::Enum(enum_definition));
            }
        }

        Symbol::Event(name) => {
            if let Some(contract_name) = scope.borrow().get_contract_name() {
                let event_name = format!("{contract_name}Event");

                if let Some(event_enum) = module.borrow().events_enums.iter().find(|e| {
                    e.0.borrow().name == event_name
                        && e.0.borrow().variants.iter().any(|v| v.name == *name)
                }) {
                    let variant = event_enum
                        .0
                        .borrow()
                        .variants
                        .iter()
                        .find(|v| v.name == *name)
                        .cloned()
                        .unwrap();

                    return Some(SymbolData::EventVariant {
                        type_name: sway::TypeName::Identifier {
                            name: event_enum.0.borrow().name.clone(),
                            generic_parameters: None,
                        },
                        variant,
                    });
                }
            }
        }

        Symbol::Error(name) => {
            if let Some(contract_name) = scope.borrow().get_contract_name() {
                let error_name = format!("{contract_name}Error");

                if let Some(error_enum) = module.borrow().errors_enums.iter().find(|e| {
                    e.0.borrow().name == error_name
                        && e.0.borrow().variants.iter().any(|v| v.name == *name)
                }) {
                    let variant = error_enum
                        .0
                        .borrow()
                        .variants
                        .iter()
                        .find(|v| v.name == *name)
                        .cloned()
                        .unwrap();

                    return Some(SymbolData::ErrorVariant {
                        type_name: sway::TypeName::Identifier {
                            name: error_enum.0.borrow().name.clone(),
                            generic_parameters: None,
                        },
                        variant,
                    });
                }
            }
        }

        Symbol::Struct(name) => {
            if let Some(struct_definition) =
                project.find_struct(module.clone(), scope.clone(), name)
            {
                return Some(SymbolData::Struct(struct_definition.clone()));
            }
        }

        Symbol::ValueSource(name) => {
            // Check to see if the variable refers to a local variable or parameter defined in scope
            if let Some(variable) = scope.borrow().get_variable_from_old_name(name) {
                return Some(SymbolData::Variable(variable));
            }

            // Check to see if the variable refers to a constant
            if let Some(constant) = module
                .borrow()
                .constants
                .iter()
                .find(|c| c.old_name == *name)
            {
                return Some(SymbolData::Constant(constant.clone()));
            }

            // Check to see if the variable refers to a configurable
            if let Some(configurable) = module.borrow().configurable.as_ref()
                && let Some(field) = configurable.fields.iter().find(|f| f.old_name == *name)
            {
                return Some(SymbolData::ConfigurableField(field.clone()));
            }

            // Check to see if the current function has a storage struct parameter and the variable refers to a field of it
            if let Some(function_name) = scope.borrow().get_function_name() {
                let (parameter_name, storage_struct_name) = if let Some(function) = module
                    .borrow()
                    .functions
                    .iter()
                    .find(|f| {
                        let sway::TypeName::Function { new_name, .. } = &f.signature else {
                            unreachable!()
                        };
                        function_name == *new_name
                    })
                    .cloned()
                    && let sway::TypeName::Function {
                        storage_struct_parameter: Some(storage_struct_parameter),
                        ..
                    } = &function.signature
                {
                    (
                        storage_struct_parameter.name.clone(),
                        storage_struct_parameter
                            .type_name
                            .as_ref()
                            .unwrap()
                            .to_string(),
                    )
                } else {
                    let contract_name = scope.borrow().get_contract_name().unwrap();
                    ("storage_struct".into(), format!("{contract_name}Storage"))
                };

                if let Some(SymbolData::Struct(storage_struct)) = resolve_symbol(
                    project,
                    module.clone(),
                    scope.clone(),
                    Symbol::Struct(storage_struct_name),
                ) && let Some(field) = storage_struct
                    .borrow()
                    .fields
                    .iter()
                    .find(|f| f.old_name == *name)
                {
                    return Some(SymbolData::StorageStructField {
                        parameter_name,
                        field: field.clone(),
                    });
                }
            }

            // Check to see if the variable refers to a storage field
            if let Some(contract_name) = scope.borrow().get_contract_name()
                && let Some(contract) =
                    project.find_contract(module.clone(), contract_name.as_str())
            {
                let storage_namespace_name = contract.borrow().name.to_case(Case::Snake);
                let storage_field_name = name.clone();

                if let Some(storage) = contract.borrow().storage.as_ref()
                    && let Some(storage_namespace) = storage
                        .borrow()
                        .namespaces
                        .iter()
                        .find(|n| n.borrow().name == storage_namespace_name)
                    && let Some(field) = storage_namespace
                        .borrow()
                        .fields
                        .iter()
                        .find(|f| f.old_name == storage_field_name)
                {
                    return Some(SymbolData::StorageField {
                        namespace: Some(storage_namespace_name),
                        field: field.clone(),
                    });
                }
            }
        }
    }

    // If we didn't find it in the current contract, try checking inherited contracts
    if let Some(contract_name) = scope.borrow().get_contract_name()
        && let Some(contract) = project.find_contract(module.clone(), &contract_name)
    {
        let inherits = contract.borrow().abi.inherits.clone();

        for inherit in inherits {
            let inherit_type_name = inherit.to_string();
            let scope = Rc::new(RefCell::new(ir::Scope::new(
                Some(inherit_type_name.as_str()),
                None,
                Some(scope.clone()),
            )));

            if let Some(result) =
                resolve_symbol(project, module.clone(), scope.clone(), symbol.clone())
            {
                return Some(result);
            }
        }
    }

    // If we didn't find it in the current module, try checking imported modules
    for use_expr in module.borrow().uses.iter() {
        if let Some(imported_module) = project.resolve_use(use_expr)
            && let Some(symbol) = resolve_symbol(
                project,
                imported_module.clone(),
                scope.clone(),
                symbol.clone(),
            )
        {
            return Some(symbol);
        }
    }

    None
}

pub fn resolve_abi_function_call(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    abi: &sway::Abi,
    contract_id: Option<&sway::Expression>,
    function_name: &str,
    named_arguments: Option<&[solidity::NamedArgument]>,
    mut parameters: Vec<sway::Expression>,
    mut parameter_types: Vec<sway::TypeName>,
) -> Result<Option<sway::Expression>, Error> {
    let functions = match contract_id {
        Some(_) => abi.functions.clone(),
        None => module
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
                    storage_struct_parameter: storage_struct_parameter
                        .as_ref()
                        .map(|x| x.as_ref().clone()),
                    return_type: return_type.as_ref().map(|x| x.as_ref().clone()),
                    body: None,
                }
            })
            .collect(),
    };

    let mut function = None;

    if let Some(named_arguments) = named_arguments {
        let mut named_parameters = vec![];

        for arg in named_arguments {
            named_parameters.push((
                translate_naming_convention(&arg.name.name, Case::Snake),
                translate_expression(project, module.clone(), scope.clone(), &arg.expr)?,
            ));
        }

        if let Some(abi_function) = functions.iter().find(|f| {
            if f.old_name != function_name {
                return false;
            }

            if f.parameters.entries.len() != named_parameters.len() {
                return false;
            }

            f.parameters
                .entries
                .iter()
                .all(|p| named_parameters.iter().any(|(name, _)| p.name == *name))
        }) {
            let function_parameters = &abi_function.parameters;

            parameters.clear();
            parameter_types.clear();

            for parameter in function_parameters.entries.iter() {
                let arg = named_arguments
                    .iter()
                    .find(|a| {
                        let new_name = translate_naming_convention(&a.name.name, Case::Snake);
                        new_name == parameter.name
                    })
                    .unwrap();

                let parameter =
                    translate_expression(project, module.clone(), scope.clone(), &arg.expr)?;
                let parameter_type =
                    get_expression_type(project, module.clone(), scope.clone(), &parameter)?;

                parameters.push(parameter);
                parameter_types.push(parameter_type);
            }

            function = Some(abi_function);
        }
    }

    let parameters_cell = Rc::new(RefCell::new(parameters));

    if function.is_none() {
        function = functions.iter().find(|function| {
            let function_parameters = &function.parameters;
            let mut parameters = parameters_cell.borrow_mut();

            // Ensure the function's old name matches the function call we're translating
            if function.old_name != function_name {
                return false;
            }

            // Ensure the supplied function call args match the function's parameters
            if parameters.len() != function_parameters.entries.len() {
                return false;
            }

            for (i, value_type_name) in parameter_types.iter().enumerate() {
                let Some(parameter_type_name) = function_parameters.entries[i].type_name.as_ref()
                else {
                    continue;
                };

                let Some(expr) = coerce_expression(
                    project,
                    module.clone(),
                    scope.clone(),
                    &parameters[i],
                    value_type_name,
                    parameter_type_name,
                ) else {
                    return false;
                };

                parameters[i] = expr;
            }

            true
        });
    }

    let Some(function) = function else {
        // If we didn't find a function, check inherited functions
        for inherit in abi.inherits.iter() {
            if let Some((module, contract)) =
                project.find_module_and_contract(module.clone(), inherit.to_string().as_str())
            {
                let abi = contract.borrow().abi.clone();

                let scope = Rc::new(RefCell::new(ir::Scope::new(
                    Some(inherit.to_string().as_str()),
                    None,
                    Some(scope.clone()),
                )));

                if let Some(result) = resolve_abi_function_call(
                    project,
                    module.clone(),
                    scope.clone(),
                    &abi,
                    contract_id,
                    function_name,
                    named_arguments,
                    parameters_cell.borrow().clone(),
                    parameter_types.clone(),
                )? {
                    return Ok(Some(result));
                }
            }
        }

        return Ok(None);
    };

    let parameters = parameters_cell.borrow().clone();

    let function_storage_access = module
        .borrow_mut()
        .function_storage_accesses
        .get(&function.new_name)
        .cloned();

    if let Some(function_storage_access) = function_storage_access
        && let Some(current_function_name) = scope.borrow().get_function_name()
    {
        let mut module = module.borrow_mut();
        let current_function_storage_access = module
            .function_storage_accesses
            .entry(current_function_name)
            .or_default();

        *current_function_storage_access = function_storage_access;
    }

    match contract_id {
        Some(contract_id) => Ok(Some(sway::Expression::create_function_calls(
            Some(contract_id.clone()),
            &[(function.new_name.as_str(), Some((None, parameters)))],
        ))),

        None => Ok(Some(sway::Expression::create_function_calls(
            None,
            &[(function.new_name.as_str(), Some((None, parameters)))],
        ))),
    }
}

#[inline]
pub fn resolve_function_call(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    function_name: &str,
    named_arguments: Option<&[solidity::NamedArgument]>,
    mut parameters: Vec<sway::Expression>,
    mut parameter_types: Vec<sway::TypeName>,
) -> Result<Option<sway::Expression>, Error> {
    let mut function = None;

    if let Some(named_arguments) = named_arguments {
        let mut named_parameters = vec![];

        for arg in named_arguments {
            named_parameters.push((
                translate_naming_convention(&arg.name.name, Case::Snake),
                translate_expression(project, module.clone(), scope.clone(), &arg.expr)?,
            ));
        }

        let functions = module.borrow().functions.clone();

        if let Some(f) = functions.iter().find(|f| {
            let sway::TypeName::Function {
                old_name: f_old_name,
                parameters: f_parameters,
                ..
            } = &f.signature
            else {
                unreachable!()
            };

            if f_old_name != function_name {
                return false;
            }

            if f_parameters.entries.len() != named_parameters.len() {
                return false;
            }

            if !f_parameters
                .entries
                .iter()
                .all(|p| named_parameters.iter().any(|(name, _)| p.name == *name))
            {
                return false;
            }

            true
        }) {
            let sway::TypeName::Function {
                parameters: f_parameters,
                ..
            } = &f.signature
            else {
                unreachable!()
            };

            parameters.clear();
            parameter_types.clear();

            for parameter in f_parameters.entries.iter() {
                let arg = named_arguments
                    .iter()
                    .find(|a| {
                        let new_name = translate_naming_convention(&a.name.name, Case::Snake);
                        new_name == parameter.name
                    })
                    .unwrap();

                let mut value =
                    translate_expression(project, module.clone(), scope.clone(), &arg.expr)?;

                let mut value_type =
                    get_expression_type(project, module.clone(), scope.clone(), &value)?;

                if let Some(parameter_type_name) = parameter.type_name.as_ref() {
                    value = coerce_expression(
                        project,
                        module.clone(),
                        scope.clone(),
                        &value,
                        &value_type,
                        parameter_type_name,
                    )
                    .unwrap();

                    value_type = parameter_type_name.clone();
                }

                parameters.push(value);
                parameter_types.push(value_type);
            }

            function = Some(f.clone());
        }
    }

    let parameters_cell = Rc::new(RefCell::new(parameters));

    let mut check_type_name = |type_name: &sway::TypeName| -> bool {
        let sway::TypeName::Function {
            parameters: function_parameters,
            ..
        } = type_name
        else {
            unreachable!()
        };

        let mut parameters = parameters_cell.borrow_mut();

        // Ensure the supplied function call args match the function's parameters
        if parameters.len() != function_parameters.entries.len() {
            return false;
        }

        for (i, value_type_name) in parameter_types.iter().enumerate() {
            let Some(parameter_type_name) = function_parameters.entries[i].type_name.as_ref()
            else {
                continue;
            };

            let Some(expr) = coerce_expression(
                project,
                module.clone(),
                scope.clone(),
                &parameters[i],
                value_type_name,
                parameter_type_name,
            ) else {
                return false;
            };

            parameters[i] = expr;
        }

        true
    };

    // Check to see if the function is defined in the current module
    if function.is_none()
        && let Some(f) = module.borrow().functions.iter().find(|f| {
            let sway::TypeName::Function { old_name, .. } = &f.signature else {
                unreachable!()
            };

            if old_name != function_name {
                return false;
            }

            check_type_name(&f.signature)
        })
    {
        function = Some(f.clone());
    }

    // Check to see if the function is a local variable function pointer in scope
    if function.is_none()
        && let Some(variable) = scope.borrow().get_variable_from_old_name(function_name)
        && check_type_name(&variable.borrow().type_name)
    {
        return Ok(Some(sway::Expression::create_identifier(
            function_name.to_string(),
        )));
    }

    let Some(function) = function else {
        // If we didn't find a function, check inherited functions
        if let Some(contract_name) = scope.borrow().get_contract_name()
            && let Some(contract) = project.find_contract(module.clone(), &contract_name)
        {
            let abi = contract.borrow().abi.clone();

            if let Some(result) = resolve_abi_function_call(
                project,
                module.clone(),
                scope.clone(),
                &abi,
                None,
                function_name,
                named_arguments,
                parameters_cell.borrow().clone(),
                parameter_types,
            )? {
                return Ok(Some(result));
            }
        }

        return Ok(None);
    };

    let sway::TypeName::Function { new_name, .. } = &function.signature else {
        unreachable!()
    };

    let function_storage_access = module
        .borrow_mut()
        .function_storage_accesses
        .get(new_name)
        .cloned();

    let current_function_name = scope.borrow().get_function_name();
    if let Some(function_storage_access) = function_storage_access
        && let Some(current_function_name) = current_function_name
    {
        let mut module = module.borrow_mut();
        let current_function_storage_access = module
            .function_storage_accesses
            .entry(current_function_name)
            .or_default();

        *current_function_storage_access = function_storage_access;
    }

    Ok(Some(sway::Expression::create_function_calls(
        None,
        &[(new_name, Some((None, parameters_cell.borrow().clone())))],
    )))
}

#[inline]
pub fn resolve_modifier(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    modifier_name: &str,
    named_arguments: Option<&[solidity::NamedArgument]>,
    mut parameters: Vec<sway::Expression>,
    mut parameter_types: Vec<sway::TypeName>,
) -> Result<
    Option<(
        ir::Item<Rc<RefCell<ir::Modifier>>>,
        Vec<sway::Expression>,
        Vec<sway::TypeName>,
    )>,
    Error,
> {
    let mut modifier = None;

    if let Some(named_arguments) = named_arguments {
        let mut named_parameters = vec![];

        for arg in named_arguments {
            named_parameters.push((
                translate_naming_convention(&arg.name.name, Case::Snake),
                translate_expression(project, module.clone(), scope.clone(), &arg.expr)?,
            ));
        }

        let modifiers = module.borrow().modifiers.clone();

        if let Some(f) = modifiers.iter().find(|f| {
            let sway::TypeName::Function {
                old_name,
                parameters,
                ..
            } = &f.signature
            else {
                unreachable!()
            };

            if old_name != modifier_name {
                return false;
            }

            if parameters.entries.len() != named_parameters.len() {
                return false;
            }

            if !parameters
                .entries
                .iter()
                .all(|p| named_parameters.iter().any(|(name, _)| p.name == *name))
            {
                return false;
            }

            true
        }) {
            {
                let sway::TypeName::Function {
                    parameters: f_parameters,
                    ..
                } = &f.signature
                else {
                    unreachable!()
                };

                parameters.clear();
                parameter_types.clear();

                for parameter in f_parameters.entries.iter() {
                    let arg = named_arguments
                        .iter()
                        .find(|a| {
                            let new_name = translate_naming_convention(&a.name.name, Case::Snake);
                            new_name == parameter.name
                        })
                        .unwrap();

                    let mut value =
                        translate_expression(project, module.clone(), scope.clone(), &arg.expr)?;

                    let mut value_type =
                        get_expression_type(project, module.clone(), scope.clone(), &value)?;

                    if let Some(parameter_type_name) = parameter.type_name.as_ref() {
                        value = coerce_expression(
                            project,
                            module.clone(),
                            scope.clone(),
                            &value,
                            &value_type,
                            parameter_type_name,
                        )
                        .unwrap();

                        value_type = parameter_type_name.clone();
                    }

                    parameters.push(value);
                    parameter_types.push(value_type);
                }
            }

            modifier = Some(f.clone());
        }
    }

    let parameters_cell = Rc::new(RefCell::new(parameters));

    let mut check_parameters = |modifier: Rc<RefCell<ir::Modifier>>,
                                storage_struct_parameter: Option<&sway::Parameter>|
     -> bool {
        let modifier_parameters = modifier.borrow().parameters.clone();

        let mut parameters = parameters_cell.borrow_mut();

        let modifier_parameter_count = modifier_parameters.entries.len()
            + if storage_struct_parameter.is_some() {
                1
            } else {
                0
            };

        // Ensure the supplied modifier call args match the modifier's parameters
        if parameters.len() != modifier_parameter_count {
            return false;
        }

        if let Some(storage_struct_parameter) = storage_struct_parameter {
            if !storage_struct_parameter
                .type_name
                .as_ref()
                .unwrap()
                .is_compatible_with(parameter_types.last().unwrap())
            {
                return false;
            }
        }

        for (i, parameter) in modifier_parameters.entries.iter().enumerate() {
            let Some(parameter_type_name) = parameter.type_name.as_ref() else {
                continue;
            };

            let value_type_name = &parameter_types[i];

            let Some(expr) = coerce_expression(
                project,
                module.clone(),
                scope.clone(),
                &parameters[i],
                value_type_name,
                parameter_type_name,
            ) else {
                return false;
            };

            parameters[i] = expr;
        }

        true
    };

    // Check to see if the modifier is defined in the current module
    if modifier.is_none()
        && let Some(f) = module.borrow().modifiers.iter().find(|modifier| {
            let sway::TypeName::Function {
                new_name: f_new_name,
                ..
            } = &modifier.signature
            else {
                unreachable!()
            };

            if f_new_name != modifier_name {
                return false;
            }

            if modifier.implementation.is_none() {
                return false;
            }

            let sway::TypeName::Function {
                storage_struct_parameter,
                ..
            } = &modifier.signature
            else {
                unreachable!()
            };

            check_parameters(
                modifier.implementation.clone().unwrap(),
                if storage_struct_parameter.is_some() {
                    storage_struct_parameter.as_deref()
                } else {
                    None
                },
            )
        })
    {
        modifier = Some(f.clone());
    }

    if modifier.is_none() {
        // If we didn't find a modifier, check inherited modifiers
        let contract_name = scope.borrow().get_contract_name();
        if let Some(contract_name) = contract_name
            && let Some((module, contract)) =
                project.find_module_and_contract(module.clone(), &contract_name)
        {
            let storage_struct_parameter = {
                let current_function_name = scope.borrow().get_function_name().unwrap();

                let module = module.borrow();
                let current_function = module.functions.iter().find(|f| {
                    let sway::TypeName::Function { new_name, .. } = &f.signature else {
                        unreachable!()
                    };
                    *new_name == current_function_name
                });

                current_function
                    .as_ref()
                    .map(|c| {
                        let sway::TypeName::Function {
                            storage_struct_parameter,
                            ..
                        } = &c.signature
                        else {
                            unreachable!()
                        };
                        storage_struct_parameter.clone()
                    })
                    .flatten()
            };

            for contract_name in contract.borrow().abi.inherits.iter() {
                let Some(module) = project
                    .find_module_containing_contract(module.clone(), &contract_name.to_string())
                else {
                    panic!("Failed to find module with contract `{contract_name}`")
                };

                let mut parameters = parameters_cell.borrow().clone();
                let mut parameter_types = parameter_types.clone();

                let inherited_field_name = contract_name.to_string().to_case(Case::Snake);
                let inherited_storage_struct_name = format!("{contract_name}Storage");

                let modifier_storage_struct_parameter = module
                    .borrow()
                    .modifiers
                    .iter()
                    .find(|m| {
                        let sway::TypeName::Function { new_name, .. } = &m.signature else {
                            unreachable!()
                        };
                        new_name == modifier_name
                    })
                    .map(|m| {
                        let sway::TypeName::Function {
                            storage_struct_parameter,
                            ..
                        } = &m.signature
                        else {
                            unreachable!()
                        };
                        storage_struct_parameter.clone()
                    })
                    .flatten();

                if modifier_storage_struct_parameter.is_some()
                    && storage_struct_parameter.is_some()
                    && !parameters.is_empty()
                {
                    *parameters.last_mut().unwrap() = sway::Expression::from(sway::MemberAccess {
                        expression: parameters.last().unwrap().clone(),
                        member: inherited_field_name,
                    });

                    *parameter_types.last_mut().unwrap() = sway::TypeName::Identifier {
                        name: inherited_storage_struct_name,
                        generic_parameters: None,
                    };
                }

                let scope = Rc::new(RefCell::new(ir::Scope::new(
                    Some(&contract_name.to_string()),
                    None,
                    Some(scope.clone()),
                )));

                if let Some(modifier) = resolve_modifier(
                    project,
                    module.clone(),
                    scope.clone(),
                    modifier_name,
                    named_arguments,
                    parameters,
                    parameter_types,
                )? {
                    return Ok(Some(modifier));
                }
            }
        }
    }

    Ok(modifier.map(|m| (m, parameters_cell.borrow().clone(), parameter_types)))
}

#[inline]
pub fn resolve_struct_constructor(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    structs: &[ir::Item<Rc<RefCell<sway::Struct>>>],
    struct_name: &str,
    named_arguments: Option<&[solidity::NamedArgument]>,
    mut parameters: Vec<sway::Expression>,
    mut parameter_types: Vec<sway::TypeName>,
) -> Result<Option<sway::Expression>, Error> {
    let Some(struct_definition) = structs
        .iter()
        .find(|s| s.signature.to_string() == struct_name)
    else {
        return Ok(None);
    };

    if parameters.len()
        != struct_definition
            .implementation
            .as_ref()
            .unwrap()
            .borrow()
            .fields
            .len()
    {
        let Some(named_arguments) = named_arguments else {
            return Ok(None);
        };

        if named_arguments.len()
            != struct_definition
                .implementation
                .as_ref()
                .unwrap()
                .borrow()
                .fields
                .len()
        {
            return Ok(None);
        }

        parameters = vec![];
        parameter_types = vec![];

        for field in struct_definition
            .implementation
            .as_ref()
            .unwrap()
            .borrow()
            .fields
            .iter()
        {
            let arg = named_arguments
                .iter()
                .find(|a| {
                    let new_name = translate_naming_convention(&a.name.name, Case::Snake);
                    new_name == field.new_name
                })
                .unwrap();

            let parameter =
                translate_expression(project, module.clone(), scope.clone(), &arg.expr)?;

            let parameter_type =
                get_expression_type(project, module.clone(), scope.clone(), &parameter)?;

            parameters.push(parameter);
            parameter_types.push(parameter_type);
        }
    }

    // Attempt to coerce each parameter value to the struct's field type
    for ((parameter, parameter_type), field) in
        parameters.iter_mut().zip(parameter_types.iter_mut()).zip(
            struct_definition
                .implementation
                .as_ref()
                .unwrap()
                .borrow()
                .fields
                .iter(),
        )
    {
        match coerce_expression(
            project,
            module.clone(),
            scope.clone(),
            parameter,
            parameter_type,
            &field.type_name,
        ) {
            Some(expression) => *parameter = expression,
            None => return Ok(None),
        }
    }

    Ok(Some(sway::Expression::from(sway::Constructor {
        type_name: sway::TypeName::Identifier {
            name: struct_definition
                .implementation
                .as_ref()
                .unwrap()
                .borrow()
                .name
                .clone(),
            generic_parameters: None,
        },

        fields: struct_definition
            .implementation
            .as_ref()
            .unwrap()
            .borrow()
            .fields
            .iter()
            .zip(parameters.iter())
            .map(|(field, value)| sway::ConstructorField {
                name: field.new_name.clone(),
                value: value.clone(),
            })
            .collect(),
    })))
}
