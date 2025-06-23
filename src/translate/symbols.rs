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

            _ => Err(Error::Wrapped(Box::new(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                format!("Invalid symbol to expression input: {self:#?}"),
            )))),
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
            if let Some(struct_definition) = project.find_struct(module.clone(), name) {
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
            if let Some(configurable) = module.borrow().configurable.as_ref() {
                if let Some(field) = configurable.fields.iter().find(|f| f.old_name == *name) {
                    return Some(SymbolData::ConfigurableField(field.clone()));
                }
            }

            // Check to see if the variable refers to a storage field
            if let Some(contract_name) = scope.borrow().get_contract_name() {
                if let Some(contract) =
                    project.find_contract(module.clone(), contract_name.as_str())
                {
                    let storage_namespace_name = contract.borrow().name.to_case(Case::Snake);
                    let storage_field_name = name.clone();

                    if let Some(storage) = contract.borrow().storage.as_ref() {
                        if let Some(storage_namespace) = storage
                            .borrow()
                            .namespaces
                            .iter()
                            .find(|n| n.borrow().name == storage_namespace_name)
                        {
                            if let Some(field) = storage_namespace
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
            }
        }
    }

    // If we didn't find it in the current contract, try checking inheritted contracts
    if let Some(contract_name) = scope.borrow().get_contract_name() {
        if let Some(contract) = project.find_contract(module.clone(), &contract_name) {
            let inherits = contract.borrow().abi.inherits.clone();

            for inherit in inherits {
                let inherit_type_name = inherit.to_string();
                let scope = Rc::new(RefCell::new(ir::Scope::new(
                    Some(&inherit_type_name.as_str()),
                    Some(scope.clone()),
                )));

                if let Some(result) =
                    resolve_symbol(project, module.clone(), scope.clone(), symbol.clone())
                {
                    return Some(result);
                }
            }
        }
    }

    // If we didn't find it in the current module, try checking imported modules
    for use_expr in module.borrow().uses.iter() {
        if let Some(imported_module) = project.resolve_use(use_expr) {
            if let Some(symbol) = resolve_symbol(
                project,
                imported_module.clone(),
                scope.clone(),
                symbol.clone(),
            ) {
                return Some(symbol);
            }
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
                    return_type,
                } = &f.signature
                else {
                    unreachable!()
                };
                sway::Function {
                    attributes: None,
                    is_public: false,
                    old_name: old_name.clone(),
                    name: new_name.clone(),
                    generic_parameters: generic_parameters.clone(),
                    parameters: parameters.clone(),
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

                // If `parameter_type_name` is `Identity`, but `container` is an abi cast expression,
                // then we need to de-cast it, so `container` turns into the 2nd parameter of the abi cast,
                // and `value_type_name` turns into `Identity`.
                if parameter_type_name.is_identity() {
                    if let sway::Expression::FunctionCall(function_call) = parameters[i].clone() {
                        if let Some(function_name) = function_call.function.as_identifier() {
                            if function_name == "abi" {
                                parameters[i] = function_call.parameters[1].clone();
                                continue;
                            }
                        }
                    }
                }

                // HACK: str -> Bytes (is this still necessary?)
                // Bytes::from(raw_slice::from_parts::<u8>(s.as_ptr(), s.len()))
                if parameter_type_name.is_bytes() && value_type_name.is_string_slice() {
                    parameters[i] = sway::Expression::create_function_calls(
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
                                                    Some(parameters[i].clone()),
                                                    &[("as_ptr", Some((None, vec![])))],
                                                ),
                                                sway::Expression::create_function_calls(
                                                    Some(parameters[i].clone()),
                                                    &[("len", Some((None, vec![])))],
                                                ),
                                            ],
                                        )),
                                    )],
                                )],
                            )),
                        )],
                    );
                    continue;
                }

                // HACK: StorageKey<*> -> *
                if let Some(value_type) = value_type_name.storage_key_type() {
                    if !parameter_type_name.is_storage_key()
                        && parameter_type_name.is_compatible_with(&value_type)
                    {
                        parameters[i] = sway::Expression::create_function_calls(
                            Some(parameters[i].clone()),
                            &[("read", Some((None, vec![])))],
                        );
                        continue;
                    }
                }

                // HACK: [u8; 32] -> b256
                if let Some(32) = value_type_name.u8_array_length() {
                    if parameter_type_name.is_b256() {
                        parameters[i] = sway::Expression::create_function_calls(
                            None,
                            &[(
                                "b256::from_be_bytes",
                                Some((None, vec![parameters[i].clone()])),
                            )],
                        );
                        continue;
                    }
                }

                // HACK: [u*; N]
                if let sway::TypeName::Array {
                    type_name: value_element_type,
                    length: value_element_length,
                } = &value_type_name
                {
                    if let sway::TypeName::Array {
                        type_name: parameter_element_type,
                        length: parameter_element_length,
                    } = parameter_type_name
                    {
                        if value_element_length != parameter_element_length {
                            return false;
                        }

                        if value_element_type.is_uint() && parameter_element_type.is_uint() {
                            return true;
                        }

                        if value_element_type.is_compatible_with(parameter_element_type) {
                            return true;
                        }
                    }
                }

                if let Some(expr) = coerce_expression(
                    project,
                    module.clone(),
                    scope.clone(),
                    &parameters[i],
                    value_type_name,
                    parameter_type_name,
                ) {
                    parameters[i] = expr;
                    continue;
                }

                if !value_type_name.is_compatible_with(parameter_type_name) {
                    return false;
                }
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

    match contract_id {
        Some(contract_id) => Ok(Some(sway::Expression::create_function_calls(
            Some(contract_id.clone()),
            &[(function.name.as_str(), Some((None, parameters)))],
        ))),

        None => Ok(Some(sway::Expression::create_function_calls(
            None,
            &[(function.name.as_str(), Some((None, parameters)))],
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

            f_parameters
                .entries
                .iter()
                .all(|p| named_parameters.iter().any(|(name, _)| p.name == *name))
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

                let parameter =
                    translate_expression(project, module.clone(), scope.clone(), &arg.expr)?;

                let parameter_type =
                    get_expression_type(project, module.clone(), scope.clone(), &parameter)?;

                parameters.push(parameter);
                parameter_types.push(parameter_type);
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

            // If `parameter_type_name` is `Identity`, but `container` is an abi cast expression,
            // then we need to de-cast it, so `container` turns into the 2nd parameter of the abi cast,
            // and `value_type_name` turns into `Identity`.
            if parameter_type_name.is_identity() {
                if let sway::Expression::FunctionCall(function_call) = parameters[i].clone() {
                    if let Some(function_name) = function_call.function.as_identifier() {
                        if function_name == "abi" {
                            parameters[i] = function_call.parameters[1].clone();
                            continue;
                        }
                    }
                }
            }

            // HACK: str -> Bytes (is this still necessary?)
            // Bytes::from(raw_slice::from_parts::<u8>(s.as_ptr(), s.len()))
            if parameter_type_name.is_bytes() && value_type_name.is_string_slice() {
                parameters[i] = sway::Expression::create_function_calls(
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
                                                Some(parameters[i].clone()),
                                                &[("as_ptr", Some((None, vec![])))],
                                            ),
                                            sway::Expression::create_function_calls(
                                                Some(parameters[i].clone()),
                                                &[("len", Some((None, vec![])))],
                                            ),
                                        ],
                                    )),
                                )],
                            )],
                        )),
                    )],
                );
                continue;
            }

            // HACK: StorageKey<*> -> *
            if let Some(value_type) = value_type_name.storage_key_type() {
                if !parameter_type_name.is_storage_key()
                    && parameter_type_name.is_compatible_with(&value_type)
                {
                    parameters[i] = sway::Expression::create_function_calls(
                        Some(parameters[i].clone()),
                        &[("read", Some((None, vec![])))],
                    );
                    continue;
                }
            }

            // HACK: [u8; 32] -> b256
            if let Some(32) = value_type_name.u8_array_length() {
                if parameter_type_name.is_b256() {
                    parameters[i] = sway::Expression::create_function_calls(
                        None,
                        &[(
                            "b256::from_be_bytes",
                            Some((None, vec![parameters[i].clone()])),
                        )],
                    );
                    continue;
                }
            }

            // HACK: [u*; N]
            if let sway::TypeName::Array {
                type_name: value_element_type,
                length: value_element_length,
            } = &value_type_name
            {
                if let sway::TypeName::Array {
                    type_name: parameter_element_type,
                    length: parameter_element_length,
                } = &parameter_type_name
                {
                    if value_element_length != parameter_element_length {
                        return false;
                    }

                    if value_element_type.is_uint() && parameter_element_type.is_uint() {
                        return true;
                    }

                    if value_element_type.is_compatible_with(parameter_element_type) {
                        return true;
                    }
                }
            }

            if let Some(expr) = coerce_expression(
                project,
                module.clone(),
                scope.clone(),
                &parameters[i],
                value_type_name,
                &parameter_type_name,
            ) {
                parameters[i] = expr;
                continue;
            }

            if !value_type_name.is_compatible_with(&parameter_type_name) {
                return false;
            }
        }

        true
    };

    // Check to see if the function is defined in the current module
    if function.is_none() {
        if let Some(f) = module.borrow().functions.iter().find(|f| {
            let sway::TypeName::Function { old_name, .. } = &f.signature else {
                unreachable!()
            };

            if old_name != function_name {
                return false;
            }

            check_type_name(&f.signature)
        }) {
            function = Some(f.clone());
        }
    }

    // Check to see if the function is a local variable function pointer in scope
    if function.is_none() {
        if let Some(variable) = scope.borrow().get_variable_from_old_name(function_name) {
            if check_type_name(&variable.borrow().type_name) {
                return Ok(Some(sway::Expression::create_identifier(
                    function_name.to_string(),
                )));
            }
        }
    }

    let Some(function) = function else {
        // If we didn't find a function, check inherited functions
        if let Some(contract_name) = scope.borrow().get_contract_name() {
            if let Some(contract) = project.find_contract(module.clone(), &contract_name) {
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
        }

        return Ok(None);
    };

    let sway::TypeName::Function { new_name, .. } = &function.signature else {
        unreachable!()
    };

    Ok(Some(sway::Expression::create_function_calls(
        None,
        &[(&new_name, Some((None, parameters_cell.borrow().clone())))],
    )))
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
        if let Some(named_arguments) = named_arguments {
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
            } else {
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
                            new_name == field.name
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
        } else {
            return Ok(None);
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
                name: field.name.clone(),
                value: value.clone(),
            })
            .collect(),
    })))
}
