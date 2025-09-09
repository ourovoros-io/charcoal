use crate::{error::Error, ir::FunctionNames, project::Project, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[derive(Clone, Debug, PartialEq)]
struct FunctionAttributes {
    is_constructor: bool,
    is_fallback: bool,
    is_receive: bool,
    is_public: bool,
    is_constant: bool,
    is_pure: bool,
    is_view: bool,
    is_payable: bool,
    is_virtual: bool,
    is_override: bool,
}

impl From<&solidity::FunctionDefinition> for FunctionAttributes {
    fn from(function_definition: &solidity::FunctionDefinition) -> Self {
        // Collect information about the function from its type
        let is_constructor = matches!(function_definition.ty, solidity::FunctionTy::Constructor);
        let is_fallback = matches!(function_definition.ty, solidity::FunctionTy::Fallback);
        let is_receive = matches!(function_definition.ty, solidity::FunctionTy::Receive);

        // Collect information about the function from its attributes
        let mut is_public = false;
        let mut is_constant = false;
        let mut is_pure = false;
        let mut is_view = false;
        let mut is_payable = false;
        let mut is_virtual = false;
        let mut is_override = false;

        for attribute in function_definition.attributes.iter() {
            match attribute {
                solidity::FunctionAttribute::Visibility(
                    solidity::Visibility::External(_) | solidity::Visibility::Public(_),
                ) => is_public = true,

                solidity::FunctionAttribute::Mutability(solidity::Mutability::Constant(_)) => {
                    is_constant = true;
                }

                solidity::FunctionAttribute::Mutability(solidity::Mutability::Pure(_)) => {
                    is_pure = true;
                }

                solidity::FunctionAttribute::Mutability(solidity::Mutability::View(_)) => {
                    is_view = true;
                }

                solidity::FunctionAttribute::Mutability(solidity::Mutability::Payable(_)) => {
                    is_payable = true;
                }

                solidity::FunctionAttribute::Virtual(_) => is_virtual = true,

                solidity::FunctionAttribute::Override(_, _) => is_override = true,

                //
                // TODO: consider checking these for additional things we may be missing...
                //
                // solidity::FunctionAttribute::Visibility(visibility) => todo!(),
                // solidity::FunctionAttribute::Mutability(mutability) => todo!(),
                // solidity::FunctionAttribute::Virtual(loc) => todo!(),
                // solidity::FunctionAttribute::Immutable(loc) => todo!(),
                // solidity::FunctionAttribute::Override(loc, identifier_paths) => todo!(),
                // solidity::FunctionAttribute::BaseOrModifier(loc, base) => todo!(),
                // solidity::FunctionAttribute::Error(loc) => todo!(),
                //
                _ => {}
            }
        }

        // If the function is a constructor, we consider it public and add an initializer requirement
        if is_constructor {
            is_public = true;
        }

        Self {
            is_constructor,
            is_fallback,
            is_receive,
            is_public,
            is_constant,
            is_pure,
            is_view,
            is_payable,
            is_virtual,
            is_override,
        }
    }
}

impl Into<Option<sway::AttributeList>> for FunctionAttributes {
    fn into(self) -> Option<sway::AttributeList> {
        if self.is_constant || self.is_pure {
            None
        } else {
            let mut attributes = vec![];

            attributes.push(sway::Attribute {
                name: "storage".into(),
                parameters: Some(if self.is_view {
                    vec!["read".into()]
                } else {
                    vec!["read".into(), "write".into()]
                }),
            });

            if self.is_payable && !self.is_fallback {
                attributes.push(sway::Attribute {
                    name: "payable".into(),
                    parameters: None,
                });
            }

            if self.is_fallback {
                attributes.push(sway::Attribute {
                    name: "fallback".into(),
                    parameters: None,
                });
            }

            Some(sway::AttributeList { attributes })
        }
    }
}

#[inline]
pub fn translate_function_name(
    _project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    contract_name: Option<&str>,
    function_name: Option<&str>,
    parameters: &solidity::ParameterList,
    function_ty: &solidity::FunctionTy,
) -> FunctionNames {
    // Generate the function signature
    let mut signature = function_name
        .unwrap_or_else(|| match function_ty {
            solidity::FunctionTy::Constructor => "constructor",
            solidity::FunctionTy::Fallback => "fallback",
            solidity::FunctionTy::Receive => "receive",
            _ => todo!(),
        })
        .to_string();

    let old_name = signature.clone();

    if let Some(contract_name) = contract_name {
        signature = format!("{contract_name}.{signature}");
    }

    signature.push('(');

    for (i, (_, parameter)) in parameters.iter().enumerate() {
        signature = format!(
            "{signature}{}{}",
            if i > 0 { "," } else { "" },
            parameter.as_ref().unwrap().ty,
        );
    }

    signature.push(')');

    // Add the translated function name to the function names mapping if we haven't already
    let (function_name_counts, function_names) = match contract_name.as_ref() {
        Some(contract_name) => {
            let function_name_counts = module
                .borrow_mut()
                .contract_function_name_counts
                .entry(contract_name.to_string())
                .or_default()
                .clone();

            let function_names = module
                .borrow_mut()
                .contract_function_names
                .entry(contract_name.to_string())
                .or_default()
                .clone();

            (function_name_counts, function_names)
        }

        None => (
            module.borrow().function_name_counts.clone(),
            module.borrow().function_names.clone(),
        ),
    };

    if !function_names.borrow().contains_key(&signature) {
        let mut new_name = translate_naming_convention(old_name.as_str(), Case::Snake);

        {
            let mut function_name_counts = function_name_counts.borrow_mut();

            // Increase the function name count
            let count = function_name_counts.entry(new_name.clone()).or_insert(0);
            *count += 1;

            // Append the function name count to the end of the function name if there is more than 1
            if *count > 1 {
                new_name = format!("{new_name}_{}", *count);
            }
        }

        function_names.borrow_mut().insert(signature.clone(), new_name);
    }

    let function_names = function_names.borrow_mut();
    let abi_fn_name = function_names.get(&signature).unwrap().clone();

    let (old_name, mut top_level_fn_name) = if matches!(
        function_ty,
        solidity::FunctionTy::Function | solidity::FunctionTy::Modifier
    ) {
        let old_name = function_name.map(|s| s.to_string()).unwrap();
        (old_name, abi_fn_name.clone())
    } else {
        (String::new(), abi_fn_name.clone())
    };

    if let Some(contract_name) = contract_name.as_ref() {
        top_level_fn_name = format!("{}_{}", contract_name.to_case(Case::Snake), top_level_fn_name);
    }

    FunctionNames {
        old_name,
        top_level_fn_name,
        abi_fn_name,
    }
}

#[inline]
pub fn translate_function_parameters(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    contract_name: Option<&str>,
    function_definition: &solidity::FunctionDefinition,
) -> sway::ParameterList {
    // Translate the functions parameters
    let mut parameters = sway::ParameterList::default();
    let mut parameter_names = 'a'..='z';

    let scope = Rc::new(RefCell::new(ir::Scope::new(
        Some(module.borrow().path.clone()),
        contract_name,
        None,
        None,
    )));

    for (_, parameter) in function_definition.params.iter() {
        let old_name = parameter
            .as_ref()
            .unwrap()
            .name
            .as_ref()
            .map(|n| n.name.clone())
            .unwrap_or(parameter_names.next().unwrap().to_string());

        let new_name = translate_naming_convention(old_name.as_str(), Case::Snake);

        let type_name = translate_type_name(
            project,
            module.clone(),
            scope.clone(),
            &parameter.as_ref().unwrap().ty,
            parameter.as_ref().and_then(|p| p.storage.as_ref()),
        );

        parameters.entries.push(sway::Parameter {
            is_ref: false,
            is_mut: false,
            name: new_name,
            type_name: Some(type_name),
        });
    }

    parameters
}

#[inline]
pub fn translate_return_type_name(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    function_definition: &solidity::FunctionDefinition,
) -> Option<sway::TypeName> {
    if function_definition.returns.is_empty() {
        return None;
    }

    if function_definition.returns.len() == 1 {
        let type_name = translate_type_name(
            project,
            module.clone(),
            scope.clone(),
            &function_definition.returns[0].1.as_ref().unwrap().ty,
            function_definition.returns[0].1.as_ref().unwrap().storage.as_ref(),
        );

        return Some(get_return_type_name(project, module.clone(), &type_name));
    }

    Some(sway::TypeName::Tuple {
        type_names: function_definition
            .returns
            .iter()
            .map(|(_, p)| {
                let type_name = translate_type_name(
                    project,
                    module.clone(),
                    scope.clone(),
                    &p.as_ref().unwrap().ty,
                    p.as_ref().and_then(|p| p.storage.as_ref()),
                );
                get_return_type_name(project, module.clone(), &type_name)
            })
            .collect(),
    })
}

#[inline]
pub fn translate_function_declaration(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    contract_name: Option<&str>,
    contract_kind: Option<&solidity::ContractTy>,
    function_definition: &solidity::FunctionDefinition,
) -> Result<ir::Function, Error> {
    // println!(
    //     "Translating function declaration `{}` at {}",
    //     match contract_name {
    //         Some(contract_name) => format!(
    //             "{}.{}",
    //             contract_name,
    //             function_definition
    //                 .name
    //                 .as_ref()
    //                 .map(|s| s.name.as_str())
    //                 .unwrap_or_else(|| "<unnamed>")
    //         ),
    //         None => format!(
    //             "{}",
    //             function_definition
    //                 .name
    //                 .as_ref()
    //                 .map(|s| s.name.as_str())
    //                 .unwrap_or_else(|| "<unnamed>")
    //         ),
    //     },
    //     project.loc_to_file_location_string(module.clone(), &function_definition.loc),
    // );

    let function_attributes = FunctionAttributes::from(function_definition);

    let function_names = translate_function_name(
        project,
        module.clone(),
        contract_name,
        function_definition.name.as_ref().map(|n| n.name.as_str()),
        &function_definition.params,
        &function_definition.ty,
    );

    let parameters = translate_function_parameters(project, module.clone(), contract_name, function_definition);

    // Create a scope for modifier invocation translations
    let scope = Rc::new(RefCell::new(ir::Scope::new(
        Some(module.borrow().path.clone()),
        contract_name,
        Some(&function_names.top_level_fn_name),
        None,
    )));

    // Create the function's storage struct parameter and add it to the scope if necessary
    let mut storage_struct_parameter = None;

    if !function_attributes.is_pure
        && let Some(contract_name) = contract_name.as_ref()
        && !matches!(contract_kind, Some(solidity::ContractTy::Library(_)))
    {
        storage_struct_parameter = Some(Box::new(sway::Parameter {
            is_ref: false,
            is_mut: false,
            name: "storage_struct".to_string(),
            type_name: Some(sway::TypeName::create_identifier(
                format!("{contract_name}Storage").as_str(),
            )),
        }));

        scope.borrow_mut().add_variable(Rc::new(RefCell::new(ir::Variable {
            old_name: "".into(),
            new_name: "storage_struct".into(),
            type_name: sway::TypeName::create_identifier(format!("{contract_name}Storage").as_str()),
            statement_index: Some(0),
            read_count: 0,
            mutation_count: 0,
        })));
    }

    // Add the function parameters to the scope
    for (_, p) in function_definition.params.iter() {
        let Some(p) = p else { continue };
        let Some(parameter_identifier) = p.name.as_ref() else {
            continue;
        };

        let old_name = parameter_identifier.name.clone();
        let new_name = translate_naming_convention(old_name.as_str(), Case::Snake);
        let type_name = translate_type_name(project, module.clone(), scope.clone(), &p.ty, p.storage.as_ref());

        scope.borrow_mut().add_variable(Rc::new(RefCell::new(ir::Variable {
            old_name,
            new_name,
            type_name,
            ..Default::default()
        })));
    }

    // Translate the function declaration
    let translated_function = ir::Function {
        old_name: function_names.old_name.clone(),
        new_name: function_names.top_level_fn_name.clone(),
        attributes: function_attributes.clone().into(),
        type_name: sway::TypeName::Function {
            old_name: function_names.old_name,
            new_name: function_names.top_level_fn_name,
            generic_parameters: None,
            parameters,
            storage_struct_parameter,
            return_type: translate_return_type_name(project, module.clone(), scope.clone(), function_definition)
                .map(Box::new),
        },
    };

    Ok(translated_function)
}

#[inline]
pub fn translate_abi_function(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    contract_name: &str,
    function_definition: &solidity::FunctionDefinition,
) -> Option<sway::Function> {
    // println!(
    //     "Translating abi function: {:#?} - {}",
    //     function_definition.name,
    //     project.loc_to_file_location_string(module.clone(), &function_definition.loc)
    // );

    let function_attributes = FunctionAttributes::from(function_definition);

    let function_name = translate_function_name(
        project,
        module.clone(),
        Some(contract_name),
        function_definition.name.as_ref().map(|n| n.name.as_str()),
        &function_definition.params,
        &function_definition.ty,
    );

    let parameters = translate_function_parameters(project, module.clone(), Some(contract_name), function_definition);

    // Create the scope for the body of the toplevel function
    let scope = Rc::new(RefCell::new(ir::Scope::new(
        Some(module.borrow().path.clone()),
        Some(contract_name),
        Some(&function_name.top_level_fn_name),
        None,
    )));

    // Create the function's storage struct parameter and add it to the scope if necessary
    let mut storage_struct_parameter = None;

    if !function_attributes.is_pure {
        storage_struct_parameter = Some(sway::Parameter {
            is_ref: false,
            is_mut: false,
            name: "storage_struct".to_string(),
            type_name: Some(sway::TypeName::create_identifier(
                format!("{contract_name}Storage").as_str(),
            )),
        });

        scope.borrow_mut().add_variable(Rc::new(RefCell::new(ir::Variable {
            old_name: "".into(),
            new_name: "storage_struct".into(),
            type_name: sway::TypeName::create_identifier(format!("{contract_name}Storage").as_str()),
            statement_index: Some(0),
            read_count: 0,
            mutation_count: 0,
        })));
    }

    // Create the function declaration
    let mut sway_function = sway::Function {
        attributes: function_attributes.clone().into(),
        is_public: false,
        old_name: function_name.old_name.clone(),
        new_name: function_name.top_level_fn_name.clone(),
        generic_parameters: None,
        parameters,
        storage_struct_parameter,
        return_type: translate_return_type_name(project, module.clone(), scope.clone(), function_definition),
        body: None,
    };

    let mut abi_fn = None;

    if function_attributes.is_public {
        sway_function.new_name = function_name.abi_fn_name.clone();

        let mut abi_function = sway_function.clone();
        let mut use_string = false;

        if !function_attributes.is_fallback {
            for p in abi_function.parameters.entries.iter_mut() {
                if p.type_name == Some(sway::TypeName::StringSlice) {
                    use_string = true;
                    p.type_name = Some(sway::TypeName::create_identifier("String"));
                }
            }

            abi_fn = Some(abi_function);
        }

        if use_string {
            module.borrow_mut().ensure_use_declared("std::string::*");
        }

        sway_function.new_name.clone_from(&function_name.top_level_fn_name)
    }

    abi_fn
}

#[inline]
pub fn translate_function_definition(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    contract_name: Option<&str>,
    function_definition: &solidity::FunctionDefinition,
) -> Result<(Option<sway::Function>, Option<sway::ImplItem>), Error> {
    // println!(
    //     "Translating function `{}` at {}",
    //     match contract_name {
    //         Some(contract_name) => format!(
    //             "{}.{}",
    //             contract_name,
    //             function_definition
    //                 .name
    //                 .as_ref()
    //                 .map(|s| s.name.as_str())
    //                 .unwrap_or_else(|| "<unnamed>")
    //         ),
    //         None => format!(
    //             "{}",
    //             function_definition
    //                 .name
    //                 .as_ref()
    //                 .map(|s| s.name.as_str())
    //                 .unwrap_or_else(|| "<unnamed>")
    //         ),
    //     },
    //     project.loc_to_file_location_string(module.clone(), &function_definition.loc),
    // );

    // We should not translate modifier functions here.
    // Use translate_modifier_definition instead.
    assert!(!matches!(function_definition.ty, solidity::FunctionTy::Modifier));

    let function_attributes = FunctionAttributes::from(function_definition);

    let function_name = translate_function_name(
        project,
        module.clone(),
        contract_name,
        function_definition.name.as_ref().map(|n| n.name.as_str()),
        &function_definition.params,
        &function_definition.ty,
    );

    let parameters = translate_function_parameters(project, module.clone(), contract_name, function_definition);

    // Create the scope for the body of the toplevel function
    let scope = Rc::new(RefCell::new(ir::Scope::new(
        Some(module.borrow().path.clone()),
        contract_name,
        Some(&function_name.top_level_fn_name),
        None,
    )));

    // Create the function's storage struct parameter and add it to the scope if necessary
    let mut storage_struct_parameter = None;

    if !(function_attributes.is_constant || function_attributes.is_pure)
        && let Some(contract_name) = contract_name
        && let Some(contract) = project.find_contract(module.clone(), contract_name)
        && let Some(storage_struct) = contract.borrow().storage_struct.as_ref()
    {
        storage_struct_parameter = Some(sway::Parameter {
            is_ref: false,
            is_mut: false,
            name: "storage_struct".to_string(),
            type_name: Some(sway::TypeName::create_identifier(storage_struct.borrow().name.as_str())),
        });

        scope.borrow_mut().add_variable(Rc::new(RefCell::new(ir::Variable {
            old_name: "".into(),
            new_name: "storage_struct".into(),
            type_name: sway::TypeName::create_identifier(storage_struct.borrow().name.as_str()),
            statement_index: Some(0),
            read_count: 0,
            mutation_count: 0,
        })));
    }

    // Create the function declaration
    let function_declaration = sway::Function {
        attributes: function_attributes.clone().into(),
        is_public: false,
        old_name: function_name.old_name.clone(),
        new_name: function_name.top_level_fn_name.clone(),
        generic_parameters: None,
        parameters,
        storage_struct_parameter: storage_struct_parameter.clone(),
        return_type: translate_return_type_name(project, module.clone(), scope.clone(), function_definition),
        body: None,
    };

    // Convert the statements in the function's body (if any)
    let Some(solidity::Statement::Block { statements, .. }) = function_definition.body.as_ref() else {
        return Ok((None, None));
    };

    // Add the function parameters to the scope
    let parameters = scope
        .borrow_mut()
        .add_function_parameters(project, module.clone(), function_definition.clone());

    // Add the function's named return parameters to the scope
    let return_parameters =
        scope
            .borrow_mut()
            .add_function_return_parameters(project, module.clone(), function_definition.clone());

    // Translate the body for the toplevel function
    let mut function_body = translate_block(project, module.clone(), scope.clone(), statements.as_slice())?;

    // Create a constructor guard if the function is a constructor
    if function_attributes.is_constructor {
        update_constructor_function_body(
            project,
            module.clone(),
            scope.clone(),
            contract_name,
            &mut function_body,
        );
    }

    // Check for parameters that were mutated and make them local variables
    for parameter in parameters.iter().rev() {
        let Some(variable) = scope.borrow().get_variable_from_new_name(&parameter.new_name) else {
            panic!("error: Variable not found in scope: \"{}\"", parameter.new_name);
        };

        if variable.borrow().mutation_count > 0 {
            function_body.statements.insert(
                0,
                sway::Statement::Let(sway::Let {
                    pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                        is_mutable: true,
                        name: parameter.new_name.clone(),
                    }),
                    type_name: Some(parameter.type_name.clone()),
                    value: sway::Expression::create_identifier(parameter.new_name.as_str()),
                }),
            );
        }
    }

    // Propagate the return variable declarations
    for return_parameter in return_parameters.iter().rev() {
        let scope = Rc::new(RefCell::new(ir::Scope::new(
            Some(module.borrow().path.clone()),
            contract_name,
            Some(&function_name.top_level_fn_name),
            None,
        )));

        function_body.statements.insert(
            0,
            sway::Statement::Let(sway::Let {
                pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                    is_mutable: true,
                    name: return_parameter.new_name.clone(),
                }),
                type_name: Some(return_parameter.type_name.clone()),
                value: create_value_expression(
                    project,
                    module.clone(),
                    scope.clone(),
                    &return_parameter.type_name,
                    None,
                ),
            }),
        );
    }

    // If the function returns values but doesn't end in a return statement, propagate the return variables
    if !return_parameters.is_empty()
        && !matches!(
            function_body.statements.last(),
            Some(sway::Statement::Expression(sway::Expression::Return(_)))
        )
    {
        function_body
            .statements
            .push(sway::Statement::from(sway::Expression::Return(Some(Box::new(
                if return_parameters.len() == 1 {
                    sway::Expression::create_identifier(return_parameters[0].new_name.as_str())
                } else {
                    sway::Expression::Tuple(
                        return_parameters
                            .iter()
                            .map(|p| sway::Expression::create_identifier(p.new_name.as_str()))
                            .collect(),
                    )
                },
            )))));
    }

    // Check if the final statement returns a value and change it to be the final expression of the block
    if let Some(sway::Statement::Expression(sway::Expression::Return(Some(value)))) =
        function_body.statements.last().cloned()
    {
        function_body.statements.pop();
        function_body.final_expr = Some(*value);
    }

    // Translate the function's constructor/modifier invocations
    let mut constructor_calls = vec![];
    let mut modifiers = vec![];

    for attr in function_definition.attributes.iter() {
        let solidity::FunctionAttribute::BaseOrModifier(_, base) = attr else {
            continue;
        };

        let old_name = base
            .name
            .identifiers
            .iter()
            .map(|i| i.name.clone())
            .collect::<Vec<_>>()
            .join(".");

        let mut new_name = translate_naming_convention(old_name.as_str(), Case::Snake);
        if let Some(contract_name) = contract_name {
            new_name = format!("{}_{}", contract_name.to_case(Case::Snake), new_name);
        }

        let mut parameters = base
            .args
            .as_ref()
            .map(|args| {
                args.iter()
                    .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
                    .collect::<Result<Vec<_>, _>>()
            })
            .unwrap_or_else(|| Ok(vec![]))?;

        {
            let mut module = module.borrow_mut();
            let &mut (storage_read, storage_write) =
                module.function_storage_accesses.entry(new_name.clone()).or_default();

            if storage_read || storage_write {
                if let Some(storage_struct_parameter) = storage_struct_parameter.as_ref() {
                    parameters.push(sway::Expression::create_identifier(
                        storage_struct_parameter.name.as_str(),
                    ));
                }
            }
        }

        let parameter_types = parameters
            .iter()
            .map(|p| get_expression_type(project, module.clone(), scope.clone(), p))
            .collect::<Result<Vec<_>, _>>()?;

        // Check to see if base is a constructor call
        if project.is_contract_declared(module.clone(), old_name.as_str()) {
            let prefix = translate_naming_convention(old_name.as_str(), Case::Snake);
            let name = format!("{prefix}_constructor");

            constructor_calls.push(sway::FunctionCall {
                function: sway::Expression::create_identifier(name.as_str()),
                generic_parameters: None,
                parameters,
            });
        }
        // Add the base to the modifiers list
        else if let Some((modifier, parameters, _)) = resolve_modifier(
            project,
            module.clone(),
            scope.clone(),
            &new_name,
            None,
            parameters.clone(),
            parameter_types,
        )? {
            let sway::TypeName::Function { new_name, .. } = &modifier.signature else {
                unreachable!()
            };
            modifiers.push(sway::FunctionCall {
                function: sway::Expression::create_identifier(new_name.as_str()),
                generic_parameters: None,
                parameters,
            });
        }
    }

    // Propagate constructor calls into the function's body
    for constructor_call in constructor_calls.iter().rev() {
        function_body.statements.insert(
            0,
            sway::Statement::from(sway::Expression::from(constructor_call.clone())),
        );
    }

    // Propagate modifier pre and post functions into the function's body
    let mut modifier_pre_calls = vec![];
    let mut modifier_post_calls = vec![];

    for modifier_invocation in modifiers.iter() {
        let Some(new_name) = modifier_invocation.function.as_identifier() else {
            panic!("Malformed modifier invocation: {modifier_invocation:#?}");
        };

        let parameter_types = modifier_invocation
            .parameters
            .iter()
            .map(|p| get_expression_type(project, module.clone(), scope.clone(), p))
            .collect::<Result<Vec<_>, _>>()?;

        let Some((modifier, parameters, _parameter_types)) = resolve_modifier(
            project,
            module.clone(),
            scope.clone(),
            new_name,
            None,
            modifier_invocation.parameters.clone(),
            parameter_types,
        )?
        else {
            panic!("Failed to find modifier: {new_name}");
        };

        let parameters_cell = Rc::new(RefCell::new(parameters));

        let Some((modifier_pre_call, modifier_post_call, modifier_body)) = resolve_modifier_invocation(
            project,
            module.clone(),
            scope.clone(),
            modifier,
            parameters_cell,
            function_declaration.return_type.clone(),
            &mut function_body,
        )?
        else {
            panic!("Failed to resolve modifier invocation: {new_name}");
        };

        assert!(modifier_body.is_some() != (modifier_pre_call.is_some() || modifier_post_call.is_some()));

        if let Some(modifier_pre_call) = modifier_pre_call {
            modifier_pre_calls.push(modifier_pre_call);
        }

        if let Some(modifier_post_call) = modifier_post_call {
            modifier_post_calls.push(modifier_post_call);
        }

        if let Some(modifier_body) = modifier_body {
            function_body = modifier_body;
        }
    }

    for modifier_pre_call in modifier_pre_calls.iter().rev() {
        function_body.statements.insert(
            0,
            sway::Statement::from(sway::Expression::from(modifier_pre_call.clone())),
        );
    }

    for modifier_post_call in modifier_post_calls.iter().rev() {
        function_body
            .statements
            .push(sway::Statement::from(sway::Expression::from(
                modifier_post_call.clone(),
            )));
    }

    // Create the toplevel function
    let mut toplevel_function = function_declaration.clone();
    toplevel_function.is_public = true;
    toplevel_function.new_name = function_name.top_level_fn_name.clone();
    toplevel_function.body = Some(function_body);

    // Remove `payable` attribute from toplevel function if present
    if let Some((index, _)) = toplevel_function
        .attributes
        .as_ref()
        .and_then(|a| a.attributes.iter().enumerate().find(|(_, a)| a.name == "payable"))
    {
        toplevel_function
            .attributes
            .as_mut()
            .map(|a| a.attributes.remove(index))
            .unwrap();
    }

    // Create the contract impl's function wrapper
    let mut impl_item = None;

    if function_attributes.is_public && !function_attributes.is_fallback {
        let mut impl_function = function_declaration.clone();
        impl_function.new_name = function_name.abi_fn_name.clone();

        let contract_name = contract_name.unwrap();

        let mut statements = vec![];

        let mut parameters = impl_function
            .parameters
            .entries
            .iter()
            .map(|p| sway::Expression::create_identifier(p.name.as_str()))
            .collect::<Vec<_>>();

        // Convert parameters of `str` to `String`, since they are not allowed in abi function signatures
        for p in impl_function.parameters.entries.iter_mut() {
            let Some(sway::TypeName::StringSlice) = p.type_name else {
                continue;
            };

            module.borrow_mut().ensure_use_declared("std::string::*");

            p.type_name = Some(sway::TypeName::create_identifier("String"));

            statements.push(sway::Statement::from(sway::Let {
                pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                    is_mutable: true,
                    name: p.name.clone(),
                }),
                type_name: None,
                value: sway::Expression::create_identifier(p.name.as_str()).with_as_str_call(),
            }));
        }

        // If the contract has a storage struct, generate bindings and a storage struct parameter for the toplevel function call
        let has_storage_struct = {
            let contract = project.find_contract(module.clone(), contract_name).unwrap();
            contract.borrow().storage_struct.is_some()
        };
        if !(function_attributes.is_constant || function_attributes.is_pure) && has_storage_struct {
            let contract = project.find_contract(module.clone(), contract_name).unwrap();

            ensure_storage_struct_constructor_exists(project, module.clone(), scope.clone(), contract.clone());

            statements.push(sway::Statement::from(sway::Let {
                pattern: sway::LetPattern::from(sway::LetIdentifier {
                    is_mutable: false,
                    name: "storage_struct".to_string(),
                }),
                type_name: None,
                value: sway::Expression::create_function_call(
                    contract
                        .borrow()
                        .storage_struct_constructor_fn
                        .as_ref()
                        .unwrap()
                        .new_name
                        .as_str(),
                    None,
                    vec![],
                ),
            }));

            parameters.push(sway::Expression::create_identifier("storage_struct".into()));
        }

        impl_function.body = Some(sway::Block {
            statements,
            final_expr: Some(sway::Expression::create_function_call(
                toplevel_function.new_name.as_str(),
                None,
                parameters,
            )),
        });

        // Create the function wrapper item for the contract impl block
        impl_item = Some(sway::ImplItem::Function(impl_function.clone()));
    }

    Ok((Some(toplevel_function), impl_item))
}

#[inline]
pub fn translate_modifier_definition(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    contract_name: Option<&str>,
    function_definition: &solidity::FunctionDefinition,
) -> Result<(), Error> {
    let old_name = function_definition.name.as_ref().unwrap().name.clone();
    let mut new_name = translate_naming_convention(old_name.as_str(), Case::Snake);

    if let Some(contract_name) = contract_name {
        new_name = format!("{}_{}", contract_name.to_case(Case::Snake), new_name);
    }

    // println!(
    //     "Translating modifier {}.{} at {}",
    //     module.borrow().name,
    //     function_definition
    //         .name
    //         .as_ref()
    //         .map(|n| n.name.as_str())
    //         .unwrap_or_else(|| new_name.as_str()),
    //     project.loc_to_file_location_string(module.clone(), &function_definition.loc),
    // );

    let mut modifier = ir::Modifier {
        old_name: old_name.clone(),
        new_name: new_name.clone(),
        parameters: sway::ParameterList::default(),
        storage_struct_parameter: None,
        attributes: None,
        has_underscore: false,
        inline_body: None,
        pre_body: None,
        post_body: None,
    };

    let scope = Rc::new(RefCell::new(ir::Scope::new(
        Some(module.borrow().path.clone()),
        contract_name,
        Some(&new_name),
        None,
    )));

    for (_, p) in function_definition.params.iter() {
        let old_name = p
            .as_ref()
            .unwrap()
            .name
            .as_ref()
            .map(|p| p.name.clone())
            .unwrap_or_else(String::new);

        let new_name = if old_name.is_empty() {
            // println!("WARNING: found unnamed parameter");
            // TODO: we should generate a unique parameter name
            "_".to_string()
        } else {
            translate_naming_convention(old_name.as_str(), Case::Snake)
        };

        let type_name = translate_type_name(
            project,
            module.clone(),
            scope.clone(),
            &p.as_ref().unwrap().ty,
            p.as_ref().and_then(|p| p.storage.as_ref()),
        );

        modifier.parameters.entries.push(sway::Parameter {
            name: new_name.clone(),
            type_name: Some(type_name.clone()),
            ..Default::default()
        });

        scope.borrow_mut().add_variable(Rc::new(RefCell::new(ir::Variable {
            old_name,
            new_name,
            type_name,
            ..Default::default()
        })));
    }

    let solidity::Statement::Block { statements, .. } = function_definition.body.as_ref().unwrap() else {
        panic!(
            "Invalid modifier body, expected block, found: {:#?}",
            function_definition.body
        );
    };

    // let mut current_body: &mut Option<sway::Block> = &mut modifier.pre_body;
    // let mut current_scope = Rc::new(RefCell::new(scope.borrow().clone()));

    // let mut has_pre_storage_read = false;
    // let mut has_pre_storage_write = false;

    // let mut has_post_storage_read = false;
    // let mut has_post_storage_write = false;

    // let mut has_storage_read = &mut has_pre_storage_read;
    // let mut has_storage_write = &mut has_pre_storage_write;

    // for statement in statements.iter() {
    //     // Translate the statement
    //     let sway_statement =
    //         translate_statement(project, module.clone(), current_scope.clone(), statement)?;

    //     // If we encounter the underscore statement, every following statement goes into the modifier's post_body block.
    //     if sway_statement
    //         .filter_map(|s| {
    //             let sway::Statement::Expression(expression) = s else {
    //                 return None;
    //             };
    //             if let Some("_") = expression.as_identifier() {
    //                 return Some(expression.clone());
    //             }
    //             None
    //         })
    //         .is_some()
    //     {
    //         modifier.has_underscore = true;

    //         if let Some(block) = current_body.as_mut() {
    //             {
    //                 let mut module = module.borrow_mut();
    //                 let &mut (storage_read, storage_write) = module
    //                     .function_storage_accesses
    //                     .entry(modifier.new_name.clone())
    //                     .or_default();

    //                 if storage_read {
    //                     *has_storage_read = true;
    //                 }

    //                 if storage_write {
    //                     *has_storage_write = true;
    //                 }
    //             }

    //             finalize_block_translation(project, current_scope.clone(), block)?;
    //         }

    //         current_body = &mut modifier.post_body;

    //         let mut new_scope = ir::Scope::new(
    //             scope.borrow().get_contract_name().as_deref(),
    //             Some(&new_name),
    //             scope.borrow().get_parent(),
    //         );

    //         for v in scope.borrow().get_variables() {
    //             let mut v = v.borrow().clone();
    //             v.statement_index = None;
    //             new_scope.add_variable(Rc::new(RefCell::new(v)));
    //         }

    //         current_scope = Rc::new(RefCell::new(new_scope));

    //         has_storage_read = &mut has_post_storage_read;
    //         has_storage_write = &mut has_post_storage_write;

    //         continue;
    //     }

    //     // Create the current body block if it hasn't already been
    //     if current_body.is_none() {
    //         *current_body = Some(sway::Block::default());
    //     }

    //     let block = current_body.as_mut().unwrap();

    //     // Store the index of the sway statement
    //     let statement_index = block.statements.len();

    //     // Add the sway statement to the sway block
    //     block.statements.push(sway_statement);

    //     // If the sway statement is a variable declaration, keep track of its statement index
    //     if let Some(sway::Statement::Let(sway_variable)) = block.statements.last() {
    //         let store_variable_statement_index = |id: &sway::LetIdentifier| {
    //             let scope = current_scope.borrow_mut();
    //             let scope_variables = scope.get_variables();
    //             let scope_entry = scope_variables
    //                 .iter()
    //                 .rev()
    //                 .find(|v| v.borrow().new_name == id.name)
    //                 .unwrap();
    //             scope_entry.borrow_mut().statement_index = Some(statement_index);
    //         };

    //         match &sway_variable.pattern {
    //             sway::LetPattern::Identifier(id) => store_variable_statement_index(id),
    //             sway::LetPattern::Tuple(ids) => ids.iter().for_each(store_variable_statement_index),
    //         }
    //     }
    // }

    // if let Some(block) = current_body.as_mut() {
    //     finalize_block_translation(project, current_scope.clone(), block)?;
    // }

    // let create_attributes =
    //     |has_storage_read: bool, has_storage_write: bool| -> Option<sway::AttributeList> {
    //         let mut parameters = vec![];

    //         if has_storage_read {
    //             parameters.push("read".into());
    //         }

    //         if has_storage_write {
    //             parameters.push("write".into());
    //         }

    //         if parameters.is_empty() {
    //             None
    //         } else {
    //             Some(sway::AttributeList {
    //                 attributes: vec![sway::Attribute {
    //                     name: "storage".into(),
    //                     parameters: Some(parameters),
    //                 }],
    //             })
    //         }
    //     };

    // // Ensure that an underscore statement was encountered while translating the modifier
    // if !modifier.has_underscore {
    //     panic!(
    //         "Malformed modifier missing underscore statement: {}",
    //         modifier.old_name
    //     );
    // }

    // if let Some(contract_name) = contract_name {
    //     let contract = project
    //         .find_contract(module.clone(), contract_name)
    //         .unwrap();

    //     if contract.borrow().storage_struct.is_none() {
    //         has_pre_storage_read = false;
    //         has_pre_storage_write = false;
    //         has_post_storage_read = false;
    //         has_post_storage_write = false;
    //     }
    // }

    // // Create a storage struct parameter if necessary
    // if has_pre_storage_read
    //     || has_pre_storage_write
    //     || has_post_storage_read
    //     || has_post_storage_write
    // {
    //     modifier.storage_struct_parameter = Some(sway::Parameter {
    //         is_ref: false,
    //         is_mut: false,
    //         name: "storage_struct".into(),
    //         type_name: Some(sway::TypeName::create_identifier(
    //             format!("{}Storage", contract_name.unwrap()).as_str(),
    //         )),
    //     });
    // }

    // Generate toplevel modifier functions
    // match (modifier.pre_body.as_ref(), modifier.post_body.as_ref()) {
    //     (Some(pre_body), Some(post_body)) => {
    //         let modifier_pre_function_name = format!("{}_pre", modifier.new_name);

    //         let modifier_pre_storage = if has_pre_storage_read || has_pre_storage_write {
    //             Some(sway::Parameter {
    //                 is_ref: false,
    //                 is_mut: false,
    //                 name: "storage_struct".into(),
    //                 type_name: Some(sway::TypeName::create_identifier(
    //                     format!("{}Storage", contract_name.unwrap()).as_str(),
    //                 )),
    //             })
    //         } else {
    //             None
    //         };

    //         module.borrow_mut().functions.push(ir::Item {
    //             signature: sway::TypeName::Function {
    //                 old_name: String::new(),
    //                 new_name: String::new(),
    //                 generic_parameters: None,
    //                 parameters: modifier.parameters.clone(),
    //                 storage_struct_parameter: modifier_pre_storage.clone().map(Box::new),
    //                 return_type: None,
    //             },
    //             implementation: Some(sway::Function {
    //                 attributes: create_attributes(has_pre_storage_read, has_pre_storage_write),
    //                 is_public: true,
    //                 old_name: String::new(), // TODO
    //                 new_name: modifier_pre_function_name.clone(),
    //                 generic_parameters: None,
    //                 parameters: modifier.parameters.clone(),
    //                 storage_struct_parameter: modifier_pre_storage,
    //                 return_type: None,
    //                 body: Some(pre_body.clone()),
    //             }),
    //         });

    //         let modifier_post_function_name = format!("{}_post", modifier.new_name);

    //         let modifier_post_storage = if has_post_storage_read || has_post_storage_write {
    //             Some(sway::Parameter {
    //                 is_ref: false,
    //                 is_mut: false,
    //                 name: "storage_struct".into(),
    //                 type_name: Some(sway::TypeName::create_identifier(
    //                     format!("{}Storage", contract_name.unwrap()).as_str(),
    //                 )),
    //             })
    //         } else {
    //             None
    //         };

    //         module.borrow_mut().functions.push(ir::Item {
    //             signature: sway::TypeName::Function {
    //                 old_name: String::new(),
    //                 new_name: String::new(),
    //                 generic_parameters: None,
    //                 parameters: modifier.parameters.clone(),
    //                 storage_struct_parameter: modifier_post_storage.clone().map(Box::new),
    //                 return_type: None,
    //             },
    //             implementation: Some(sway::Function {
    //                 attributes: create_attributes(has_post_storage_read, has_post_storage_write),
    //                 is_public: true,
    //                 old_name: String::new(), // TODO
    //                 new_name: modifier_post_function_name.clone(),
    //                 generic_parameters: None,
    //                 parameters: modifier.parameters.clone(),
    //                 storage_struct_parameter: modifier_post_storage,
    //                 return_type: None,
    //                 body: Some(post_body.clone()),
    //             }),
    //         });
    //     }

    //     (Some(pre_body), None) => {
    //         let modifier_pre_storage = if has_pre_storage_read || has_pre_storage_write {
    //             Some(sway::Parameter {
    //                 is_ref: false,
    //                 is_mut: false,
    //                 name: "storage_struct".into(),
    //                 type_name: Some(sway::TypeName::create_identifier(
    //                     format!("{}Storage", contract_name.unwrap()).as_str(),
    //                 )),
    //             })
    //         } else {
    //             None
    //         };

    //         module.borrow_mut().functions.push(ir::Item {
    //             signature: sway::TypeName::Function {
    //                 old_name: String::new(),
    //                 new_name: String::new(),
    //                 generic_parameters: None,
    //                 parameters: modifier.parameters.clone(),
    //                 storage_struct_parameter: modifier_pre_storage.clone().map(Box::new),
    //                 return_type: None,
    //             },
    //             implementation: Some(sway::Function {
    //                 attributes: create_attributes(has_pre_storage_read, has_pre_storage_write),
    //                 is_public: true,
    //                 old_name: modifier.old_name.clone(),
    //                 new_name: modifier.new_name.clone(),
    //                 generic_parameters: None,
    //                 parameters: modifier.parameters.clone(),
    //                 storage_struct_parameter: modifier_pre_storage,
    //                 return_type: None,
    //                 body: Some(pre_body.clone()),
    //             }),
    //         });
    //     }

    //     (None, Some(post_body)) => {
    //         let modifier_post_storage = if has_post_storage_read || has_post_storage_write {
    //             Some(sway::Parameter {
    //                 is_ref: false,
    //                 is_mut: false,
    //                 name: "storage_struct".into(),
    //                 type_name: Some(sway::TypeName::create_identifier(
    //                     format!("{}Storage", contract_name.unwrap()).as_str(),
    //                 )),
    //             })
    //         } else {
    //             None
    //         };

    //         module.borrow_mut().functions.push(ir::Item {
    //             signature: sway::TypeName::Function {
    //                 old_name: String::new(),
    //                 new_name: String::new(),
    //                 generic_parameters: None,
    //                 parameters: modifier.parameters.clone(),
    //                 storage_struct_parameter: modifier_post_storage.clone().map(Box::new),
    //                 return_type: None,
    //             },
    //             implementation: Some(sway::Function {
    //                 attributes: create_attributes(has_post_storage_read, has_post_storage_write),
    //                 is_public: true,
    //                 old_name: modifier.old_name.clone(),
    //                 new_name: modifier.new_name.clone(),
    //                 generic_parameters: None,
    //                 parameters: modifier.parameters.clone(),
    //                 storage_struct_parameter: modifier_post_storage,
    //                 return_type: None,
    //                 body: Some(post_body.clone()),
    //             }),
    //         });
    //     }

    //     (None, None) => {
    //         if !function_definition.params.is_empty() {
    //             todo!("Support inlining modifier with parameters");
    //         }

    //         let block = translate_block(project, module.clone(), scope.clone(), statements)?;
    //         modifier.inline_body = Some(block);
    //     }
    // }

    // HACK: inline all modifiers for now...
    // if !function_definition.params.is_empty() {
    //     todo!("Support inlining modifier with parameters");
    // }
    let block = translate_block(project, module.clone(), scope.clone(), statements)?;
    modifier.inline_body = Some(block);

    // Add the translated modifier to the translated definition
    let mut module = module.borrow_mut();

    let Some(modifier_entry) = module.modifiers.iter_mut().find(|m| {
        let sway::TypeName::Function { new_name, .. } = &m.signature else {
            unreachable!()
        };

        *new_name == modifier.new_name
    }) else {
        panic!("Failed to find modifier: {}", modifier.new_name);
    };

    if let Some(storage_struct_parameter) = modifier.storage_struct_parameter.as_ref() {
        let sway::TypeName::Function {
            storage_struct_parameter: modifier_entry_storage_struct,
            ..
        } = &mut modifier_entry.signature
        else {
            unreachable!()
        };

        *modifier_entry_storage_struct = Some(Box::new(storage_struct_parameter.clone()));
    }

    modifier_entry.implementation = Some(Rc::new(RefCell::new(modifier)));

    Ok(())
}

#[inline(always)]
pub fn ensure_constructor_functions_exist(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    contract: Rc<RefCell<ir::Contract>>,
) {
    if contract.borrow().abi_impl.items.iter().any(|i| {
        let sway::ImplItem::Function(f) = i else {
            return false;
        };
        f.new_name == "constructor"
    }) {
        return;
    }

    let contract_name = contract.borrow().name.clone();

    let function_name = translate_function_name(
        project,
        module.clone(),
        Some(contract_name.as_str()),
        None,
        &vec![],
        &solidity::FunctionTy::Constructor,
    );

    if module.borrow().functions.iter().any(|f| {
        let sway::TypeName::Function { new_name, .. } = &f.signature else {
            unreachable!()
        };
        *new_name == function_name.top_level_fn_name
    }) {
        return;
    }

    // Create the ABI function
    let abi_function = sway::Function {
        attributes: None,
        is_public: false,
        old_name: String::new(),
        new_name: "constructor".into(),
        generic_parameters: None,
        storage_struct_parameter: None,
        parameters: sway::ParameterList::default(),
        return_type: None,
        body: None,
    };

    // Create the ABI impl function
    let mut impl_function = abi_function.clone();

    impl_function.body = Some(sway::Block {
        statements: vec![sway::Statement::from(sway::Let {
            pattern: sway::LetPattern::from(sway::LetIdentifier {
                is_mutable: false,
                name: "storage_struct".to_string(),
            }),
            type_name: None,
            value: sway::Expression::create_function_call(
                contract
                    .borrow()
                    .storage_struct_constructor_fn
                    .as_ref()
                    .unwrap()
                    .new_name
                    .as_str(),
                None,
                vec![],
            ),
        })],
        final_expr: Some(sway::Expression::create_function_call(
            function_name.top_level_fn_name.as_str(),
            None,
            vec![sway::Expression::create_identifier("storage_struct".into())],
        )),
    });

    // Create the the toplevel function
    let mut toplevel_function = abi_function.clone();
    toplevel_function.is_public = true;
    toplevel_function.new_name = function_name.top_level_fn_name.clone();

    toplevel_function.storage_struct_parameter = Some(sway::Parameter {
        is_ref: false,
        is_mut: false,
        name: "storage_struct".to_string(),
        type_name: Some(sway::TypeName::create_identifier(
            format!("{contract_name}Storage").as_str(),
        )),
    });

    toplevel_function.body = Some(sway::Block::default());

    update_constructor_function_body(
        project,
        module.clone(),
        scope.clone(),
        Some(contract_name.as_str()),
        toplevel_function.body.as_mut().unwrap(),
    );

    // Add the functions to the contract and module
    contract.borrow_mut().abi.functions.insert(0, abi_function.clone());

    contract
        .borrow_mut()
        .abi_impl
        .items
        .insert(0, sway::ImplItem::Function(impl_function));

    module.borrow_mut().functions.push(ir::Item {
        signature: sway::TypeName::Function {
            old_name: String::new(),
            new_name: function_name.top_level_fn_name,
            generic_parameters: None,
            parameters: sway::ParameterList::default(),
            storage_struct_parameter: Some(Box::new(sway::Parameter {
                is_ref: false,
                is_mut: false,
                name: "storage_struct".into(),
                type_name: Some(sway::TypeName::create_identifier(
                    format!("{contract_name}Storage").as_str(),
                )),
            })),
            return_type: None,
        },
        implementation: Some(toplevel_function),
    });
}

#[inline(always)]
pub fn ensure_constructor_called_fields_exist(
    _project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
) {
    let constructor_called_field_name = "constructor_called".to_string();

    let mut module = module.borrow_mut();
    let storage_struct = module.get_storage_struct(scope.clone());

    if !storage_struct
        .borrow()
        .storage
        .fields
        .iter()
        .any(|f| f.new_name == constructor_called_field_name)
    {
        // Add the `constructor_called` field to the storage struct
        storage_struct.borrow_mut().storage.fields.push(sway::StructField {
            is_public: true,
            new_name: constructor_called_field_name.clone(),
            old_name: String::new(),
            type_name: sway::TypeName::create_identifier("bool").to_storage_key(),
        });
    }

    // Add the `constructor_called` field to the storage block
    let storage_namespace = module.get_storage_namespace(scope.clone()).unwrap();

    if !storage_namespace
        .borrow()
        .fields
        .iter()
        .any(|s| s.name == constructor_called_field_name)
    {
        storage_namespace.borrow_mut().fields.push(sway::StorageField {
            old_name: String::new(),
            name: constructor_called_field_name.clone(),
            type_name: sway::TypeName::create_identifier("bool"),
            value: sway::Expression::from(sway::Literal::Bool(false)),
        });
    }
}

#[inline(always)]
pub fn update_constructor_function_body(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    contract_name: Option<&str>,
    function_body: &mut sway::Block,
) {
    let constructor_called_field_name = "constructor_called".to_string();

    ensure_constructor_called_fields_exist(project, module.clone(), scope.clone());

    // Add the `constructor_called` requirement to the beginning of the function
    // require(!storage.initialized.read(), "The Contract constructor has already been called");
    function_body.statements.insert(
        0,
        sway::Statement::from(sway::Expression::create_function_call(
            "require",
            None,
            vec![
                sway::Expression::from(sway::UnaryExpression {
                    operator: "!".into(),
                    expression: sway::Expression::create_identifier("storage_struct")
                        .with_member(constructor_called_field_name.as_str())
                        .with_read_call(),
                }),
                sway::Expression::from(sway::Literal::String(format!(
                    "The {} constructor has already been called",
                    contract_name
                        .map(|s| s.to_string())
                        .unwrap_or_else(|| module.borrow().name.clone()),
                ))),
            ],
        )),
    );

    // Set the `constructor_called` storage field to `true` at the end of the function
    // storage.initialized.write(true);
    function_body.statements.push(sway::Statement::from(
        sway::Expression::create_identifier("storage_struct")
            .with_member(constructor_called_field_name.as_str())
            .with_write_call(sway::Expression::from(sway::Literal::Bool(true))),
    ));
}

#[inline(always)]
pub fn ensure_storage_struct_constructor_exists(
    _project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    contract: Rc<RefCell<ir::Contract>>,
) {
    let contract_name = contract.borrow().name.clone();

    if contract.borrow().storage_struct.is_none() || contract.borrow().storage_struct_constructor_fn.is_some() {
        return;
    }

    let storage_namespace_name = module.borrow_mut().get_storage_namespace_name(scope.clone()).unwrap();

    let constructor = sway::Constructor {
        type_name: sway::TypeName::create_identifier(format!("{contract_name}Storage").as_str()),
        fields: contract
            .borrow()
            .storage_struct
            .as_ref()
            .unwrap()
            .borrow()
            .storage
            .fields
            .iter()
            .map(|f| sway::ConstructorField {
                name: f.new_name.clone(),
                value: sway::Expression::from(sway::MemberAccess {
                    expression: sway::Expression::from(sway::PathExpr {
                        root: sway::PathExprRoot::Identifier("storage".to_string()),
                        segments: vec![sway::PathExprSegment {
                            name: storage_namespace_name.clone(),
                            generic_parameters: None,
                        }],
                    }),
                    member: f.new_name.clone(),
                }),
            })
            .collect(),
    };

    contract.borrow_mut().storage_struct_constructor_fn = Some(sway::Function {
        attributes: None,
        is_public: true,
        old_name: String::new(),
        new_name: format!("create_{}_storage_struct", contract_name.to_case(Case::Snake)),
        generic_parameters: None,
        parameters: sway::ParameterList::default(),
        storage_struct_parameter: None,
        return_type: Some(sway::TypeName::create_identifier(
            format!("{contract_name}Storage").as_str(),
        )),
        body: Some(sway::Block {
            statements: vec![],
            final_expr: Some(sway::Expression::from(constructor)),
        }),
    });
}

fn resolve_modifier_invocation(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    modifier: ir::Item<Rc<RefCell<ir::Modifier>>>,
    parameters_cell: Rc<RefCell<Vec<sway::Expression>>>,
    return_type: Option<sway::TypeName>,
    function_body: &mut sway::Block,
) -> Result<
    Option<(
        Option<sway::FunctionCall>,
        Option<sway::FunctionCall>,
        Option<sway::Block>,
    )>,
    Error,
> {
    let modifier = modifier.implementation.as_ref().unwrap().borrow();
    if modifier.pre_body.is_some() && modifier.post_body.is_some() {
        return Ok(Some((
            Some(sway::FunctionCall {
                function: sway::Expression::create_identifier(format!("{}_pre", modifier.new_name).as_str()),
                generic_parameters: None,
                parameters: parameters_cell.borrow().clone(),
            }),
            Some(sway::FunctionCall {
                function: sway::Expression::create_identifier(format!("{}_post", modifier.new_name).as_str()),
                generic_parameters: None,
                parameters: parameters_cell.borrow().clone(),
            }),
            None,
        )));
    } else if modifier.pre_body.is_some() {
        return Ok(Some((
            Some(sway::FunctionCall {
                function: sway::Expression::create_identifier(modifier.new_name.as_str()),
                generic_parameters: None,
                parameters: parameters_cell.borrow().clone(),
            }),
            None,
            None,
        )));
    } else if modifier.post_body.is_some() {
        return Ok(Some((
            None,
            Some(sway::FunctionCall {
                function: sway::Expression::create_identifier(modifier.new_name.as_str()),
                generic_parameters: None,
                parameters: parameters_cell.borrow().clone(),
            }),
            None,
        )));
    } else if let Some(mut block) = modifier.inline_body.clone() {
        fn inline_expression(expression: &mut sway::Expression, inline_statements: &[sway::Statement]) {
            match expression {
                sway::Expression::Literal(_) => {}
                sway::Expression::PathExpr(_) => {}

                sway::Expression::FunctionCall(function_call) => {
                    inline_expression(&mut function_call.function, inline_statements);

                    for parameter in function_call.parameters.iter_mut() {
                        inline_expression(parameter, inline_statements);
                    }
                }

                sway::Expression::FunctionCallBlock(function_call_block) => {
                    inline_expression(&mut function_call_block.function, inline_statements);

                    for parameter in function_call_block.parameters.iter_mut() {
                        inline_expression(parameter, inline_statements);
                    }

                    for field in function_call_block.fields.iter_mut() {
                        inline_expression(&mut field.value, inline_statements);
                    }
                }

                sway::Expression::Block(block) => {
                    for i in 0..block.statements.len() {
                        inline_statement(&mut block.statements, i, inline_statements);
                    }

                    if let Some(final_expr) = block.final_expr.as_mut() {
                        inline_expression(final_expr, inline_statements);
                    }
                }

                sway::Expression::Return(expression) => {
                    if let Some(expression) = expression.as_mut() {
                        inline_expression(expression, inline_statements);
                    }
                }

                sway::Expression::Array(array) => {
                    for expr in array.elements.iter_mut() {
                        inline_expression(expr, inline_statements);
                    }
                }

                sway::Expression::ArrayAccess(array_access) => {
                    inline_expression(&mut array_access.expression, inline_statements);
                    inline_expression(&mut array_access.index, inline_statements);
                }

                sway::Expression::MemberAccess(member_access) => {
                    inline_expression(&mut member_access.expression, inline_statements);
                }

                sway::Expression::Tuple(expressions) => {
                    for expression in expressions.iter_mut() {
                        inline_expression(expression, inline_statements);
                    }
                }

                sway::Expression::If(if_expr) => {
                    let mut current_if = Some(if_expr);

                    while let Some(if_expr) = current_if.take() {
                        if let Some(condition) = if_expr.condition.as_mut() {
                            inline_expression(condition, inline_statements);
                        }

                        for i in 0..if_expr.then_body.statements.len() {
                            inline_statement(&mut if_expr.then_body.statements, i, inline_statements);
                        }

                        if let Some(final_expr) = if_expr.then_body.final_expr.as_mut() {
                            inline_expression(final_expr, inline_statements);
                        }

                        current_if = if_expr.else_if.as_mut();
                    }
                }

                sway::Expression::Match(match_expr) => {
                    inline_expression(&mut match_expr.expression, inline_statements);

                    for branch in match_expr.branches.iter_mut() {
                        inline_expression(&mut branch.pattern, inline_statements);
                        inline_expression(&mut branch.value, inline_statements);
                    }
                }

                sway::Expression::While(while_expr) => {
                    inline_expression(&mut while_expr.condition, inline_statements);

                    for i in 0..while_expr.body.statements.len() {
                        inline_statement(&mut while_expr.body.statements, i, inline_statements);
                    }

                    if let Some(final_expr) = while_expr.body.final_expr.as_mut() {
                        inline_expression(final_expr, inline_statements);
                    }
                }

                sway::Expression::UnaryExpression(unary_expression) => {
                    inline_expression(&mut unary_expression.expression, inline_statements);
                }

                sway::Expression::BinaryExpression(binary_expression) => {
                    inline_expression(&mut binary_expression.lhs, inline_statements);
                    inline_expression(&mut binary_expression.rhs, inline_statements);
                }

                sway::Expression::Constructor(constructor) => {
                    for field in constructor.fields.iter_mut() {
                        inline_expression(&mut field.value, inline_statements);
                    }
                }

                sway::Expression::Continue => {}
                sway::Expression::Break => {}

                sway::Expression::AsmBlock(asm_block) => {
                    for register in asm_block.registers.iter_mut() {
                        if let Some(value) = register.value.as_mut() {
                            inline_expression(value, inline_statements);
                        }
                    }
                }

                sway::Expression::Commented(_, expression) => {
                    inline_expression(expression, inline_statements);
                }
            }
        }

        fn inline_statement(
            statements: &mut Vec<sway::Statement>,
            statement_index: usize,
            inline_statements: &[sway::Statement],
        ) {
            match &mut statements[statement_index] {
                sway::Statement::Let(let_stmt) => {
                    inline_expression(&mut let_stmt.value, inline_statements);
                }

                sway::Statement::Expression(expression) => {
                    if let Some(ident) = expression.as_identifier()
                        && ident == "_"
                    {
                        statements.remove(statement_index);

                        for statement in inline_statements.iter().rev() {
                            statements.insert(statement_index, statement.clone());
                        }

                        return;
                    }

                    inline_expression(expression, inline_statements);
                }

                sway::Statement::Commented(_, _) => {
                    unimplemented!()
                }
            }
        }

        // If the function's body ends in an expression, make it an explicit return statement
        let mut needs_return = false;

        let mut function_body = function_body.clone();

        if let Some(final_expr) = function_body.final_expr.take() {
            function_body
                .statements
                .push(sway::Statement::from(sway::Expression::Return(Some(Box::new(
                    final_expr,
                )))));

            needs_return = true;
        }

        // Inline the function body into the copy of the modifier body
        if let Some(final_expr) = block.final_expr.take() {
            block
                .statements
                .push(sway::Statement::from(sway::Expression::Return(Some(Box::new(
                    final_expr,
                )))));
        }

        for i in 0..block.statements.len() {
            inline_statement(&mut block.statements, i, &function_body.statements);
        }

        // Insert a comment above the inlined code
        block.statements.insert(
            0,
            sway::Statement::Commented(
                format!(
                    "inlined modifier invocation: {}({})",
                    modifier.new_name,
                    parameters_cell
                        .borrow()
                        .iter()
                        .map(|p| sway::TabbedDisplayer(p).to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                None,
            ),
        );

        // Generate a return value at the end of the inlined code if necessary
        if needs_return {
            let mut has_return = false;

            if let Some(statement) = block.statements.last()
                && let sway::Statement::Expression(expression) = statement
                && let sway::Expression::Return(_) = expression
            {
                has_return = true;
            }

            // If the last statement is a return with a value, make the value the final expression
            if has_return {
                assert!(block.final_expr.is_none() && !block.statements.is_empty());

                let Some(sway::Statement::Expression(sway::Expression::Return(value))) = block.statements.pop() else {
                    unreachable!()
                };

                if let Some(value) = value {
                    block.final_expr = Some(*value);
                }
            }
            // Otherwise if we have a return type, generate the default value for whatever the function returns
            else if let Some(return_type) = &return_type {
                block.final_expr = Some(create_value_expression(
                    project,
                    module.clone(),
                    scope.clone(),
                    return_type,
                    None,
                ));
            }
        }

        return Ok(Some((None, None, Some(block))));
    }
    Ok(None)
}
