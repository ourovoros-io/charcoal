use crate::{error::Error, ir::FunctionName, project::Project, translate::*};
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
) -> FunctionName {
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

        function_names
            .borrow_mut()
            .insert(signature.clone(), new_name);
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
        top_level_fn_name = format!(
            "{}_{}",
            contract_name.to_case(Case::Snake),
            top_level_fn_name
        );
    }

    FunctionName {
        old_name,
        top_level_fn_name,
        abi_fn_name,
    }
}

#[inline]
fn translate_function_parameters(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    contract_name: Option<&str>,
    function_definition: &solidity::FunctionDefinition,
) -> sway::ParameterList {
    // Translate the functions parameters
    let mut parameters = sway::ParameterList::default();
    let mut parameter_names = 'a'..='z';

    let scope = Rc::new(RefCell::new(ir::Scope::new(contract_name, None, None)));

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
            parameter.as_ref().map(|p| p.storage.as_ref()).flatten(),
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
            function_definition.returns[0]
                .1
                .as_ref()
                .unwrap()
                .storage
                .as_ref(),
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
                    p.as_ref().map(|p| p.storage.as_ref()).flatten(),
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
    function_definition: &solidity::FunctionDefinition,
) -> Result<ir::Function, Error> {
    let function_attributes = FunctionAttributes::from(function_definition);

    let function_name = translate_function_name(
        project,
        module.clone(),
        contract_name,
        function_definition.name.as_ref().map(|n| n.name.as_str()),
        &function_definition.params,
        &function_definition.ty,
    );

    let parameters =
        translate_function_parameters(project, module.clone(), contract_name, function_definition);

    // Create a scope for modifier invocation translations
    let scope = Rc::new(RefCell::new(ir::Scope::new(
        contract_name,
        Some(&function_name.top_level_fn_name),
        None,
    )));

    // Create the function's storage struct parameter and add it to the scope if necessary
    let mut storage_struct_parameter = None;

    if !function_attributes.is_pure
        && let Some(contract_name) = contract_name.as_ref()
    {
        storage_struct_parameter = Some(Box::new(sway::Parameter {
            is_ref: false,
            is_mut: false,
            name: "storage_struct".to_string(),
            type_name: Some(sway::TypeName::Identifier {
                name: format!("{contract_name}Storage"),
                generic_parameters: None,
            }),
        }));

        scope
            .borrow_mut()
            .add_variable(Rc::new(RefCell::new(ir::Variable {
                old_name: "".into(),
                new_name: "storage_struct".into(),
                type_name: sway::TypeName::Identifier {
                    name: format!("{contract_name}Storage"),
                    generic_parameters: None,
                },
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
        let type_name = translate_type_name(
            project,
            module.clone(),
            scope.clone(),
            &p.ty,
            p.storage.as_ref(),
        );

        scope
            .borrow_mut()
            .add_variable(Rc::new(RefCell::new(ir::Variable {
                old_name,
                new_name,
                type_name,
                ..Default::default()
            })));
    }

    let mut constructor_calls = vec![];
    let mut modifiers = vec![];

    // Translate the function's constructor/modifier invocations
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
        let new_name = translate_naming_convention(old_name.as_str(), Case::Snake);

        let parameters = base
            .args
            .as_ref()
            .map(|args| {
                args.iter()
                    .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
                    .collect::<Result<Vec<_>, _>>()
            })
            .unwrap_or_else(|| Ok(vec![]))?;

        // Check to see if base is a constructor call
        if project.is_contract_declared(module.clone(), old_name.as_str()) {
            let prefix = translate_naming_convention(old_name.as_str(), Case::Snake);
            let name = format!("{prefix}_constructor");

            constructor_calls.push(sway::FunctionCall {
                function: sway::Expression::create_identifier(name),
                generic_parameters: None,
                parameters,
            });

            continue;
        }

        // Add the base to the modifiers list
        modifiers.push(sway::FunctionCall {
            function: sway::Expression::create_identifier(new_name),
            generic_parameters: None,
            parameters,
        });
    }

    // Translate the function declaration
    let translated_function = ir::Function {
        old_name: function_name.old_name.clone(),
        new_name: function_name.top_level_fn_name.clone(),
        attributes: function_attributes.clone().into(),
        constructor_calls,
        modifiers,
        type_name: sway::TypeName::Function {
            old_name: function_name.old_name,
            new_name: function_name.top_level_fn_name,
            generic_parameters: None,
            parameters,
            storage_struct_parameter,
            return_type: translate_return_type_name(
                project,
                module.clone(),
                scope.clone(),
                function_definition,
            )
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

    let parameters = translate_function_parameters(
        project,
        module.clone(),
        Some(contract_name),
        function_definition,
    );

    // Create the scope for the body of the toplevel function
    let scope = Rc::new(RefCell::new(ir::Scope::new(
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
            type_name: Some(sway::TypeName::Identifier {
                name: format!("{contract_name}Storage"),
                generic_parameters: None,
            }),
        });

        scope
            .borrow_mut()
            .add_variable(Rc::new(RefCell::new(ir::Variable {
                old_name: "".into(),
                new_name: "storage_struct".into(),
                type_name: sway::TypeName::Identifier {
                    name: format!("{contract_name}Storage"),
                    generic_parameters: None,
                },
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
        return_type: translate_return_type_name(
            project,
            module.clone(),
            scope.clone(),
            function_definition,
        ),
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
                    p.type_name = Some(sway::TypeName::Identifier {
                        name: "String".into(),
                        generic_parameters: None,
                    });
                }
            }

            abi_fn = Some(abi_function);
        }

        if use_string {
            module.borrow_mut().ensure_use_declared("std::string::*");
        }

        sway_function
            .new_name
            .clone_from(&function_name.top_level_fn_name)
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
    assert!(!matches!(
        function_definition.ty,
        solidity::FunctionTy::Modifier
    ));

    let function_attributes = FunctionAttributes::from(function_definition);

    let function_name = translate_function_name(
        project,
        module.clone(),
        contract_name,
        function_definition.name.as_ref().map(|n| n.name.as_str()),
        &function_definition.params,
        &function_definition.ty,
    );

    let parameters =
        translate_function_parameters(project, module.clone(), contract_name, function_definition);

    // Create the scope for the body of the toplevel function
    let scope = Rc::new(RefCell::new(ir::Scope::new(
        contract_name,
        Some(&function_name.top_level_fn_name),
        None,
    )));

    // Create the function's storage struct parameter and add it to the scope if necessary
    let mut storage_struct_parameter = None;

    if !function_attributes.is_pure
        && let Some(contract_name) = contract_name.as_ref()
    {
        storage_struct_parameter = Some(sway::Parameter {
            is_ref: false,
            is_mut: false,
            name: "storage_struct".to_string(),
            type_name: Some(sway::TypeName::Identifier {
                name: format!("{contract_name}Storage"),
                generic_parameters: None,
            }),
        });

        scope
            .borrow_mut()
            .add_variable(Rc::new(RefCell::new(ir::Variable {
                old_name: "".into(),
                new_name: "storage_struct".into(),
                type_name: sway::TypeName::Identifier {
                    name: format!("{contract_name}Storage"),
                    generic_parameters: None,
                },
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
        storage_struct_parameter,
        return_type: translate_return_type_name(
            project,
            module.clone(),
            scope.clone(),
            function_definition,
        ),
        body: None,
    };

    // Convert the statements in the function's body (if any)
    let Some(solidity::Statement::Block { statements, .. }) = function_definition.body.as_ref()
    else {
        return Ok((None, None));
    };

    // Add the function parameters to the scope
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
            scope.clone(),
            &p.as_ref().unwrap().ty,
            p.as_ref().map(|p| p.storage.as_ref()).flatten(),
        );

        let translated_variable = ir::Variable {
            old_name,
            new_name,
            type_name,
            ..Default::default()
        };

        parameters.push(translated_variable.clone());

        scope
            .borrow_mut()
            .add_variable(Rc::new(RefCell::new(translated_variable)));
    }

    // Add the function's named return parameters to the scope
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
            scope.clone(),
            &return_parameter.ty,
            return_parameter.storage.as_ref(),
        );

        let translated_variable = ir::Variable {
            old_name,
            new_name,
            type_name,
            ..Default::default()
        };

        return_parameters.push(translated_variable.clone());

        scope
            .borrow_mut()
            .add_variable(Rc::new(RefCell::new(translated_variable)));
    }

    // Translate the body for the toplevel function
    let mut function_body = translate_block(
        project,
        module.clone(),
        scope.clone(),
        statements.as_slice(),
    )?;

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
        let Some(variable) = scope
            .borrow()
            .get_variable_from_new_name(&parameter.new_name)
        else {
            panic!(
                "error: Variable not found in scope: \"{}\"",
                parameter.new_name
            );
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
                    value: sway::Expression::create_identifier(parameter.new_name.clone()),
                }),
            );
        }
    }

    // Propagate the return variable declarations
    for return_parameter in return_parameters.iter().rev() {
        let scope = Rc::new(RefCell::new(ir::Scope::new(
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
            .push(sway::Statement::from(sway::Expression::Return(Some(
                Box::new(if return_parameters.len() == 1 {
                    sway::Expression::create_identifier(return_parameters[0].new_name.clone())
                } else {
                    sway::Expression::Tuple(
                        return_parameters
                            .iter()
                            .map(|p| sway::Expression::create_identifier(p.new_name.clone()))
                            .collect(),
                    )
                }),
            ))));
    }

    // Check if the final statement returns a value and change it to be the final expression of the block
    if let Some(sway::Statement::Expression(sway::Expression::Return(Some(value)))) =
        function_body.statements.last().cloned()
    {
        function_body.statements.pop();
        function_body.final_expr = Some(*value);
    }

    // Propagate constructor calls into the function's body
    if let Some(function_constructor_calls) = module
        .borrow()
        .function_constructor_calls
        .get(&function_name.top_level_fn_name)
    {
        for constructor_call in function_constructor_calls.iter().rev() {
            function_body.statements.insert(
                0,
                sway::Statement::from(sway::Expression::from(constructor_call.clone())),
            );
        }
    }

    // Propagate modifier pre and post functions into the function's body
    let mut modifier_pre_calls = vec![];
    let mut modifier_post_calls = vec![];

    if let Some(modifier_invocations) = module
        .borrow()
        .function_modifiers
        .get(&function_name.top_level_fn_name)
    {
        for modifier_invocation in modifier_invocations.iter() {
            let Some(new_name) = modifier_invocation.function.as_identifier() else {
                panic!("Malformed modifier invocation: {modifier_invocation:#?}");
            };

            let module = module.borrow();
            let Some(modifier) = module.modifiers.iter().find(|v| v.new_name == *new_name) else {
                panic!("Failed to find modifier: {new_name}");
            };

            if modifier.pre_body.is_some() && modifier.post_body.is_some() {
                modifier_pre_calls.push(sway::FunctionCall {
                    function: sway::Expression::create_identifier(format!(
                        "{}_pre",
                        modifier.new_name
                    )),
                    generic_parameters: None,
                    parameters: modifier_invocation.parameters.clone(),
                });

                modifier_post_calls.push(sway::FunctionCall {
                    function: sway::Expression::create_identifier(format!(
                        "{}_post",
                        modifier.new_name
                    )),
                    generic_parameters: None,
                    parameters: modifier_invocation.parameters.clone(),
                });
            } else if modifier.pre_body.is_some() {
                modifier_pre_calls.push(sway::FunctionCall {
                    function: sway::Expression::create_identifier(modifier.new_name.clone()),
                    generic_parameters: None,
                    parameters: modifier_invocation.parameters.clone(),
                });
            } else if modifier.post_body.is_some() {
                modifier_post_calls.push(sway::FunctionCall {
                    function: sway::Expression::create_identifier(modifier.new_name.clone()),
                    generic_parameters: None,
                    parameters: modifier_invocation.parameters.clone(),
                });
            } else if let Some(mut block) = modifier.inline_body.clone() {
                fn inline_expression(
                    expression: &mut sway::Expression,
                    inline_statements: &[sway::Statement],
                ) {
                    match expression {
                        sway::Expression::Literal(literal) => {}
                        sway::Expression::PathExpr(path_expr) => {}
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
                                    inline_statement(
                                        &mut if_expr.then_body.statements,
                                        i,
                                        inline_statements,
                                    );
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
                                inline_statement(
                                    &mut while_expr.body.statements,
                                    i,
                                    inline_statements,
                                );
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
                        sway::Statement::Commented(_, statement) => {
                            unimplemented!()
                        }
                    }
                }

                for i in 0..block.statements.len() {
                    inline_statement(&mut block.statements, i, &function_body.statements);
                }

                function_body = block;
            }
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
        .map(|a| {
            a.attributes
                .iter()
                .enumerate()
                .find(|(_, a)| a.name == "payable")
        })
        .flatten()
        .clone()
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

        let contract = project
            .find_contract(module.clone(), contract_name)
            .unwrap();

        let mut statements = vec![];

        let mut parameters = impl_function
            .parameters
            .entries
            .iter()
            .map(|p| sway::Expression::create_identifier(p.name.clone()))
            .collect::<Vec<_>>();

        // Convert parameters of `str` to `String`, since they are not allowed in abi function signatures
        for p in impl_function.parameters.entries.iter_mut() {
            let Some(sway::TypeName::StringSlice) = p.type_name else {
                continue;
            };

            module.borrow_mut().ensure_use_declared("std::string::*");

            p.type_name = Some(sway::TypeName::Identifier {
                name: "String".into(),
                generic_parameters: None,
            });

            statements.push(sway::Statement::from(sway::Let {
                pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                    is_mutable: true,
                    name: p.name.clone(),
                }),
                type_name: None,
                value: sway::Expression::create_function_calls(
                    None,
                    &[(p.name.as_str(), None), ("as_str", Some((None, vec![])))],
                ),
            }));
        }

        // If the contract has a storage struct, generate bindings and a storage struct parameter for the toplevel function call
        if let Some(storage_struct) = contract.borrow().storage_struct.as_ref() {
            let storage_namespace_name = module
                .borrow()
                .get_storage_namespace_name(scope.clone())
                .unwrap();

            statements.push(sway::Statement::from(sway::Let {
                pattern: sway::LetPattern::from(sway::LetIdentifier {
                    is_mutable: false,
                    name: "storage_struct".to_string(),
                }),
                type_name: None,
                value: sway::Expression::from(sway::Constructor {
                    type_name: sway::TypeName::Identifier {
                        name: format!("{}Storage", contract_name),
                        generic_parameters: None,
                    },
                    fields: storage_struct
                        .borrow()
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
                }),
            }));

            parameters.push(sway::Expression::create_identifier("storage_struct".into()));
        }

        impl_function.body = Some(sway::Block {
            statements,
            final_expr: Some(sway::Expression::create_function_calls(
                None,
                &[(
                    toplevel_function.new_name.as_str(),
                    Some((None, parameters)),
                )],
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
    let new_name = translate_naming_convention(old_name.as_str(), Case::Snake);

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
        attributes: None,
        has_underscore: false,
        inline_body: None,
        pre_body: None,
        post_body: None,
    };

    let scope = Rc::new(RefCell::new(ir::Scope::new(
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
            p.as_ref().map(|p| p.storage.as_ref()).flatten(),
        );

        modifier.parameters.entries.push(sway::Parameter {
            name: new_name.clone(),
            type_name: Some(type_name.clone()),
            ..Default::default()
        });

        scope
            .borrow_mut()
            .add_variable(Rc::new(RefCell::new(ir::Variable {
                old_name,
                new_name,
                type_name,
                ..Default::default()
            })));
    }

    let solidity::Statement::Block { statements, .. } = function_definition.body.as_ref().unwrap()
    else {
        panic!(
            "Invalid modifier body, expected block, found: {:#?}",
            function_definition.body
        );
    };

    let mut current_body: &mut Option<sway::Block> = &mut modifier.pre_body;
    let mut current_scope = Rc::new(RefCell::new(scope.borrow().clone()));

    let mut has_pre_storage_read = false;
    let mut has_pre_storage_write = false;

    let mut has_post_storage_read = false;
    let mut has_post_storage_write = false;

    let mut has_storage_read = &mut has_pre_storage_read;
    let mut has_storage_write = &mut has_pre_storage_write;

    for statement in statements.iter() {
        // Translate the statement
        let sway_statement =
            translate_statement(project, module.clone(), current_scope.clone(), statement)?;

        // If we encounter the underscore statement, every following statement goes into the modifier's post_body block.
        if sway_statement
            .filter_map(|s| {
                let sway::Statement::Expression(expression) = s else {
                    return None;
                };
                if let Some("_") = expression.as_identifier() {
                    return Some(expression.clone());
                }
                None
            })
            .is_some()
        {
            modifier.has_underscore = true;

            if let Some(block) = current_body.as_mut() {
                //
                // TODO: check if any storage fields were read from or written to
                //

                let mut scope = Some(current_scope.clone());

                while let Some(current_scope) = scope.clone() {
                    // for variable in current_scope.borrow_mut().variables.iter() {
                    //     //
                    //     // TODO: check if variable is a storage key that was read from or written to
                    //     //

                    //     if *has_storage_read && *has_storage_write {
                    //         break;
                    //     }
                    // }

                    if *has_storage_read && *has_storage_write {
                        break;
                    }

                    scope.clone_from(&current_scope.borrow().get_parent())
                }

                finalize_block_translation(project, current_scope.clone(), block)?;
            }

            current_body = &mut modifier.post_body;

            let mut new_scope = ir::Scope::new(
                scope
                    .borrow()
                    .get_contract_name()
                    .as_ref()
                    .map(|s| s.as_str()),
                Some(&new_name),
                scope.borrow().get_parent(),
            );

            for v in scope.borrow().get_variables() {
                let mut v = v.borrow().clone();
                v.statement_index = None;
                new_scope.add_variable(Rc::new(RefCell::new(v)));
            }

            current_scope = Rc::new(RefCell::new(new_scope));

            has_storage_read = &mut has_post_storage_read;
            has_storage_write = &mut has_post_storage_write;

            continue;
        }

        // Create the current body block if it hasn't already been
        if current_body.is_none() {
            *current_body = Some(sway::Block::default());
        }

        let block = current_body.as_mut().unwrap();

        // Store the index of the sway statement
        let statement_index = block.statements.len();

        // Add the sway statement to the sway block
        block.statements.push(sway_statement);

        // If the sway statement is a variable declaration, keep track of its statement index
        if let Some(sway::Statement::Let(sway_variable)) = block.statements.last() {
            let store_variable_statement_index = |id: &sway::LetIdentifier| {
                let scope = current_scope.borrow_mut();
                let scope_variables = scope.get_variables();
                let scope_entry = scope_variables
                    .iter()
                    .rev()
                    .find(|v| v.borrow().new_name == id.name)
                    .unwrap();
                scope_entry.borrow_mut().statement_index = Some(statement_index);
            };

            match &sway_variable.pattern {
                sway::LetPattern::Identifier(id) => store_variable_statement_index(id),
                sway::LetPattern::Tuple(ids) => ids.iter().for_each(store_variable_statement_index),
            }
        }
    }

    if let Some(block) = current_body.as_mut() {
        finalize_block_translation(project, current_scope.clone(), block)?;
    }

    let create_attributes =
        |has_storage_read: bool, has_storage_write: bool| -> Option<sway::AttributeList> {
            let mut parameters = vec![];

            if has_storage_read {
                parameters.push("read".into());
            }

            if has_storage_write {
                parameters.push("write".into());
            }

            if parameters.is_empty() {
                None
            } else {
                Some(sway::AttributeList {
                    attributes: vec![sway::Attribute {
                        name: "storage".into(),
                        parameters: Some(parameters),
                    }],
                })
            }
        };

    // Ensure that an underscore statement was encountered while translating the modifier
    if !modifier.has_underscore {
        panic!(
            "Malformed modifier missing underscore statement: {}",
            modifier.old_name
        );
    }

    // Generate toplevel modifier functions
    match (modifier.pre_body.as_ref(), modifier.post_body.as_ref()) {
        (Some(pre_body), Some(post_body)) => {
            let modifier_pre_function_name = format!("{}_pre", modifier.new_name);

            module.borrow_mut().functions.push(ir::Item {
                signature: sway::TypeName::Function {
                    old_name: String::new(),
                    new_name: String::new(),
                    generic_parameters: None,
                    parameters: modifier.parameters.clone(),
                    storage_struct_parameter: None,
                    return_type: None,
                },
                implementation: Some(sway::Function {
                    attributes: create_attributes(has_pre_storage_read, has_pre_storage_write),
                    is_public: true,
                    old_name: String::new(), // TODO
                    new_name: modifier_pre_function_name.clone(),
                    generic_parameters: None,
                    parameters: modifier.parameters.clone(),
                    storage_struct_parameter: None,
                    return_type: None,
                    body: Some(pre_body.clone()),
                }),
            });

            let modifier_post_function_name = format!("{}_post", modifier.new_name);

            module.borrow_mut().functions.push(ir::Item {
                signature: sway::TypeName::Function {
                    old_name: String::new(),
                    new_name: String::new(),
                    generic_parameters: None,
                    parameters: modifier.parameters.clone(),
                    storage_struct_parameter: None,
                    return_type: None,
                },
                implementation: Some(sway::Function {
                    attributes: create_attributes(has_post_storage_read, has_post_storage_write),
                    is_public: true,
                    old_name: String::new(), // TODO
                    new_name: modifier_post_function_name.clone(),
                    generic_parameters: None,
                    parameters: modifier.parameters.clone(),
                    storage_struct_parameter: None,
                    return_type: None,
                    body: Some(post_body.clone()),
                }),
            });
        }

        (Some(pre_body), None) => {
            module.borrow_mut().functions.push(ir::Item {
                signature: sway::TypeName::Function {
                    old_name: String::new(),
                    new_name: String::new(),
                    generic_parameters: None,
                    parameters: modifier.parameters.clone(),
                    storage_struct_parameter: None,
                    return_type: None,
                },
                implementation: Some(sway::Function {
                    attributes: create_attributes(has_pre_storage_read, has_pre_storage_write),
                    is_public: true,
                    old_name: modifier.old_name.clone(),
                    new_name: modifier.new_name.clone(),
                    generic_parameters: None,
                    parameters: modifier.parameters.clone(),
                    storage_struct_parameter: None,
                    return_type: None,
                    body: Some(pre_body.clone()),
                }),
            });
        }

        (None, Some(post_body)) => {
            module.borrow_mut().functions.push(ir::Item {
                signature: sway::TypeName::Function {
                    old_name: String::new(),
                    new_name: String::new(),
                    generic_parameters: None,
                    parameters: modifier.parameters.clone(),
                    storage_struct_parameter: None,
                    return_type: None,
                },
                implementation: Some(sway::Function {
                    attributes: create_attributes(has_post_storage_read, has_post_storage_write),
                    is_public: true,
                    old_name: modifier.old_name.clone(),
                    new_name: modifier.new_name.clone(),
                    generic_parameters: None,
                    parameters: modifier.parameters.clone(),
                    storage_struct_parameter: None,
                    return_type: None,
                    body: Some(post_body.clone()),
                }),
            });
        }

        (None, None) => {
            if !function_definition.params.is_empty() {
                todo!("Support inlining modifier with parameters");
            }

            let block = translate_block(project, module.clone(), scope.clone(), statements)?;
            modifier.inline_body = Some(block);
        }
    }

    // Add the translated modifier to the translated definition
    module.borrow_mut().modifiers.push(modifier);

    Ok(())
}

#[inline(always)]
pub fn ensure_constructor_functions_exist(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    contract: Rc<RefCell<ir::Contract>>,
) {
    // Create the constructor if it doesn't exist
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

    assert!(
        module
            .borrow()
            .functions
            .iter()
            .find(|f| {
                let sway::TypeName::Function { new_name, .. } = &f.signature else {
                    unreachable!()
                };
                *new_name == function_name.top_level_fn_name
            })
            .is_none()
    );

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

    let storage_namespace_name = module
        .borrow()
        .get_storage_namespace_name(scope.clone())
        .unwrap();

    impl_function.body = Some(sway::Block {
        statements: vec![sway::Statement::from(sway::Let {
            pattern: sway::LetPattern::from(sway::LetIdentifier {
                is_mutable: false,
                name: "storage_struct".to_string(),
            }),
            type_name: None,
            value: sway::Expression::from(sway::Constructor {
                type_name: sway::TypeName::Identifier {
                    name: format!("{}Storage", contract_name),
                    generic_parameters: None,
                },
                fields: contract
                    .borrow()
                    .storage_struct
                    .as_ref()
                    .unwrap()
                    .borrow()
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
            }),
        })],
        final_expr: Some(sway::Expression::create_function_calls(
            None,
            &[(
                function_name.top_level_fn_name.as_str(),
                Some((
                    None,
                    vec![sway::Expression::create_identifier("storage_struct".into())],
                )),
            )],
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
        type_name: Some(sway::TypeName::Identifier {
            name: format!("{}Storage", contract_name),
            generic_parameters: None,
        }),
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
    contract
        .borrow_mut()
        .abi
        .functions
        .insert(0, abi_function.clone());

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
                type_name: Some(sway::TypeName::Identifier {
                    name: format!("{}Storage", contract_name),
                    generic_parameters: None,
                }),
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

    // Add the `constructor_called` field to the storage struct
    module
        .borrow_mut()
        .get_storage_struct(scope.clone())
        .borrow_mut()
        .fields
        .push(sway::StructField {
            is_public: true,
            new_name: constructor_called_field_name.clone(),
            old_name: String::new(),
            type_name: sway::TypeName::Identifier {
                name: "StorageKey".to_string(),
                generic_parameters: Some(sway::GenericParameterList {
                    entries: vec![sway::GenericParameter {
                        type_name: sway::TypeName::Identifier {
                            name: "bool".into(),
                            generic_parameters: None,
                        },
                        implements: None,
                    }],
                }),
            },
        });

    // Add the `constructor_called` field to the storage block
    module
        .borrow_mut()
        .get_storage_namespace(scope.clone())
        .unwrap()
        .borrow_mut()
        .fields
        .push(sway::StorageField {
            old_name: String::new(),
            name: constructor_called_field_name.clone(),
            type_name: sway::TypeName::Identifier {
                name: "bool".into(),
                generic_parameters: None,
            },
            value: sway::Expression::from(sway::Literal::Bool(false)),
        });
}

#[inline(always)]
pub fn update_constructor_function_body(
    _project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    contract_name: Option<&str>,
    function_body: &mut sway::Block,
) {
    let constructor_called_field_name = "constructor_called".to_string();

    let storage_namespace = module
        .borrow_mut()
        .get_storage_namespace(scope.clone())
        .unwrap();

    if !storage_namespace
        .borrow()
        .fields
        .iter()
        .any(|s| s.name == constructor_called_field_name)
    {
        // Add the `constructor_called` field to the storage struct
        module
            .borrow_mut()
            .get_storage_struct(scope.clone())
            .borrow_mut()
            .fields
            .push(sway::StructField {
                is_public: true,
                new_name: constructor_called_field_name.clone(),
                old_name: String::new(),
                type_name: sway::TypeName::Identifier {
                    name: "StorageKey".to_string(),
                    generic_parameters: Some(sway::GenericParameterList {
                        entries: vec![sway::GenericParameter {
                            type_name: sway::TypeName::Identifier {
                                name: "bool".into(),
                                generic_parameters: None,
                            },
                            implements: None,
                        }],
                    }),
                },
            });

        // Add the `constructor_called` field to the storage block
        storage_namespace
            .borrow_mut()
            .fields
            .push(sway::StorageField {
                old_name: String::new(),
                name: constructor_called_field_name.clone(),
                type_name: sway::TypeName::Identifier {
                    name: "bool".into(),
                    generic_parameters: None,
                },
                value: sway::Expression::from(sway::Literal::Bool(false)),
            });
    }

    // Add the `constructor_called` requirement to the beginning of the function
    // require(!storage.initialized.read(), "The Contract constructor has already been called");
    function_body.statements.insert(
        0,
        sway::Statement::from(sway::Expression::create_function_calls(
            None,
            &[(
                "require",
                Some((
                    None,
                    vec![
                        sway::Expression::from(sway::UnaryExpression {
                            operator: "!".into(),
                            expression: sway::Expression::create_function_calls(
                                None,
                                &[
                                    ("storage_struct", None),
                                    (constructor_called_field_name.as_str(), None),
                                    ("read", Some((None, vec![]))),
                                ],
                            ),
                        }),
                        sway::Expression::from(sway::Literal::String(format!(
                            "The {} constructor has already been called",
                            contract_name
                                .map(|s| s.to_string())
                                .unwrap_or_else(|| module.borrow().name.clone()),
                        ))),
                    ],
                )),
            )],
        )),
    );

    // Set the `constructor_called` storage field to `true` at the end of the function
    // storage.initialized.write(true);
    function_body.statements.push(sway::Statement::from(
        sway::Expression::create_function_calls(
            None,
            &[
                ("storage_struct", None),
                (constructor_called_field_name.as_str(), None),
                (
                    "write",
                    Some((
                        None,
                        vec![sway::Expression::from(sway::Literal::Bool(true))],
                    )),
                ),
            ],
        ),
    ));
}
