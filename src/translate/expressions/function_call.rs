use crate::{error::Error, project::Project, sway, translate::*};
use solang_parser::helpers::CodeLocation;
use std::{cell::RefCell, rc::Rc};

#[inline(always)]
pub fn translate_function_call_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    expression: &solidity::Expression,
    function: &solidity::Expression,
    named_arguments: Option<&[solidity::NamedArgument]>,
    arguments: &[solidity::Expression],
) -> Result<sway::Expression, Error> {
    if named_arguments.is_some() && !arguments.is_empty() {
        panic!(
            "Invalid call to translate_function_call_expression: named_arguments is Some(_) and arguments is not empty"
        );
    }

    // println!(
    //     "Translating function call: {expression}; from {}",
    //     project.loc_to_file_location_string(module.clone(), &expression.loc()),
    // );

    match function {
        solidity::Expression::Type(_, _) => translate_type_function_call(
            project,
            module.clone(),
            scope.clone(),
            function,
            arguments,
            named_arguments,
        ),

        solidity::Expression::Variable(solidity::Identifier { name, .. }) => {
            translate_variable_function_call(
                project,
                module.clone(),
                scope.clone(),
                expression,
                name.as_str(),
                arguments,
                named_arguments,
            )
        }

        solidity::Expression::MemberAccess(_, container, member) => {
            translate_member_access_function_call(
                project,
                module.clone(),
                scope.clone(),
                expression,
                container.as_ref(),
                member,
                arguments,
                named_arguments,
            )
        }

        solidity::Expression::FunctionCall(_, function, args) => {
            translate_function_call_function_call(
                project,
                module.clone(),
                scope.clone(),
                function,
                args,
                expression,
                arguments,
            )
        }

        solidity::Expression::FunctionCallBlock(_, function, block) => {
            let mut function = function.clone();

            if let solidity::Expression::Parenthesis(_, expr) = function.as_ref() {
                function = expr.clone();
            }

            match function.as_ref() {
                solidity::Expression::MemberAccess(_, container, member) => {
                    translate_function_call_block_member_access_function_call(
                        project,
                        module.clone(),
                        scope.clone(),
                        container,
                        member,
                        block,
                        arguments,
                    )
                }

                _ => translate_new_expression(project, module.clone(), scope.clone(), expression),
            }
        }

        _ => todo!("translate function call expression: {expression}"),
    }
}

#[inline(always)]
pub fn translate_function_call_block_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    function: &solidity::Expression,
    block: &solidity::Statement,
) -> Result<sway::Expression, Error> {
    if block.is_empty() {
        return translate_expression(project, module.clone(), scope.clone(), function);
    }

    panic!(
        "{}: TODO: Translate function call block expression: `{function}{block}`",
        project.loc_to_file_location_string(module.clone(), &function.loc()),
    )
}

#[inline(always)]
fn translate_type_function_call(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    expression: &solidity::Expression,
    arguments: &[solidity::Expression],
    named_arguments: Option<&[solidity::NamedArgument]>,
) -> Result<sway::Expression, Error> {
    assert!(named_arguments.is_none());

    // Type casting
    if arguments.len() != 1 {
        panic!("Invalid type cast expression: {expression}");
    }

    // Check for `address(this)`
    if let solidity::Expression::Type(
        _,
        solidity::Type::Address | solidity::Type::AddressPayable | solidity::Type::Payable,
    ) = expression
        && let solidity::Expression::Variable(ident) = &arguments[0]
        && ident.name == "this"
    {
        return Ok(sway::Expression::create_function_calls(
            None,
            &[(
                "Identity::ContractId",
                Some((
                    None,
                    vec![sway::Expression::create_function_calls(
                        None,
                        &[("ContractId::this", Some((None, vec![])))],
                    )],
                )),
            )],
        ));
    }

    let from_expression =
        translate_expression(project, module.clone(), scope.clone(), &arguments[0])?;

    let from_type_name =
        get_expression_type(project, module.clone(), scope.clone(), &from_expression)?;

    let to_type_name =
        translate_type_name(project, module.clone(), scope.clone(), expression, None);

    Ok(coerce_expression(
        project,
        module.clone(),
        scope.clone(),
        &from_expression,
        &from_type_name,
        &to_type_name,
    )
    .unwrap())
}

fn translate_variable_function_call(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    expression: &solidity::Expression,
    name: &str,
    arguments: &[solidity::Expression],
    named_arguments: Option<&[solidity::NamedArgument]>,
) -> Result<sway::Expression, Error> {
    if name == "type" {
        return Ok(sway::Expression::create_todo(Some(format!(
            "type({})",
            arguments
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        ))));
    }

    let parameters = arguments
        .iter()
        .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
        .collect::<Result<Vec<_>, _>>()?;

    // Check to see if the function is a built-in function
    if let Some(expression) = translate_builtin_function_call(
        project,
        module.clone(),
        scope.clone(),
        expression,
        name,
        parameters.clone(),
    )? {
        return Ok(expression);
    }

    let parameter_types = parameters
        .iter()
        .map(|p| get_expression_type(project, module.clone(), scope.clone(), p))
        .collect::<Result<Vec<_>, _>>()?;

    // Check to see if the expression is a by-value struct constructor
    let structs = {
        let module = module.borrow();
        module.structs.clone()
    };

    if let Some(result) = resolve_struct_constructor(
        project,
        module.clone(),
        scope.clone(),
        structs.as_slice(),
        name,
        named_arguments,
        parameters.clone(),
        parameter_types.clone(),
    )? {
        return Ok(result);
    }

    // Check to see if the expression is an ABI cast
    if parameters.len() == 1 && project.is_contract_declared(module.clone(), name) {
        match get_expression_type(project, module.clone(), scope.clone(), &parameters[0])? {
            sway::TypeName::Identifier {
                name: type_name,
                generic_parameters,
            } => match (type_name.as_str(), generic_parameters.as_ref()) {
                ("Identity", None) => {
                    // abi(T, x.bits())
                    return Ok(sway::Expression::create_function_calls(
                        None,
                        &[(
                            "abi",
                            Some((
                                None,
                                vec![
                                    sway::Expression::create_identifier(name.into()),
                                    sway::Expression::create_function_calls(
                                        Some(parameters[0].clone()),
                                        &[("bits", Some((None, vec![])))],
                                    ),
                                ],
                            )),
                        )],
                    ));
                }

                ("ContractId", None) => {
                    // abi(T, x.into())
                    return Ok(sway::Expression::create_function_calls(
                        None,
                        &[(
                            "abi",
                            Some((
                                None,
                                vec![
                                    sway::Expression::create_identifier(name.into()),
                                    sway::Expression::create_function_calls(
                                        Some(parameters[0].clone()),
                                        &[("into", Some((None, vec![])))],
                                    ),
                                ],
                            )),
                        )],
                    ));
                }

                ("u256" | "b256", None) => {
                    // Thing(x) => abi(Thing, Identity::from(ContractId::from(x)))
                    let mut value = parameters[0].clone();

                    if type_name == "u256" {
                        value = sway::Expression::create_function_calls(
                            Some(value),
                            &[("into", Some((None, vec![])))],
                        )
                    }

                    // abi(T, Identity::from(ContractId::from(x)))
                    return Ok(sway::Expression::create_function_calls(
                        None,
                        &[(
                            "abi",
                            Some((
                                None,
                                vec![sway::Expression::create_identifier(name.into()), value],
                            )),
                        )],
                    ));
                }

                _ => {}
            },

            sway::TypeName::Abi { .. } => {
                // abi(T, x.bits())
                return Ok(sway::Expression::create_function_calls(
                    None,
                    &[(
                        "abi",
                        Some((
                            None,
                            vec![
                                sway::Expression::create_identifier(name.into()),
                                sway::Expression::create_function_calls(
                                    Some(parameters[0].clone()),
                                    &[("bits", Some((None, vec![])))],
                                ),
                            ],
                        )),
                    )],
                ));
            }

            _ => {}
        }
    }

    // Try to resolve the function call
    if let Some(result) = resolve_function_call(
        project,
        module.clone(),
        scope.clone(),
        name,
        named_arguments,
        parameters.clone(),
        parameter_types.clone(),
    )? {
        return Ok(result);
    }

    // Try to resolve the function call under the current contract
    let contract_name = scope.borrow().get_contract_name();
    if let Some(contract_name) = contract_name
        && let Some(contract) = project.find_contract(module.clone(), &contract_name)
    {
        let abi = contract.borrow().abi.clone();

        if let Some(result) = resolve_abi_function_call(
            project,
            module.clone(),
            scope.clone(),
            &abi,
            None,
            name,
            named_arguments,
            parameters.clone(),
            parameter_types.clone(),
        )? {
            return Ok(result);
        }
    }

    // Check all of the module's `use` statements for crate-local imports,
    // find the module being imported, then check if the function lives there.
    for use_item in module.borrow().uses.iter() {
        // Try to resolve the function call
        if let Some(found_module) = project.resolve_use(use_item)
            && let Some(result) = resolve_function_call(
                project,
                found_module.clone(),
                scope.clone(),
                name,
                named_arguments,
                parameters.clone(),
                parameter_types.clone(),
            )?
        {
            return Ok(result);
        }
    }

    panic!(
        "{}: ERROR: Failed to find function `{name}({})` in scope: {name}({})",
        project.loc_to_file_location_string(module.clone(), &expression.loc()),
        parameter_types
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(", "),
        parameters
            .iter()
            .map(|t| sway::TabbedDisplayer(t).to_string())
            .collect::<Vec<_>>()
            .join(", "),
    )
}

#[inline(always)]
fn translate_builtin_function_call(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    expression: &solidity::Expression,
    name: &str,
    mut parameters: Vec<sway::Expression>,
) -> Result<Option<sway::Expression>, Error> {
    match name {
        "blockhash" => {
            // blockhash(block_number) => std::block::block_header_hash(block_height).unwrap_or(0)

            if parameters.len() != 1 {
                panic!("Invalid blockhash call: {expression:#?}");
            }

            Ok(Some(sway::Expression::create_function_calls(
                None,
                &[
                    ("std::block::block_header_hash", Some((None, parameters))),
                    (
                        "unwrap_or",
                        Some((
                            None,
                            vec![sway::Expression::from(sway::Literal::DecInt(
                                BigUint::zero(),
                                None,
                            ))],
                        )),
                    ),
                ],
            )))
        }

        "gasleft" => {
            // gasleft() => std::registers::global_gas()

            if !parameters.is_empty() {
                panic!("Invalid gasleft call: {expression:#?}");
            }

            Ok(Some(sway::Expression::create_function_calls(
                None,
                &[("std::registers::global_gas", Some((None, parameters)))],
            )))
        }

        "addmod" => {
            // addmod(x, y, k) => (x + y) % k

            if parameters.len() != 3 {
                panic!("Invalid addmod call: {expression:#?}");
            }

            Ok(Some(sway::Expression::from(sway::BinaryExpression {
                operator: "%".into(),
                lhs: sway::Expression::Tuple(vec![sway::Expression::from(
                    sway::BinaryExpression {
                        operator: "+".into(),
                        lhs: parameters[0].clone(),
                        rhs: parameters[1].clone(),
                    },
                )]),
                rhs: parameters[2].clone(),
            })))
        }

        "mulmod" => {
            // mulmod(x, y, k) => (x * y) % k

            if parameters.len() != 3 {
                panic!("Invalid mulmod call: {expression:#?}");
            }

            Ok(Some(sway::Expression::from(sway::BinaryExpression {
                operator: "%".into(),
                lhs: sway::Expression::Tuple(vec![sway::Expression::from(
                    sway::BinaryExpression {
                        operator: "*".into(),
                        lhs: parameters[0].clone(),
                        rhs: parameters[1].clone(),
                    },
                )]),
                rhs: parameters[2].clone(),
            })))
        }

        "keccak256" => {
            // keccak256(value) => std::hash::keccak256(value)

            if parameters.len() != 1 {
                panic!("Invalid keccak256 call: {expression:#?}");
            }

            Ok(Some(sway::Expression::create_function_calls(
                None,
                &[("std::hash::keccak256", Some((None, parameters)))],
            )))
        }

        "sha256" => {
            // sha256(value) => std::hash::sha256(value)

            if parameters.len() != 1 {
                panic!("Invalid sha256 call: {expression:#?}");
            }

            Ok(Some(sway::Expression::create_function_calls(
                None,
                &[("std::hash::sha256", Some((None, parameters)))],
            )))
        }

        "ripemd160" => {
            // ripemd160() => /*unsupported: block.basefee; using:*/ 0

            Ok(Some(sway::Expression::Commented(
                "unsupported: ripemd160(); using:".into(),
                Box::new(sway::Expression::from(sway::Literal::DecInt(
                    BigUint::zero(),
                    None,
                ))),
            )))
        }

        "ecrecover" => {
            // ecrecover(hash, v, r, s) => Identity::Address(Address::from(Secp256k1::from((r, s)).address(Message::from(hash)).unwrap()))
            if parameters.len() != 4 {
                panic!("Invalid ecrecover call: {expression:#?}");
            }

            module
                .borrow_mut()
                .ensure_use_declared("std::crypto::secp256k1::Secp256k1");
            module
                .borrow_mut()
                .ensure_use_declared("std::crypto::message::Message");

            Ok(Some(sway::Expression::create_function_calls(
                None,
                &[(
                    "Identity::Address",
                    Some((
                        None,
                        vec![sway::Expression::create_function_calls(
                            None,
                            &[(
                                "Address::from",
                                Some((
                                    None,
                                    vec![sway::Expression::create_function_calls(
                                        None,
                                        &[
                                            (
                                                "Secp256k1::from",
                                                Some((
                                                    None,
                                                    vec![sway::Expression::Tuple(vec![
                                                        parameters[2].clone(),
                                                        parameters[3].clone(),
                                                    ])],
                                                )),
                                            ),
                                            (
                                                "address",
                                                Some((
                                                    None,
                                                    vec![sway::Expression::create_function_calls(
                                                        None,
                                                        &[(
                                                            "Message::from",
                                                            Some((
                                                                None,
                                                                vec![parameters[0].clone()],
                                                            )),
                                                        )],
                                                    )],
                                                )),
                                            ),
                                            ("unwrap", Some((None, vec![]))),
                                        ],
                                    )],
                                )),
                            )],
                        )],
                    )),
                )],
            )))
        }

        "selfdestruct" => {
            //
            // TODO: how should we handle this?
            //

            Ok(Some(sway::Expression::create_unimplemented(Some(
                "selfdestruct is not supported in sway".into(),
            ))))
        }

        "assert" => {
            // assert(x) => assert(x)

            if parameters.len() != 1 {
                panic!("Invalid assert call: {expression:#?}");
            }

            Ok(Some(sway::Expression::create_function_calls(
                None,
                &[("assert", Some((None, parameters)))],
            )))
        }

        "require" => {
            // require(x) => require(x, "Requirement failed: x")
            // require(x, "msg") => require(x, "msg")

            if parameters.len() == 1 {
                parameters.push(sway::Expression::from(sway::Literal::String(format!(
                    "Requirement failed: {}",
                    sway::TabbedDisplayer(&parameters[0])
                ))));
            }

            if parameters.len() != 2 {
                panic!("Invalid require call: {expression:#?}");
            }

            let parameter_type =
                get_expression_type(project, module.clone(), scope.clone(), &parameters[0])?;

            parameters[0] = coerce_expression(
                project,
                module.clone(),
                scope.clone(),
                &parameters[0],
                &parameter_type,
                &sway::TypeName::Identifier {
                    name: "bool".into(),
                    generic_parameters: None,
                },
            )
            .unwrap();

            Ok(Some(sway::Expression::create_function_calls(
                None,
                &[("require", Some((None, parameters)))],
            )))
        }

        "revert" => {
            // revert() => revert(0)
            // revert("msg") => {
            //     log("msg");
            //     revert(0);
            // }

            if parameters.is_empty() {
                return Ok(Some(sway::Expression::create_function_calls(
                    None,
                    &[(
                        "assert",
                        Some((
                            None,
                            vec![sway::Expression::from(sway::Literal::DecInt(
                                BigUint::zero(),
                                None,
                            ))],
                        )),
                    )],
                )));
            }

            if parameters.len() != 1 {
                panic!("Invalid revert call: {expression:#?}");
            }

            Ok(Some(sway::Expression::from(sway::Block {
                statements: vec![
                    sway::Statement::from(sway::Expression::create_function_calls(
                        None,
                        &[("log", Some((None, parameters)))],
                    )),
                    sway::Statement::from(sway::Expression::create_function_calls(
                        None,
                        &[(
                            "revert",
                            Some((
                                None,
                                vec![sway::Expression::from(sway::Literal::DecInt(
                                    BigUint::zero(),
                                    None,
                                ))],
                            )),
                        )],
                    )),
                ],

                final_expr: None,
            })))
        }

        _ => Ok(None),
    }
}

#[inline(always)]
fn translate_builtin_type_member_access_function_call(
    _project: &mut Project,
    _module: Rc<RefCell<ir::Module>>,
    _scope: Rc<RefCell<ir::Scope>>,
    ty: &solidity::Type,
    member: &str,
    arguments: &[solidity::Expression],
    named_arguments: Option<&[solidity::NamedArgument]>,
) -> Result<Option<sway::Expression>, Error> {
    assert!(named_arguments.is_none());

    match ty {
        solidity::Type::String => match member {
            "concat" => {
                // string.concat(x) => ???

                //
                // TODO: how should this be handled?
                //

                Ok(Some(sway::Expression::create_todo(Some(format!(
                    "string.concat({})",
                    arguments
                        .iter()
                        .map(|p| p.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )))))
            }

            _ => Ok(None),
        },

        solidity::Type::DynamicBytes => match member {
            "concat" => {
                // bytes.concat(x) => ???

                //
                // TODO: how should this be handled?
                //

                Ok(Some(sway::Expression::create_todo(Some(format!(
                    "bytes.concat({})",
                    arguments
                        .iter()
                        .map(|p| p.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )))))
            }

            _ => Ok(None),
        },

        _ => Ok(None),
    }
}

#[inline(always)]
fn translate_builtin_variable_member_access_function_call(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    name: &str,
    member: &str,
    arguments: &[solidity::Expression],
    named_arguments: Option<&[solidity::NamedArgument]>,
) -> Result<Option<sway::Expression>, Error> {
    match name {
        "abi" => translate_builtin_abi_member_access_function_call(
            project,
            module.clone(),
            scope.clone(),
            member,
            arguments,
            named_arguments,
        ),

        "super" => translate_super_member_access_function_call(
            project,
            module.clone(),
            scope.clone(),
            member,
            arguments,
            named_arguments,
        ),

        "this" => translate_this_member_access_function_call(
            project,
            module.clone(),
            scope.clone(),
            member,
            arguments,
            named_arguments,
        ),

        _ => Ok(None),
    }
}

#[inline(always)]
fn translate_member_access_function_call(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    expression: &solidity::Expression,
    container: &solidity::Expression,
    member: &solidity::Identifier,
    arguments: &[solidity::Expression],
    named_arguments: Option<&[solidity::NamedArgument]>,
) -> Result<sway::Expression, Error> {
    // Check for built-in type member access function calls
    if let solidity::Expression::Type(_, ty) = container {
        if let Some(result) = translate_builtin_type_member_access_function_call(
            project,
            module.clone(),
            scope.clone(),
            ty,
            member.name.as_str(),
            arguments,
            named_arguments,
        )? {
            return Ok(result);
        }
    }

    // Check for built-in variable member access function calls
    if let solidity::Expression::Variable(solidity::Identifier { name, .. }) = container {
        if let Some(value) = translate_builtin_variable_member_access_function_call(
            project,
            module.clone(),
            scope.clone(),
            name.as_str(),
            member.name.as_str(),
            arguments,
            named_arguments,
        )? {
            return Ok(value);
        }

        let parameters = arguments
            .iter()
            .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
            .collect::<Result<Vec<_>, _>>()?;

        let parameter_types = parameters
            .iter()
            .map(|p| get_expression_type(project, module.clone(), scope.clone(), p))
            .collect::<Result<Vec<_>, _>>()?;

        // Check for explicit super function calls
        if let Some((module, contract)) = project.find_module_and_contract(module.clone(), name) {
            let abi = contract.borrow().abi.clone();

            let scope = Rc::new(RefCell::new(ir::Scope::new(
                Some(name),
                None,
                Some(scope.clone()),
            )));

            if let Some(result) = resolve_abi_function_call(
                project,
                module.clone(),
                scope.clone(),
                &abi,
                None,
                &member.name,
                named_arguments,
                parameters.clone(),
                parameter_types.clone(),
            )? {
                return Ok(result);
            }
        }

        // Check for user-defined type value wrapping
        if let "wrap" | "unwrap" = member.name.as_str()
            && let Some(SymbolData::TypeDefinition(_)) = resolve_symbol(
                project,
                module.clone(),
                scope.clone(),
                Symbol::TypeDefinition(name.clone()),
            )
        {
            return Ok(parameters[0].clone());
        }

        // Check if function is contained in an external definition
        if let Some(external_module) = project.find_module_containing_contract(module.clone(), name)
        {
            // Check to see if the expression is a by-value struct constructor
            let structs = {
                let module = external_module.borrow();
                module.structs.clone()
            };

            if let Some(result) = resolve_struct_constructor(
                project,
                module.clone(),
                scope.clone(),
                structs.as_slice(),
                member.name.as_str(),
                named_arguments,
                parameters.clone(),
                parameter_types.clone(),
            )? {
                return Ok(result);
            }

            // Try to resolve the function call
            if let Some(result) = resolve_function_call(
                project,
                external_module.clone(),
                scope.clone(),
                member.name.as_str(),
                named_arguments,
                parameters.clone(),
                parameter_types.clone(),
            )? {
                return Ok(result);
            }
        }

        let container = translate_expression(project, module.clone(), scope.clone(), container)?;
        let type_name = get_expression_type(project, module.clone(), scope.clone(), &container)?;

        // Check to see if the member function is defined in the ABI type
        {
            let mut abi_type_name = None;

            if let sway::TypeName::Abi {
                type_name: abi_type_name2,
            } = &type_name
            {
                abi_type_name = Some(abi_type_name2.to_string());
            }

            if let Some(abi_type_name_string) = abi_type_name
                && let Some(contract) = project.find_contract(module.clone(), &abi_type_name_string)
            {
                let abi = contract.borrow().abi.clone();

                let container = coerce_expression(
                    project,
                    module.clone(),
                    scope.clone(),
                    &container,
                    &sway::TypeName::Identifier {
                        name: "Identity".to_string(),
                        generic_parameters: None,
                    },
                    &type_name,
                )
                .unwrap();

                if let Some(result) = resolve_abi_function_call(
                    project,
                    module.clone(),
                    scope.clone(),
                    &abi,
                    Some(&container),
                    member.name.as_str(),
                    named_arguments,
                    parameters.clone(),
                    parameter_types.clone(),
                )? {
                    return Ok(result);
                }
            }
        }

        // Check if the member call is an address-specific built-in function
        if type_name.is_identity() {
            match member.name.as_str() {
                "call" if arguments.len() == 1 => {
                    let payload = translate_expression(
                        project,
                        module.clone(),
                        scope.clone(),
                        &arguments[0],
                    )?;
                    return translate_address_call_expression(
                        project,
                        module.clone(),
                        scope.clone(),
                        &payload,
                        None,
                        None,
                        None,
                    );
                }

                "delegatecall" => {
                    //
                    // TODO: is delegatecall possible?
                    //

                    return Ok(sway::Expression::create_todo(Some(expression.to_string())));
                }

                "staticcall" => {
                    //
                    // TODO: is staticcall possible?
                    //

                    return Ok(sway::Expression::create_todo(Some(expression.to_string())));
                }

                _ => {}
            }
        }

        // TODO
        // Check to see if the function is from using library
        // if let Some(f) =  module.borrow().using_directives.iter().find_map(|using| using.functions.iter().find(|fnc| fnc.old_name == member.name)) {
        //     return Ok(sway::Expression::create_function_calls(Some(container), &[
        //         (&f.new_name, Some((None, parameters)))
        //     ]))
        // }
    }

    let solidity_container = container;

    let mut container = translate_expression(project, module.clone(), scope.clone(), container)?;
    let mut type_name = get_expression_type(project, module.clone(), scope.clone(), &container)?;

    // println!(
    //     "type of {} is {}",
    //     sway::TabbedDisplayer(&container),
    //     sway::TabbedDisplayer(&type_name),
    // );

    // HACK: tack `.unwrap().read()` if the container is a Option<StorageKey>
    if let Some(option_type) = type_name.option_type()
        && let Some(storage_key_type) = option_type.storage_key_type()
    {
        scope
            .borrow_mut()
            .set_function_storage_accesses(module.clone(), true, false);

        container = sway::Expression::create_function_calls(
            Some(container),
            &[
                ("unwrap", Some((None, vec![]))),
                ("read", Some((None, vec![]))),
            ],
        );
        type_name = storage_key_type;
    }
    // HACK: tack `.read()` onto the end if the container is a StorageKey
    else if let Some(storage_key_type) = type_name.storage_key_type() {
        scope
            .borrow_mut()
            .set_function_storage_accesses(module.clone(), true, false);

        container = sway::Expression::create_function_calls(
            Some(container),
            &[("read", Some((None, vec![])))],
        );
        type_name = storage_key_type;
    }

    match &type_name {
        sway::TypeName::Undefined => panic!("Undefined type name"),

        sway::TypeName::Identifier {
            name,
            generic_parameters,
        } => {
            match (name.as_str(), generic_parameters.as_ref()) {
                ("Identity", None) => {
                    return translate_identity_member_access_function_call(
                        project,
                        module.clone(),
                        scope.clone(),
                        expression,
                        arguments,
                        named_arguments,
                        container,
                        member,
                        solidity_container,
                        name.to_string(),
                    );
                }

                ("StorageVec", Some(_)) => {
                    return translate_storage_vec_member_access_function_call(
                        project,
                        module.clone(),
                        scope.clone(),
                        arguments,
                        member,
                        solidity_container,
                        &container,
                    );
                }

                ("Vec", Some(_)) => {
                    return translate_vec_member_access_function_call(
                        project,
                        module.clone(),
                        scope.clone(),
                        arguments,
                        member,
                        solidity_container,
                        &container,
                    );
                }

                _ => {}
            }

            let parameters = arguments
                .iter()
                .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
                .collect::<Result<Vec<_>, _>>()?;

            let parameter_types = parameters
                .iter()
                .map(|p| get_expression_type(project, module.clone(), scope.clone(), p))
                .collect::<Result<Vec<_>, _>>()?;

            // Check if this is a function from a using directive
            let mut using_parameters = parameters.clone();
            using_parameters.insert(0, container.clone());

            let mut using_parameter_types = parameter_types.clone();
            using_parameter_types.insert(
                0,
                get_expression_type(project, module.clone(), scope.clone(), &container)?,
            );

            let using_directives = module.borrow().using_directives.clone();
            for using_directive in using_directives {
                // Make sure the type names match
                if let Some(for_type) = using_directive.for_type.as_ref()
                    && *for_type != type_name
                {
                    let sway::TypeName::Identifier {
                        name: for_name,
                        generic_parameters: None,
                    } = &for_type
                    else {
                        continue;
                    };

                    let sway::TypeName::Identifier {
                        name: struct_name,
                        generic_parameters: None,
                    } = &type_name
                    else {
                        continue;
                    };

                    if let (Some(for_struct_definition), Some(struct_definition)) = (
                        project.find_struct(module.clone(), scope.clone(), &for_name),
                        project.find_struct(module.clone(), scope.clone(), &struct_name),
                    ) && for_struct_definition == struct_definition
                    {
                        container = coerce_expression(
                            project,
                            module.clone(),
                            scope.clone(),
                            &container,
                            for_type,
                            &type_name,
                        )
                        .unwrap();
                    } else {
                        continue;
                    }
                }

                // Look up the definition of the using directive
                let Some(external_module) = project
                    .find_module_containing_contract(module.clone(), &using_directive.library_name)
                else {
                    continue;
                };

                if let Some(result) = resolve_function_call(
                    project,
                    external_module.clone(),
                    scope.clone(),
                    member.name.as_str(),
                    named_arguments,
                    using_parameters.clone(),
                    using_parameter_types.clone(),
                )? {
                    return Ok(result);
                }
            }

            if let Some(contract) = project.find_contract(module.clone(), name) {
                let abi = contract.borrow().abi.clone();

                let container = coerce_expression(
                    project,
                    module.clone(),
                    scope.clone(),
                    &container,
                    &type_name,
                    &sway::TypeName::Identifier {
                        name: abi.name.clone(),
                        generic_parameters: None,
                    },
                )
                .unwrap();

                if let Some(result) = resolve_abi_function_call(
                    project,
                    module.clone(),
                    scope.clone(),
                    &abi,
                    Some(&container),
                    &member.name,
                    named_arguments,
                    parameters.clone(),
                    parameter_types.clone(),
                )? {
                    return Ok(result);
                }
            }

            panic!(
                "{}: TODO: translate {name} member function call: {}.{member}({})",
                project.loc_to_file_location_string(module.clone(), &expression.loc()),
                sway::TabbedDisplayer(&container),
                parameter_types
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
            )
        }

        sway::TypeName::Array { .. } => {
            //
            // TODO: Is this still necessary?
            //
            // let mut parameters = arguments
            //     .iter()
            //     .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
            //     .collect::<Result<Vec<_>, _>>()?;
            //
            // let mut parameter_types = parameters
            //     .iter()
            //     .map(|p| get_expression_type(project, module.clone(), scope.clone(), p))
            //     .collect::<Result<Vec<_>, _>>()?;
            //
            // parameters.insert(0, container.clone());
            //
            // parameter_types.insert(
            //     0,
            //     get_expression_type(project, module.clone(), scope.clone(), &container)?,
            // );
            //
            // // Check if this is a function from a using directive
            // for using_directive in module.borrow().using_directives.clone() {
            //     // Make sure the type names match
            //     if let Some(for_type) = using_directive.for_type.as_ref()
            //         && *for_type != type_name
            //     {
            //         continue;
            //     }
            //
            //     // Look up the definition of the using directive
            //     let external_scope = if matches!(module.borrow().kind, Some(solidity::ContractTy::Library(_))) && using_directive.library_name == module.borrow().name {
            //         module.borrow().toplevel_scope.clone()
            //     } else {
            //         match project.find_module_with_contract(module.clone(), &using_directive.library_name)
            //         .map(|d| d.toplevel_scope.clone()) {
            //             Some(s) => s,
            //             None => continue,
            //         }
            //     };
            //
            //     // Try to resolve the function call
            //     if let Some(result) = resolve_function_call(
            //         project,
            //         module.clone(),
            //         scope.clone(),
            //         &external_scope,
            //         member.name.as_str(),
            //         named_arguments,
            //         parameters.clone(),
            //         parameter_types.clone(),
            //     )? {
            //         return Ok(result);
            //     }
            // }

            panic!(
                "{}: TODO: translate array member function call: {} - {}",
                project.loc_to_file_location_string(module.clone(), &expression.loc()),
                expression,
                sway::TabbedDisplayer(&container),
            )
        }

        sway::TypeName::Tuple { .. } => todo!(
            "translate tuple member function call: {}",
            sway::TabbedDisplayer(&container)
        ),

        sway::TypeName::StringSlice => {
            //
            // TODO: Is this still necessary?
            //
            // let mut parameters = arguments
            //     .iter()
            //     .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
            //     .collect::<Result<Vec<_>, _>>()?;
            //
            // let mut parameter_types = parameters
            //     .iter()
            //     .map(|p| get_expression_type(project, module.clone(), scope.clone(), p))
            //     .collect::<Result<Vec<_>, _>>()?;
            //
            // parameters.insert(0, container.clone());
            // parameter_types.insert(
            //     0,
            //     get_expression_type(project, module.clone(), scope.clone(), &container)?,
            // );
            //
            // // Check if this is a function from a using directive
            // for using_directive in module.borrow().using_directives.clone() {
            //     // Make sure the type names match
            //     if let Some(for_type) = using_directive.for_type.as_ref()
            //         && *for_type != type_name
            //     {
            //         continue;
            //     }
            //
            //     // Look up the definition of the using directive
            //     let Some(external_scope) = project.find_module_with_contract(module.clone(), &using_directive.library_name)
            //         .map(|d| d.toplevel_scope.clone())
            //     else { continue };
            //
            //     // Try to resolve the function call
            //     if let Some(result) = resolve_function_call(
            //         project,
            //         module.clone(),
            //         scope.clone(),
            //         &external_scope,
            //         member.name.as_str(),
            //         named_arguments,
            //         parameters.clone(),
            //         parameter_types.clone(),
            //     )? {
            //         return Ok(result);
            //     }
            // }

            panic!(
                "{}: TODO: translate string slice member function call: {expression}",
                project.loc_to_file_location_string(module.clone(), &expression.loc()),
            )
        }

        sway::TypeName::StringArray { .. } => todo!(
            "translate string array member function call: {}",
            sway::TabbedDisplayer(&container)
        ),

        sway::TypeName::Function { .. } => todo!(
            "translate fn member function call: {}",
            sway::TabbedDisplayer(&container)
        ),

        sway::TypeName::Abi { .. } => translate_identity_member_access_function_call(
            project,
            module.clone(),
            scope.clone(),
            expression,
            arguments,
            named_arguments,
            container,
            member,
            solidity_container,
            "Identity".to_string(),
        ),
    }
}

#[inline(always)]
fn translate_function_call_function_call(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    function: &solidity::Expression,
    args: &[solidity::Expression],
    _solidity_expression: &solidity::Expression,
    arguments: &[solidity::Expression],
) -> Result<sway::Expression, Error> {
    // timelock.executeTransaction.value(proposal.values[i])
    match function {
        solidity::Expression::MemberAccess(_, container, member) => {
            // timelock.executeTransaction
            // .value

            let mut coins = None;
            let mut gas = None;

            match member.name.as_str() {
                "value" if args.len() == 1 => {
                    coins = Some(translate_expression(
                        project,
                        module.clone(),
                        scope.clone(),
                        &args[0],
                    )?)
                }

                "gas" if args.len() == 1 => {
                    gas = Some(translate_expression(
                        project,
                        module.clone(),
                        scope.clone(),
                        &args[0],
                    )?)
                }

                _ => todo!("translate member function call: {member}"),
            };

            match container.as_ref() {
                solidity::Expression::MemberAccess(_, container, member) => {
                    let container =
                        translate_expression(project, module.clone(), scope.clone(), container)?;

                    let type_name =
                        get_expression_type(project, module.clone(), scope.clone(), &container)?;

                    if type_name.is_identity() && member.name == "call" {
                        if arguments.len() != 1 {
                            panic!(
                                "Malformed `address.call` call, expected 1 argument, found {}",
                                arguments.len()
                            );
                        }

                        let payload = translate_expression(
                            project,
                            module.clone(),
                            scope.clone(),
                            &arguments[0],
                        )?;

                        return translate_address_call_expression(
                            project,
                            module.clone(),
                            scope.clone(),
                            &payload,
                            coins,
                            None,
                            gas,
                        );
                    }

                    // Check to see if the function is located in an external ABI
                    {
                        let mut type_name = type_name.clone();

                        if let sway::TypeName::Abi {
                            type_name: abi_type_name,
                        } = &type_name
                        {
                            type_name = abi_type_name.as_ref().clone();
                        }

                        if let sway::TypeName::Identifier {
                            name: contract_name,
                            generic_parameters: None,
                        } = &type_name
                        {
                            let function_new_name =
                                translate_naming_convention(member.name.as_str(), Case::Snake);

                            if let Some(contract) =
                                project.find_contract(module.clone(), contract_name)
                            {
                                if contract
                                    .borrow()
                                    .abi
                                    .functions
                                    .iter()
                                    .any(|f| f.new_name == function_new_name)
                                {
                                    let mut fields = vec![];

                                    if let Some(coins) = coins {
                                        fields.push(sway::ConstructorField {
                                            name: "coins".into(),
                                            value: coins,
                                        });
                                    }

                                    if let Some(gas) = gas {
                                        fields.push(sway::ConstructorField {
                                            name: "gas".into(),
                                            value: gas,
                                        });
                                    }

                                    return Ok(sway::Expression::from(sway::FunctionCallBlock {
                                        function: sway::Expression::create_member_access(
                                            container,
                                            &[function_new_name.as_str()],
                                        ),
                                        generic_parameters: None,
                                        fields,
                                        parameters: arguments
                                            .iter()
                                            .map(|a| {
                                                translate_expression(
                                                    project,
                                                    module.clone(),
                                                    scope.clone(),
                                                    a,
                                                )
                                            })
                                            .collect::<Result<Vec<_>, _>>()?,
                                    }));
                                }
                            }
                        }
                    }

                    todo!("translate member function call: {member}")
                }

                _ => todo!("translate member function call: {member}"),
            }
        }

        _ => todo!("translate function call: {function}"),
    }
}

#[inline(always)]
fn translate_function_call_block_member_access_function_call(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    solidity_container: &solidity::Expression,
    member: &solidity::Identifier,
    block: &solidity::Statement,
    arguments: &[solidity::Expression],
) -> Result<sway::Expression, Error> {
    let container =
        translate_expression(project, module.clone(), scope.clone(), solidity_container)?;

    let type_name = get_expression_type(project, module.clone(), scope.clone(), &container)?;

    let solidity::Statement::Args(_, block_args) = block else {
        panic!("Malformed `address.call` call, expected args block, found: {block:#?}");
    };

    let mut coins = None;
    let mut gas = None;

    for block_arg in block_args.iter() {
        match block_arg.name.name.as_str() {
            "value" => {
                coins = Some(translate_expression(
                    project,
                    module.clone(),
                    scope.clone(),
                    &block_arg.expr,
                )?)
            }

            "gas" => {
                gas = Some(translate_expression(
                    project,
                    module.clone(),
                    scope.clone(),
                    &block_arg.expr,
                )?)
            }

            arg => todo!("address.transfer block arg: {arg}"),
        }
    }

    if type_name.is_identity() && member.name == "call" {
        if arguments.len() != 1 {
            panic!(
                "Malformed `address.call` call, expected 1 argument, found {}",
                arguments.len()
            );
        }

        let payload = translate_expression(project, module.clone(), scope.clone(), &arguments[0])?;

        return translate_address_call_expression(
            project,
            module.clone(),
            scope.clone(),
            &payload,
            coins,
            None,
            gas,
        );
    }

    // Check to see if the type is a contract ABI
    {
        let mut abi_type_name = type_name.clone();
        let mut was_abi = false;

        if let sway::TypeName::Abi {
            type_name: abi_type_name2,
        } = &abi_type_name
        {
            abi_type_name = abi_type_name2.as_ref().clone();
            was_abi = true;
        }

        if let sway::TypeName::Identifier {
            name: contract_name,
            generic_parameters: None,
        } = &abi_type_name
            && let Some(contract) = project.find_contract(module.clone(), contract_name)
        {
            let abi = contract.borrow().abi.clone();

            let parameters = arguments
                .iter()
                .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
                .collect::<Result<Vec<_>, _>>()?;

            let parameter_types = parameters
                .iter()
                .map(|p| get_expression_type(project, module.clone(), scope.clone(), p))
                .collect::<Result<Vec<_>, _>>()?;

            let container = coerce_expression(
                project,
                module.clone(),
                scope.clone(),
                &container,
                &type_name,
                &if was_abi {
                    sway::TypeName::Abi {
                        type_name: Box::new(abi_type_name),
                    }
                } else {
                    abi_type_name
                },
            )
            .unwrap();

            if let Some(result) = resolve_abi_function_call(
                project,
                module.clone(),
                scope.clone(),
                &abi,
                Some(&container),
                &member.name,
                None,
                parameters,
                parameter_types,
            )? {
                return Ok(result);
            }
        }
    }

    todo!(
        "{}: translate Identity member function call block `{member}{}`: {}",
        project.loc_to_file_location_string(module.clone(), &solidity_container.loc()),
        block.to_string(),
        sway::TabbedDisplayer(&container)
    )
}

#[inline(always)]
fn translate_identity_member_access_function_call(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    expression: &solidity::Expression,
    arguments: &[solidity::Expression],
    named_arguments: Option<&[solidity::NamedArgument]>,
    container: sway::Expression,
    member: &solidity::Identifier,
    solidity_container: &solidity::Expression,
    name: String,
) -> Result<sway::Expression, Error> {
    match member.name.as_str() {
        // to.transfer(amount) => std::asset::transfer(to, asset_id, amount)
        "transfer" if arguments.len() == 1 => {
            let argument =
                translate_expression(project, module.clone(), scope.clone(), &arguments[0])?;
            let argument_type_name =
                get_expression_type(project, module.clone(), scope.clone(), &argument)?;

            let container_type = sway::TypeName::Identifier {
                name: "u64".to_string(),
                generic_parameters: None,
            };

            let argument = coerce_expression(
                project,
                module.clone(),
                scope.clone(),
                &argument,
                &argument_type_name,
                &container_type,
            )
            .unwrap();

            return Ok(sway::Expression::create_function_calls(
                None,
                &[(
                    "std::asset::transfer",
                    Some((
                        None,
                        vec![
                            container,
                            sway::Expression::create_function_calls(
                                None,
                                &[("AssetId::default", Some((None, vec![])))],
                            ),
                            argument,
                        ],
                    )),
                )],
            ));
        }

        // to.send(amount) => {
        //     std::asset::transfer(to, asset_id, amount);
        //     true
        // }
        "send" if arguments.len() == 1 => {
            let argument =
                translate_expression(project, module.clone(), scope.clone(), &arguments[0])?;
            let argument_type_name =
                get_expression_type(project, module.clone(), scope.clone(), &argument)?;

            let u64_type = sway::TypeName::Identifier {
                name: "u64".to_string(),
                generic_parameters: None,
            };

            let argument = coerce_expression(
                project,
                module.clone(),
                scope.clone(),
                &argument,
                &argument_type_name,
                &u64_type,
            )
            .unwrap();

            return Ok(sway::Expression::from(sway::Block {
                statements: vec![sway::Statement::from(
                    sway::Expression::create_function_calls(
                        None,
                        &[(
                            "std::asset::transfer",
                            Some((
                                None,
                                vec![
                                    container,
                                    sway::Expression::create_function_calls(
                                        None,
                                        &[("AssetId::default", Some((None, vec![])))],
                                    ),
                                    argument,
                                ],
                            )),
                        )],
                    ),
                )],
                final_expr: Some(sway::Expression::from(sway::Literal::Bool(true))),
            }));
        }

        "call" if arguments.len() == 1 => {
            let payload =
                translate_expression(project, module.clone(), scope.clone(), &arguments[0])?;
            return translate_address_call_expression(
                project,
                module.clone(),
                scope.clone(),
                &payload,
                None,
                None,
                None,
            );
        }

        "delegatecall" => {
            //
            // TODO: is delegatecall possible?
            //

            return Ok(sway::Expression::create_todo(Some(expression.to_string())));
        }

        "staticcall" => {
            //
            // TODO: is staticcall possible?
            //

            return Ok(sway::Expression::create_todo(Some(expression.to_string())));
        }

        _ => {}
    }

    let mut name = name.clone();

    //
    // TODO: is this still necessary?
    //
    // // Check using directives for Identity-specific function
    // for using_directive in module.using_directives.iter() {
    //     let Some(external_definition) =
    //         project.find_module_with_contract(module.clone(), &using_directive.library_name).cloned()
    //     else {
    //         continue;
    //     };
    //
    //     if let Some(for_type_name) = &using_directive.for_type {
    //         if !type_name.is_identity() && for_type_name != type_name {
    //             // println!(
    //             //     "Using directive type {} is not {}, skipping...",
    //             //     sway::TabbedDisplayer(for_type_name),
    //             //     sway::TabbedDisplayer(&type_name),
    //             // );
    //             continue;
    //         }
    //     }
    //
    //     for f in external_definition.toplevel_scope.borrow().functions.iter() {
    //         let f = f.borrow();
    //
    //         let sway::TypeName::Function { parameters: f_parameters, .. } = &f.type_name else {
    //             panic!("Invalid function type name: {:#?}", f.type_name)
    //         };
    //
    //         if f.old_name != member.name {
    //             continue;
    //         }
    //
    //         let Some(parameter) = f_parameters.entries.first() else { continue };
    //         let Some(parameter_type_name) = parameter.type_name.as_ref() else { continue };
    //
    //         if parameter_type_name == type_name {
    //             let mut parameters = arguments.iter()
    //                 .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
    //                 .collect::<Result<Vec<_>, _>>()?;
    //
    //             parameters.insert(0, container.clone());
    //
    //             return Ok(sway::Expression::create_function_calls(None, &[(f.new_name.as_str(), Some((None, parameters)))]));
    //         }
    //     }
    // }

    let variable = match translate_variable_access_expression(
        project,
        module.clone(),
        scope.clone(),
        solidity_container,
    )? {
        Some(ir::VariableAccess { variable, .. }) => variable,
        None => None,
    };

    // Check if expression is a variable that had an ABI type
    if let Some(variable) = variable.as_ref() {
        let variable = variable.borrow();
        let mut variable_type_name = variable.type_name.clone();

        if let sway::TypeName::Abi { type_name } = &variable_type_name {
            variable_type_name = type_name.as_ref().clone();
        }

        if let sway::TypeName::Identifier {
            name: abi_type_name,
            generic_parameters: None,
        } = &variable_type_name
        {
            name = abi_type_name.clone();
        }
    } else {
        let mut expression = container.clone();

        // HACK: remove `.read()`
        if let sway::Expression::FunctionCall(f) = &expression
            && let sway::Expression::MemberAccess(m) = &f.function
            && m.member == "read"
            && f.parameters.is_empty()
        {
            expression = m.expression.clone();
        }

        match &expression {
            sway::Expression::PathExpr(p) => {
                if let sway::PathExprRoot::Identifier(ident) = &p.root
                    && p.segments.is_empty()
                {
                    let mut name_found = false;

                    // Check if ident is a configurable field
                    if let Some(configurable) = module.borrow().configurable.as_ref() {
                        for field in configurable.fields.iter() {
                            if field.name == *ident {
                                let mut field_type_name = field.type_name.clone();

                                if let sway::TypeName::Abi { type_name } = &field_type_name {
                                    field_type_name = type_name.as_ref().clone();
                                }

                                if let sway::TypeName::Identifier {
                                    name: abi_type_name,
                                    generic_parameters: None,
                                } = &field_type_name
                                {
                                    name = abi_type_name.clone();
                                    name_found = true;
                                }
                            }
                        }
                    }

                    // Check if ident is a constant
                    if !name_found {
                        if let Some(constant) =
                            module.borrow().constants.iter().find(|c| c.name == *ident)
                        {
                            let mut constant_type_name = constant.type_name.clone();

                            if let sway::TypeName::Abi { type_name } = &constant_type_name {
                                constant_type_name = type_name.as_ref().clone();
                            }

                            if let sway::TypeName::Identifier {
                                name: abi_type_name,
                                generic_parameters: None,
                            } = &constant_type_name
                            {
                                name = abi_type_name.clone();
                                // name_found = true;
                            }
                        }
                    }
                }
            }

            sway::Expression::MemberAccess(m) => {
                if let sway::Expression::PathExpr(path_expr) = &m.expression
                    && let sway::PathExprRoot::Identifier(root_ident) = &path_expr.root
                    && root_ident == "storage"
                {
                    let mut storage_namespace: Option<Rc<RefCell<sway::StorageNamespace>>> = None;

                    for segment in path_expr.segments.iter() {
                        let namespace = match storage_namespace {
                            None => {
                                let mut module = module.borrow_mut();
                                let storage = module.get_storage(scope.clone());
                                storage
                                    .borrow()
                                    .namespaces
                                    .iter()
                                    .find(|s| s.borrow().name == segment.name)
                                    .cloned()
                            }

                            Some(storage_namespace) => storage_namespace
                                .borrow()
                                .namespaces
                                .iter()
                                .find(|s| s.borrow().name == segment.name)
                                .cloned(),
                        };

                        if namespace.is_none() {
                            storage_namespace = None;
                            break;
                        }

                        storage_namespace = namespace;
                    }

                    if let Some(storage_namespace) = storage_namespace
                        && let Some(storage_field) = storage_namespace
                            .borrow()
                            .fields
                            .iter()
                            .find(|s| s.name == m.member)
                    {
                        let mut storage_field_type_name = storage_field.type_name.clone();

                        if let sway::TypeName::Abi { type_name } = &storage_field_type_name {
                            storage_field_type_name = type_name.as_ref().clone();
                        }

                        if let sway::TypeName::Identifier {
                            name: abi_type_name,
                            generic_parameters: None,
                        } = &storage_field_type_name
                        {
                            name = abi_type_name.clone();
                        }
                    }
                }
            }

            _ => {}
        }
    }

    let parameters = arguments
        .iter()
        .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
        .collect::<Result<Vec<_>, _>>()?;

    let parameter_types = parameters
        .iter()
        .map(|p| get_expression_type(project, module.clone(), scope.clone(), p))
        .collect::<Result<Vec<_>, _>>()?;

    let type_name = get_expression_type(project, module.clone(), scope.clone(), &container)?;

    // Check to see if the type is located in an external ABI
    if let Some(abi_type_name) = type_name.abi_type()
        && let sway::TypeName::Identifier { name, .. } = abi_type_name
        && let Some((module, contract)) = project.find_module_and_contract(module.clone(), &name)
    {
        let abi = contract.borrow().abi.clone();

        let scope = Rc::new(RefCell::new(ir::Scope::new(
            Some(&name),
            None,
            Some(scope.clone()),
        )));

        if let Some(result) = resolve_abi_function_call(
            project,
            module.clone(),
            scope.clone(),
            &abi,
            Some(&container),
            &member.name,
            named_arguments,
            parameters,
            parameter_types,
        )? {
            return Ok(result);
        }
    }

    panic!(
        "{}: TODO: translate Identity member function call `{member}`: {}",
        project.loc_to_file_location_string(module.clone(), &expression.loc()),
        sway::TabbedDisplayer(&container),
    )
}

#[inline(always)]
fn translate_storage_vec_member_access_function_call(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    arguments: &[solidity::Expression],
    member: &solidity::Identifier,
    solidity_container: &solidity::Expression,
    container: &sway::Expression,
) -> Result<sway::Expression, Error> {
    let Some(ir::VariableAccess { mut expression, .. }) = translate_variable_access_expression(
        project,
        module.clone(),
        scope.clone(),
        solidity_container,
    )?
    else {
        panic!("Failed to translate storage vec member access function call: {solidity_container}")
    };

    // HACK: remove `.read()`
    if let sway::Expression::FunctionCall(f) = &expression
        && let sway::Expression::MemberAccess(m) = &f.function
        && m.member == "read"
        && f.parameters.is_empty()
        && get_expression_type(project, module.clone(), scope.clone(), &m.expression)?
            .is_storage_key()
    {
        expression = m.expression.clone();
    }

    let type_name = get_expression_type(project, module.clone(), scope.clone(), &expression)?;

    match member.name.as_str() {
        "push" | "pop" | "remove" => {}

        _ => todo!(
            "translate StorageVec member function call `{member}`: {}",
            sway::TabbedDisplayer(container)
        ),
    }

    let mut storage_key_type = None;

    if let Some(s) = type_name.storage_key_type() {
        storage_key_type = Some(s);
    } else if let Some(option_type) = type_name.option_type()
        && let Some(s) = option_type.storage_key_type()
    {
        expression = sway::Expression::create_function_calls(
            Some(expression),
            &[("unwrap", Some((None, vec![])))],
        );
        storage_key_type = Some(s);
    }

    if let Some(storage_key_type) = storage_key_type {
        scope
            .borrow_mut()
            .set_function_storage_accesses(module.clone(), false, true);

        match storage_key_type {
            sway::TypeName::Identifier {
                name,
                generic_parameters,
            } => match (name.as_str(), generic_parameters.as_ref()) {
                ("StorageVec", Some(generic_parameters))
                    if generic_parameters.entries.len() == 1 =>
                {
                    return Ok(sway::Expression::create_function_calls(
                        Some(expression),
                        &[(
                            member.name.as_str(),
                            Some((
                                None,
                                arguments
                                    .iter()
                                    .map(|a| {
                                        let expr = translate_expression(
                                            project,
                                            module.clone(),
                                            scope.clone(),
                                            a,
                                        )
                                        .unwrap();

                                        let from_type_name = get_expression_type(
                                            project,
                                            module.clone(),
                                            scope.clone(),
                                            &expr,
                                        )
                                        .unwrap();

                                        coerce_expression(
                                            project,
                                            module.clone(),
                                            scope.clone(),
                                            &expr,
                                            &from_type_name,
                                            &generic_parameters.entries[0].type_name,
                                        )
                                        .unwrap()
                                    })
                                    .collect::<Vec<_>>(),
                            )),
                        )],
                    ));
                }

                _ => {}
            },

            _ => {}
        }
    }

    todo!(
        "translate StorageVec member function call `{member}`: {}",
        sway::TabbedDisplayer(container)
    )
}

#[inline(always)]
fn translate_vec_member_access_function_call(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    arguments: &[solidity::Expression],
    member: &solidity::Identifier,
    solidity_container: &solidity::Expression,
    container: &sway::Expression,
) -> Result<sway::Expression, Error> {
    match member.name.as_str() {
        "push" => {
            let (variable, container_access) = match translate_variable_access_expression(
                project,
                module.clone(),
                scope.clone(),
                solidity_container,
            )? {
                Some(ir::VariableAccess {
                    variable,
                    expression,
                }) => (Some(variable), Some(expression)),
                None => (None, None),
            };

            let (Some(_), Some(container_access)) = (variable, container_access) else {
                panic!("Vec is not a variable");
            };

            Ok(sway::Expression::create_function_calls(
                Some(container_access),
                &[(
                    "push",
                    Some((
                        None,
                        arguments
                            .iter()
                            .map(|a| {
                                translate_expression(project, module.clone(), scope.clone(), a)
                            })
                            .collect::<Result<Vec<_>, _>>()?,
                    )),
                )],
            ))
        }

        "pop" => {
            let (variable, container_access) = match translate_variable_access_expression(
                project,
                module.clone(),
                scope.clone(),
                solidity_container,
            )? {
                Some(ir::VariableAccess {
                    variable,
                    expression,
                }) => (Some(variable), Some(expression)),
                None => (None, None),
            };

            let (Some(_), Some(container_access)) = (variable, container_access) else {
                panic!("Vec is not a variable");
            };

            Ok(sway::Expression::create_function_calls(
                Some(container_access),
                &[("pop", Some((None, vec![])))],
            ))
        }

        "remove" => {
            let (variable, container_access) = match translate_variable_access_expression(
                project,
                module.clone(),
                scope.clone(),
                solidity_container,
            )? {
                Some(ir::VariableAccess {
                    variable,
                    expression,
                }) => (Some(variable), Some(expression)),
                None => (None, None),
            };

            let (Some(_), Some(container_access)) = (variable, container_access) else {
                panic!("Vec is not a variable");
            };

            Ok(sway::Expression::create_function_calls(
                Some(container_access),
                &[(
                    "remove",
                    Some((
                        None,
                        arguments
                            .iter()
                            .map(|a| {
                                translate_expression(project, module.clone(), scope.clone(), a)
                            })
                            .collect::<Result<Vec<_>, _>>()?,
                    )),
                )],
            ))
        }

        _ => todo!(
            "translate Vec member function call `{member}`: {}",
            sway::TabbedDisplayer(container)
        ),
    }
}

#[inline(always)]
fn translate_builtin_abi_member_access_function_call(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    member_name: &str,
    arguments: &[solidity::Expression],
    named_arguments: Option<&[solidity::NamedArgument]>,
) -> Result<Option<sway::Expression>, Error> {
    assert!(named_arguments.is_none());

    match member_name {
        "decode" => {
            // abi.decode(encodedData, (uint256, bool)) =>
            // let (a, b): (u256, bool) = {
            //     let slice = encoded_data.as_raw_slice();
            //     let mut ptr = slice.ptr();
            //     let a = ptr.read::<u256>();
            //     ptr = ptr.add::<u256>(1);
            //     let b = ptr.read::<bool>();
            //     ptr = ptr.add::<bool>(1);
            //     (a, b)
            // };

            if arguments.len() != 2 {
                return Ok(None);
            }

            let encoded_data =
                translate_expression(project, module.clone(), scope.clone(), &arguments[0])?;

            let parameter_types = match &arguments[1] {
                solidity::Expression::List(_, parameter_types) => parameter_types
                    .iter()
                    .map(|(_, p)| {
                        translate_type_name(
                            project,
                            module.clone(),
                            scope.clone(),
                            &p.as_ref().unwrap().ty,
                            p.as_ref().and_then(|p| p.storage.as_ref()),
                        )
                    })
                    .collect::<Vec<_>>(),

                solidity::Expression::Parenthesis(_, expression) => {
                    vec![translate_type_name(
                        project,
                        module.clone(),
                        scope.clone(),
                        expression,
                        None,
                    )]
                }

                _ => {
                    return Ok(None);
                }
            };

            let parameter_names = ('a'..='z')
                .enumerate()
                .take_while(|(i, _)| *i < parameter_types.len())
                .map(|(_, c)| c.to_string())
                .collect::<Vec<_>>();

            if parameter_types.len() != parameter_names.len() {
                return Ok(None);
            }

            // If we only have 1 parameter to decode, just decode it directly
            if parameter_types.len() == 1 {
                // encoded_data.as_raw_slice().ptr().read::<u256>()
                return Ok(Some(sway::Expression::create_function_calls(
                    Some(encoded_data.clone()),
                    &[
                        ("as_raw_slice", Some((None, vec![]))),
                        ("ptr", Some((None, vec![]))),
                        (
                            "read",
                            Some((
                                Some(sway::GenericParameterList {
                                    entries: vec![sway::GenericParameter {
                                        type_name: parameter_types[0].clone(),
                                        implements: None,
                                    }],
                                }),
                                vec![],
                            )),
                        ),
                    ],
                )));
            }

            let mut block = sway::Block {
                statements: vec![
                    // let mut ptr = encoded_data.as_raw_slice().ptr();
                    sway::Statement::from(sway::Let {
                        pattern: sway::LetPattern::from(sway::LetIdentifier {
                            // This only needs to be mutable if there's multiple parameters to decode
                            is_mutable: parameter_names.len() > 1,
                            name: "ptr".into(),
                        }),
                        type_name: None,
                        value: sway::Expression::create_function_calls(
                            Some(encoded_data.clone()),
                            &[
                                ("as_raw_slice", Some((None, vec![]))),
                                ("ptr", Some((None, vec![]))),
                            ],
                        ),
                    }),
                ],
                final_expr: Some(sway::Expression::Tuple(
                    parameter_names
                        .iter()
                        .map(|p| sway::Expression::create_identifier(p.clone()))
                        .collect(),
                )),
            };

            for (i, (parameter_name, parameter_type)) in parameter_names
                .iter()
                .zip(parameter_types.iter())
                .enumerate()
            {
                // let a = ptr.read::<u256>();
                block.statements.push(sway::Statement::from(sway::Let {
                    pattern: sway::LetPattern::from(sway::LetIdentifier {
                        is_mutable: false,
                        name: parameter_name.clone(),
                    }),
                    type_name: None,
                    value: sway::Expression::create_function_calls(
                        None,
                        &[
                            ("ptr", None),
                            (
                                "read",
                                Some((
                                    Some(sway::GenericParameterList {
                                        entries: vec![sway::GenericParameter {
                                            type_name: parameter_type.clone(),
                                            implements: None,
                                        }],
                                    }),
                                    vec![],
                                )),
                            ),
                        ],
                    ),
                }));

                // If we have more parameters to decode, increase the ptr
                if i < parameter_names.len() - 1 {
                    // ptr = ptr.add::<u256>(1);
                    block
                        .statements
                        .push(sway::Statement::from(sway::Expression::from(
                            sway::BinaryExpression {
                                operator: "=".into(),
                                lhs: sway::Expression::create_identifier("ptr".into()),
                                rhs: sway::Expression::create_function_calls(
                                    None,
                                    &[
                                        ("ptr", None),
                                        (
                                            "add",
                                            Some((
                                                Some(sway::GenericParameterList {
                                                    entries: vec![sway::GenericParameter {
                                                        type_name: parameter_type.clone(),
                                                        implements: None,
                                                    }],
                                                }),
                                                vec![sway::Expression::from(
                                                    sway::Literal::DecInt(BigUint::one(), None),
                                                )],
                                            )),
                                        ),
                                    ],
                                ),
                            },
                        )));
                }
            }

            Ok(Some(sway::Expression::from(block)))
        }

        "encode" | "encodePacked" => {
            // abi.encode(a, b, ...) | abi.encodePacked(a, b, ...) => {
            //     let mut bytes = Bytes::new();
            //     bytes.append(Bytes::from(std::codec::encode(a)));
            //     bytes.append(Bytes::from(std::codec::encode(b)));
            //     // ...
            //     bytes
            // }

            // Ensure `std::bytes::Bytes` is imported
            module.borrow_mut().ensure_use_declared("std::bytes::Bytes");

            // Generate a unique variable name
            let variable_name = scope.borrow_mut().generate_unique_variable_name("bytes");

            let parameters = arguments
                .iter()
                .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
                .collect::<Result<Vec<_>, _>>()?;

            // Create the abi encoding block
            let mut block = sway::Block {
                statements: vec![sway::Statement::from(sway::Let {
                    pattern: sway::LetPattern::from(sway::LetIdentifier {
                        is_mutable: true,
                        name: variable_name.clone(),
                    }),
                    type_name: None,
                    value: sway::Expression::create_function_calls(
                        None,
                        &[("Bytes::new", Some((None, vec![])))],
                    ),
                })],
                final_expr: Some(sway::Expression::create_identifier(variable_name.clone())),
            };

            // Add the encoding statements to the block
            for parameter in parameters {
                block.statements.push(sway::Statement::from(
                    sway::Expression::create_function_calls(
                        None,
                        &[
                            (variable_name.as_str(), None),
                            (
                                "append",
                                Some((
                                    None,
                                    vec![sway::Expression::create_function_calls(
                                        None,
                                        &[(
                                            "Bytes::from",
                                            Some((
                                                None,
                                                vec![sway::Expression::create_function_calls(
                                                    None,
                                                    &[(
                                                        "std::codec::encode",
                                                        Some((None, vec![
                                                            match get_expression_type(project, module.clone(), scope.clone(), &parameter)? {
                                                                sway::TypeName::Identifier { name, generic_parameters } => {
                                                                    match (name.as_str(), generic_parameters.as_ref()) {
                                                                        ("Identity", None) => {
                                                                            let identity_variant_branch = |name: &str| -> sway::MatchBranch {
                                                                                sway::MatchBranch {
                                                                                    pattern: sway::Expression::create_function_calls(None, &[
                                                                                        (format!("Identity::{name}").as_str(), Some((None, vec![
                                                                                            sway::Expression::create_identifier("x".into()),
                                                                                        ]))),
                                                                                    ]),
                                                                                    value: sway::Expression::create_function_calls(None, &[
                                                                                        ("x", None),
                                                                                        ("bits", Some((None, vec![]))),
                                                                                    ]),
                                                                                }
                                                                            };

                                                                            sway::Expression::from(sway::Match {
                                                                                expression: parameter,
                                                                                branches: vec![
                                                                                    identity_variant_branch("Address"),
                                                                                    identity_variant_branch("ContractId"),
                                                                                ],
                                                                            })
                                                                        },

                                                                        ("I8" | "I16" | "I32" | "I64" | "I128" | "I256", None) => {
                                                                            sway::Expression::create_function_calls(Some(parameter), &[
                                                                                ("underlying", Some((None, vec![])))
                                                                            ])
                                                                        }

                                                                        _ => parameter
                                                                    }
                                                                }

                                                                sway::TypeName::Abi { .. } => {
                                                                    let identity_variant_branch = |name: &str| -> sway::MatchBranch {
                                                                        sway::MatchBranch {
                                                                            pattern: sway::Expression::create_function_calls(None, &[
                                                                                (format!("Identity::{name}").as_str(), Some((None, vec![
                                                                                    sway::Expression::create_identifier("x".into()),
                                                                                ]))),
                                                                            ]),
                                                                            value: sway::Expression::create_function_calls(None, &[
                                                                                ("x", None),
                                                                                ("bits", Some((None, vec![]))),
                                                                            ]),
                                                                        }
                                                                    };

                                                                    sway::Expression::from(sway::Match {
                                                                        expression: parameter,
                                                                        branches: vec![
                                                                            identity_variant_branch("Address"),
                                                                            identity_variant_branch("ContractId"),
                                                                        ],
                                                                    })
                                                                }
                                                                _ => parameter
                                                            }
                                                        ])),
                                                    )],
                                                )],
                                            )),
                                        )],
                                    )],
                                )),
                            ),
                        ],
                    ),
                ));
            }

            Ok(Some(sway::Expression::from(block)))
        }

        "encodeWithSelector" => {
            // abi.encodeWithSelector(selector, ...) => ???

            //
            // TODO: how should this be handled?
            //

            Ok(Some(sway::Expression::create_todo(Some(format!(
                "abi.encodeWithSelector({})",
                arguments
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
            )))))
        }

        "encodeWithSignature" => {
            // abi.encodeWithSignature(signature, ...) => ???

            //
            // TODO: how should this be handled?
            //

            Ok(Some(sway::Expression::create_todo(Some(format!(
                "abi.encodeWithSignature({})",
                arguments
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
            )))))
        }

        "encodeCall" => {
            // abi.encodeCall(functionPointer, (...)) => ???

            //
            // TODO: how should this be handled?
            //

            Ok(Some(sway::Expression::create_todo(Some(format!(
                "abi.encodeCall({})",
                arguments
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
            )))))
        }

        _ => Ok(None),
    }
}

#[inline(always)]
fn translate_super_member_access_function_call(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    member: &str,
    arguments: &[solidity::Expression],
    named_arguments: Option<&[solidity::NamedArgument]>,
) -> Result<Option<sway::Expression>, Error> {
    let parameters = arguments
        .iter()
        .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
        .collect::<Result<Vec<_>, _>>()?;

    let parameter_types = parameters
        .iter()
        .map(|p| get_expression_type(project, module.clone(), scope.clone(), p))
        .collect::<Result<Vec<_>, _>>()?;

    let contract_name = scope.borrow().get_contract_name();
    if let Some(contract_name) = contract_name
        && let Some((module, contract)) =
            project.find_module_and_contract(module.clone(), &contract_name)
    {
        let abi = contract.borrow().abi.clone();

        for inherit in abi.inherits.iter() {
            let contract_name = inherit.to_string();

            let scope = Rc::new(RefCell::new(ir::Scope::new(
                Some(&contract_name),
                None,
                Some(scope.clone()),
            )));

            if let Some(result) = resolve_abi_function_call(
                project,
                module.clone(),
                scope.clone(),
                &abi,
                None,
                member,
                named_arguments,
                parameters.clone(),
                parameter_types.clone(),
            )? {
                return Ok(Some(result));
            }
        }
    }

    Ok(None)
}

#[inline(always)]
fn translate_this_member_access_function_call(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    member: &str,
    arguments: &[solidity::Expression],
    named_arguments: Option<&[solidity::NamedArgument]>,
) -> Result<Option<sway::Expression>, Error> {
    let parameters = arguments
        .iter()
        .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
        .collect::<Result<Vec<_>, _>>()?;

    let parameter_types = parameters
        .iter()
        .map(|p| get_expression_type(project, module.clone(), scope.clone(), p))
        .collect::<Result<Vec<_>, _>>()?;

    if let Some(result) = resolve_function_call(
        project,
        module.clone(),
        scope.clone(),
        member,
        named_arguments,
        parameters,
        parameter_types,
    )? {
        return Ok(Some(result));
    }

    Ok(None)
}
