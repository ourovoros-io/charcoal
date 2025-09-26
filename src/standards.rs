use crate::{ir, sway};
use convert_case::{Case, Casing};
use solang_parser::pt as solidity;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Standard {
    ERC20,
}

#[derive(Debug)]
pub struct StandardDefinition {
    pub name: Standard,
    pub parts: &'static [StandardDefinitionPart],
}

#[derive(Debug)]
pub enum StandardDefinitionPart {
    Function {
        name: &'static str,
        arguments: &'static [solidity::Type],
        returns: &'static [solidity::Type],
    },
    Event {
        name: &'static str,
        arguments: &'static [solidity::Type],
    },
}

pub const STANDARDS: &[StandardDefinition] = &[
    // ERC20
    StandardDefinition {
        name: Standard::ERC20,
        parts: &[
            // Functions
            // function name() public view returns (string);
            StandardDefinitionPart::Function {
                name: "name",
                arguments: &[],
                returns: &[solidity::Type::String],
            },
            // function symbol() public view returns (string);
            StandardDefinitionPart::Function {
                name: "symbol",
                arguments: &[],
                returns: &[solidity::Type::String],
            },
            // function decimals() public view returns (uint8);
            StandardDefinitionPart::Function {
                name: "decimals",
                arguments: &[],
                returns: &[solidity::Type::Uint(8)],
            },
            // function totalSupply() public view returns (uint256);
            StandardDefinitionPart::Function {
                name: "totalSupply",
                arguments: &[],
                returns: &[solidity::Type::Uint(256)],
            },
            // function balanceOf(address _owner) public view returns (uint256 balance);
            StandardDefinitionPart::Function {
                name: "balanceOf",
                arguments: &[solidity::Type::Address],
                returns: &[solidity::Type::Uint(256)],
            },
            // function transfer(address _to, uint256 _value) public returns (bool success);
            StandardDefinitionPart::Function {
                name: "transfer",
                arguments: &[solidity::Type::Address, solidity::Type::Uint(256)],
                returns: &[solidity::Type::Bool],
            },
            // function transferFrom(address _from, address _to, uint256 _value) public returns (bool success);
            StandardDefinitionPart::Function {
                name: "transferFrom",
                arguments: &[
                    solidity::Type::Address,
                    solidity::Type::Address,
                    solidity::Type::Uint(256),
                ],
                returns: &[solidity::Type::Bool],
            },
            // function approve(address _spender, uint256 _value) public returns (bool success);
            StandardDefinitionPart::Function {
                name: "approve",
                arguments: &[solidity::Type::Address, solidity::Type::Uint(256)],
                returns: &[solidity::Type::Bool],
            },
            // function allowance(address _owner, address _spender) public view returns (uint256 remaining);
            StandardDefinitionPart::Function {
                name: "allowance",
                arguments: &[solidity::Type::Address, solidity::Type::Address],
                returns: &[solidity::Type::Uint(256)],
            },
            // Events
            // event Transfer(address indexed _from, address indexed _to, uint256 _value);
            StandardDefinitionPart::Event {
                name: "Transfer",
                arguments: &[
                    solidity::Type::Address,
                    solidity::Type::Address,
                    solidity::Type::Uint(256),
                ],
            },
            // event Approval(address indexed _owner, address indexed _spender, uint256 _value);
            StandardDefinitionPart::Event {
                name: "Approval",
                arguments: &[
                    solidity::Type::Address,
                    solidity::Type::Address,
                    solidity::Type::Uint(256),
                ],
            },
        ],
    },
];

/// ERC20 to SRC20
pub fn ensure_src20_dependencies(module: &mut ir::Module) {
    // src20 = "0.8.1"
    module.ensure_dependency_declared("src20 = \"0.8.1\"");

    // use src20::{SetDecimalsEvent, SetNameEvent, SetSymbolEvent, SRC20, TotalSupplyEvent};
    module.ensure_use_declared("src20::SetDecimalsEvent");
    module.ensure_use_declared("src20::SetNameEvent");
    module.ensure_use_declared("src20::SetSymbolEvent");
    module.ensure_use_declared("src20::TotalSupplyEvent");
    module.ensure_use_declared("src20::SRC20");
}

pub fn remove_abi_functions_for_standard(
    contract: Rc<RefCell<ir::Contract>>,
    standard_definition: &StandardDefinition,
) -> HashMap<String, sway::Block> {
    let abi_fn_count = {
        let contract = contract.borrow();
        contract.abi.functions.len()
    };

    for i in (0..abi_fn_count).rev() {
        let function = {
            let contract = contract.borrow();
            contract.abi.functions[i].clone()
        };

        let mut found = false;

        for part in standard_definition.parts.iter() {
            let StandardDefinitionPart::Function { name, .. } = part else {
                continue;
            };

            if function.old_name == *name {
                found = true;
                break;
            }
        }

        if found {
            contract.borrow_mut().abi.functions.remove(i);
        }
    }

    // Remove from abi implementation functions
    let mut function_bodies = HashMap::new();

    let abi_impl_item_count = {
        let contract = contract.borrow();
        contract.abi_impl.items.len()
    };

    for i in (0..abi_impl_item_count).rev() {
        let item = {
            let contract = contract.borrow();
            contract.abi_impl.items[i].clone()
        };

        let sway::ImplItem::Function(function) = item else {
            continue;
        };

        let mut found = false;

        for part in standard_definition.parts.iter() {
            let StandardDefinitionPart::Function { name, .. } = part else {
                continue;
            };

            if function.old_name == *name {
                found = true;
                break;
            }
        }

        if found {
            function_bodies.insert(function.old_name.clone(), function.body.as_ref().unwrap().clone());
            contract.borrow_mut().abi_impl.items.remove(i);
        }
    }

    function_bodies
}

pub fn remove_transfer_function(
    module: &mut ir::Module,
    function_bodies: &HashMap<String, sway::Block>,
    removed_function_names: &mut Vec<String>,
) {
    let Some(abi_body) = function_bodies.get("transfer") else {
        panic!("Transfer function body not found");
    };

    let Some(sway::Expression::FunctionCall(f)) = &abi_body.final_expr else {
        panic!("Failed to get transfer expression from final expression");
    };

    let Some(toplevel_fn_name) = f.function.as_identifier() else {
        panic!("Failed to get the function name for transfer");
    };

    // Get the top level function and index
    let Some((i, toplevel_fn)) = module
        .functions
        .iter()
        .enumerate()
        .find(|(_, f)| {
            let sway::TypeName::Function { new_name, .. } = &f.signature else {
                return false;
            };
            new_name == toplevel_fn_name
        })
        .map(|(i, f)| (i, f.clone()))
    else {
        panic!("Failed to get top level function");
    };

    let toplevel_fn_body = toplevel_fn.implementation.as_ref().unwrap().body.as_ref().unwrap();

    let toplevel_calls = toplevel_fn_body
        .statements
        .iter()
        .filter(|s| matches!(s, sway::Statement::Expression(sway::Expression::FunctionCall(_))))
        .collect::<Vec<_>>();

    for toplevel_call in toplevel_calls {
        let sway::Statement::Expression(sway::Expression::FunctionCall(function)) = toplevel_call else {
            unreachable!()
        };

        let Some(private_fn_name) = function.function.as_identifier() else {
            panic!("Failed to get function name")
        };

        let Some((index, _)) = module
            .functions
            .iter()
            .enumerate()
            .find(|(_, f)| {
                let sway::TypeName::Function { new_name, .. } = &f.signature else {
                    return false;
                };
                new_name == private_fn_name
            })
            .map(|(i, f)| (i, f.clone()))
        else {
            panic!("Failed to get private function");
        };

        module.functions.remove(index);
        removed_function_names.push(private_fn_name.to_string());
    }

    module.functions.remove(i);
    removed_function_names.push(toplevel_fn_name.to_string());

    remove_unnecessary_functions(module, removed_function_names);
}

pub fn remove_allowances(
    module: &mut ir::Module,
    function_bodies: &HashMap<String, sway::Block>,
    removed_function_names: &mut Vec<String>,
) -> String {
    let Some(allowance_body) = function_bodies.get("allowance") else {
        panic!("Allowance function body not found");
    };

    let Some(sway::Expression::FunctionCall(f)) = &allowance_body.final_expr else {
        panic!("Failed to get allowance expression from final expression");
    };

    let Some(allowance_fn_name) = f.function.as_identifier() else {
        panic!("Failed to get the function name for allowance");
    };

    // Get the top level function and index
    let Some((i, allowance_toplevel_fn)) = module
        .functions
        .iter()
        .enumerate()
        .find(|(_, f)| {
            let sway::TypeName::Function { new_name, .. } = &f.signature else {
                return false;
            };
            new_name == allowance_fn_name
        })
        .map(|(i, f)| (i, f.clone()))
    else {
        panic!("Failed to get top level function");
    };

    let Some(allowance_expr) = allowance_toplevel_fn
        .implementation
        .as_ref()
        .unwrap()
        .body
        .as_ref()
        .unwrap()
        .final_expr
        .as_ref()
    else {
        panic!("Failed to get allowance expression from final expression")
    };

    // Remove .read .get .get
    let Some(expr) = allowance_expr.to_read_call_parts() else {
        panic!(
            "Failed to get expression from read call parts of: {}",
            allowance_expr.display()
        )
    };

    let Some((expr, _)) = expr.to_get_call_parts() else {
        panic!("Failed to get expression from get call parts")
    };

    let Some((expr, _)) = expr.to_get_call_parts() else {
        panic!("Failed to get expression from get call parts")
    };

    // Get the member access
    let sway::Expression::MemberAccess(member_access) = expr else {
        panic!("Failed to get member access");
    };

    removed_function_names.push(allowance_fn_name.to_string());

    module.functions.remove(i);

    remove_unnecessary_functions(module, removed_function_names);

    member_access.member.clone()
}

pub fn remove_balance_of(
    module: &mut ir::Module,
    function_bodies: &HashMap<String, sway::Block>,
    removed_function_names: &mut Vec<String>,
) -> String {
    let Some(balance_of_body) = function_bodies.get("balanceOf") else {
        panic!("balanceOf function body not found");
    };

    let Some(sway::Expression::FunctionCall(f)) = &balance_of_body.final_expr else {
        panic!("Failed to get balanceOf expression from final expression");
    };

    let Some(balance_of_fn_name) = f.function.as_identifier() else {
        panic!("Failed to get the function name for balanceOf");
    };

    // Get the top level function and index
    let Some((i, balance_of_toplevel_fn)) = module
        .functions
        .iter()
        .enumerate()
        .find(|(_, f)| {
            let sway::TypeName::Function { new_name, .. } = &f.signature else {
                return false;
            };
            new_name == balance_of_fn_name
        })
        .map(|(i, f)| (i, f.clone()))
    else {
        panic!("Failed to get top level function");
    };

    let Some(balance_of_expr) = balance_of_toplevel_fn
        .implementation
        .as_ref()
        .unwrap()
        .body
        .as_ref()
        .unwrap()
        .final_expr
        .as_ref()
    else {
        panic!("Failed to get balanceOf expression from final expression")
    };

    // Remove .read .get
    let Some(expr) = balance_of_expr.to_read_call_parts() else {
        panic!(
            "Failed to get expression from read call parts of: {}",
            balance_of_expr.display()
        )
    };

    let Some((expr, _)) = expr.to_get_call_parts() else {
        panic!("Failed to get expression from get call parts")
    };

    // Get the member access
    let sway::Expression::MemberAccess(member_access) = expr else {
        panic!("Failed to get member access");
    };

    removed_function_names.push(balance_of_fn_name.to_string());

    module.functions.remove(i);

    remove_unnecessary_functions(module, removed_function_names);

    member_access.member.clone()
}

pub fn update_balance_of_usage(module: &mut ir::Module, name: &str) {
    for function in module.functions.iter_mut() {
        let Some(implementation) = function.implementation.as_mut() else {
            continue;
        };

        let Some(storage_struct_parameter) = implementation.storage_struct_parameter.as_ref() else {
            continue;
        };

        let Some(body) = implementation.body.as_mut() else {
            continue;
        };

        while let Some(expression) = body.find_expression_mut(|expr| {
            let Some((expr, _value)) = expr.to_write_call_parts() else {
                return false;
            };

            let Some((expr, _recipient)) = expr.to_get_call_parts() else {
                return false;
            };

            let sway::Expression::MemberAccess(member_access) = expr else {
                return false;
            };

            if member_access.member != name {
                return false;
            }

            let Some(identifier) = member_access.expression.as_identifier() else {
                return false;
            };

            identifier == storage_struct_parameter.name
        }) {
            let Some((expr, value)) = expression.to_write_call_parts() else {
                unreachable!()
            };

            let Some((_expr, recipient)) = expr.to_get_call_parts() else {
                unreachable!()
            };

            *expression = sway::Expression::create_function_call(
                "std::asset::transfer",
                None,
                vec![
                    recipient.clone(),
                    sway::Expression::create_function_call("std::asset::AssetId::default", None, vec![]),
                    sway::Expression::create_function_call("u64::try_from", None, vec![value.clone()])
                        .with_unwrap_call(),
                ],
            );
        }
    }
}

pub fn remove_storage_and_storage_struct_items(contract: Rc<RefCell<ir::Contract>>, name: &str) {
    let storage = contract.borrow().storage.as_ref().unwrap().clone();
    let storage_struct = contract.borrow().storage_struct.as_ref().unwrap().clone();

    let namespace_name = contract.borrow().name.to_case(Case::Snake);

    let Some(storage_namespace) = storage
        .borrow()
        .namespaces
        .iter()
        .find(|n| n.borrow().name == namespace_name)
        .cloned()
    else {
        panic!("Failed to get storage namespace")
    };

    let Some((i, _)) = storage_namespace
        .borrow()
        .fields
        .iter()
        .enumerate()
        .find(|(_, f)| f.name == name)
    else {
        panic!("Failed to find storage field")
    };

    storage_namespace.borrow_mut().fields.remove(i);

    let Some((i, _)) = storage_struct
        .borrow()
        .storage
        .fields
        .iter()
        .enumerate()
        .find(|(_, f)| f.new_name == name)
    else {
        panic!("Failed to find storage struct field")
    };

    storage_struct.borrow_mut().storage.fields.remove(i);
    let mut contract = contract.borrow_mut();

    let body = contract
        .storage_struct_constructor_fn
        .as_mut()
        .unwrap()
        .body
        .as_mut()
        .unwrap();

    let Some(sway::Expression::Constructor(constructor)) = body.final_expr.as_mut() else {
        panic!("Malformed struct constructor");
    };

    let Some((i, _)) = constructor.fields.iter().enumerate().find(|(_, c)| c.name == name) else {
        panic!("Failed to find constructor storage struct field")
    };

    constructor.fields.remove(i);
}

pub fn remove_approval(
    module: &mut ir::Module,
    function_bodies: &HashMap<String, sway::Block>,
    removed_function_names: &mut Vec<String>,
    allowances_name: &str,
) {
    let Some(approve_body) = function_bodies.get("approve") else {
        panic!("Approval function body not found");
    };

    let Some(sway::Expression::FunctionCall(f)) = &approve_body.final_expr else {
        panic!("Failed to get approval expression from final expression");
    };

    let Some(approve_fn_name) = f.function.as_identifier() else {
        panic!("Failed to get the function name for approval");
    };

    let Some((i, approve_toplevel_fn)) = module
        .functions
        .iter()
        .enumerate()
        .find(|(_, f)| {
            let sway::TypeName::Function { new_name, .. } = &f.signature else {
                return false;
            };
            new_name == approve_fn_name
        })
        .map(|(i, f)| (i, f.clone()))
    else {
        panic!("Failed to get top level function");
    };

    let toplevel_fn_body = approve_toplevel_fn
        .implementation
        .as_ref()
        .unwrap()
        .body
        .as_ref()
        .unwrap();

    let toplevel_calls = toplevel_fn_body
        .statements
        .iter()
        .filter(|s| matches!(s, sway::Statement::Expression(sway::Expression::FunctionCall(_))))
        .collect::<Vec<_>>();

    assert!(toplevel_calls.len() == 1);

    removed_function_names.push(approve_fn_name.to_string());

    module.functions.remove(i);

    let sway::Statement::Expression(sway::Expression::FunctionCall(function)) = toplevel_calls[0] else {
        unreachable!()
    };

    let Some(approve_private_fn_name) = function.function.as_identifier() else {
        panic!("Failed to get function name")
    };

    let Some((index, private_function)) = module
        .functions
        .iter()
        .enumerate()
        .find(|(_, f)| {
            let sway::TypeName::Function { new_name, .. } = &f.signature else {
                return false;
            };
            new_name == approve_private_fn_name
        })
        .map(|(i, f)| (i, f.clone()))
    else {
        panic!("Failed to get private function");
    };

    let private_fn_body = private_function.implementation.as_ref().unwrap().body.as_ref().unwrap();

    let mut has_allowance_assignment = false;
    let mut has_log = false;

    if private_fn_body
        .find_expression(|expr| {
            if let sway::Expression::FunctionCall(_) = &expr
                && let Some((expr, _)) = expr.to_write_call_parts()
                && let Some((expr, _)) = expr.to_get_call_parts()
                && let Some((expr, _)) = expr.to_get_call_parts()
                && let sway::Expression::MemberAccess(m) = expr
                && let Some("storage_struct") = m.expression.as_identifier()
                && m.member == allowances_name
            {
                return true;
            }

            false
        })
        .is_some()
    {
        has_allowance_assignment = true;
    }

    if private_fn_body
        .find_expression(|expr| {
            if let sway::Expression::FunctionCall(f) = expr
                && let Some(function_name) = f.function.as_identifier()
                && function_name == "log"
                && f.parameters.len() == 1
                && let sway::Expression::FunctionCall(f) = &f.parameters[0]
                && let sway::Expression::PathExpr(path_expr) = &f.function
                && path_expr.segments.len() == 1
                && path_expr.segments[0].name == "Approval"
                && path_expr.segments[0].generic_parameters.is_none()
                && f.parameters.len() == 1
                && let sway::Expression::Tuple(t) = &f.parameters[0]
                && t.len() == 3
            {
                return true;
            }
            false
        })
        .is_some()
    {
        has_log = true;
    }

    if has_allowance_assignment && has_log {
        removed_function_names.push(approve_private_fn_name.to_string());

        module.functions.remove(index);
    }

    remove_unnecessary_functions(module, removed_function_names);
}

pub fn remove_unnecessary_functions(module: &mut ir::Module, removed_function_names: &mut Vec<String>) {
    loop {
        let mut remove_fn_indexes = vec![];

        for (index, function) in module.functions.iter().enumerate() {
            if let Some(function) = &function.implementation
                && let Some(function_body) = &function.body
            {
                if let Some(_) = function_body.find_expression(|x| {
                    if let sway::Expression::FunctionCall(f) = &x {
                        if let Some(identifier) = f.function.as_identifier()
                            && removed_function_names.contains(&identifier.to_string())
                        {
                            return true;
                        }
                    }
                    false
                }) {
                    remove_fn_indexes.push(index);
                    removed_function_names.push(function.new_name.clone());

                    break;
                }
            }
        }

        remove_fn_indexes.sort();

        for i in remove_fn_indexes.iter().rev() {
            module.functions.remove(*i);
        }

        if remove_fn_indexes.is_empty() {
            break;
        }
    }
}

pub fn remove_abi_functions_that_called_removed_functionality(
    contract: Rc<RefCell<ir::Contract>>,
    removed_function_names: &Vec<String>,
) {
    // Remove abi functions that called removed functionality
    let abi_impl_item_count = {
        let contract = contract.borrow();
        contract.abi_impl.items.len()
    };

    for i in (0..abi_impl_item_count).rev() {
        let item = {
            let contract = contract.borrow();
            contract.abi_impl.items[i].clone()
        };

        let sway::ImplItem::Function(function) = item else {
            continue;
        };

        let remove = function
            .body
            .as_ref()
            .unwrap()
            .find_expression(|x| {
                if let sway::Expression::FunctionCall(f) = &x {
                    if let Some(identifier) = f.function.as_identifier() {
                        if removed_function_names.contains(&identifier.to_string()) {
                            return true;
                        }
                    }
                }
                false
            })
            .is_some();

        if remove {
            let sway::ImplItem::Function(function) = contract.borrow().abi_impl.items[i].clone() else {
                unreachable!()
            };

            let index = {
                let Some((index, _)) = contract
                    .borrow()
                    .abi
                    .functions
                    .iter()
                    .enumerate()
                    .find(|(_, f)| f.new_name == function.new_name)
                else {
                    unreachable!()
                };

                index
            };

            contract.borrow_mut().abi.functions.remove(index);
            contract.borrow_mut().abi_impl.items.remove(i);
        }
    }
}

pub fn remove_event_variant(module: &mut ir::Module, contract_name: &str, variant_name: &str) {
    let event_name = format!("{}Event", contract_name);
    let Some(events_enum) = module.events_enums.iter_mut().find(|e| e.0.borrow().name == event_name) else {
        return;
    };

    let index = {
        if let Some((index, _)) = events_enum
            .0
            .borrow()
            .variants
            .iter()
            .enumerate()
            .find(|(_, e)| e.name == variant_name)
        {
            Some(index)
        } else {
            None
        }
    };

    if let Some(index) = index {
        events_enum.0.borrow_mut().variants.remove(index);
    }

    let mut events_enum_impl = events_enum.1.borrow_mut();

    let Some(event_impl) = events_enum_impl.items.iter_mut().find(|i| {
        let sway::ImplItem::Function(f) = i else { return false };
        f.new_name == "abi_encode"
    }) else {
        return;
    };

    let sway::ImplItem::Function(f) = event_impl else {
        unreachable!();
    };

    let sway::Statement::Let(let_expr) = &mut f.body.as_mut().unwrap().statements[0] else {
        unreachable!()
    };

    let sway::Expression::Match(match_expr) = &mut let_expr.value else {
        unreachable!()
    };

    let Some((index, _)) = match_expr.branches.iter().enumerate().find(|(_, b)| {
        if let sway::Expression::FunctionCall(f) = &b.pattern
            && let sway::Expression::PathExpr(path_expr) = &f.function
            && path_expr.segments.len() == 1
            && let sway::PathExprRoot::Identifier(identifier) = &path_expr.root
            && *identifier == event_name
            && path_expr.segments[0].name == variant_name
        {
            return true;
        }

        false
    }) else {
        return;
    };

    match_expr.branches.remove(index);

    for function in module.functions.iter_mut() {
        let Some(implementation) = function.implementation.as_mut() else {
            continue;
        };

        let Some(body) = implementation.body.as_mut() else {
            continue;
        };

        fn check_block(block: &mut sway::Block, event_name: &str, variant_name: &str) {
            let mut remove_indexes = vec![];

            for i in 0..block.statements.len() {
                if let sway::Statement::Expression(stmt_expr) = &block.statements[i]
                    && let sway::Expression::FunctionCall(function_call) = stmt_expr
                    && let Some("log") = function_call.function.as_identifier()
                    && let Some(sway::Expression::FunctionCall(f)) = function_call.parameters.first()
                    && let sway::Expression::PathExpr(path_expr) = &f.function
                    && let sway::PathExprRoot::Identifier(identifier) = &path_expr.root
                    && path_expr.segments.len() == 1
                    && *identifier == event_name
                    && path_expr.segments[0].name == variant_name
                    && path_expr.segments[0].generic_parameters.is_none()
                {
                    remove_indexes.push(i);
                    continue;
                }

                check_statement(&mut block.statements[i], event_name, variant_name);
            }

            for i in remove_indexes.iter().rev() {
                block.statements.remove(*i);
            }

            if let Some(final_expr) = block.final_expr.as_mut() {
                check_expression(final_expr, event_name, variant_name);
            }
        }

        fn check_expression(expression: &mut sway::Expression, event_name: &str, variant_name: &str) {
            match expression {
                sway::Expression::FunctionCall(function_call) => {
                    check_expression(&mut function_call.function, event_name, variant_name);

                    for parameter in function_call.parameters.iter_mut() {
                        check_expression(parameter, event_name, variant_name);
                    }
                }
                sway::Expression::FunctionCallBlock(function_call_block) => {
                    check_expression(&mut function_call_block.function, event_name, variant_name);

                    for field in function_call_block.fields.iter_mut() {
                        check_expression(&mut field.value, event_name, variant_name);
                    }

                    for parameter in function_call_block.parameters.iter_mut() {
                        check_expression(parameter, event_name, variant_name);
                    }
                }
                sway::Expression::Block(block) => {
                    check_block(block, event_name, variant_name);
                }
                sway::Expression::Return(expression) => {
                    if let Some(expression) = expression {
                        check_expression(expression, event_name, variant_name);
                    }
                }
                sway::Expression::Array(array) => {
                    for element in array.elements.iter_mut() {
                        check_expression(element, event_name, variant_name);
                    }
                }
                sway::Expression::ArrayAccess(array_access) => {
                    check_expression(&mut array_access.expression, event_name, variant_name);
                    check_expression(&mut array_access.index, event_name, variant_name);
                }
                sway::Expression::MemberAccess(member_access) => {
                    check_expression(&mut member_access.expression, event_name, variant_name)
                }
                sway::Expression::Tuple(expressions) => {
                    for item in expressions.iter_mut() {
                        check_expression(item, event_name, variant_name);
                    }
                }
                sway::Expression::If(if_expr) => {
                    let mut current_if = Some(if_expr);

                    while let Some(if_expr) = current_if {
                        if let Some(condition) = &mut if_expr.condition {
                            check_expression(condition, event_name, variant_name);
                        }

                        check_block(&mut if_expr.then_body, event_name, variant_name);

                        current_if = if_expr.else_if.as_mut();
                    }
                }
                sway::Expression::Match(match_expr) => {
                    check_expression(&mut match_expr.expression, event_name, variant_name);

                    for branch in match_expr.branches.iter_mut() {
                        check_expression(&mut branch.pattern, event_name, variant_name);
                        check_expression(&mut branch.value, event_name, variant_name);
                    }
                }
                sway::Expression::While(while_expr) => {
                    check_expression(&mut while_expr.condition, event_name, variant_name);

                    check_block(&mut while_expr.body, event_name, variant_name);
                }
                sway::Expression::UnaryExpression(unary_expression) => {
                    check_expression(&mut unary_expression.expression, event_name, variant_name)
                }
                sway::Expression::BinaryExpression(binary_expression) => {
                    check_expression(&mut binary_expression.lhs, event_name, variant_name);
                    check_expression(&mut binary_expression.rhs, event_name, variant_name);
                }
                sway::Expression::Constructor(constructor) => {
                    for field in constructor.fields.iter_mut() {
                        check_expression(&mut field.value, event_name, variant_name);
                    }
                }
                sway::Expression::AsmBlock(asm_block) => {
                    for register in asm_block.registers.iter_mut() {
                        if let Some(register) = register.value.as_mut() {
                            check_expression(register, event_name, variant_name);
                        }
                    }
                }
                sway::Expression::Commented(_, expression) => check_expression(expression, event_name, variant_name),

                _ => {}
            }
        }

        fn check_statement(statement: &mut sway::Statement, event_name: &str, variant_name: &str) {
            match statement {
                sway::Statement::Let(let_expr) => check_expression(&mut let_expr.value, event_name, variant_name),
                sway::Statement::Expression(expression) => check_expression(expression, event_name, variant_name),
                sway::Statement::Commented(_, statement) => {
                    if let Some(statement) = statement {
                        check_statement(statement, event_name, variant_name)
                    }
                }
            }
        }

        check_block(body, &event_name, variant_name);
    }
}

pub fn emplace_src20_events(module: &mut ir::Module, function_bodies: &HashMap<String, sway::Block>) {
    let mut name_name = None;
    let mut symbol_name = None;
    let mut decimals_name = None;
    let mut total_supply_name = None;

    let name_fn_body = function_bodies.get("name").unwrap();
    let symbol_fn_body = function_bodies.get("symbol").unwrap();
    let decimals_fn_body = function_bodies.get("decimals").unwrap();
    let total_supply_fn_body = function_bodies.get("totalSupply").unwrap();

    //
    // Get the name of the `name` storage field
    //

    let Some(sway::Expression::FunctionCall(function_call)) = name_fn_body.final_expr.as_ref() else {
        unreachable!();
    };

    let Some(toplevel_fn_name) = function_call.function.as_identifier() else {
        unreachable!();
    };

    let Some(toplevel_fn) = module.functions.iter().find(|f| {
        let sway::TypeName::Function { new_name, .. } = &f.signature else {
            unreachable!()
        };
        new_name == toplevel_fn_name
    }) else {
        unreachable!();
    };

    let Some(final_expr) = toplevel_fn
        .implementation
        .as_ref()
        .unwrap()
        .body
        .as_ref()
        .unwrap()
        .final_expr
        .as_ref()
    else {
        unreachable!();
    };

    let Some(expr) = final_expr.to_unwrap_call_parts() else {
        unreachable!();
    };

    let Some(expr) = expr.to_read_slice_call_parts() else {
        unreachable!();
    };

    let sway::Expression::MemberAccess(member_access) = expr else {
        unreachable!()
    };

    let Some(storage_struct_parameter) = toplevel_fn
        .implementation
        .as_ref()
        .unwrap()
        .storage_struct_parameter
        .as_ref()
    else {
        unreachable!();
    };

    let Some(identifier) = member_access.expression.as_identifier() else {
        unreachable!();
    };

    assert!(identifier == storage_struct_parameter.name);

    name_name = Some(member_access.member.clone());

    //
    // Get the name of the `symbol` storage field
    //

    let Some(sway::Expression::FunctionCall(function_call)) = symbol_fn_body.final_expr.as_ref() else {
        unreachable!();
    };

    let Some(toplevel_fn_name) = function_call.function.as_identifier() else {
        unreachable!();
    };

    let Some(toplevel_fn) = module.functions.iter().find(|f| {
        let sway::TypeName::Function { new_name, .. } = &f.signature else {
            unreachable!()
        };
        new_name == toplevel_fn_name
    }) else {
        unreachable!();
    };

    let Some(final_expr) = toplevel_fn
        .implementation
        .as_ref()
        .unwrap()
        .body
        .as_ref()
        .unwrap()
        .final_expr
        .as_ref()
    else {
        unreachable!();
    };

    let Some(expr) = final_expr.to_unwrap_call_parts() else {
        unreachable!();
    };

    let Some(expr) = expr.to_read_slice_call_parts() else {
        unreachable!();
    };

    let sway::Expression::MemberAccess(member_access) = expr else {
        unreachable!()
    };

    let Some(storage_struct_parameter) = toplevel_fn
        .implementation
        .as_ref()
        .unwrap()
        .storage_struct_parameter
        .as_ref()
    else {
        unreachable!();
    };

    let Some(identifier) = member_access.expression.as_identifier() else {
        unreachable!();
    };

    assert!(identifier == storage_struct_parameter.name);

    symbol_name = Some(member_access.member.clone());

    //
    // Get the name of the `decimals` storage field
    //

    let Some(sway::Expression::FunctionCall(function_call)) = decimals_fn_body.final_expr.as_ref() else {
        unreachable!();
    };

    let Some(toplevel_fn_name) = function_call.function.as_identifier() else {
        unreachable!();
    };

    let Some(toplevel_fn) = module.functions.iter().find(|f| {
        let sway::TypeName::Function { new_name, .. } = &f.signature else {
            unreachable!()
        };
        new_name == toplevel_fn_name
    }) else {
        unreachable!();
    };

    let Some(final_expr) = toplevel_fn
        .implementation
        .as_ref()
        .unwrap()
        .body
        .as_ref()
        .unwrap()
        .final_expr
        .as_ref()
    else {
        unreachable!();
    };

    let Some(expr) = final_expr.to_read_call_parts() else {
        unreachable!();
    };

    let sway::Expression::MemberAccess(member_access) = expr else {
        unreachable!()
    };

    let Some(storage_struct_parameter) = toplevel_fn
        .implementation
        .as_ref()
        .unwrap()
        .storage_struct_parameter
        .as_ref()
    else {
        unreachable!();
    };

    let Some(identifier) = member_access.expression.as_identifier() else {
        unreachable!();
    };

    assert!(identifier == storage_struct_parameter.name);

    decimals_name = Some(member_access.member.clone());

    //
    // Get the name of the `total_supply` storage field
    //

    let Some(sway::Expression::FunctionCall(function_call)) = total_supply_fn_body.final_expr.as_ref() else {
        unreachable!();
    };

    let Some(toplevel_fn_name) = function_call.function.as_identifier() else {
        unreachable!();
    };

    let Some(toplevel_fn) = module.functions.iter().find(|f| {
        let sway::TypeName::Function { new_name, .. } = &f.signature else {
            unreachable!()
        };
        new_name == toplevel_fn_name
    }) else {
        unreachable!();
    };

    let Some(final_expr) = toplevel_fn
        .implementation
        .as_ref()
        .unwrap()
        .body
        .as_ref()
        .unwrap()
        .final_expr
        .as_ref()
    else {
        unreachable!();
    };

    let Some(expr) = final_expr.to_read_call_parts() else {
        unreachable!();
    };

    let sway::Expression::MemberAccess(member_access) = expr else {
        unreachable!()
    };

    let Some(storage_struct_parameter) = toplevel_fn
        .implementation
        .as_ref()
        .unwrap()
        .storage_struct_parameter
        .as_ref()
    else {
        unreachable!();
    };

    let Some(identifier) = member_access.expression.as_identifier() else {
        unreachable!();
    };

    assert!(identifier == storage_struct_parameter.name);

    total_supply_name = Some(member_access.member.clone());

    //
    // Emplace event logs when any storage fields are written to
    //

    let name_name = (name_name.unwrap(), true);
    let symbol_name = (symbol_name.unwrap(), true);
    let decimals_name = (decimals_name.unwrap(), false);
    let total_supply_name = (total_supply_name.unwrap(), false);

    for function in module.functions.iter_mut() {
        if let Some(implementation) = function.implementation.as_mut() {
            if let Some(body) = implementation.body.as_mut() {
                if let Some(storage_struct_parameter) = implementation.storage_struct_parameter.as_ref() {
                    let mut check_expression = |(name, is_write_slice): (String, bool)| {
                        if let Some(expression) = body.find_expression_mut(|expr| {
                            if let Some((expr, _)) = expr.to_write_slice_call_parts()
                                && is_write_slice
                            {
                                let sway::Expression::MemberAccess(member_access) = expr else {
                                    return false;
                                };

                                let Some(storage_struct_name) = member_access.expression.as_identifier() else {
                                    return false;
                                };

                                if storage_struct_name != storage_struct_parameter.name {
                                    return false;
                                }

                                return member_access.member == name;
                            } else if let Some((expr, _)) = expr.to_write_call_parts()
                                && !is_write_slice
                            {
                                let sway::Expression::MemberAccess(member_access) = expr else {
                                    return false;
                                };

                                let Some(storage_struct_name) = member_access.expression.as_identifier() else {
                                    return false;
                                };

                                if storage_struct_name != storage_struct_parameter.name {
                                    return false;
                                }

                                return member_access.member == name;
                            }
                            false
                        }) {
                            if let Some((expr, value)) = expression.to_write_slice_call_parts() {
                                let sway::Expression::MemberAccess(member_access) = expr else {
                                    unreachable!();
                                };

                                *expression = sway::Expression::from(sway::Block {
                                    statements: vec![
                                        sway::Statement::from(expression.clone()),
                                        sway::Statement::from(if member_access.member == name_name.0 {
                                            sway::Expression::create_function_call(
                                                "src20::SetNameEvent::new",
                                                None,
                                                vec![
                                                    sway::Expression::create_function_call(
                                                        "AssetId::default",
                                                        None,
                                                        vec![],
                                                    ),
                                                    value.into_some_call(),
                                                    sway::Expression::create_function_call("msg_sender", None, vec![])
                                                        .with_unwrap_call(),
                                                ],
                                            )
                                            .with_function_call(
                                                "log",
                                                None,
                                                vec![],
                                            )
                                        } else if member_access.member == symbol_name.0 {
                                            sway::Expression::create_function_call(
                                                "src20::SetSymbolEvent::new",
                                                None,
                                                vec![
                                                    sway::Expression::create_function_call(
                                                        "AssetId::default",
                                                        None,
                                                        vec![],
                                                    ),
                                                    value.into_some_call(),
                                                    sway::Expression::create_function_call("msg_sender", None, vec![])
                                                        .with_unwrap_call(),
                                                ],
                                            )
                                            .with_function_call(
                                                "log",
                                                None,
                                                vec![],
                                            )
                                        } else {
                                            unreachable!()
                                        }),
                                    ],
                                    final_expr: None,
                                });
                            } else if let Some((expr, value)) = expression.to_write_call_parts() {
                                let sway::Expression::MemberAccess(member_access) = expr else {
                                    unreachable!();
                                };

                                if member_access.member == decimals_name.0 {
                                    *expression =
                                        sway::Expression::from(sway::Block {
                                            statements: vec![
                                                sway::Statement::from(expression.clone()),
                                                sway::Statement::from(
                                                    sway::Expression::create_function_call(
                                                        "src20::SetDecimalsEvent::new",
                                                        None,
                                                        vec![
                                                            sway::Expression::create_function_call(
                                                                "AssetId::default",
                                                                None,
                                                                vec![],
                                                            ),
                                                            value.clone(),
                                                            sway::Expression::create_function_call(
                                                                "msg_sender",
                                                                None,
                                                                vec![],
                                                            )
                                                            .with_unwrap_call(),
                                                        ],
                                                    )
                                                    .with_function_call("log", None, vec![]),
                                                ),
                                            ],
                                            final_expr: None,
                                        })
                                } else if member_access.member == total_supply_name.0 {
                                    let is_burn = if let sway::Expression::BinaryExpression(binary_expr) = &value {
                                        binary_expr.operator == "-" || binary_expr.operator == "/"
                                    } else {
                                        false
                                    };

                                    let value = sway::Expression::create_function_call(
                                        "u64::try_from",
                                        None,
                                        vec![value.clone()],
                                    )
                                    .with_unwrap_call();

                                    *expression = sway::Expression::from(sway::Block {
                                        statements: vec![
                                            sway::Statement::from(expression.clone()),
                                            sway::Statement::from(sway::Expression::create_function_call(
                                                if is_burn {
                                                    "std::asset::burn"
                                                } else {
                                                    "std::asset::mint"
                                                },
                                                None,
                                                vec![
                                                    sway::Expression::create_function_call("SubId::zero", None, vec![]),
                                                    value.clone(),
                                                ],
                                            )),
                                            sway::Statement::from(
                                                sway::Expression::create_function_call(
                                                    "src20::TotalSupplyEvent::new",
                                                    None,
                                                    vec![
                                                        sway::Expression::create_function_call(
                                                            "AssetId::default",
                                                            None,
                                                            vec![],
                                                        ),
                                                        value.clone(),
                                                        sway::Expression::create_function_call(
                                                            "msg_sender",
                                                            None,
                                                            vec![],
                                                        )
                                                        .with_unwrap_call(),
                                                    ],
                                                )
                                                .with_function_call(
                                                    "log",
                                                    None,
                                                    vec![],
                                                ),
                                            ),
                                        ],
                                        final_expr: None,
                                    })
                                }
                            }
                        }
                    };

                    check_expression(name_name.clone());
                    check_expression(symbol_name.clone());
                    check_expression(decimals_name.clone());
                    check_expression(total_supply_name.clone());
                }
            }
        }
    }
}

pub fn implement_src20_for_contract(
    contract: Rc<RefCell<ir::Contract>>,
    function_bodies: &HashMap<String, sway::Block>,
) {
    contract.borrow_mut().impls.push(sway::Impl {
        generic_parameters: None,
        type_name: sway::TypeName::create_identifier("SRC20"),
        for_type_name: Some(sway::TypeName::create_identifier("Contract")),
        items: vec![
            // #[storage(read)]
            // fn total_assets() -> u64 {
            //     1
            // }
            sway::ImplItem::Function(sway::Function {
                attributes: Some(sway::AttributeList {
                    attributes: vec![sway::Attribute {
                        name: "storage".to_string(),
                        parameters: Some(vec!["read".to_string()]),
                    }],
                }),
                is_public: false,
                old_name: String::new(),
                new_name: "total_assets".to_string(),
                generic_parameters: None,
                parameters: sway::ParameterList { entries: vec![] },
                storage_struct_parameter: None,
                return_type: Some(sway::TypeName::create_identifier("u64")),
                body: Some(sway::Block {
                    statements: vec![],
                    final_expr: Some(sway::Expression::create_dec_int_literal(1_u8.into(), None)),
                }),
            }),
            // #[storage(read)]
            // fn total_supply(asset: AssetId) -> Option<u64> {
            //     if asset == AssetId::default() {
            //         Some(storage.total_supply.read())
            //     } else {
            //         None
            //     }
            // }
            sway::ImplItem::Function(sway::Function {
                attributes: Some(sway::AttributeList {
                    attributes: vec![sway::Attribute {
                        name: "storage".to_string(),
                        parameters: Some(vec!["read".to_string()]),
                    }],
                }),
                is_public: false,
                old_name: String::new(),
                new_name: "total_supply".to_string(),
                generic_parameters: None,
                parameters: sway::ParameterList {
                    entries: vec![sway::Parameter {
                        is_ref: false,
                        is_mut: false,
                        name: "asset".to_string(),
                        type_name: Some(sway::TypeName::create_identifier("AssetId")),
                    }],
                },
                storage_struct_parameter: None,
                return_type: Some(sway::TypeName::create_identifier("u64").to_option()),
                body: Some(sway::Block {
                    statements: vec![],
                    final_expr: Some(sway::Expression::from(sway::If {
                        condition: Some(sway::Expression::from(sway::BinaryExpression {
                            operator: "==".to_string(),
                            lhs: sway::Expression::create_identifier("asset"),
                            rhs: sway::Expression::create_function_call("AssetId::default", None, vec![]),
                        })),
                        then_body: sway::Block {
                            statements: vec![],
                            final_expr: Some(sway::Expression::create_function_call(
                                "Some",
                                None,
                                vec![
                                    sway::Expression::create_function_call(
                                        "u64::try_from",
                                        None,
                                        vec![function_bodies.get("totalSupply").unwrap().clone().into()],
                                    )
                                    .with_unwrap_call(),
                                ],
                            )),
                        },
                        else_if: Some(Box::new(sway::If {
                            condition: None,
                            then_body: sway::Block {
                                statements: vec![],
                                final_expr: Some(sway::Expression::create_identifier("None")),
                            },
                            else_if: None,
                        })),
                    })),
                }),
            }),
            // #[storage(read)]
            // fn name(asset: AssetId) -> Option<String> {
            //     if asset == AssetId::default() {
            //         Some(storage.name.read_slice().unwrap())
            //     } else {
            //         None
            //     }
            // }
            sway::ImplItem::Function(sway::Function {
                attributes: Some(sway::AttributeList {
                    attributes: vec![sway::Attribute {
                        name: "storage".to_string(),
                        parameters: Some(vec!["read".to_string()]),
                    }],
                }),
                is_public: false,
                old_name: String::new(),
                new_name: "name".to_string(),
                generic_parameters: None,
                parameters: sway::ParameterList {
                    entries: vec![sway::Parameter {
                        is_ref: false,
                        is_mut: false,
                        name: "asset".to_string(),
                        type_name: Some(sway::TypeName::create_identifier("AssetId")),
                    }],
                },
                storage_struct_parameter: None,
                return_type: Some(sway::TypeName::create_identifier("String").to_option()),
                body: Some(sway::Block {
                    statements: vec![],
                    final_expr: Some(sway::Expression::from(sway::If {
                        condition: Some(sway::Expression::from(sway::BinaryExpression {
                            operator: "==".to_string(),
                            lhs: sway::Expression::create_identifier("asset"),
                            rhs: sway::Expression::create_function_call("AssetId::default", None, vec![]),
                        })),
                        then_body: sway::Block {
                            statements: vec![],
                            final_expr: Some(sway::Expression::create_function_call(
                                "Some",
                                None,
                                vec![function_bodies.get("name").unwrap().clone().into()],
                            )),
                        },
                        else_if: Some(Box::new(sway::If {
                            condition: None,
                            then_body: sway::Block {
                                statements: vec![],
                                final_expr: Some(sway::Expression::create_identifier("None")),
                            },
                            else_if: None,
                        })),
                    })),
                }),
            }),
            // #[storage(read)]
            // fn symbol(asset: AssetId) -> Option<String> {
            //     if asset == AssetId::default() {
            //         Some(storage.symbol.read_slice().unwrap())
            //     } else {
            //         None
            //     }
            // }
            sway::ImplItem::Function(sway::Function {
                attributes: Some(sway::AttributeList {
                    attributes: vec![sway::Attribute {
                        name: "storage".to_string(),
                        parameters: Some(vec!["read".to_string()]),
                    }],
                }),
                is_public: false,
                old_name: String::new(),
                new_name: "symbol".to_string(),
                generic_parameters: None,
                parameters: sway::ParameterList {
                    entries: vec![sway::Parameter {
                        is_ref: false,
                        is_mut: false,
                        name: "asset".to_string(),
                        type_name: Some(sway::TypeName::create_identifier("AssetId")),
                    }],
                },
                storage_struct_parameter: None,
                return_type: Some(sway::TypeName::create_identifier("String").to_option()),
                body: Some(sway::Block {
                    statements: vec![],
                    final_expr: Some(sway::Expression::from(sway::If {
                        condition: Some(sway::Expression::from(sway::BinaryExpression {
                            operator: "==".to_string(),
                            lhs: sway::Expression::create_identifier("asset"),
                            rhs: sway::Expression::create_function_call("AssetId::default", None, vec![]),
                        })),
                        then_body: sway::Block {
                            statements: vec![],
                            final_expr: Some(sway::Expression::create_function_call(
                                "Some",
                                None,
                                vec![function_bodies.get("symbol").unwrap().clone().into()],
                            )),
                        },
                        else_if: Some(Box::new(sway::If {
                            condition: None,
                            then_body: sway::Block {
                                statements: vec![],
                                final_expr: Some(sway::Expression::create_identifier("None")),
                            },
                            else_if: None,
                        })),
                    })),
                }),
            }),
            // #[storage(read)]
            // fn decimals(asset: AssetId) -> Option<u8> {
            //     if asset == AssetId::default() {
            //         Some(storage.decimals.read())
            //     } else {
            //         None
            //     }
            // }
            sway::ImplItem::Function(sway::Function {
                attributes: Some(sway::AttributeList {
                    attributes: vec![sway::Attribute {
                        name: "storage".to_string(),
                        parameters: Some(vec!["read".to_string()]),
                    }],
                }),
                is_public: false,
                old_name: String::new(),
                new_name: "decimals".to_string(),
                generic_parameters: None,
                parameters: sway::ParameterList {
                    entries: vec![sway::Parameter {
                        is_ref: false,
                        is_mut: false,
                        name: "asset".to_string(),
                        type_name: Some(sway::TypeName::create_identifier("AssetId")),
                    }],
                },
                storage_struct_parameter: None,
                return_type: Some(sway::TypeName::create_identifier("u8").to_option()),
                body: Some(sway::Block {
                    statements: vec![],
                    final_expr: Some(sway::Expression::from(sway::If {
                        condition: Some(sway::Expression::from(sway::BinaryExpression {
                            operator: "==".to_string(),
                            lhs: sway::Expression::create_identifier("asset"),
                            rhs: sway::Expression::create_function_call("AssetId::default", None, vec![]),
                        })),
                        then_body: sway::Block {
                            statements: vec![],
                            final_expr: Some(sway::Expression::create_function_call(
                                "Some",
                                None,
                                vec![function_bodies.get("decimals").unwrap().clone().into()],
                            )),
                        },
                        else_if: Some(Box::new(sway::If {
                            condition: None,
                            then_body: sway::Block {
                                statements: vec![],
                                final_expr: Some(sway::Expression::create_identifier("None")),
                            },
                            else_if: None,
                        })),
                    })),
                }),
            }),
        ],
    });
}
