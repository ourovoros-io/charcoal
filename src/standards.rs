use crate::project::Project;
use crate::translate::generate_enum_abi_encode_function;
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
        optional: bool,
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
                optional: false,
            },
            // function symbol() public view returns (string);
            StandardDefinitionPart::Function {
                name: "symbol",
                arguments: &[],
                returns: &[solidity::Type::String],
                optional: false,
            },
            // function decimals() public view returns (uint8);
            StandardDefinitionPart::Function {
                name: "decimals",
                arguments: &[],
                returns: &[solidity::Type::Uint(8)],
                optional: false,
            },
            // function totalSupply() public view returns (uint256);
            StandardDefinitionPart::Function {
                name: "totalSupply",
                arguments: &[],
                returns: &[solidity::Type::Uint(256)],
                optional: false,
            },
            // function balanceOf(address _owner) public view returns (uint256 balance);
            StandardDefinitionPart::Function {
                name: "balanceOf",
                arguments: &[solidity::Type::Address],
                returns: &[solidity::Type::Uint(256)],
                optional: false,
            },
            // function transfer(address _to, uint256 _value) public returns (bool success);
            StandardDefinitionPart::Function {
                name: "transfer",
                arguments: &[solidity::Type::Address, solidity::Type::Uint(256)],
                returns: &[solidity::Type::Bool],
                optional: false,
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
                optional: false,
            },
            // function approve(address _spender, uint256 _value) public returns (bool success);
            StandardDefinitionPart::Function {
                name: "approve",
                arguments: &[solidity::Type::Address, solidity::Type::Uint(256)],
                returns: &[solidity::Type::Bool],
                optional: false,
            },
            // function allowance(address _owner, address _spender) public view returns (uint256 remaining);
            StandardDefinitionPart::Function {
                name: "allowance",
                arguments: &[solidity::Type::Address, solidity::Type::Address],
                returns: &[solidity::Type::Uint(256)],
                optional: false,
            },
            // function mint(address to, uint256 amount) external;
            StandardDefinitionPart::Function {
                name: "mint",
                arguments: &[solidity::Type::Address, solidity::Type::Uint(256)],
                returns: &[],
                optional: true,
            },
            // function burn(address to, uint256 amount) external;
            StandardDefinitionPart::Function {
                name: "burn",
                arguments: &[solidity::Type::Address, solidity::Type::Uint(256)],
                returns: &[],
                optional: true,
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

#[inline(always)]
pub fn remove_abi_functions_for_standard(
    contract: Rc<RefCell<ir::Contract>>,
    standard_definition: &StandardDefinition,
) -> HashMap<String, sway::Block> {
    let functions_names = match standard_definition.name {
        Standard::ERC20 => &["name", "symbol", "decimals", "total_supply", "mint", "burn"],
    };

    let abi_fn_count = {
        let contract = contract.borrow();
        contract.abi.functions.len()
    };

    for i in (0..abi_fn_count).rev() {
        let function = {
            let contract = contract.borrow();
            contract.abi.functions[i].clone()
        };

        if functions_names.contains(&function.new_name.as_str()) {
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

        if functions_names.contains(&function.new_name.as_str()) {
            function_bodies.insert(function.old_name.clone(), function.body.as_ref().unwrap().clone());
            contract.borrow_mut().abi_impl.items.remove(i);
        }
    }

    function_bodies
}

#[inline(always)]
pub fn implement_src20_for_contract(
    module: &mut ir::Module,
    contract: Rc<RefCell<ir::Contract>>,
    function_bodies: &HashMap<String, sway::Block>,
) {
    // src20 = "0.8.1"
    module.ensure_dependency_declared("src20 = \"0.8.1\"");

    // use src20::{SetDecimalsEvent, SetNameEvent, SetSymbolEvent, SRC20, TotalSupplyEvent};
    module.ensure_use_declared("src20::SetDecimalsEvent");
    module.ensure_use_declared("src20::SetNameEvent");
    module.ensure_use_declared("src20::SetSymbolEvent");
    module.ensure_use_declared("src20::TotalSupplyEvent");
    module.ensure_use_declared("src20::SRC20");

    contract
        .borrow_mut()
        .abi
        .inherits
        .push(sway::TypeName::create_identifier("SRC20"));

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
                modifier_calls: vec![],
                contract: None,
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
                modifier_calls: vec![],
                contract: None,
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
                modifier_calls: vec![],
                contract: None,
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
                modifier_calls: vec![],
                contract: None,
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
                modifier_calls: vec![],
                contract: None,
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

#[inline(always)]
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
            let expr2 = expression.clone();
            let Some((expr, value)) = expr2.to_write_call_parts() else {
                unreachable!()
            };

            let mut value = value.clone();

            let Some((_expr, recipient)) = expr.to_get_call_parts() else {
                unreachable!()
            };

            match &value {
                sway::Expression::BinaryExpression(binary_expression) => {
                    let is_balance_of_read = |expr: &sway::Expression| -> bool {
                        let Some(expr) = expr.to_read_call_parts() else {
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
                    };

                    if is_balance_of_read(&binary_expression.lhs) && is_balance_of_read(&binary_expression.rhs) {
                        panic!("lhs and rhs are balance of reads");
                    } else if is_balance_of_read(&binary_expression.lhs) {
                        if binary_expression.operator == "-" {
                            *expression = sway::Expression::Comment(format!("Not supported: {}", expression.display()));
                            continue;
                        }

                        assert!(binary_expression.operator == "+");
                        value = binary_expression.rhs.clone();
                    } else if is_balance_of_read(&binary_expression.rhs) {
                        if binary_expression.operator == "-" {
                            *expression = sway::Expression::Comment(format!("Not supported: {}", expression.display()));
                            continue;
                        }

                        assert!(binary_expression.operator == "+");
                        value = binary_expression.lhs.clone();
                    } else {
                        panic!("unexpected binary expression: {}", value.display());
                    }
                }

                _ => {}
            }

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

#[inline(always)]
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

    if let Some(expr) = final_expr.to_unwrap_call_parts() {
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
    }

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

    if let Some(expr) = final_expr.to_unwrap_call_parts() {
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
    }

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

    if let Some(expr) = final_expr.to_read_call_parts() {
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
    }

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

    if let Some(expr) = final_expr.to_read_call_parts() {
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
    }

    //
    // Emplace event logs when any storage fields are written to
    //

    let name_name = (name_name, true);
    let symbol_name = (symbol_name, true);
    let decimals_name = (decimals_name, false);
    let total_supply_name = (total_supply_name, false);

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

                                *expression =
                                    sway::Expression::from(sway::Block {
                                        statements: vec![
                                            sway::Statement::from(expression.clone()),
                                            sway::Statement::from(
                                                if name_name
                                                    .0
                                                    .as_ref()
                                                    .map(|n| *n == member_access.member)
                                                    .unwrap_or(false)
                                                {
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
                                                            sway::Expression::create_function_call(
                                                                "msg_sender",
                                                                None,
                                                                vec![],
                                                            )
                                                            .with_unwrap_call(),
                                                        ],
                                                    )
                                                    .with_function_call("log", None, vec![])
                                                } else if symbol_name
                                                    .0
                                                    .as_ref()
                                                    .map(|n| *n == member_access.member)
                                                    .unwrap_or(false)
                                                {
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
                                                            sway::Expression::create_function_call(
                                                                "msg_sender",
                                                                None,
                                                                vec![],
                                                            )
                                                            .with_unwrap_call(),
                                                        ],
                                                    )
                                                    .with_function_call("log", None, vec![])
                                                } else {
                                                    unreachable!()
                                                },
                                            ),
                                        ],
                                        final_expr: None,
                                    });
                            } else if let Some((expr, value)) = expression.to_write_call_parts() {
                                let sway::Expression::MemberAccess(member_access) = expr else {
                                    unreachable!();
                                };

                                if decimals_name
                                    .0
                                    .as_ref()
                                    .map(|n| *n == member_access.member)
                                    .unwrap_or(false)
                                {
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
                                } else if total_supply_name
                                    .0
                                    .as_ref()
                                    .map(|n| *n == member_access.member)
                                    .unwrap_or(false)
                                {
                                    let mut value = value.clone();

                                    let is_burn = if let sway::Expression::BinaryExpression(binary_expr) = value.clone()
                                    {
                                        if let Some(expr) = binary_expr.lhs.to_read_call_parts() {
                                            let sway::Expression::MemberAccess(member_access) = expr else {
                                                unreachable!()
                                            };

                                            let Some(storage_struct_name) = member_access.expression.as_identifier()
                                            else {
                                                unreachable!()
                                            };

                                            if storage_struct_name != storage_struct_parameter.name {
                                                unreachable!()
                                            }

                                            if member_access.member == name {
                                                value = binary_expr.rhs.clone();
                                            } else {
                                                panic!()
                                            }
                                        }

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

                    if let Some(x) = name_name.0.as_ref() {
                        check_expression((x.to_string(), name_name.1))
                    }
                    if let Some(x) = symbol_name.0.as_ref() {
                        check_expression((x.to_string(), symbol_name.1))
                    }
                    if let Some(x) = decimals_name.0.as_ref() {
                        check_expression((x.to_string(), decimals_name.1))
                    }
                    if let Some(x) = total_supply_name.0.as_ref() {
                        check_expression((x.to_string(), total_supply_name.1))
                    }
                }
            }
        }
    }
}

#[inline(always)]
fn get_balances_storage_field_name(module: Rc<RefCell<ir::Module>>, contract: Rc<RefCell<ir::Contract>>) -> String {
    let contract = contract.borrow();

    let Some(balance_of_impl_item) = contract.abi_impl.items.iter().find(|f| {
        let sway::ImplItem::Function(f) = f else {
            return false;
        };
        f.new_name == "balance_of" && f.body.is_some()
    }) else {
        panic!("balanceOf function body not found");
    };

    let sway::ImplItem::Function(sway::Function {
        body: Some(balance_of_body),
        ..
    }) = balance_of_impl_item
    else {
        unreachable!()
    };

    let Some(sway::Expression::FunctionCall(f)) = &balance_of_body.final_expr else {
        panic!("Failed to get balanceOf expression from final expression");
    };

    let Some(balance_of_fn_name) = f.function.as_identifier() else {
        panic!("Failed to get the function name for balanceOf");
    };

    // Get the top level function and index
    let Some(balance_of_toplevel_fn) = module
        .borrow()
        .functions
        .iter()
        .find(|f| {
            let sway::TypeName::Function { new_name, .. } = &f.signature else {
                return false;
            };
            new_name == balance_of_fn_name
        })
        .map(|f| f.clone())
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

    member_access.member.clone()
}

#[inline(always)]
pub fn implement_withdraw_function_for_src20(
    project: &Project,
    module: Rc<RefCell<ir::Module>>,
    contract: Rc<RefCell<ir::Contract>>,
) {
    let balance_of_field_name = get_balances_storage_field_name(module.clone(), contract.clone());

    let mut contract = contract.borrow_mut();
    assert!(!contract.abi.functions.iter().any(|f| f.new_name == "withdraw"));

    let event_name = format!("{}Event", contract.name);

    let mut needs_abi_encode = false;

    if !module
        .borrow()
        .events_enums
        .iter()
        .any(|e| e.0.borrow().name == event_name)
    {
        module.borrow_mut().events_enums.push((
            Rc::new(RefCell::new(sway::Enum {
                attributes: None,
                is_public: true,
                name: event_name.clone(),
                generic_parameters: None,
                variants: vec![],
            })),
            Rc::new(RefCell::new(sway::Impl {
                type_name: sway::TypeName::create_identifier("AbiEncode"),
                for_type_name: Some(sway::TypeName::create_identifier(event_name.as_str())),
                ..Default::default()
            })),
        ));
        needs_abi_encode = true;
    }

    let events_enum = {
        let module = module.borrow();
        let events_enum = module
            .events_enums
            .iter()
            .find(|e| e.0.borrow().name == event_name)
            .cloned()
            .unwrap();

        (events_enum.0.clone(), events_enum.1.clone())
    };

    assert!(!events_enum.0.borrow().variants.iter().any(|v| v.name == "Withdraw"));

    events_enum.0.borrow_mut().variants.push(sway::EnumVariant {
        name: "Withdraw".to_string(),
        type_name: sway::TypeName::Tuple {
            type_names: vec![
                sway::TypeName::create_identifier("Identity"),
                sway::TypeName::create_identifier("u64"),
            ],
        },
    });

    if !needs_abi_encode {
        events_enum.1.borrow_mut().items.clear();
    }

    generate_enum_abi_encode_function(project, module, events_enum.0.clone(), events_enum.1.clone()).unwrap();

    // #[storage(read, write)]
    // fn withdraw(amount: u64)
    let mut function = sway::Function {
        attributes: Some(sway::AttributeList {
            attributes: vec![sway::Attribute {
                name: "storage".to_string(),
                parameters: Some(vec!["read".to_string(), "write".to_string()]),
            }],
        }),
        is_public: false,
        old_name: String::new(),
        new_name: "withdraw".to_string(),
        generic_parameters: None,
        parameters: sway::ParameterList {
            entries: vec![sway::Parameter {
                is_ref: false,
                is_mut: false,
                name: "amount".to_string(),
                type_name: Some(sway::TypeName::create_identifier("u64")),
            }],
        },
        storage_struct_parameter: None,
        return_type: None,
        modifier_calls: vec![],
        contract: None,
        body: None,
    };

    contract.abi.functions.push(function.clone());

    function.body = Some(sway::Block {
        statements: vec![
            // let recipient = msg_sender().unwrap();
            sway::Statement::from(sway::Let {
                pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                    is_mutable: false,
                    name: "recipient".to_string(),
                }),
                type_name: None,
                value: sway::Expression::create_function_call("msg_sender", None, vec![]).with_unwrap_call(),
            }),
            // let balance = storage::contract_erc_20_to_src_20._balances.get(recipient).read();
            sway::Statement::from(sway::Let {
                pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                    is_mutable: false,
                    name: "balance".to_string(),
                }),
                type_name: None,
                value: sway::Expression::create_identifier(
                    format!("storage::{}", contract.name.to_case(Case::Snake)).as_str(),
                )
                .with_member(&balance_of_field_name)
                .with_get_call(sway::Expression::create_identifier("recipient"))
                .with_read_call(),
            }),
            // require(balance >= amount, "Insufficient funds");
            sway::Statement::from(sway::Expression::create_function_call(
                "require",
                None,
                vec![
                    sway::Expression::from(sway::BinaryExpression {
                        operator: ">=".to_string(),
                        lhs: sway::Expression::create_identifier("balance"),
                        rhs: sway::Expression::create_identifier("amount").with_as_u256_call(),
                    }),
                    sway::Expression::create_string_literal("Insufficient funds"),
                ],
            )),
            // storage::contract_erc_20_to_src_20._balances.get(recipient).write(balance - amount);
            sway::Statement::from(
                sway::Expression::create_identifier(
                    format!("storage::{}", contract.name.to_case(Case::Snake)).as_str(),
                )
                .with_member(&balance_of_field_name)
                .with_get_call(sway::Expression::create_identifier("recipient"))
                .with_write_call(sway::Expression::from(sway::BinaryExpression {
                    operator: "-".to_string(),
                    lhs: sway::Expression::create_identifier("balance"),
                    rhs: sway::Expression::create_identifier("amount").with_as_u256_call(),
                })),
            ),
            // std::asset::transfer(recipient, AssetId::default(), amount);
            sway::Statement::from(sway::Expression::create_function_call(
                "std::asset::transfer",
                None,
                vec![
                    sway::Expression::create_identifier("recipient"),
                    sway::Expression::create_function_call("AssetId::default", None, vec![]),
                    sway::Expression::create_identifier("amount"),
                ],
            )),
            // log(ContractERC20ToSRC20Event::Withdraw((recipient, amount)));
            sway::Statement::from(sway::Expression::create_function_call(
                "log",
                None,
                vec![sway::Expression::create_function_call(
                    format!("{}::Withdraw", event_name).as_str(),
                    None,
                    vec![sway::Expression::Tuple(vec![
                        sway::Expression::create_identifier("recipient"),
                        sway::Expression::create_identifier("amount"),
                    ])],
                )],
            )),
        ],
        final_expr: None,
    });

    contract.abi_impl.items.push(sway::ImplItem::Function(function));
}

#[inline(always)]
pub fn implement_src3_for_contract(
    module: &mut ir::Module,
    contract: Rc<RefCell<ir::Contract>>,
    function_bodies: &HashMap<String, sway::Block>,
) {
    let Some(mut mint_body) = function_bodies.get("mint").cloned() else {
        return;
    };

    let Some(mut burn_body) = function_bodies.get("burn").cloned() else {
        return;
    };

    let Some(sway::Expression::FunctionCall(mint_function_call)) = mint_body.final_expr.as_mut() else {
        panic!()
    };

    let Some(sway::Expression::FunctionCall(burn_function_call)) = burn_body.final_expr.as_mut() else {
        panic!()
    };

    let Some(_mint_fn_name) = mint_function_call.function.as_identifier().map(|i| i.to_string()) else {
        panic!("Failed to get the mint fn name");
    };

    let Some(_burn_fn_name) = burn_function_call.function.as_identifier().map(|i| i.to_string()) else {
        panic!("Failed to get the mint fn name");
    };

    let Some(mint_recipient_name) = mint_function_call.parameters[0].as_identifier().map(|i| i.to_string()) else {
        panic!()
    };

    let Some(mint_amount_name) = mint_function_call.parameters[1].as_identifier().map(|i| i.to_string()) else {
        panic!()
    };

    let Some(burn_recipient_name) = burn_function_call.parameters[0].as_identifier().map(|i| i.to_string()) else {
        panic!()
    };

    let Some(burn_amount_name) = burn_function_call.parameters[1].as_identifier().map(|i| i.to_string()) else {
        panic!()
    };

    mint_function_call.parameters[1] = mint_function_call.parameters[1].with_as_u256_call();
    burn_function_call.parameters[1] = burn_function_call.parameters[1].with_as_u256_call();

    // src3 = "0.8.1"
    module.ensure_dependency_declared("src3 = \"0.8.1\"");

    // use src3::SRC3;
    module.ensure_use_declared("src3::SRC3");

    // require(sub_id.is_some() && sub_id.unwrap() == DEFAULT_SUB_ID,"Incorrect Sub Id");
    mint_body.statements.insert(
        0,
        sway::Statement::from(sway::Expression::create_function_call(
            "require",
            None,
            vec![
                sway::Expression::from(sway::BinaryExpression {
                    operator: "&&".to_string(),
                    lhs: sway::Expression::create_identifier("sub_id").with_function_call("is_some", None, vec![]),
                    rhs: sway::Expression::from(sway::BinaryExpression {
                        operator: "==".to_string(),
                        lhs: sway::Expression::create_identifier("sub_id").with_unwrap_call(),
                        rhs: sway::Expression::create_identifier("std::constants::DEFAULT_SUB_ID"),
                    }),
                }),
                sway::Expression::create_string_literal("Incorrect Sub Id"),
            ],
        )),
    );

    // let address = msg_sender().unwrap();

    burn_body.statements.insert(
        0,
        sway::Statement::from(sway::Let {
            pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                is_mutable: false,
                name: burn_recipient_name,
            }),
            type_name: None,
            value: sway::Expression::create_function_call("msg_sender", None, vec![]).with_unwrap_call(),
        }),
    );

    // require(msg_asset_id() == AssetId::default(), "Incorrect asset provided");
    burn_body.statements.insert(
        0,
        sway::Statement::from(sway::Expression::create_function_call(
            "require",
            None,
            vec![
                sway::Expression::from(sway::BinaryExpression {
                    operator: "==".to_string(),
                    lhs: sway::Expression::create_function_call("std::call_frames::msg_asset_id", None, vec![]),
                    rhs: sway::Expression::create_function_call("AssetId::default", None, vec![]),
                }),
                sway::Expression::create_string_literal("Incorrect asset provided"),
            ],
        )),
    );

    // require(msg_amount() >= amount, "Incorrect amount provided");
    burn_body.statements.insert(
        0,
        sway::Statement::from(sway::Expression::create_function_call(
            "require",
            None,
            vec![
                sway::Expression::from(sway::BinaryExpression {
                    operator: ">=".to_string(),
                    lhs: sway::Expression::create_function_call("std::context::msg_amount", None, vec![]),
                    rhs: sway::Expression::create_identifier(&burn_amount_name),
                }),
                sway::Expression::create_string_literal("Incorrect amount provided"),
            ],
        )),
    );

    // require(sub_id == DEFAULT_SUB_ID, "Incorrect Sub Id");
    burn_body.statements.insert(
        0,
        sway::Statement::from(sway::Expression::create_function_call(
            "require",
            None,
            vec![
                sway::Expression::from(sway::BinaryExpression {
                    operator: "==".to_string(),
                    lhs: sway::Expression::create_identifier("sub_id"),
                    rhs: sway::Expression::create_identifier("std::constants::DEFAULT_SUB_ID"),
                }),
                sway::Expression::create_string_literal("Incorrect Sub Id"),
            ],
        )),
    );

    contract.borrow_mut().impls.push(sway::Impl {
        generic_parameters: None,
        type_name: sway::TypeName::create_identifier("SRC3"),
        for_type_name: Some(sway::TypeName::create_identifier("Contract")),
        items: vec![
            sway::ImplItem::Function(sway::Function {
                attributes: Some(sway::AttributeList {
                    attributes: vec![sway::Attribute {
                        name: "storage".to_string(),
                        parameters: Some(vec!["read".to_string(), "write".to_string().to_string()]),
                    }],
                }),
                is_public: false,
                old_name: String::new(),
                new_name: "mint".to_string(),
                generic_parameters: None,
                parameters: sway::ParameterList {
                    entries: vec![
                        sway::Parameter {
                            is_ref: false,
                            is_mut: false,
                            name: mint_recipient_name.to_string(),
                            type_name: Some(sway::TypeName::create_identifier("Identity")),
                        },
                        sway::Parameter {
                            is_ref: false,
                            is_mut: false,
                            name: "sub_id".to_string(),
                            type_name: Some(sway::TypeName::create_identifier("SubId").to_option()),
                        },
                        sway::Parameter {
                            is_ref: false,
                            is_mut: false,
                            name: mint_amount_name.to_string(),
                            type_name: Some(sway::TypeName::create_identifier("u64")),
                        },
                    ],
                },
                storage_struct_parameter: None,
                return_type: None,
                modifier_calls: vec![],
                contract: None,
                body: Some(mint_body),
            }),
            sway::ImplItem::Function(sway::Function {
                attributes: Some(sway::AttributeList {
                    attributes: vec![
                        sway::Attribute {
                            name: "storage".to_string(),
                            parameters: Some(vec!["read".to_string(), "write".to_string().to_string()]),
                        },
                        sway::Attribute {
                            name: "payable".to_string(),
                            parameters: None,
                        },
                    ],
                }),
                is_public: false,
                old_name: String::new(),
                new_name: "burn".to_string(),
                generic_parameters: None,
                parameters: sway::ParameterList {
                    entries: vec![
                        sway::Parameter {
                            is_ref: false,
                            is_mut: false,
                            name: "sub_id".to_string(),
                            type_name: Some(sway::TypeName::create_identifier("SubId")),
                        },
                        sway::Parameter {
                            is_ref: false,
                            is_mut: false,
                            name: burn_amount_name.to_string(),
                            type_name: Some(sway::TypeName::create_identifier("u64")),
                        },
                    ],
                },
                storage_struct_parameter: None,
                return_type: None,
                modifier_calls: vec![],
                contract: None,
                body: Some(burn_body),
            }),
        ],
    })
}

#[inline(always)]
pub fn implement_src5_for_contract(module: Rc<RefCell<ir::Module>>, contract: Rc<RefCell<ir::Contract>>) {
    let mut only_owner_modifiers = vec![];
    let mut storage_field_name = String::new();

    for modifier in module.borrow().modifiers.iter() {
        let Some(implementation) = modifier.implementation.as_ref() else {
            continue;
        };

        let implementation = implementation.borrow();

        let Some(inline_body) = implementation.inline_body.as_ref() else {
            continue;
        };

        // If case
        if let Some(expr) = inline_body.find_expression(|expr| {
            let sway::Expression::If(if_expr) = expr else {
                return false;
            };

            let Some(sway::Expression::BinaryExpression(binary_expr)) = if_expr.condition.as_ref() else {
                return false;
            };

            if binary_expr.operator != "!=" {
                return false;
            }

            if binary_expr.rhs != sway::Expression::create_function_call("msg_sender", None, vec![]).with_unwrap_call()
            {
                return false;
            }

            if !if_expr
                .then_body
                .find_expression(|expr| {
                    let sway::Expression::FunctionCall(f) = &expr else {
                        return false;
                    };

                    let Some("revert") = f.function.as_identifier() else {
                        return false;
                    };

                    if f.parameters.len() != 1 {
                        return false;
                    }

                    if f.parameters[0] != sway::Expression::create_dec_int_literal(0_u8.into(), None) {
                        return false;
                    }

                    true
                })
                .is_some()
            {
                return false;
            }

            true
        }) {
            let sway::Expression::If(if_expr) = expr else {
                unreachable!();
            };

            let Some(sway::Expression::BinaryExpression(binary_expr)) = if_expr.condition.as_ref() else {
                unreachable!();
            };

            let sway::Expression::FunctionCall(func_call) = &binary_expr.lhs else {
                unreachable!();
            };

            let sway::Expression::MemberAccess(member_access) = &func_call.function else {
                unreachable!();
            };

            let sway::Expression::MemberAccess(member_access) = &member_access.expression else {
                unreachable!();
            };

            if !only_owner_modifiers.contains(&implementation.new_name) {
                only_owner_modifiers.push(implementation.new_name.clone());
            }

            storage_field_name = member_access.member.clone();
            continue;
        }

        // Require case
        if let Some(expr) = inline_body.find_expression(|expr| {
            let sway::Expression::FunctionCall(function_call) = expr else {
                return false;
            };

            let Some("require") = function_call.function.as_identifier() else {
                return false;
            };

            if function_call.parameters.len() != 2 {
                return false;
            }

            let sway::Expression::BinaryExpression(binary_expr) = &function_call.parameters[0] else {
                return false;
            };

            if binary_expr.operator != "==" {
                return false;
            }

            if binary_expr.rhs != sway::Expression::create_function_call("msg_sender", None, vec![]).with_unwrap_call()
            {
                return false;
            }

            true
        }) {
            let sway::Expression::FunctionCall(function_call) = expr else {
                unreachable!();
            };

            let sway::Expression::BinaryExpression(binary_expr) = &function_call.parameters[0] else {
                unreachable!();
            };

            let sway::Expression::FunctionCall(func_call) = &binary_expr.lhs else {
                unreachable!();
            };

            let sway::Expression::MemberAccess(member_access) = &func_call.function else {
                unreachable!();
            };

            let sway::Expression::MemberAccess(member_access) = &member_access.expression else {
                unreachable!();
            };

            if !only_owner_modifiers.contains(&implementation.new_name) {
                only_owner_modifiers.push(implementation.new_name.clone());
            }

            storage_field_name = member_access.member.clone();
            continue;
        }

        // Assert case
        if let Some(expr) = inline_body.find_expression(|expr| {
            let sway::Expression::FunctionCall(function_call) = expr else {
                return false;
            };

            let Some("assert") = function_call.function.as_identifier() else {
                return false;
            };

            if function_call.parameters.len() != 1 {
                return false;
            }

            let sway::Expression::BinaryExpression(binary_expr) = &function_call.parameters[0] else {
                return false;
            };

            if binary_expr.operator != "==" {
                return false;
            }

            if binary_expr.rhs != sway::Expression::create_function_call("msg_sender", None, vec![]).with_unwrap_call()
            {
                return false;
            }

            true
        }) {
            let sway::Expression::FunctionCall(function_call) = expr else {
                unreachable!();
            };

            let sway::Expression::BinaryExpression(binary_expr) = &function_call.parameters[0] else {
                unreachable!();
            };

            let sway::Expression::FunctionCall(func_call) = &binary_expr.lhs else {
                unreachable!();
            };

            let sway::Expression::MemberAccess(member_access) = &func_call.function else {
                unreachable!();
            };

            let sway::Expression::MemberAccess(member_access) = &member_access.expression else {
                unreachable!();
            };

            if !only_owner_modifiers.contains(&implementation.new_name) {
                only_owner_modifiers.push(implementation.new_name.clone());
            }

            storage_field_name = member_access.member.clone();
            continue;
        }
    }

    if !only_owner_modifiers.is_empty() {
        let mut module = module.borrow_mut();
        // src5 = "0.8.1"
        module.ensure_dependency_declared("src5 = \"0.8.1\"");

        // use src5::{SRC5, State};
        module.ensure_use_declared("src5::SRC5");
        module.ensure_use_declared("src5::State");

        let contract_name = {
            let contract = contract.borrow();
            contract.name.clone()
        };

        let storage_namespace_name = contract_name.to_case(Case::Snake);

        contract.borrow_mut().impls.push(sway::Impl {
            generic_parameters: None,
            type_name: sway::TypeName::create_identifier("SRC5"),
            for_type_name: Some(sway::TypeName::create_identifier("Contract")),
            items: vec![sway::ImplItem::Function(sway::Function {
                attributes: Some(sway::AttributeList {
                    attributes: vec![sway::Attribute {
                        name: "storage".to_string(),
                        parameters: Some(vec!["read".to_string()]),
                    }],
                }),
                is_public: false,
                old_name: String::new(),
                new_name: "owner".to_string(),
                generic_parameters: None,
                parameters: sway::ParameterList { entries: vec![] },
                storage_struct_parameter: None,
                return_type: Some(sway::TypeName::create_identifier("State")),
                modifier_calls: vec![],
                contract: None,
                body: Some(sway::Block {
                    statements: vec![],
                    final_expr: Some(
                        sway::Expression::create_identifier(format!("storage::{}", storage_namespace_name).as_str())
                            .with_member(storage_field_name.as_str())
                            .with_read_call(),
                    ),
                }),
            })],
        });

        let storage = contract.borrow_mut().storage.as_ref().unwrap().clone();

        if let Some(namespace) = storage
            .borrow_mut()
            .namespaces
            .iter_mut()
            .find(|ns| ns.borrow().name == storage_namespace_name)
        {
            if let Some(owner_field) = namespace
                .borrow_mut()
                .fields
                .iter_mut()
                .find(|f| f.name == storage_field_name)
            {
                if owner_field.type_name != sway::TypeName::create_identifier("Identity") {
                    panic!("Owner field must be of type Identity");
                }

                *owner_field = sway::StorageField {
                    old_name: String::new(),
                    name: storage_field_name.clone(),
                    type_name: sway::TypeName::create_identifier("State"),
                    value: sway::Expression::create_identifier("State::Uninitialized"),
                };
            } else {
                panic!("Owner field not found in storage struct");
            }
        }

        let storage_struct_rc = contract.borrow_mut().storage_struct.as_ref().unwrap().clone();

        // Now we need to update the owner field in the storage struct
        if let Some(owner_field) = storage_struct_rc
            .borrow_mut()
            .storage
            .fields
            .iter_mut()
            .find(|f| f.new_name.contains(&storage_field_name) && f.type_name.is_storage_key())
        {
            owner_field.type_name = sway::TypeName::create_identifier("StorageKey<State>");
        }

        // Finally, we need to update the constructor to initialize the owner field
        for item in contract.borrow_mut().abi_impl.items.iter_mut() {
            let sway::ImplItem::Function(sway::Function { new_name, body, .. }) = item else {
                continue;
            };

            if new_name != "constructor" {
                continue;
            }

            let Some(body) = body.as_mut() else {
                continue;
            };

            let Some(sway::Expression::FunctionCall(function_call)) = body.final_expr.as_mut() else {
                unreachable!();
            };

            // First we need to get the toplevel function constructor function name
            let Some(toplevel_fn_name) = function_call.function.as_identifier() else {
                unreachable!();
            };

            let Some(toplevel_fn) = module.functions.iter_mut().find(|f| {
                let sway::TypeName::Function { new_name, .. } = &f.signature else {
                    unreachable!()
                };
                new_name == toplevel_fn_name
            }) else {
                unreachable!();
            };

            let Some(expr) = toplevel_fn.implementation.as_mut() else {
                unreachable!();
            };

            let Some(function_body) = expr.body.as_mut() else {
                unreachable!();
            };

            // Now we can iterate over the statements in the function body to find the statement that initializes the owner field
            for statement in function_body.statements.iter_mut() {
                let sway::Statement::Expression(sway::Expression::FunctionCall(function_call)) = statement else {
                    continue;
                };

                let sway::Expression::MemberAccess(member_access) = &function_call.function else {
                    continue;
                };

                let sway::Expression::MemberAccess(member_access) = &member_access.expression else {
                    continue;
                };

                if member_access.member != storage_field_name {
                    continue;
                }

                *statement = sway::Statement::from(
                    sway::Expression::create_identifier("storage_struct")
                        .with_member(storage_field_name.as_str())
                        .with_write_call(sway::Expression::create_function_call(
                            "State::Initialized",
                            None,
                            vec![sway::Expression::create_function_call("msg_sender", None, vec![]).with_unwrap_call()],
                        )),
                );
            }
        }

        // We need to add the only_owner function to the contract shared code
        module.functions.push(ir::Item {
            signature: sway::TypeName::Function {
                old_name: String::new(),
                new_name: format!("{}_only_owner", contract_name.to_case(Case::Snake)),
                generic_parameters: None,
                parameters: sway::ParameterList { entries: vec![] },
                storage_struct_parameter: None,
                return_type: None,
                contract: None,
            },
            implementation: Some(sway::Function {
                attributes: Some(sway::AttributeList {
                    attributes: vec![sway::Attribute {
                        name: "storage".to_string(),
                        parameters: Some(vec!["read".to_string()]),
                    }],
                }),
                is_public: true,
                old_name: String::new(),
                new_name: format!("{}_only_owner", contract_name.to_case(Case::Snake)),
                generic_parameters: None,
                parameters: sway::ParameterList {
                    entries: vec![sway::Parameter {
                        is_ref: false,
                        is_mut: false,
                        name: "storage_struct".to_string(),
                        type_name: Some(sway::TypeName::create_identifier(
                            format!("{}Storage", contract_name).as_str(),
                        )),
                    }],
                },
                storage_struct_parameter: None,
                return_type: None,
                modifier_calls: vec![],
                contract: None,
                body: Some(sway::Block {
                    statements: vec![sway::Statement::from(sway::Expression::create_function_call(
                        "require",
                        None,
                        vec![
                            sway::Expression::from(sway::BinaryExpression {
                                operator: "==".to_string(),
                                lhs: sway::Expression::create_function_call(
                                    "State::Initialized",
                                    None,
                                    vec![
                                        sway::Expression::create_function_call("msg_sender", None, vec![])
                                            .with_unwrap_call(),
                                    ],
                                ),
                                rhs: sway::Expression::create_identifier("storage_struct")
                                    .with_member(storage_field_name.as_str())
                                    .with_read_call(),
                            }),
                            sway::Expression::create_string_literal("Ownable: caller is not the owner"),
                        ],
                    ))],
                    final_expr: None,
                }),
            }),
        });

        // check any functions that had the owner check modifier and insert the owner check logic in the top of the function
        for function in module.functions.iter_mut() {
            let Some(implementation) = function.implementation.as_mut() else {
                unreachable!()
            };

            let Some(body) = implementation.body.as_mut() else {
                unreachable!()
            };

            let mut modifier_new_name = None;

            for modifier_call in implementation.modifier_calls.iter() {
                if only_owner_modifiers.contains(&modifier_call.new_name) {
                    modifier_new_name = Some(modifier_call.new_name.clone());
                    break;
                }
            }

            let Some(modifier_new_name) = modifier_new_name else {
                continue;
            };

            for i in (0..body.statements.len()).rev() {
                let sway::Statement::Commented(comment, _) = &body.statements[i] else {
                    continue;
                };

                if comment.starts_with(format!("inlined modifier invocation: {}(", modifier_new_name).as_str()) {
                    body.statements.remove(i);
                }
            }

            // If case
            if let Some(expr) = body.find_expression_mut(|expr| {
                let sway::Expression::If(if_expr) = expr else {
                    return false;
                };

                let Some(sway::Expression::BinaryExpression(binary_expr)) = if_expr.condition.as_ref() else {
                    return false;
                };

                if binary_expr.operator != "!=" {
                    return false;
                }

                if binary_expr.rhs
                    != sway::Expression::create_function_call("msg_sender", None, vec![]).with_unwrap_call()
                {
                    return false;
                }

                if !if_expr
                    .then_body
                    .find_expression(|expr| {
                        let sway::Expression::FunctionCall(f) = &expr else {
                            return false;
                        };

                        let Some("revert") = f.function.as_identifier() else {
                            return false;
                        };

                        if f.parameters.len() != 1 {
                            return false;
                        }

                        if f.parameters[0] != sway::Expression::create_dec_int_literal(0_u8.into(), None) {
                            return false;
                        }

                        true
                    })
                    .is_some()
                {
                    return false;
                }

                true
            }) {
                *expr = sway::Expression::create_function_call(
                    format!("{}_only_owner", contract_name.to_case(Case::Snake)).as_str(),
                    None,
                    vec![sway::Expression::create_identifier("storage_struct")],
                );
                continue;
            }

            // Require case
            if let Some(expr) = body.find_expression_mut(|expr| {
                let sway::Expression::FunctionCall(function_call) = expr else {
                    return false;
                };

                let Some("require") = function_call.function.as_identifier() else {
                    return false;
                };

                if function_call.parameters.len() != 2 {
                    return false;
                }

                let sway::Expression::BinaryExpression(binary_expr) = &function_call.parameters[0] else {
                    return false;
                };

                if binary_expr.operator != "==" {
                    return false;
                }

                if binary_expr.rhs
                    != sway::Expression::create_function_call("msg_sender", None, vec![]).with_unwrap_call()
                {
                    return false;
                }

                true
            }) {
                *expr = sway::Expression::create_function_call(
                    format!("{}_only_owner", contract_name.to_case(Case::Snake)).as_str(),
                    None,
                    vec![sway::Expression::create_identifier("storage_struct")],
                );
                continue;
            }

            // Assert case
            if let Some(expr) = body.find_expression_mut(|expr| {
                let sway::Expression::FunctionCall(function_call) = expr else {
                    return false;
                };

                let Some("assert") = function_call.function.as_identifier() else {
                    return false;
                };

                if function_call.parameters.len() != 1 {
                    return false;
                }

                let sway::Expression::BinaryExpression(binary_expr) = &function_call.parameters[0] else {
                    return false;
                };

                if binary_expr.operator != "==" {
                    return false;
                }

                if binary_expr.rhs
                    != sway::Expression::create_function_call("msg_sender", None, vec![]).with_unwrap_call()
                {
                    return false;
                }

                true
            }) {
                *expr = sway::Expression::create_function_call(
                    format!("{}_only_owner", contract_name.to_case(Case::Snake)).as_str(),
                    None,
                    vec![sway::Expression::create_identifier("storage_struct")],
                );
                continue;
            }

            if let Some(expr) = body.find_expression_mut(|expr| {
                let sway::Expression::If(if_expr) = expr else {
                    return false;
                };

                let Some(sway::Expression::BinaryExpression(binary_expr)) = if_expr.condition.as_ref() else {
                    return false;
                };

                if binary_expr.operator != "!=" {
                    return false;
                }

                let sway::Expression::FunctionCall(func_call) = &binary_expr.lhs else {
                    return false;
                };

                let sway::Expression::MemberAccess(member_access) = &func_call.function else {
                    return false;
                };

                let sway::Expression::MemberAccess(member_access) = &member_access.expression else {
                    return false;
                };

                if !member_access.member.contains("owner") {
                    return false;
                }

                if binary_expr.rhs
                    != sway::Expression::create_function_call("msg_sender", None, vec![]).with_unwrap_call()
                {
                    return false;
                }

                true
            }) {
                *expr = sway::Expression::create_function_call(
                    format!("{}_only_owner", contract_name.to_case(Case::Snake)).as_str(),
                    None,
                    vec![sway::Expression::create_identifier("storage_struct")],
                );
            }
        }
    }
}
