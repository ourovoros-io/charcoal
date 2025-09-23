use solang_parser::pt as solidity;

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
