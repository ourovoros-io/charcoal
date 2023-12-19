contract;

use std::bytes::Bytes;
use std::constants::ZERO_B256;
use std::hash::Hash;

pub enum ERC20Error {
    ERC20InsufficientBalance: (Identity, u64, u64),
    ERC20InvalidSender: Identity,
    ERC20InvalidReceiver: Identity,
    ERC20InsufficientAllowance: (Identity, u64, u64),
    ERC20InvalidApprover: Identity,
    ERC20InvalidSpender: Identity,
}

pub enum ERC20Event {
    Transfer: (Identity, Identity, u64),
    Approval: (Identity, Identity, u64),
}

abi ERC20 {
    // function name() external view returns (string memory);
    #[storage(read)]
    fn name() -> str[32];

    // function symbol() external view returns (string memory);
    #[storage(read)]
    fn symbol() -> str[32];

    // function decimals() external view returns (uint8);
    #[storage(read)]
    fn decimals() -> u8;

    // function totalSupply() external view returns (uint256);
    #[storage(read)]
    fn total_supply() -> u64;

    // function balanceOf(address account) external view returns (uint256);
    #[storage(read)]
    fn balance_of(account: Identity) -> u64;

    // function transfer(address to, uint256 value) external returns (bool);
    #[storage(read, write)]
    fn transfer(to: Identity, value: u64) -> bool;

    // function allowance(address owner, address spender) external view returns (uint256);
    #[storage(read)]
    fn allowance(owner: Identity, spender: Identity) -> u64;

    // function approve(address spender, uint256 value) external returns (bool);
    #[storage(read, write)]
    fn approve(spender: Identity, value: u64) -> bool;

    // function transferFrom(address from, address to, uint256 value) external returns (bool);
    #[storage(read, write)]
    fn transfer_from(from: Identity, to: Identity, value: u64) -> bool;
}

storage {
    // mapping(address account => uint256) private _balances;
    _balances: StorageMap<Identity, u64> = StorageMap {},

    // mapping(address account => mapping(address spender => uint256)) private _allowances;
    _allowances: StorageMap<Identity, StorageMap<Identity, u64>> = StorageMap {},

    // uint256 private _totalSupply;
    _total_supply: u64 = 0,

    // string private _name;
    _name: str[32] = __to_str_array("                                "),

    // string private _symbol;
    _symbol: str[32] = __to_str_array("                                "),
}

// function _msgSender() internal view virtual returns (address)
#[storage(read)]
fn _msg_sender() -> Identity {
    msg_sender().unwrap()
}

// function _msgData() internal view virtual returns (bytes calldata)
#[storage(read)]
fn _msg_data() -> Bytes {
    // NOTE:
    // In strict translation mode, this should be a translation error, since there is no `msg.data` in sway.
    // In easy mode, we can generate a log-and-revert like so:
    log("ERROR: `msg.data` not available");
    revert(0);

    // NOTE:
    // Even though this code is known to be unreachable by the friggin compiler,
    // we still have to return a value of the expected type. :(
    Bytes::new()
}

// function _contextSuffixLength() internal view virtual returns (uint256)
#[storage(read)]
fn _context_suffix_length() -> u64 {
    0
}

// function totalSupply() external view returns (uint256);
#[storage(read)]
fn total_supply() -> u64 {
    storage._total_supply.read()
}

// function balanceOf(address account) external view returns (uint256);
#[storage(read)]
fn balance_of(account: Identity) -> u64 {
    storage._balances.get(account).read()
}

// function transfer(address to, uint256 value) external returns (bool);
#[storage(read, write)]
fn transfer(to: Identity, value: u64) -> bool {
    let owner = _msg_sender();
    _transfer(owner, to, value);
    true
}

// function allowance(address owner, address spender) external view returns (uint256);
#[storage(read)]
fn allowance(owner: Identity, spender: Identity) -> u64 {
    storage._allowances.get(owner).get(spender).read()
}

// function approve(address spender, uint256 value) external returns (bool);
#[storage(read, write)]
fn approve(spender: Identity, value: u64) -> bool {
    let owner = _msg_sender();
    _approve(owner, spender, value);
    true
}

// function transferFrom(address from, address to, uint256 value) external returns (bool);
#[storage(read, write)]
fn transfer_from(from: Identity, to: Identity, value: u64) -> bool {
    let spender = _msg_sender();
    _spend_allowance(from, spender, value);
    _transfer(from, to, value);
    true
}

// function name() external view returns (string memory);
#[storage(read)]
fn name() -> str[32] {
    storage._name.read()
}

// function symbol() external view returns (string memory);
#[storage(read)]
fn symbol() -> str[32] {
    storage._symbol.read()
}

// function decimals() external view returns (uint8);
fn decimals() -> u8 {
    18
}

// function _transfer(address from, address to, uint256 value) internal;
#[storage(read, write)]
fn _transfer(from: Identity, to: Identity, value: u64) {
    if from == Identity::Address(Address::from(ZERO_B256)) {
        log(ERC20Error::ERC20InvalidSender(Identity::Address(Address::from(ZERO_B256))));
        revert(0);
    }
    if to == Identity::Address(Address::from(ZERO_B256)) {
        log(ERC20Error::ERC20InvalidReceiver(Identity::Address(Address::from(ZERO_B256))));
        revert(0);
    }
    _update(from, to, value);
}

// function _update(address from, address to, uint256 value) internal virtual;
#[storage(read, write)]
fn _update(from: Identity, to: Identity, value: u64) {
    if from == Identity::Address(Address::from(ZERO_B256)) {
        storage._total_supply.write(storage._total_supply.read() + value);
    } else {
        let from_balance = storage._balances.get(from).read();
        if from_balance < value {
            log(ERC20Error::ERC20InsufficientBalance((from, from_balance, value)));
            revert(0);
        }
        storage._balances.insert(from, from_balance - value);
    }

    if from == Identity::Address(Address::from(ZERO_B256)) {
        storage._total_supply.write(storage._total_supply.read() - value);
    } else {
        storage._balances.insert(to, value);
    }

    log(ERC20Event::Transfer((from, to, value)));
}

// function _mint(address account, uint256 value) internal;
#[storage(read, write)]
fn _mint(account: Identity, value: u64) {
    if account == Identity::Address(Address::from(ZERO_B256)) {
        log(ERC20Error::ERC20InvalidReceiver(Identity::Address(Address::from(ZERO_B256))));
        revert(0);
    }
    _update(Identity::Address(Address::from(ZERO_B256)), account, value);
}

// function _burn(address account, uint256 value) internal;
#[storage(read, write)]
fn _burn(account: Identity, value: u64) {
    if account == Identity::Address(Address::from(ZERO_B256)) {
        log(ERC20Error::ERC20InvalidSender(Identity::Address(Address::from(ZERO_B256))));
        revert(0);
    }
    _update(account, Identity::Address(Address::from(ZERO_B256)), value);
}

// function _approve(address owner, address spender, uint256 value) internal;
#[storage(read, write)]
fn _approve(owner: Identity, spender: Identity, value: u64) {
    _approve2(owner, spender, value, true);
}

// NOTE: can't have multiple functions with the same name, we need to determine how to handle it
// function _approve(address owner, address spender, uint256 value, bool emitEvent) internal virtual;
#[storage(read, write)]
fn _approve2(owner: Identity, spender: Identity, value: u64, emit_event: bool) {
    if owner == Identity::Address(Address::from(ZERO_B256)) {
        log(ERC20Error::ERC20InvalidApprover(Identity::Address(Address::from(ZERO_B256))));
        revert(0);
    }
    if spender == Identity::Address(Address::from(ZERO_B256)) {
        log(ERC20Error::ERC20InvalidSpender(Identity::Address(Address::from(ZERO_B256))));
        revert(0);
    }
    storage._allowances.get(owner).insert(spender, value);
    if emit_event {
        log(ERC20Event::Approval((owner, spender, value)));
    }
}

// function _spendAllowance(address owner, address spender, uint256 value) internal virtual;
#[storage(read, write)]
fn _spend_allowance(owner: Identity, spender: Identity, value: u64) {
    let current_allowance = allowance(owner, spender);

    // TODO: find equivalent u64 max instead of using a constant
    if current_allowance != 0xFFFFFFFFFFFFFFFF {
        if current_allowance < value {
            log(ERC20Error::ERC20InsufficientAllowance((spender, current_allowance, value)));
            revert(0);
        }

        _approve2(owner, spender, current_allowance - value, false);
    }
}

impl ERC20 for Contract {
    // function name() external view returns (string memory);
    #[storage(read)]
    fn name() -> str[32] {
        ::name()
    }

    // function symbol() external view returns (string memory);
    #[storage(read)]
    fn symbol() -> str[32] {
        ::symbol()
    }

    // function decimals() external view returns (uint8);
    #[storage(read)]
    fn decimals() -> u8 {
        ::decimals()
    }

    // function totalSupply() external view returns (uint256);
    #[storage(read)]
    fn total_supply() -> u64 {
        ::total_supply()
    }

    // function balanceOf(address account) external view returns (uint256);
    #[storage(read)]
    fn balance_of(account: Identity) -> u64 {
        ::balance_of(account)
    }

    // function transfer(address to, uint256 value) external returns (bool);
    #[storage(read, write)]
    fn transfer(to: Identity, value: u64) -> bool {
        ::transfer(to, value)
    }

    // function allowance(address owner, address spender) external view returns (uint256);
    #[storage(read)]
    fn allowance(owner: Identity, spender: Identity) -> u64 {
        ::allowance(owner, spender)
    }

    // function approve(address spender, uint256 value) external returns (bool);
    #[storage(read, write)]
    fn approve(spender: Identity, value: u64) -> bool {
        ::approve(spender, value)
    }

    // function transferFrom(address from, address to, uint256 value) external returns (bool);
    #[storage(read, write)]
    fn transfer_from(from: Identity, to: Identity, value: u64) -> bool {
        ::transfer_from(from, to, value)
    }
}
