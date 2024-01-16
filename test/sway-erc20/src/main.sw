contract;

use std::hash::Hash;
use std::constants::ZERO_B256;

enum ERC20Event {
    Transfer: (Identity, Identity, u64),
    Approval: (Identity, Identity, u64),
}

enum ERC20Error {
    ERC20InsufficientBalance: (Identity, u64, u64),
    ERC20InvalidSender: Identity,
    ERC20InvalidReceiver: Identity,
    ERC20InsufficientAllowance: (Identity, u64, u64),
    ERC20InvalidApprover: Identity,
    ERC20InvalidSpender: Identity,
}

abi ERC20 {
    #[storage(read)]
    fn total_supply() -> u64;

    #[storage(read)]
    fn balance_of(account: Identity) -> u64;

    #[storage(read, write)]
    fn transfer(to: Identity, value: u64) -> bool;

    #[storage(read)]
    fn allowance(owner: Identity, spender: Identity) -> u64;

    #[storage(read, write)]
    fn approve(spender: Identity, value: u64) -> bool;

    #[storage(read, write)]
    fn transfer_from(from: Identity, to: Identity, value: u64) -> bool;

    #[storage(read)]
    fn name() -> str[32];

    #[storage(read)]
    fn symbol() -> str[32];

    #[storage(read)]
    fn decimals() -> u8;
}

storage {
    _balances: StorageMap<Identity, u64> = StorageMap {},
    _allowances: StorageMap<Identity, StorageMap<Identity, u64>> = StorageMap {},
    _total_supply: u64 = 0,
    _name: str[32] = __to_str_array("                                "),
    _symbol: str[32] = __to_str_array("                                "),
}

#[storage(read)]
fn _msg_sender() -> Identity {
    msg_sender().unwrap()
}

#[storage(read)]
fn _msg_data() -> std::bytes::Bytes {
    std::inputs::input_message_data(0, 0)
}

#[storage(read)]
fn _context_suffix_length() -> u64 {
    0
}

#[storage(read, write)]
fn constructor(name_: str[32], symbol_: str[32]) {
    storage._name.write(name_);
    storage._symbol.write(symbol_);
}

#[storage(read)]
fn allowance(owner: Identity, spender: Identity) -> u64 {
    storage._allowances.get(owner).get(spender).read()
}

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

#[storage(read, write)]
fn _update(from: Identity, to: Identity, value: u64) {
    if from == Identity::Address(Address::from(ZERO_B256)) {
        storage._total_supply.write(storage._total_supply.read() + value);
    } else {
        let from_balance: u64 = storage._balances.get(from).read();
        if from_balance < value {
            log(ERC20Error::ERC20InsufficientBalance((from, from_balance, value)));
            revert(0);
        }
        storage._balances.get(from).write(from_balance - value);
    }
    if to == Identity::Address(Address::from(ZERO_B256)) {
        storage._total_supply.write(storage._total_supply.read() - value);
    } else {
        storage._balances.get(to).write(storage._balances.get(to).read() + value);
    }
    log(ERC20Event::Transfer((from, to, value)));
}

#[storage(read, write)]
fn _mint(account: Identity, value: u64) {
    if account == Identity::Address(Address::from(ZERO_B256)) {
        log(ERC20Error::ERC20InvalidReceiver(Identity::Address(Address::from(ZERO_B256))));
        revert(0);
    }
    _update(Identity::Address(Address::from(ZERO_B256)), account, value);
}

#[storage(read, write)]
fn _burn(account: Identity, value: u64) {
    if account == Identity::Address(Address::from(ZERO_B256)) {
        log(ERC20Error::ERC20InvalidSender(Identity::Address(Address::from(ZERO_B256))));
        revert(0);
    }
    _update(account, Identity::Address(Address::from(ZERO_B256)), value);
}

#[storage(read, write)]
fn _approve(owner: Identity, spender: Identity, value: u64) {
    _approve_2(owner, spender, value, true);
}

#[storage(read, write)]
fn _approve_2(owner: Identity, spender: Identity, value: u64, emit_event: bool) {
    if owner == Identity::Address(Address::from(ZERO_B256)) {
        log(ERC20Error::ERC20InvalidApprover(Identity::Address(Address::from(ZERO_B256))));
        revert(0);
    }
    if spender == Identity::Address(Address::from(ZERO_B256)) {
        log(ERC20Error::ERC20InvalidSpender(Identity::Address(Address::from(ZERO_B256))));
        revert(0);
    }
    storage._allowances.get(owner).get(spender).write(value);
    if emit_event {
        log(ERC20Event::Approval((owner, spender, value)));
    }
}

#[storage(read, write)]
fn _spend_allowance(owner: Identity, spender: Identity, value: u64) {
    let current_allowance: u64 = allowance(owner, spender);
    if current_allowance != 0xFFFFFFFFFFFFFFFF {
        if current_allowance < value {
            log(ERC20Error::ERC20InsufficientAllowance((spender, current_allowance, value)));
            revert(0);
        }
        _approve_2(owner, spender, current_allowance - value, false);
    }
}

impl ERC20 for Contract {
    #[storage(read)]
    fn name() -> str[32] {
        storage._name.read()
    }

    #[storage(read)]
    fn symbol() -> str[32] {
        storage._symbol.read()
    }

    #[storage(read)]
    fn decimals() -> u8 {
        18
    }

    #[storage(read)]
    fn total_supply() -> u64 {
        storage._total_supply.read()
    }

    #[storage(read)]
    fn balance_of(account: Identity) -> u64 {
        storage._balances.get(account).read()
    }

    #[storage(read, write)]
    fn transfer(to: Identity, value: u64) -> bool {
        let owner: Identity = _msg_sender();
        _transfer(owner, to, value);
        true
    }

    #[storage(read)]
    fn allowance(owner: Identity, spender: Identity) -> u64 {
        ::allowance(owner, spender)
    }

    #[storage(read, write)]
    fn approve(spender: Identity, value: u64) -> bool {
        let owner: Identity = _msg_sender();
        _approve(owner, spender, value);
        true
    }

    #[storage(read, write)]
    fn transfer_from(from: Identity, to: Identity, value: u64) -> bool {
        let spender: Identity = _msg_sender();
        _spend_allowance(from, spender, value);
        _transfer(from, to, value);
        true
    }
}
