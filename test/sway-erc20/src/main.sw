contract;

use std::hash::Hash;
use std::constants::ZERO_B256;
use std::math::Power;

type Rounding = u8;

impl Rounding {
    const FLOOR: Rounding = 0;
    const CEIL: Rounding = 1;
    const TRUNC: Rounding = 2;
    const EXPAND: Rounding = 3;
}

enum MathError {
    MathOverflowedMulDiv: (),
}

fn try_add(a: u64, b: u64) -> (bool, u64) {
    let c: u64 = a + b;
    if c < a {
        return (false, 0);
    }
    (true, c)
}

fn try_sub(a: u64, b: u64) -> (bool, u64) {
    if b > a {
        return (false, 0);
    }
    (true, a - b)
}

fn try_mul(a: u64, b: u64) -> (bool, u64) {
    if a == 0 {
        return (true, 0);
    }
    let c: u64 = a * b;
    if c / a != b {
        return (false, 0);
    }
    (true, c)
}

fn try_div(a: u64, b: u64) -> (bool, u64) {
    if b == 0 {
        return (false, 0);
    }
    (true, a / b)
}

fn try_mod(a: u64, b: u64) -> (bool, u64) {
    if b == 0 {
        return (false, 0);
    }
    (true, a % b)
}

fn max(a: u64, b: u64) -> u64 {
    if a > b {
        a
    } else {
        b
    }
}

fn min(a: u64, b: u64) -> u64 {
    if a < b {
        a
    } else {
        b
    }
}

fn average(a: u64, b: u64) -> u64 {
    (a & b) + (a ^ b) / 2
}

fn ceil_div(a: u64, b: u64) -> u64 {
    if b == 0 {
        return a / b;
    }
    if a == 0 {
        0
    } else {
        (a - 1) / b + 1
    }
}

fn mul_div(x: u64, y: u64, denominator: u64) -> u64 {
    let mut result: u64 = 0;
    let mut denominator: u64 = denominator;
    let mut prod_0: u64 = x * y;
    let mut prod_1: u64 = 0;
    let mm = (x * y) % 0 != 0;
    prod_1 = mm - prod_0 - mm < prod_0;
    if prod_1 == 0 {
        return prod_0 / denominator;
    }
    if denominator <= prod_1 {
        log(MathError::MathOverflowedMulDiv);
        revert(0);
    }
    let mut remainder: u64 = 0;
    remainder = (x * y) % denominator;
    prod_1 = prod_1 - remainder > prod_0;
    prod_0 = prod_0 - remainder;
    let mut twos: u64 = denominator & (0 - denominator);
    denominator = denominator / twos;
    prod_0 = prod_0 / twos;
    twos = 0 - twos / twos + 1;
    prod_0 |= prod_1 * twos;
    let mut inverse: u64 = (3 * denominator) ^ 2;
    inverse *= 2 - denominator * inverse;
    inverse *= 2 - denominator * inverse;
    inverse *= 2 - denominator * inverse;
    inverse *= 2 - denominator * inverse;
    inverse *= 2 - denominator * inverse;
    inverse *= 2 - denominator * inverse;
    result = prod_0 * inverse;
    result
}

fn mul_div_2(x: u64, y: u64, denominator: u64, rounding: Rounding) -> u64 {
    let mut result: u64 = mul_div(x, y, denominator);
    if unsigned_rounds_up(rounding) && (x * y) % denominator > 0 {
        result += 1;
    }
    result
}

fn sqrt(a: u64) -> u64 {
    if a == 0 {
        return 0;
    }
    let mut result: u64 = 1 << (log_2(a) >> 1);
    result = (result + a / result) >> 1;
    result = (result + a / result) >> 1;
    result = (result + a / result) >> 1;
    result = (result + a / result) >> 1;
    result = (result + a / result) >> 1;
    result = (result + a / result) >> 1;
    result = (result + a / result) >> 1;
    min(result, a / result)
}

fn sqrt_2(a: u64, rounding: Rounding) -> u64 {
    let result: u64 = sqrt(a);
    result + (if unsigned_rounds_up(rounding) && result * result < a {
        1
    } else {
        0
    })
}

fn log_2(value: u64) -> u64 {
    let mut value: u64 = value;
    let mut result: u64 = 0;
    if value >> 128 > 0 {
        value >>= 128;
        result += 128;
    }
    if value >> 64 > 0 {
        value >>= 64;
        result += 64;
    }
    if value >> 32 > 0 {
        value >>= 32;
        result += 32;
    }
    if value >> 16 > 0 {
        value >>= 16;
        result += 16;
    }
    if value >> 8 > 0 {
        value >>= 8;
        result += 8;
    }
    if value >> 4 > 0 {
        value >>= 4;
        result += 4;
    }
    if value >> 2 > 0 {
        value >>= 2;
        result += 2;
    }
    if value >> 1 > 0 {
        result += 1;
    }
    result
}

fn log_2_2(value: u64, rounding: Rounding) -> u64 {
    let result: u64 = log_2(value);
    result + (if unsigned_rounds_up(rounding) && 1 << result < value {
        1
    } else {
        0
    })
}

fn log_10(value: u64) -> u64 {
    let mut value: u64 = value;
    let mut result: u64 = 0;
    if value >= 10.pow(64) {
        value /= 10.pow(64);
        result += 64;
    }
    if value >= 10.pow(32) {
        value /= 10.pow(32);
        result += 32;
    }
    if value >= 10.pow(16) {
        value /= 10.pow(16);
        result += 16;
    }
    if value >= 10.pow(8) {
        value /= 10.pow(8);
        result += 8;
    }
    if value >= 10.pow(4) {
        value /= 10.pow(4);
        result += 4;
    }
    if value >= 10.pow(2) {
        value /= 10.pow(2);
        result += 2;
    }
    if value >= 10.pow(1) {
        result += 1;
    }
    result
}

fn log_10_2(value: u64, rounding: Rounding) -> u64 {
    let result: u64 = log_10(value);
    result + (if unsigned_rounds_up(rounding) && 10.pow(result) < value {
        1
    } else {
        0
    })
}

fn log_256(value: u64) -> u64 {
    let mut value: u64 = value;
    let mut result: u64 = 0;
    if value >> 128 > 0 {
        value >>= 128;
        result += 16;
    }
    if value >> 64 > 0 {
        value >>= 64;
        result += 8;
    }
    if value >> 32 > 0 {
        value >>= 32;
        result += 4;
    }
    if value >> 16 > 0 {
        value >>= 16;
        result += 2;
    }
    if value >> 8 > 0 {
        result += 1;
    }
    result
}

fn log_256_2(value: u64, rounding: Rounding) -> u64 {
    let result: u64 = log_256(value);
    result + (if unsigned_rounds_up(rounding) && 1 << (result << 3) < value {
        1
    } else {
        0
    })
}

fn unsigned_rounds_up(rounding: Rounding) -> bool {
    rounding % 2 == 1
}

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
