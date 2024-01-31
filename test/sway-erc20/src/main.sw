contract;
use core::codec::AbiEncode;
use std::bytes::Bytes;
use std::constants::ZERO_B256;
use std::hash::Hash;
use std::string::*;

pub const GRACE_PERIOD: u256 = 14;
pub const MINIMUM_DELAY: u256 = 2;
pub const MAXIMUM_DELAY: u256 = 30;

enum TimelockEvent {
    NewAdmin: Identity,
    NewPendingAdmin: Identity,
    NewDelay: u256,
    CancelTransaction: (b256, Identity, u256, str, Bytes, u256),
    ExecuteTransaction: (b256, Identity, u256, str, Bytes, u256),
    QueueTransaction: (b256, Identity, u256, str, Bytes, u256),
}

impl AbiEncode for TimelockEvent {
    fn abi_encode(self, ref mut buffer: core::codec::Buffer) {
        match self {
            TimelockEvent::NewAdmin(a) => {
                "NewAdmin".abi_encode(buffer);
                match a {
                    Identity::Address(x) => x.abi_encode(buffer),
                    Identity::ContractId(x) => x.abi_encode(buffer),
                }
            },
            TimelockEvent::NewPendingAdmin(a) => {
                "NewPendingAdmin".abi_encode(buffer);
                match a {
                    Identity::Address(x) => x.abi_encode(buffer),
                    Identity::ContractId(x) => x.abi_encode(buffer),
                }
            },
            TimelockEvent::NewDelay(a) => {
                "NewDelay".abi_encode(buffer);
                a.abi_encode(buffer);
            },
            TimelockEvent::CancelTransaction((a, b, c, d, e, f)) => {
                "CancelTransaction".abi_encode(buffer);
                a.abi_encode(buffer);
                match b {
                    Identity::Address(x) => x.abi_encode(buffer),
                    Identity::ContractId(x) => x.abi_encode(buffer),
                }
                c.abi_encode(buffer);
                d.abi_encode(buffer);
                e.abi_encode(buffer);
                f.abi_encode(buffer);
            },
            TimelockEvent::ExecuteTransaction((a, b, c, d, e, f)) => {
                "ExecuteTransaction".abi_encode(buffer);
                a.abi_encode(buffer);
                match b {
                    Identity::Address(x) => x.abi_encode(buffer),
                    Identity::ContractId(x) => x.abi_encode(buffer),
                }
                c.abi_encode(buffer);
                d.abi_encode(buffer);
                e.abi_encode(buffer);
                f.abi_encode(buffer);
            },
            TimelockEvent::QueueTransaction((a, b, c, d, e, f)) => {
                "QueueTransaction".abi_encode(buffer);
                a.abi_encode(buffer);
                match b {
                    Identity::Address(x) => x.abi_encode(buffer),
                    Identity::ContractId(x) => x.abi_encode(buffer),
                }
                c.abi_encode(buffer);
                d.abi_encode(buffer);
                e.abi_encode(buffer);
                f.abi_encode(buffer);
            },
        }
    }
}

abi Timelock {
    #[storage(read, write)]
    fn constructor(admin_: Identity, delay_: u256);

    fn GRACE_PERIOD() -> u256;

    fn MINIMUM_DELAY() -> u256;

    fn MAXIMUM_DELAY() -> u256;

    #[storage(read)]
    fn admin() -> Identity;

    #[storage(read)]
    fn pending_admin() -> Identity;

    #[storage(read)]
    fn delay() -> u256;

    #[storage(read)]
    fn queued_transactions(a: b256) -> bool;

    #[storage(read, write), payable]
    fn fallback();

    #[storage(read, write)]
    fn set_delay(delay_: u256);

    #[storage(read, write)]
    fn accept_admin();

    #[storage(read, write)]
    fn set_pending_admin(pending_admin_: Identity);

    #[storage(read, write)]
    fn queue_transaction(target: Identity, value: u256, signature: str, data: Bytes, eta: u256) -> b256;

    #[storage(read, write)]
    fn cancel_transaction(target: Identity, value: u256, signature: str, data: Bytes, eta: u256);

    #[storage(read, write), payable]
    fn execute_transaction(target: Identity, value: u256, signature: str, data: Bytes, eta: u256) -> Bytes;
}

storage {
    admin: Identity = Identity::Address(Address::from(ZERO_B256)),
    pending_admin: Identity = Identity::Address(Address::from(ZERO_B256)),
    delay: u256 = 0,
    queued_transactions: StorageMap<b256, bool> = StorageMap {},
    timelock_constructor_called: bool = false,
}

fn safe_math_add(a: u256, b: u256) -> u256 {
    let mut c = 0;
    c = a + b;
    require(c >= a, "SafeMath: addition overflow");
    c
}

fn safe_math_add_2(a: u256, b: u256, error_message: str) -> u256 {
    let mut c = 0;
    c = a + b;
    require(c >= a, error_message);
    c
}

fn safe_math_sub(a: u256, b: u256) -> u256 {
    safe_math_sub_2(a, b, "SafeMath: subtraction underflow")
}

fn safe_math_sub_2(a: u256, b: u256, error_message: str) -> u256 {
    require(b <= a, error_message);
    let c = a - b;
    c
}

fn safe_math_mul(a: u256, b: u256) -> u256 {
    if a == 0 {
        return 0;
    }
    let mut c = 0;
    c = a * b;
    require(c / a == b, "SafeMath: multiplication overflow");
    c
}

fn safe_math_mul_2(a: u256, b: u256, error_message: str) -> u256 {
    if a == 0 {
        return 0;
    }
    let mut c = 0;
    c = a * b;
    require(c / a == b, error_message);
    c
}

fn safe_math_div(a: u256, b: u256) -> u256 {
    safe_math_div_2(a, b, "SafeMath: division by zero")
}

fn safe_math_div_2(a: u256, b: u256, error_message: str) -> u256 {
    require(b > 0, error_message);
    let c = a / b;
    c
}

fn safe_math_mod(a: u256, b: u256) -> u256 {
    safe_math_mod_2(a, b, "SafeMath: modulo by zero")
}

fn safe_math_mod_2(a: u256, b: u256, error_message: str) -> u256 {
    require(b != 0, error_message);
    a % b
}

#[storage(read)]
fn get_block_timestamp() -> u256 {
    std::block::timestamp().as_u256()
}

impl Timelock for Contract {
    #[storage(read, write)]
    fn constructor(admin_: Identity, delay_: u256) {
        require(!storage.timelock_constructor_called.read(), "The Timelock constructor has already been called");
        require(delay_ >= MINIMUM_DELAY, "Timelock::constructor: Delay must exceed minimum delay.");
        require(delay_ <= MAXIMUM_DELAY, "Timelock::setDelay: Delay must not exceed maximum delay.");
        storage.admin.write(admin_);
        storage.delay.write(delay_);
        storage.timelock_constructor_called.write(true);
    }

    fn GRACE_PERIOD() -> u256 {
        GRACE_PERIOD
    }

    fn MINIMUM_DELAY() -> u256 {
        MINIMUM_DELAY
    }

    fn MAXIMUM_DELAY() -> u256 {
        MAXIMUM_DELAY
    }

    #[storage(read)]
    fn admin() -> Identity {
        storage.admin.read()
    }

    #[storage(read)]
    fn pending_admin() -> Identity {
        storage.pending_admin.read()
    }

    #[storage(read)]
    fn delay() -> u256 {
        storage.delay.read()
    }

    #[storage(read)]
    fn queued_transactions(a: b256) -> bool {
        storage.queued_transactions.get(a).read()
    }

    #[storage(read, write), payable]
    fn fallback() {
    }

    #[storage(read, write)]
    fn set_delay(delay_: u256) {
        require(msg_sender().unwrap() == Identity::ContractId(ContractId::this()), "Timelock::setDelay: Call must come from Timelock.");
        require(delay_ >= MINIMUM_DELAY, "Timelock::setDelay: Delay must exceed minimum delay.");
        require(delay_ <= MAXIMUM_DELAY, "Timelock::setDelay: Delay must not exceed maximum delay.");
        storage.delay.write(delay_);
        log(TimelockEvent::NewDelay(storage.delay.read()));
    }

    #[storage(read, write)]
    fn accept_admin() {
        require(msg_sender().unwrap() == storage.pending_admin.read(), "Timelock::acceptAdmin: Call must come from pendingAdmin.");
        storage.admin.write(msg_sender().unwrap());
        storage.pending_admin.write(Identity::Address(Address::from(ZERO_B256)));
        log(TimelockEvent::NewAdmin(storage.admin.read()));
    }

    #[storage(read, write)]
    fn set_pending_admin(pending_admin_: Identity) {
        require(msg_sender().unwrap() == Identity::ContractId(ContractId::this()), "Timelock::setPendingAdmin: Call must come from Timelock.");
        storage.pending_admin.write(pending_admin_);
        log(TimelockEvent::NewPendingAdmin(storage.pending_admin.read()));
    }

    #[storage(read, write)]
    fn queue_transaction(target: Identity, value: u256, signature: str, data: Bytes, eta: u256) -> b256 {
        require(msg_sender().unwrap() == storage.admin.read(), "Timelock::queueTransaction: Call must come from admin.");
        require(eta >= safe_math_add(get_block_timestamp(), storage.delay.read()), "Timelock::queueTransaction: Estimated execution block must satisfy delay.");
        let tx_hash = std::hash::keccak256({
            let mut bytes = Bytes::new();
            bytes.append(Bytes::from(core::codec::encode(target)));
            bytes.append(Bytes::from(core::codec::encode(value)));
            bytes.append(Bytes::from(core::codec::encode(signature)));
            bytes.append(Bytes::from(core::codec::encode(data)));
            bytes.append(Bytes::from(core::codec::encode(eta)));
            bytes
        });
        storage.queued_transactions.get(tx_hash).write(true);
        log(TimelockEvent::QueueTransaction((tx_hash, target, value, signature, data, eta)));
        tx_hash
    }

    #[storage(read, write)]
    fn cancel_transaction(target: Identity, value: u256, signature: str, data: Bytes, eta: u256) {
        require(msg_sender().unwrap() == storage.admin.read(), "Timelock::cancelTransaction: Call must come from admin.");
        let tx_hash = std::hash::keccak256({
            let mut bytes = Bytes::new();
            bytes.append(Bytes::from(core::codec::encode(target)));
            bytes.append(Bytes::from(core::codec::encode(value)));
            bytes.append(Bytes::from(core::codec::encode(signature)));
            bytes.append(Bytes::from(core::codec::encode(data)));
            bytes.append(Bytes::from(core::codec::encode(eta)));
            bytes
        });
        storage.queued_transactions.get(tx_hash).write(false);
        log(TimelockEvent::CancelTransaction((tx_hash, target, value, signature, data, eta)));
    }

    #[storage(read, write), payable]
    fn execute_transaction(target: Identity, value: u256, signature: str, data: Bytes, eta: u256) -> Bytes {
        require(msg_sender().unwrap() == storage.admin.read(), "Timelock::executeTransaction: Call must come from admin.");
        let tx_hash = std::hash::keccak256({
            let mut bytes = Bytes::new();
            bytes.append(Bytes::from(core::codec::encode(target)));
            bytes.append(Bytes::from(core::codec::encode(value)));
            bytes.append(Bytes::from(core::codec::encode(signature)));
            bytes.append(Bytes::from(core::codec::encode(data)));
            bytes.append(Bytes::from(core::codec::encode(eta)));
            bytes
        });
        require(storage.queued_transactions.get(tx_hash).read(), "Timelock::executeTransaction: Transaction hasn't been queued.");
        require(get_block_timestamp() >= eta, "Timelock::executeTransaction: Transaction hasn't surpassed time lock.");
        require(get_block_timestamp() <= safe_math_add(eta, GRACE_PERIOD), "Timelock::executeTransaction: Transaction is stale.");
        storage.queued_transactions.get(tx_hash).write(false);
        let mut call_data = Bytes::new();
        if String::from_ascii_str(signature).as_bytes().len() == 0 {
            call_data = data;
        } else {
            call_data = {
                let mut bytes = Bytes::new();
                bytes.append(Bytes::from(core::codec::encode({
                    let bytes = Bytes::from(std::hash::keccak256(Bytes::from(raw_slice::from_parts::<u8>(signature.as_ptr(), signature.len()))));
                    let (bytes, _) = bytes.split_at(4);
                    bytes
                })));
                bytes.append(Bytes::from(core::codec::encode(data)));
                bytes
            };
        }
        let (success, return_data): (bool, Bytes) = {
            let return_ptr = asm (r1: call_data.buf.ptr, r2: value, r3: std::inputs::input_asset_id(0).unwrap(), r4: std::registers::global_gas()) {
                call r1 r2 r3 r4;
                ret: raw_ptr
            };
            let return_length = asm () {
                retl: u64
            };
            let result_ptr = std::alloc::alloc_bytes(return_length);
            return_ptr.copy_to::<u8>(result_ptr, return_length);
            (true, Bytes::from(raw_slice::from_parts::<u8>(result_ptr, return_length)))
        };
        require(success, "Timelock::executeTransaction: Transaction execution reverted.");
        log(TimelockEvent::ExecuteTransaction((tx_hash, target, value, signature, data, eta)));
        return_data
    }
}
