// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract StructMappings {
    struct MyStruct {
        mapping(address => uint) balances;
        mapping(address => uint64) amounts;
    }

    MyStruct private myStruct;

    event Z();

    constructor() {
        emit Z();
    }

    // Function to set balance for an address
    function setBalance(address _address, uint _balance) public {
        myStruct.balances[_address] = _balance;
    }

    // Function to get balance for an address
    function getBalance(address _address) public view returns (uint) {
        return myStruct.balances[_address];
    }
}


/*
contract;

use std::hash::Hash;

struct MyStruct {
    balances: Option<StorageKey<StorageMap<Identity, u256>>>,
}

abi MyContract {
    #[storage(read,write)]
    fn constructor();
    #[storage(read, write)]
    fn set_balance(_address: Identity, _balance: u256);
    #[storage(read)]
    fn get_balance(_address: Identity) -> u256;
}

storage {
    my_struct: MyStruct = MyStruct {
        balances: None,
    },
    my_struct_count: u64 = 0,
    my_struct_balances: StorageMap<u64, StorageMap<Identity, u256>> = StorageMap {}
}

impl MyContract for Contract {
    #[storage(read,write)]
    fn constructor() {
        require(!storage.my_contract_constructor_called.read(), "The MyContract constructor has already been called");

        let i = storage.my_struct_count.read();
        storage.my_struct_count.write(i + 1);
        let balances = storage.my_struct_balances.get(i);
        storage.my_struct.balances.write(Some(balances));
        
        log(MyContractEvent::Z);
        storage.my_contract_constructor_called.write(true);
    }

    #[storage(read, write)]
    fn set_balance(_address: Identity, _balance: u256) {
        storage.my_struct.read().balances.unwrap().get(_address).write(_balance);
    }

    #[storage(read)]
    fn get_balance(_address: Identity) -> u256 {
        storage.my_struct.read().balances.unwrap().get(_address).read()
    }
}
*/