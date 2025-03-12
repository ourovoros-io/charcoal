// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract MyContract {
    struct MyStruct {
        mapping(address => uint) balances;
    }

    MyStruct private myStruct;

    // Function to set balance for an address
    function setBalance(address _address, uint _balance) public {
        myStruct.balances[_address] = _balance;
    }

    // Function to get balance for an address
    function getBalance(address _address) public view returns (uint) {
        return myStruct.balances[_address];
    }
}