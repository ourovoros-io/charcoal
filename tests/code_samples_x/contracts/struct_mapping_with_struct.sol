// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract StructMappingsWithStructs {
    struct MyStruct {
        mapping(address => uint) balances;
        mapping(address => uint64) amounts;
        mapping(address => I) ies;
    }

    struct I {
        uint256 a;
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

    // Function to get balance for an address
    function getIes(address _address) public view returns (uint256) {
        return myStruct.ies[_address].a;
    }
}