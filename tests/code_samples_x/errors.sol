// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract Errors {
    // Define custom errors
    error InsufficientBalance(uint256 available, uint256 required);
    error Unauthorized(address caller);

    // State variable to store balances
    mapping(address => uint256) private balances;

    // State variable to store the owner
    address public owner;

    // Constructor to set the owner
    constructor() {
        owner = msg.sender;
    }

    // Function to deposit Ether into the contract
    function deposit() public payable {
        balances[msg.sender] += msg.value;
    }

    // Function to withdraw Ether from the contract
    function withdraw(uint256 amount) public {
        uint256 balance = balances[msg.sender];
        if (balance < amount) {
            revert InsufficientBalance(balance, amount);
        }
        balances[msg.sender] -= amount;
        payable(msg.sender).transfer(amount);
    }

    // Function to transfer ownership
    function transferOwnership(address newOwner) public {
        if (msg.sender != owner) {
            revert Unauthorized(msg.sender);
        }
        owner = newOwner;
    }

    // Function to get the balance of the caller
    function getBalance() public view returns (uint256) {
        return balances[msg.sender];
    }
}