// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract FallbackExample {
    event FallbackCalled(address sender, uint256 value, bytes data);

    // Fallback function
    fallback() external payable {
        emit FallbackCalled(msg.sender, msg.value, msg.data);
    }

    // Function to receive Ether
    receive() external payable {
        emit FallbackCalled(msg.sender, msg.value, "");
    }

    // Function to get the contract balance
    function getBalance() public view returns (uint256) {
        return address(this).balance;
    }
}