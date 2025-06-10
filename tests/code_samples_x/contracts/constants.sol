// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract Constants {
    // Define some constants
    uint256 public constant MAX_SUPPLY = 1000000;
    uint256 public constant MIN_BALANCE = 1 ether;
    string public constant TOKEN_NAME = "MyToken";
    string public constant TOKEN_SYMBOL = "MTK";

    // Function to return the constants
    function getConstants() public pure returns (uint256, uint256, string memory, string memory) {
        return (MAX_SUPPLY, MIN_BALANCE, TOKEN_NAME, TOKEN_SYMBOL);
    }
}