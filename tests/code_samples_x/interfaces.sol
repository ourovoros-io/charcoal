// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

interface IToken {
    function totalSupply() external view returns (uint256);
    function balanceOf(address account) external view returns (uint256);
    function transfer(address recipient, uint256 amount) external returns (bool);
}

contract TokenUser {
    IToken public token;

    constructor(address tokenAddress) {
        token = IToken(tokenAddress);
    }

    function getTotalSupply() public view returns (uint256) {
        return token.totalSupply();
    }

    function getBalance(address account) public view returns (uint256) {
        return token.balanceOf(account);
    }

    function transferTokens(address recipient, uint256 amount) public returns (bool) {
        return token.transfer(recipient, amount);
    }
}