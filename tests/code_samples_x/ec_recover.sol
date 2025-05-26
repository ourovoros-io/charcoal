// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract ECRecover {
    function recoverAddress(bytes32 hash, uint8 v, bytes32 r, bytes32 s) public pure returns (address) {
        return ecrecover(hash, v, r, s);
    }
}