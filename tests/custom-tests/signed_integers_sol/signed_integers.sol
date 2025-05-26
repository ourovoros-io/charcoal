// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract SignedIntegersExample {
    int8 public signedValue8;
    int16 public signedValue16;
    int32 public signedValue32;
    int64 public signedValue64;
    int128 public signedValue128;
    int256 public signedValue256;

    constructor(
        int8 _initialValue8,
        int16 _initialValue16,
        int32 _initialValue32,
        int64 _initialValue64,
        int128 _initialValue128,
        int256 _initialValue256
    ) {
        signedValue8 = _initialValue8;
        signedValue16 = _initialValue16;
        signedValue32 = _initialValue32;
        signedValue64 = _initialValue64;
        signedValue128 = _initialValue128;
        signedValue256 = _initialValue256;
    }

    // Functions to set the signed values
    function setSignedValue8(int8 _value) public {
        signedValue8 = _value;
    }

    function setSignedValue16(int16 _value) public {
        signedValue16 = _value;
    }

    function setSignedValue32(int32 _value) public {
        signedValue32 = _value;
    }

    function setSignedValue64(int64 _value) public {
        signedValue64 = _value;
    }

    function setSignedValue128(int128 _value) public {
        signedValue128 = _value;
    }

    function setSignedValue256(int256 _value) public {
        signedValue256 = _value;
    }

    // Functions to get the signed values
    function getSignedValue8() public view returns (int8) {
        return signedValue8;
    }

    function getSignedValue16() public view returns (int16) {
        return signedValue16;
    }

    function getSignedValue32() public view returns (int32) {
        return signedValue32;
    }

    function getSignedValue64() public view returns (int64) {
        return signedValue64;
    }

    function getSignedValue128() public view returns (int128) {
        return signedValue128;
    }

    function getSignedValue256() public view returns (int256) {
        return signedValue256;
    }

    // Functions to perform arithmetic operations on signed integers
    function add(int256 a, int256 b) public pure returns (int256) {
        return a + b;
    }

    function subtract(int256 a, int256 b) public pure returns (int256) {
        return a - b;
    }

    function multiply(int256 a, int256 b) public pure returns (int256) {
        return a * b;
    }

    function divide(int256 a, int256 b) public pure returns (int256) {
        require(b != 0, "Division by zero");
        return a / b;
    }
}