// SPDX-License-Identifier: Unlicense
pragma solidity ^0.8.13;

contract SimpleMemoryStorageContract {
    // Basic storage variables
    uint256 public storageUint;
    string public storageString;
    uint256[] public storageArray;
    int public x;
    // Basic struct
    struct SimpleData {
        uint256 id;
    }
    SimpleData public storageStruct;
    mapping(uint256 => uint256) public storageMapping;
    // Events
    event DataUpdated(uint256 value);
    // Memory to storage - basic types
    function setBasicData(uint256 _uint, string memory _string) public {
        storageUint = _uint;
        storageString = _string;
        emit DataUpdated(_uint);
    }
    // Memory to storage - array
    function setArrayData(uint256[] memory _array) public {
        // Clear existing array using pop instead of delete
        while (storageArray.length > 0) {
            storageArray.pop();
        }
        // Copy from memory to storage
        for (uint256 i = 0; i < _array.length; i++) {
            storageArray.push(_array[i]);
        }
    }
    // Memory to storage - struct
    function setStructData(SimpleData memory _data) public {
        storageStruct = _data;
    }
    // Memory to storage - mapping
    function setMappingData(uint256 _key, uint256 _value) public {
        storageMapping[_key] = _value;
    }
    // Storage to memory - basic types
    function getBasicData() public view returns (uint256, string memory) {
        return (storageUint, storageString);
    }
    // Storage to memory - array
    function getArrayData() public view returns (uint256[] memory) {
        return storageArray;
    }
    // Storage to memory - struct
    function getStructData() public view returns (SimpleData memory) {
        return storageStruct;
    }
    // Memory operations
    function processArrayInMemory(
        uint256[] memory _input
    ) public pure returns (uint256[] memory) {
        uint256[] memory result = new uint256[](_input.length);
        for (uint256 i = 0; i < _input.length; i++) {
            result[i] = _input[i] * 2;
        }
        return result;
    }
    // Mixed operations
    function complexOperation(
        uint256[] memory _input,
        uint256 _multiplier
    ) public {
        // Process in memory
        uint256[] memory processed = new uint256[](_input.length);
        for (uint256 i = 0; i < _input.length; i++) {
            processed[i] = _input[i] * _multiplier;
        }
        // Store to storage
        while (storageArray.length > 0) {
            storageArray.pop();
        }
        for (uint256 i = 0; i < processed.length; i++) {
            storageArray.push(processed[i]);
        }
        emit DataUpdated(processed.length);
    }
    // Utility function
    function initializeData() public {
        storageUint = 100;
        storageString = "Initial data";
        storageArray.push(1);
        storageArray.push(2);
        storageArray.push(3);
    }
}
