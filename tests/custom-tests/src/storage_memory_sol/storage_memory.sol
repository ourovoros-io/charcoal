// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract StorageToMemory {
    uint256[] public storageArray;

    constructor() {
        // Initialize the storage array with some values
        storageArray.push(1);
        storageArray.push(2);
        storageArray.push(3);
    }

    function getMemoryArray() public view returns (uint256[] memory) {
        // Create a new memory array with the same length as the storage array
        uint256[] memory memoryArray = new uint256[](storageArray.length);

        // Copy elements from the storage array to the memory array
        for (uint256 i = 0; i < storageArray.length; i++) {
            memoryArray[i] = storageArray[i];
        }

        return memoryArray;
    }
}