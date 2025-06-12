// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract ConversionContract {
    struct MyStruct {
        uint value;
        string name;
    }

    MyStruct[] private myStructs;

    // Function to add a new MyStruct to the storage array
    function addStruct(uint _value, string memory _name) public {
        myStructs.push(MyStruct(_value, _name));
    }

    // Function to get a MyStruct from storage and convert it to memory
    function getStruct(uint index) public view returns (uint, string memory) {
        require(index < myStructs.length, "Index out of bounds");
        
        // Copy the storage struct to memory
        MyStruct memory tempStruct = myStructs[index];
        
        return (tempStruct.value, tempStruct.name);
    }
}