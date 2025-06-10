// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract SimpleOracle {
    address public owner;
    mapping(string => uint256) public data;     // Store multiple values
    mapping(string => uint256) public timestamps;
    
    event DataUpdated(string key, uint256 value, uint256 timestamp);
    
    modifier onlyOwner() {
        require(msg.sender == owner, "Only owner can call this function");
        _;
    }
    
    constructor() {
        owner = msg.sender;
    }
    
    // Update data with a specific key
    function updateData(string memory _key, uint256 _value) external onlyOwner {
        data[_key] = _value;
        timestamps[_key] = block.timestamp;
        emit DataUpdated(_key, _value, block.timestamp);
    }
    
    // Get data for a specific key
    function getData(string memory _key) external view returns (uint256 value, uint256 timestamp) {
        return (data[_key], timestamps[_key]);
    }
    
    // Batch update multiple values
    function updateBatch(string[] memory _keys, uint256[] memory _values) external onlyOwner {
        require(_keys.length == _values.length, "Arrays must have same length");
        
        for (uint256 i = 0; i < _keys.length; i++) {
            data[_keys[i]] = _values[i];
            timestamps[_keys[i]] = block.timestamp;
            emit DataUpdated(_keys[i], _values[i], block.timestamp);
        }
    }
    
    function transferOwnership(address _newOwner) external onlyOwner {
        require(_newOwner != address(0), "New owner cannot be zero address");
        owner = _newOwner;
    }
}