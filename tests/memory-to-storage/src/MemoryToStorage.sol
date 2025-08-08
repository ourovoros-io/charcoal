// SPDX-License-Identifier: Unlicense
pragma solidity ^0.8.13;

contract MemoryStorageTestContract {
    // ==================== STORAGE VARIABLES ====================
    // Basic types
    uint256 public storageUint;
    int256 public storageInt;
    bool public storageBool;
    address public storageAddress;
    bytes32 public storageBytes32;
    string public storageString;
    // Arrays
    uint256[] public storageArray;
    uint256[5] public storageFixedArray;
    bytes public storageBytes;
    // Mappings
    mapping(uint256 => uint256) public storageMapping;
    mapping(address => string) public addressToString;
    mapping(uint256 => uint256[]) public mappingToArray;
    // Struct definition
    struct Person {
        string name;
        uint256 age;
        address wallet;
        uint256[] scores;
        mapping(string => uint256) attributes;
    }
    struct SimpleStruct {
        uint256 id;
        string name;
        bool active;
    }
    // Storage structs
    Person public storagePerson;
    SimpleStruct[] public storageStructArray;
    mapping(uint256 => Person) public people;
    mapping(uint256 => SimpleStruct) public simpleStructs;
    // Nested mappings
    mapping(address => mapping(uint256 => string)) public nestedMapping;
    mapping(uint256 => mapping(string => bool)) public complexNestedMapping;
    // Events for testing
    event DataCopied(string operation, uint256 value);
    event ArrayOperation(string operation, uint256 length);
    event StructOperation(string operation, uint256 id);
    // ==================== MEMORY TO STORAGE OPERATIONS ====================
    function copyBasicTypesToStorage(
        uint256 _uint,
        int256 _int,
        bool _bool,
        address _address,
        bytes32 _bytes32,
        string memory _string
    ) public {
        // Memory to storage - basic types
        storageUint = _uint;
        storageInt = _int;
        storageBool = _bool;
        storageAddress = _address;
        storageBytes32 = _bytes32;
        storageString = _string;
        emit DataCopied("basicTypes", _uint);
    }
    function copyArrayToStorage(uint256[] memory _array) public {
        // Memory array to storage array - clear and rebuild
        // storageArray = _array; // Direct assignment (simpler approach)
        // Alternative: manual copy to avoid delete statement
        while (storageArray.length > 0) {
            storageArray.pop();
        }
        for (uint256 i = 0; i < _array.length; i++) {
            storageArray.push(_array[i]);
        }
        emit ArrayOperation("memoryToStorage", _array.length);
    }
    function copyFixedArrayToStorage(uint256[5] memory _fixedArray) public {
        // Memory fixed array to storage fixed array
        storageFixedArray = _fixedArray;
        emit ArrayOperation("fixedArrayToStorage", 5);
    }
    function copyBytesToStorage(bytes memory _bytes) public {
        // Memory bytes to storage bytes
        storageBytes = _bytes;
        emit DataCopied("bytesToStorage", _bytes.length);
    }
    function copyStructToStorage(SimpleStruct memory _struct) public {
        // Memory struct to storage
        storageStructArray.push(_struct);
        emit StructOperation("memoryStructToStorage", _struct.id);
    }
    function copyComplexStructToStorage(
        uint256 _id,
        string memory _name,
        uint256 _age,
        address _wallet,
        uint256[] memory _scores
    ) public {
        // Complex struct operations
        Person storage person = people[_id];
        person.name = _name;
        person.age = _age;
        person.wallet = _wallet;
        // Array assignment within struct
        while (person.scores.length > 0) {
            person.scores.pop();
        }
        for (uint256 i = 0; i < _scores.length; i++) {
            person.scores.push(_scores[i]);
        }
        emit StructOperation("complexStructToStorage", _id);
    }
    function updateStorageMapping(uint256 _key, uint256 _value) public {
        // Memory value to storage mapping
        storageMapping[_key] = _value;
        emit DataCopied("mappingUpdate", _value);
    }
    function updateNestedMapping(
        address _addr,
        uint256 _key,
        string memory _value
    ) public {
        // Memory string to nested mapping
        nestedMapping[_addr][_key] = _value;
        emit DataCopied("nestedMappingUpdate", _key);
    }
    function batchUpdateStorage(
        uint256[] memory _keys,
        uint256[] memory _values
    ) public {
        require(_keys.length == _values.length, "Arrays length mismatch");
        // Batch memory to storage operations
        for (uint256 i = 0; i < _keys.length; i++) {
            storageMapping[_keys[i]] = _values[i];
        }
        emit ArrayOperation("batchUpdate", _keys.length);
    }
    // ==================== STORAGE TO MEMORY OPERATIONS ====================
    function getBasicTypesFromStorage()
        public
        view
        returns (uint256, int256, bool, address, bytes32, string memory)
    {
        // Storage to memory - basic types
        return (
            storageUint,
            storageInt,
            storageBool,
            storageAddress,
            storageBytes32,
            storageString
        );
    }
    function getArrayFromStorage() public view returns (uint256[] memory) {
        // Storage array to memory array
        return storageArray;
    }
    function getFixedArrayFromStorage()
        public
        view
        returns (uint256[5] memory)
    {
        // Storage fixed array to memory fixed array
        return storageFixedArray;
    }
    function getBytesFromStorage() public view returns (bytes memory) {
        // Storage bytes to memory bytes
        return storageBytes;
    }
    function getPersonFromStorage(
        uint256 _id
    )
        public
        view
        returns (
            string memory name,
            uint256 age,
            address wallet,
            uint256[] memory scores
        )
    {
        // Complex storage to memory operations
        Person storage person = people[_id];
        return (person.name, person.age, person.wallet, person.scores);
    }
    function getSimpleStructFromStorage(
        uint256 _index
    ) public view returns (SimpleStruct memory) {
        // Storage struct to memory struct
        require(_index < storageStructArray.length, "Index out of bounds");
        return storageStructArray[_index];
    }
    function getMappingValuesFromStorage(
        uint256[] memory _keys
    ) public view returns (uint256[] memory) {
        // Storage mapping to memory array
        uint256[] memory values = new uint256[](_keys.length);
        for (uint256 i = 0; i < _keys.length; i++) {
            values[i] = storageMapping[_keys[i]];
        }
        return values;
    }
    // ==================== MEMORY TO MEMORY OPERATIONS ====================
    function processMemoryArrays(
        uint256[] memory _input
    ) public pure returns (uint256[] memory) {
        // Memory to memory operations
        uint256[] memory result = new uint256[](_input.length);
        for (uint256 i = 0; i < _input.length; i++) {
            result[i] = _input[i] * 2;
        }
        return result;
    }
    function processMemoryStruct(
        SimpleStruct memory _struct
    ) public pure returns (SimpleStruct memory) {
        // Memory struct processing
        SimpleStruct memory result = SimpleStruct({
            id: _struct.id + 1,
            name: string(abi.encodePacked("Processed_", _struct.name)),
            active: !_struct.active
        });
        return result;
    }
    function combineMemoryArrays(
        uint256[] memory _array1,
        uint256[] memory _array2
    ) public pure returns (uint256[] memory) {
        // Memory array combination
        uint256[] memory combined = new uint256[](
            _array1.length + _array2.length
        );
        uint256 index = 0;
        for (uint256 i = 0; i < _array1.length; i++) {
            combined[index] = _array1[i];
            index++;
        }
        for (uint256 i = 0; i < _array2.length; i++) {
            combined[index] = _array2[i];
            index++;
        }
        return combined;
    }
    // ==================== COMPLEX MIXED OPERATIONS ====================
    function complexMemoryStorageOperation(
        uint256 _personId,
        string memory _name,
        uint256[] memory _newScores,
        uint256[] memory _mappingKeys,
        uint256[] memory _mappingValues
    ) public {
        // Mixed memory-storage operations
        // 1. Update person in storage from memory
        Person storage person = people[_personId];
        person.name = _name;
        // 2. Process scores in memory first, then store
        uint256[] memory processedScores = new uint256[](_newScores.length);
        for (uint256 i = 0; i < _newScores.length; i++) {
            processedScores[i] = _newScores[i] + 10; // Add bonus points
        }
        // 3. Copy processed scores to storage
        while (person.scores.length > 0) {
            person.scores.pop();
        }
        for (uint256 i = 0; i < processedScores.length; i++) {
            person.scores.push(processedScores[i]);
        }
        // 4. Update mapping from memory arrays
        require(
            _mappingKeys.length == _mappingValues.length,
            "Array length mismatch"
        );
        for (uint256 i = 0; i < _mappingKeys.length; i++) {
            storageMapping[_mappingKeys[i]] = _mappingValues[i];
        }
        emit StructOperation("complexOperation", _personId);
    }
    function storageToMemoryToStorage(uint256 _key) public {
        // Storage -> Memory -> Storage round trip
        // 1. Load from storage to memory
        uint256 value = storageMapping[_key];
        // 2. Process in memory
        uint256 processedValue = value * 2 + 1;
        // 3. Store back to storage
        storageMapping[_key + 1] = processedValue;
        emit DataCopied("roundTrip", processedValue);
    }
    function copyArrayBetweenMappings(
        uint256 _sourceKey,
        uint256 _destKey
    ) public {
        // Storage array -> Memory -> Storage array (different location)
        // 1. Load array from storage mapping
        uint256[] storage sourceArray = mappingToArray[_sourceKey];
        // 2. Copy to memory
        uint256[] memory tempArray = new uint256[](sourceArray.length);
        for (uint256 i = 0; i < sourceArray.length; i++) {
            tempArray[i] = sourceArray[i];
        }
        // 3. Process in memory
        for (uint256 i = 0; i < tempArray.length; i++) {
            tempArray[i] = tempArray[i] + 5;
        }
        // 4. Store to different location
        while (mappingToArray[_destKey].length > 0) {
            mappingToArray[_destKey].pop();
        }
        for (uint256 i = 0; i < tempArray.length; i++) {
            mappingToArray[_destKey].push(tempArray[i]);
        }
        emit ArrayOperation("arrayTransfer", tempArray.length);
    }
    // ==================== UTILITY FUNCTIONS ====================
    function initializeTestData() public {
        // Initialize storage with test data
        storageUint = 12345;
        storageInt = -6789;
        storageBool = true;
        storageAddress = msg.sender;
        storageBytes32 = keccak256("test");
        storageString = "Initial storage string";
        // Initialize arrays
        storageArray.push(1);
        storageArray.push(2);
        storageArray.push(3);
        storageFixedArray = [10, 20, 30, 40, 50];
        storageBytes = "Initial bytes data";
        // Initialize mappings
        storageMapping[1] = 100;
        storageMapping[2] = 200;
        storageMapping[3] = 300;
        // Initialize struct
        storageStructArray.push(
            SimpleStruct({id: 1, name: "First struct", active: true})
        );
        emit DataCopied("initialization", 1);
    }
    function getStorageArrayLength() public view returns (uint256) {
        return storageArray.length;
    }
    function getStructArrayLength() public view returns (uint256) {
        return storageStructArray.length;
    }
    function getPersonScoresLength(
        uint256 _personId
    ) public view returns (uint256) {
        return people[_personId].scores.length;
    }
}
