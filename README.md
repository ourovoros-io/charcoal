# charcoal
A Solidity to Sway translator written in Rust. This tool aims to translate contracts written in Solidity into Sway source code.

This is primarily an educational tool, and some generated Sway code may be incorrect or unoptimal.

Please feel free to [file an issue](https://github.com/camden-smallwood/charcoal/issues/new/choose).

## Requirements

Compiling the `charcoal` binary requires the Rust compiler and its package manager, cargo.

See the [Rust Install](https://www.rust-lang.org/tools/install) page for installation options.

## Usage

`cargo run --release -- [OPTIONS]`

| Flags | |
|-|-|
| `-h`, `--help` | Prints help information |
| `-V`, `--version` | Prints version information |

| Options | |
|-|-|
| `-d`, `--definition-name <definition-name>` | The name of the specific definition to translate. (Optional; Leave unused for all) |
| `-o`, `--output-directory <output-directory>` | The path to save the translated Forc project to. (Optional; Must be a directory) |
| `-t`, `--target <target>` | The Solidity target file or folder to translate. |

## Implementation Status

* Language Items
    - [x] Import Directives
    - [x] Struct Types
    - [x] Enum Types
    - [x] Interfaces
    - [x] Libraries
    - [x] `using`/`for`
    - [ ] Inline Assembly
* State Variables
    - [x] Constant
    - [x] Immutable
    - [x] Regular
* Functions
    - [x] Parameters
    - [x] Return Variables
    - [x] Returning Multiple Values
    - [x] View Functions
    - [x] Pure Functions
    - [ ] ~~Receive Ether Function~~ (NOTE: Unsupported)
    - [ ] ~~Fallback Function~~ (NOTE: Unsupported)
    - [x] Function Overloading
    - [x] Function Modifiers
    - [ ] ~~Function Signatures~~ (NOTE: Unsupported)
    - [ ] ~~Function Selectors~~ (NOTE: Unsupported)
* Inheritance
    - [x] Inheritance Specification
    - [x] Function Overriding
    - [ ] Modifier Overriding
    - [x] Constructors
* Events
    - [x] Event Type Declarations
    - [ ] ~~Indexed Parameters~~ (NOTE: Unsupported)
    - [ ] ~~`<event>.selector`~~ (NOTE: Unsupported)
* Error Handling
    - [x] Error Type Declarations
    - [ ] ~~`<error>.selector`~~ (NOTE: Unsupported)
    - [x] `assert(bool condition)`
    - [x] `require(bool condition)`
    - [x] `require(bool condition, string memory message)`
    - [ ] `revert()`
    - [ ] `revert(string memory reason)`
* Control Structures
    - [x] `if`/`else`
    - [x] `while`
    - [ ] `do`/`while`
    - [x] `for`
    - [x] `break`
    - [x] `continue`
    - [x] `return`
    - [ ] `try`/`catch`
* Function Calls
    - [x] Internal Function Calls
    - [x] External Function Calls
    - [ ] Function Calls with Named Parameters
    - [ ] Omitted Names in Function Definitions
    - [ ] Creating Contracts via `new`
* Ether Units
    - [ ] `wei`
    - [ ] `gwei`
    - [ ] `ether`
* Time Units
    - [ ] `seconds`
    - [ ] `minutes`
    - [ ] `hours`
    - [ ] `days`
    - [ ] `weeks`
* Block and Transaction Properties
    - [x] `blockhash(uint blockNumber) returns (bytes32)`
    - [ ] `blobhash(uint index) returns (bytes32)`
    - [ ] ~~`block.basefee`~~ (NOTE: Unsupported)
    - [ ] `block.blobbasefee`
    - [x] `block.chainid`
    - [x] `block.coinbase`
    - [ ] ~~`block.difficulty`~~ (NOTE: Unsupported)
    - [ ] `block.gaslimit`
    - [x] `block.number`
    - [ ] ~~`block.prevrandao`~~ (NOTE: Unsupported)
    - [x] `block.timestamp`
    - [ ] `gasleft() returns (uint256)`
    - [x] `msg.data`
    - [x] `msg.sender`
    - [ ] ~~`msg.sig`~~ (NOTE: Unsupported)
    - [x] `msg.value`
    - [x] `tx.gasprice`
    - [ ] ~~`tx.origin`~~ (NOTE: Unsupported)
* ABI Encoding and Decoding Functions
    - [x] `abi.decode(bytes memory encodedData, (...)) returns (...)`
    - [x] `abi.encode(...) returns (bytes memory)`
    - [x] `abi.encodePacked(...) returns (bytes memory)`
    - [ ] `abi.encodeWithSelector(bytes4 selector, ...) returns (bytes memory)`
    - [ ] `abi.encodeWithSignature(string memory signature, ...) returns (bytes memory)`
    - [ ] `abi.encodeCall(function functionPointer, (...)) returns (bytes memory)`
* Members of `bytes`
    - [ ] `bytes.concat(...) returns (bytes memory)`
* Members of `string`
    - [ ] `string.concat(...) returns (string memory)`
* Mathematical and Cryptographic Functions
    - [x] `addmod(uint x, uint y, uint k) returns (uint)`
    - [x] `mulmod(uint x, uint y, uint k) returns (uint)`
    - [x] `keccak256(bytes memory) returns (bytes32)`
    - [x] `sha256(bytes memory) returns (bytes32)`
    - [ ] ~~`ripemd160(bytes memory) returns (bytes20)`~~ (NOTE: Unsupported)
    - [ ] `ecrecover(bytes32 hash, uint8 v, bytes32 r, bytes32 s) returns (address)`
* Members of Address Types
    - [ ] `<address>.balance (uint256)`
    - [ ] `<address>.code (bytes memory)`
    - [ ] `<address>.codehash (bytes32)`
    - [x] `<address payable>.transfer(uint256 amount)`
    - [x] `<address payable>.send(uint256 amount) returns (bool)`
    - [x] `<address>.call(bytes memory) returns (bool, bytes memory)`
    - [ ] ~~`<address>.delegatecall(bytes memory) returns (bool, bytes memory)`~~ (NOTE: Unsupported)
    - [ ] ~~`<address>.staticcall(bytes memory) returns (bool, bytes memory)`~~ (NOTE: Unsupported)
* Contract-related
    - [x] `this`
    - [x] `super`
    - [ ] ~~`selfdestruct(address payable recipient)`~~ (NOTE: Unsupported)
* Type Information
    - [ ] `type(C).name`
    - [ ] `type(C).creationCode`
    - [ ] `type(C).runtimeCode`
    - [ ] `type(I).interfaceId`
    - [x] `type(T).min`
    - [x] `type(T).max`
