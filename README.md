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
| `--contract-files <contract-files>...` | The Solidity source files to translate |
| `--definition-name <definition-name>` | The name of the specific definition to translate. (Optional; Leave unused for all) |
