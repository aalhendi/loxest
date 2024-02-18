# Loxest

## What is Loxest

A dynamically typed, interpreted language compiled and executed on its own virtual machine.

It aims to compatible with Bob Nystrom's Lox implementation in C.

## Installation

Stable binaries are available on the [releases] page.

## Usage

### Running the REPL

```bash
./loxest
```

### Running a file

```bash
./loxest <filename>.lox
```

## Features

Loxest comes with a disassembler and a trace execution printing. To enable these features, follow the development guide and enable the features within the ``Cargo.toml`` file or on the command line for a single run.

### Cargo.toml

Uncomment this line:

```toml
# default = ["debug-trace-execution", "debug-print-code"]
```

### CLI

To run a single feature:

```bash
cargo run --release --features "debug-print-code" # or whatever feature
```

To run all features:

```bash
cargo run --release --all-features
```

## Development

To download and develop on Loxest, clone the repository and build using `Cargo`. Cargo, as well as Rust can be installed via [rustup].

Once downloaded, navigate to the project root directory and run:

```bash
cargo build --release
```

## Documentation

TODO. Someday. Maybe. Or read the code.

## Notes

The benchmark examples are copied from the [craftinginterpreters] repository and are under the MIT License.

## License

MIT License - Copyright (c) 2024 Abdulrazzaq Alhendi.

[releases]: https://github.com/aalhendi/loxest/releases
[rustup]: https://rustup.rs/
[craftinginterpreters]: https://github.com/munificent/craftinginterpreters
