# rlox

Lox interpreter as part of the Crafting Interpreters book whose source code can be found at <https://github.com/munificent/craftinginterpreters>

<https://github.com/rodaine/rlox> was useful as a reference for translating the Java visitor pattern to Rust

Current chapter: 7

the main binary can parse a script or work as a repl

## Future items

- Handle multi-line comments
- Split out LoxError into more errors like ScannerError and ParserError
- Avoid OOP style of mutable state and pass in more params
