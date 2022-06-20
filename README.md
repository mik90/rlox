# rlox

Lox interpreter as part of the Crafting Interpreters book whose source code can be found at <https://github.com/munificent/craftinginterpreters>

<https://github.com/rodaine/rlox> was useful as a reference for translating the Java visitor pattern to Rust

Current chapter: 8

the main binary can parse a script or work as a repl

## Cleanup items

- Split out LoxError into more errors like ScannerError and ParserError
- Avoid OOP style of mutable state and pass in more params
- Visitor pattern should be taking in Expr::Unary directly and not Expr alone somehow. Currently handling this by using EvalError::UnreachableError

## Future features

- Handle multi-line comments
