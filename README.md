# rlox

Lox interpreter as part of the Crafting Interpreters book whose source code can be found at <https://github.com/munificent/craftinginterpreters>

<https://github.com/rodaine/rlox> was useful as a reference for translating the Java visitor pattern to Rust

Current chapter: 8

the main binary can parse a script or work as a repl

## Cleanup items

- LoxError encomposses a few types of errors while EvalError has its own set. So far it makes sense to split these out but im not quite sure that I need all encompassing errors or if I should keep them within their own modules
- ErrorAvoid OOP style of mutable state and pass in more params
- Visitor pattern should be taking in Expr::Unary directly and not Expr alone somehow. Currently handling this by using EvalError::UnreachableError
- LiteralKind has its own copy of the value that Token's lexeme stores although the lexeme is always stringified
  - Maybe there should be an optional LoxValue type for a token instead
  - The TokenKind enum could have an `is_literal` function that tells you whether or not the specific enumeration is of a literal type
- Environment should return by ref instead of copy for `get()`
- Replace `token_matches` with `token_matches_any` and add a new single tokenkind sig for `token_matches`
