# rlox

## Tree walk interpreter

Lox tree-walk interpreter as part of the Crafting Interpreters book whose source code can be found at <https://github.com/munificent/craftinginterpreters>

<https://github.com/rodaine/rlox> was useful as a reference for translating the Java visitor pattern to Rust

the main binary can parse a script or work as a repl

On Chapter 11.5 hit some bug where for loops results in undefined variables in the increment. I figured it'd be best to just move on to the bytecode compiler since that was somewhat close to the end of the section

### Cleanup items

- LoxError encomposses a few types of errors while EvalError has its own set. So far it makes sense to split these out but im not quite sure that I need all encompassing errors or if I should keep them within their own modules
- Avoid OOP style of mutable state in the parser
- Visitor pattern should be taking in Expr::Unary directly and not Expr alone somehow. Currently handling this by using EvalError::UnreachableError
- LiteralKind has its own copy of the value that Token's lexeme stores although the lexeme is always stringified
  - Maybe there should be an optional LoxValue type for a token instead
  - The TokenKind enum could have an `is_literal` function that tells you whether or not the specific enumeration is of a literal type
- Environment should return by ref instead of copy for `get()`
- Replace `token_matches` with `token_matches_any` and add a new single tokenkind sig for `token_matches`
- Use something other than the `return_value` member of `Interpreter` to handle return values
- EnvironmentStack copies do not share any common values so closures sharing globally mutated values are not going to work
- Interpreter's locals should not hold Expr directly but a wrapper around Expr that is hashable/eq disregarding line number while the normal token hashable/eq takes into account line number

### Features I want to add

- The REPL should print out the result of expressions without requiring another `print` statement
  - So it'll just execute statements but for expressions it'll evaluate it and then print out the value
- Support for `break` and `continue` statements. They may be nested inside other blocks.
- Anonymous functions

## Virtual machine

Fairly direct translation of `clox` to rust

### Things I skipped

- Ch 20, pretty much entirely. I'm using the Rust HashMap isntead of rolling my own as the book does. I'm skipping string interning as well.
