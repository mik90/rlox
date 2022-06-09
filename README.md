# rlox

Lox interpreter as part of the Crafting Interpreters book whose source code can be found at <https://github.com/munificent/craftinginterpreters>

Current chapter: 5

the main binary can parse a script or work as a repl

The extra `write-ast` binary creates an [src/expr.rs](src/expr.rs) file that translates a grammar definition to rust code

Can run it with

```bash
cargo r --bin write-ast src/
```
