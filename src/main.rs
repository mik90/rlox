use crate::resolver::Resolver;
use crate::{interpreter::Interpreter, parser::Parser, scanner::Scanner, token::Token};
use std::io::{BufRead, BufReader};
use std::path::Path;

mod ast_printer;
mod environment;
mod error;
mod expr;
mod interpreter;
mod lox_value;
mod parser;
mod resolver;
mod scanner;
mod stmt;
mod token;

fn main() {
    let args: Vec<_> = std::env::args().collect();
    match args.len() {
        1 => {
            println!("Starting rlox repl. Enter 'Ctrl+D' to exit");
            if !run_repl() {
                std::process::exit(1);
            }
        }
        2 => {
            let p = Path::new(args[1].as_str());
            println!("Executing script '{}'", p.to_string_lossy());
            if !run_file(p) {
                std::process::exit(1);
            }
        }
        _ => {
            eprintln!("Usage: rlox [script]");
            std::process::exit(1);
        }
    }
}

/// Scans input for tokens, parses into AST, then interprets AST
fn run(code: String, interpreter: &mut Interpreter) -> bool {
    print!("(echo) {}", code);
    let mut scanner = Scanner::new(code);
    if let Err(e) = scanner.scan_tokens() {
        eprintln!("{}", e);
        return false;
    }
    let tokens = scanner.copy_tokens();

    let statements = match Parser::new(tokens).parse() {
        Ok(statements) => statements,
        Err(errors) => {
            eprintln!("Found at least one error during parsing:");
            for error in errors {
                eprintln!("{}", error);
            }
            return false;
        }
    };
    let mut resolver = Resolver::new(interpreter);
    if let Err(e) = resolver.resolve_stmts(&statements) {
        eprintln!("{}", e);
        return false;
    }
    interpreter.interpret(statements)
}

fn run_repl() -> bool {
    let mut interpreter = Interpreter::new();
    let mut input = BufReader::new(std::io::stdin());
    loop {
        print!("> ");
        let mut buffer = String::new();
        match input.read_line(&mut buffer) {
            Ok(0) => {
                // No bytes left to read, this happens on Ctrl+D
                println!("\nExiting repl");
                return true;
            }
            Ok(_) => {
                // no need to exit even if there's an error in the repl
                run(buffer, &mut interpreter);
            }
            Err(e) => {
                eprintln!("Could not process input, error: {}", e);
                return false;
            }
        }
    }
}

fn run_file(path: &Path) -> bool {
    let mut interpreter = Interpreter::new();
    match std::fs::read_to_string(path) {
        Ok(code) => run(code, &mut interpreter),
        Err(e) => {
            eprintln!(
                "Could not open file '{}', hit error {}",
                path.to_string_lossy(),
                e
            );
            false
        }
    }
}

// These are mostly intergration tests that pull in the scanner, parser, and interpreter
#[cfg(test)]
mod main_test {
    use super::*;
    use crate::lox_value::LoxValue;

    #[test]
    fn eval_simple_expression() {
        let mut interpreter = Interpreter::new();

        let code = "5 + 7;".to_string();
        assert!(run(code, &mut interpreter));
    }

    #[test]
    fn eval_simple_statements() {
        let mut interpreter = Interpreter::new();

        let code = "var foo = 5 + 7;".to_string();
        assert!(run(code, &mut interpreter));

        let code = "print foo;".to_string();
        assert!(run(code, &mut interpreter));
    }

    #[test]
    fn eval_invalid_expression() {
        let mut interpreter = Interpreter::new();

        let code = "var a = 1.0;".to_string();
        assert!(run(code, &mut interpreter));
        let code = "var b = 1.0;".to_string();
        assert!(run(code, &mut interpreter));
        let code = "var c = 1.0;".to_string();
        assert!(run(code, &mut interpreter));

        let code = "a + b = c;".to_string();
        assert_eq!(
            run(code, &mut interpreter),
            false,
            "You should not be able to assign to a binary expression"
        );
    }

    #[test]
    fn eval_blocks() {
        let mut interpreter = Interpreter::new();

        let code = r#"
var a = "global a"; 
var b = "global b"; 
{
  var b = "scope b";
  {
    var b = "shadowing";
    a = b;
  }
}
"#
        .to_string();
        assert!(run(code, &mut interpreter));
        let env = interpreter.get_environment();

        let value = env.get_copy("a");
        assert!(value.is_some());
        let value = value.unwrap();
        assert_eq!(
            value,
            LoxValue::String("shadowing".to_string()),
            "Value was {}",
            value
        );

        let value = env.get_copy("b");
        assert!(value.is_some());
        let value = value.unwrap();
        assert_eq!(
            value,
            LoxValue::String("global b".to_string()),
            "Value was {}",
            value
        );
    }

    #[test]
    fn eval_if() {
        let mut interpreter = Interpreter::new();

        let code = r#"
var a = "foo"; 
if (true)
  a = "bar";
"#
        .to_string();
        assert!(run(code, &mut interpreter));
        let env = interpreter.get_environment();

        let value = env.get_copy("a");
        assert!(value.is_some());
        assert_eq!(value.unwrap(), LoxValue::String("bar".to_string()));
    }

    #[test]
    fn eval_else() {
        let mut interpreter = Interpreter::new();

        let code = r#"
var a = ""; 
if (false)
  a = "foo";
else
  a = "bar";
"#
        .to_string();
        assert!(run(code, &mut interpreter));
        let env = interpreter.get_environment();

        let value = env.get_copy("a");
        assert!(value.is_some());
        assert_eq!(value.unwrap(), LoxValue::String("bar".to_string()));
    }

    #[test]
    fn eval_for() {
        let mut interpreter = Interpreter::new();

        let code = r#"
var a = 0; 
for (var i = 0; i < 2; i = i + 1) {
    a = i;
}
"#
        .to_string();
        assert!(run(code, &mut interpreter));
        let env = interpreter.get_environment();

        let value = env.get_copy("a");
        assert!(value.is_some());
        assert_eq!(value.unwrap(), LoxValue::Number(9.0));
    }

    #[test]
    fn eval_while() {
        let mut interpreter = Interpreter::new();

        let code = r#"
var a = 0;
while (a < 5) {
  a = a + 1;
}
"#
        .to_string();
        assert!(run(code, &mut interpreter));

        let env = interpreter.get_environment();
        let value = env.get_copy("a");
        assert!(value.is_some());
        assert_eq!(value.unwrap(), LoxValue::Number(5.0));
    }

    /// This really just checks that this evaluates without error, it doesn't check stdout. I dont have return values yet
    #[test]
    fn eval_add_func() {
        let mut interpreter = Interpreter::new();

        let code = r#"
fun add(a, b) {
    print a + b;
    return a + b;
}
var c = add(1, 2);
"#
        .to_string();
        assert!(run(code, &mut interpreter));

        let env = interpreter.get_environment();
        let value = env.get_copy("c");
        assert!(value.is_some());
        assert_eq!(value.unwrap(), LoxValue::Number(3.0));
    }

    #[test]
    fn eval_fibonacci() {
        let mut interpreter = Interpreter::new();

        let code = r#"
fun fib(n) {
    if (n <= 1) return n;
    return fib(n - 2) + fib(n - 1);
}
var c = fib(6);
"#
        .to_string();
        assert!(run(code, &mut interpreter));

        let env = interpreter.get_environment();
        let value = env.get_copy("c");
        assert!(value.is_some());
        assert_eq!(value.unwrap(), LoxValue::Number(8.0));
    }

    #[test]
    fn eval_nested_func() {
        let mut interpreter = Interpreter::new();

        let code = r#"
fun makeCounter() {
    var i = 0;
    fun count() {
        i = i + 1;
        return i;
    }
    return count();
}
var output = makeCounter();
"#
        .to_string();
        assert!(run(code, &mut interpreter));

        let env = interpreter.get_environment();
        let value = env.get_copy("output");
        assert!(value.is_some());
        assert_eq!(value.unwrap(), LoxValue::Number(1.0));
    }
}
