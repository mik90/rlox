use crate::{interpreter::Interpreter, parser::Parser, scanner::Scanner, token::Token};
use std::io::{BufRead, BufReader};
use std::path::Path;

mod ast_printer;
mod error;
mod expr;
mod interpreter;
mod parser;
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
fn run(code: String) -> bool {
    print!("(echo) {}", code);
    let mut scanner = Scanner::new(code);
    if let Err(e) = scanner.scan_tokens() {
        eprintln!("{}", e);
        return false;
    }
    let tokens = scanner.copy_tokens();

    let statements = match Parser::new(tokens).parse() {
        Ok(statements) => statements,
        Err(e) => {
            eprintln!("{}", e);
            return false;
        }
    };
    Interpreter::interpret(statements)
}

fn run_repl() -> bool {
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
                run(buffer);
            }
            Err(e) => {
                eprintln!("Could not process input, error: {}", e);
                return false;
            }
        }
    }
}

fn run_file(path: &Path) -> bool {
    match std::fs::read_to_string(path) {
        Ok(v) => run(v),
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

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn eval_simple_expression() {
        assert!(run("5 + 7;".to_string()));
    }
}
