use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};

mod error;
mod scanner;
mod token;
mod token_type;
use error::LoxError;

fn main() {
    let args: Vec<_> = std::env::args().collect();
    match args.len() {
        0..=1 => {
            println!("Starting rlox repl. Enter 'Ctrl+D' to exit");
            if let Err(e) = run_repl() {
                eprintln!("Could not process repl session, {}", e);
                std::process::exit(1);
            }
        }
        2 => {
            let p = Path::new(args[1].as_str());
            println!("Running rlox on script at '{}'", p.to_string_lossy());
            if let Err(e) = run_file(&p) {
                eprintln!(
                    "Could not run file '{}', hit error: {}",
                    p.to_string_lossy(),
                    e
                );
                std::process::exit(1);
            }
        }
        _ => {
            eprintln!("Usage: rlox [script]");
            std::process::exit(1);
        }
    }
}

/// Runs some code and executes it
fn run(code: String) -> Result<(), LoxError> {
    print!("(echo) {}", code);
    todo!("Code execution not implemented yet")
}

fn run_repl() -> Result<(), LoxError> {
    let mut input = BufReader::new(std::io::stdin());
    loop {
        print!("> ");
        let mut buffer = String::new();
        match input.read_line(&mut buffer) {
            Ok(0) => {
                // No bytes left to read, this happens on Ctrl+D
                println!("\nExiting repl");
                return Ok(());
            }
            Ok(_) => run(buffer)?,
            Err(e) => {
                eprintln!("Could not process input, error: {}", e);
                return Err(e.into());
            }
        }
    }
}

fn run_file(path: &Path) -> Result<(), LoxError> {
    let text = std::fs::read_to_string(path)?;
    run(text)
}
