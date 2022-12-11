mod chunk;
mod compiler;
mod macros;
mod scanner;
mod value;
mod vm;

use vm::Vm;

use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use crate::vm::VmState;

fn run_file(path: &Path) -> ExitCode {
    let vm = Vm::new();
    // Since interpret is only run once, no state will persist
    let state = VmState::new();

    match std::fs::read_to_string(path) {
        Ok(code) => match vm.interpret(&code, state) {
            Ok(_) => ExitCode::SUCCESS,
            Err(e) => {
                eprintln!("{}", e);
                ExitCode::FAILURE
            }
        },
        Err(e) => {
            eprintln!(
                "Could not open file '{}', hit error {}",
                path.to_string_lossy(),
                e
            );
            ExitCode::FAILURE
        }
    }
}

fn repl() -> ExitCode {
    print!("> ");
    let mut input = BufReader::new(std::io::stdin());

    let vm = Vm::new();
    let mut state = VmState::new();

    loop {
        let mut buffer = String::new();
        match input.read_line(&mut buffer) {
            Ok(0) => {
                // No bytes left to read, this happens on Ctrl+D
                println!("\nExiting repl");
                return ExitCode::SUCCESS;
            }
            Ok(_) => {
                buffer.push('\0'); // Add terminator
                match vm.interpret(&buffer, state) {
                    Ok(new_state) => {
                        state = new_state;
                    }
                    Err(e) => {
                        eprintln!("{}", e);
                        return ExitCode::FAILURE;
                    }
                }
            }
            Err(e) => {
                eprintln!("Could not process input, error: {}", e);
                return ExitCode::FAILURE;
            }
        }
    }
}

fn main() -> ExitCode {
    let mut args = std::env::args();
    match args.len() {
        1 => repl(),
        2 => {
            let path = PathBuf::from(args.nth(1).expect("Cannot acces args[1]"));
            run_file(&path);
            ExitCode::SUCCESS
        }
        _ => {
            eprintln!("Usage: rlox [path]");
            ExitCode::FAILURE
        }
    }
}
