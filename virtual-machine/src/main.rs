mod chunk;
mod compiler;
mod macros;
mod scanner;
mod value;
mod vm;

use chunk::Chunk;
use vm::Vm;

use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::ExitCode;

fn run_file(path: &Path) -> ExitCode {
    let vm = Vm::new();
    // Since interpret is only run once, no state will persist
    let state = None;
    let mut chunks: Vec<Chunk> = vec![];
    match std::fs::read_to_string(path) {
        Ok(code) => match vm.interpret(&code, &mut chunks, state) {
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
    let mut state = None;
    let mut chunks: Vec<Chunk> = vec![Chunk::new()];
    let mut cmd_history = Vec::<String>::new();

    loop {
        let mut buffer = String::new();
        match input.read_line(&mut buffer) {
            Ok(0) => {
                // No bytes left to read, this happens on Ctrl+D
                println!("\nExiting repl");
                return ExitCode::SUCCESS;
            }
            Ok(_) => {
                cmd_history.push(buffer.to_owned());
                let command = cmd_history.last().unwrap();
                match vm.interpret(command, &mut chunks, state) {
                    Ok(new_state) => {
                        state = Some(new_state);
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
