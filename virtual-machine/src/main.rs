mod chunk;
mod compiler;
mod macros;
mod scanner;
mod value;
mod vm;

use chunk::{Chunk, OpCode};
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use vm::Vm;

fn run_file(path: &Path) -> ExitCode {
    //let mut vm = Vm::new(chunk_iter, instruction_iter);
    match std::fs::read_to_string(path) {
        //vm.interpret(buffer);
        Ok(code) => {
            //vm.interpret(buffer)
            todo!("interpret({})", code)
        }
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
    let mut input = BufReader::new(std::io::stdin());
    //let mut vm = Vm::new(chunk_iter, instruction_iter);
    loop {
        print!("> ");
        let mut buffer = String::new();
        match input.read_line(&mut buffer) {
            Ok(0) => {
                // No bytes left to read, this happens on Ctrl+D
                println!("\nExiting repl");
                return ExitCode::SUCCESS;
            }
            Ok(_) => {
                //vm.interpret(buffer);
                todo!("pass in source code to interpret({})", buffer)
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
