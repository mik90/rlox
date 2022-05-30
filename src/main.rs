use std::path::{PathBuf, Path};
use std::io::{BufReader, BufRead};

fn main() {
    let args: Vec<_> = std::env::args().collect();
    if args.len() > 2 {
        eprintln!("Usage: rlox [script]");
        std::process::exit(1);
    } else if args.len() == 2 {
        let p = Path::new(args[1].as_str());
        println!("Running rlox on script at '{}'", p.to_string_lossy());
        if let Err(e) = run_file(&p) {
            eprintln!("Could not run file '{}', hit error: {}", p.to_string_lossy(), e);
            std::process::exit(1);
        }
    } else {
        println!("Starting rlox repl. Enter 'Ctrl+D' to exit");
        if let Err(e) = run_repl() {
            eprintln!("Could not process repl session, {}", e);
            std::process::exit(1);
        }
    }
}

/// Runs some code and executes it
fn run(code: String) {
    print!("(echo) {}", code);
    todo!("Code execution not implemented yet")
}

fn run_repl() -> Result<(), std::io::Error> {
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
            Ok(_) => run(buffer),
            Err(e) => {
                eprintln!("Could not process input, error: {}", e);
                return Err(e);
            }
        }
    }

}

fn run_file(path: &Path) -> Result<(), std::io::Error> {
    let text = std::fs::read_to_string(path)?;
    run(text);
    Ok(())
}
