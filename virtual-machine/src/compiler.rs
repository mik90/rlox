use crate::scanner::{Scanner, TokenKind};

pub fn compile(source: &str) {
    let mut scanner = Scanner::new(source);
    let mut line = 0;
    loop {
        match scanner.scan_token() {
            Ok(token) => {
                if token.line != line || line == 0 {
                    print!("line:{:>4} ", token.line);
                    line = token.line;
                } else {
                    print!("        | ");
                }
                let text: String = token
                    .start
                    .clone()
                    .into_iter()
                    .take(token.length)
                    .map(|(_, c)| c)
                    .collect();
                println!("{:>8?} {}", token.kind, text);

                if let TokenKind::Eof = token.kind {
                    break;
                }
            }
            Err(e) => eprintln!("Failed to scan token during compilation: {}", e),
        }
    }
}
