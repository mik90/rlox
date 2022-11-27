use crate::{
    chunk::Chunk,
    scanner::{Scanner, ScannerError, Token, TokenKind},
};
use std::fmt;

#[derive(Debug, Clone)]
pub enum CompilerError {
    Scanner(ScannerError),
}
impl std::error::Error for CompilerError {}

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            CompilerError::Scanner(err) => {
                write!(f, "Unable to compile due to error during scan: {}", err)
            }
        }
    }
}

struct Parser<'a> {
    current: Token<'a>,
    previous: Token<'a>,
}

impl<'a> Parser<'a> {
    pub fn new() -> Parser<'a> {
        Parser {
            current: Token::new_uninit(),
            previous: Token::new_uninit(),
        }
    }
}

pub struct Compiler<'a> {
    // TODO, these may not need to be members since their init logic is so odd
    parser: Parser<'a>,
    scanner: Scanner<'a>,
}

impl<'a> Compiler<'a> {
    pub fn new() -> Compiler<'a> {
        Compiler {
            parser: Parser::new(),
            scanner: Scanner::new(""),
        }
    }

    fn error_on_current_token(&mut self) {
        todo!()
    }

    fn advance(&'a mut self) -> Result<(), ScannerError> {
        self.parser.previous = self.parser.current.clone();

        loop {
            {
                self.parser.current = self.scanner.scan_token()?;
                if !matches!(self.parser.current.kind, TokenKind::Error(_)) {
                    // Break on non-error types
                    break;
                }
            }
            self.error_on_current_token();
        }
        todo!()
    }

    fn consume(&mut self, kind: TokenKind, error_msg: &str) {
        todo!()
    }

    fn expression(&mut self) {
        todo!()
    }

    pub fn compile(&mut self, source: &'a str, chunk: &mut Chunk) -> Result<(), CompilerError> {
        self.scanner = Scanner::new(source);
        self.advance();
        self.expression();
        self.consume(TokenKind::Eof, "Expect end of expression");

        todo!()
    }
}
