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

struct Parser<'parser_lifetime> {
    current: Token<'parser_lifetime>,
    previous: Token<'parser_lifetime>,
}

impl<'parser_lifetime> Parser<'parser_lifetime> {
    pub fn new() -> Parser<'parser_lifetime> {
        Parser {
            current: Token::new_uninit(),
            previous: Token::new_uninit(),
        }
    }
}

pub struct Compiler<'src_lifetime> {
    // TODO, these may not need to be members since their init logic is so odd
    parser: Parser<'src_lifetime>,
    scanner: Scanner<'src_lifetime>,
}

impl<'src_lifetime> Compiler<'src_lifetime> {
    pub fn new() -> Compiler<'src_lifetime> {
        Compiler {
            parser: Parser::new(),
            scanner: Scanner::new(""),
        }
    }

    fn error_on_current_token(&mut self) {
        todo!()
    }

    fn advance(&mut self) -> Result<(), ScannerError> {
        self.parser.previous = self.parser.current.clone();

        loop {
            self.parser.current = self.scanner.scan_token()?;
            if !matches!(self.parser.current.kind, TokenKind::Error(_)) {
                // Break on non-error types
                break;
            }
            self.error_on_current_token();
        }
        Ok(())
    }

    fn consume(&mut self, kind: TokenKind, error_msg: &str) {
        todo!()
    }

    fn expression(&self) {
        todo!()
    }

    pub fn compile(
        &mut self,
        source: &'src_lifetime str,
        chunk: &mut Chunk,
    ) -> Result<(), CompilerError> {
        self.scanner = Scanner::new(source);
        self.advance().map_err(CompilerError::Scanner)?;
        self.expression();
        self.consume(TokenKind::Eof, "Expect end of expression");

        todo!()
    }
}
