use crate::{
    chunk::{Chunk, OpCode},
    scanner::{Scanner, ScannerError, Token, TokenKind},
};
use std::{clone, collections::HashMap, fmt, hash::Hash};

#[derive(Debug, Clone)]
pub enum CompilerError {
    Scanner(ScannerError),
    Parse(Vec<String>),  // multiple errors can be stored internally
    Bytecode(String),    // error when building bytecode
    Unreachable(String), // error when the parse tree reaches something previously thought impossible
}
impl std::error::Error for CompilerError {}

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            CompilerError::Scanner(err) => {
                write!(f, "Unable to compile due to error during scan: {}", err)
            }
            CompilerError::Parse(errors) => {
                write!(
                    f,
                    "Unable to compile due parse errors: {}",
                    errors.join("\n")
                )
            }
            CompilerError::Bytecode(error) => {
                write!(f, "Hit error while emitting bytecode: {}", error)
            }
            CompilerError::Unreachable(error) => {
                write!(f, "Hit unreachable path during compilation: {}", error)
            }
        }
    }
}

struct Parser<'source_lifetime> {
    current: Token<'source_lifetime>,
    previous: Token<'source_lifetime>,
    panic_mode: bool,
    errors: Vec<String>,
}

impl<'source_lifetime> Parser<'source_lifetime> {
    pub fn new() -> Parser<'source_lifetime> {
        Parser {
            current: Token::new_uninit(),
            previous: Token::new_uninit(),
            panic_mode: false,
            errors: vec![],
        }
    }
}

/// Pecedence from lowest to highest
enum Precedence {
    None = 0,
    Assignment, //< =
    Or,         //< or
    And,        //< and
    Equality,   //< == !=
    Comparison, //< < > <= >=
    Term,       //< + -
    Factor,     //< * /
    Unary,      //< ! -
    Call,       //< . ()
    Primary,
}

type ParseFn = dyn Fn(&mut Compiler, Chunk) -> Result<Chunk, CompilerError>;

struct ParserRule {
    prefix: Option<Box<ParseFn>>,
    infix: Option<Box<ParseFn>>,
    precedence: Precedence,
}

struct ParserRuleResult {
    /// Always returns the parsed chunk
    chunk: Chunk,
    error: Option<CompilerError>,
    precedence: Precedence,
}

impl ParserRule {
    fn new(
        prefix: Option<Box<ParseFn>>,
        infix: Option<Box<ParseFn>>,
        precedence: Precedence,
    ) -> ParserRule {
        ParserRule {
            prefix,
            infix,
            precedence,
        }
    }
    fn get_next_precedence(&self) -> Precedence {
        todo!()
    }
}

pub struct Compiler<'source_lifetime> {
    // TODO, these may not need to be members since their init logic is so odd
    parser: Parser<'source_lifetime>,
    rules: HashMap<std::mem::Discriminant<TokenKind>, ParserRule>,
    scanner: Scanner<'source_lifetime>,
}

enum ErrorAtKind {
    Previous,
    Current,
}

impl<'source_lifetime> Compiler<'source_lifetime> {
    pub fn new() -> Compiler<'source_lifetime> {
        let mut compiler = Compiler {
            parser: Parser::new(),
            rules: HashMap::new(),
            scanner: Scanner::new(""),
        };
        compiler.set_rules();
        compiler
    }

    /// This is a hacky workaround to the parse table in clox. A better approach would be to have a big match expression
    /// that matches a TokenKind to a ParserRule. It would either return a reference to an already-constructed ParserRule or, if given the appropriate args,
    /// it would return the request information from the ParserRule such as the precedence or the result of the applied infix or prefix operation
    /// TODO use a better approach
    fn set_rules(&mut self) {
        self.rules = HashMap::from([
            (
                std::mem::discriminant(&TokenKind::LeftParen),
                ParserRule::new(
                    Some(Box::new(|compiler: &mut Compiler, chunk: Chunk| {
                        compiler.grouping(chunk)
                    })),
                    None,
                    Precedence::None,
                ),
            ),
            (
                std::mem::discriminant(&TokenKind::LeftBrace),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::RightBrace),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Comma),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Dot),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Minus),
                ParserRule::new(
                    Some(Box::new(|compiler: &mut Compiler, chunk: Chunk| {
                        compiler.unary(chunk)
                    })),
                    Some(Box::new(|compiler: &mut Compiler, chunk: Chunk| {
                        compiler.binary(chunk)
                    })),
                    Precedence::Term,
                ),
            ),
            (
                std::mem::discriminant(&TokenKind::Plus),
                ParserRule::new(
                    Some(Box::new(|compiler: &mut Compiler, chunk: Chunk| {
                        compiler.unary(chunk)
                    })),
                    Some(Box::new(|compiler: &mut Compiler, chunk: Chunk| {
                        compiler.binary(chunk)
                    })),
                    Precedence::Term,
                ),
            ),
            (
                std::mem::discriminant(&TokenKind::SemiColon),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Slash),
                ParserRule::new(
                    None,
                    Some(Box::new(|compiler: &mut Compiler, chunk: Chunk| {
                        compiler.binary(chunk)
                    })),
                    Precedence::Factor,
                ),
            ),
            (
                std::mem::discriminant(&TokenKind::Star),
                ParserRule::new(
                    None,
                    Some(Box::new(|compiler: &mut Compiler, chunk: Chunk| {
                        compiler.binary(chunk)
                    })),
                    Precedence::Factor,
                ),
            ),
            (
                std::mem::discriminant(&TokenKind::Bang),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::BangEqual),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Equal),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::EqualEqual),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Greater),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::GreaterEqual),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Less),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::LessEqual),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Identifier),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::String),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Number),
                ParserRule::new(
                    Some(Box::new(|compiler: &mut Compiler, chunk: Chunk| {
                        compiler.number(chunk)
                    })),
                    None,
                    Precedence::None,
                ),
            ),
            (
                std::mem::discriminant(&TokenKind::And),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Class),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Else),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::False),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::For),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Fun),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::If),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Nil),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Or),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Print),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Return),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Super),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::This),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::True),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Var),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::While),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                // Eugh, hacky. gets the discriminant on a random instantiated enum variant
                std::mem::discriminant(&TokenKind::Error(String::new())),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Eof),
                ParserRule::new(None, None, Precedence::None),
            ),
        ]);
    }

    /// TODO this function call needs to be cleaned up
    fn error_at(&mut self, error_at_kind: ErrorAtKind, msg: Option<&'static str>) {
        if self.parser.panic_mode {
            return;
        }
        self.parser.panic_mode = true;

        let token = match error_at_kind {
            ErrorAtKind::Previous => &self.parser.previous,
            ErrorAtKind::Current => &self.parser.current,
        };
        let mut current_error: Vec<String> = vec![];
        current_error.push(format!("[line {}] Error", token.line));

        match &token.kind {
            TokenKind::Eof => {
                current_error.push(format!(" at end: {}", msg.unwrap_or_default()));
            }
            TokenKind::Error(error_msg) => {
                // The error message itself isn't stored in the source, it's stored as part of the enum
                current_error.push(format!(": {}", error_msg));
            }
            _ => {
                current_error.push(format!(
                    "at '{}': {}",
                    token.to_string(),
                    msg.unwrap_or_default()
                ));
            }
        }
        self.parser.errors.push(current_error.join(""));
    }

    fn advance(&mut self) -> Result<(), CompilerError> {
        self.parser.previous = self.parser.current.clone();

        loop {
            self.parser.current = self.scanner.scan_token().map_err(CompilerError::Scanner)?;
            if !matches!(self.parser.current.kind, TokenKind::Error(_)) {
                // Break on non-error types
                break;
            }
            self.error_at(ErrorAtKind::Current, None);
        }
        Ok(())
    }

    fn consume(
        &mut self,
        expected_kind: TokenKind,
        error_msg: &'static str,
    ) -> Result<(), CompilerError> {
        if matches!(&self.parser.current.kind, kind) {
            return self.advance();
        }

        self.error_at(ErrorAtKind::Current, Some(error_msg));
        Ok(())
    }

    /// The book stores the current chunk as a static variable, ill just pass it in
    /// This may not be sufficient for later
    fn emit_byte(&self, byte: u8, mut current_chunk: Chunk) -> Chunk {
        current_chunk.write_byte(byte, self.parser.previous.line);
        current_chunk
    }

    fn emit_opcode(&self, opcode: OpCode, mut current_chunk: Chunk) -> Chunk {
        current_chunk.write_opcode(opcode, self.parser.previous.line);
        current_chunk
    }

    fn emit_opcode_and_byte(&self, opcode: OpCode, byte: u8, mut current_chunk: Chunk) -> Chunk {
        current_chunk.write_opcode(opcode, self.parser.previous.line);
        current_chunk.write_byte(byte, self.parser.previous.line);
        current_chunk
    }

    fn emit_constant(&self, value: f64, mut current_chunk: Chunk) -> Result<Chunk, CompilerError> {
        current_chunk
            .write_constant(value, self.parser.previous.line)
            .map_err(|e| {
                CompilerError::Bytecode(format!("On line {}, {}", self.parser.previous.line, e))
            })?;
        Ok(current_chunk)
    }

    fn call_prefix(
        &mut self,
        operator_kind: &TokenKind,
        chunk: Chunk,
    ) -> Result<Chunk, CompilerError> {
        self.rules
            .get(&std::mem::discriminant(operator_kind))
            .ok_or_else(|| {
                CompilerError::Unreachable(format!(
                    "Could not find ParserRule for operator {:?} on line {}",
                    operator_kind, self.parser.previous.line
                ))
            })?
            .prefix
            .as_ref()
            .map(|f| f(self, chunk))
            .ok_or_else(|| {
                self.error_at(ErrorAtKind::Previous, Some("Expect expression"));
                CompilerError::Unreachable(format!("No prefix handler provided"))
            })?
    }

    fn get_rule(&self, operator_kind: &TokenKind) -> Result<&ParserRule, CompilerError> {
        self.rules
            .get(&std::mem::discriminant(operator_kind))
            .ok_or_else(|| {
                CompilerError::Unreachable(format!(
                    "Could not find ParserRule for operator {:?} on line {}",
                    operator_kind, self.parser.previous.line
                ))
            })
    }

    /// Temporary measure as per the book to print hte value of our single expression
    fn end_compiler(&mut self, mut current_chunk: Chunk) -> Chunk {
        current_chunk.write_opcode(OpCode::Return, self.parser.previous.line);
        current_chunk
    }

    fn binary(&mut self, mut chunk: Chunk) -> Result<Chunk, CompilerError> {
        let operator_kind = self.parser.previous.kind.clone();
        let rule = self.get_rule(&operator_kind)?;
        let precedence = rule.get_next_precedence();
        chunk = self.parse_precedence(precedence, chunk)?;

        match operator_kind {
            TokenKind::Plus => Ok(self.emit_opcode(OpCode::Add, chunk)),
            TokenKind::Minus => Ok(self.emit_opcode(OpCode::Subtract, chunk)),
            TokenKind::Star => Ok(self.emit_opcode(OpCode::Multiply, chunk)),
            TokenKind::Slash => Ok(self.emit_opcode(OpCode::Divide, chunk)),
            _ => Err(CompilerError::Unreachable(format!(
                "Did not expect operator {:?} in binary expression on line {}",
                operator_kind, self.parser.previous.line
            ))),
        }
    }

    // parses and generates bytecode for an expression
    fn expression(&mut self, mut chunk: Chunk) -> Result<Chunk, CompilerError> {
        // Parse lowest precedence level
        chunk = self.parse_precedence(Precedence::Assignment, chunk)?;
        todo!()
    }

    fn number(&self, mut current_chunk: Chunk) -> Result<Chunk, CompilerError> {
        let value = self
            .parser
            .previous
            .to_string()
            .parse::<f64>()
            .map_err(|e| {
                CompilerError::Parse(vec![format!(
                    "Could not parse float from {:?}, {}",
                    self.parser.previous, e
                )])
            })?;
        current_chunk = self.emit_constant(value, current_chunk)?;
        Ok(current_chunk)
    }

    fn unary(&mut self, mut chunk: Chunk) -> Result<Chunk, CompilerError> {
        let operator_kind = self.parser.previous.kind.clone();

        // parse only the operand
        chunk = self.parse_precedence(Precedence::Unary, chunk)?;

        if let TokenKind::Minus = operator_kind {
            Ok(self.emit_byte(OpCode::Negate as u8, chunk))
        } else {
            Err(CompilerError::Unreachable(format!(
                "Expected unary negation on line {} but instead saw {:?}",
                self.parser.previous.line, operator_kind
            )))
        }
    }

    /// Starts at current token and parses any expressin at the current precedence or higher
    fn parse_precedence(
        &mut self,
        precedence: Precedence,
        mut chunk: Chunk,
    ) -> Result<Chunk, CompilerError> {
        self.advance()?;
        let parser_rule = self.get_rule(&self.parser.previous.kind)?;

        if let Some(prefix_fn) = &parser_rule.prefix {
            chunk = prefix_fn(self, chunk)?;
        } else {
            self.error_at(ErrorAtKind::Previous, Some("Expect expression"));
            // Non-fatal error, evidently?
            return Ok(chunk);
        };
        Ok(chunk)
    }

    fn grouping(&mut self, chunk: Chunk) -> Result<Chunk, CompilerError> {
        let chunk = self.expression(chunk)?;
        self.consume(TokenKind::RightParen, "Expect ')' after expression")?;
        Ok(chunk)
    }

    pub fn compile(&mut self, source: &'source_lifetime str) -> Result<Chunk, CompilerError> {
        self.scanner = Scanner::new(source);
        let mut chunk = Chunk::new();
        self.advance()?;

        chunk = self.expression(chunk)?;

        self.consume(TokenKind::Eof, "Expect end of expression")?;

        if self.parser.errors.is_empty() {
            Ok(chunk)
        } else {
            let errors: Vec<String> = self.parser.errors.drain(0..).collect();
            Err(CompilerError::Parse(errors))
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn advance_parser() {
        let mut compiler = Compiler::new();
        compiler.scanner = Scanner::new("1 + 2\0");

        let expected_tokens = vec![
            TokenKind::Number,
            TokenKind::Plus,
            TokenKind::Number,
            TokenKind::Eof,
        ];

        for _token_kind in expected_tokens {
            let res = compiler.advance();
            assert!(res.is_ok(), "{}", res.unwrap_err());
            assert!(
                matches!(&compiler.parser.current.kind, _token_kind),
                "{:?}",
                compiler.parser.current.kind
            );
        }
    }
}
