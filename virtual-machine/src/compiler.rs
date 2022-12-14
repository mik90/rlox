use crate::{
    chunk::{debug::dissassemble_chunk, Chunk, OpCode},
    debugln, herefmt,
    scanner::{Scanner, ScannerError, Token, TokenKind},
    value::Value,
};
use std::{collections::HashMap, fmt, hash::Hash, rc::Rc};

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

/// Precedence from lowest to highest
#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
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

impl Precedence {
    fn next(&self) -> Precedence {
        let next_enum_integer = *self as u8 + 1;
        Precedence::from(next_enum_integer)
    }
}

impl From<u8> for Precedence {
    fn from(v: u8) -> Self {
        match v {
            x if x == Precedence::None as u8 => Precedence::None,
            x if x == Precedence::Assignment as u8 => Precedence::Assignment,
            x if x == Precedence::Or as u8 => Precedence::Or,
            x if x == Precedence::And as u8 => Precedence::And,
            x if x == Precedence::Equality as u8 => Precedence::Equality,
            x if x == Precedence::Comparison as u8 => Precedence::Comparison,
            x if x == Precedence::Term as u8 => Precedence::Term,
            x if x == Precedence::Factor as u8 => Precedence::Factor,
            x if x == Precedence::Unary as u8 => Precedence::Unary,
            x if x == Precedence::Call as u8 => Precedence::Call,
            x if x == Precedence::Primary as u8 => Precedence::Primary,
            _ => Precedence::Primary,
        }
    }
}

type ParseFn = dyn Fn(&mut Compiler, Chunk) -> Result<Chunk, CompilerError>;

// The functions are stored in an Rc so that they can be cloned from the map and called
// We cannot call the function if we're borrowing it since it uses the mutable Compiler
// Another option would be to only call it in-place but that isn't very ergonomic
#[derive(Clone)]
struct ParserRule {
    prefix: Option<Rc<ParseFn>>,
    infix: Option<Rc<ParseFn>>,
    precedence: Precedence,
}

impl ParserRule {
    fn new(
        prefix: Option<Rc<ParseFn>>,
        infix: Option<Rc<ParseFn>>,
        precedence: Precedence,
    ) -> ParserRule {
        ParserRule {
            prefix,
            infix,
            precedence,
        }
    }
}

type ParseRuleMap = HashMap<
    std::mem::Discriminant<TokenKind>, //< Kind of token
    ParserRule,
>;

pub struct Compiler<'source_lifetime> {
    // TODO, these may not need to be members since their init logic is so odd
    parser: Parser<'source_lifetime>,
    scanner: Scanner<'source_lifetime>,

    rules: ParseRuleMap,
}

enum ErrorAtKind {
    Previous,
    Current,
}

impl<'source_lifetime> Compiler<'source_lifetime> {
    pub fn new() -> Compiler<'source_lifetime> {
        Compiler {
            parser: Parser::new(),
            scanner: Scanner::new(""),
            rules: Compiler::make_rules(),
        }
    }

    fn make_rules() -> ParseRuleMap {
        // Closures that can be used for any of these parser rules
        let grouping = Rc::new(|compiler: &mut Compiler, chunk: Chunk| compiler.grouping(chunk));
        let literal = Rc::new(|compiler: &mut Compiler, chunk: Chunk| compiler.literal(chunk));
        let number = Rc::new(|compiler: &mut Compiler, chunk: Chunk| compiler.number(chunk));
        let binary = Rc::new(|compiler: &mut Compiler, chunk: Chunk| compiler.binary(chunk));
        let unary = Rc::new(|compiler: &mut Compiler, chunk: Chunk| compiler.unary(chunk));

        HashMap::from([
            (
                std::mem::discriminant(&TokenKind::LeftParen),
                ParserRule::new(Some(grouping.clone()), None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::RightParen),
                ParserRule::new(None, None, Precedence::None),
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
                ParserRule::new(Some(unary.clone()), Some(binary.clone()), Precedence::Term),
            ),
            (
                std::mem::discriminant(&TokenKind::Plus),
                ParserRule::new(None, Some(binary.clone()), Precedence::Term),
            ),
            (
                std::mem::discriminant(&&TokenKind::SemiColon),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Slash),
                ParserRule::new(None, Some(binary.clone()), Precedence::Factor),
            ),
            (
                std::mem::discriminant(&TokenKind::Star),
                ParserRule::new(None, Some(binary.clone()), Precedence::Factor),
            ),
            (
                std::mem::discriminant(&TokenKind::Bang),
                ParserRule::new(Some(unary.clone()), None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::BangEqual),
                ParserRule::new(None, Some(binary.clone()), Precedence::Equality),
            ),
            (
                std::mem::discriminant(&TokenKind::Equal),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::EqualEqual),
                ParserRule::new(None, Some(binary.clone()), Precedence::Equality),
            ),
            (
                std::mem::discriminant(&TokenKind::Greater),
                ParserRule::new(None, Some(binary.clone()), Precedence::Comparison),
            ),
            (
                std::mem::discriminant(&TokenKind::GreaterEqual),
                ParserRule::new(None, Some(binary.clone()), Precedence::Comparison),
            ),
            (
                std::mem::discriminant(&TokenKind::Less),
                ParserRule::new(None, Some(binary.clone()), Precedence::Comparison),
            ),
            (
                std::mem::discriminant(&TokenKind::LessEqual),
                ParserRule::new(None, Some(binary.clone()), Precedence::Comparison),
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
                ParserRule::new(Some(number.clone()), None, Precedence::None),
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
                ParserRule::new(Some(literal.clone()), None, Precedence::None),
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
                ParserRule::new(Some(literal.clone()), None, Precedence::None),
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
                ParserRule::new(Some(literal.clone()), None, Precedence::None),
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
                std::mem::discriminant(&TokenKind::Error("".to_owned())),
                ParserRule::new(None, None, Precedence::None),
            ),
            (
                std::mem::discriminant(&TokenKind::Eof),
                ParserRule::new(None, None, Precedence::None),
            ),
        ])
    }

    fn get_rule(&self, operator_kind: &TokenKind) -> ParserRule {
        self.rules
            .get(&std::mem::discriminant(operator_kind))
            .expect(&herefmt!(
                "Could not find parser rule for TokenKind '{:?}'",
                operator_kind
            ))
            .clone()
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
                    " at '{}': {}",
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
        if std::mem::discriminant(&self.parser.current.kind)
            == std::mem::discriminant(&expected_kind)
        {
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

    fn emit_constant(
        &self,
        value: Value,
        mut current_chunk: Chunk,
    ) -> Result<Chunk, CompilerError> {
        current_chunk
            .write_constant(value, self.parser.previous.line)
            .map_err(|e| {
                CompilerError::Bytecode(format!("On line {}, {}", self.parser.previous.line, e))
            })?;
        Ok(current_chunk)
    }

    /// Temporary measure as per the book to print hte value of our single expression
    fn end_compiler(&mut self, mut current_chunk: Chunk) -> Chunk {
        current_chunk.write_opcode(OpCode::Return, self.parser.previous.line);
        if self.parser.errors.is_empty() {
            debugln!("{}", dissassemble_chunk(&current_chunk, "code"));
        }

        current_chunk
    }

    fn binary(&mut self, mut chunk: Chunk) -> Result<Chunk, CompilerError> {
        let operator_kind = self.parser.previous.kind.clone();
        // Technically either infix/prefix works since the precedence doesnt care
        let rule = self.get_rule(&operator_kind);
        chunk = self.parse_precedence(rule.precedence.next(), chunk)?;

        match operator_kind {
            // Comparisons
            // Equality
            TokenKind::BangEqual => {
                let chunk = self.emit_opcode(OpCode::Equal, chunk);
                Ok(self.emit_opcode(OpCode::Not, chunk))
            }
            TokenKind::EqualEqual => Ok(self.emit_opcode(OpCode::Equal, chunk)),
            // Greater
            TokenKind::Greater => Ok(self.emit_opcode(OpCode::Greater, chunk)),
            TokenKind::GreaterEqual => {
                let chunk = self.emit_opcode(OpCode::Less, chunk);
                Ok(self.emit_opcode(OpCode::Not, chunk))
            }
            // Less
            TokenKind::Less => Ok(self.emit_opcode(OpCode::Less, chunk)),
            TokenKind::LessEqual => {
                let chunk = self.emit_opcode(OpCode::Greater, chunk);
                Ok(self.emit_opcode(OpCode::Not, chunk))
            }
            // Arithmetic
            TokenKind::Plus => Ok(self.emit_opcode(OpCode::Add, chunk)),
            TokenKind::Minus => Ok(self.emit_opcode(OpCode::Subtract, chunk)),
            TokenKind::Star => Ok(self.emit_opcode(OpCode::Multiply, chunk)),
            TokenKind::Slash => Ok(self.emit_opcode(OpCode::Divide, chunk)),
            // Other
            _ => Err(CompilerError::Unreachable(format!(
                "Did not expect operator {:?} in binary expression on line {}",
                operator_kind, self.parser.previous.line
            ))),
        }
    }

    fn literal(&mut self, chunk: Chunk) -> Result<Chunk, CompilerError> {
        match &self.parser.previous.kind {
            TokenKind::False => Ok(self.emit_opcode(OpCode::False, chunk)),
            TokenKind::Nil => Ok(self.emit_opcode(OpCode::Nil, chunk)),
            TokenKind::True => Ok(self.emit_opcode(OpCode::True, chunk)),
            other => Err(CompilerError::Unreachable(herefmt!(
                "Expected literal but saw {:?}",
                other
            ))),
        }
    }

    // parses and generates bytecode for an expression
    fn expression(&mut self, chunk: Chunk) -> Result<Chunk, CompilerError> {
        // Parse lowest precedence level
        self.parse_precedence(Precedence::Assignment, chunk)
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
        current_chunk = self.emit_constant(Value::Number(value), current_chunk)?;
        Ok(current_chunk)
    }

    fn unary(&mut self, mut chunk: Chunk) -> Result<Chunk, CompilerError> {
        let operator_kind = self.parser.previous.kind.clone();

        // parse only the operand
        chunk = self.parse_precedence(Precedence::Unary, chunk)?;

        match operator_kind {
            TokenKind::Minus => Ok(self.emit_opcode(OpCode::Negate, chunk)),
            TokenKind::Bang => Ok(self.emit_opcode(OpCode::Not, chunk)),
            other => Err(CompilerError::Unreachable(format!(
                "Expected unary negation on line {} but instead saw {:?}",
                self.parser.previous.line, other
            ))),
        }
    }

    /// Starts at current token and parses any expressin at the current precedence or higher
    fn parse_precedence(
        &mut self,
        precedence: Precedence,
        mut chunk: Chunk,
    ) -> Result<Chunk, CompilerError> {
        self.advance()?;
        let prev_kind = self.parser.previous.kind.clone();
        let rule = self.get_rule(&prev_kind);

        if let Some(prefix_fn) = rule.prefix {
            chunk = prefix_fn(self, chunk)?;
        } else {
            self.error_at(ErrorAtKind::Previous, Some("Expect expression"));
            // Non-fatal error, evidently?
            return Ok(chunk);
        };

        // Handle infix
        loop {
            let current_kind = self.parser.current.kind.clone();
            let rule = self.get_rule(&current_kind);
            if precedence > rule.precedence {
                break;
            }
            self.advance()?;
            let prev_kind = self.parser.previous.kind.clone();
            let rule = self.get_rule(&prev_kind);
            match rule.infix {
                Some(r) => chunk = r(self, chunk)?,
                None => {
                    return Err(CompilerError::Unreachable(format!(
                        "No infix parser rule provided for {:?} on line {}",
                        &self.parser.previous.kind, self.parser.previous.line
                    )))
                }
            }
        }

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
    use crate::chunk;

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

        for token_kind in expected_tokens {
            let res = compiler.advance();
            assert!(res.is_ok(), "{}", res.unwrap_err());
            assert_eq!(
                std::mem::discriminant(&compiler.parser.current.kind),
                std::mem::discriminant(&token_kind),
                "{:?}",
                compiler.parser.current.kind
            );
        }
    }

    #[test]
    fn compile_literal() {
        let mut compiler = Compiler::new();

        let source = "true\0";
        let chunk = compiler.compile(source);
        assert!(chunk.is_ok());
        let chunk = chunk.unwrap();

        let mut code = chunk.code_iter();
        assert_eq!(code.clone().count(), 1);
        let byte = code.next().unwrap();

        let maybe_op = OpCode::try_from(*byte);
        assert!(maybe_op.is_ok());
        assert_eq!(maybe_op.unwrap(), OpCode::True);
    }
}
