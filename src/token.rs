use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
    Identifier(String),
    String(String),
    Number(f64), // <-- all numbers are floating point at rutnime
    Bool(bool),
    Nil,
    None,
}

impl ToString for LiteralKind {
    fn to_string(&self) -> String {
        match &self {
            LiteralKind::Identifier(identifier) => identifier.clone(),
            LiteralKind::String(string) => string.clone(),
            LiteralKind::Number(num) => num.to_string(),
            LiteralKind::None => "nil".to_string(),
            LiteralKind::Bool(b) => b.to_string(),
            LiteralKind::Nil => "nil".to_string(),
        }
    }
}

impl std::hash::Hash for LiteralKind {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            LiteralKind::Identifier(v) => v.hash(state),
            LiteralKind::String(v) => v.hash(state),
            // just hash the raw bites for this
            LiteralKind::Number(v) => v.to_bits().hash(state),
            LiteralKind::Bool(v) => v.hash(state),
            LiteralKind::Nil => "".hash(state),
            LiteralKind::None => "".hash(state),
        }
    }
}
impl std::cmp::Eq for LiteralKind {}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TokenKind {
    // Single-char tokenInto<String>s
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    SemiColon,
    Slash,
    Star,

    // one or two-char tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

#[derive(Debug, Eq, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    // TODO this duplicates 'lexeme', and LiteralToken should be an optional
    pub literal: LiteralKind,
    pub line: usize,
}

impl std::hash::Hash for Token {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
        self.lexeme.hash(state);
        self.literal.hash(state);
        // Don't include the line number in the hash since an Expr can be referring to the same thing on different lines
    }
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: String, line: usize) -> Token {
        Token {
            kind,
            lexeme,
            literal: LiteralKind::None,
            line,
        }
    }
    /// Eugh, LiteralKind of None returns an EOF token
    /// Entails copying a string although we would have to copy it anyways from the slice of input
    pub fn new_literal(literal: LiteralKind, line: usize) -> Token {
        match &literal {
            LiteralKind::Identifier(i) => Token {
                kind: TokenKind::Identifier,
                lexeme: i.clone(),
                literal,
                line,
            },
            LiteralKind::String(s) => Token {
                kind: TokenKind::String,
                lexeme: s.clone(),
                literal,
                line,
            },
            LiteralKind::Number(n) => Token {
                kind: TokenKind::Number,
                lexeme: n.to_string(),
                literal,
                line,
            },
            LiteralKind::None => Token {
                kind: TokenKind::Eof,
                lexeme: literal.to_string(),
                literal,
                line,
            },
            LiteralKind::Bool(b) => {
                let kind = if *b {
                    TokenKind::True
                } else {
                    TokenKind::False
                };
                Token {
                    kind,
                    lexeme: b.to_string(),
                    literal,
                    line,
                }
            }
            LiteralKind::Nil => Token {
                kind: TokenKind::Nil,
                lexeme: literal.to_string(),
                literal,
                line,
            },
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} on line {}", self.lexeme, self.line)
    }
}
