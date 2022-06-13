#[derive(Debug, Clone, PartialEq)]
pub enum LiteralType {
    Identifier(String),
    String(String),
    Number(f64), // <-- all numbers are floating point at rutnime
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Single-char tokens
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

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenType,
    pub text: String,
    // TODO this duplicates 'text', and LiteralToken should be an optional
    pub literal: LiteralType,
    pub line: usize,
}

impl Token {
    pub fn new(kind: TokenType, text: String, literal: LiteralType, line: usize) -> Token {
        Token {
            kind,
            text,
            literal,
            line,
        }
    }
}
