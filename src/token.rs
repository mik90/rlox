#[derive(Debug, Clone, PartialEq)]
pub enum LiteralType {
    Identifier(String),
    String(String),
    Number(f64), // <-- all numbers are floating point at rutnime
    Bool(bool),
    Nil,
    None,
}

impl LiteralType {
    pub fn to_string(&self) -> String {
        match &self {
            LiteralType::Identifier(identifier) => identifier.clone(),
            LiteralType::String(string) => string.clone(),
            LiteralType::Number(num) => num.to_string(),
            LiteralType::None => "nil".to_string(),
            LiteralType::Bool(b) => b.to_string(),
            LiteralType::Nil => "nil".to_string(),
        }
    }
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
    /// Eugh, LiteralType of None returns an EOF token
    pub fn new_literal(literal: LiteralType, line: usize) -> Token {
        match &literal {
            LiteralType::Identifier(i) => Token {
                kind: TokenType::Identifier,
                text: i.clone(),
                literal,
                line,
            },
            LiteralType::String(s) => Token {
                kind: TokenType::String,
                text: s.clone(),
                literal,
                line,
            },
            LiteralType::Number(n) => Token {
                kind: TokenType::Number,
                text: n.to_string(),
                literal,
                line,
            },
            LiteralType::None => Token {
                kind: TokenType::Eof,
                text: literal.to_string(),
                literal,
                line,
            },
            LiteralType::Bool(b) => {
                let kind = if *b {
                    TokenType::True
                } else {
                    TokenType::False
                };
                Token {
                    kind: kind,
                    text: b.to_string(),
                    literal: literal,
                    line: line,
                }
            }
            LiteralType::Nil => Token {
                kind: TokenType::Nil,
                text: literal.to_string(),
                literal,
                line,
            },
        }
    }
}
