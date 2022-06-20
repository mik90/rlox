#[derive(Debug, Clone, PartialEq)]

pub enum LiteralKind {
    Identifier(String),
    String(String),
    Number(f64), // <-- all numbers are floating point at rutnime
    Bool(bool),
    Nil,
    None,
}

impl LiteralKind {
    pub fn to_string(&self) -> String {
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

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
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
    pub kind: TokenKind,
    pub lexeme: String,
    // TODO this duplicates 'lexeme', and LiteralToken should be an optional
    pub literal: LiteralKind,
    pub line: usize,
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
                    kind: kind,
                    lexeme: b.to_string(),
                    literal: literal,
                    line: line,
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
