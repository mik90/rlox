use crate::token_type::TokenType;

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralToken {
    Identifier(String),
    String(String),
    Number(i64), // <-- may want to store a float as well
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenType,
    pub lexeme: String,
    pub literal: LiteralToken,
    pub line: usize,
}
