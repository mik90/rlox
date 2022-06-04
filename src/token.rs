use crate::token_type::TokenType;

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralType {
    Identifier(String),
    String(String),
    Number(f64), // <-- all numbers are floating point at rutnime
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenType,
    pub text: String,
    // TODO this duplicates 'text', and LiteralToken should be an optional
    pub literal: LiteralType,
    pub line: usize,
}
