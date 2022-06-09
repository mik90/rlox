use crate::token::LiteralType;
use crate::token::Token;

pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>), // left, operator, right
    Grouping(Box<Expr>),                 // expression
    Literal(LiteralType),                // whatever literal value except None i guess
    Unary(Token, Box<Expr>),             // operator, right
}
