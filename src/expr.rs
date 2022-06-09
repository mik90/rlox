use crate::token::LiteralType;
use crate::token::Token;

#[derive(Debug)]
pub enum Expr {
    /// Binary   : Expr left, Token operator, Expr right
    Binary(Box<Expr>, Token, Box<Expr>),
    /// Grouping : Expr expression
    Grouping(Box<Expr>),
    /// Literal  : Object value
    Literal(LiteralType),
    /// Unary    : Token operator, Expr right
    Unary(Token, Box<Expr>),
}

pub trait Visitor<T> {
    fn visit_binary(&mut self, lhs: Box<Expr>, op: Token, rhs: Box<Expr>) -> T;
    fn visit_grouping(&mut self, expr: Box<Expr>) -> T;
    fn visit_literal(&mut self, value: LiteralType) -> T;
    fn visit_unary(&mut self, op: Token, right: Box<Expr>) -> T;
}

impl Expr {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
        match &self {
            Expr::Binary(lhs, op, rhs) => todo!(),
            Expr::Grouping(expr) => todo!(),
            Expr::Literal(lit) => todo!(),
            Expr::Unary(op, expr) => todo!(),
        }
    }
}
