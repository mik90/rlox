use crate::token::LiteralKind;
use crate::token::Token;

// Expr is boxed in order to allow this enum to be recursive
// Otherwise it wouldn't be able to figure out the size of Expr
#[derive(Debug, PartialEq)]
pub enum Expr {
    /// Binary   : Expr left, Token operator, Expr right
    Binary(Box<Expr>, Token, Box<Expr>),
    /// Grouping : Expr expression
    Grouping(Box<Expr>),
    /// Literal  : Object value
    Literal(LiteralKind),
    /// Unary    : Token operator, Expr right
    Unary(Token, Box<Expr>),
}

pub trait Visitor<T> {
    fn visit_binary(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> T;
    fn visit_grouping(&mut self, expr: &Expr) -> T;
    fn visit_literal(&mut self, value: &LiteralKind) -> T;
    fn visit_unary(&mut self, op: &Token, right: &Expr) -> T;
}

impl Expr {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
        match self {
            Expr::Binary(lhs, op, rhs) => visitor.visit_binary(lhs, op, rhs),
            Expr::Grouping(expr) => visitor.visit_grouping(expr),
            Expr::Literal(lit) => visitor.visit_literal(lit),
            Expr::Unary(op, expr) => visitor.visit_unary(op, expr),
        }
    }
}
