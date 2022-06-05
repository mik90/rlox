use crate::token::Token;

pub trait Expr {}

pub struct BinaryExpr {
    left: Box<dyn Expr>,
    operator: Token,
    right: Box<dyn Expr>,
}

impl Expr for BinaryExpr {}

impl BinaryExpr {
    pub fn new(left: Box<dyn Expr>, operator: Token, right: Box<dyn Expr>) -> BinaryExpr {
        BinaryExpr {
            left,
            operator,
            right,
        }
    }
}
