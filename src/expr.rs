use crate::token::LiteralType;
use crate::token::Token;

pub trait Expr {}

pub struct Binary {
    left: Box<dyn Expr>,
    operator: Token,
    right: Box<dyn Expr>,
}

impl Expr for Binary {}
impl Binary {
    pub fn new(left: Box<dyn Expr>, operator: Token, right: Box<dyn Expr>) -> Binary {
        Binary {
            left,
            operator,
            right,
        }
    }
}
pub struct Grouping {
    expression: Box<dyn Expr>,
}

impl Expr for Grouping {}
impl Grouping {
    pub fn new(expression: Box<dyn Expr>) -> Grouping {
        Grouping { expression }
    }
}
pub struct Literal {
    value: LiteralType,
}

impl Expr for Literal {}
impl Literal {
    pub fn new(value: LiteralType) -> Literal {
        Literal { value }
    }
}
pub struct Unary {
    operator: Token,
    right: Box<dyn Expr>,
}

impl Expr for Unary {}
impl Unary {
    pub fn new(operator: Token, right: Box<dyn Expr>) -> Unary {
        Unary { operator, right }
    }
}
