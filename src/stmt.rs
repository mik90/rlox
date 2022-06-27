use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug, PartialEq)]
pub enum Stmt {
    /// Expression : Expr expression
    Expression(Expr),
    /// Print      : Expr expression
    Print(Expr),
    /// The initializer is optional
    /// Var        : Token name, [Expr initializer]
    Var(Token, Option<Expr>),
    /// Block      : Vec<Stmt> statements
    Block(Vec<Stmt>),
}

pub trait Visitor<E> {
    fn visit_expression_stmt(&mut self, expr: &Expr) -> Result<(), E>;
    fn visit_print_stmt(&mut self, expr: &Expr) -> Result<(), E>;
    fn visit_var_stmt(&mut self, name: &Token, initializer: &Option<Expr>) -> Result<(), E>;
    fn visit_block(&mut self, statements: &Vec<Stmt>) -> Result<(), E>;
}

impl Stmt {
    pub fn accept<E>(&self, visitor: &mut dyn Visitor<E>) -> Result<(), E> {
        match self {
            Stmt::Expression(expr) => visitor.visit_expression_stmt(expr),
            Stmt::Print(expr) => visitor.visit_print_stmt(expr),
            Stmt::Var(name, initializer) => visitor.visit_var_stmt(name, initializer),
            Stmt::Block(statements) => visitor.visit_block(statements),
        }
    }
}
