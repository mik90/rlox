use crate::expr::Expr;

#[derive(Debug, PartialEq)]
pub enum Stmt {
    /// Expression : Expr expression
    Expression(Expr),
    /// Print     : Expr expression
    Print(Expr),
}

pub trait Visitor<E> {
    fn visit_expression_stmt(&mut self, expr: &Expr) -> Result<(), E>;
    fn visit_print_stmt(&mut self, expr: &Expr) -> Result<(), E>;
}

impl Stmt {
    pub fn accept<E>(&self, visitor: &mut dyn Visitor<E>) -> Result<(), E> {
        match self {
            Stmt::Expression(expr) => visitor.visit_expression_stmt(expr),
            Stmt::Print(expr) => visitor.visit_print_stmt(expr),
        }
    }
}
