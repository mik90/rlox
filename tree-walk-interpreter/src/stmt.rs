use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    /// Expression : Expr expression
    Expression(Expr),
    /// If         : Expr condition, Stmt thenBranch, [Stmt elseBranch]
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    /// Function : Token Name, Vec<Token> params, Vec<Stmt> body
    Function(Token, Vec<Token>, Vec<Stmt>),
    /// Print      : Expr expression
    Print(Expr),
    /// Return     : Token keyword, [Expr value]
    Return(Token, Option<Expr>),
    /// While      : Expr condition, Stmt body
    While(Expr, Box<Stmt>),
    /// Var        : Token name, [Expr initializer]
    Var(Token, Option<Expr>),
    /// Block      : Vec<Stmt> statements
    Block(Vec<Stmt>),
}

pub trait Visitor<E> {
    fn visit_expression_stmt(&mut self, expr: &Expr) -> Result<(), E>;
    fn visit_print_stmt(&mut self, expr: &Expr) -> Result<(), E>;
    fn visit_function_stmt(
        &mut self,
        name: &Token,
        params: &[Token],
        body: &[Stmt],
    ) -> Result<(), E>;
    fn visit_while_stmt(&mut self, condition: &Expr, body: &Stmt) -> Result<(), E>;
    fn visit_var_stmt(&mut self, name: &Token, initializer: &Option<Expr>) -> Result<(), E>;
    fn visit_block_stmt(&mut self, statements: &[Stmt]) -> Result<(), E>;
    fn visit_if_stmt(
        &mut self,
        condition: &Expr,
        then_branch: &Stmt,
        else_branch: &Option<Box<Stmt>>,
    ) -> Result<(), E>;
    fn visit_return_stmt(&mut self, keyword: &Token, value: &Option<Expr>) -> Result<(), E>;
}

impl Stmt {
    pub fn accept<E>(&self, visitor: &mut dyn Visitor<E>) -> Result<(), E> {
        match self {
            Stmt::Expression(expr) => visitor.visit_expression_stmt(expr),
            Stmt::Print(expr) => visitor.visit_print_stmt(expr),
            Stmt::Var(name, initializer) => visitor.visit_var_stmt(name, initializer),
            Stmt::Block(statements) => visitor.visit_block_stmt(statements),
            Stmt::If(condition, then_branch, else_branch) => {
                visitor.visit_if_stmt(condition, then_branch, else_branch)
            }
            Stmt::While(condition, body) => visitor.visit_while_stmt(condition, body),
            Stmt::Function(name, params, body) => visitor.visit_function_stmt(name, params, body),
            Stmt::Return(keyword, value) => visitor.visit_return_stmt(keyword, value),
        }
    }
}
