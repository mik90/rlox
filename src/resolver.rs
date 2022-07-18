use crate::{
    expr,
    interpreter::Interpreter,
    stmt,
    token::{LiteralKind, Token},
};
use std::{collections::HashMap, error, fmt};

struct Resolver<'a> {
    interpreter: &'a mut Interpreter,
    /// Using the vec as a stack
    /// Per scope, maps varialbe names to whether or not they're visible
    scopes: Vec<HashMap<String, bool>>,
}

impl Resolver<'_> {
    pub fn new<'a>(interpreter: &'a mut Interpreter) -> Resolver<'a> {
        Resolver {
            interpreter,
            scopes: Vec::new(),
        }
    }
    fn begin_scope(&mut self) -> Result<(), ResolverError> {
        self.scopes.push(HashMap::new());
        Ok(())
    }
    fn end_scope(&mut self) -> Result<(), ResolverError> {
        self.scopes.pop();
        Ok(())
    }
    /// Adds variable to innermost scope
    fn declare(&mut self, name: &Token) -> Result<(), ResolverError> {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.clone(), false);
            Ok(())
        } else {
            // No scopes are there, which is fine
            Ok(())
        }
    }
    fn define(&mut self, name: &Token) -> Result<(), ResolverError> {
        if let Some(scope) = self.scopes.last() {
            todo!()
        } else {
            Ok(())
        }
    }
    fn resolve_expr(&mut self, expr: &expr::Expr) -> Result<(), ResolverError> {
        expr.accept(self);
        Ok(())
    }
    fn resolve_stmt(&mut self, statement: &stmt::Stmt) -> Result<(), ResolverError> {
        statement.accept(self)
    }
    fn resolve_stmts(&mut self, statements: &[stmt::Stmt]) -> Result<(), ResolverError> {
        for stmt in statements {
            self.resolve_stmt(stmt)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
enum ResolverError {
    Semantic(String),
    UnknownVariable(Token),
}

impl fmt::Display for ResolverError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            ResolverError::Semantic(e) => write!(f, "Resolver: Semantic error: {}", e),
            ResolverError::UnknownVariable(t) => write!(
                f,
                "Resolver: Unknown variable '{}' on line {}",
                t.lexeme, t.line
            ),
        }
    }
}

impl error::Error for ResolverError {}

impl stmt::Visitor<ResolverError> for Resolver<'_> {
    fn visit_expression_stmt(&mut self, expr: &expr::Expr) -> Result<(), ResolverError> {
        todo!()
    }

    fn visit_print_stmt(&mut self, expr: &expr::Expr) -> Result<(), ResolverError> {
        todo!()
    }

    fn visit_function_stmt(
        &mut self,
        name: &Token,
        params: &[Token],
        body: &[stmt::Stmt],
    ) -> Result<(), ResolverError> {
        todo!()
    }

    fn visit_while_stmt(
        &mut self,
        condition: &expr::Expr,
        body: &stmt::Stmt,
    ) -> Result<(), ResolverError> {
        todo!()
    }

    fn visit_var_stmt(
        &mut self,
        name: &Token,
        initializer: &Option<expr::Expr>,
    ) -> Result<(), ResolverError> {
        self.declare(&name)?;
        if let Some(init) = initializer {
            self.resolve_expr(&init)?;
        }
        self.define(&name)
    }

    fn visit_block_stmt(&mut self, statements: &[stmt::Stmt]) -> Result<(), ResolverError> {
        self.begin_scope()?;
        self.resolve_stmts(statements)?;
        self.end_scope()
    }

    fn visit_if_stmt(
        &mut self,
        condition: &expr::Expr,
        then_branch: &stmt::Stmt,
        else_branch: &Option<Box<stmt::Stmt>>,
    ) -> Result<(), ResolverError> {
        todo!()
    }

    fn visit_return_stmt(
        &mut self,
        keyword: &Token,
        value: &Option<expr::Expr>,
    ) -> Result<(), ResolverError> {
        todo!()
    }
}

impl expr::Visitor<()> for Resolver<'_> {
    fn visit_binary(&mut self, lhs: &expr::Expr, op: &Token, rhs: &expr::Expr) -> () {
        todo!()
    }

    fn visit_call(&mut self, callee: &expr::Expr, paren: &Token, arguments: &[expr::Expr]) -> () {
        todo!()
    }

    fn visit_grouping(&mut self, expr: &expr::Expr) -> () {
        todo!()
    }

    fn visit_literal(&mut self, value: &LiteralKind) -> () {
        todo!()
    }

    fn visit_logical(&mut self, lhs: &expr::Expr, op: &Token, rhs: &expr::Expr) -> () {
        todo!()
    }

    fn visit_unary(&mut self, op: &Token, right: &expr::Expr) -> () {
        todo!()
    }

    fn visit_variable(&mut self, name: &Token) -> () {
        todo!()
    }

    fn visit_assign(&mut self, name: &Token, value: &expr::Expr) -> () {
        todo!()
    }
}
