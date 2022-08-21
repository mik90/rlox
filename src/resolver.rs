use crate::{
    expr,
    interpreter::Interpreter,
    stmt,
    token::{LiteralKind, Token},
};
use std::{collections::HashMap, error, fmt};

pub struct Resolver<'a> {
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

    pub fn resolve_stmts(&mut self, statements: &[stmt::Stmt]) -> Result<(), ResolverError> {
        for stmt in statements {
            self.resolve_stmt(stmt)?;
        }
        Ok(())
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
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.clone(), true);
            Ok(())
        } else {
            Ok(())
        }
    }

    fn resolve_expr(&mut self, expr: &expr::Expr) -> Result<(), ResolverError> {
        expr.accept(self)
    }

    fn resolve_local(&mut self, expr: &expr::Expr, name: &Token) -> Result<(), ResolverError> {
        if self.scopes.is_empty() {
            // No scopes, just assume the variable is global
            return Ok(());
        }
        let scope_idx = self.scopes.len() - 1;
        // Start at deepest scope in the stack and work outwards
        for i in (0..=scope_idx).rev() {
            if self.scopes[i].contains_key(&name.lexeme) {
                // Pass in variable and distance between innermost scope and this scope
                let _ = self.interpreter.resolve(&expr, scope_idx - i);
                // If we cant resolve something, assume it's global
            }
            return Ok(());
        }
        // If we aren't able to resolve the variable, just assume it's global
        Ok(())
    }

    fn resolve_stmt(&mut self, statement: &stmt::Stmt) -> Result<(), ResolverError> {
        statement.accept(self)
    }

    fn resolve_function(
        &mut self,
        _name: &Token, // the name is already defined in visit_function_stmt
        params: &[Token],
        body: &[stmt::Stmt],
    ) -> Result<(), ResolverError> {
        self.begin_scope()?;
        for param in params {
            self.declare(param)?;
            self.define(param)?;
        }
        self.resolve_stmts(body)?;
        self.end_scope()
    }
}

#[derive(Debug)]
pub enum ResolverError {
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
        self.resolve_expr(expr)
    }

    fn visit_print_stmt(&mut self, expr: &expr::Expr) -> Result<(), ResolverError> {
        self.resolve_expr(expr)
    }

    fn visit_function_stmt(
        &mut self,
        name: &Token,
        params: &[Token],
        body: &[stmt::Stmt],
    ) -> Result<(), ResolverError> {
        // define name asap so the function can call itself
        self.declare(name)?;
        self.define(name)?;
        self.resolve_function(name, params, body)
    }

    fn visit_while_stmt(
        &mut self,
        condition: &expr::Expr,
        body: &stmt::Stmt,
    ) -> Result<(), ResolverError> {
        self.resolve_expr(condition)?;
        self.resolve_stmt(body)
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
        self.resolve_expr(condition)?;
        self.resolve_stmt(then_branch)?;
        if let Some(else_branch) = else_branch {
            self.resolve_stmt(else_branch)?;
        }
        Ok(())
    }

    fn visit_return_stmt(
        &mut self,
        _keyword: &Token,
        value: &Option<expr::Expr>,
    ) -> Result<(), ResolverError> {
        if let Some(expr) = value {
            self.resolve_expr(expr)?;
        }
        Ok(())
    }
}

impl expr::Visitor<Result<(), ResolverError>> for Resolver<'_> {
    fn visit_binary(
        &mut self,
        lhs: &expr::Expr,
        _op: &Token,
        rhs: &expr::Expr,
    ) -> Result<(), ResolverError> {
        self.resolve_expr(lhs)?;
        self.resolve_expr(rhs)
    }

    fn visit_call(
        &mut self,
        callee: &expr::Expr,
        _paren: &Token,
        arguments: &[expr::Expr],
    ) -> Result<(), ResolverError> {
        self.resolve_expr(callee)?;
        for arg in arguments {
            self.resolve_expr(arg)?;
        }
        Ok(())
    }

    fn visit_grouping(&mut self, expr: &expr::Expr) -> Result<(), ResolverError> {
        self.resolve_expr(expr)
    }

    // A literal doesn't actually contain any variables or subexpressions
    fn visit_literal(&mut self, _value: &LiteralKind) -> Result<(), ResolverError> {
        Ok(())
    }

    fn visit_logical(
        &mut self,
        lhs: &expr::Expr,
        _op: &Token,
        rhs: &expr::Expr,
    ) -> Result<(), ResolverError> {
        self.resolve_expr(lhs)?;
        self.resolve_expr(rhs)
    }

    fn visit_unary(&mut self, _op: &Token, right: &expr::Expr) -> Result<(), ResolverError> {
        self.resolve_expr(right)
    }

    fn visit_variable(&mut self, name: &Token) -> Result<(), ResolverError> {
        // TODO clean up this hilarious nesting
        if !self.scopes.is_empty() {
            if let Some(scope) = self.scopes.last() {
                if let Some(var) = scope.get(&name.lexeme) {
                    if *var == false {
                        return Err(ResolverError::Semantic(format!(
                            "Can't read local variable '{}' in its own initializer",
                            name.lexeme
                        )));
                    }
                }
            }
        }
        // TODO: The book is kinda confusing here, why are we creating another expr out of this?
        self.resolve_local(&expr::Expr::Variable(name.clone()), name)
    }

    fn visit_assign(&mut self, name: &Token, value: &expr::Expr) -> Result<(), ResolverError> {
        self.resolve_expr(&value)?;
        self.resolve_local(value, &name)?;
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lox_value::LoxValue;

    #[test]
    fn test_define() -> Result<(), ResolverError> {
        let name = Token::new_literal(LiteralKind::Identifier("foo".to_string()), 0);
        let mut interpreter = Interpreter::new();
        interpreter
            .get_environment_mut()
            .define(&name.lexeme, LoxValue::Bool(false));
        let mut resolver = Resolver::new(&mut interpreter);

        resolver.begin_scope()?;
        resolver.declare(&name)?;
        resolver.define(&name)?;
        resolver.end_scope()?;
        Ok(())
    }
}
