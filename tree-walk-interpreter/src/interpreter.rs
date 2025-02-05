use crate::{
    environment,
    environment::Environment,
    error::ErrorMessage,
    expr::{self, Expr},
    lox_value::{LoxCallable, LoxFunction, LoxValue},
    stmt,
    token::{LiteralKind, Token, TokenKind},
    trace,
};
use std::fmt;
use std::rc::Rc;
use std::time;
use std::{collections::HashMap, sync::Mutex};
use std::{error, sync::Arc};

#[derive(Debug)]
pub enum EvalError {
    UnreachableError(ErrorMessage),
    // Line number, error msg
    InvalidType(usize, ErrorMessage),
    UndefinedVariable(Token),
    /// Hacky, but this is analgous to how jlox uses try/catch for control flow in return values
    Return(LoxValue),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            EvalError::UnreachableError(e) => write!(f, "UnreachableError: {}", e),
            EvalError::InvalidType(l, e) => write!(f, "InvalidType: On line {}, message: {}", l, e),
            EvalError::UndefinedVariable(token) => write!(f, "Undefined variable: {}", token),
            EvalError::Return(value) => write!(f, "Return value: {:?}", value),
        }
    }
}

impl error::Error for EvalError {}

pub struct Interpreter {
    /// ew why does this have to be public? For exposure to LoxCallable but why cant this be an arg?
    pub env: Arc<Mutex<Environment>>,
    pub globals: Arc<Mutex<Environment>>,
    // Expression to 'depth' (scopes between current scope (top of envionment stack) and the one where a variable is defined)
    // TODO May need to have a way to uniquely identify expressions
    locals: HashMap<expr::Expr, usize>,
}

/// Returns the time since the epoch in seconds as a double
struct NativeClock {}
impl LoxCallable for NativeClock {
    fn arity(&self) -> usize {
        0
    }

    fn call(&self, _: &mut Interpreter, _: &[LoxValue]) -> Result<LoxValue, EvalError> {
        match time::SystemTime::now().duration_since(time::SystemTime::UNIX_EPOCH) {
            Ok(t) => Ok(LoxValue::Number(t.as_secs_f64())),
            Err(t) => Err(EvalError::UnreachableError(format!(
                "System time {} is before unix epoch!",
                t
            ))),
        }
    }
}
impl fmt::Display for NativeClock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn>")
    }
}

impl Interpreter {
    fn evaluate(&mut self, expr: &Expr) -> Result<LoxValue, EvalError> {
        expr.accept(self)
    }

    pub fn new() -> Interpreter {
        let globals = Environment::new_empty();
        globals
            .lock()
            .unwrap()
            .define("clock", LoxValue::Callable(Rc::new(NativeClock {})));
        Interpreter {
            env: globals.clone(),
            globals,
            locals: HashMap::new(),
        }
    }

    pub fn resolve(&mut self, expr: &Expr, scope_distance: usize) -> Result<(), EvalError> {
        trace!("resolved {:?} at distance {}", expr, scope_distance);
        self.locals.insert(expr.clone(), scope_distance);
        Ok(())
    }

    fn execute(&mut self, stmt: &stmt::Stmt) -> Result<(), EvalError> {
        stmt.accept(self)
    }

    fn look_up_variable(&self, name: &Token, expr: &expr::Expr) -> Option<LoxValue> {
        for (value, dist) in self.locals.iter() {
            trace!("local value '{:?}' has dist={}", value, dist);
        }
        if let Some(distance) = self.locals.get(expr) {
            trace!(
                "looking up {} with distance from the top of the env stack {}",
                name.lexeme,
                distance
            );
            environment::get_copy_at(self.env.clone(), *distance, &name.lexeme)
        } else {
            trace!("looking up {} from globals", name.lexeme);
            self.globals.lock().unwrap().get_copy(&name.lexeme)
        }
    }

    /// Helper for the Stmt visitor
    pub fn execute_block(
        &mut self,
        statements: &[stmt::Stmt],
        new_env: Arc<Mutex<Environment>>,
    ) -> Result<(), EvalError> {
        let prev = self.env.clone();

        let execute_statements = || -> Result<(), EvalError> {
            self.env = new_env;
            // push a new env onto our current stack since we're in a new blcok
            for stmt in statements {
                self.execute(stmt)?;
            }
            Ok(())
        };

        let res = execute_statements();
        // Reset the env in case any stmt couldnt be executed
        self.env = prev;

        res
    }

    /// Interpret an expression, return true on success and false on error
    pub fn interpret(&mut self, statements: Vec<stmt::Stmt>) -> bool {
        for stmt in statements {
            if let Err(e) = self.execute(&stmt) {
                eprintln!("Error during interpret(): {}", e);
                return false;
            }
        }
        true
    }
}

impl expr::Visitor<Result<LoxValue, EvalError>> for Interpreter {
    fn visit_binary(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> Result<LoxValue, EvalError> {
        let (left, right) = (self.evaluate(lhs)?, self.evaluate(rhs)?);
        let line = op.line;

        match op.kind {
            // Arithmetic operations on numbers
            TokenKind::Minus => {
                let (l, r) = LoxValue::as_numbers(line, left, right)?;
                 Ok(LoxValue::Number(l - r))
            },
            TokenKind::Slash => {
                let (l, r) = LoxValue::as_numbers(line, left, right)?;
                 Ok(LoxValue::Number(l / r))
            },
            TokenKind::Star => {
                let (l, r) = LoxValue::as_numbers(line, left, right)?;
                 Ok(LoxValue::Number(l * r))
            },

            // Comparisons
            TokenKind::Greater => {
                let (l, r) = LoxValue::as_numbers(line, left, right)?;
                 Ok(LoxValue::Bool(l > r))
            },
            TokenKind::GreaterEqual => {
                let (l, r) = LoxValue::as_numbers(line, left, right)?;
                 Ok(LoxValue::Bool(l >= r))
            },
            TokenKind::Less => {
                let (l, r) = LoxValue::as_numbers(line, left, right)?;
                 Ok(LoxValue::Bool(l < r))
            },
            TokenKind::LessEqual => {
                let (l, r) = LoxValue::as_numbers(line, left, right)?;
                 Ok(LoxValue::Bool(l <= r))
            },

            // Aithmetic and concat operations
            TokenKind::Plus => match (left, right) {
                // String concatenation
                (LoxValue::String(l), LoxValue::String(r)) => {
                    Ok(LoxValue::String(format!("{l}{r}")))
                }
                (LoxValue::Number(l), LoxValue::Number(r)) => Ok(LoxValue::Number(l + r)),
                (l, r) => Err(EvalError::InvalidType(op.line, format!(
                    "Cannot use operator '+' with left hand side operand '{:?}' and right side operand '{:?}'",
                     l, r
                ))),
            },

            // Unknown
            _ => Err(EvalError::InvalidType(op.line,format!(
                "Unexpected operator '{:?}' for binary expression",
                op,
            ))),
        }
    }

    fn visit_grouping(&mut self, expr: &Expr) -> Result<LoxValue, EvalError> {
        self.evaluate(expr)
    }

    fn visit_literal(&mut self, value: &LiteralKind) -> Result<LoxValue, EvalError> {
        match value {
            LiteralKind::String(s) => Ok(LoxValue::String(s.clone())),
            LiteralKind::Number(n) => Ok(LoxValue::Number(*n)),
            LiteralKind::Bool(b) => Ok(LoxValue::Bool(*b)),
            LiteralKind::Nil => Ok(LoxValue::Nil),
            // TODO unsure how to handle an Identifier here since there's no LoxValue for it
            _ => Err(EvalError::UnreachableError(format!(
                "Expected literal but found {value:?}"
            ))),
        }
    }

    fn visit_logical(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> Result<LoxValue, EvalError> {
        let left = self.evaluate(lhs)?;

        if op.kind == TokenKind::Or {
            if left.is_truthy() {
                return Ok(left);
            }
        } else {
            // Not an or, if lhs is falsey then just return that
            if !left.is_truthy() {
                return Ok(left);
            }
        }

        self.evaluate(rhs)
    }

    fn visit_unary(&mut self, op: &Token, right: &Expr) -> Result<LoxValue, EvalError> {
        // evaluate the rhs of the unary
        let rhs_value = self.evaluate(right)?;

        // apply the operator to the rhs
        match op.kind {
            TokenKind::Minus => {
                if let LoxValue::Number(n) = rhs_value {
                    Ok(LoxValue::Number(-n))
                } else {
                    Err(EvalError::InvalidType(
                        op.line,
                        format!("Cannot use unary '-' with {rhs_value:?}"),
                    ))
                }
            }
            TokenKind::Bang => Ok(LoxValue::Bool(!rhs_value.is_truthy())),
            _ => Err(EvalError::InvalidType(
                op.line,
                format!(
                    "Cannot use {:?} as the operator in a unary expression",
                    op.kind
                ),
            )),
        }
    }

    fn visit_variable(&mut self, name: &Token) -> Result<LoxValue, EvalError> {
        match self.look_up_variable(name, &Expr::Variable(name.clone())) {
            Some(v) => Ok(v),
            None => Err(EvalError::UndefinedVariable(name.clone())),
        }
    }

    fn visit_assign(&mut self, name: &Token, value_expr: &Expr) -> Result<LoxValue, EvalError> {
        let value = self.evaluate(value_expr)?;
        if let Some(distance) = self.locals.get(value_expr) {
            environment::assign_at(self.env.clone(), *distance, &name.lexeme, value.clone());
        } else {
            self.globals
                .lock()
                .unwrap()
                .assign(&name.lexeme, value.clone());
        }
        Ok(value)
    }

    fn visit_call(
        &mut self,
        callee: &Expr,
        paren: &Token,
        arguments: &[Expr],
    ) -> Result<LoxValue, EvalError> {
        let mut evaluated_args = Vec::new();
        for arg in arguments {
            evaluated_args.push(self.evaluate(arg)?);
        }

        let function = match self.evaluate(callee)? {
            LoxValue::Callable(function) => function,
            non_callable_type => {
                return Err(EvalError::InvalidType(
                    paren.line,
                    format!(
                        "Can only call functions and classes. Cannot call type {:?}",
                        non_callable_type
                    ),
                ));
            }
        };

        if arguments.len() != function.arity() {
            return Err(EvalError::InvalidType(
                paren.line,
                format!(
                    "Expected {} arguments but got {}.",
                    function.arity(),
                    arguments.len()
                ),
            ));
        }
        function.call(self, &evaluated_args)
    }
}

impl stmt::Visitor<EvalError> for Interpreter {
    fn visit_expression_stmt(&mut self, expr: &Expr) -> Result<(), EvalError> {
        match self.evaluate(expr) {
            // This isn't an error in the sense that a statement that is
            // > 3 + 5
            // isn't an error
            Ok(_) => Ok(()), // discards the value from the expression statemetn
            Err(e) => Err(e),
        }
    }

    fn visit_print_stmt(&mut self, expr: &Expr) -> Result<(), EvalError> {
        let value = self.evaluate(expr)?;
        println!("{}", value);
        Ok(())
    }

    fn visit_var_stmt(
        &mut self,
        name: &Token,
        initializer: &Option<Expr>,
    ) -> Result<(), EvalError> {
        let value = if let Some(initializing_expr) = initializer {
            Some(self.evaluate(initializing_expr)?)
        } else {
            None
        };

        self.env
            .lock()
            .unwrap()
            .define(&name.lexeme, value.unwrap_or(LoxValue::Nil));
        Ok(())
    }

    fn visit_block_stmt(&mut self, statements: &[stmt::Stmt]) -> Result<(), EvalError> {
        // environment per block
        trace!("creating new environment at block");
        self.execute_block(statements, Environment::new_enclosing(self.env.clone()))?;
        Ok(())
    }

    fn visit_if_stmt(
        &mut self,
        condition: &Expr,
        then_branch: &stmt::Stmt,
        else_branch: &Option<Box<stmt::Stmt>>,
    ) -> Result<(), EvalError> {
        if self.evaluate(condition)?.is_truthy() {
            self.execute(then_branch)
        } else if let Some(else_branch) = else_branch {
            self.execute(else_branch)
        } else {
            // The 'if' was falsy and there was no 'else'
            Ok(())
        }
    }

    fn visit_while_stmt(&mut self, condition: &Expr, body: &stmt::Stmt) -> Result<(), EvalError> {
        while self.evaluate(condition)?.is_truthy() {
            self.execute(body)?;
        }
        Ok(())
    }

    fn visit_function_stmt(
        &mut self,
        name: &Token,
        params: &[Token],
        body: &[stmt::Stmt],
    ) -> Result<(), EvalError> {
        let function = LoxFunction::new(
            name.clone(),
            params.to_vec(),
            body.to_vec(),
            self.env.clone(),
        );

        self.env
            .lock()
            .unwrap()
            .define(&name.lexeme, LoxValue::Callable(Rc::new(function)));
        Ok(())
    }

    fn visit_return_stmt(
        &mut self,
        _keyword: &Token,
        value: &Option<Expr>,
    ) -> Result<(), EvalError> {
        let value: LoxValue = value
            .as_ref()
            .map_or_else(|| Ok(LoxValue::Nil), |expr| self.evaluate(expr))?;
        // Hacky, but we don't have exceptions here and I am NOT using a panic handler for this
        Err(EvalError::Return(value))
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::resolver::Resolver;
    use crate::token::*;
    use crate::{expr::Expr, stmt::Stmt};

    fn number_expr(n: f64) -> Box<Expr> {
        Box::new(Expr::Literal(LiteralKind::Number(n)))
    }

    fn negated_number_expr(n: f64) -> Box<Expr> {
        Box::new(Expr::Unary(
            Token::new(TokenKind::Minus, "-".to_string(), 1),
            number_expr(n),
        ))
    }

    /// Declares a value of 'name' and inits it with 'value' in one statement
    fn declare_and_init_number(name: &str, value: f64) -> Stmt {
        let token = Token::new_literal(LiteralKind::Identifier(name.to_string()), 1);
        Stmt::Var(token, Some(Expr::Literal(LiteralKind::Number(value))))
    }

    fn assign_to_var(name: &str, value: f64) -> Stmt {
        let token = Token::new_literal(LiteralKind::Identifier(name.to_string()), 1);
        let expr = Expr::Assign(token, Box::new(Expr::Literal(LiteralKind::Number(value))));
        Stmt::Expression(expr)
    }

    fn print_variable(name: &str) -> Stmt {
        let var_to_print = Expr::Variable(Token::new_literal(
            LiteralKind::Identifier(name.to_string()),
            1,
        ));
        Stmt::Print(var_to_print)
    }

    fn string_expr(s: &str) -> Box<Expr> {
        Box::new(Expr::Literal(LiteralKind::String(s.to_owned())))
    }

    fn execute_and_resolve(
        mut interpreter: &mut Interpreter,
        stmt: &stmt::Stmt,
    ) -> Result<(), String> {
        let mut resolver = Resolver::new(&mut interpreter);
        let res = resolver.resolve_stmt(&stmt);
        if let Err(e) = res {
            return Err(format!("resolve_stmt failed with: {}", e.to_string()));
        }

        let res = interpreter.execute(&stmt);
        if let Err(e) = res {
            return Err(format!("execute failed with: {}", e.to_string()));
        }
        return Ok(());
    }

    #[test]
    fn eval_addition() {
        let expr = Expr::Binary(
            number_expr(3.0),
            Token::new(TokenKind::Plus, "+".to_string(), 1),
            number_expr(3.0),
        );
        let mut interpreter = Interpreter::new();
        let res = interpreter.evaluate(&expr);
        assert!(res.is_ok(), "evaluate() failed with {:?}", res.err());
        let res = res.unwrap();
        if let LoxValue::Number(n) = res {
            assert_eq!(n, 6.0)
        } else {
            assert!(false, "Expected LoxValue::Number but was {:?}", res)
        }
    }

    #[test]
    fn eval_muffin() {
        let expr = Expr::Binary(
            number_expr(3.0),
            Token::new(TokenKind::Slash, "/".to_string(), 1),
            string_expr("muffin"),
        );
        let mut interpreter = Interpreter::new();
        let res = interpreter.evaluate(&expr);
        assert!(res.is_err(), "Expected evaluate() to fail but it didn't");
    }

    #[test]
    fn eval_negated_number() {
        let expr = negated_number_expr(5.0);
        let mut interpreter = Interpreter::new();
        let res = interpreter.evaluate(&expr);
        assert!(res.is_ok(), "evaluate() failed with {:?}", res.err());

        let res = res.unwrap();
        if let LoxValue::Number(n) = res {
            assert_eq!(n, -5.0)
        } else {
            assert!(false, "Expected LoxValue::Number but was {:?}", res)
        }
    }
    #[test]
    fn eval_assignment() {
        let stmt = declare_and_init_number("foo", 5.0);

        let mut interpreter = Interpreter::new();

        let res = execute_and_resolve(&mut interpreter, &stmt);
        assert!(res.is_ok(), "{}", res.unwrap_err());

        let stmt = print_variable("foo");
        let res = execute_and_resolve(&mut interpreter, &stmt);
        assert!(res.is_ok(), "{}", res.unwrap_err());
    }

    #[test]
    fn eval_var_update() {
        let mut interpreter = Interpreter::new();

        let stmt = declare_and_init_number("foo", 5.0);

        let res = execute_and_resolve(&mut interpreter, &stmt);
        assert!(res.is_ok(), "{}", res.unwrap_err());

        let value = interpreter.env.lock().unwrap().get_copy("foo");
        assert!(value.is_some());
        assert_eq!(value.unwrap(), LoxValue::Number(5.0));

        let stmt = assign_to_var("foo", 10.0);

        let res = execute_and_resolve(&mut interpreter, &stmt);
        assert!(res.is_ok(), "{}", res.unwrap_err());

        let value = interpreter.env.lock().unwrap().get_copy("foo");
        assert!(value.is_some());
        assert_eq!(value.unwrap(), LoxValue::Number(10.0));
    }

    #[test]
    fn native_call() {
        let mut interpreter = Interpreter::new();

        let clock_func = interpreter.env.lock().unwrap().get_copy("clock");
        assert!(clock_func.is_some());
        let clock_func = clock_func.unwrap();
        if let LoxValue::Callable(callable) = clock_func {
            assert_eq!(callable.arity(), 0);
            let lox_value = callable.call(&mut interpreter, &[]);
            assert!(
                lox_value.is_ok(),
                "Expected Ok but was {}",
                lox_value.is_err()
            );
            if let LoxValue::Number(n) = lox_value.unwrap() {
                // Time should be past the unix epoch
                assert!(n > 0.0);
            }
        } else {
            assert!(
                false,
                "Expected clock_func to be a callable but was {:?}",
                clock_func
            );
        }
    }

    #[test]
    fn test_assignment() {
        let mut interpreter = Interpreter::new();
        let mut resolver = Resolver::new(&mut interpreter);

        let name = Token::new_literal(LiteralKind::Identifier("foo".to_string()), 0);
        let initializing_expr = Some(expr::Expr::Literal(LiteralKind::Number(5.0)));
        let stmt = stmt::Stmt::Var(name, initializing_expr);
        let res = resolver.resolve_stmt(&stmt);

        assert!(res.is_ok(), "{}", res.unwrap_err());
    }

    /// Equivalent to
    ///        for (var i = 0; i < 2; i = i + 1) {
    ///        }
    #[test]
    fn test_desugared_for() {
        let mut interpreter = Interpreter::new();
        let mut resolver = Resolver::new(&mut interpreter);

        let var_assign_stmt = {
            let i_identifier = Token::new_literal(LiteralKind::Identifier("i".to_string()), 0);
            let initializing_expr = Some(expr::Expr::Literal(LiteralKind::Number(1.0)));
            stmt::Stmt::Var(i_identifier, initializing_expr)
        };
        let condition = {
            let i_identifier = Expr::Variable(Token::new_literal(
                LiteralKind::Identifier("i".to_string()),
                0,
            ));
            let less_than = Token::new(TokenKind::Less, "<".to_string(), 0);
            let limit = Expr::Literal(LiteralKind::Number(1.0));
            let condition = Expr::Binary(Box::new(i_identifier), less_than, Box::new(limit));
            condition
        };

        let increment = {
            let i_token = Token::new_literal(LiteralKind::Identifier("i".to_string()), 0);
            let i_identifier = Expr::Variable(i_token.clone());

            let plus = Token::new(TokenKind::Plus, "+".to_string(), 0);
            let increment = Expr::Literal(LiteralKind::Number(1.0));
            let increment_expr = Expr::Binary(Box::new(i_identifier), plus, Box::new(increment));

            let assign_expr = Expr::Assign(i_token, Box::new(increment_expr));

            Stmt::Expression(assign_expr)
        };

        let empty_body = Stmt::Block(vec![]);

        let mut body = Stmt::Block(vec![empty_body, increment]);
        body = Stmt::While(condition, Box::new(body));
        body = Stmt::Block(vec![var_assign_stmt, body]);

        let res = resolver.resolve_stmt(&body);
        assert!(res.is_ok(), "{}", res.unwrap_err());

        assert!(interpreter.interpret(vec![body]));
        // TODO whittle down to min test case
    }

    #[test]
    fn test_increment() {
        let mut interpreter = Interpreter::new();
        let mut resolver = Resolver::new(&mut interpreter);

        let var_assign_stmt = {
            let i_identifier = Token::new_literal(LiteralKind::Identifier("i".to_string()), 0);
            let initializing_expr = Some(expr::Expr::Literal(LiteralKind::Number(1.0)));
            stmt::Stmt::Var(i_identifier, initializing_expr)
        };

        let condition = {
            let i_identifier = Expr::Variable(Token::new_literal(
                LiteralKind::Identifier("i".to_string()),
                0,
            ));
            let less_than = Token::new(TokenKind::Less, "<".to_string(), 0);
            let limit = Expr::Literal(LiteralKind::Number(1.0));
            let condition = Expr::Binary(Box::new(i_identifier), less_than, Box::new(limit));
            condition
        };

        let increment = {
            let i_token = Token::new_literal(LiteralKind::Identifier("i".to_string()), 0);
            let i_identifier = Expr::Variable(i_token.clone());

            let plus = Token::new(TokenKind::Plus, "+".to_string(), 0);
            let increment = Expr::Literal(LiteralKind::Number(1.0));
            let increment_expr = Expr::Binary(Box::new(i_identifier), plus, Box::new(increment));

            let assign_expr = Expr::Assign(i_token, Box::new(increment_expr));

            Stmt::Expression(assign_expr)
        };

        /*
        Also breaks
        let body = Stmt::Block(vec![
            var_assign_stmt,
            Stmt::Block(vec![Stmt::While(
                condition,
                Box::new(Stmt::Block(vec![Stmt::Block(vec![]), increment])),
            )]),
        ]);
         */

        /*
        // Works
        let body = Stmt::Block(vec![
            var_assign_stmt,
            Stmt::Block(vec![Stmt::While(condition, Box::new(increment))]),
        ]);
        */
        // should be the simplest case that fails
        let body = Stmt::Block(vec![
            var_assign_stmt,
            Stmt::Block(vec![Stmt::While(
                condition,
                Box::new(Stmt::Block(vec![increment])),
            )]),
        ]);

        /*
        So something here is breaking

            This subset doesn't work

            Stmt::Block(vec![Stmt::While(
                condition,
                Box::new(Stmt::Block(vec![Stmt::Block(vec![]), increment])),
            )]),

            nor does

            Stmt::Block(vec![Stmt::While(
                condition,
                Box::new(Stmt::Block(vec![increment])),
            )]),

            -----------------------------------------

            But this does

            Stmt::Block(vec![Stmt::While(
                condition,
                Box::new(increment)
            )]),

        */

        //let empty_body = Stmt::Block(vec![]);
        //let mut body = Stmt::Block(vec![increment]);
        //let mut body = Stmt::Block(vec![empty_body, increment]);
        //let mut body = Stmt::Block(vec![empty_body]);
        //body = Stmt::Block(vec![var_assign_stmt, body]);
        /*
           for (var i = 0; i < 2; i = i + 1) {
           }

           ----------


           {
           var-assign;
            {

                While(
                condition,
                    {
                        {
                            empty body
                        };
                        increment;
                    }
                );
            };
           };
        */

        let res = resolver.resolve_stmt(&body);
        assert!(res.is_ok(), "{}", res.unwrap_err());

        assert!(interpreter.interpret(vec![body]));
    }
}
