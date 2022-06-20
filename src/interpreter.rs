use crate::error::ErrorMessage;
use crate::expr::Expr;
use crate::expr::Visitor;
use crate::token::TokenKind;
use crate::token::{LiteralKind, Token};
use std::error;
use std::fmt;

/// This somewhat duplicated tokens::LiteralKind but eschews the identifier and None type
#[derive(Debug, PartialEq)]
pub enum LoxValue {
    // Possible value types in lox
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
}

impl LoxValue {
    /// Follows ruby rules where everything is truthy except false and nil
    pub fn is_truthy(&self) -> bool {
        match &self {
            LoxValue::String(_) => true,
            LoxValue::Number(_) => true,
            LoxValue::Bool(b) => b.to_owned(),
            LoxValue::Nil => false,
        }
    }
    pub fn as_number(self, line_number: usize) -> Result<f64, EvalError> {
        match self {
            LoxValue::Number(n) => Ok(n),
            _ => Err(EvalError::InvalidType(
                line_number,
                format!("Could not convert {:?} to a double", &self,),
            )),
        }
    }
}

#[derive(Debug)]
pub enum EvalError {
    UnreachableError(ErrorMessage),
    // Line number, error msg
    InvalidType(usize, ErrorMessage),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            EvalError::UnreachableError(e) => write!(f, "UnreachableError: {}", e),
            EvalError::InvalidType(l, e) => write!(f, "InvalidType: On line {}, message: {}", l, e),
        }
    }
}

impl error::Error for EvalError {}

pub struct Interpreter {}

impl Interpreter {
    pub fn evaluate(&mut self, expr: &Expr) -> Result<LoxValue, EvalError> {
        expr.accept(self)
    }
    pub fn new() -> Interpreter {
        Interpreter {}
    }
}

/// Possibly should switch this to an Option or Result
impl Visitor<Result<LoxValue, EvalError>> for Interpreter {
    fn visit_binary(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> Result<LoxValue, EvalError> {
        let (left, right) = (self.evaluate(lhs)?, self.evaluate(rhs)?);
        let line = op.line;

        match op.kind {
            // Arithmetic operations on numbers
            TokenKind::Minus => Ok(LoxValue::Number(left.as_number(line)? - right.as_number(line)?)),
            TokenKind::Slash => Ok(LoxValue::Number(left.as_number(line)? / right.as_number(line)?)),
            TokenKind::Star => Ok(LoxValue::Number(left.as_number(line)? * right.as_number(line)?)),

            // Comparisons
            TokenKind::Greater => Ok(LoxValue::Bool(left.as_number(line)? > right.as_number(line)?)),
            TokenKind::GreaterEqual => Ok(LoxValue::Bool(left.as_number(line)? >= right.as_number(line)?)),
            TokenKind::Less => Ok(LoxValue::Bool(left.as_number(line)? < right.as_number(line)?)),
            TokenKind::LessEqual => Ok(LoxValue::Bool(left.as_number(line)? <= right.as_number(line)?)),

            // Aithmetic and concat operations
            TokenKind::Plus => match (left, right) {
                // Handle string concatenation
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
        if let Expr::Grouping(g) = expr {
            self.evaluate(g)
        } else {
            Err(EvalError::UnreachableError(format!(
                "Expected grouping but found {expr:?}"
            )))
        }
    }

    fn visit_literal(&mut self, value: &LiteralKind) -> Result<LoxValue, EvalError> {
        match value {
            LiteralKind::String(s) => Ok(LoxValue::String(s.clone())),
            LiteralKind::Number(n) => Ok(LoxValue::Number(n.clone())),
            LiteralKind::Bool(b) => Ok(LoxValue::Bool(b.clone())),
            LiteralKind::Nil => Ok(LoxValue::Nil),
            _ => Err(EvalError::UnreachableError(format!(
                "Expected literal but found {value:?}"
            ))),
        }
    }

    fn visit_unary(&mut self, op: &Token, right: &Expr) -> Result<LoxValue, EvalError> {
        // evaluate the rhs of the unary
        let rhs_value = if let Expr::Unary(_, right) = right {
            self.evaluate(right)
        } else {
            Err(EvalError::UnreachableError(format!(
                "Expected Unary but found {right:?}"
            )))
        }?;

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
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::Expr;

    fn number_expr(n: f64) -> Box<Expr> {
        Box::new(Expr::Literal(LiteralKind::Number(n)))
    }

    fn string_expr(s: &str) -> Box<Expr> {
        Box::new(Expr::Literal(LiteralKind::String(s.to_owned())))
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
}
