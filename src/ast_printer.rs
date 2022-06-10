use std::fmt::format;

use crate::expr::{Expr, Visitor};
use crate::token::{LiteralType, Token};

pub struct AstPrinter {}

impl AstPrinter {
    pub fn print(&mut self, expr: &Expr) -> String {
        expr.accept(self)
    }
}

fn parenthisize(visitor: &mut dyn Visitor<String>, name: &str, exprs: &[&Expr]) -> String {
    let mut ast = format!("({}", name);

    for expr in exprs {
        ast.push(' ');
        let value = expr.accept(visitor);
        ast.push_str(&value);
    }
    ast.push(')');
    ast
}

impl Visitor<String> for AstPrinter {
    fn visit_binary(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> String {
        parenthisize(self, &op.text, &[lhs, rhs])
    }

    fn visit_grouping(&mut self, expr: &Expr) -> String {
        parenthisize(self, "group", &[&expr])
    }

    fn visit_literal(&mut self, value: &LiteralType) -> String {
        match value {
            LiteralType::Identifier(i) => i.to_string(),
            LiteralType::String(s) => s.to_string(),
            LiteralType::Number(n) => n.to_string(),
            LiteralType::None => "nil".to_string(),
        }
    }

    fn visit_unary(&mut self, op: &Token, right: &Expr) -> String {
        parenthisize(self, &op.text, &[&right])
    }
}
