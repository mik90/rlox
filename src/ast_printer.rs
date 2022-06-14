use std::fmt::format;

use crate::expr::{Expr, Visitor};
use crate::token::{LiteralType, Token};

pub struct AstPrinter {}

impl AstPrinter {
    pub fn print(&mut self, expr: &Expr) -> String {
        expr.accept(self)
    }
    pub fn new() -> AstPrinter {
        AstPrinter {}
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
        value.to_string()
    }

    fn visit_unary(&mut self, op: &Token, right: &Expr) -> String {
        parenthisize(self, &op.text, &[&right])
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::LiteralType;
    use crate::TokenType;

    #[test]
    fn print_ast() {
        let token_1 = Token::new(TokenType::Minus, "-".to_string(), LiteralType::None, 1);
        let token_2 = Token::new(TokenType::Star, "*".to_string(), LiteralType::None, 1);
        let literal_1 = Expr::Literal(LiteralType::Number(123.0));
        let literal_2 = Expr::Literal(LiteralType::Number(45.67));
        let expr = Expr::Binary(
            Box::new(Expr::Unary(token_1, Box::new(literal_1))),
            token_2,
            Box::new(Expr::Grouping(Box::new(literal_2))),
        );
        let mut printer = AstPrinter::new();
        let text = printer.print(&expr);
        assert_eq!(text, "(* (- 123) (group 45.67))");
    }
}
