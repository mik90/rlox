use crate::error::LoxError;
use crate::expr::Expr;
use crate::token::{LiteralType, Token, TokenType};

pub struct Parser {
    tokens: Vec<Token>,
    cur_idx: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, cur_idx: 0 }
    }

    fn consume(&mut self, token_type: &TokenType, msg: &str) -> Result<(), LoxError> {
        todo!()
    }
    /// Grammar rule: primary -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    fn primary(&mut self) -> Result<Expr, LoxError> {
        if self.token_matches(&[&TokenType::False])? {
            return Ok(Expr::Literal(LiteralType::Bool(false)));
        }
        if self.token_matches(&[&TokenType::True])? {
            return Ok(Expr::Literal(LiteralType::Bool(true)));
        }
        if self.token_matches(&[&TokenType::Nil])? {
            return Ok(Expr::Literal(LiteralType::Nil));
        }
        if self.token_matches(&[&TokenType::Number, &TokenType::String])? {
            return Ok(Expr::Literal(self.previous()?.literal));
        }
        if self.token_matches(&[&TokenType::LeftParen])? {
            let expr = self.expression();
            // TODO just throw in msg here
            self.consume(&TokenType::RightParen, "Expect ')' after expression.")?;
            return Ok(Expr::Grouping(Box::from(expr)));
        }
        Err(LoxError::SyntaxError(
            "Could not parse grammar: primary".to_string(),
        ))
    }

    /// Grammar rule: unary -> ( "!" | "-" ) unary | primary ;
    fn unary(&mut self) -> Result<Expr, LoxError> {
        if self.token_matches(&[&TokenType::Bang, &TokenType::Minus])? {
            let operator = self.previous()?;
            let right = self.unary()?;
            return Ok(Expr::Unary(operator, Box::from(right)));
        }
        self.primary()
    }

    /// Grammar rule: factor -> unary ( ( "/" | "*" ) unary )* ;
    fn factor(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.unary()?;

        while self.token_matches(&[&TokenType::Slash, &TokenType::Star])? {
            let operator = self.previous()?;
            let right = self.unary()?;
            expr = Expr::Binary(Box::from(expr), operator, Box::from(right));
        }
        Ok(expr)
    }

    /// Grammar rule: term -> factor ( ( "-" | "+" ) factor)* ;
    fn term(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.factor()?;

        while self.token_matches(&[&TokenType::Minus, &TokenType::Plus])? {
            let operator = self.previous()?;
            let right = self.factor()?;
            expr = Expr::Binary(Box::from(expr), operator, Box::from(right));
        }
        Ok(expr)
    }

    /// Grammar rule: comparison -> term ( ( ">" | ">=" | "<" | "<=") term)* ;
    fn comaprison(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.term()?;

        while self.token_matches(&[
            &TokenType::Greater,
            &TokenType::GreaterEqual,
            &TokenType::Less,
            &TokenType::LessEqual,
        ])? {
            let operator = self.previous()?;
            let right = self.term()?;
            expr = Expr::Binary(Box::from(expr), operator, Box::from(right));
        }
        Ok(expr)
    }

    /// Grammar rule: equality -> comparison ( ("!=" | "==") comparison )* ;
    /// Matches an equality or anything of higher precedence (ex: comparison)
    fn equality(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.comaprison()?;
        // This pattern could be shared somehow
        while self.token_matches(&[&TokenType::BangEqual, &TokenType::EqualEqual])? {
            let operator = self.previous()?;
            let right = self.comaprison()?;
            // TODO Don't allocate in a hot loop. This is a bit too 1:1 with the java impl for my comfort
            // This creates an expression including itself if there are multiple equality comparisons
            expr = Expr::Binary(Box::from(expr), operator, Box::from(right));
        }
        Ok(expr)
    }
    fn previous(&mut self) -> Result<Token, LoxError> {
        self.tokens
            .iter()
            .nth(self.cur_idx - 1)
            .ok_or(LoxError::ParseError(format!(
                "cur_idx={} it out of range of tokens.len()={}, you may be missing an EOF token",
                self.cur_idx,
                self.tokens.len()
            )))
            .cloned()
    }

    fn peek(&self) -> Result<Token, LoxError> {
        // TODO not very safe :(
        self.tokens
            .iter()
            .nth(self.cur_idx)
            .ok_or(LoxError::ParseError(format!(
                "cur_idx={} is out of range of tokens.len={}, you may be missing an EOF token",
                self.cur_idx,
                self.tokens.len()
            )))
            .cloned()
    }

    /// Checks that the current token being parsed is of a given type
    fn check(&self, token_type: &TokenType) -> Result<bool, LoxError> {
        if self.is_at_end()? {
            return Ok(false);
        }
        Ok(&self.peek()?.kind == token_type)
    }

    fn is_at_end(&self) -> Result<bool, LoxError> {
        Ok(self.peek()?.kind == TokenType::Eof)
    }

    /// Consumes a token, returns the previous even if we're at the end
    fn advance(&mut self) -> Result<Token, LoxError> {
        if !self.is_at_end()? {
            self.cur_idx += 1;
        }
        self.previous()
    }

    /// Return true if the token matches any of the given types
    fn token_matches(&mut self, token_types: &[&TokenType]) -> Result<bool, LoxError> {
        for token_type in token_types {
            if self.check(token_type)? {
                self.advance()?;
                return Ok(true);
            }
        }
        return Ok(false);
    }

    fn expression(&mut self) -> Expr {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::Expr;
    use crate::LiteralType;
    use crate::Token;
    use crate::TokenType;
    #[test]
    fn equality_test() {
        let line = 1;
        let tokens = vec![
            Token::new_literal(LiteralType::Number(5.0), line),
            Token::new(
                TokenType::EqualEqual,
                "==".to_string(),
                LiteralType::None,
                line,
            ),
            Token::new_literal(LiteralType::Number(1.0), line),
            Token::new(TokenType::Eof, "EOF".to_string(), LiteralType::None, line),
        ];
        let mut parser = Parser::new(tokens);
        match parser.equality() {
            Ok(e) => match e {
                Expr::Binary(lhs, op, rhs) => {
                    assert_eq!(*lhs, Expr::Literal(LiteralType::Number(5.0)));
                    assert_eq!(op.kind, TokenType::EqualEqual);
                    assert_eq!(*rhs, Expr::Literal(LiteralType::Number(1.0)));
                }
                e => assert!(
                    false,
                    "Expected equality to return Expr::Binary and not {:?}",
                    e
                ),
            },
            Err(e) => assert!(false, "{:?}", e),
        }
    }
}
