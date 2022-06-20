use crate::error::LoxError;
use crate::expr::Expr;
use crate::token::{LiteralKind, Token, TokenKind};

pub struct Parser {
    tokens: Vec<Token>,
    cur_idx: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, cur_idx: 0 }
    }
    pub fn parse(&mut self) -> Option<Expr> {
        match self.expression() {
            Ok(v) => Some(v),
            Err(e) => {
                eprint!("Could not parse input. {}", e);
                return None;
            }
        }
    }

    /// Discard tokens until we reach a new synchronization point
    /// Useful for using after a syntax error occurs
    fn synchronize(&mut self) -> Result<(), LoxError> {
        self.advance()?;
        while !self.is_at_end()? {
            if self.previous()?.kind == TokenKind::SemiColon {
                return Ok(());
            }
            match self.peek()?.kind {
                // Any of these are synchronization points
                TokenKind::Class
                | TokenKind::For
                | TokenKind::Fun
                | TokenKind::If
                | TokenKind::Print
                | TokenKind::Return
                | TokenKind::Var
                | TokenKind::While => return Ok(()),
                // Not at a synchronization point, keep advancign
                _ => (),
            }
            self.advance()?;
        }
        Ok(())
    }

    fn consume(&mut self, token_type: &TokenKind, msg: &str) -> Result<Token, LoxError> {
        if self.check(token_type)? {
            return self.advance();
        }
        Err(LoxError::new_parser_err(self.peek()?, msg))
    }
    /// Grammar rule: primary -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    fn primary(&mut self) -> Result<Expr, LoxError> {
        if self.token_matches(&[&TokenKind::False])? {
            return Ok(Expr::Literal(LiteralKind::Bool(false)));
        }
        if self.token_matches(&[&TokenKind::True])? {
            return Ok(Expr::Literal(LiteralKind::Bool(true)));
        }
        if self.token_matches(&[&TokenKind::Nil])? {
            return Ok(Expr::Literal(LiteralKind::Nil));
        }
        if self.token_matches(&[&TokenKind::Number, &TokenKind::String])? {
            return Ok(Expr::Literal(self.previous()?.literal));
        }
        if self.token_matches(&[&TokenKind::LeftParen])? {
            let expr = self.expression()?;
            // TODO just throw in msg here
            self.consume(&TokenKind::RightParen, "Expect ')' after expression.")?;
            return Ok(Expr::Grouping(Box::from(expr)));
        }
        Err(LoxError::new_parser_err(
            self.peek()?,
            "Expected expression but did not find one.",
        ))
    }

    /// Grammar rule: unary -> ( "!" | "-" ) unary | primary ;
    fn unary(&mut self) -> Result<Expr, LoxError> {
        if self.token_matches(&[&TokenKind::Bang, &TokenKind::Minus])? {
            let operator = self.previous()?;
            let right = self.unary()?;
            return Ok(Expr::Unary(operator, Box::from(right)));
        }
        self.primary()
    }

    /// Grammar rule: factor -> unary ( ( "/" | "*" ) unary )* ;
    fn factor(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.unary()?;

        while self.token_matches(&[&TokenKind::Slash, &TokenKind::Star])? {
            let operator = self.previous()?;
            let right = self.unary()?;
            expr = Expr::Binary(Box::from(expr), operator, Box::from(right));
        }
        Ok(expr)
    }

    /// Grammar rule: term -> factor ( ( "-" | "+" ) factor)* ;
    fn term(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.factor()?;

        while self.token_matches(&[&TokenKind::Minus, &TokenKind::Plus])? {
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
            &TokenKind::Greater,
            &TokenKind::GreaterEqual,
            &TokenKind::Less,
            &TokenKind::LessEqual,
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
        while self.token_matches(&[&TokenKind::BangEqual, &TokenKind::EqualEqual])? {
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
            .ok_or(LoxError::ScanError(format!(
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
            .ok_or(LoxError::ScanError(format!(
                "cur_idx={} is out of range of tokens.len={}, you may be missing an EOF token",
                self.cur_idx,
                self.tokens.len()
            )))
            .cloned()
    }

    /// Checks that the current token being parsed is of a given type
    fn check(&self, token_type: &TokenKind) -> Result<bool, LoxError> {
        if self.is_at_end()? {
            return Ok(false);
        }
        Ok(&self.peek()?.kind == token_type)
    }

    fn is_at_end(&self) -> Result<bool, LoxError> {
        Ok(self.peek()?.kind == TokenKind::Eof)
    }

    /// Consumes a token, returns the previous even if we're at the end
    fn advance(&mut self) -> Result<Token, LoxError> {
        if !self.is_at_end()? {
            self.cur_idx += 1;
        }
        self.previous()
    }

    /// Return true if the token matches any of the given types
    fn token_matches(&mut self, token_types: &[&TokenKind]) -> Result<bool, LoxError> {
        for token_type in token_types {
            if self.check(token_type)? {
                self.advance()?;
                return Ok(true);
            }
        }
        return Ok(false);
    }

    /// Grammar rules: expression -> equality ;
    fn expression(&mut self) -> Result<Expr, LoxError> {
        self.equality()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::scanner::Scanner;
    use crate::Expr;
    use crate::LiteralKind;
    use crate::Token;
    use crate::TokenKind;
    #[test]
    fn equality_test() {
        let line = 1;
        let tokens = vec![
            Token::new_literal(LiteralKind::Number(5.0), line),
            Token::new(TokenKind::EqualEqual, "==".to_string(), line),
            Token::new_literal(LiteralKind::Number(1.0), line),
            Token::new(TokenKind::Eof, "EOF".to_string(), line),
        ];
        let mut parser = Parser::new(tokens);
        match parser.equality() {
            Ok(e) => match e {
                Expr::Binary(lhs, op, rhs) => {
                    assert_eq!(*lhs, Expr::Literal(LiteralKind::Number(5.0)));
                    assert_eq!(op.kind, TokenKind::EqualEqual);
                    assert_eq!(*rhs, Expr::Literal(LiteralKind::Number(1.0)));
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

    #[test]
    fn parse_tokens_from_scanner() {
        let input = "1 * 2 + (3 - 5)".to_string();
        let mut scan = Scanner::new(input);
        let res = scan.scan_tokens();
        assert!(res.is_ok(), "{}", res.unwrap_err().to_string());

        let tokens = scan.copy_tokens();

        // This is where the tokens are actually tested
        let mut parser = Parser::new(tokens);
        let expr = parser.parse();
        assert!(expr.is_some());
    }

    #[test]
    fn parse_invalid_tokens_from_scanner() {
        let input = "*( 2 + (3 - 5)".to_string();
        let mut scan = Scanner::new(input);
        let res = scan.scan_tokens();
        assert!(res.is_ok(), "{}", res.unwrap_err().to_string());

        let tokens = scan.copy_tokens();

        // This is where the tokens are actually tested
        let mut parser = Parser::new(tokens);
        let expr = parser.parse();
        assert!(expr.is_none());
    }
}
