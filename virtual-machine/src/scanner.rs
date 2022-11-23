use core::iter::Enumerate;
use std::str::Chars;

pub struct Scanner<'a> {
    start: Enumerate<Chars<'a>>,
    current: Enumerate<Chars<'a>>,
    line: usize,
}

#[derive(PartialEq, Debug)]
pub enum TokenKind {
    Eof,
    Other,
}

pub struct Token<'a> {
    pub line: usize,
    pub start: Enumerate<Chars<'a>>,
    pub length: usize,
    pub kind: TokenKind,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            start: source.chars().into_iter().enumerate(),
            current: source.chars().into_iter().enumerate(),
            line: 1,
        }
    }
    pub fn scan_token(&mut self) -> Token {
        todo!()
    }
}
