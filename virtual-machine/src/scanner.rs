use core::iter::Enumerate;
use std::str::Chars;

pub struct Scanner<'a> {
    start: Enumerate<Chars<'a>>,
    current: Enumerate<Chars<'a>>,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            start: source.chars().into_iter().enumerate(),
            current: source.chars().into_iter().enumerate(),
            line: 1,
        }
    }
}
