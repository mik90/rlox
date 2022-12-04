use std::fmt;

#[derive(Debug, Clone)]
pub enum LiteralKind {
    Identifier(String),
    String(String),
    Number(f64), // <-- all numbers are floating point at rutnime
    Bool(bool),
    Nil,
    None,
}

impl ToString for LiteralKind {
    fn to_string(&self) -> String {
        match &self {
            LiteralKind::Identifier(identifier) => identifier.clone(),
            LiteralKind::String(string) => string.clone(),
            LiteralKind::Number(num) => num.to_string(),
            LiteralKind::None => "nil".to_string(),
            LiteralKind::Bool(b) => b.to_string(),
            LiteralKind::Nil => "nil".to_string(),
        }
    }
}

impl std::hash::Hash for LiteralKind {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            LiteralKind::Identifier(v) => v.hash(state),
            LiteralKind::String(v) => v.hash(state),
            // just hash the raw bites for this
            LiteralKind::Number(v) => v.to_bits().hash(state),
            LiteralKind::Bool(v) => v.hash(state),
            LiteralKind::Nil => "".hash(state),
            LiteralKind::None => "".hash(state),
        }
    }
}

// This may need more elaboration
impl std::cmp::PartialEq for LiteralKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Identifier(l0), Self::Identifier(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}
impl std::cmp::Eq for LiteralKind {}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TokenKind {
    // Single-char tokenInto<String>s
    LeftParen = 0,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    SemiColon,
    Slash,
    Star,

    // one or two-char tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    // TODO this duplicates 'lexeme', and LiteralToken should be an optional
    pub literal: LiteralKind,
    pub line: usize,
}

impl Eq for Token {}
impl PartialEq for Token {
    // Although tokens can technically be on different lines and be separate tokens, we only really care about their context
    // This is needed in order to make the token hashable
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.lexeme == other.lexeme && self.literal == other.literal
    }
}

impl std::hash::Hash for Token {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
        self.lexeme.hash(state);
        self.literal.hash(state);
        // Don't include the line number in the hash since an Expr can be referring to the same thing on different lines
    }
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: String, line: usize) -> Token {
        Token {
            kind,
            lexeme,
            literal: LiteralKind::None,
            line,
        }
    }
    /// Eugh, LiteralKind of None returns an EOF token
    /// Entails copying a string although we would have to copy it anyways from the slice of input
    pub fn new_literal(literal: LiteralKind, line: usize) -> Token {
        match &literal {
            LiteralKind::Identifier(i) => Token {
                kind: TokenKind::Identifier,
                lexeme: i.clone(),
                literal,
                line,
            },
            LiteralKind::String(s) => Token {
                kind: TokenKind::String,
                lexeme: s.clone(),
                literal,
                line,
            },
            LiteralKind::Number(n) => Token {
                kind: TokenKind::Number,
                lexeme: n.to_string(),
                literal,
                line,
            },
            LiteralKind::None => Token {
                kind: TokenKind::Eof,
                lexeme: literal.to_string(),
                literal,
                line,
            },
            LiteralKind::Bool(b) => {
                let kind = if *b {
                    TokenKind::True
                } else {
                    TokenKind::False
                };
                Token {
                    kind,
                    lexeme: b.to_string(),
                    literal,
                    line,
                }
            }
            LiteralKind::Nil => Token {
                kind: TokenKind::Nil,
                lexeme: literal.to_string(),
                literal,
                line,
            },
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} on line {}", self.lexeme, self.line)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::collections::hash_map::{DefaultHasher, HashMap};
    use std::hash::{Hash, Hasher};

    fn check_hash_and_value_equality(a: LiteralKind, b: LiteralKind) {
        let mut hasher = DefaultHasher::new();
        let hash_a = a.hash(&mut hasher);
        hasher.finish();
        let hash_b = b.hash(&mut hasher);
        hasher.finish();
        assert_eq!(hash_a, hash_b);
    }

    /// https://rust-lang.github.io/rust-clippy/master/index.html#derive_hash_xor_eq
    /// Is there a way to fuzz over this or just define a type to use to test with?
    #[test]
    fn equality_hash_value() {
        {
            let val_0 = LiteralKind::Identifier("i".to_string());
            let val_1 = LiteralKind::Identifier("i".to_string());
            check_hash_and_value_equality(val_0, val_1);
        }
        {
            let val_0 = LiteralKind::String("hello".to_string());
            let val_1 = LiteralKind::String("hello".to_string());
            check_hash_and_value_equality(val_0, val_1);
        }
        {
            let val_0 = LiteralKind::Number(999.0);
            let val_1 = LiteralKind::Number(999.0);
            check_hash_and_value_equality(val_0, val_1);
        }
        {
            let val_0 = LiteralKind::Bool(true);
            let val_1 = LiteralKind::Bool(true);
            check_hash_and_value_equality(val_0, val_1);
        }
        {
            let val_0 = LiteralKind::Nil;
            let val_1 = LiteralKind::Nil;
            check_hash_and_value_equality(val_0, val_1);
        }
        {
            let val_0 = LiteralKind::None;
            let val_1 = LiteralKind::None;
            check_hash_and_value_equality(val_0, val_1);
        }
    }

    #[test]
    fn token_hash() {
        let literal = LiteralKind::Identifier("i".to_string());

        let i_3 = Token::new_literal(literal.clone(), 3);
        let i_4 = Token::new_literal(literal.clone(), 4);

        let mut hasher = DefaultHasher::new();
        i_3.hash(&mut hasher);
        let hash_i_3 = hasher.finish();

        let mut hasher = DefaultHasher::new();
        i_4.hash(&mut hasher);
        let hash_i_4 = hasher.finish();
        assert_eq!(hash_i_3, hash_i_4);
    }

    #[test]
    fn token_hash_map() {
        let mut map = HashMap::<Token, usize>::new();

        let literal = LiteralKind::Identifier("i".to_string());
        let i_3 = Token::new_literal(literal.clone(), 3);
        let i_4 = Token::new_literal(literal, 4);

        map.insert(i_3.clone(), 0);
        assert!(map.contains_key(&i_3));

        assert!(
            map.contains_key(&i_4),
            "The same expr w/ diff line numbers should be interchangeable in the map"
        );
    }
}
