pub mod types;

use self::types::TokenType;

#[derive(Debug, PartialEq, Eq)]
pub struct Span {
    // TODO: add field to know which file the Span belongs to
    start: usize,
    end: usize,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Token {
    pub kind: TokenType,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenType, start: usize, end: usize) -> Self {
        Self {
            kind,
            span: Span { start, end },
        }
    }
}
