use std::ops::Range;

pub struct CharStream<'a> {
    text: &'a [u8],
    pos: usize,
    is_eof: bool,
}

impl<'a> CharStream<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            pos: 0,
            text: text.as_bytes(),
            is_eof: text.is_empty(),
        }
    }

    pub fn current_char(&self) -> Option<char> {
        self.text.get(self.pos).map(|&byte| byte as char)
    }

    pub fn next_char(&self) -> Option<char> {
        self.text.get(self.pos + 1).map(|&byte| byte as char)
    }

    pub fn peek_char(&self, offset: usize) -> Option<char> {
        self.text.get(offset).map(|&byte| byte as char)
    }

    pub fn get_slice(&self, range: Range<usize>) -> Option<&[u8]> {
        self.text.get(range)
    }

    pub fn advance_by(&mut self, offset: usize) {
        self.pos += offset;

        if self.pos >= self.text.len() {
            self.is_eof = true;
        }
    }

    pub fn is_eof(&self) -> bool {
        self.is_eof
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    /// Skip ASCII whitespace and return the total amount skiped
    pub fn skip_whitespace(&mut self) -> usize {
        let mut whitespace_total = 0;
        while !self.is_eof() && self.current_char().map_or(false, |char| char == ' ') {
            whitespace_total += 1;
            self.advance_by(1)
        }

        whitespace_total
    }
}
