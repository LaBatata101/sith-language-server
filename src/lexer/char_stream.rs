use super::span::Position;

pub struct CharStream<'a> {
    text: &'a [u8],
    index: u32,
    is_eof: bool,
    pos: Position,
}

impl<'a> CharStream<'a> {
    pub fn new(text: &'a str) -> Self {
        let text = text.as_bytes();

        Self {
            index: 0,
            text,
            is_eof: text.is_empty(),
            pos: Position::default(),
        }
    }

    pub fn current_char(&self) -> Option<char> {
        self.text.get(self.index as usize).map(|&byte| byte as char)
    }

    pub fn next_char(&self) -> Option<char> {
        self.text.get(self.index as usize + 1).map(|&byte| byte as char)
    }

    pub fn peek_char(&self, offset: u32) -> Option<char> {
        self.text.get(offset as usize).map(|&byte| byte as char)
    }

    pub fn get_slice(&self, start: u32, end: u32) -> Option<&'a [u8]> {
        self.text.get(start as usize..end as usize)
    }

    pub fn advance_by(&mut self, offset: u32) {
        if self.is_at_eol().is_some() {
            self.pos.new_line();
        } else {
            self.pos.go_right(offset);
        }

        self.index += offset;
        self.pos.index = self.index;

        if self.index as usize >= self.text.len() {
            self.is_eof = true;
        }
    }

    pub fn is_eof(&self) -> bool {
        self.is_eof
    }

    pub fn pos(&self) -> Position {
        self.pos
    }

    /// Skip ASCII whitespace and return the total amount skiped
    pub fn skip_whitespace(&mut self) -> usize {
        let mut whitespace_total = 0;
        while !self.is_eof() && matches!(self.current_char(), Some(' ' | '\t' | '\u{0c}')) {
            whitespace_total += 1;
            self.advance_by(1)
        }

        whitespace_total
    }

    /// Checks if the current char is EOL (\n, \r or \r\n).
    /// Returns the size of the EOL character, otherwise return `None`.
    pub fn is_at_eol(&self) -> Option<u32> {
        match (self.current_char(), self.next_char()) {
            (Some('\r'), Some('\n')) => Some(2),
            (Some('\n' | '\r'), _) => Some(1),
            _ => None,
        }
    }

    pub fn advance_while<F>(&mut self, step: u32, cond: F)
    where
        F: Fn(char) -> bool,
    {
        while self.current_char().map_or(false, &cond) {
            self.advance_by(step);
        }
    }
}
