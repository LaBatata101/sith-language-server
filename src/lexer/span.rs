/// This represents the position in the character stream.
/// - `index` is the current position within the character stream.
/// - `row` is the current row number within the character stream.
/// - `column` is the current column number within the character stream.
#[derive(PartialEq, Clone, Copy, Debug)]
pub struct Position {
    pub index: u32,
    pub row: u32,
    pub column: u32,
}

impl Default for Position {
    fn default() -> Self {
        Self {
            index: 0,
            row: 1,
            column: 0,
        }
    }
}

impl Position {
    pub fn go_right(&mut self, offset: u32) {
        self.column += offset;
    }

    pub fn new_line(&mut self) {
        self.row += 1;
        self.column = 0;
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default)]
pub struct Span {
    pub row_start: u32,
    pub row_end: u32,
    pub column_start: u32,
    pub column_end: u32,
}
