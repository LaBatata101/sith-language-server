#[derive(Clone, Copy, Debug)]
pub struct Position {
    pub read_pos: usize,
    pub row: u32,
    pub column: u32,
}

impl Position {
    pub fn new() -> Self {
        Self {
            read_pos: 0,
            row: 1,
            column: 1,
        }
    }

    pub fn go_right(&mut self, offset: u32) {
        self.column += offset;
    }

    pub fn new_line(&mut self) {
        self.row += 1;
        self.column = 1;
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Default)]
pub struct Span {
    pub row_start: u32,
    pub row_end: u32,
    pub column_start: u32,
    pub column_end: u32,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self {
            row_start: start.row,
            row_end: end.row,
            column_start: start.column,
            column_end: end.column,
        }
    }
}
