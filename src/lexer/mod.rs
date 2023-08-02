mod helpers;
pub mod span;
pub mod token;

use token::Token;

use unicode_xid::UnicodeXID;

use crate::error::{PythonError, PythonErrorType};

use self::{
    helpers::{
        convert_byte_to_unicode_codepoint, is_binary_number_valid, is_char_operator, is_decimal_number_valid, is_eol,
        is_float_number_valid, is_hex_number_valid, is_octal_number_valid, is_string_prefix, is_whitespace,
        unicode_char_size,
    },
    span::{Position, Span},
    token::types::{IntegerType, KeywordType, NumberType, OperatorType, SoftKeywordType, TokenType},
};

pub struct Lexer<'src> {
    text: &'src str,
    text_bytes: &'src [u8],
    current_pos: Position,

    is_at_beginning_of_line: bool,
    /// Use to track the indentations levels. Always has one indentation level
    /// like the CPython implementation.
    indent_stack: Vec<usize>,

    /// Used to check wheter we are inside a `[]`, `()` or `{}`, and skip the `NewLine` token creation.
    implicit_line_joining: i32,

    errors: Vec<PythonError>,
}

// FIXME: handle mix of tabs and spaces
impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            text: src,
            errors: vec![],
            is_at_beginning_of_line: true,
            indent_stack: vec![0],
            implicit_line_joining: 0,
            text_bytes: src.as_bytes(),
            current_pos: Position::new(),
        }
    }

    fn add_error(&mut self, ty: PythonErrorType, msg: String, span: Span) {
        self.errors.push(PythonError { error: ty, msg, span });
    }

    #[inline(always)]
    fn is_eof(&self) -> bool {
        self.current_pos.read_pos >= self.text_bytes.len()
    }

    /// Consumes the following whitespace characters: ' ', '\t' and '\x0C'.
    fn consume_whitespace(&mut self) {
        self.consume_while(is_whitespace);
    }

    /// Advances the current position by the specified `offset` and returns the original
    /// position and the updated position as a tuple.
    fn advance_pos(&mut self, offset: usize) -> (Position, Position) {
        let start = self.current_pos;

        self.current_pos.read_pos += offset;
        self.current_pos
            .go_right(offset.try_into().expect("offset to big to fit into u32!"));

        (start, self.current_pos)
    }

    /// Consumes the specified number of characters by the specified `total` from the input
    /// and returns a token of the specified [`TokenType`] with the corresponding span.
    fn consume_n_chars(&mut self, total: usize, token_kind: TokenType<'src>) -> Token<'src> {
        let (start, end) = self.advance_pos(total);

        Token {
            kind: token_kind,
            span: Span::new(start, end),
        }
    }

    /// Consumes a single character from the input and returns a token of the specified
    /// [`TokenType`] with the corresponding span.
    fn consume_single_char(&mut self, token_kind: TokenType<'src>) -> Token<'src> {
        self.consume_n_chars(1, token_kind)
    }

    /// Consumes one character at a time while `predicate` returns `true`.
    /// Returns the original position before consuming and the updated position after
    /// consuming as a tuple.
    fn consume_while<F>(&mut self, predicate: F) -> (Position, Position)
    where
        F: Fn(u8) -> bool,
    {
        self.consume_while_with_offset(1, predicate)
    }

    /// Consumes `total` characters while `predicate` returns `true`.
    /// Returns the original position before consuming and the updated position after
    /// consuming as a tuple.
    fn consume_while_with_offset<F>(&mut self, total: usize, predicate: F) -> (Position, Position)
    where
        F: Fn(u8) -> bool,
    {
        let start = self.current_pos;
        while !self.is_eof() && predicate(self.text_bytes[self.current_pos.read_pos]) {
            self.advance_pos(total);
        }
        let end = self.current_pos;

        (start, end)
    }

    pub fn tokenize(mut self) -> (Vec<Token<'src>>, Vec<PythonError>) {
        let mut tokens = Vec::new();

        while !self.is_eof() {
            if self.is_at_beginning_of_line && self.implicit_line_joining == 0 {
                tokens.extend(self.handle_indentation());
            }

            let Some(&char_byte) = self.text_bytes.get( self.current_pos.read_pos ) else {
                break;
            };

            let token = if char_byte.is_ascii_digit() {
                self.consume_number()
            } else if char_byte == b'"' || char_byte == b'\'' {
                self.consume_string()
            } else if char_byte == b'(' {
                self.implicit_line_joining += 1;
                self.consume_single_char(TokenType::OpenParenthesis)
            } else if char_byte == b')' {
                self.implicit_line_joining -= 1;
                self.consume_single_char(TokenType::CloseParenthesis)
            } else if char_byte == b'[' {
                self.implicit_line_joining += 1;
                self.consume_single_char(TokenType::OpenBrackets)
            } else if char_byte == b']' {
                self.implicit_line_joining -= 1;
                self.consume_single_char(TokenType::CloseBrackets)
            } else if char_byte == b'{' {
                self.implicit_line_joining += 1;
                self.consume_single_char(TokenType::OpenBrace)
            } else if char_byte == b'}' {
                self.implicit_line_joining -= 1;
                self.consume_single_char(TokenType::CloseBrace)
            } else if char_byte == b';' {
                self.consume_single_char(TokenType::SemiColon)
            } else if char_byte == b',' {
                self.consume_single_char(TokenType::Comma)
            } else if char_byte == b'#' {
                self.consume_comment();
                continue;
            } else if char_byte == b'\\' {
                self.handle_explicit_line_join();
                continue;
            } else if is_char_operator(char_byte) {
                if char_byte == b'-' && self.text_bytes.get(self.current_pos.read_pos + 1) == Some(&b'>') {
                    self.consume_n_chars(2, TokenType::RightArrow)
                } else {
                    self.consume_operator()
                }
            } else if is_whitespace(char_byte) {
                self.consume_whitespace();
                continue;
            } else if is_eol(char_byte) {
                let eol_token = self.consume_eol();
                if self.implicit_line_joining > 0 {
                    continue;
                }

                self.is_at_beginning_of_line = true;
                eol_token
            } else if char_byte == b':' {
                if let Some(b'=') = self.text_bytes.get(self.current_pos.read_pos + 1) {
                    self.consume_n_chars(2, TokenType::Operator(OperatorType::ColonEqual))
                } else {
                    self.consume_single_char(TokenType::Colon)
                }
            } else if char_byte == b'.' {
                if self
                    .text_bytes
                    .get(self.current_pos.read_pos + 1)
                    .map_or(false, |char| char.is_ascii_digit())
                {
                    self.consume_float(self.current_pos)
                } else if let (Some(b'.'), Some(b'.')) = (
                    self.text_bytes.get(self.current_pos.read_pos + 1),
                    self.text_bytes.get(self.current_pos.read_pos + 2),
                ) {
                    self.consume_n_chars(3, TokenType::Ellipsis)
                } else {
                    self.consume_single_char(TokenType::Dot)
                }
            } else if char_byte == b'_'
                || convert_byte_to_unicode_codepoint(self.text_bytes, char_byte, self.current_pos.read_pos)
                    .is_xid_start()
            {
                self.consume_id_or_keyword()
            } else {
                let token = self.consume_single_char(TokenType::Invalid(char_byte as char));
                self.add_error(
                    PythonErrorType::InvalidToken,
                    format!(
                        "SyntaxError: character {} only allowed in string literal",
                        char_byte as char
                    ),
                    token.span,
                );

                token
            };

            tokens.push(token);
        }

        while *self.indent_stack.last().unwrap() > 0 {
            tokens.push(Token {
                kind: TokenType::Dedent,
                span: Span {
                    row_start: self.current_pos.row,
                    row_end: self.current_pos.row,
                    column_start: 1,
                    column_end: 1,
                },
            });
            self.indent_stack.pop();
        }

        tokens.push(Token {
            kind: TokenType::Eof,
            span: Span::new(self.current_pos, self.current_pos),
        });

        (tokens, self.errors)
    }

    fn handle_explicit_line_join(&mut self) {
        let (start, end) = self.advance_pos(1);

        let char = self.text_bytes[self.current_pos.read_pos];
        if !is_eol(char) {
            self.add_error(
                PythonErrorType::Syntax,
                format!(
                    "SyntaxError: unexpected character '{}' after line continuation character",
                    char as char
                ),
                Span::new(start, end),
            );
        } else {
            self.consume_eol();
        }
    }

    /// Given we are at the start of a line, count the number of spaces and/or tabs until the first character.
    /// Code borrowed from:
    /// https://github.com/RustPython/Parser/blob/704eb40108239a8faf9bd1d4217e8dad0ac7edb3/parser/src/lexer.rs#L608
    fn consume_indentation(&mut self) -> usize {
        let mut indent_level = 0;

        while !self.is_eof() {
            match self.text_bytes[self.current_pos.read_pos] {
                b' ' => {
                    self.advance_pos(1);
                    indent_level += 1;
                }
                b'#' => {
                    self.consume_comment();
                    indent_level = 0;
                }
                b'\t' => {
                    // FIXME: handle the case where spaces and tabs are mixed!
                    self.advance_pos(1);
                }
                0x0C => {
                    self.advance_pos(1);
                    indent_level = 0;
                }
                b'\n' | b'\r' => {
                    // Empty line
                    self.consume_eol();
                    indent_level = 0;
                }
                _ => {
                    self.is_at_beginning_of_line = false;
                    break;
                }
            }
        }

        indent_level
    }

    fn handle_indentation(&mut self) -> Vec<Token<'src>> {
        let current_indent_level = self.consume_indentation();
        let top_of_stack = &self.indent_stack.last().copied().unwrap();
        let mut indentation_tokens = vec![];

        match current_indent_level.cmp(top_of_stack) {
            std::cmp::Ordering::Less => {
                while self
                    .indent_stack
                    .last()
                    .map_or(false, |&top_of_stack| current_indent_level < top_of_stack)
                {
                    self.indent_stack.pop();
                    indentation_tokens.push(Token {
                        kind: TokenType::Dedent,
                        span: Span::new(self.current_pos, self.current_pos),
                    });
                }

                // compare the top of the stack with the current indent level
                if *self.indent_stack.last().unwrap() != current_indent_level {
                    self.add_error(
                        PythonErrorType::Indentation,
                        format!(
                            "IndentError: current indent amount '{current_indent_level}' does not match previous indent '{}'",
                            self.indent_stack.last().unwrap()
                        ),
                        Span::new(self.current_pos, self.current_pos),
                    );
                }
            }
            std::cmp::Ordering::Greater => {
                self.indent_stack.push(current_indent_level);
                indentation_tokens.push(Token {
                    kind: TokenType::Indent,
                    span: Span::new(self.current_pos, self.current_pos),
                });
            }
            std::cmp::Ordering::Equal => (), // Do nothing!
        }

        indentation_tokens
    }

    fn consume_string(&mut self) -> Token<'src> {
        let mut is_str_unclosed = false;
        let start = self.current_pos;

        let quote_char = self.text_bytes[self.current_pos.read_pos];
        self.advance_pos(1);

        let is_triple_quote =
            if self.text_bytes[self.current_pos.read_pos..self.current_pos.read_pos + 2] == [quote_char; 2] {
                self.advance_pos(2);
                true
            } else {
                false
            };

        while !self.is_eof() {
            match self.text_bytes[self.current_pos.read_pos] {
                // Consume '\' and the following char. Also handles the case where '\' is the
                // explicit line join character.
                b'\\' => {
                    self.advance_pos(1);
                    if is_eol(self.text_bytes[self.current_pos.read_pos]) {
                        self.consume_eol();
                    } else {
                        self.advance_pos(1);
                    }
                }
                c if is_eol(c) && !is_triple_quote => {
                    is_str_unclosed = true;
                    break;
                }
                // correctly handles the EOL inside triple quote strings
                c if is_eol(c) && is_triple_quote => {
                    self.consume_eol();
                }
                c if c == quote_char => {
                    if is_triple_quote {
                        self.advance_pos(1);

                        if self.text_bytes[self.current_pos.read_pos..self.current_pos.read_pos + 2] == [quote_char; 2]
                        {
                            self.advance_pos(2);
                            break;
                        }
                    } else {
                        self.advance_pos(1);
                        break;
                    }
                }
                // consume the content inside the string
                _ => {
                    self.advance_pos(1);
                }
            }

            if self.is_eof() {
                is_str_unclosed = true;
            }
        }
        let end = self.current_pos;

        if is_str_unclosed {
            self.add_error(
                PythonErrorType::Syntax,
                "SyntaxError: unclosed string literal".to_string(),
                Span::new(start, end),
            );
        }

        Token {
            kind: TokenType::String(&self.text[start.read_pos..end.read_pos]),
            span: Span::new(start, end),
        }
    }

    fn consume_eol(&mut self) -> Token<'src> {
        let token = if self.text_bytes[self.current_pos.read_pos] == b'\r'
            && self.text_bytes.get(self.current_pos.read_pos + 1) == Some(&b'\n')
        {
            self.consume_n_chars(2, TokenType::NewLine)
        } else {
            self.consume_single_char(TokenType::NewLine)
        };

        self.current_pos.new_line();
        token
    }

    fn consume_comment(&mut self) {
        self.consume_while(|char| !matches!(char, b'\n' | b'\r'));
    }

    fn consume_id_or_keyword(&mut self) -> Token<'src> {
        // TODO: check for a string prefix before consuming it.
        let start = self.current_pos;
        while !self.is_eof()
            && convert_byte_to_unicode_codepoint(
                self.text_bytes,
                self.text_bytes[self.current_pos.read_pos],
                self.current_pos.read_pos,
            )
            .is_xid_continue()
        {
            self.advance_pos(unicode_char_size(self.text_bytes[self.current_pos.read_pos]));
        }
        let end = self.current_pos;
        let id_str = &self.text[start.read_pos..end.read_pos];

        if !self.is_eof()
            && is_string_prefix(id_str)
            && matches!(self.text_bytes[self.current_pos.read_pos], b'\'' | b'"')
        {
            return self.consume_string();
        }

        let token_type = match id_str {
            "and" => TokenType::Keyword(KeywordType::And),
            "as" => TokenType::Keyword(KeywordType::As),
            "assert" => TokenType::Keyword(KeywordType::Assert),
            "async" => TokenType::Keyword(KeywordType::Async),
            "await" => TokenType::Keyword(KeywordType::Await),
            "break" => TokenType::Keyword(KeywordType::Break),
            "case" => TokenType::SoftKeyword(SoftKeywordType::Case),
            "class" => TokenType::Keyword(KeywordType::Class),
            "continue" => TokenType::Keyword(KeywordType::Continue),
            "def" => TokenType::Keyword(KeywordType::Def),
            "del" => TokenType::Keyword(KeywordType::Del),
            "elif" => TokenType::Keyword(KeywordType::Elif),
            "else" => TokenType::Keyword(KeywordType::Else),
            "except" => TokenType::Keyword(KeywordType::Except),
            "False" => TokenType::Keyword(KeywordType::False),
            "finally" => TokenType::Keyword(KeywordType::Finally),
            "for" => TokenType::Keyword(KeywordType::For),
            "from" => TokenType::Keyword(KeywordType::From),
            "global" => TokenType::Keyword(KeywordType::Global),
            "if" => TokenType::Keyword(KeywordType::If),
            "import" => TokenType::Keyword(KeywordType::Import),
            "in" => TokenType::Keyword(KeywordType::In),
            "is" => TokenType::Keyword(KeywordType::Is),
            "lambda" => TokenType::Keyword(KeywordType::Lambda),
            "match" => TokenType::SoftKeyword(SoftKeywordType::Match),
            "None" => TokenType::Keyword(KeywordType::None),
            "nonlocal" => TokenType::Keyword(KeywordType::NonLocal),
            "not" => TokenType::Keyword(KeywordType::Not),
            "or" => TokenType::Keyword(KeywordType::Or),
            "pass" => TokenType::Keyword(KeywordType::Pass),
            "raise" => TokenType::Keyword(KeywordType::Raise),
            "return" => TokenType::Keyword(KeywordType::Return),
            "True" => TokenType::Keyword(KeywordType::True),
            "try" => TokenType::Keyword(KeywordType::Try),
            "while" => TokenType::Keyword(KeywordType::While),
            "with" => TokenType::Keyword(KeywordType::With),
            "yield" => TokenType::Keyword(KeywordType::Yield),
            "_" => TokenType::SoftKeyword(SoftKeywordType::Underscore),
            _ => TokenType::Id(id_str),
        };

        Token {
            kind: token_type,
            span: Span::new(start, end),
        }
    }

    fn consume_float(&mut self, float_start: Position) -> Token<'src> {
        if matches!(self.text_bytes[self.current_pos.read_pos], b'e' | b'E') {
            self.advance_pos(1);
            // after consuming the 'e'/'E', we may see a '+' or '-'
            if !self.is_eof() && matches!(self.text_bytes[self.current_pos.read_pos], b'-' | b'+') {
                self.advance_pos(1);
            }
        } else if self.text_bytes[self.current_pos.read_pos] == b'.' {
            // consume '.'
            self.advance_pos(1);
        }

        let (_, mut end) = self.consume_while(|char| char.is_ascii_digit() || char == b'_');

        if !self.is_eof() && matches!(self.text_bytes[self.current_pos.read_pos], b'e' | b'E') {
            self.advance_pos(1);
            // after consuming the 'e'/'E', we may see a '+' or '-'
            if !self.is_eof() && matches!(self.text_bytes[self.current_pos.read_pos], b'-' | b'+') {
                self.advance_pos(1);
            }

            (_, end) = self.consume_while(|char| char.is_ascii_digit() || char == b'_');
        }

        let token_kind = if !self.is_eof() && matches!(self.text_bytes[end.read_pos], b'j' | b'J') {
            // consume 'j'/'J'
            self.advance_pos(1);
            let float = &self.text[float_start.read_pos..self.current_pos.read_pos];
            if !is_float_number_valid(float) {
                self.add_error(
                    PythonErrorType::Syntax,
                    format!("SyntaxError: invalid imaginary number literal \"{float}\""),
                    Span::new(float_start, end),
                );
            }
            TokenType::Number(NumberType::Imaginary(float))
        } else {
            let float = &self.text[float_start.read_pos..end.read_pos];
            if !is_float_number_valid(float) {
                self.add_error(
                    PythonErrorType::Syntax,
                    format!("SyntaxError: invalid float number literal \"{float}\""),
                    Span::new(float_start, end),
                );
            }
            TokenType::Number(NumberType::Float(float))
        };

        Token {
            kind: token_kind,
            span: Span::new(float_start, end),
        }
    }

    fn consume_number(&mut self) -> Token<'src> {
        let (start, mut end) = self.consume_while(|char| char.is_ascii_digit() || char == b'_');

        if !self.is_eof()
            && matches!(
                self.text_bytes[self.current_pos.read_pos],
                b'.' | b'e' | b'E' | b'j' | b'J'
            )
        {
            return self.consume_float(start);
        }

        // In case we find any alphabetic character after the number, consume it.
        // TODO: probably do this check with `is_xid_continue`
        if !self.is_eof() && self.text_bytes[self.current_pos.read_pos].is_ascii_alphabetic() {
            (_, end) = self.consume_while(|char| char.is_ascii_alphanumeric() || char == b'_');
        }

        let number = &self.text[start.read_pos..end.read_pos];

        // check if the number is valid and get their type.
        let number_type = if number.len() > 2 {
            match &number[..2] {
                "0b" | "0B" => {
                    if !is_binary_number_valid(number) {
                        self.add_error(
                            PythonErrorType::Syntax,
                            "SyntaxError: invalid binary number literal".to_string(),
                            Span::new(start, end),
                        );
                    }
                    TokenType::Number(NumberType::Integer(IntegerType::Binary(number)))
                }
                "0x" | "0X" => {
                    if !is_hex_number_valid(number) {
                        self.add_error(
                            PythonErrorType::Syntax,
                            "SyntaxError: invalid hexadecimal number literal".to_string(),
                            Span::new(start, end),
                        );
                    }
                    TokenType::Number(NumberType::Integer(IntegerType::Hex(number)))
                }
                "0o" | "0O" => {
                    if !is_octal_number_valid(number) {
                        self.add_error(
                            PythonErrorType::Syntax,
                            "SyntaxError: invalid octal number literal".to_string(),
                            Span::new(start, end),
                        );
                    }
                    TokenType::Number(NumberType::Integer(IntegerType::Octal(number)))
                }
                _ => {
                    if !is_decimal_number_valid(number) {
                        self.add_error(
                            PythonErrorType::Syntax,
                            "SyntaxError: invalid decimal number literal".to_string(),
                            Span::new(start, end),
                        );
                    }
                    TokenType::Number(NumberType::Integer(IntegerType::Decimal(number)))
                }
            }
        } else {
            if !is_decimal_number_valid(number) {
                self.add_error(
                    PythonErrorType::Syntax,
                    "SyntaxError: invalid decimal number literal".to_string(),
                    Span::new(start, end),
                );
            }

            TokenType::Number(NumberType::Integer(IntegerType::Decimal(number)))
        };

        Token {
            kind: number_type,
            span: Span::new(start, end),
        }
    }

    fn consume_operator(&mut self) -> Token<'src> {
        let (token_kind, total_chars) = match (
            self.text_bytes[self.current_pos.read_pos],
            self.text_bytes.get(self.current_pos.read_pos + 1),
        ) {
            (b'=', Some(b'=')) => (TokenType::Operator(OperatorType::Equals), 2),
            (b'+', Some(b'=')) => (TokenType::Operator(OperatorType::PlusEqual), 2),
            (b'*', Some(b'=')) => (TokenType::Operator(OperatorType::AsteriskEqual), 2),
            (b'-', Some(b'=')) => (TokenType::Operator(OperatorType::MinusEqual), 2),
            (b'<', Some(b'=')) => (TokenType::Operator(OperatorType::LessThanOrEqual), 2),
            (b'>', Some(b'=')) => (TokenType::Operator(OperatorType::GreaterThanOrEqual), 2),
            (b'^', Some(b'=')) => (TokenType::Operator(OperatorType::BitwiseXOrEqual), 2),
            (b'~', Some(b'=')) => (TokenType::Operator(OperatorType::BitwiseNotEqual), 2),
            (b'!', Some(b'=')) => (TokenType::Operator(OperatorType::NotEquals), 2),
            (b'%', Some(b'=')) => (TokenType::Operator(OperatorType::ModuloEqual), 2),
            (b'&', Some(b'=')) => (TokenType::Operator(OperatorType::BitwiseAndEqual), 2),
            (b'|', Some(b'=')) => (TokenType::Operator(OperatorType::BitwiseOrEqual), 2),
            (b'@', Some(b'=')) => (TokenType::Operator(OperatorType::AtEqual), 2),
            (b'/', Some(b'=')) => (TokenType::Operator(OperatorType::DivideEqual), 2),
            (b'/', Some(b'/')) => {
                if self
                    .text_bytes
                    .get(self.current_pos.read_pos + 2)
                    .map_or(false, |char| *char == b'=')
                {
                    (TokenType::Operator(OperatorType::FloorDivisionEqual), 3)
                } else {
                    (TokenType::Operator(OperatorType::FloorDivision), 2)
                }
            }
            (b'<', Some(b'<')) => {
                if self
                    .text_bytes
                    .get(self.current_pos.read_pos + 2)
                    .map_or(false, |char| *char == b'=')
                {
                    (TokenType::Operator(OperatorType::BitwiseLeftShiftEqual), 3)
                } else {
                    (TokenType::Operator(OperatorType::BitwiseLeftShift), 2)
                }
            }
            (b'>', Some(b'>')) => {
                if self
                    .text_bytes
                    .get(self.current_pos.read_pos + 2)
                    .map_or(false, |char| *char == b'=')
                {
                    (TokenType::Operator(OperatorType::BitwiseRightShiftEqual), 3)
                } else {
                    (TokenType::Operator(OperatorType::BitwiseRightShift), 2)
                }
            }
            (b'*', Some(b'*')) => {
                if self
                    .text_bytes
                    .get(self.current_pos.read_pos + 2)
                    .map_or(false, |char| *char == b'=')
                {
                    (TokenType::Operator(OperatorType::ExponentEqual), 3)
                } else {
                    (TokenType::Operator(OperatorType::Exponent), 2)
                }
            }

            (b'%', _) => (TokenType::Operator(OperatorType::Modulo), 1),
            (b'&', _) => (TokenType::Operator(OperatorType::BitwiseAnd), 1),
            (b'*', _) => (TokenType::Operator(OperatorType::Asterisk), 1),
            (b'+', _) => (TokenType::Operator(OperatorType::Plus), 1),
            (b'-', _) => (TokenType::Operator(OperatorType::Minus), 1),
            (b'<', _) => (TokenType::Operator(OperatorType::LessThan), 1),
            (b'=', _) => (TokenType::Operator(OperatorType::Assign), 1),
            (b'>', _) => (TokenType::Operator(OperatorType::GreaterThan), 1),
            (b'^', _) => (TokenType::Operator(OperatorType::BitwiseXOR), 1),
            (b'|', _) => (TokenType::Operator(OperatorType::BitwiseOr), 1),
            (b'~', _) => (TokenType::Operator(OperatorType::BitwiseNot), 1),
            (b'@', _) => (TokenType::Operator(OperatorType::At), 1),
            (b'/', _) => (TokenType::Operator(OperatorType::Divide), 1),

            (_, _) => {
                let mut end = self.current_pos;
                end.column += 2;

                self.add_error(
                    PythonErrorType::Syntax,
                    "SyntaxError: invalid operator".to_string(),
                    Span::new(self.current_pos, end),
                );

                (TokenType::Operator(OperatorType::Invalid), 2)
            }
        };

        self.consume_n_chars(total_chars, token_kind)
    }
}
