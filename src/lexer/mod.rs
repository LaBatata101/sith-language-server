mod char_stream;
mod helpers;
pub mod span;
pub mod token;

use std::borrow::Cow;
use unicode_ident::{is_xid_continue, is_xid_start};

pub use char_stream::CharStream;
use token::Token;

use crate::error::{PythonError, PythonErrorType};

use self::{
    span::{Position, Span},
    token::types::{IntegerType, KeywordType, NumberType, OperatorType, SoftKeywordType, TokenType},
};

pub struct Lexer<'a> {
    cs: CharStream<'a>,
    tokens: Vec<Token<'a>>,
    indent_stack: Vec<usize>,
    /// This is used to check if we are inside a [], () or {}
    implicit_line_joining: i32,
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            cs: CharStream::new(text),
            tokens: Vec::new(),
            indent_stack: vec![0],
            implicit_line_joining: 0,
        }
    }

    pub fn tokenize(&mut self) -> Option<Vec<PythonError>> {
        let mut errors: Vec<PythonError> = vec![];
        let mut is_beginning_of_line = true;

        while !self.cs.is_eof() {
            // TODO: refactor this
            if is_beginning_of_line {
                is_beginning_of_line = false;

                let whitespace_total = self.cs.skip_whitespace();

                if self.cs.is_eof() {
                    break;
                }

                // Skip comments
                if self.cs.current_char().map_or(false, |char| char == '#') {
                    self.skip_comment();
                }

                // skip lines containing only white spaces or \n, \r, \r\n
                if let Some(eol_size) = self.cs.is_at_eol() {
                    if self.implicit_line_joining == 0 {
                        is_beginning_of_line = true;
                        self.cs.advance_by(eol_size);
                        continue;
                    }
                }

                if self.implicit_line_joining == 0 {
                    if let Err(error) = self.handle_indentation(whitespace_total) {
                        errors.push(error);
                    }
                }
            }

            let Some(char) = self.cs.current_unicode_char() else { break };

            if char == '_' || is_xid_start(char) {
                self.lex_identifier_or_keyword()
            } else if char.is_ascii_digit() {
                if let Some(number_errors) = self.lex_number() {
                    errors.extend(number_errors);
                }
            } else if char == '"' || char == '\'' {
                if let Some(string_errors) = self.lex_string() {
                    errors.extend(string_errors);
                }
            } else if char == '(' {
                self.implicit_line_joining += 1;

                self.lex_single_char(TokenType::OpenParenthesis);
            } else if char == ')' {
                self.implicit_line_joining -= 1;

                self.lex_single_char(TokenType::CloseParenthesis);
            } else if char == '[' {
                self.implicit_line_joining += 1;

                self.lex_single_char(TokenType::OpenBrackets);
            } else if char == ']' {
                self.implicit_line_joining -= 1;

                self.lex_single_char(TokenType::CloseBrackets);
            } else if char == '{' {
                self.implicit_line_joining += 1;

                self.lex_single_char(TokenType::OpenBrace);
            } else if char == '}' {
                self.implicit_line_joining -= 1;

                self.lex_single_char(TokenType::CloseBrace);
            } else if char == '.' {
                if self.cs.next_char().map_or(false, |char| char.is_ascii_digit()) {
                    self.lex_number();
                    continue;
                }

                if let (Some('.'), Some('.')) = (self.cs.next_char(), self.cs.peek_char(self.cs.pos().index + 2)) {
                    let start = self.cs.pos();
                    self.cs.advance_by(3);
                    let end = self.cs.pos();
                    self.tokens.push(Token {
                        kind: TokenType::Ellipsis,
                        span: self.make_span(start, end),
                    })
                } else {
                    self.lex_single_char(TokenType::Dot)
                }
            } else if char == ';' {
                self.lex_single_char(TokenType::SemiColon)
            } else if char == ',' {
                self.lex_single_char(TokenType::Comma)
            } else if char == ':' {
                if let Some('=') = self.cs.next_char() {
                    let start = self.cs.pos();
                    self.cs.advance_by(2);
                    let end = self.cs.pos();
                    self.tokens.push(Token {
                        kind: TokenType::Operator(OperatorType::ColonEqual),
                        span: self.make_span(start, end),
                    })
                } else {
                    self.lex_single_char(TokenType::Colon)
                }
            } else if let '*' | '+' | '=' | '-' | '<' | '>' | '&' | '|' | '%' | '~' | '^' | '!' | '@' | '/' = char {
                if let (Some('-'), Some('>')) = (self.cs.current_char(), self.cs.next_char()) {
                    let start = self.cs.pos();
                    self.cs.advance_by(2);
                    let end = self.cs.pos();
                    self.tokens.push(Token {
                        kind: TokenType::RightArrow,
                        span: self.make_span(start, end),
                    });
                } else {
                    self.lex_operator();
                }
            } else if let ' ' | '\t' | '\u{0c}' = char {
                self.cs.skip_whitespace();
            } else if let '\n' | '\r' = char {
                is_beginning_of_line = true;
                let eol_size = self.cs.is_at_eol().unwrap();

                let start = self.cs.pos();

                self.cs.advance_by(eol_size);

                if self.implicit_line_joining > 0 {
                    continue;
                }

                let end = Position {
                    column: start.column + 1,
                    ..start
                };

                self.tokens.push(Token {
                    kind: TokenType::NewLine,
                    span: self.make_span(start, end),
                });
            } else if char == '\\' {
                let start = self.cs.pos();
                // consume \
                self.cs.advance_by(1);
                let end = self.cs.pos();

                if let Some(offset) = self.cs.is_at_eol() {
                    self.cs.advance_by(offset);
                } else {
                    errors.push(PythonError {
                        msg: String::from("SyntaxError: unexpected characters after line continuation character"),
                        error: PythonErrorType::Syntax,
                        span: self.make_span(start, end),
                    });
                }
            } else if char == '#' {
                self.skip_comment();
            } else {
                let start = self.cs.pos();
                self.cs.advance_by(1);
                let end = self.cs.pos();

                errors.push(PythonError {
                    error: PythonErrorType::Syntax,
                    msg: format!("SyntaxError: invalid syntax, character '{char}' only allowed inside string literals"),
                    span: self.make_span(start, end),
                })
            }
        }

        while self.indent_stack.last().copied().unwrap() > 0 {
            self.tokens.push(Token {
                kind: TokenType::Dedent,
                span: Span {
                    column_start: 1,
                    column_end: 1,
                    ..self.make_span(self.cs.pos(), self.cs.pos())
                },
            });
            self.indent_stack.pop();
        }

        let eof_span = self.make_span(self.cs.pos(), self.cs.pos());
        self.tokens.push(Token {
            kind: TokenType::Eof,
            span: Span {
                column_end: eof_span.column_end + 1,
                ..eof_span
            },
        });

        if errors.is_empty() {
            None
        } else {
            Some(errors)
        }
    }

    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    // FIXME: Handle mix of tabs and spaces in indentation
    fn handle_indentation(&mut self, whitespace_total: usize) -> Result<(), PythonError> {
        let top_of_stack = self.indent_stack.last().copied().unwrap();

        match whitespace_total.cmp(&top_of_stack) {
            std::cmp::Ordering::Less => {
                while self
                    .indent_stack
                    .last()
                    .map_or(false, |&top_of_stack| whitespace_total < top_of_stack)
                {
                    self.indent_stack.pop();
                    self.tokens.push(Token {
                        kind: TokenType::Dedent,
                        span: Span {
                            column_start: 1,
                            column_end: 1,
                            ..self.make_span(self.cs.pos(), self.cs.pos())
                        },
                    });
                }

                if self
                    .indent_stack
                    .last()
                    .map_or(false, |&top_of_stack| whitespace_total != top_of_stack)
                {
                    return Err(PythonError {
                        error: PythonErrorType::Indentation,
                        msg: "IndentError: indent amount does not match previous indent".to_string(),
                        span: Span {
                            column_start: 1,
                            column_end: 1,
                            ..self.make_span(self.cs.pos(), self.cs.pos())
                        },
                    });
                }
            }
            std::cmp::Ordering::Greater => {
                self.indent_stack.push(whitespace_total);
                self.tokens.push(Token {
                    kind: TokenType::Indent,
                    span: Span {
                        column_start: 1,
                        column_end: 1,
                        ..self.make_span(self.cs.pos(), self.cs.pos())
                    },
                });
            }
            std::cmp::Ordering::Equal => (), // Do nothing!
        }

        Ok(())
    }

    fn lex_single_char(&mut self, token: TokenType<'a>) {
        let start = self.cs.pos();
        self.cs.advance_by(1);
        let end = self.cs.pos();

        self.tokens.push(Token {
            kind: token,
            span: self.make_span(start, end),
        });
    }

    fn lex_identifier_or_keyword(&mut self) {
        let start = self.cs.pos();
        while self.cs.current_unicode_char().map_or(false, is_xid_continue) {
            self.cs.advance_by(self.cs.current_char_size());
        }
        let end = self.cs.pos();

        let str = self.cs.get_str(start.index, end.index).unwrap();

        if self.is_str_prefix(str) && matches!(self.cs.current_char(), Some('"' | '\'')) {
            self.lex_string();
            return;
        }

        let token_type = match str {
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
            _ => TokenType::Id(Cow::Borrowed(str)),
        };

        self.tokens.push(Token {
            kind: token_type,
            span: self.make_span(start, end),
        });
    }

    fn is_str_prefix(&self, str: &str) -> bool {
        matches!(
            str,
            // string prefixes
            "r" | "u" | "R" | "U" | "f" | "F" | "fr" | "Fr" | "fR" | "FR" | "rf" | "rF" | "Rf" | "RF"
            // byte string prefixes
            | "b" | "B" | "br" | "Br" | "bR" | "BR" | "rb" | "rB" | "Rb" | "RB"
        )
    }

    // TODO: Define a different string type for string prefixes
    // TODO: try to refactor this code
    //https://docs.python.org/3/reference/lexical_analysis.html#string-and-bytes-literals
    fn lex_string(&mut self) -> Option<Vec<PythonError>> {
        if self.implicit_line_joining > 0 {
            return self.lex_string_within_parens();
        }

        let mut errors = vec![];
        let (string, mut str_span, string_errors) = self.process_string();
        let mut string = String::from(string);

        if let Some(string_errors) = string_errors {
            errors.extend(string_errors);
        }

        self.cs.skip_whitespace();
        // check for any string prefix
        if let Some('r' | 'b' | 'f' | 'F' | 'R' | 'B' | 'u' | 'U') = self.cs.current_char() {
            while self.cs.current_char().map_or(false, is_xid_continue) {
                self.cs.advance_by(1);
            }
        }

        while let Some('\'' | '"') = self.cs.current_char() {
            let (str, span, str_errors) = self.process_string();

            string.push_str(str);
            str_span.column_end = span.column_end;
            str_span.row_end = span.row_end;

            if let Some(str_errors) = str_errors {
                errors.extend(str_errors);
            }

            self.cs.skip_whitespace();

            if let Some('#') = self.cs.current_char() {
                self.skip_comment();
            }

            // check for any string prefix
            if let Some('r' | 'b' | 'f' | 'F' | 'R' | 'B' | 'u' | 'U') = self.cs.current_char() {
                while self.cs.current_char().map_or(false, is_xid_continue) {
                    self.cs.advance_by(1);
                }
            }
        }

        // If we encounter a '\' (explicit line join char) followed by a string,
        // we need to treat these strings as one Token.
        self.cs.skip_whitespace();
        while self.cs.current_char().map_or(false, |char| char == '\\') {
            self.cs.advance_by(1);

            // Skip any whitespace after the "\"
            self.cs.skip_whitespace();
            if let Some('#') = self.cs.current_char() {
                self.skip_comment();
            }

            if let Some(offset) = self.cs.is_at_eol() {
                self.cs.advance_by(offset);
            }

            // Skip any whitespace before the string
            self.cs.skip_whitespace();

            // check for any string prefix
            if let Some('r' | 'b' | 'f' | 'F' | 'R' | 'B' | 'u' | 'U') = self.cs.current_char() {
                while self.cs.current_char().map_or(false, is_xid_continue) {
                    self.cs.advance_by(1);
                }
            }

            if let Some('"' | '\'') = self.cs.current_char() {
                let (str, span, str_errors) = self.process_string();

                string.push_str(str);
                str_span.column_end = span.column_end;
                str_span.row_end = span.row_end;

                if let Some(str_errors) = str_errors {
                    errors.extend(str_errors);
                }
            }

            // Skip any whitespace after the string
            self.cs.skip_whitespace();
        }

        self.tokens.push(Token {
            kind: TokenType::String(Cow::Owned(string)),
            span: str_span,
        });

        if errors.is_empty() {
            None
        } else {
            Some(errors)
        }
    }

    fn lex_string_within_parens(&mut self) -> Option<Vec<PythonError>> {
        let mut errors = Vec::new();
        let mut out_string: Cow<str> = Cow::default();

        let start = self.cs.pos();
        while let Some('"' | '\'') = self.cs.current_char() {
            let (string, _, str_errors) = self.process_string();
            out_string.to_mut().push_str(string);

            if let Some(str_errors) = str_errors {
                errors.extend(str_errors);
            }

            // ignore comments in the same line of the string
            self.cs.skip_whitespace();
            if let Some('#') = self.cs.current_char() {
                self.skip_comment();
            }

            if let Some('\\') = self.cs.current_char() {
                self.cs.advance_by(1);
            }

            while let Some(offset) = self.cs.is_at_eol() {
                self.cs.advance_by(offset);
            }
            self.cs.skip_whitespace();

            // ignore comments between strings
            while let Some('#') = self.cs.current_char() {
                self.skip_comment();

                while let Some(offset) = self.cs.is_at_eol() {
                    self.cs.advance_by(offset);
                    self.cs.skip_whitespace();
                }
            }

            // check for a str prefix and then consume it.
            if let (
                Some('r' | 'b' | 'f' | 'F' | 'R' | 'B' | 'u' | 'U'),
                Some('\'' | '"' | 'r' | 'R' | 'f' | 'F' | 'b' | 'B'),
            ) = (self.cs.current_char(), self.cs.next_char())
            {
                let start = self.cs.pos();
                while self.cs.current_char().map_or(false, is_xid_continue) {
                    self.cs.advance_by(1);
                }
                let end = self.cs.pos();

                let str_prefix = self.cs.get_str(start.index, end.index).unwrap();
                if !self.is_str_prefix(str_prefix) {
                    errors.push(PythonError {
                        error: PythonErrorType::Syntax,
                        msg: format!("SyntaxError: Invalid str prefix \"{}\"", str_prefix),
                        span: self.make_span(start, end),
                    });
                }
            }
        }
        let end = self.cs.pos();

        self.tokens.push(Token {
            kind: TokenType::String(out_string),
            span: self.make_span(start, end),
        });

        if errors.is_empty() {
            None
        } else {
            Some(errors)
        }
    }

    fn count_quotes_in_current_pos(&self, quote: char) -> u32 {
        let mut pos = self.cs.pos().index;
        let mut total_quotes = 0;

        while self.cs.peek_char(pos).map_or(false, |char| char == quote) {
            total_quotes += 1;
            pos += 1;
        }

        total_quotes
    }

    fn process_string(&mut self) -> (&'a str, Span, Option<Vec<PythonError>>) {
        let mut errors: Vec<PythonError> = vec![];

        let start_total_quote: u32;
        let end_total_quote: u32;

        let mut is_missing_close_quote = false;

        // Can be `"` or `'`
        let quote_char = self.cs.current_char().unwrap();

        let start = self.cs.pos();

        if self.is_triple_quote_str_in_pos(start) {
            start_total_quote = 3;
            self.cs.advance_by(3);
            let mut pos = self.cs.pos();

            // FIXME: infinite loop when the string is not closed
            while !self.is_triple_quote_str_in_pos(pos) {
                // consume escaped triple quote
                if self.cs.current_char().map_or(false, |char| char == '\\')
                    && self.cs.next_char().map_or(false, |char| char == quote_char)
                {
                    // consume "\"
                    self.cs.advance_by(1);

                    // FIXME: has to work with single and double quote
                    let total_quotes = self.count_quotes_in_current_pos(quote_char);
                    // case when there is a triple escaped quote -> \"""
                    if total_quotes == 3 {
                        self.cs.advance_by(3);

                    // case when there is one escaped quote followed by triple quote -> \""""
                    } else if total_quotes == 4 {
                        self.cs.advance_by(1);
                    }
                } else {
                    self.cs.advance_by(1);
                }

                if self.cs.current_char().map_or(false, |char| char == quote_char) {
                    pos = self.cs.pos();
                }
            }

            if self.is_triple_quote_str_in_pos(pos) {
                end_total_quote = 3;
                self.cs.advance_by(3);
            } else {
                end_total_quote = 0;
                is_missing_close_quote = true;
            }
        } else {
            start_total_quote = 1;
            // consume " or '
            self.cs.advance_by(1);

            while self
                .cs
                .current_char()
                .map_or(false, |char| char != quote_char && !(char == '\n' || char == '\r'))
            {
                if let (Some('\\'), Some(_)) = (self.cs.current_char(), self.cs.next_char()) {
                    // FIXME: temporary, if we advance with an offset larger than 1 the code
                    // that handles the eol in `advance_by` is only executed one time
                    self.cs.advance_by(1);
                    self.cs.advance_by(1);
                } else {
                    self.cs.advance_by(1);
                }
            }

            if self.cs.current_char().map_or(false, |char| char == quote_char) {
                end_total_quote = 1;
                self.cs.advance_by(1);
            } else {
                end_total_quote = 0;
                is_missing_close_quote = true;
            }
        }
        let end = self.cs.pos();

        if is_missing_close_quote {
            errors.push(PythonError {
                error: PythonErrorType::Syntax,
                msg: String::from("SyntaxError: unterminated string literal"),
                span: self.make_span(start, end),
            });
        }

        (
            self.cs
                .get_str(start.index + start_total_quote, end.index - end_total_quote)
                .unwrap_or_else(|| {
                    panic!(
                        "Failed to get the string slice in pos: ({}, {}) -> ({}, {})",
                        start.row, start.column, end.row, end.column
                    )
                }),
            self.make_span(start, end),
            if errors.is_empty() { None } else { Some(errors) },
        )
    }

    fn is_triple_quote_str_in_pos(&self, pos: Position) -> bool {
        matches!(
            (
                self.cs.peek_char(pos.index),
                self.cs.peek_char(pos.index + 1),
                self.cs.peek_char(pos.index + 2)
            ),
            (Some('"'), Some('"'), Some('"')) | (Some('\''), Some('\''), Some('\''))
        )
    }

    fn lex_operator(&mut self) {
        let start = self.cs.pos();
        let (token_type, advance_offset) = match (self.cs.current_char().unwrap(), self.cs.next_char()) {
            ('=', Some('=')) => (TokenType::Operator(OperatorType::Equals), 2),
            ('+', Some('=')) => (TokenType::Operator(OperatorType::PlusEqual), 2),
            ('*', Some('=')) => (TokenType::Operator(OperatorType::AsteriskEqual), 2),
            ('-', Some('=')) => (TokenType::Operator(OperatorType::MinusEqual), 2),
            ('<', Some('=')) => (TokenType::Operator(OperatorType::LessThanOrEqual), 2),
            ('>', Some('=')) => (TokenType::Operator(OperatorType::GreaterThanOrEqual), 2),
            ('^', Some('=')) => (TokenType::Operator(OperatorType::BitwiseXOrEqual), 2),
            ('~', Some('=')) => (TokenType::Operator(OperatorType::BitwiseNotEqual), 2),
            ('!', Some('=')) => (TokenType::Operator(OperatorType::NotEquals), 2),
            ('%', Some('=')) => (TokenType::Operator(OperatorType::ModuloEqual), 2),
            ('&', Some('=')) => (TokenType::Operator(OperatorType::BitwiseAndEqual), 2),
            ('|', Some('=')) => (TokenType::Operator(OperatorType::BitwiseOrEqual), 2),
            ('@', Some('=')) => (TokenType::Operator(OperatorType::AtEqual), 2),
            ('/', Some('=')) => (TokenType::Operator(OperatorType::DivideEqual), 2),
            ('/', Some('/')) => {
                if self
                    .cs
                    .peek_char(self.cs.pos().index + 2)
                    .map_or(false, |char| char == '=')
                {
                    (TokenType::Operator(OperatorType::FloorDivisionEqual), 3)
                } else {
                    (TokenType::Operator(OperatorType::FloorDivision), 2)
                }
            }
            ('<', Some('<')) => {
                if self
                    .cs
                    .peek_char(self.cs.pos().index + 2)
                    .map_or(false, |char| char == '=')
                {
                    (TokenType::Operator(OperatorType::BitwiseLeftShiftEqual), 3)
                } else {
                    (TokenType::Operator(OperatorType::BitwiseLeftShift), 2)
                }
            }
            ('>', Some('>')) => {
                if self
                    .cs
                    .peek_char(self.cs.pos().index + 2)
                    .map_or(false, |char| char == '=')
                {
                    (TokenType::Operator(OperatorType::BitwiseRightShiftEqual), 3)
                } else {
                    (TokenType::Operator(OperatorType::BitwiseRightShift), 2)
                }
            }
            ('*', Some('*')) => {
                if self
                    .cs
                    .peek_char(self.cs.pos().index + 2)
                    .map_or(false, |char| char == '=')
                {
                    (TokenType::Operator(OperatorType::ExponentEqual), 3)
                } else {
                    (TokenType::Operator(OperatorType::Exponent), 2)
                }
            }

            ('%', _) => (TokenType::Operator(OperatorType::Modulo), 1),
            ('&', _) => (TokenType::Operator(OperatorType::BitwiseAnd), 1),
            ('*', _) => (TokenType::Operator(OperatorType::Asterisk), 1),
            ('+', _) => (TokenType::Operator(OperatorType::Plus), 1),
            ('-', _) => (TokenType::Operator(OperatorType::Minus), 1),
            ('<', _) => (TokenType::Operator(OperatorType::LessThan), 1),
            ('=', _) => (TokenType::Operator(OperatorType::Assign), 1),
            ('>', _) => (TokenType::Operator(OperatorType::GreaterThan), 1),
            ('^', _) => (TokenType::Operator(OperatorType::BitwiseXOR), 1),
            ('|', _) => (TokenType::Operator(OperatorType::BitwiseOr), 1),
            ('~', _) => (TokenType::Operator(OperatorType::BitwiseNot), 1),
            ('@', _) => (TokenType::Operator(OperatorType::At), 1),
            ('/', _) => (TokenType::Operator(OperatorType::Divide), 1),

            (char, _) => (TokenType::Invalid(char), 1),
        };

        self.cs.advance_by(advance_offset);
        let end = self.cs.pos();
        self.tokens.push(Token {
            kind: token_type,
            span: self.make_span(start, end),
        });
    }

    // TODO: handle invalid numbers
    fn lex_number(&mut self) -> Option<Vec<PythonError>> {
        let mut errors = vec![];
        let mut number_type = NumberType::Invalid;
        let start = self.cs.pos();

        if self.cs.current_char().unwrap() == '0' && self.cs.next_char().is_some() {
            let next_char = self.cs.next_char().unwrap();
            // Try to lex binary number
            if next_char == 'b' || next_char == 'B' {
                self.cs.advance_by(2);
                self.cs.advance_while(1, |char| matches!(char, '0' | '1' | '_'));
                number_type = NumberType::Integer(IntegerType::Binary);
            }

            // Try to lex hex number
            if next_char == 'x' || next_char == 'X' {
                self.cs.advance_by(2);
                self.cs.advance_while(1, |char| char.is_ascii_hexdigit() || char == '_');
                number_type = NumberType::Integer(IntegerType::Hex);
            }

            // Try to lex octal number
            if next_char == 'o' || next_char == 'O' {
                self.cs.advance_by(2);
                // FIXME: use `is_ascii_octdigit` when stable
                self.cs.advance_while(1, |char| matches!(char, '0'..='7' | '_'));
                number_type = NumberType::Integer(IntegerType::Octal);
            }
        }

        // Try to lex decimal number
        if self.handle_decimal_number().is_some() {
            number_type = NumberType::Integer(IntegerType::Decimal);

            // Check for any character after the digits
            self.cs.advance_while(1, is_xid_continue);

            if let Err(error) = self.check_if_decimal_is_valid_in_pos(start, self.cs.pos()) {
                errors.push(error);
            }
        }

        // Handle float and imaginary numbers
        if self
            .cs
            .current_char()
            .map_or(false, |char| matches!(char, '.' | 'e' | 'E'))
        {
            let (float_or_imaginary, syntax_errors) = self.handle_float_or_imaginary_number(start);
            number_type = float_or_imaginary;
            errors.extend(syntax_errors);
        }

        let end = self.cs.pos();

        let number = self.cs.get_str(start.index, end.index).unwrap();
        self.tokens.push(Token {
            kind: TokenType::Number(number_type, Cow::Borrowed(number)),
            span: self.make_span(start, end),
        });

        if errors.is_empty() {
            None
        } else {
            Some(errors)
        }
    }

    // Handle the fraction part of the float number
    fn handle_float_or_imaginary_number(&mut self, float_start: Position) -> (NumberType, Vec<PythonError>) {
        let mut errors = vec![];
        let mut allow_sign = false;

        // Here we are consuming all the valid characters that a float string can have, we don't care
        // at this point if the string is valid, the checking if the string is a valid float is done
        // later. The goal is to keep the float string in one Token, even if it is an invalid float.
        while self.cs.current_char().map_or(false, |char| {
            char.is_ascii_digit() || matches!(char, '.' | 'e' | 'E' | '_') || (allow_sign && matches!(char, '-' | '+'))
        }) {
            if self
                .cs
                .current_char()
                .map_or(false, |char| char.is_ascii_digit() || matches!(char, '-' | '+'))
            {
                let (decimal_start, decimal_end) = self.handle_decimal_number().unwrap();

                if let Err(error) = self.check_if_decimal_is_valid_in_pos(decimal_start, decimal_end) {
                    errors.push(error);
                }
            } else {
                if let Some('e' | 'E') = self.cs.current_char() {
                    allow_sign = true;
                }
                self.cs.advance_by(1);
            }
        }

        if let Some('j' | 'J') = self.cs.current_char() {
            // Consume j or J
            self.cs.advance_by(1);

            // In case we have multiple j's, consume all of them.
            if let Some('j' | 'J') = self.cs.current_char() {
                self.cs.advance_while(1, |char| matches!(char, 'j' | 'J'));

                if let Err(mut error) = self.check_if_float_number_is_valid_in_pos(float_start, self.cs.pos()) {
                    error.msg = "SyntaxError: invalid imaginary literal".to_string();
                    errors.push(error);
                }
            }

            (NumberType::Imaginary, errors)
        } else {
            if let Err(error) = self.check_if_float_number_is_valid_in_pos(float_start, self.cs.pos()) {
                errors.push(error);
            }
            (NumberType::Float, errors)
        }
    }

    fn handle_decimal_number(&mut self) -> Option<(Position, Position)> {
        let start = self.cs.pos();
        if let Some('+' | '-') = self.cs.current_char() {
            // Consumer + or -
            self.cs.advance_by(1);
        }

        self.cs.advance_while(1, |char| char.is_ascii_digit());
        let end = self.cs.pos();

        if start == end {
            None
        } else {
            Some((start, end))
        }
    }

    /// check if is a valid decimal e.g.: 123, 1_2_3, 1_2344, +1, -1
    /// invalid decimals: 1_2_, 1__2_3, 1234abc, --42
    fn check_if_decimal_is_valid_in_pos(&self, start: Position, end: Position) -> Result<(), PythonError> {
        const VALID_STATE: i8 = 2;
        const INVALID_STATE: i8 = 0;

        let mut curr_state: i8 = -1;

        for &char in self.cs.get_slice(start.index, end.index).unwrap() {
            if curr_state == INVALID_STATE && char == b'_' {
                break;
            }

            if char.is_ascii_digit() {
                curr_state = VALID_STATE;
            } else {
                curr_state = INVALID_STATE;
            }
        }

        if curr_state == INVALID_STATE {
            return Err(PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: invalid decimal literal".to_string(),
                span: self.make_span(start, end),
            });
        }

        Ok(())
    }

    /// Check if is a valid float syntax e.g.: .2, 1.3, 1_000e+40, 24., 3.14E-2, 1e5J
    /// https://docs.python.org/3/reference/lexical_analysis.html#floating-point-literals
    fn check_if_float_number_is_valid_in_pos(&self, start: Position, end: Position) -> Result<(), PythonError> {
        let mut state: u8 = 1;

        // This is a state machine that validates a float number string
        for &char in self.cs.get_slice(start.index, end.index).unwrap() {
            if (state == 4 || state == 1) && char == b'.'
                || state == 7 && matches!(char, b'j' | b'J')
                || state >= 5 && matches!(char, b'e' | b'E')
            {
                state = 0;
                break;
            }

            if state == 4 && matches!(char, b'e' | b'E') {
                state += 1;
                continue;
            }

            if (state == 2 || state == 4) && matches!(char, b'e' | b'E' | b'j' | b'J') {
                state += 3;
            }

            if matches!(state, 2 | 4 | 6) && char.is_ascii_digit() {
                continue;
            }

            if state == 5 && char.is_ascii_digit() || state == 6 && matches!(char, b'j' | b'J') || char.is_ascii_digit()
            {
                state += 1;
            }

            if char == b'.' {
                state += 2;
            }
        }

        if matches!(state, 4 | 6 | 7) {
            Ok(())
        } else {
            Err(PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: invalid float literal".to_string(),
                span: self.make_span(start, end),
            })
        }
    }

    fn make_span(&self, start: Position, end: Position) -> Span {
        Span {
            row_start: start.row,
            row_end: end.row,
            // Due to the column value starting at 0, we need to add 1 to get the correct column position
            column_start: start.column + 1,
            column_end: end.column,
        }
    }

    fn skip_comment(&mut self) {
        self.cs.advance_while(1, |char| !matches!(char, '\n' | '\r'));
    }
}
