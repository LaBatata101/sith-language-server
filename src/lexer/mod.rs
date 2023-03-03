mod char_codepoints;
mod char_stream;
pub mod token;

use std::borrow::Cow;

use char_stream::CharStream;
use token::Token;

use crate::{
    error::{PythonError, PythonErrorType},
    lexer::token::Span,
    valid_id_initial_chars, valid_id_noninitial_chars,
};

use self::token::types::{IntegerType, KeywordType, NumberType, OperatorType, SoftKeywordType, TokenType};

pub struct Lexer<'a> {
    cs: CharStream<'a>,
    tokens: Vec<Token>,
    indent_stack: Vec<usize>,
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            cs: CharStream::new(text),
            tokens: Vec::new(),
            indent_stack: vec![0],
        }
    }

    pub fn tokenize(&mut self) -> Option<Vec<PythonError>> {
        // This is used to check if we are inside a [], () or {} and then skip the NewLine Token.
        let mut implicit_line_joining = 0;
        let mut errors: Vec<PythonError> = vec![];
        let mut is_beginning_of_line = true;

        while !self.cs.is_eof() {
            if is_beginning_of_line {
                is_beginning_of_line = false;

                let whitespace_total = self.cs.skip_whitespace();

                if self.cs.is_eof() {
                    break;
                }

                // Skip comments
                if self.cs.current_char().map_or(false, |char| char == '#') {
                    self.cs.advance_while(1, |char| char != '\n');
                }

                // skip lines containing only white spaces or \n
                // FIXME: handle \r and \r\n
                if self.cs.current_char().map_or(false, |char| char == '\n') && implicit_line_joining == 0 {
                    is_beginning_of_line = true;
                    // Consume \n
                    self.cs.advance_by(1);
                    continue;
                }

                if implicit_line_joining == 0 {
                    if let Err(error) = self.handle_indentation(whitespace_total) {
                        errors.push(error);
                    }
                }
            }

            match self.cs.current_char().unwrap() {
                valid_id_initial_chars!() => self.lex_identifier_or_keyword(),
                '0'..='9' => {
                    if let Some(number_errors) = self.lex_number() {
                        errors.extend(number_errors);
                    }
                }
                '"' | '\'' => {
                    if let Some(string_errors) = self.lex_string() {
                        errors.extend(string_errors);
                    }
                }
                '(' => {
                    implicit_line_joining += 1;

                    self.lex_single_char(TokenType::OpenParenthesis);

                    if self.cs.current_char().map_or(false, |char| matches!(char, '"' | '\'')) {
                        self.lex_string_within_parens();
                    }
                }
                ')' => {
                    implicit_line_joining -= 1;
                    self.lex_single_char(TokenType::CloseParenthesis);
                }
                '[' => {
                    implicit_line_joining += 1;
                    self.lex_single_char(TokenType::OpenBrackets);
                }
                ']' => {
                    implicit_line_joining -= 1;
                    self.lex_single_char(TokenType::CloseBrackets);
                }
                '{' => {
                    implicit_line_joining += 1;
                    self.lex_single_char(TokenType::OpenBrace);
                }
                '}' => {
                    implicit_line_joining -= 1;
                    self.lex_single_char(TokenType::CloseBrace);
                }
                '.' => {
                    if self.cs.next_char().map_or(false, |char| char.is_ascii_digit()) {
                        self.lex_number();
                        continue;
                    }

                    if matches!(
                        (self.cs.next_char(), self.cs.peek_char(self.cs.pos() + 2)),
                        (Some('.'), Some('.'))
                    ) {
                        let start = self.cs.pos();
                        self.cs.advance_by(3);
                        let end = self.cs.pos();
                        self.tokens.push(Token::new(TokenType::Ellipsis, start, end))
                    } else {
                        self.lex_single_char(TokenType::Dot)
                    }
                }
                ';' => self.lex_single_char(TokenType::SemiColon),
                ',' => self.lex_single_char(TokenType::Comma),
                ':' => {
                    if let Some('=') = self.cs.next_char() {
                        let start = self.cs.pos();
                        self.cs.advance_by(2);
                        let end = self.cs.pos();
                        self.tokens
                            .push(Token::new(TokenType::Operator(OperatorType::ColonEqual), start, end))
                    } else {
                        self.lex_single_char(TokenType::Colon)
                    }
                }
                '*' | '+' | '=' | '-' | '<' | '>' | '&' | '|' | '%' | '~' | '^' | '!' | '@' | '/' => {
                    if matches!((self.cs.current_char(), self.cs.next_char()), (Some('-'), Some('>'))) {
                        let start = self.cs.pos();
                        self.cs.advance_by(2);
                        let end = self.cs.pos();
                        self.tokens.push(Token::new(TokenType::RightArrow, start, end));
                    } else {
                        self.lex_operator();
                    }
                }
                ' ' => {
                    self.cs.skip_whitespace();
                }
                '\n' | '\r' => {
                    is_beginning_of_line = true;
                    let mut advance_offset = 1;

                    let start = self.cs.pos();

                    if self.cs.current_char().map_or(false, |char| char == '\n') {
                        self.cs.advance_by(advance_offset);
                    }

                    let mut end = self.cs.pos();

                    if self.cs.current_char().map_or(false, |char| char == '\r') {
                        if self.cs.next_char().map_or(false, |char| char == '\n') {
                            advance_offset = 2;
                        }
                        self.cs.advance_by(advance_offset);

                        end = self.cs.pos();
                    }

                    if implicit_line_joining > 0 {
                        continue;
                    }

                    self.tokens.push(Token::new(TokenType::NewLine, start, end));
                }
                '\\' => {
                    if self.cs.next_char().map_or(false, |char| char == '\n') {
                        self.cs.advance_by(2);
                    } else {
                        let start = self.cs.pos();
                        // consume \
                        self.cs.advance_by(1);
                        let end = self.cs.pos();

                        errors.push(PythonError {
                            msg: String::from("SyntaxError: unexpected characters after line continuation character"),
                            error: PythonErrorType::Syntax,
                            span: Span { start, end },
                        });
                    }
                }
                '#' => {
                    self.cs.advance_while(1, |char| char != '\n');
                }
                _ => {
                    let start = self.cs.pos();
                    self.cs.advance_by(1);
                    let end = self.cs.pos();

                    errors.push(PythonError {
                        error: PythonErrorType::Syntax,
                        msg: String::from("SyntaxError: invalid syntax, character only allowed inside string literals"),
                        span: Span { start, end },
                    })
                }
            }
        }

        while self.indent_stack.last().copied().unwrap() > 0 {
            self.tokens.push(Token::new(TokenType::Dedent, 0, 0));
            self.indent_stack.pop();
        }

        self.tokens
            .push(Token::new(TokenType::Eof, self.cs.pos(), self.cs.pos() + 1));

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
                    self.tokens.push(Token::new(TokenType::Dedent, 0, 0));
                }

                if self
                    .indent_stack
                    .last()
                    .map_or(false, |&top_of_stack| whitespace_total != top_of_stack)
                {
                    return Err(PythonError {
                        error: PythonErrorType::Indentation,
                        msg: "IndentError: indent amount does not match previous indent".to_string(),
                        span: Span { start: 0, end: 0 }, // FIXME: set the span position
                    });
                }
            }
            std::cmp::Ordering::Greater => {
                self.indent_stack.push(whitespace_total);
                self.tokens.push(Token::new(TokenType::Indent, 0, 0));
            }
            std::cmp::Ordering::Equal => (), // Do nothing!
        }

        Ok(())
    }

    fn lex_single_char(&mut self, token: TokenType) {
        let start = self.cs.pos();
        self.cs.advance_by(1);
        let end = self.cs.pos();

        self.tokens.push(Token::new(token, start, end));
    }

    fn lex_identifier_or_keyword(&mut self) {
        let start = self.cs.pos();
        while self
            .cs
            .current_char()
            .map_or(false, |char| matches!(char, valid_id_noninitial_chars!()))
        {
            self.cs.advance_by(1);
        }
        let end = self.cs.pos();

        let str = self.cs.get_slice(start..end).unwrap();

        // FIXME: check if the string is within a parenthesis and use `lex_string_within_parens`
        if self.is_str_prefix(str) && self.cs.current_char().map_or(false, |char| matches!(char, '"' | '\'')) {
            self.lex_string();
            return;
        }

        let token_type = match str {
            b"and" => TokenType::Keyword(KeywordType::And),
            b"as" => TokenType::Keyword(KeywordType::As),
            b"assert" => TokenType::Keyword(KeywordType::Assert),
            b"async" => TokenType::Keyword(KeywordType::Async),
            b"await" => TokenType::Keyword(KeywordType::Await),
            b"break" => TokenType::Keyword(KeywordType::Break),
            b"case" => TokenType::SoftKeyword(SoftKeywordType::Case),
            b"class" => TokenType::Keyword(KeywordType::Class),
            b"continue" => TokenType::Keyword(KeywordType::Continue),
            b"def" => TokenType::Keyword(KeywordType::Def),
            b"del" => TokenType::Keyword(KeywordType::Del),
            b"elif" => TokenType::Keyword(KeywordType::Elif),
            b"else" => TokenType::Keyword(KeywordType::Else),
            b"except" => TokenType::Keyword(KeywordType::Except),
            b"False" => TokenType::Keyword(KeywordType::False),
            b"finally" => TokenType::Keyword(KeywordType::Finally),
            b"for" => TokenType::Keyword(KeywordType::For),
            b"from" => TokenType::Keyword(KeywordType::From),
            b"global" => TokenType::Keyword(KeywordType::Global),
            b"if" => TokenType::Keyword(KeywordType::If),
            b"import" => TokenType::Keyword(KeywordType::Import),
            b"in" => TokenType::Keyword(KeywordType::In),
            b"is" => TokenType::Keyword(KeywordType::Is),
            b"lambda" => TokenType::Keyword(KeywordType::Lambda),
            b"match" => TokenType::SoftKeyword(SoftKeywordType::Match),
            b"None" => TokenType::Keyword(KeywordType::None),
            b"nonlocal" => TokenType::Keyword(KeywordType::NonLocal),
            b"not" => TokenType::Keyword(KeywordType::Not),
            b"or" => TokenType::Keyword(KeywordType::Or),
            b"pass" => TokenType::Keyword(KeywordType::Pass),
            b"raise" => TokenType::Keyword(KeywordType::Raise),
            b"return" => TokenType::Keyword(KeywordType::Return),
            b"True" => TokenType::Keyword(KeywordType::True),
            b"try" => TokenType::Keyword(KeywordType::Try),
            b"while" => TokenType::Keyword(KeywordType::While),
            b"with" => TokenType::Keyword(KeywordType::With),
            b"yield" => TokenType::Keyword(KeywordType::Yield),
            b"_" => TokenType::SoftKeyword(SoftKeywordType::Underscore),
            _ => TokenType::Id(String::from_utf8_lossy(str).into()),
        };

        self.tokens.push(Token::new(token_type, start, end));
    }

    fn is_str_prefix(&self, str: &[u8]) -> bool {
        matches!(
            str,
            // string prefixes
            b"r" | b"u" | b"R" | b"U" | b"f" | b"F" | b"fr" | b"Fr" | b"fR" | b"FR" | b"rf" | b"rF" | b"Rf" | b"RF"
            // bytes prefixes
            | b"b" | b"B" | b"br" | b"Br" | b"bR" | b"BR" | b"rb" | b"rB" | b"Rb" | b"RB"
        )
    }

    // TODO: Define a different string type for string prefixes
    // TODO: try to refactor this code
    //https://docs.python.org/3/reference/lexical_analysis.html#string-and-bytes-literals
    fn lex_string(&mut self) -> Option<Vec<PythonError>> {
        let (string, str_span, errors) = self.process_string();
        let string = string.into();

        self.tokens.push(Token {
            kind: TokenType::String(string),
            span: str_span,
        });

        errors
    }

    fn lex_string_within_parens(&mut self) -> Option<Vec<PythonError>> {
        let mut errors = Vec::new();
        let mut out_string = String::new();
        let mut str_span = Span {
            start: self.cs.pos(),
            ..Default::default()
        };

        while self.cs.current_char().map_or(false, |char| matches!(char, '"' | '\'')) {
            let (string, span, str_errors) = self.process_string();
            out_string.push_str(&string);
            str_span.end = span.end;

            if let Some(str_errors) = str_errors {
                errors.extend(str_errors);
            }

            // this is not good
            self.cs.skip_whitespace();
            if self.cs.current_char().map_or(false, |char| char == '\n') {
                self.cs.advance_by(1);
            }
            self.cs.skip_whitespace();
        }

        self.tokens.push(Token {
            kind: TokenType::String(out_string),
            span: str_span,
        });

        if errors.is_empty() {
            None
        } else {
            Some(errors)
        }
    }

    fn process_string(&mut self) -> (Cow<str>, Span, Option<Vec<PythonError>>) {
        let mut errors: Vec<PythonError> = vec![];

        let mut has_explicit_line_join = false;
        let mut backslash_positions = vec![];
        let mut start_quote_total = 0;
        let mut end_quote_total = 0;

        // Can be `"` or `'`
        let quote_char = self.cs.current_char().unwrap();

        let start = self.cs.pos();
        // Consume `"` or `'`
        while self.cs.current_char().map_or(false, |char| char == quote_char) {
            self.cs.advance_by(1);
            start_quote_total += 1;
        }

        if start_quote_total == 3 {
            loop {
                self.cs.advance_by(1);

                if self.cs.current_char().map_or(false, |char| char == quote_char) {
                    let quote_pos = self.cs.pos();
                    if matches!(
                        (
                            self.cs.peek_char(quote_pos),
                            self.cs.peek_char(quote_pos + 1),
                            self.cs.peek_char(quote_pos + 2)
                        ),
                        (Some('"'), Some('"'), Some('"')) | (Some('\''), Some('\''), Some('\''))
                    ) {
                        break;
                    }
                }
            }
        } else {
            while self
                .cs
                .current_char()
                .map_or(false, |char| char != quote_char && char != '\n')
            {
                if self.cs.current_char().map_or(false, |char| char == '\\') {
                    // store the backslashes positions
                    if self.cs.next_char().map_or(false, |char| char == '\n') {
                        has_explicit_line_join = true;
                        backslash_positions.push(self.cs.pos());
                        self.cs.advance_by(2);
                    }

                    // Skip escaped `quote_char`
                    if self.cs.next_char().map_or(false, |char| char == quote_char) {
                        self.cs.advance_by(2);
                        continue;
                    }
                }
                self.cs.advance_by(1);
            }
        }

        // Consume `"` or `'`
        while self.cs.current_char().map_or(false, |char| char == quote_char) {
            self.cs.advance_by(1);
            end_quote_total += 1;
        }
        let end = self.cs.pos();

        if start_quote_total != end_quote_total {
            errors.push(PythonError {
                error: PythonErrorType::Syntax,
                msg: String::from("SyntaxError: unterminated string literal"),
                span: Span { start, end },
            });
        }

        let out_string = if has_explicit_line_join {
            let mut str: Cow<str> = Cow::default();
            // string literals are the only type of Token that can be split across lines,
            // so we need to take every string char until the backslash, for every string line that
            // has a backslash, to join as one string.
            for backslash_position in &backslash_positions {
                str.to_mut().push_str(&String::from_utf8_lossy(
                    self.cs
                        .get_slice(start + start_quote_total..*backslash_position)
                        .unwrap(),
                ));
            }
            // Here we join the last string line that doesn't have the backslash.
            str.to_mut().push_str(&String::from_utf8_lossy(
                self.cs
                    .get_slice(*backslash_positions.last().unwrap() + 2..end - end_quote_total)
                    .unwrap(),
            ));

            str
        } else {
            String::from_utf8_lossy(
                self.cs
                    .get_slice(start + start_quote_total..end - end_quote_total)
                    .unwrap(),
            )
        };

        (
            out_string,
            Span { start, end },
            if errors.is_empty() { None } else { Some(errors) },
        )
    }

    fn lex_operator(&mut self) {
        let start = self.cs.pos();
        let (token_type, advance_offset) = match (self.cs.current_char().unwrap(), self.cs.next_char()) {
            ('=', Some('=')) => (TokenType::Operator(OperatorType::Equals), 2),
            ('+', Some('=')) => (TokenType::Operator(OperatorType::PlusEqual), 2),
            ('*', Some('=')) => (TokenType::Operator(OperatorType::AsteriskEqual), 2),
            ('*', Some('*')) => (TokenType::Operator(OperatorType::Exponent), 2),
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
                if self.cs.peek_char(self.cs.pos() + 2).map_or(false, |char| char == '=') {
                    (TokenType::Operator(OperatorType::FloorDivisionEqual), 3)
                } else {
                    (TokenType::Operator(OperatorType::FloorDivision), 2)
                }
            }
            ('<', Some('<')) => {
                if self.cs.peek_char(self.cs.pos() + 2).map_or(false, |char| char == '=') {
                    (TokenType::Operator(OperatorType::BitwiseLeftShiftEqual), 3)
                } else {
                    (TokenType::Operator(OperatorType::BitwiseLeftShift), 2)
                }
            }
            ('>', Some('>')) => {
                if self.cs.peek_char(self.cs.pos() + 2).map_or(false, |char| char == '=') {
                    (TokenType::Operator(OperatorType::BitwiseRightShiftEqual), 3)
                } else {
                    (TokenType::Operator(OperatorType::BitwiseRightShift), 2)
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
        self.tokens.push(Token::new(token_type, start, end));
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
            self.cs
                .advance_while(1, |char| matches!(char, valid_id_noninitial_chars!()));

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

        let number = self.cs.get_slice(start..end).unwrap();
        self.tokens.push(Token::new(
            TokenType::Number(number_type, String::from_utf8_lossy(number).into()),
            start,
            end,
        ));

        if errors.is_empty() {
            None
        } else {
            Some(errors)
        }
    }

    // Handle the fraction part of the float number
    fn handle_float_or_imaginary_number(&mut self, float_start: usize) -> (NumberType, Vec<PythonError>) {
        let mut errors = vec![];

        // Here we are consuming all the valid characters that a float string can have, we don't care
        // at this point if the string is valid, the checking if the string is a valid float is done
        // later. The goal is to keep the float string in one Token, even if it is an invalid float.
        while self.cs.current_char().map_or(false, |char| {
            char.is_ascii_digit() || matches!(char, '.' | 'e' | 'E' | '-' | '+' | '_')
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
                self.cs.advance_by(1);
            }
        }

        if matches!(self.cs.current_char(), Some('j' | 'J')) {
            // Consume j or J
            self.cs.advance_by(1);

            // In case we have multiple j's, consume all of them.
            if matches!(self.cs.current_char(), Some('j' | 'J')) {
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

    fn handle_decimal_number(&mut self) -> Option<(usize, usize)> {
        let start = self.cs.pos();
        if matches!(self.cs.current_char(), Some('+' | '-')) {
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
    fn check_if_decimal_is_valid_in_pos(&self, start: usize, end: usize) -> Result<(), PythonError> {
        const VALID_STATE: i8 = 2;
        const INVALID_STATE: i8 = 0;

        let mut curr_state: i8 = -1;

        for &char in self.cs.get_slice(start..end).unwrap() {
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
                span: Span { start, end },
            });
        }

        Ok(())
    }

    /// Check if is a valid float syntax e.g.: .2, 1.3, 1_000e+40, 24., 3.14E-2, 1e5J
    /// https://docs.python.org/3/reference/lexical_analysis.html#floating-point-literals
    fn check_if_float_number_is_valid_in_pos(&self, start: usize, end: usize) -> Result<(), PythonError> {
        let mut state: u8 = 1;

        // This is a state machine that validates a float number string
        for &char in self.cs.get_slice(start..end).unwrap() {
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
                span: Span { start, end },
            })
        }
    }
}
