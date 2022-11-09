mod char_codepoints;
mod char_stream;
pub mod token;

use char_stream::CharStream;
use token::Token;

use crate::{valid_id_initial_chars, valid_id_noninitial_chars};

use self::token::types::{KeywordType, OperatorType, SoftKeywordType, TokenType};

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

    pub fn tokenize(&mut self) {
        // This is used to check if we are inside a [], () or {} and then skip the NewLine Token.
        let mut implicit_line_joining = 0;

        while !self.cs.is_eof() {
            self.cs.skip_whitespace();

            match self.cs.current_char().unwrap() {
                valid_id_initial_chars!() => self.lex_identifier_or_keyword(),
                '0'..='9' => self.lex_number(),
                '"' | '\'' => self.lex_string(),
                '(' => {
                    implicit_line_joining += 1;
                    self.lex_single_char(TokenType::OpenParenthesis);
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
                '*' | '+' | '=' | '-' | '<' | '>' | '&' | '|' | '%' | '~' | '^' | '!' | '@' => {
                    if matches!((self.cs.current_char(), self.cs.next_char()), (Some('-'), Some('>'))) {
                        let start = self.cs.pos();
                        self.cs.advance_by(2);
                        let end = self.cs.pos();
                        self.tokens.push(Token::new(TokenType::RightArrow, start, end));
                    } else {
                        self.lex_operator();
                    }
                }
                '\n' | '\r' => {
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
                    let whitespace_total = self.cs.skip_whitespace();

                    self.handle_indentation(whitespace_total);
                }
                c => self.lex_single_char(TokenType::Invalid(c)),
            }
        }

        while self.indent_stack.last().copied().unwrap() > 0 {
            self.tokens.push(Token::new(TokenType::Dedent, 0, 0));
            self.indent_stack.pop();
        }

        self.tokens
            .push(Token::new(TokenType::Eof, self.cs.pos(), self.cs.pos() + 1));
    }

    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    // FIXME: Handle mix of tabs and spaces in indentation
    // TODO: Return error instead of panicking
    fn handle_indentation(&mut self, whitespace_total: usize) {
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
                    panic!("IndentError!")
                }
            }
            std::cmp::Ordering::Greater => {
                self.indent_stack.push(whitespace_total);
                self.tokens.push(Token::new(TokenType::Ident, 0, 0));
            }
            std::cmp::Ordering::Equal => (), // Do nothing!
        }
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

        if self.is_str_prefix(str) && self.cs.current_char().map_or(false, |char| matches!(char, '"' | '\'')) {
            self.lex_string();
            return;
        }

        let token_type = match str {
            b"and" => TokenType::Keyword(KeywordType::And),
            b"as" => TokenType::Keyword(KeywordType::As),
            b"assert" => TokenType::Keyword(KeywordType::Assert),
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
    //https://docs.python.org/3/reference/lexical_analysis.html#string-and-bytes-literals
    fn lex_string(&mut self) {
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

        while self.cs.current_char().map_or(false, |char| char != quote_char) {
            // Skip escaped `quote_char`
            if self.cs.current_char().map_or(false, |char| char == '\\')
                && self.cs.next_char().map_or(false, |char| char == quote_char)
            {
                self.cs.advance_by(2);
                continue;
            }

            self.cs.advance_by(1);
        }

        // Consume `"` or `'`
        while self.cs.current_char().map_or(false, |char| char == quote_char) {
            self.cs.advance_by(1);
            end_quote_total += 1;
        }
        let end = self.cs.pos();

        if start_quote_total != end_quote_total {
            panic!("Missing closing quote {quote_char}!")
        }

        self.tokens.push(Token::new(
            TokenType::String(
                String::from_utf8_lossy(
                    self.cs
                        .get_slice(start + start_quote_total..end - end_quote_total)
                        .unwrap(),
                )
                .into(),
            ),
            start,
            end,
        ));
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
            ('%', Some('=')) => (TokenType::Operator(OperatorType::ModulusEqual), 2),
            ('&', Some('=')) => (TokenType::Operator(OperatorType::BitwiseAndEqual), 2),
            ('|', Some('=')) => (TokenType::Operator(OperatorType::BitwiseOrEqual), 2),
            ('@', Some('=')) => (TokenType::Operator(OperatorType::AtEqual), 2),
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

            ('%', _) => (TokenType::Operator(OperatorType::Modulus), 1),
            ('&', _) => (TokenType::Operator(OperatorType::BitwiseAnd), 1),
            ('*', _) => (TokenType::Operator(OperatorType::Asterisk), 1),
            ('+', _) => (TokenType::Operator(OperatorType::Plus), 1),
            ('-', _) => (TokenType::Operator(OperatorType::Minus), 1),
            ('<', _) => (TokenType::Operator(OperatorType::LessThan), 1),
            ('=', _) => (TokenType::Operator(OperatorType::Assign), 1),
            ('>', _) => (TokenType::Operator(OperatorType::GreaterThan), 1),
            ('^', _) => (TokenType::Operator(OperatorType::BitwiseXOr), 1),
            ('|', _) => (TokenType::Operator(OperatorType::BitwiseOr), 1),
            ('~', _) => (TokenType::Operator(OperatorType::BitwiseNot), 1),
            ('@', _) => (TokenType::Operator(OperatorType::At), 1),

            ('!', _) => (TokenType::Invalid('!'), 1),
            (char, _) => (TokenType::Invalid(char), 1),
        };

        self.cs.advance_by(advance_offset);
        let end = self.cs.pos();
        self.tokens.push(Token::new(token_type, start, end));
    }

    fn lex_number(&mut self) {
        let start = self.cs.pos();
        while self
            .cs
            .current_char()
            .map_or(false, |char| char.is_ascii_digit() || char == '_')
        {
            self.cs.advance_by(1);
        }
        let end = self.cs.pos();

        let number = self.cs.get_slice(start..end).unwrap();
        self.tokens.push(Token::new(
            TokenType::Number(String::from_utf8_lossy(number).into()),
            start,
            end,
        ));
    }
}
