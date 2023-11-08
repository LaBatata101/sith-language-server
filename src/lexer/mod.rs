// This module takes care of lexing Python source text.
//
// This means source code is scanned and translated into separate tokens. The rules
// governing what is and is not a valid token are defined in the Python reference
// guide section on [Lexical analysis].
//
// The primary function in this module is [`lex`], which takes a string slice
// and returns an iterator over the tokens in the source code. The tokens are currently returned
// as a `Result<Spanned, LexicalError>`, where [`Spanned`] is a tuple containing the
// start and end [`TextSize`] and a [`Tok`] denoting the token.
//
// # Example
//
// ```
// use ruff_python_parser::{lexer::lex, Tok, Mode, StringKind};
//
// let source = "x = 'RustPython'";
// let tokens = lex(source, Mode::Module)
//     .map(|tok| tok.expect("Failed to lex"))
//     .collect::<Vec<_>>();
//
// for (token, range) in tokens {
//     println!(
//         "{token:?}@{range:?}",
//     );
// }
// ```
//
// [Lexical analysis]: https://docs.python.org/3/reference/lexical_analysis.html

use std::fmt::{Debug, Display};
use std::{char, cmp::Ordering};

use ruff_text_size::{TextRange, TextSize};

use cursor::{Cursor, EOF_CHAR};
use indentation::{Indentation, Indentations};

use crate::lexer::fstring::{FStringContext, FStringContextFlags};

use self::fstring::FStrings;
use self::types::{KeywordKind, NumberKind, OperatorKind, SoftKeywordKind, StringKind, TokenKind};

use self::helpers::{
    is_ascii_identifier_start, is_binary_number_valid, is_decimal_number_valid, is_float_number_valid,
    is_hex_number_valid, is_identifier_continuation, is_octal_number_valid, is_quote, is_unicode_identifier_start,
};
use self::soft_keywords::SoftKeywordTransformer;

mod cursor;
mod fstring;
mod helpers;
mod indentation;
mod soft_keywords;
pub mod types;

/// A lexer for Python source code.
#[derive(Debug, Clone)]
pub struct Lexer<'source> {
    // Contains the source code to be lexed.
    cursor: Cursor<'source>,
    source: &'source str,

    state: State,
    // Amount of parenthesis.
    nesting: u32,
    // Indentation levels.
    indentations: Indentations,
    pending_indentation: Option<Indentation>,
    // f-string contexts.
    fstrings: FStrings,
}

/// Contains a Token along with its `range`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token(TokenKind, TextRange);
/// The result of lexing a token.
pub type LexResult = Result<Token, LexicalError>;

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {:?}", self.0, self.1)
    }
}

impl Token {
    pub fn new(kind: TokenKind, range: TextRange) -> Self {
        Self(kind, range)
    }

    #[inline]
    pub fn kind(&self) -> TokenKind {
        self.0
    }

    #[inline]
    pub fn range(&self) -> TextRange {
        self.1
    }
}

// Create a new lexer from a source string.
//
// # Examples
//
// ```
// use ruff_python_parser::{Mode, lexer::lex};
//
// let source = "def hello(): return 'world'";
// let lexer = lex(source, Mode::Module);
//
// for token in lexer {
//    println!("{:?}", token);
// }
// ```
#[inline]
pub fn lex(source: &str) -> SoftKeywordTransformer<Lexer> {
    SoftKeywordTransformer::new(Lexer::new(source))
}

impl<'source> Lexer<'source> {
    /// Create a new lexer from T and a starting location. You probably want to use
    /// [`lex`] instead.
    pub fn new(input: &'source str) -> Self {
        assert!(
            u32::try_from(input.len()).is_ok(),
            "Lexer only supports files with a size up to 4GB"
        );

        let mut lexer = Lexer {
            state: State::AfterNewline,
            nesting: 0,
            indentations: Indentations::default(),
            pending_indentation: None,

            source: input,
            cursor: Cursor::new(input),
            fstrings: FStrings::default(),
        };
        // TODO: Handle possible mismatch between BOM and explicit encoding declaration.
        lexer.cursor.eat_char('\u{feff}');

        lexer
    }

    /// Lex an identifier. Also used for keywords and string/bytes literals with a prefix.
    fn lex_identifier(&mut self, first: char) -> Result<TokenKind, LexicalError> {
        // Detect potential string like rb'' b'' f'' u'' r''
        match (first, self.cursor.first()) {
            ('f' | 'F', quote @ ('\'' | '"')) => {
                self.cursor.bump();
                return Ok(self.lex_fstring_start(quote, false));
            }
            ('r' | 'R', 'f' | 'F') | ('f' | 'F', 'r' | 'R') if is_quote(self.cursor.second()) => {
                self.cursor.bump();
                let quote = self.cursor.bump().unwrap();
                return Ok(self.lex_fstring_start(quote, true));
            }
            (_, quote @ ('\'' | '"')) => {
                if let Ok(string_kind) = StringKind::try_from(first) {
                    self.cursor.bump();
                    return self.lex_string(string_kind, quote);
                }
            }
            (_, second @ ('r' | 'R' | 'b' | 'B')) if is_quote(self.cursor.second()) => {
                self.cursor.bump();

                if let Ok(string_kind) = StringKind::try_from([first, second]) {
                    let quote = self.cursor.bump().unwrap();
                    return self.lex_string(string_kind, quote);
                }
            }
            _ => {}
        }

        self.cursor.eat_while(is_identifier_continuation);

        let text = self.token_text();

        let keyword = match text {
            "and" => TokenKind::Keyword(KeywordKind::And),
            "as" => TokenKind::Keyword(KeywordKind::As),
            "assert" => TokenKind::Keyword(KeywordKind::Assert),
            "async" => TokenKind::Keyword(KeywordKind::Async),
            "await" => TokenKind::Keyword(KeywordKind::Await),
            "break" => TokenKind::Keyword(KeywordKind::Break),
            "case" => TokenKind::SoftKeyword(SoftKeywordKind::Case),
            "class" => TokenKind::Keyword(KeywordKind::Class),
            "continue" => TokenKind::Keyword(KeywordKind::Continue),
            "def" => TokenKind::Keyword(KeywordKind::Def),
            "del" => TokenKind::Keyword(KeywordKind::Del),
            "elif" => TokenKind::Keyword(KeywordKind::Elif),
            "else" => TokenKind::Keyword(KeywordKind::Else),
            "except" => TokenKind::Keyword(KeywordKind::Except),
            "False" => TokenKind::Keyword(KeywordKind::False),
            "finally" => TokenKind::Keyword(KeywordKind::Finally),
            "for" => TokenKind::Keyword(KeywordKind::For),
            "from" => TokenKind::Keyword(KeywordKind::From),
            "global" => TokenKind::Keyword(KeywordKind::Global),
            "if" => TokenKind::Keyword(KeywordKind::If),
            "import" => TokenKind::Keyword(KeywordKind::Import),
            "in" => TokenKind::Keyword(KeywordKind::In),
            "is" => TokenKind::Keyword(KeywordKind::Is),
            "lambda" => TokenKind::Keyword(KeywordKind::Lambda),
            "match" => TokenKind::SoftKeyword(SoftKeywordKind::Match),
            "None" => TokenKind::Keyword(KeywordKind::None),
            "nonlocal" => TokenKind::Keyword(KeywordKind::NonLocal),
            "not" => TokenKind::Keyword(KeywordKind::Not),
            "or" => TokenKind::Keyword(KeywordKind::Or),
            "pass" => TokenKind::Keyword(KeywordKind::Pass),
            "raise" => TokenKind::Keyword(KeywordKind::Raise),
            "return" => TokenKind::Keyword(KeywordKind::Return),
            "True" => TokenKind::Keyword(KeywordKind::True),
            "try" => TokenKind::Keyword(KeywordKind::Try),
            "type" => TokenKind::SoftKeyword(SoftKeywordKind::Type),
            "while" => TokenKind::Keyword(KeywordKind::While),
            "with" => TokenKind::Keyword(KeywordKind::With),
            "yield" => TokenKind::Keyword(KeywordKind::Yield),
            _ => return Ok(TokenKind::Id),
        };

        Ok(keyword)
    }

    /// Numeric lexing. The feast can start!
    fn lex_number(&mut self, first: char) -> Result<TokenKind, LexicalError> {
        if first == '0' {
            if self.cursor.eat_if(|c| matches!(c, 'x' | 'X')).is_some() {
                self.lex_number_radix(Radix::Hex)
            } else if self.cursor.eat_if(|c| matches!(c, 'o' | 'O')).is_some() {
                self.lex_number_radix(Radix::Octal)
            } else if self.cursor.eat_if(|c| matches!(c, 'b' | 'B')).is_some() {
                self.lex_number_radix(Radix::Binary)
            } else {
                self.lex_decimal_number(first)
            }
        } else {
            self.lex_decimal_number(first)
        }
    }

    /// Lex a hex/octal/decimal/binary number without a decimal point.
    fn lex_number_radix(&mut self, radix: Radix) -> Result<TokenKind, LexicalError> {
        // #[cfg(debug_assertions)]
        // debug_assert!(matches!(self.cursor.previous().to_ascii_lowercase(), 'x' | 'o' | 'b'));

        self.cursor.eat_while(|char| radix.is_digit(char) || char == '_');

        // If we find a valid identifier character after consuming the number,
        // consume all of that, unless is one of the characters that can be
        // found in a float number (`e`, `E`, `j` and `J`).
        if is_unicode_identifier_start(self.cursor.first()) {
            self.cursor.eat_while(|char| {
                !matches!(char, 'e' | 'E' | 'j' | 'J')
                    && (char == '_' || char.is_ascii_digit() || is_identifier_continuation(char))
            });
        }

        Ok(TokenKind::Number(
            radix
                .try_number_kind(self.token_text())
                .map_err(|error| LexicalError::new(error, self.token_range()))?,
        ))
    }

    /// Lex a normal number, that is, no octal, hex or binary number.
    fn lex_decimal_number(&mut self, first_digit_or_dot: char) -> Result<TokenKind, LexicalError> {
        #[cfg(debug_assertions)]
        debug_assert!(self.cursor.previous().is_ascii_digit() || self.cursor.previous() == '.');

        // For cases where the float number doesn't have the decimal, e.g. .1, .1314
        if matches!(first_digit_or_dot, '.') {
            return self.lex_float_number();
        }

        let decimal_number = self.lex_number_radix(Radix::Decimal);

        // After lexing the decimal number we may encounter a `.`, `e` or `E`.
        // This means the number is a float number.
        if matches!(self.cursor.first(), '.' | 'e' | 'E') {
            return self.lex_float_number();
        } else if self.cursor.eat_if(|char| matches!(char, 'j' | 'J')).is_some() {
            if is_float_number_valid(self.token_text()) {
                return Ok(TokenKind::Number(NumberKind::Complex));
            } else {
                self.state = State::Other;
                return Err(LexicalError::new(
                    LexicalErrorType::InvalidNumberError(InvalidNumberErrorKind::Complex),
                    self.token_range(),
                ));
            }
        }

        decimal_number
    }

    fn lex_float_number(&mut self) -> Result<TokenKind, LexicalError> {
        // Consume `.`, e` or `E`.
        if matches!(self.cursor.bump(), Some('e' | 'E')) {
            self.cursor.eat_if(|char| matches!(char, '-' | '+'));
        }

        let _ = self.lex_number_radix(Radix::Decimal);

        if self.cursor.eat_if(|char| matches!(char, 'e' | 'E')).is_some() {
            self.cursor.eat_if(|char| matches!(char, '-' | '+'));
            let _ = self.lex_number_radix(Radix::Decimal);
        }

        let is_complex = self.cursor.eat_if(|char| matches!(char, 'j' | 'J')).is_some();

        if is_float_number_valid(self.token_text()) {
            let number_kind = if is_complex {
                NumberKind::Complex
            } else {
                NumberKind::Float
            };

            Ok(TokenKind::Number(number_kind))
        } else {
            self.state = State::Other;
            let error = if is_complex {
                LexicalErrorType::InvalidNumberError(InvalidNumberErrorKind::Complex)
            } else {
                LexicalErrorType::InvalidNumberError(InvalidNumberErrorKind::Float)
            };
            Err(LexicalError::new(error, self.token_range()))
        }
    }

    /// Lex a string literal.
    fn lex_string(&mut self, kind: StringKind, quote: char) -> Result<TokenKind, LexicalError> {
        #[cfg(debug_assertions)]
        debug_assert_eq!(self.cursor.previous(), quote);

        // If the next two characters are also the quote character, then we have a triple-quoted
        // string; consume those two characters and ensure that we require a triple-quote to close
        let triple_quoted = self.cursor.eat_char2(quote, quote);

        loop {
            match self.cursor.first() {
                '\\' => {
                    self.cursor.bump();
                    if self.cursor.eat_char('\r') {
                        self.cursor.eat_char('\n');
                    } else {
                        self.cursor.bump();
                    }
                }
                '\r' | '\n' if !triple_quoted => {
                    self.state = State::Other;
                    if let Some(fstring) = self.fstrings.current() {
                        // When we are in an f-string, check whether does the initial quote
                        // matches with f-strings quotes and if it is, then this must be a
                        // missing '}' token so raise the proper error.
                        if fstring.quote_char() == quote && !fstring.is_triple_quoted() {
                            return Err(LexicalError {
                                error: LexicalErrorType::FStringError(FStringErrorType::UnclosedLbrace),
                                range: self.token_range(),
                            });
                        }
                    }
                    return Err(LexicalError::new(
                        LexicalErrorType::UnclosedStringError,
                        self.token_range(),
                    ));
                }
                c if c == quote => {
                    self.cursor.bump();
                    if triple_quoted {
                        if self.cursor.eat_char2(quote, quote) {
                            break;
                        }
                    } else {
                        break;
                    }
                }
                EOF_CHAR => {
                    self.state = State::Other;
                    if let Some(fstring) = self.fstrings.current() {
                        // When we are in an f-string, check whether does the initial quote
                        // matches with f-strings quotes and if it is, then this must be a
                        // missing '}' token so raise the proper error.
                        if fstring.quote_char() == quote && fstring.is_triple_quoted() == triple_quoted {
                            return Err(LexicalError {
                                error: LexicalErrorType::FStringError(FStringErrorType::UnclosedLbrace),
                                range: self.token_range(),
                            });
                        }
                    }
                    return Err(LexicalError::new(
                        LexicalErrorType::UnclosedStringError,
                        self.token_range(),
                    ));
                }
                _ => {
                    self.cursor.bump();
                }
            }
        }

        Ok(TokenKind::String {
            kind,
            is_triple_quote: triple_quoted,
        })
    }

    fn lex_fstring_start(&mut self, quote: char, is_raw_string: bool) -> TokenKind {
        #[cfg(debug_assertions)]
        debug_assert_eq!(self.cursor.previous(), quote);

        let mut flags = FStringContextFlags::empty();
        if quote == '"' {
            flags |= FStringContextFlags::DOUBLE;
        }
        if is_raw_string {
            flags |= FStringContextFlags::RAW;
        }
        if self.cursor.eat_char2(quote, quote) {
            flags |= FStringContextFlags::TRIPLE;
        }

        self.fstrings.push(FStringContext::new(flags, self.nesting));
        TokenKind::FStringStart
    }

    fn lex_fstring_middle_or_end(&mut self) -> Result<Option<TokenKind>, LexicalError> {
        let fstring = self.fstrings.current().unwrap();
        self.cursor.start_token();

        // Check if we're at the end of the f-string
        if fstring.is_triple_quoted() {
            let quote_char = fstring.quote_char();
            if self.cursor.eat_char3(quote_char, quote_char, quote_char) {
                return Ok(Some(TokenKind::FStringEnd));
            }
        } else if self.cursor.eat_char(fstring.quote_char()) {
            return Ok(Some(TokenKind::FStringEnd));
        }

        // This isn't going to change for the duration of the loop.
        let in_format_spec = fstring.is_in_format_spec(self.nesting);
        let mut in_named_unicode = false;

        loop {
            match self.cursor.first() {
                EOF_CHAR => {
                    let error = if fstring.is_triple_quoted() {
                        FStringErrorType::UnterminatedTripleQuotedString
                    } else {
                        FStringErrorType::UnterminatedString
                    };
                    return Err(LexicalError {
                        error: LexicalErrorType::FStringError(error),
                        range: self.token_range(),
                    });
                }
                '\n' | '\r' if !fstring.is_triple_quoted() => {
                    // If we encounter a newline while we're in a format spec, then
                    // we stop here and let the lexer emit the newline token.
                    //
                    // Relevant discussion: https://github.com/python/cpython/issues/110259
                    if in_format_spec {
                        break;
                    }
                    return Err(LexicalError {
                        error: LexicalErrorType::FStringError(FStringErrorType::UnterminatedString),
                        range: self.token_range(),
                    });
                }
                '\\' => {
                    self.cursor.bump(); // '\'
                    if matches!(self.cursor.first(), '{' | '}') {
                        // Don't consume `{` or `}` as we want them to be emitted as tokens.
                        continue;
                    } else if !fstring.is_raw_string() && self.cursor.eat_char2('N', '{') {
                        in_named_unicode = true;
                        continue;
                    }
                    // Consume the escaped character.
                    if self.cursor.eat_char('\r') {
                        self.cursor.eat_char('\n');
                    } else {
                        self.cursor.bump();
                    }
                }
                quote @ ('\'' | '"') if quote == fstring.quote_char() => {
                    if let Some(triple_quotes) = fstring.triple_quotes() {
                        if self.cursor.rest().starts_with(triple_quotes) {
                            break;
                        }
                        self.cursor.bump();
                    } else {
                        break;
                    }
                }
                '{' => {
                    if self.cursor.second() == '{' && !in_format_spec {
                        self.cursor.bump();
                        self.cursor.bump(); // Skip the second `{`
                    } else {
                        break;
                    }
                }
                '}' => {
                    if in_named_unicode {
                        in_named_unicode = false;
                        self.cursor.bump();
                    } else if self.cursor.second() == '}' && !in_format_spec {
                        self.cursor.bump();
                        self.cursor.bump(); // Skip the second `}`
                    } else {
                        break;
                    }
                }
                _ => {
                    self.cursor.bump();
                }
            }
        }
        if self.token_range().is_empty() {
            return Ok(None);
        }

        let is_raw = fstring.is_raw_string();
        Ok(Some(TokenKind::FStringMiddle { is_raw }))
    }

    // This is the main entry point. Call this function to retrieve the next token.
    // This function is used by the iterator implementation.
    pub fn next_token(&mut self) -> LexResult {
        if let Some(fstring) = self.fstrings.current() {
            if !fstring.is_in_expression(self.nesting) {
                match self.lex_fstring_middle_or_end() {
                    Ok(Some(token)) => {
                        if token == TokenKind::FStringEnd {
                            self.fstrings.pop();
                        }
                        return Ok(Token(token, self.token_range()));
                    }
                    Err(e) => {
                        // This is to prevent an infinite loop in which the lexer
                        // continuously returns an error token because the f-string
                        // remains on the stack.
                        self.fstrings.pop();
                        return Err(e);
                    }
                    _ => {}
                }
            }
        }

        // Return dedent tokens until the current indentation level matches the indentation of the next token.
        if let Some(indentation) = self.pending_indentation.take() {
            match self.indentations.current().try_compare(indentation) {
                Ok(Ordering::Greater) => {
                    self.pending_indentation = Some(indentation);
                    let offset = self.offset();
                    self.indentations
                        .dedent_one(indentation)
                        .map_err(|_| LexicalError::new(LexicalErrorType::IndentationError, self.token_range()))?;
                    return Ok(Token(TokenKind::Dedent, TextRange::empty(offset)));
                }
                Ok(_) => {}
                Err(_) => {
                    return Err(LexicalError::new(
                        LexicalErrorType::IndentationError,
                        self.token_range(),
                    ));
                }
            }
        }

        if self.state.is_after_newline() {
            if let Some(indentation) = self.eat_indentation()? {
                return Ok(indentation);
            }
        } else {
            self.skip_whitespace()?;
        }

        self.cursor.start_token();
        if let Some(c) = self.cursor.bump() {
            if c.is_ascii() {
                self.consume_ascii_character(c)
            } else if is_unicode_identifier_start(c) {
                let identifier = self.lex_identifier(c)?;
                self.state = State::Other;

                Ok(Token(identifier, self.token_range()))
            } else {
                Err(LexicalError::new(
                    LexicalErrorType::UnrecognizedToken,
                    self.token_range(),
                ))
            }
        } else {
            // Reached the end of the file. Emit a trailing newline token if not at the beginning of a logical line,
            // empty the dedent stack, and finally, return the EndOfFile token.
            self.consume_end()
        }
    }

    fn skip_whitespace(&mut self) -> Result<(), LexicalError> {
        loop {
            match self.cursor.first() {
                ' ' => {
                    self.cursor.bump();
                }
                '\t' => {
                    self.cursor.bump();
                }
                '\\' => {
                    self.cursor.bump();
                    if self.cursor.eat_char('\r') {
                        self.cursor.eat_char('\n');
                    } else if self.cursor.is_eof() {
                        return Err(LexicalError::new(LexicalErrorType::Eof, self.token_range()));
                    } else if !self.cursor.eat_char('\n') {
                        // bump the cursor to avoid creating a `Id` token for this character
                        self.cursor.bump();
                        return Err(LexicalError::new(
                            LexicalErrorType::LineContinuationError,
                            TextRange::new(
                                self.offset().checked_sub(1.into()).expect("subtraction overflowed"),
                                self.offset(),
                            ),
                        ));
                    }
                }
                // Form feed
                '\x0C' => {
                    self.cursor.bump();
                }
                _ => break,
            }
        }

        Ok(())
    }

    fn eat_indentation(&mut self) -> Result<Option<Token>, LexicalError> {
        let mut indentation = Indentation::root();
        self.cursor.start_token();

        loop {
            match self.cursor.first() {
                ' ' => {
                    self.cursor.bump();
                    indentation = indentation.add_space();
                }
                '\t' => {
                    self.cursor.bump();
                    indentation = indentation.add_tab();
                }
                '\\' => {
                    self.cursor.bump();
                    if self.cursor.eat_char('\r') {
                        self.cursor.eat_char('\n');
                    } else if self.cursor.is_eof() {
                        return Err(LexicalError::new(LexicalErrorType::Eof, self.token_range()));
                    } else if !self.cursor.eat_char('\n') {
                        // bump the cursor to avoid creating a `Id` token for this character
                        self.cursor.bump();
                        return Err(LexicalError::new(
                            LexicalErrorType::LineContinuationError,
                            TextRange::new(
                                self.offset().checked_sub(1.into()).expect("subtraction overflowed"),
                                self.offset(),
                            ),
                        ));
                    }
                    indentation = Indentation::root();
                }
                // Form feed
                '\x0C' => {
                    self.cursor.bump();
                    indentation = Indentation::root();
                }
                _ => break,
            }
        }

        if self.state.is_after_newline() {
            // Handle indentation if this is a new, not all empty, logical line
            if !matches!(self.cursor.first(), '\n' | '\r' | '#' | EOF_CHAR) {
                self.state = State::NonEmptyLogicalLine;

                if let Some(token) = self.handle_indentation(indentation)? {
                    // Set to false so that we don't handle indentation on the next call.

                    return Ok(Some(token));
                }
            }
        }

        Ok(None)
    }

    fn handle_indentation(&mut self, indentation: Indentation) -> Result<Option<Token>, LexicalError> {
        let token = match self.indentations.current().try_compare(indentation) {
            // Dedent
            Ok(Ordering::Greater) => {
                self.pending_indentation = Some(indentation);

                self.indentations
                    .dedent_one(indentation)
                    .map_err(|_| LexicalError::new(LexicalErrorType::IndentationError, self.token_range()))?;
                Some(Token(TokenKind::Dedent, TextRange::empty(self.offset())))
            }

            Ok(Ordering::Equal) => None,

            // Indent
            Ok(Ordering::Less) => {
                self.indentations.indent(indentation);
                Some(Token(TokenKind::Indent, self.token_range()))
            }
            Err(_) => {
                return Err(LexicalError::new(
                    LexicalErrorType::IndentationError,
                    self.token_range(),
                ));
            }
        };

        Ok(token)
    }

    fn consume_end(&mut self) -> Result<Token, LexicalError> {
        // We reached end of file.
        // First of all, we need all nestings to be finished.
        if self.nesting > 0 {
            self.nesting = 0;
        }

        // Clear any remaining fstrings contexts
        if !self.fstrings.is_empty() {
            self.fstrings.clear();
        }

        // Next, insert a trailing newline, if required.
        if !self.state.is_new_logical_line() {
            self.state = State::AfterNewline;
            Ok(Token(TokenKind::NewLine, TextRange::empty(self.offset())))
        }
        // Next, flush the indentation stack to zero.
        else if self.indentations.dedent().is_some() {
            Ok(Token(TokenKind::Dedent, TextRange::empty(self.offset())))
        } else {
            Ok(Token(TokenKind::Eof, TextRange::empty(self.offset())))
        }
    }

    fn lex_comment(&mut self) -> Result<Token, LexicalError> {
        #[cfg(debug_assertions)]
        debug_assert_eq!(self.cursor.previous(), '#');

        let bytes = self.cursor.rest().as_bytes();
        let offset = memchr::memchr2(b'\n', b'\r', bytes).unwrap_or(bytes.len());
        self.cursor.skip_bytes(offset);

        Ok(Token(TokenKind::Comment, self.token_range()))
    }

    // Dispatch based on the given character.
    fn consume_ascii_character(&mut self, c: char) -> Result<Token, LexicalError> {
        let token = match c {
            c if is_ascii_identifier_start(c) => self.lex_identifier(c)?,
            '0'..='9' => self.lex_number(c)?,
            '#' => return self.lex_comment(),
            '"' | '\'' => self.lex_string(StringKind::String, c)?,
            '=' => {
                if self.cursor.eat_char('=') {
                    TokenKind::Operator(OperatorKind::Equals)
                } else {
                    self.state = State::AfterEqual;
                    return Ok(Token(TokenKind::Operator(OperatorKind::Assign), self.token_range()));
                }
            }
            '+' => {
                if self.cursor.eat_char('=') {
                    TokenKind::Operator(OperatorKind::PlusEqual)
                } else {
                    TokenKind::Operator(OperatorKind::Plus)
                }
            }
            '*' => {
                if self.cursor.eat_char('=') {
                    TokenKind::Operator(OperatorKind::AsteriskEqual)
                } else if self.cursor.eat_char('*') {
                    if self.cursor.eat_char('=') {
                        TokenKind::Operator(OperatorKind::ExponentEqual)
                    } else {
                        TokenKind::Operator(OperatorKind::Exponent)
                    }
                } else {
                    TokenKind::Operator(OperatorKind::Asterisk)
                }
            }
            '/' => {
                if self.cursor.eat_char('=') {
                    TokenKind::Operator(OperatorKind::DivideEqual)
                } else if self.cursor.eat_char('/') {
                    if self.cursor.eat_char('=') {
                        TokenKind::Operator(OperatorKind::FloorDivisionEqual)
                    } else {
                        TokenKind::Operator(OperatorKind::DoubleSlash)
                    }
                } else {
                    TokenKind::Operator(OperatorKind::Slash)
                }
            }
            '%' => {
                if self.cursor.eat_char('=') {
                    TokenKind::Operator(OperatorKind::ModulusEqual)
                } else {
                    TokenKind::Operator(OperatorKind::Modulus)
                }
            }
            '|' => {
                if self.cursor.eat_char('=') {
                    TokenKind::Operator(OperatorKind::BitwiseOrEqual)
                } else {
                    TokenKind::Operator(OperatorKind::BitwiseOr)
                }
            }
            '^' => {
                if self.cursor.eat_char('=') {
                    TokenKind::Operator(OperatorKind::BitwiseXOREqual)
                } else {
                    TokenKind::Operator(OperatorKind::BitwiseXor)
                }
            }
            '&' => {
                if self.cursor.eat_char('=') {
                    TokenKind::Operator(OperatorKind::BitwiseAndEqual)
                } else {
                    TokenKind::Operator(OperatorKind::BitwiseAnd)
                }
            }
            '-' => {
                if self.cursor.eat_char('=') {
                    TokenKind::Operator(OperatorKind::MinusEqual)
                } else if self.cursor.eat_char('>') {
                    TokenKind::RightArrow
                } else {
                    TokenKind::Operator(OperatorKind::Minus)
                }
            }
            '@' => {
                if self.cursor.eat_char('=') {
                    TokenKind::Operator(OperatorKind::AtEqual)
                } else {
                    TokenKind::Operator(OperatorKind::At)
                }
            }
            '!' => {
                if self.cursor.eat_char('=') {
                    TokenKind::Operator(OperatorKind::NotEquals)
                } else {
                    TokenKind::Exclamation
                }
            }
            '~' => TokenKind::Operator(OperatorKind::BitwiseNot),
            '(' => {
                self.nesting += 1;
                TokenKind::OpenParenthesis
            }
            ')' => {
                self.nesting = self.nesting.saturating_sub(1);
                TokenKind::CloseParenthesis
            }
            '[' => {
                self.nesting += 1;
                TokenKind::OpenBracket
            }
            ']' => {
                self.nesting = self.nesting.saturating_sub(1);
                TokenKind::CloseBracket
            }
            '{' => {
                self.nesting += 1;
                TokenKind::OpenBrace
            }
            '}' => {
                if let Some(fstring) = self.fstrings.current_mut() {
                    if fstring.nesting() == self.nesting {
                        return Err(LexicalError {
                            error: LexicalErrorType::FStringError(FStringErrorType::SingleRbrace),
                            range: self.token_range(),
                        });
                    }
                    fstring.try_end_format_spec(self.nesting);
                }
                self.nesting = self.nesting.saturating_sub(1);
                TokenKind::CloseBrace
            }
            ':' => {
                if self
                    .fstrings
                    .current_mut()
                    .is_some_and(|fstring| fstring.try_start_format_spec(self.nesting))
                {
                    TokenKind::Colon
                } else if self.cursor.eat_char('=') {
                    TokenKind::Operator(OperatorKind::ColonEqual)
                } else {
                    TokenKind::Colon
                }
            }
            ';' => TokenKind::SemiColon,
            '<' => {
                if self.cursor.eat_char('<') {
                    if self.cursor.eat_char('=') {
                        TokenKind::Operator(OperatorKind::BitwiseLeftShiftEqual)
                    } else {
                        TokenKind::Operator(OperatorKind::BitwiseLeftShift)
                    }
                } else if self.cursor.eat_char('=') {
                    TokenKind::Operator(OperatorKind::LessThanOrEqual)
                } else {
                    TokenKind::Operator(OperatorKind::LessThan)
                }
            }
            '>' => {
                if self.cursor.eat_char('>') {
                    if self.cursor.eat_char('=') {
                        TokenKind::Operator(OperatorKind::BitwiseRightShiftEqual)
                    } else {
                        TokenKind::Operator(OperatorKind::BitwiseRightShift)
                    }
                } else if self.cursor.eat_char('=') {
                    TokenKind::Operator(OperatorKind::GreaterThanOrEqual)
                } else {
                    TokenKind::Operator(OperatorKind::GreaterThan)
                }
            }
            ',' => TokenKind::Comma,
            '.' => {
                if self.cursor.first().is_ascii_digit() {
                    self.lex_decimal_number('.')?
                } else if self.cursor.eat_char2('.', '.') {
                    TokenKind::Ellipsis
                } else {
                    TokenKind::Dot
                }
            }
            '\n' => {
                return Ok(Token(
                    if self.nesting == 0 && !self.state.is_new_logical_line() {
                        self.state = State::AfterNewline;
                        TokenKind::NewLine
                    } else {
                        if let Some(fstring) = self.fstrings.current_mut() {
                            fstring.try_end_format_spec(self.nesting)
                        }
                        TokenKind::NonLogicalNewline
                    },
                    self.token_range(),
                ))
            }
            '\r' => {
                self.cursor.eat_char('\n');

                return Ok(Token(
                    if self.nesting == 0 && !self.state.is_new_logical_line() {
                        self.state = State::AfterNewline;
                        TokenKind::NewLine
                    } else {
                        if let Some(fstring) = self.fstrings.current_mut() {
                            fstring.try_end_format_spec(self.nesting)
                        }
                        TokenKind::NonLogicalNewline
                    },
                    self.token_range(),
                ));
            }

            _ => {
                self.state = State::Other;

                return Err(LexicalError::new(
                    LexicalErrorType::UnrecognizedToken,
                    self.token_range(),
                ));
            }
        };

        self.state = State::Other;

        Ok(Token(token, self.token_range()))
    }

    #[inline]
    fn token_range(&self) -> TextRange {
        let end = self.offset();
        let len = self.cursor.token_len();

        TextRange::at(end - len, len)
    }

    #[inline]
    fn token_text(&self) -> &'source str {
        &self.source[self.token_range()]
    }

    // Lexer doesn't allow files larger than 4GB
    #[allow(clippy::cast_possible_truncation)]
    #[inline]
    fn offset(&self) -> TextSize {
        TextSize::new(self.source.len() as u32) - self.cursor.text_len()
    }

    pub fn debug(&self) -> LexerDebug<'_, '_, Lexer> {
        LexerDebug::new(self, self.source)
    }
}

// Implement iterator pattern for Lexer.
// Calling the next element in the iterator will yield the next lexical
// token.
impl Iterator for Lexer<'_> {
    type Item = LexResult;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();

        match token {
            Ok(Token(TokenKind::Eof, _)) => None,
            r => Some(r),
        }
    }
}

pub struct LexerDebug<'lexer, 'source, I: Clone> {
    lexer: &'lexer I,
    src: &'source str,
}

impl<'lexer, 'source, I> LexerDebug<'lexer, 'source, I>
where
    I: Clone + Iterator<Item = LexResult>,
{
    pub fn new(lexer: &'lexer I, src: &'source str) -> Self {
        Self { lexer, src }
    }
}

impl<I> Debug for LexerDebug<'_, '_, I>
where
    I: Clone + Iterator<Item = LexResult>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(self.lexer.clone().map(|result| match result {
                Ok(token) => LexerDebugItem::Token(self.src[token.range()].to_string(), token),
                Err(err) => LexerDebugItem::Error(self.src[err.range].to_string(), TokenKind::Invalid, err.error),
            }))
            .finish()
    }
}

enum LexerDebugItem {
    Token(String, Token),
    Error(String, TokenKind, LexicalErrorType),
}

impl Debug for LexerDebugItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerDebugItem::Token(src, tok) => {
                write!(f, "{:?} {}", src, tok)
            }
            LexerDebugItem::Error(src, tok_kind, err_kind) => {
                write!(f, "{:?} {:?} {}", src, tok_kind, err_kind)
            }
        }
    }
}

/// Represents an error that occur during lexing and are
/// returned by the `parse_*` functions in the iterator in the
/// [lexer] implementation.
///
/// [lexer]: crate::lexer
#[derive(Debug, PartialEq, Clone)]
pub struct LexicalError {
    /// The type of error that occurred.
    pub error: LexicalErrorType,
    /// The range of the error.
    pub range: TextRange,
}

impl LexicalError {
    /// Creates a new `LexicalError` with the given error type and location.
    pub fn new(error: LexicalErrorType, range: TextRange) -> Self {
        Self { error, range }
    }
}

/// Represents the different types of errors that can occur during lexing.
#[derive(Debug, PartialEq, Clone)]
pub enum LexicalErrorType {
    /// The indentation is not consistent.
    IndentationError,
    /// An unrecognized token was encountered.
    UnrecognizedToken,
    /// An unexpected character was encountered after a line continuation.
    LineContinuationError,
    /// An unexpected end of file was encountered.
    Eof,
    // A string literal doesn't have the closing quote.
    UnclosedStringError,
    // A invalid number literal, can be an integer, float or complex.
    InvalidNumberError(InvalidNumberErrorKind),

    FStringError(FStringErrorType),
}

impl std::fmt::Display for LexicalErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LexicalErrorType::IndentationError => {
                write!(f, "unindent does not match any outer indentation level")
            }
            LexicalErrorType::UnrecognizedToken => {
                write!(f, "unexpected token")
            }
            LexicalErrorType::LineContinuationError => {
                write!(f, "unexpected character after line continuation character")
            }
            LexicalErrorType::Eof => write!(f, "unexpected EOF while parsing"),
            LexicalErrorType::UnclosedStringError => write!(f, "missing closing quote in string literal"),
            LexicalErrorType::InvalidNumberError(kind) => write!(f, "{kind}"),
            LexicalErrorType::FStringError(error) => write!(f, "f-string: {error}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FStringErrorType {
    UnterminatedTripleQuotedString,
    UnterminatedString,
    UnclosedLbrace,
    SingleRbrace,
}

impl Display for FStringErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FStringErrorType::UnterminatedString => write!(f, "unterminated string"),
            FStringErrorType::UnterminatedTripleQuotedString => write!(f, "unterminated triple-quoted string"),
            FStringErrorType::UnclosedLbrace => write!(f, "expecting `}}`"),
            FStringErrorType::SingleRbrace => write!(f, "single `}}` is not allowed"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum InvalidNumberErrorKind {
    Hex,
    Int,
    Octal,
    Float,
    Binary,
    Complex,
}

impl std::fmt::Display for InvalidNumberErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            InvalidNumberErrorKind::Hex => write!(f, "invalid hexadecimal number"),
            InvalidNumberErrorKind::Int => write!(f, "invalid integer number"),
            InvalidNumberErrorKind::Octal => write!(f, "invalid octal number"),
            InvalidNumberErrorKind::Float => write!(f, "invalid float number"),
            InvalidNumberErrorKind::Binary => write!(f, "invalid binary number"),
            InvalidNumberErrorKind::Complex => write!(f, "invalid complex number"),
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum State {
    /// Lexer is right at the beginning of the file or after a `Newline` token.
    AfterNewline,

    /// The lexer is at the start of a new logical line but **after** the indentation
    NonEmptyLogicalLine,

    /// Lexer is right after an equal token
    AfterEqual,

    /// Inside of a logical line
    Other,
}

impl State {
    const fn is_after_newline(self) -> bool {
        matches!(self, State::AfterNewline)
    }

    const fn is_new_logical_line(self) -> bool {
        matches!(self, State::AfterNewline | State::NonEmptyLogicalLine)
    }
}

#[derive(Copy, Clone, Debug)]
enum Radix {
    Binary,
    Octal,
    Decimal,
    Hex,
}

impl Radix {
    const fn is_digit(self, c: char) -> bool {
        match self {
            Radix::Binary => matches!(c, '0'..='1'),
            Radix::Octal => matches!(c, '0'..='7'),
            Radix::Decimal => c.is_ascii_digit(),
            Radix::Hex => matches!(c, '0'..='9' | 'a'..='f' | 'A'..='F'),
        }
    }

    /// Based on the `Radix`, try to convert `str` to `NumberKind` if `str` is a
    /// valid number.
    fn try_number_kind(self, str: &str) -> Result<NumberKind, LexicalErrorType> {
        match self {
            Radix::Binary if !is_binary_number_valid(str) => {
                Err(LexicalErrorType::InvalidNumberError(InvalidNumberErrorKind::Binary))
            }
            Radix::Octal if !is_octal_number_valid(str) => {
                Err(LexicalErrorType::InvalidNumberError(InvalidNumberErrorKind::Octal))
            }
            Radix::Hex if !is_hex_number_valid(str) => {
                Err(LexicalErrorType::InvalidNumberError(InvalidNumberErrorKind::Hex))
            }
            Radix::Decimal if !is_decimal_number_valid(str) => {
                Err(LexicalErrorType::InvalidNumberError(InvalidNumberErrorKind::Int))
            }

            Radix::Octal => Ok(NumberKind::Octal),
            Radix::Decimal => Ok(NumberKind::Decimal),
            Radix::Hex => Ok(NumberKind::Hex),
            Radix::Binary => Ok(NumberKind::Binary),
        }
    }
}

#[cfg(test)]
mod tests {

    use insta::assert_debug_snapshot;

    use crate::lexer::{lex, LexerDebug};

    use super::{FStringErrorType, LexicalErrorType};

    #[test]
    fn lex_identifiers() {
        let source = "x _abc foo_bar Foo FooBar __foo__ X abc123 abc_123 aBc_123";
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn lex_decimal_number() {
        let source = "1_000 12 0b1_0_1 0B10 0o0_1_2_3_4_5_6_7 0O01234567 0xABCDEF_0123456789 0X0123456789ABCDEF";
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn lex_float_number() {
        let source = "3.14 10. .001 1e100 3.14e-10 0e0 3.14_15_93";
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn lex_complex_number() {
        let source = "3.14j 10.j 10j .001j 1e100j 3.14e-10j 3.14_15_93j";
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn lex_unclosed_square_bracket() {
        let source = "[1";
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    const WINDOWS_EOL: &str = "\r\n";
    const MAC_EOL: &str = "\r";
    const UNIX_EOL: &str = "\n";

    macro_rules! test_line_comment {
        ($($name:ident: $eol:expr,)*) => {
            $(
                #[test]

                fn $name() {
                    let source = format!(r"99232  #{}", $eol);
                    let lexer = lex(&source);
                    assert_debug_snapshot!(LexerDebug::new(&lexer, &source));
                }
            )*
        }
    }

    test_line_comment! {
        lex_line_comment_long: " foo",
        lex_line_comment_whitespace: "  ",
        lex_line_comment_single_whitespace: " ",
        lex_line_comment_empty: "",
    }

    macro_rules! test_comment_until_eol {
        ($($name:ident: $eol:expr,)*) => {
            $(
                #[test]

                fn $name() {
                    let source = format!("123  # Foo{}456", $eol);
                    let lexer = lex(&source);
                    assert_debug_snapshot!(LexerDebug::new(&lexer, &source));
                }
            )*
        }
    }

    test_comment_until_eol! {
        lex_comment_until_windows_eol: WINDOWS_EOL,
        lex_comment_until_mac_eol: MAC_EOL,
        lex_comment_until_unix_eol: UNIX_EOL,
    }

    #[test]
    fn lex_assignment() {
        let source = r"a_variable = 99 + 2-0";
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    macro_rules! test_indentation_with_eol {
        ($($name:ident: $eol:expr,)*) => {
            $(
                #[test]

                fn $name() {
                    let source = format!("def foo():{}   return 99{}{}", $eol, $eol, $eol);
                    let lexer = lex(&source);
                    assert_debug_snapshot!(LexerDebug::new(&lexer, &source));
                }
            )*
        };
    }

    test_indentation_with_eol! {
        lex_indentation_windows_eol: WINDOWS_EOL,
        lex_indentation_mac_eol: MAC_EOL,
        lex_indentation_unix_eol: UNIX_EOL,
    }

    macro_rules! test_double_dedent_with_eol {
        ($($name:ident: $eol:expr,)*) => {
            $(
                #[test]

                fn $name() {
                    let source = format!("def foo():{} if x:{}{}  return 99{}{}", $eol, $eol, $eol, $eol, $eol);
                    let lexer = lex(&source);
                    assert_debug_snapshot!(LexerDebug::new(&lexer, &source));
                }
            )*
        }
    }

    test_double_dedent_with_eol! {
        lex_double_dedent_windows_eol: WINDOWS_EOL,
        lex_double_dedent_mac_eol: MAC_EOL,
        lex_double_dedent_unix_eol: UNIX_EOL,
    }

    macro_rules! test_double_dedent_with_tabs {
        ($($name:ident: $eol:expr,)*) => {
            $(
                #[test]

                fn $name() {
                    let source = format!("def foo():{}\tif x:{}{}\t return 99{}{}", $eol, $eol, $eol, $eol, $eol);
                    let lexer = lex(&source);
                    assert_debug_snapshot!(LexerDebug::new(&lexer, &source));
                }
            )*
        }
    }

    test_double_dedent_with_tabs! {
        lex_double_dedent_tabs_windows_eol: WINDOWS_EOL,
        lex_double_dedent_tabs_mac_eol: MAC_EOL,
        lex_double_dedent_tabs_unix_eol: UNIX_EOL,
    }

    macro_rules! test_newline_in_brackets {
        ($($name:ident: $eol:expr,)*) => {
            $(
                #[test]

                fn $name() {
                    let source = r"x = [

        1,2
    ,(3,
    4,
    ), {
    5,
    6,\
    7}]
    ".replace("\n", $eol);
                    let lexer = lex(&source);
                    assert_debug_snapshot!(LexerDebug::new(&lexer, &source));
                }
            )*
        };
    }

    test_newline_in_brackets! {
        lex_newline_in_brackets_windows_eol: WINDOWS_EOL,
        lex_newline_in_brackets_mac_eol: MAC_EOL,
        lex_newline_in_brackets_unix_eol: UNIX_EOL,
    }

    #[test]
    fn lex_non_logical_newline_in_string_continuation() {
        let source = r"(
        'a'
        'b'

        'c' \
        'd'
    )";
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn lex_logical_newline_line_comment() {
        let source = "#Hello\n#World\n";
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn lex_operators() {
        let source = "//////=/ /";
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn lex_string() {
        let source = r#""double" 'single' 'can\'t' "\\\"" '\t\r\n' '\g' r'raw\'' '\420' '\200\0a'"#;
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn lex_soft_keywords() {
        let source = "match x:\n\tcase None:\n\t...";
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    macro_rules! test_string_continuation_with_eol {
        ($($name:ident: $eol:expr,)*) => {
            $(
                #[test]

                fn $name() {
                    let source = format!("\"abc\\{}def\"", $eol);
                    let lexer = lex(&source);
                    assert_debug_snapshot!(LexerDebug::new(&lexer, &source));
                }
            )*
        }
    }

    test_string_continuation_with_eol! {
        lex_string_continuation_windows_eol: WINDOWS_EOL,
        lex_string_continuation_mac_eol: MAC_EOL,
        lex_string_continuation_unix_eol: UNIX_EOL,
    }

    macro_rules! test_triple_quoted {
        ($($name:ident: $eol:expr,)*) => {
            $(
                #[test]

                fn $name() {
                    let source = format!("\"\"\"{} test string{} \"\"\"", $eol, $eol);
                    let lexer = lex(&source);
                    assert_debug_snapshot!(LexerDebug::new(&lexer, &source));
                }
            )*
        }
    }

    test_triple_quoted! {
        lex_triple_quoted_windows_eol: WINDOWS_EOL,
        lex_triple_quoted_unix_eol: UNIX_EOL,
        lex_triple_quoted_macos_eol: MAC_EOL,
    }

    #[test]
    fn test_too_low_dedent() {
        let source = r"if True:
    pass
  pass
";
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn test_empty_fstrings() {
        let source = r#"f"" "" F"" f'' '' f"""""" f''''''"#;
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn test_fstring_prefix() {
        let source = r#"f"" F"" rf"" rF"" Rf"" RF"" fr"" Fr"" fR"" FR"""#;
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn test_fstring() {
        let source = r#"f"normal {foo} {{another}} {bar} {{{three}}}""#;
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn test_fstring_parentheses() {
        let source = r#"f"{}" f"{{}}" f" {}" f"{{{}}}" f"{{{{}}}}" f" {} {{}} {{{}}} {{{{}}}}  ""#;
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn test_fstring_escape() {
        let source = r#"f"\{x:\"\{x}} \"\"\
 end""#;
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn test_fstring_escape_raw() {
        let source = r#"rf"\{x:\"\{x}} \"\"\
 end""#;
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn test_fstring_named_unicode() {
        let source = r#"f"\N{BULLET} normal \Nope \N""#;
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn test_fstring_named_unicode_raw() {
        let source = r#"rf"\N{BULLET} normal""#;
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn test_fstring_with_named_expression() {
        let source = r#"f"{x:=10} {(x:=10)} {x,{y:=10}} {[x:=10]}""#;
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn test_fstring_with_format_spec() {
        let source = r#"f"{foo:} {x=!s:.3f} {x:.{y}f} {'':*^{1:{1}}} {x:{{1}.pop()}}""#;
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn test_fstring_with_multiline_format_spec() {
        // The last f-string is invalid syntactically but we should still lex it.
        // Note that the `b` is a `Name` token and not a `FStringMiddle` token.
        let source = r"f'''__{
    x:d
}__'''
f'''__{
    x:a
        b
          c
}__'''
f'__{
    x:d
}__'
f'__{
    x:a
        b
}__'
";
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn test_fstring_conversion() {
        let source = r#"f"{x!s} {x=!r} {x:.3f!r} {{x!r}}""#;
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn test_fstring_nested() {
        let source = r#"f"foo {f"bar {x + f"{wow}"}"} baz" f'foo {f'bar'} some {f"another"}'"#;
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn test_fstring_expression_multiline() {
        let source = r#"f"first {
    x
        *
            y
} second""#;
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn test_fstring_multiline() {
        let source = r#"f"""
hello
    world
""" f'''
    world
hello
''' f"some {f"""multiline
allowed {x}"""} string""#;
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn test_fstring_comments() {
        let source = r#"f"""
# not a comment { # comment {
    x
} # not a comment
""""#;
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn test_fstring_with_lambda_expression() {
        let source = r#"
f"{lambda x:{x}}"
f"{(lambda x:{x})}"
"#
        .trim();
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    #[test]
    fn test_fstring_with_nul_char() {
        let source = r"f'\0'";
        let lexer = lex(source);
        assert_debug_snapshot!(LexerDebug::new(&lexer, source));
    }

    fn lex_fstring_error(source: &str) -> FStringErrorType {
        match lex(source).find_map(std::result::Result::err) {
            Some(err) => match err.error {
                LexicalErrorType::FStringError(error) => error,
                _ => panic!("Expected FStringError: {err:?}"),
            },
            _ => panic!("Expected atleast one FStringError"),
        }
    }

    #[test]
    fn test_fstring_error() {
        use FStringErrorType::{SingleRbrace, UnclosedLbrace, UnterminatedString, UnterminatedTripleQuotedString};

        assert_eq!(lex_fstring_error("f'}'"), SingleRbrace);
        assert_eq!(lex_fstring_error("f'{{}'"), SingleRbrace);
        assert_eq!(lex_fstring_error("f'{{}}}'"), SingleRbrace);
        assert_eq!(lex_fstring_error("f'foo}'"), SingleRbrace);
        assert_eq!(lex_fstring_error(r"f'\u007b}'"), SingleRbrace);
        assert_eq!(lex_fstring_error("f'{a:b}}'"), SingleRbrace);
        assert_eq!(lex_fstring_error("f'{3:}}>10}'"), SingleRbrace);
        assert_eq!(lex_fstring_error(r"f'\{foo}\}'"), SingleRbrace);

        assert_eq!(lex_fstring_error("f'{'"), UnclosedLbrace);
        assert_eq!(lex_fstring_error("f'{foo!r'"), UnclosedLbrace);
        assert_eq!(lex_fstring_error("f'{foo='"), UnclosedLbrace);
        assert_eq!(
            lex_fstring_error(
                r#"f"{"
"#
            ),
            UnclosedLbrace
        );
        assert_eq!(lex_fstring_error(r#"f"""{""""#), UnclosedLbrace);

        assert_eq!(lex_fstring_error(r#"f""#), UnterminatedString);
        assert_eq!(lex_fstring_error(r#"f'"#), UnterminatedString);

        assert_eq!(lex_fstring_error(r#"f""""#), UnterminatedTripleQuotedString);
        assert_eq!(lex_fstring_error(r#"f'''"#), UnterminatedTripleQuotedString);
        assert_eq!(lex_fstring_error(r#"f"""""#), UnterminatedTripleQuotedString);
        assert_eq!(lex_fstring_error(r#"f""""""#), UnterminatedTripleQuotedString);
    }

    macro_rules! fstring_single_quote_escape_eol {
        ($($name:ident: $eol:expr,)*) => {
            $(
                #[test]

                fn $name() {
                    let source = format!(r"f'text \{} more text'", $eol);
                    let lexer = lex(&source);
                    assert_debug_snapshot!(LexerDebug::new(&lexer, &source));
                }
            )*
        }
    }

    fstring_single_quote_escape_eol! {
        test_fstring_single_quote_escape_windows_eol: WINDOWS_EOL,
        test_fstring_single_quote_escape_mac_eol: MAC_EOL,
        test_fstring_single_quote_escape_unix_eol: UNIX_EOL,
    }
}
