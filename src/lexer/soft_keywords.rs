use itertools::{Itertools, MultiPeek};

use crate::lexer::types::{KeywordKind, OperatorKind, SoftKeywordKind, TokenKind};

use super::{LexResult, Token};

/// An [`Iterator`] that transforms a token stream to accommodate soft keywords (namely, `match`
/// `case`, and `type`).
///
/// [PEP 634](https://www.python.org/dev/peps/pep-0634/) introduced the `match` and `case` keywords
/// as soft keywords, meaning that they can be used as identifiers (e.g., variable names) in certain
/// contexts.
///
/// Later, [PEP 695](https://peps.python.org/pep-0695/#generic-type-alias) introduced the `type`
/// soft keyword.
///
/// This function modifies a token stream to accommodate this change. In particular, it replaces
/// soft keyword tokens with `identifier` tokens if they are used as identifiers.
///
/// Handling soft keywords in this intermediary pass allows us to simplify both the lexer and
/// `ruff_python_parser`, as neither of them need to be aware of soft keywords.
#[derive(Clone)]
pub struct SoftKeywordTransformer<I>
where
    I: Clone + Iterator<Item = LexResult>,
{
    underlying: MultiPeek<I>,
    start_of_line: bool,
}

impl<I> SoftKeywordTransformer<I>
where
    I: Clone + Iterator<Item = LexResult>,
{
    pub fn new(lexer: I) -> Self {
        Self {
            underlying: lexer.multipeek(),
            start_of_line: true,
        }
    }
}

impl<I> Iterator for SoftKeywordTransformer<I>
where
    I: Clone + Iterator<Item = LexResult>,
{
    type Item = LexResult;

    #[inline]
    fn next(&mut self) -> Option<LexResult> {
        let mut next = self.underlying.next();
        if let Some(Ok(Token(token, range))) = next.as_ref() {
            // If the token is a soft keyword e.g. `type`, `match`, or `case`, check if it's
            // used as an identifier. We assume every soft keyword use is an identifier unless
            // a heuristic is met.

            match token {
                // For `match` and `case`, all of the following conditions must be met:
                // 1. The token is at the start of a logical line.
                // 2. The logical line contains a top-level colon (that is, a colon that is not nested
                //    inside a parenthesized expression, list, or dictionary).
                // 3. The top-level colon is not the immediate sibling of a `match` or `case` token.
                //    (This is to avoid treating `match` or `case` as identifiers when annotated with
                //    type hints.)   type hints.)
                TokenKind::SoftKeyword(SoftKeywordKind::Match | SoftKeywordKind::Case) => {
                    if self.start_of_line {
                        let mut nesting = 0;
                        let mut first = true;
                        let mut seen_colon = false;
                        let mut seen_lambda = false;
                        while let Some(Ok(Token(token, _))) = self.underlying.peek() {
                            match token {
                                TokenKind::NewLine => break,
                                TokenKind::Keyword(KeywordKind::Lambda) if nesting == 0 => seen_lambda = true,
                                TokenKind::Colon if nesting == 0 => {
                                    if seen_lambda {
                                        seen_lambda = false;
                                    } else if !first {
                                        seen_colon = true;
                                    }
                                }
                                TokenKind::OpenParenthesis | TokenKind::OpenBracket | TokenKind::OpenBrace => {
                                    nesting += 1
                                }
                                TokenKind::CloseParenthesis | TokenKind::CloseBracket | TokenKind::CloseBrace => {
                                    nesting -= 1
                                }
                                _ => {}
                            }
                            first = false;
                        }
                        if !seen_colon {
                            next = Some(Ok(Token(TokenKind::Id, *range)));
                        }
                    } else {
                        next = Some(Ok(Token(TokenKind::Id, *range)));
                    }
                }
                // For `type` all of the following conditions must be met:
                // 1. The token is at the start of a logical line.
                // 2. The type token is immediately followed by a name token.
                // 3. The name token is eventually followed by an equality token.
                TokenKind::SoftKeyword(SoftKeywordKind::Type) => {
                    if self.start_of_line {
                        let mut is_type_alias = false;
                        if let Some(Ok(Token(token, _))) = self.underlying.peek() {
                            if matches!(
                                token,
                                TokenKind::Id |
                                // We treat a soft keyword token following a type token as a
                                // name to support cases like `type type = int` or `type match = int`
                                TokenKind::SoftKeyword(_)
                            ) {
                                let mut nesting = 0;
                                while let Some(Ok(Token(token, _))) = self.underlying.peek() {
                                    match token {
                                        TokenKind::NewLine => break,
                                        TokenKind::Operator(OperatorKind::Assign) if nesting == 0 => {
                                            is_type_alias = true;
                                            break;
                                        }
                                        TokenKind::OpenBracket => nesting += 1,
                                        TokenKind::CloseBracket => nesting -= 1,
                                        // Allow arbitrary content within brackets for now
                                        _ if nesting > 0 => {}
                                        // Exit if unexpected tokens are seen
                                        _ => break,
                                    }
                                }
                            }
                        }
                        if !is_type_alias {
                            next = Some(Ok(Token(TokenKind::Id, *range)));
                        }
                    } else {
                        next = Some(Ok(Token(TokenKind::Id, *range)));
                    }
                }
                _ => (), // Not a soft keyword token
            }
        }

        self.start_of_line = next.as_ref().is_some_and(|lex_result| {
            lex_result.as_ref().is_ok_and(|Token(token, _)| {
                if matches!(token, TokenKind::NonLogicalNewline | TokenKind::Comment) {
                    return self.start_of_line;
                }

                matches!(token, TokenKind::NewLine | TokenKind::Indent | TokenKind::Dedent)
            })
        });

        next
    }
}
