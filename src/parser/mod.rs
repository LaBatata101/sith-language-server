mod helpers;
pub mod nodes;
mod tests;

use std::{borrow::Cow, fmt::Display};

use bitflags::bitflags;
use itertools::{Itertools, PeekNth};
use ruff_text_size::TextRange;

use crate::{
    lexer::types::{KeywordKind, NumberKind, OperatorKind, StringKind, TokenKind},
    lexer::{lex, types::SoftKeywordKind, LexResult, LexicalErrorType, Token},
};

use self::{
    helpers::remove_str_quotes,
    nodes::{
        BinaryOp, BoolOp, BoolValue, CompareOp, ContextExpr, ConversionFlag, Expression, Literal, Operator, Pattern,
        Statement, UnaryOp,
    },
};

#[derive(Debug)]
pub struct ParsedFile<'a> {
    pub ast: nodes::Module<'a>,
    pub parse_errors: Vec<ParseError>,
}

pub fn parse(src: &str) -> ParsedFile<'_> {
    let mut parser = Parser::new(lex(src), src);
    ParsedFile {
        ast: parser.parse(),
        parse_errors: parser.errors,
    }
}

bitflags! {
    #[derive(Default)]
    struct ParserCtxFlags: u32 {
        const DICT_EXPR = 1 << 0;
        const LIST_EXPR = 1 << 1;
        const SLICE_EXPR = 1 << 2;
        const TUPLE_EXPR = 1 << 3;
        const SUBSCRIPT_EXPR = 1 << 4;
        const PARENTHESIZED_EXPR = 1 << 5;
        const BRACESIZED_EXPR = 1 << 6;
        const SET_COMP_EXPR = 1 << 7;
        const LAMBDA_EXPR = 1 << 8;
        const LIST_COMP_EXPR = 1 << 9;
        const GENERATOR_EXPR = 1 << 10;
        const DICT_COMP_EXPR = 1 << 11;

        const IF_STMT = 1 << 12;
        const DEL_STMT = 1 << 13;
        const FUNC_DEF_STMT = 1 << 14;
        const CLASS_DEF_STMT = 1 << 15;
        const WITH_STMT = 1 << 16;
        const FOR_STMT = 1 << 17;
        const WHILE_STMT  = 1 << 18;
        const MATCH_STMT  = 1 << 19;

        const COMPREHENSION_TARGET = 1 << 20;
        const ARGUMENTS = 1 << 21;
        const SEQUENCE_PATTERN = 1 << 22;
        const CLASS_PATTERN = 1 << 23;
        const MAPPING_PATTERN = 1 << 24;
        const FOR_TARGET = 1 << 25;

        const AWAIT_EXPR = 1 << 26;
    }
}

#[derive(Debug)]
pub struct ParseError {
    pub kind: ParseErrorType,
    pub range: TextRange,
}

#[derive(Debug)]
pub enum ParseErrorType {
    Other(String),
    EmptySubscript,
    InvalidIdentifier,
    SimpleStmtsInSameLine,
    UnexpectedIndentation,
    StmtIsNotAsync(TokenKind),
    Lexical(LexicalErrorType),
    ParamFollowsVarKeywordParam,
    PosArgFollowsKeywordArgUnpack,
    IterableUnpackFollowsKeywordArg,
    NonDefaultParamFollowsDefaultParam,
    SimpleStmtAndCompoundStmtInSameLine,
    InvalidMatchPattern { pattern: TokenKind },
    ExpectedToken { expected: TokenKind, found: TokenKind },
}

impl Display for ParseErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseErrorType::ExpectedToken { found, expected } => write!(f, "expected {expected:?}, found {found:?}"),
            ParseErrorType::Lexical(lex_error) => write!(f, "{lex_error}"),
            ParseErrorType::SimpleStmtsInSameLine => write!(f, "use `;` to separate simple statements"),
            ParseErrorType::SimpleStmtAndCompoundStmtInSameLine => write!(
                f,
                "compound statements not allowed in the same line as simple statements"
            ),
            ParseErrorType::StmtIsNotAsync(kind) => write!(f, "`{kind:?}` statement cannot be async"),
            ParseErrorType::Other(msg) => write!(f, "{msg}"),
            ParseErrorType::IterableUnpackFollowsKeywordArg => {
                write!(f, "iterable argument unpacking follows keyword argument unpacking")
            }
            ParseErrorType::PosArgFollowsKeywordArgUnpack => {
                write!(f, "positional argument follows keyword argument unpacking")
            }
            ParseErrorType::EmptySubscript => write!(f, "subscript expression cannot be empty"),
            ParseErrorType::ParamFollowsVarKeywordParam => write!(f, "parameters cannot follow var-keyword parameter"),
            ParseErrorType::NonDefaultParamFollowsDefaultParam => {
                write!(f, "non-default parameter follows default parameters")
            }
            ParseErrorType::InvalidMatchPattern { pattern } => write!(f, "invalid pattern `{pattern:?}`"),
            ParseErrorType::UnexpectedIndentation => write!(f, "unexpected indentation"),
            ParseErrorType::InvalidIdentifier => write!(f, "invalid identifier"),
        }
    }
}

type ExprWithRange<'src> = (Expression<'src>, TextRange);
type StmtWithRange<'src> = (Statement<'src>, TextRange);

/// Binding power associativity
enum Associativity {
    Left,
    Right,
}

struct Parser<'src, I>
where
    I: Iterator<Item = LexResult>,
{
    source: &'src str,
    lexer: PeekNth<I>,
    /// Stores all the syntax errors found during the parsing.
    errors: Vec<ParseError>,
    /// This tracks the current expression or statement being parsed. For example,
    /// if we're parsing a list expression, e.g. `[1, 2]`, `ctx` has the value
    /// `ParserCtxFlags::LIST_EXPR`.
    ///
    /// The `ctx` is also used to create custom error messages and forbid certain
    /// expressions or statements of being parsed. The `ctx` should be empty after
    /// an expression or statement is done parsing.
    ctx: ParserCtxFlags,
    /// During the parsing of expression or statement, multiple `ctx`s can be created.
    /// This stores the previous `ctx`s during the parsing, for example, when parsing
    /// a list expression, e.g. `[1, 2, 3]`, the `ParserCtxFlags::LIST_EXPR` context is created.
    ///
    /// When the parsing for the list starts, the first `ctx` created is `ParserCtxFlags::BRACKETSIZED_EXPR`,
    /// this is because we cannot be sure if we're actually parsing a list expression.
    /// When inside brackets, there's two possibilities, we could parse a list or a
    /// list comprehension. After verifying that we're parsing a list expression, the
    /// `ctx` now becomes `ParserCtxFlags::LIST_EXPR` and `ParserCtxFlags::BRACKETSIZED_EXPR`
    /// goes into `ctx_stack`. `ParserCtxFlags::BRACKETSIZED_EXPR` is restored to `ctx` after
    /// the list expression is done parsing.
    ///
    /// The `ctx_stack` should be empty after an expression or statement is done parsing.
    ctx_stack: Vec<ParserCtxFlags>,
    /// Stores the last `ctx` of an expression or statement that was parsed.
    last_ctx: ParserCtxFlags,
}

/// Parses a sequence of elements separated by a delimiter.
///
/// Arguments of this macro are:
/// - The first argument is unnamed and represents the parser instance.
/// - `delim=` : the `TokenKind` used as the delimiter.
/// - `ending=` : The ending `TokenKind`. The parser will stop when it encounters
/// this token.
/// - `allow_trailing_delim=` : This indicates whether trailing delimiters are allowed
/// `true` or not `false`.
/// - `parsing=` : This is a code block that represents the parsing logic for individual
/// elements within the sequence. It is executed repeatedly until an ending token or a
/// `NewLine` token is encountered.
macro_rules! parse_separated {
    ($parser:ident, delim=$delim:expr, ending=$ending:expr, allow_trailing_delim=$trailing:expr, parsing=$block:block) => {
        {
            let delim: TokenKind = $delim;
            let ending: TokenKind = $ending;
            let has_trailing: bool = $trailing;

            let __range = $parser.current_range();
            while !$parser.at(ending) && !matches!($parser.current_token().kind(), TokenKind::NewLine | TokenKind::Eof) {
                $block

                // exit the loop if a trailing `delim` is not allowed
                if !has_trailing && $parser.lookahead(1).kind() == ending {
                    break;
                }

                if !$parser.eat(delim) {
                    if $parser.at_expr() {
                        $parser.expect(delim);
                    } else {
                        break;
                    }
                }
            }
            let __range  = __range.cover_offset($parser.current_range().start());

            __range
        }
    };
}

/// Parses elements enclosed within a delimiter pair, such as parentheses, brackets,
/// or braces.
///
/// Arguments of this macro are:
/// - The first argument is unnamed and represents the parser instance.
/// - `openning=` : Specify the opening `TokenKind, the openning delimiter.
/// - `closing=` : Specify the closing `TokenKind, the closing delimiter. The
/// parser will stop when it encounters this token. The parser also expects this
/// token to be present.
/// - `delim=` : the `TokenKind` used as the delimiter.
/// - `allow_trailing_delim=` : This indicates whether trailing delimiters are allowed
/// `true` or not `false`.
/// - `parsing=` : This is a code block that represents the parsing logic for individual
/// elements within the sequence. It is executed repeatedly until an ending token or a
/// `NewLine` token is encountered.
macro_rules! parse_delimited {
    ($parser:ident, openning=$openning:expr, closing=$closing:expr, delim=$delim:expr, allow_trailing_delim=$trailing:expr, parsing=$block:block) => {{
        let openning: TokenKind = $openning;
        let closing: TokenKind = $closing;
        let delim: TokenKind = $delim;
        let has_trailing: bool = $trailing;

        let start_range = $parser.current_range();
        assert!($parser.eat(openning));

        parse_separated! {
            $parser,
            delim=delim,
            ending=closing,
            allow_trailing_delim=has_trailing,
            parsing=$block
        };

        let end_range = $parser.current_range();
        $parser.expect_or_skip(closing);

        let __range = start_range.cover(end_range);
        __range
    }};
}

impl<'src, I> Parser<'src, I>
where
    I: Iterator<Item = LexResult>,
{
    pub fn new(lexer: I, source: &'src str) -> Parser<'src, impl Iterator<Item = LexResult>> {
        Parser {
            source,
            lexer: itertools::peek_nth(
                lexer.filter_ok(|token| !matches!(token.kind(), TokenKind::Comment | TokenKind::NonLogicalNewline)),
            ),
            errors: Vec::new(),
            ctx: ParserCtxFlags::empty(),
            ctx_stack: vec![],
            last_ctx: ParserCtxFlags::empty(),
        }
    }

    pub fn parse(&mut self) -> nodes::Module<'src> {
        let mut body = vec![];
        let mut range = TextRange::default();

        while !self.is_eof() {
            if self.at(TokenKind::Indent) {
                range = range.cover(self.handle_unexpected_indentation(&mut body, "unexpected indentation"));
                continue;
            }
            let (stmt, stmt_range) = self.parse_statement();
            range = range.cover(stmt_range);
            body.push(stmt);
        }

        // After parsing, the `ctx` and `ctx_stack` should be empty.
        // If it's not, you probably forgot to call `clear_ctx` somewhere.
        assert!(self.ctx.is_empty() && self.ctx_stack.is_empty());

        nodes::Module { body, range }
    }

    #[inline]
    fn set_ctx(&mut self, ctx: ParserCtxFlags) {
        self.ctx_stack.push(self.ctx);
        self.ctx = ctx;
    }

    #[inline]
    fn clear_ctx(&mut self, ctx: ParserCtxFlags) {
        assert_eq!(self.ctx, ctx);
        self.last_ctx = ctx;
        if let Some(top) = self.ctx_stack.pop() {
            self.ctx = top;
        }
    }

    #[inline]
    fn has_ctx(&self, ctx: ParserCtxFlags) -> bool {
        self.ctx.intersects(ctx)
    }

    #[inline]
    fn has_in_curr_or_parent_ctx(&self, ctx: ParserCtxFlags) -> bool {
        self.has_ctx(ctx) || self.parent_ctx().intersects(ctx)
    }

    #[inline]
    fn parent_ctx(&self) -> ParserCtxFlags {
        self.ctx_stack.last().copied().unwrap_or(ParserCtxFlags::empty())
    }

    #[inline]
    fn is_eof(&mut self) -> bool {
        matches!(self.current_token().kind(), TokenKind::Eof)
    }

    fn next_token(&mut self) -> Token {
        self.lexer
            .next()
            .map(|result| match result {
                Ok(token) => token,
                Err(lex_error) => {
                    self.add_error(ParseErrorType::Lexical(lex_error.error), lex_error.range);

                    // Return a `Invalid` token when encoutering an error
                    Token::new(TokenKind::Invalid, lex_error.range)
                }
            })
            .unwrap()
    }

    fn lookahead(&mut self, offset: usize) -> Token {
        self.lexer
            .peek_nth(offset)
            .map(|result| match result {
                Ok(token) => *token,
                // Return a `Invalid` token when encoutering an error
                Err(err) => Token::new(TokenKind::Invalid, err.range),
            })
            // Create an `Eof` token when reaching the end of the iterator
            .unwrap_or(Token::new(
                TokenKind::Eof,
                TextRange::empty(
                    self.source
                        .len()
                        .try_into()
                        .expect("source length couldn't fit into a u32"),
                ),
            ))
    }

    #[inline]
    fn current_token(&mut self) -> Token {
        self.lookahead(0)
    }

    fn eat(&mut self, kind: TokenKind) -> bool {
        if !self.at(kind) {
            return false;
        }

        self.next_token();
        true
    }

    fn expect(&mut self, expected: TokenKind) -> bool {
        if self.eat(expected) {
            return true;
        }

        let token = self.current_token();
        let found = token.kind();
        self.add_error(ParseErrorType::ExpectedToken { found, expected }, token.range());
        false
    }

    fn expect_or_skip(&mut self, expected: TokenKind) {
        if !self.expect(expected) {
            while !self.at(expected) && !self.at(TokenKind::NewLine) {
                self.next_token();
            }
            // If we don't see the token we are expecting, it doesn't necessarily
            // indicates that it's missing. An unexpected token may have taken its
            // place. Therefore, after skipping the unexpected tokens, we proceed
            // to consume the token we originally expected.
            self.eat(expected);
        }
    }

    fn add_error(&mut self, error: ParseErrorType, range: TextRange) {
        self.errors.push(ParseError { kind: error, range });
    }

    fn advance_until(&mut self, target: TokenKind) {
        while self.current_token().kind() != target
            && !matches!(self.current_token().kind(), TokenKind::NewLine | TokenKind::Eof)
        {
            self.next_token();
        }
    }

    fn advance_until_end_of_expr(&mut self) {
        while !self.current_token().kind().is_end_of_expr() {
            self.next_token();
        }
    }

    #[inline]
    fn at(&mut self, kind: TokenKind) -> bool {
        self.current_token().kind() == kind
    }

    #[inline]
    fn at_expr(&mut self) -> bool {
        let kind = self.current_token().kind();
        kind.is_start_of_expr()
        // If we're inside of a subscript expression and the current token is `:`, treat it as an expression.
            || (self.has_in_curr_or_parent_ctx(ParserCtxFlags::SUBSCRIPT_EXPR) && kind == TokenKind::Colon)
    }

    #[inline]
    fn at_simple_stmt(&mut self) -> bool {
        self.current_token().kind().is_simple_stmt()
    }

    #[inline]
    fn at_compound_stmt(&mut self) -> bool {
        self.current_token().kind().is_compound_stmt()
    }

    #[inline]
    fn src_text(&self, range: TextRange) -> &'src str {
        &self.source[range]
    }

    #[inline]
    fn current_range(&mut self) -> TextRange {
        self.current_token().range()
    }

    fn is_current_token_postfix(&mut self) -> bool {
        match self.current_token().kind() {
            TokenKind::OpenParenthesis | TokenKind::OpenBracket | TokenKind::Dot => true,
            TokenKind::Colon
                if self.has_in_curr_or_parent_ctx(ParserCtxFlags::SUBSCRIPT_EXPR)
                    && !self.has_ctx(ParserCtxFlags::SLICE_EXPR) =>
            {
                true
            }
            TokenKind::Keyword(KeywordKind::Async | KeywordKind::For) => true,
            TokenKind::Operator(OperatorKind::ColonEqual)
                if self.has_in_curr_or_parent_ctx(
                    ParserCtxFlags::IF_STMT
                        | ParserCtxFlags::ARGUMENTS
                        | ParserCtxFlags::BRACESIZED_EXPR
                        | ParserCtxFlags::LIST_EXPR
                        | ParserCtxFlags::DICT_EXPR
                        | ParserCtxFlags::SUBSCRIPT_EXPR
                        | ParserCtxFlags::PARENTHESIZED_EXPR
                        | ParserCtxFlags::WHILE_STMT
                        | ParserCtxFlags::MATCH_STMT
                        | ParserCtxFlags::WITH_STMT,
                ) =>
            {
                true
            }
            _ => false,
        }
    }

    fn handle_unexpected_indentation(&mut self, stmts: &mut Vec<Statement<'src>>, error_msg: &str) -> TextRange {
        self.eat(TokenKind::Indent);

        let range = self.current_range();
        self.add_error(ParseErrorType::Other(error_msg.to_string()), range);

        let mut i = 0;
        let mut count_stmt = 0;
        let mut nested_indentations = 0;
        let mut is_inside_body = false;
        let mut range = self.current_range();
        loop {
            let kind = self.lookahead(i).kind();

            if kind == TokenKind::Dedent && nested_indentations == 0 {
                break;
            }
            if kind == TokenKind::Indent {
                is_inside_body = true;
                nested_indentations += 1;
            }
            if kind == TokenKind::Dedent {
                is_inside_body = false;
                nested_indentations -= 1;
            }
            if !is_inside_body && kind == TokenKind::NewLine {
                count_stmt += 1;
            }

            i += 1;
        }

        for _ in 0..count_stmt {
            let (stmt, stmt_range) = self.parse_statement();
            stmts.push(stmt);
            range = range.cover(stmt_range);
        }

        assert!(self.eat(TokenKind::Dedent));

        range
    }

    fn get_expr_ctx(&self) -> ContextExpr {
        match self.ctx {
            ParserCtxFlags::DEL_STMT => ContextExpr::Del,
            // Context for assignments can't be handled here
            ParserCtxFlags::FOR_TARGET | ParserCtxFlags::COMPREHENSION_TARGET => ContextExpr::Store,
            _ => ContextExpr::Load,
        }
    }

    fn parse_statement(&mut self) -> StmtWithRange<'src> {
        let token = self.current_token();
        match token.kind() {
            TokenKind::Keyword(KeywordKind::If) => self.parse_if_stmt(),
            TokenKind::Keyword(KeywordKind::Try) => self.parse_try_stmt(),
            TokenKind::Keyword(KeywordKind::For) => self.parse_for_stmt(),
            TokenKind::Keyword(KeywordKind::With) => self.parse_with_stmt(),
            TokenKind::Operator(OperatorKind::At) => self.parse_decorators(),
            TokenKind::Keyword(KeywordKind::Async) => self.parse_async_stmt(),
            TokenKind::Keyword(KeywordKind::While) => self.parse_while_stmt(),
            TokenKind::Keyword(KeywordKind::Def) => self.parse_func_def_stmt(vec![], token.range()),
            TokenKind::Keyword(KeywordKind::Class) => self.parse_class_def_stmt(vec![], token.range()),
            TokenKind::SoftKeyword(SoftKeywordKind::Match) => self.parse_match_stmt(),
            _ => self.parse_simple_stmt_newline(),
        }
    }

    fn parse_match_stmt(&mut self) -> StmtWithRange<'src> {
        self.set_ctx(ParserCtxFlags::MATCH_STMT);
        let mut range = self.current_range();

        self.eat(TokenKind::SoftKeyword(SoftKeywordKind::Match));
        let (subject, _) = self.parse_exprs_or_add_error("expecting expression after `match` keyword");
        self.expect_or_skip(TokenKind::Colon);

        self.eat(TokenKind::NewLine);
        if !self.eat(TokenKind::Indent) {
            let range = self.current_range();
            self.add_error(
                ParseErrorType::Other("expected an indented block after `match` statement".to_string()),
                range,
            );
        }

        let (cases, cases_range) = self.parse_match_cases();
        range = range.cover(cases_range);

        self.eat(TokenKind::Dedent);

        self.clear_ctx(ParserCtxFlags::MATCH_STMT);
        (
            Statement::Match(nodes::MatchStmt {
                subject: Box::new(subject),
                cases,
                range,
            }),
            range,
        )
    }

    fn parse_match_case(&mut self) -> nodes::MatchCase<'src> {
        let mut range = self.current_range();

        self.eat(TokenKind::SoftKeyword(SoftKeywordKind::Case));
        let (pattern, _) = self.parse_match_pattern();

        let guard = if self.eat(TokenKind::Keyword(KeywordKind::If)) {
            let (expr, _) = self.parse_exprs();
            Some(Box::new(expr))
        } else {
            None
        };

        self.expect_or_skip(TokenKind::Colon);
        let (body, body_range) = self.parse_body();
        range = range.cover(body_range);

        nodes::MatchCase {
            pattern,
            guard,
            body,
            range: range.cover(range),
        }
    }

    fn parse_match_cases(&mut self) -> (Vec<nodes::MatchCase<'src>>, TextRange) {
        let mut range = self.current_range();

        // FIX: change heuristic in `soft_keywords.rs`. When having a `case (:`
        // `case` is an `Id` instead of a `SoftKeyword`
        if !self.at(TokenKind::SoftKeyword(SoftKeywordKind::Case)) {
            self.add_error(
                ParseErrorType::Other("expecting `case` block after `match`".to_string()),
                range,
            );
        }

        let mut cases = vec![];
        while self.at(TokenKind::SoftKeyword(SoftKeywordKind::Case)) {
            let case = self.parse_match_case();
            range = range.cover(case.range);

            cases.push(case);
        }

        (cases, range)
    }

    fn parse_attr_expr_for_match_pattern(
        &mut self,
        mut lhs: Expression<'src>,
        mut lhs_range: TextRange,
    ) -> ExprWithRange<'src> {
        loop {
            (lhs, lhs_range) = match self.current_token().kind() {
                TokenKind::Dot => self.parse_attribute_expr(lhs, lhs_range),
                _ => break,
            }
        }

        (lhs, lhs_range)
    }

    fn parse_match_pattern_literal(&mut self) -> (Pattern<'src>, TextRange) {
        let token = self.current_token();
        match token.kind() {
            TokenKind::Keyword(KeywordKind::None) => {
                self.next_token();
                (
                    Pattern::MatchSingleton(nodes::PatternMatchSingleton {
                        value: Literal::None,
                        range: token.range(),
                    }),
                    token.range(),
                )
            }
            TokenKind::Keyword(KeywordKind::True) => {
                self.next_token();
                (
                    Pattern::MatchSingleton(nodes::PatternMatchSingleton {
                        value: Literal::Bool(BoolValue::True),
                        range: token.range(),
                    }),
                    token.range(),
                )
            }
            TokenKind::Keyword(KeywordKind::False) => {
                self.next_token();
                (
                    Pattern::MatchSingleton(nodes::PatternMatchSingleton {
                        value: Literal::Bool(BoolValue::False),
                        range: token.range(),
                    }),
                    token.range(),
                )
            }
            TokenKind::Number(_) | TokenKind::String { .. } => {
                let (expr, range) = self.parse_atom();
                (
                    Pattern::MatchValue(nodes::PatternMatchValue {
                        value: Box::new(expr),
                        range,
                    }),
                    range,
                )
            }
            TokenKind::Id if self.lookahead(1).kind() == TokenKind::Dot => {
                let (id, range) = self.parse_id_expr();
                let (expr, range) = self.parse_attr_expr_for_match_pattern(id, range);
                (
                    Pattern::MatchValue(nodes::PatternMatchValue {
                        value: Box::new(expr),
                        range,
                    }),
                    range,
                )
            }
            TokenKind::Id => {
                let ident = self.parse_identifier();
                let range = ident.range();
                (
                    Pattern::MatchAs(nodes::PatternMatchAs {
                        range,
                        pattern: None,
                        name: if ident.is_valid_and(|ident| ident.id == "_") {
                            None
                        } else {
                            Some(ident)
                        },
                    }),
                    range,
                )
            }
            TokenKind::Operator(OperatorKind::Minus) if matches!(self.lookahead(1).kind(), TokenKind::Number(_)) => {
                let (expr, range) = self.parse_lhs();
                (
                    Pattern::MatchValue(nodes::PatternMatchValue {
                        value: Box::new(expr),
                        range,
                    }),
                    range,
                )
            }
            kind => {
                self.add_error(ParseErrorType::InvalidMatchPattern { pattern: kind }, token.range());
                self.advance_until(TokenKind::Colon);
                (
                    Pattern::Invalid,
                    token.range().cover_offset(self.current_range().start()),
                )
            }
        }
    }

    fn parse_delimited_match_pattern(&mut self) -> (Pattern<'src>, TextRange) {
        self.set_ctx(ParserCtxFlags::SEQUENCE_PATTERN);
        let mut range = self.current_range();

        let is_paren = self.at(TokenKind::OpenParenthesis);
        let is_bracket = self.at(TokenKind::OpenBracket);

        let closing = if is_paren {
            self.eat(TokenKind::OpenParenthesis);
            TokenKind::CloseParenthesis
        } else {
            self.eat(TokenKind::OpenBracket);
            TokenKind::CloseBracket
        };

        if matches!(self.current_token().kind(), TokenKind::NewLine | TokenKind::Colon) {
            let range = self.current_range();
            self.add_error(
                ParseErrorType::Other(format!("missing `{}`", if is_paren { ')' } else { ']' })),
                range,
            );
        }

        if self.at(closing) {
            range = range.cover(self.current_range());
            self.eat(closing);

            return (
                Pattern::MatchSequence(nodes::PatternMatchSequence {
                    patterns: vec![],
                    range,
                }),
                range,
            );
        }

        let (mut pattern, pattern_range) = self.parse_match_pattern();

        if is_bracket || self.at(TokenKind::Comma) {
            (pattern, _) = self.parse_sequence_match_pattern(pattern, range, closing);
        }

        range = range.cover(self.current_range());
        self.expect_or_skip(closing);

        self.clear_ctx(ParserCtxFlags::SEQUENCE_PATTERN);

        if let Pattern::MatchSequence(mut sequence) = pattern {
            // Update the range to include the parenthesis or brackets
            sequence.range = range;
            (Pattern::MatchSequence(sequence), range)
        } else {
            (pattern, pattern_range)
        }
    }

    fn parse_sequence_match_pattern(
        &mut self,
        first: Pattern<'src>,
        range: TextRange,
        ending: TokenKind,
    ) -> (Pattern<'src>, TextRange) {
        self.eat(TokenKind::Comma);
        self.set_ctx(ParserCtxFlags::SEQUENCE_PATTERN);

        let mut patterns = vec![first];

        let sequence_range = parse_separated! {
            self,
            delim=TokenKind::Comma,
            ending=ending,
            allow_trailing_delim=true,
            parsing={
                let (pattern, _) = self.parse_match_pattern();
                patterns.push(pattern);
            }
        };

        self.clear_ctx(ParserCtxFlags::SEQUENCE_PATTERN);
        let range = range.cover(sequence_range);
        (
            Pattern::MatchSequence(nodes::PatternMatchSequence { patterns, range }),
            range,
        )
    }

    fn parse_match_pattern_lhs(&mut self) -> (Pattern<'src>, TextRange) {
        let (mut lhs, mut range) = match self.current_token().kind() {
            TokenKind::OpenBrace => self.parse_match_pattern_mapping(),
            TokenKind::Operator(OperatorKind::Asterisk) => self.parse_match_pattern_star(),
            TokenKind::OpenParenthesis | TokenKind::OpenBracket => self.parse_delimited_match_pattern(),
            _ => self.parse_match_pattern_literal(),
        };

        if self.at(TokenKind::OpenParenthesis) {
            (lhs, range) = self.parse_match_pattern_class(lhs, range);
        }

        if self.at(TokenKind::Operator(OperatorKind::Plus)) || self.at(TokenKind::Operator(OperatorKind::Minus)) {
            let op_kind = self.current_token().kind();
            self.next_token();

            let (lhs_value, lhs_range) = if let Pattern::MatchValue(lhs) = lhs {
                if !matches!(lhs.value.as_ref(), Expression::Literal(_)) {
                    self.add_error(
                        ParseErrorType::Other(format!(
                            "invalid `{}` expression for match pattern",
                            self.src_text(lhs.range)
                        )),
                        lhs.range,
                    );
                }
                (lhs.value, lhs.range)
            } else {
                self.add_error(ParseErrorType::Other("invalid lhs pattern".to_string()), range);
                (Box::new(Expression::Invalid(range)), range)
            };

            let (rhs_pattern, rhs_range) = self.parse_match_pattern_lhs();
            let (rhs_value, rhs_range) = if let Pattern::MatchValue(rhs) = rhs_pattern {
                if !matches!(rhs.value.as_ref(), Expression::Literal(_)) {
                    self.add_error(
                        ParseErrorType::Other(format!(
                            "invalid `{}` expression for match pattern",
                            self.src_text(rhs_range)
                        )),
                        rhs_range,
                    );
                }
                (rhs.value, rhs.range)
            } else {
                self.add_error(ParseErrorType::Other("invalid rhs pattern".to_string()), rhs_range);
                (Box::new(Expression::Invalid(rhs_range)), rhs_range)
            };

            if matches!(
                rhs_value.as_ref(),
                Expression::UnaryOp(nodes::UnaryOpExpr { op: UnaryOp::USub, .. })
            ) {
                self.add_error(
                    ParseErrorType::Other("`-` not allowed in rhs of match pattern".to_string()),
                    rhs_range,
                );
            }

            let op = if op_kind == TokenKind::Operator(OperatorKind::Plus) {
                BinaryOp::Add
            } else {
                BinaryOp::Sub
            };
            let range = lhs_range.cover(rhs_range);
            return (
                Pattern::MatchValue(nodes::PatternMatchValue {
                    value: Box::new(Expression::BinaryOp(nodes::BinaryOpExpr {
                        left: lhs_value,
                        op,
                        right: rhs_value,
                        range,
                    })),
                    range,
                }),
                range,
            );
        }

        (lhs, range)
    }

    fn parse_match_pattern(&mut self) -> (Pattern<'src>, TextRange) {
        let (mut lhs, mut range) = self.parse_match_pattern_lhs();

        if self.at(TokenKind::Operator(OperatorKind::BitwiseOr)) {
            let mut patterns = vec![lhs];

            while self.eat(TokenKind::Operator(OperatorKind::BitwiseOr)) {
                let (pattern, pattern_range) = self.parse_match_pattern_lhs();
                range = range.cover(pattern_range);
                patterns.push(pattern);
            }

            lhs = Pattern::MatchOr(nodes::PatternMatchOr { patterns, range });
        }

        if !self
            .has_ctx(ParserCtxFlags::SEQUENCE_PATTERN | ParserCtxFlags::CLASS_PATTERN | ParserCtxFlags::MAPPING_PATTERN)
            && self.eat(TokenKind::Comma)
        {
            (lhs, range) = self.parse_sequence_match_pattern(lhs, range, TokenKind::Colon);
        } else if self.eat(TokenKind::Keyword(KeywordKind::As)) {
            let ident = self.parse_identifier();
            range = range.cover(ident.range());
            lhs = Pattern::MatchAs(nodes::PatternMatchAs {
                range,
                name: Some(ident),
                pattern: Some(Box::new(lhs)),
            });
        }

        (lhs, range)
    }

    fn parse_match_pattern_star(&mut self) -> (Pattern<'src>, TextRange) {
        let mut range = self.current_range();
        self.eat(TokenKind::Operator(OperatorKind::Asterisk));

        let ident = self.parse_identifier();

        range = range.cover(ident.range());
        (
            Pattern::MatchStar(nodes::PatternMatchStar {
                range,
                value: if ident.is_valid_and(|ident| ident.id == "_") {
                    None
                } else {
                    Some(ident)
                },
            }),
            range,
        )
    }

    fn parse_match_pattern_class(
        &mut self,
        cls: Pattern<'src>,
        mut cls_range: TextRange,
    ) -> (Pattern<'src>, TextRange) {
        self.set_ctx(ParserCtxFlags::CLASS_PATTERN);

        let mut patterns = vec![];
        let mut keywords = vec![];
        let mut has_seen_pattern;
        let mut has_seen_keyword_pattern = false;

        let args_range = parse_delimited! {
            self,
            openning=TokenKind::OpenParenthesis,
            closing=TokenKind::CloseParenthesis,
            delim=TokenKind::Comma,
            allow_trailing_delim=true,
            parsing={
                let (pattern, pattern_range) = self.parse_match_pattern();

                if self.eat(TokenKind::Operator(OperatorKind::Assign)) {
                    has_seen_pattern = false;
                    has_seen_keyword_pattern = true;

                    if let Pattern::MatchAs(nodes::PatternMatchAs { name: Some(attr), range, ..}) = pattern {
                        let (pattern, _) = self.parse_match_pattern();

                        keywords.push(nodes::PatternKeyword {
                            attr,
                            pattern,
                            range: range.cover_offset(self.current_range().start()),
                        });
                    } else {
                        self.advance_until_end_of_expr();
                        self.add_error(ParseErrorType::Other(format!(
                            "`{}` not valid keyword pattern",
                            self.src_text(pattern_range)
                        )), pattern_range);
                    }
                } else {
                    has_seen_pattern = true;
                    patterns.push(pattern);
                }

                if has_seen_keyword_pattern && has_seen_pattern {
                    self.add_error(
                        ParseErrorType::Other("pattern not allowed after keyword pattern".to_string()),
                        pattern_range,
                    );
                }
            }
        };

        let cls = match cls {
            Pattern::MatchAs(nodes::PatternMatchAs { name: Some(ident), .. }) => {
                cls_range = ident.range();
                if ident.is_valid() {
                    Box::new(Expression::Id(nodes::IdExpr {
                        id: self.src_text(cls_range),
                        ctx: ContextExpr::Load,
                        range: cls_range,
                    }))
                } else {
                    Box::new(Expression::Invalid(cls_range))
                }
            }
            Pattern::MatchValue(nodes::PatternMatchValue {
                value,
                range: value_range,
            }) if matches!(value.as_ref(), Expression::Attribute(_)) => {
                cls_range = value_range;
                value
            }
            _ => {
                self.add_error(
                    ParseErrorType::Other(format!("`{}` invalid pattern match class", self.src_text(cls_range))),
                    cls_range,
                );
                Box::new(Expression::Invalid(cls_range))
            }
        };

        self.clear_ctx(ParserCtxFlags::CLASS_PATTERN);

        let range = cls_range.cover(args_range);
        (
            Pattern::MatchClass(nodes::PatternMatchClass {
                cls,
                arguments: nodes::PatternArguments {
                    patterns,
                    keywords,
                    range: args_range,
                },
                range,
            }),
            range,
        )
    }

    fn parse_match_pattern_mapping(&mut self) -> (Pattern<'src>, TextRange) {
        self.set_ctx(ParserCtxFlags::MAPPING_PATTERN);
        let mut keys = vec![];
        let mut patterns = vec![];
        let mut rest = None;

        let range = parse_delimited! {
            self,
            openning=TokenKind::OpenBrace,
            closing=TokenKind::CloseBrace,
            delim=TokenKind::Comma,
            allow_trailing_delim=true,
            parsing={
                if self.eat(TokenKind::Operator(OperatorKind::Exponent)) {
                    rest = Some(self.parse_identifier());
                    continue; // We are inside of a loop in this macro!
                }

                let (pattern, pattern_range) = self.parse_match_pattern_literal();
                let key = match pattern {
                    Pattern::MatchValue(nodes::PatternMatchValue { value, ..}) => *value,
                    Pattern::MatchSingleton(nodes::PatternMatchSingleton {value, range }) => {
                    Expression::Literal(nodes::LiteralExpr {
                            value,
                            range
                        })
                    },
                    _ => {
                        self.add_error(ParseErrorType::Other(format!(
                            "invalid mapping pattern key `{}`",
                            self.src_text(pattern_range)
                        )), pattern_range);
                        Expression::Invalid(pattern_range)
                    }
                };
                keys.push(key);

                self.expect_or_skip(TokenKind::Colon);

                let (pattern, _) = self.parse_match_pattern();
                patterns.push(pattern);
            }
        };
        self.clear_ctx(ParserCtxFlags::MAPPING_PATTERN);

        (
            Pattern::MatchMapping(nodes::PatternMatchMapping {
                keys,
                patterns,
                rest,
                range,
            }),
            range,
        )
    }

    fn parse_async_stmt(&mut self) -> StmtWithRange<'src> {
        let mut range = self.current_range();
        self.eat(TokenKind::Keyword(KeywordKind::Async));

        let token = self.current_token();
        let (stmt, stmt_range) = match token.kind() {
            TokenKind::Keyword(KeywordKind::Def) => self.parse_func_def_stmt(vec![], token.range()),
            TokenKind::Keyword(KeywordKind::With) => self.parse_with_stmt(),
            TokenKind::Keyword(KeywordKind::For) => self.parse_for_stmt(),
            kind => {
                // Although this statement is not a valid `async` statement,
                // we still parse it.
                self.add_error(ParseErrorType::StmtIsNotAsync(kind), token.range());
                self.parse_statement()
            }
        };
        range = range.cover(stmt_range);

        let async_stmt = match stmt {
            Statement::FunctionDef(mut func) => {
                func.range = range;
                Statement::AsyncFunctionDef(func)
            }
            Statement::For(mut for_stmt) => {
                for_stmt.range = range;
                Statement::AsyncFor(for_stmt)
            }
            Statement::With(mut with_stmt) => {
                with_stmt.range = range;
                Statement::AsyncWith(with_stmt)
            }
            // Not valid `async` statement here, just return as is.
            _ => stmt,
        };
        (async_stmt, range)
    }

    fn parse_while_stmt(&mut self) -> StmtWithRange<'src> {
        self.set_ctx(ParserCtxFlags::WHILE_STMT);
        let mut range = self.current_range();
        self.eat(TokenKind::Keyword(KeywordKind::While));

        let (test, _) = self.parse_expr_or_add_error("expecting expression after `while` keyword");
        self.expect_or_skip(TokenKind::Colon);

        let (body, body_range) = self.parse_body();
        range = range.cover(body_range);

        let orelse = if self.eat(TokenKind::Keyword(KeywordKind::Else)) {
            self.expect_or_skip(TokenKind::Colon);

            let (else_body, else_body_range) = self.parse_body();
            range = range.cover(else_body_range);
            else_body
        } else {
            vec![]
        };

        self.clear_ctx(ParserCtxFlags::WHILE_STMT);
        (
            Statement::While(nodes::WhileStmt {
                test: Box::new(test),
                body,
                orelse,
                range,
            }),
            range,
        )
    }

    fn parse_for_stmt(&mut self) -> StmtWithRange<'src> {
        self.set_ctx(ParserCtxFlags::FOR_STMT);

        let mut range = self.current_range();
        self.eat(TokenKind::Keyword(KeywordKind::For));

        self.set_ctx(ParserCtxFlags::FOR_TARGET);
        let (target, _) = self.parse_exprs_or_add_error("expecting expression after `for` keyword");
        self.clear_ctx(ParserCtxFlags::FOR_TARGET);

        if !self.expect(TokenKind::Keyword(KeywordKind::In)) {
            // skip leading unexpected tokens
            while matches!(
                self.current_token().kind(),
                TokenKind::Keyword(KeywordKind::In) | TokenKind::Colon
            ) {
                self.next_token();
            }
        }

        let (iter, _) = self.parse_exprs_or_add_error("expecting an expression after `in` keyword");
        self.expect_or_skip(TokenKind::Colon);

        let (body, body_range) = self.parse_body();
        range = range.cover(body_range);

        let orelse = if self.eat(TokenKind::Keyword(KeywordKind::Else)) {
            self.expect_or_skip(TokenKind::Colon);

            let (else_body, else_body_range) = self.parse_body();
            range = range.cover(else_body_range);
            else_body
        } else {
            vec![]
        };

        self.clear_ctx(ParserCtxFlags::FOR_STMT);
        (
            Statement::For(nodes::ForStmt {
                target: Box::new(target),
                iter: Box::new(iter),
                body,
                orelse,
                range,
            }),
            range,
        )
    }

    fn parse_try_stmt(&mut self) -> StmtWithRange<'src> {
        let mut range = self.current_range();
        self.eat(TokenKind::Keyword(KeywordKind::Try));
        self.expect_or_skip(TokenKind::Colon);

        let mut is_star = false;
        let mut has_except = false;
        let mut has_finally = false;

        let (try_body, _) = self.parse_body();

        let mut handlers = vec![];
        loop {
            let mut except_range = self.current_range();
            if self.eat(TokenKind::Keyword(KeywordKind::Except)) {
                has_except = true;
            } else {
                break;
            }

            is_star = self.eat(TokenKind::Operator(OperatorKind::Asterisk));

            let ty = if self.at(TokenKind::Colon) && !is_star {
                None
            } else {
                let (expr, expr_range) = self.parse_exprs();
                if self.last_ctx.contains(ParserCtxFlags::TUPLE_EXPR) && matches!(expr, Expression::Tuple(_)) {
                    self.add_error(
                        ParseErrorType::Other("multiple exception types must be parenthesized".to_string()),
                        expr_range,
                    );
                }
                Some(Box::new(expr))
            };

            let name = if self.eat(TokenKind::Keyword(KeywordKind::As)) {
                Some(self.parse_identifier())
            } else {
                None
            };

            self.expect_or_skip(TokenKind::Colon);

            let (except_body, except_body_range) = self.parse_body();

            except_range = except_range.cover(except_body_range);
            range = range.cover(except_range);

            handlers.push(nodes::ExceptHandler {
                ty,
                name,
                body: except_body,
                range: except_range,
            });

            if !self.at(TokenKind::Keyword(KeywordKind::Except)) {
                break;
            }
        }

        let orelse = if self.eat(TokenKind::Keyword(KeywordKind::Else)) {
            self.expect_or_skip(TokenKind::Colon);

            let (else_body, else_body_range) = self.parse_body();
            range = range.cover(else_body_range);
            else_body
        } else {
            vec![]
        };

        let final_body = if self.eat(TokenKind::Keyword(KeywordKind::Finally)) {
            has_finally = true;
            self.expect_or_skip(TokenKind::Colon);

            let (finally_body, finally_body_range) = self.parse_body();
            range = range.cover(finally_body_range);
            finally_body
        } else {
            vec![]
        };

        if !has_except && !has_finally {
            let range = self.current_range();
            self.add_error(
                ParseErrorType::Other("expecting `except` or `finally` after `try` block".to_string()),
                range,
            );
        }

        (
            Statement::Try(nodes::TryStmt {
                body: try_body,
                handlers,
                orelse,
                final_body,
                is_star,
                range,
            }),
            range,
        )
    }

    fn parse_decorators(&mut self) -> StmtWithRange<'src> {
        let range = self.current_range();
        let mut decorators = vec![];

        while self.eat(TokenKind::Operator(OperatorKind::At)) {
            let (expr, expr_range) = self.parse_exprs();
            decorators.push(nodes::Decorator {
                expr,
                range: range.cover(expr_range),
            });
            self.eat(TokenKind::NewLine);
        }

        let token = self.current_token();
        match token.kind() {
            TokenKind::Keyword(KeywordKind::Def) => self.parse_func_def_stmt(decorators, range),
            TokenKind::Keyword(KeywordKind::Class) => self.parse_class_def_stmt(decorators, range),
            TokenKind::Keyword(KeywordKind::Async)
                if self.lookahead(1).kind() == TokenKind::Keyword(KeywordKind::Def) =>
            {
                let mut async_range = self.current_range();
                self.eat(TokenKind::Keyword(KeywordKind::Async));

                let (Statement::FunctionDef(mut func), stmt_range) = self.parse_func_def_stmt(decorators, range) else {
                    unreachable!()
                };

                async_range = async_range.cover(stmt_range);
                func.range = async_range;

                (Statement::AsyncFunctionDef(func), async_range)
            }
            _ => {
                self.add_error(
                    ParseErrorType::Other(
                        "expected class, function definition or async function definition after decorator".to_string(),
                    ),
                    token.range(),
                );
                self.parse_statement()
            }
        }
    }

    fn parse_func_def_stmt(
        &mut self,
        decorators: Vec<nodes::Decorator<'src>>,
        func_range: TextRange,
    ) -> StmtWithRange<'src> {
        self.set_ctx(ParserCtxFlags::FUNC_DEF_STMT);

        self.eat(TokenKind::Keyword(KeywordKind::Def));
        let name = self.parse_identifier();
        let type_params = if self.at(TokenKind::OpenBracket) {
            Some(self.parse_type_params())
        } else {
            None
        };

        self.eat(TokenKind::OpenParenthesis);
        let parameters = self.parse_parameters();
        self.expect_or_skip(TokenKind::CloseParenthesis);

        let returns = if self.eat(TokenKind::RightArrow) {
            let (returns, range) = self.parse_exprs();
            if self.last_ctx.contains(ParserCtxFlags::TUPLE_EXPR) && matches!(returns, Expression::Tuple(_)) {
                self.add_error(
                    ParseErrorType::Other("multiple return types must be parenthesized".to_string()),
                    range,
                );
            }
            Some(Box::new(returns))
        } else {
            None
        };

        self.expect_or_skip(TokenKind::Colon);

        let (body, body_range) = self.parse_body();
        let range = func_range.cover(body_range);

        self.clear_ctx(ParserCtxFlags::FUNC_DEF_STMT);

        (
            Statement::FunctionDef(nodes::FunctionDefStmt {
                name,
                type_params,
                parameters: Box::new(parameters),
                body,
                decorators,
                returns,
                range,
            }),
            range,
        )
    }

    fn parse_class_def_stmt(
        &mut self,
        decorators: Vec<nodes::Decorator<'src>>,
        class_range: TextRange,
    ) -> StmtWithRange<'src> {
        self.set_ctx(ParserCtxFlags::CLASS_DEF_STMT);

        self.eat(TokenKind::Keyword(KeywordKind::Class));

        let name = self.parse_identifier();
        let type_params = if self.at(TokenKind::OpenBracket) {
            Some(self.parse_type_params())
        } else {
            None
        };
        let arguments = if self.at(TokenKind::OpenParenthesis) {
            Some(Box::new(self.parse_arguments()))
        } else {
            None
        };

        self.expect_or_skip(TokenKind::Colon);

        let (body, body_range) = self.parse_body();
        let range = class_range.cover(body_range);

        self.clear_ctx(ParserCtxFlags::CLASS_DEF_STMT);

        (
            Statement::Class(nodes::ClassDefStmt {
                decorators,
                name,
                type_params,
                arguments,
                body,
                range,
            }),
            range,
        )
    }

    fn parse_with_item(&mut self) -> nodes::WithItem<'src> {
        let (item, mut range) = self.parse_expr();

        let target = if self.eat(TokenKind::Keyword(KeywordKind::As)) {
            let (mut target, target_range) = self.parse_expr();
            range = range.cover(target_range);

            if matches!(target, Expression::BoolOp(_) | Expression::Compare(_)) {
                // Should we make `target` an `Expression::Invalid` here?
                self.add_error(
                    ParseErrorType::Other(format!("expression `{:?}` not allowed in `with` statement", target)),
                    target_range,
                );
            }

            helpers::set_expr_ctx(&mut target, ContextExpr::Store);

            Some(target)
        } else {
            None
        };

        nodes::WithItem { item, target, range }
    }

    fn parse_with_items(&mut self) -> Vec<nodes::WithItem<'src>> {
        let mut items = vec![];

        if !self.at_expr() {
            let range = self.current_range();
            self.add_error(
                ParseErrorType::Other("expecting expression after `with` keyword".to_string()),
                range,
            );
            return items;
        }

        let mut is_as_keyword_outside_parens = false;
        let has_seen_open_paren = self.at(TokenKind::OpenParenthesis);

        // Consider the two `WithItem` examples below::
        //      1) `(a) as A`
        //      2) `(a)`
        //
        // In the first example, the `item` contains a parenthesized expression,
        // while the second example is a parenthesized `WithItem`. This situation
        // introduces ambiguity during parsing. When encountering an opening parenthesis
        // `(,` the parser may initially assume it's parsing a parenthesized `WithItem`.
        // However, this assumption doesn't hold for the first case, `(a) as A`, where
        // `(a)` represents a parenthesized expression.
        //
        // To disambiguate, the following heuristic was created. We lookahead in the
        // token stream to locate the `as` keyword outside of parenthesis. If the `as`
        // keyword is found outiside of parenthesis, we're dealing with case 1; otherwise
        // it's case 2.
        if self.at(TokenKind::OpenParenthesis) {
            let mut paren_nesting: u32 = 1;
            let mut index = 1;
            loop {
                let kind = self.lookahead(index).kind();

                if kind == TokenKind::OpenParenthesis {
                    paren_nesting += 1;
                }

                if kind == TokenKind::CloseParenthesis {
                    paren_nesting -= 1;
                }

                if paren_nesting == 0 && kind == TokenKind::Keyword(KeywordKind::As) {
                    is_as_keyword_outside_parens = true;
                }

                if matches!(kind, TokenKind::Colon | TokenKind::NewLine) {
                    break;
                }

                index += 1;
            }
        }

        if !is_as_keyword_outside_parens && has_seen_open_paren {
            self.eat(TokenKind::OpenParenthesis);
        }

        parse_separated! {
            self,
            delim=TokenKind::Comma,
            ending=if has_seen_open_paren { TokenKind::CloseParenthesis } else { TokenKind::Colon },
            // only allow a trailing delimiter, in this case a comma, if we've seen a `(`
            allow_trailing_delim=has_seen_open_paren,
            parsing={
                items.push(self.parse_with_item());
            }
        };

        if !is_as_keyword_outside_parens && has_seen_open_paren {
            self.expect(TokenKind::CloseParenthesis);
        }

        items
    }

    fn parse_with_stmt(&mut self) -> StmtWithRange<'src> {
        self.set_ctx(ParserCtxFlags::WITH_STMT);
        let mut range = self.current_range();

        self.eat(TokenKind::Keyword(KeywordKind::With));

        let items = self.parse_with_items();
        self.expect_or_skip(TokenKind::Colon);

        let (body, body_range) = self.parse_body();
        range = range.cover(body_range);

        self.clear_ctx(ParserCtxFlags::WITH_STMT);

        (Statement::With(nodes::WithStmt { items, body, range }), range)
    }

    fn parse_assign_stmt(&mut self, target_stmt: Statement<'src>, mut range: TextRange) -> StmtWithRange<'src> {
        self.eat(TokenKind::Operator(OperatorKind::Assign));
        let Statement::Expression(mut target) = target_stmt else {
            unreachable!()
        };

        helpers::set_expr_ctx(&mut target.value, ContextExpr::Store);

        let mut targets = vec![*target.value];
        let (mut value, value_range) = self.parse_exprs();
        range = range.cover(value_range);

        while self.eat(TokenKind::Operator(OperatorKind::Assign)) {
            let (mut expr, expr_range) = self.parse_exprs();

            std::mem::swap(&mut value, &mut expr);

            range = range.cover(expr_range);
            targets.push(expr);
        }

        (
            Statement::Assign(nodes::AssignStmt {
                targets,
                value: Box::new(value),
                range,
            }),
            range,
        )
    }

    fn parse_ann_assign_stmt(&mut self, target: Statement<'src>, mut range: TextRange) -> StmtWithRange<'src> {
        self.eat(TokenKind::Colon);
        let Statement::Expression(mut target) = target else {
            unreachable!()
        };

        if self.last_ctx.intersects(ParserCtxFlags::TUPLE_EXPR) {
            // Should we make `target` an `Expression::Invalid` here?
            self.add_error(
                ParseErrorType::Other("unparenthesized tuple cannot have type annotation".to_string()),
                range,
            );
        }

        helpers::set_expr_ctx(&mut target.value, ContextExpr::Store);

        let simple = matches!(target.value.as_ref(), Expression::Id(_))
            && !self.last_ctx.intersects(ParserCtxFlags::PARENTHESIZED_EXPR);
        let (annotation, ann_range) = self.parse_expr();
        range = range.cover(ann_range);

        let value = if self.eat(TokenKind::Operator(OperatorKind::Assign)) {
            let (value, value_range) = self.parse_exprs();
            range = range.cover(value_range);

            Some(Box::new(value))
        } else {
            None
        };

        (
            Statement::AnnAssign(nodes::AnnAssignStmt {
                target: target.value,
                annotation: Box::new(annotation),
                value,
                simple,
                range,
            }),
            range,
        )
    }

    fn parse_aug_assign_stmt(
        &mut self,
        target: Statement<'src>,
        op: Operator,
        mut range: TextRange,
    ) -> StmtWithRange<'src> {
        // Consume the operator
        self.next_token();
        let Statement::Expression(mut target) = target else {
            unreachable!()
        };

        helpers::set_expr_ctx(&mut target.value, ContextExpr::Store);

        let (value, value_range) = self.parse_exprs();
        range = range.cover(value_range);

        (
            Statement::AugAssign(nodes::AugAssignStmt {
                target: target.value,
                op,
                value: Box::new(value),
                range,
            }),
            range,
        )
    }

    fn parse_simple_stmt_newline(&mut self) -> StmtWithRange<'src> {
        let stmt = self.parse_simple_stmt();

        self.last_ctx = ParserCtxFlags::empty();
        let has_eaten_semicolon = self.eat(TokenKind::SemiColon);
        let has_eaten_newline = self.eat(TokenKind::NewLine);

        if !has_eaten_newline && !has_eaten_semicolon && self.at_simple_stmt() {
            let range = self.current_range();
            self.add_error(ParseErrorType::SimpleStmtsInSameLine, stmt.1.cover(range));
        }

        if !has_eaten_newline && self.at_compound_stmt() {
            let range = self.current_range();
            self.add_error(ParseErrorType::SimpleStmtAndCompoundStmtInSameLine, stmt.1.cover(range));
        }

        stmt
    }

    fn parse_simple_stmts(&mut self) -> (Vec<Statement<'src>>, TextRange) {
        let mut range;
        let mut stmts = vec![];

        loop {
            let (stmt, stmt_range) = self.parse_simple_stmt();
            stmts.push(stmt);
            range = stmt_range;

            if !self.eat(TokenKind::SemiColon) {
                if self.at_simple_stmt() {
                    // TODO: cover range here?
                    self.add_error(ParseErrorType::SimpleStmtsInSameLine, range);
                } else {
                    break;
                }
            }

            if !self.at_simple_stmt() {
                break;
            }
        }

        if !self.eat(TokenKind::NewLine) && self.at_compound_stmt() {
            self.add_error(ParseErrorType::SimpleStmtAndCompoundStmtInSameLine, range);
        }

        (stmts, range)
    }

    fn parse_simple_stmt(&mut self) -> StmtWithRange<'src> {
        let token = self.current_token();
        let stmt = match token.kind() {
            TokenKind::Keyword(KeywordKind::Del) => self.parse_del_stmt(token.range()),
            TokenKind::Keyword(KeywordKind::Pass) => self.parse_pass_stmt(token.range()),
            TokenKind::Keyword(KeywordKind::Break) => self.parse_break_stmt(token.range()),
            TokenKind::Keyword(KeywordKind::Raise) => self.parse_raise_stmt(token.range()),
            TokenKind::Keyword(KeywordKind::Assert) => self.parse_assert_stmt(token.range()),
            TokenKind::Keyword(KeywordKind::Global) => self.parse_global_stmt(token.range()),
            TokenKind::Keyword(KeywordKind::Import) => self.parse_import_stmt(token.range()),
            TokenKind::Keyword(KeywordKind::Return) => self.parse_return_stmt(token.range()),
            TokenKind::Keyword(KeywordKind::From) => self.parse_import_from_stmt(token.range()),
            TokenKind::Keyword(KeywordKind::Continue) => self.parse_continue_stmt(token.range()),
            TokenKind::Keyword(KeywordKind::NonLocal) => self.parse_nonlocal_stmt(token.range()),
            TokenKind::SoftKeyword(SoftKeywordKind::Type) => self.parse_type_stmt(token.range()),
            _ => {
                let (expr, range) = self.parse_expr_stmt();

                let stmt = if self.at(TokenKind::Operator(OperatorKind::Assign)) {
                    self.parse_assign_stmt(expr, range)
                } else if self.at(TokenKind::Colon) {
                    self.parse_ann_assign_stmt(expr, range)
                } else if let Ok(op) = Operator::try_from(self.current_token().kind()) {
                    self.parse_aug_assign_stmt(expr, op, range)
                } else {
                    (expr, range)
                };

                stmt
            }
        };

        stmt
    }

    #[inline]
    fn parse_pass_stmt(&mut self, range: TextRange) -> StmtWithRange<'src> {
        self.eat(TokenKind::Keyword(KeywordKind::Pass));
        (Statement::Pass(nodes::PassStmt { range }), range)
    }

    #[inline]
    fn parse_continue_stmt(&mut self, range: TextRange) -> StmtWithRange<'src> {
        self.eat(TokenKind::Keyword(KeywordKind::Continue));
        (Statement::Continue(nodes::ContinueStmt { range }), range)
    }

    #[inline]
    fn parse_break_stmt(&mut self, range: TextRange) -> StmtWithRange<'src> {
        self.eat(TokenKind::Keyword(KeywordKind::Break));
        (Statement::Break(nodes::BreakStmt { range }), range)
    }

    fn parse_del_stmt(&mut self, del_range: TextRange) -> StmtWithRange<'src> {
        self.set_ctx(ParserCtxFlags::DEL_STMT);
        let mut targets = vec![];

        self.eat(TokenKind::Keyword(KeywordKind::Del));

        let mut range = parse_separated! {
            self,
            delim=TokenKind::Comma,
            ending=TokenKind::NewLine,
            allow_trailing_delim=true,
            parsing={
                let (target, target_range) = self.parse_expr();
                if matches!(target, Expression::BoolOp(_) | Expression::Compare(_)) {
                    // Should we make `target` an `Expression::Invalid` here?
                    self.add_error(ParseErrorType::Other(format!(
                        "`{}` not allowed in `del` statement",
                        self.src_text(target_range)
                    )), target_range);
                }
                targets.push(target);
            }
        };
        range = del_range.cover(range);
        self.clear_ctx(ParserCtxFlags::DEL_STMT);

        (Statement::Del(nodes::DelStmt { targets, range }), range)
    }

    fn parse_assert_stmt(&mut self, mut range: TextRange) -> StmtWithRange<'src> {
        self.eat(TokenKind::Keyword(KeywordKind::Assert));

        let (test, test_range) = self.parse_expr();
        range = range.cover(test_range);

        let message = if self.eat(TokenKind::Comma) {
            let (msg, msg_range) = self.parse_expr();
            range = range.cover(msg_range);

            Some(Box::new(msg))
        } else {
            None
        };

        (
            Statement::Assert(nodes::AssertStmt {
                test: Box::new(test),
                message,
                range,
            }),
            range,
        )
    }

    fn parse_global_stmt(&mut self, global_range: TextRange) -> StmtWithRange<'src> {
        self.eat(TokenKind::Keyword(KeywordKind::Global));

        let mut names = vec![];

        let mut range = parse_separated! {
            self,
            delim=TokenKind::Comma,
            ending=TokenKind::NewLine,
            allow_trailing_delim=false,
            parsing={
                names.push(self.parse_identifier());
            }
        };
        range = global_range.cover(range);

        (Statement::Global(nodes::GlobalStmt { names, range }), range)
    }

    fn parse_nonlocal_stmt(&mut self, nonlocal_range: TextRange) -> StmtWithRange<'src> {
        self.eat(TokenKind::Keyword(KeywordKind::NonLocal));

        let mut names = vec![];

        let mut range = parse_separated! {
            self,
            delim=TokenKind::Comma,
            ending=TokenKind::NewLine,
            allow_trailing_delim=false,
            parsing={
                names.push(self.parse_identifier());
            }
        };
        range = nonlocal_range.cover(range);

        (Statement::NonLocal(nodes::NonLocalStmt { names, range }), range)
    }

    fn parse_return_stmt(&mut self, mut range: TextRange) -> StmtWithRange<'src> {
        self.eat(TokenKind::Keyword(KeywordKind::Return));

        let value = if self.at(TokenKind::NewLine) {
            None
        } else {
            let (value, value_range) = self.parse_exprs();
            range = range.cover(value_range);
            Some(Box::new(value))
        };

        (Statement::Return(nodes::ReturnStmt { value, range }), range)
    }

    fn parse_raise_stmt(&mut self, mut range: TextRange) -> StmtWithRange<'src> {
        self.eat(TokenKind::Keyword(KeywordKind::Raise));

        let exc = if self.at(TokenKind::NewLine) {
            None
        } else {
            let (exc, exc_range) = self.parse_exprs();
            range = range.cover(exc_range);

            Some(Box::new(exc))
        };

        if exc
            .as_ref()
            .is_some_and(|expr| matches!(expr.as_ref(), Expression::Tuple(_)))
            && !self.last_ctx.intersects(ParserCtxFlags::PARENTHESIZED_EXPR)
        {
            // TODO: use only the tuple range here
            // Should we make `exc` an `Expression::Invalid` here?
            self.add_error(
                ParseErrorType::Other("unparenthesized tuple not allowed in `raise` statement".to_string()),
                range,
            );
        }

        let cause = if exc.is_some() && self.eat(TokenKind::Keyword(KeywordKind::From)) {
            let (cause, cause_range) = self.parse_exprs();
            range = range.cover(cause_range);

            Some(Box::new(cause))
        } else {
            None
        };

        if cause
            .as_ref()
            .is_some_and(|expr| matches!(expr.as_ref(), Expression::Tuple(_)))
            && !self.last_ctx.intersects(ParserCtxFlags::PARENTHESIZED_EXPR)
        {
            // TODO: use only the tuple range here
            self.add_error(
                ParseErrorType::Other("unparenthesized tuple not allowed in `raise from` statement".to_string()),
                range,
            );
        }

        (Statement::Raise(nodes::RaiseStmt { exc, cause, range }), range)
    }

    fn parse_type_stmt(&mut self, range: TextRange) -> StmtWithRange<'src> {
        self.eat(TokenKind::SoftKeyword(SoftKeywordKind::Type));

        let (name, _) = self.parse_id_expr();
        let type_params = if self.at(TokenKind::OpenBracket) {
            Some(self.parse_type_params())
        } else {
            None
        };
        self.expect_or_skip(TokenKind::Operator(OperatorKind::Assign));

        let (value, value_range) = self.parse_expr();
        let range = range.cover(value_range);

        (
            Statement::TypeAlias(nodes::TypeAlias {
                name: Box::new(name),
                type_params,
                value: Box::new(value),
                range,
            }),
            range,
        )
    }

    fn parse_type_params(&mut self) -> nodes::TypeParams<'src> {
        let mut type_params = vec![];
        let range = parse_delimited! {
            self,
            openning=TokenKind::OpenBracket,
            closing=TokenKind::CloseBracket,
            delim=TokenKind::Comma,
            allow_trailing_delim=true,
            parsing={
               type_params.push(self.parse_type_param());
            }
        };

        nodes::TypeParams { type_params, range }
    }

    fn parse_type_param(&mut self) -> nodes::TypeParam<'src> {
        let mut range = self.current_range();
        if self.eat(TokenKind::Operator(OperatorKind::Asterisk)) {
            let name = self.parse_identifier();
            nodes::TypeParam::TypeVarTuple(nodes::TypeParamTypeVarTuple {
                range: range.cover(name.range()),
                name,
            })
        } else if self.eat(TokenKind::Operator(OperatorKind::Exponent)) {
            let name = self.parse_identifier();
            nodes::TypeParam::ParamSpec(nodes::TypeParamSpec {
                range: range.cover(name.range()),
                name,
            })
        } else {
            let name = self.parse_identifier();
            let bound = if self.eat(TokenKind::Colon) {
                let (bound, bound_range) = self.parse_expr();
                range = range.cover(bound_range);
                Some(Box::new(bound))
            } else {
                None
            };
            nodes::TypeParam::TypeVar(nodes::TypeParamTypeVar { range, name, bound })
        }
    }

    fn parse_dotted_name(&mut self) -> nodes::Identifier<'src> {
        let id = self.parse_identifier();
        let mut range = id.range();

        while self.eat(TokenKind::Dot) {
            let id = self.parse_identifier();
            if !id.is_valid() {
                self.add_error(ParseErrorType::InvalidIdentifier, id.range());
            }
            range = range.cover(id.range());
        }

        nodes::Identifier {
            id: self.src_text(range),
            range,
        }
    }

    fn parse_alias(&mut self) -> nodes::Alias<'src> {
        let token = self.current_token();
        if token.kind() == TokenKind::Operator(OperatorKind::Asterisk) {
            self.eat(TokenKind::Operator(OperatorKind::Asterisk));
            return nodes::Alias {
                name: nodes::Identifier {
                    id: "*",
                    range: token.range(),
                },
                asname: None,
                range: token.range(),
            };
        }

        let mut name = self.parse_dotted_name();

        let asname = if self.eat(TokenKind::Keyword(KeywordKind::As)) {
            let id = self.parse_identifier();
            name.range = name.range.cover(id.range());
            Some(id)
        } else {
            None
        };

        nodes::Alias {
            range: name.range,
            name,
            asname,
        }
    }

    fn parse_import_stmt(&mut self, import_range: TextRange) -> StmtWithRange<'src> {
        self.eat(TokenKind::Keyword(KeywordKind::Import));

        let mut names = vec![];
        let mut range = parse_separated! {
            self,
            delim=TokenKind::Comma,
            ending=TokenKind::NewLine,
            allow_trailing_delim=false,
            parsing={
                names.push(self.parse_alias());
            }
        };
        range = import_range.cover(range);

        (Statement::Import(nodes::ImportStmt { names, range }), range)
    }

    fn parse_import_from_stmt(&mut self, from_range: TextRange) -> StmtWithRange<'src> {
        self.eat(TokenKind::Keyword(KeywordKind::From));

        let mut module = None;
        let mut level = if self.eat(TokenKind::Ellipsis) { 3 } else { 0 };

        while self.at(TokenKind::Dot) || self.at(TokenKind::Ellipsis) {
            if self.eat(TokenKind::Dot) {
                level += 1;
            }

            if self.eat(TokenKind::Ellipsis) {
                level += 3;
            }
        }

        if self.at(TokenKind::Id) {
            module = Some(self.parse_dotted_name());
        };

        if level == 0 && module.is_none() {
            let range = self.current_range();
            self.add_error(ParseErrorType::Other("missing module name".to_string()), range);
        }

        self.expect_or_skip(TokenKind::Keyword(KeywordKind::Import));

        let mut names = vec![];
        let mut range = if self.at(TokenKind::OpenParenthesis) {
            parse_delimited! {
                self,
                openning=TokenKind::OpenParenthesis,
                closing=TokenKind::CloseParenthesis,
                delim=TokenKind::Comma,
                allow_trailing_delim=true,
                parsing={
                    names.push(self.parse_alias());
                }
            }
        } else {
            parse_separated! {
                self,
                delim=TokenKind::Comma,
                ending=TokenKind::NewLine,
                allow_trailing_delim=false,
                parsing={
                    names.push(self.parse_alias());
                }
            }
        };
        range = from_range.cover(range);

        (
            Statement::ImportFrom(nodes::ImportFromStmt {
                module,
                names,
                level,
                range,
            }),
            range,
        )
    }

    fn parse_if_stmt(&mut self) -> StmtWithRange<'src> {
        self.set_ctx(ParserCtxFlags::IF_STMT);
        let mut if_range = self.current_range();
        assert!(self.eat(TokenKind::Keyword(KeywordKind::If)));

        let (test, _) = self.parse_expr_or_add_error("expecting expression after `if` keyword");
        self.expect_or_skip(TokenKind::Colon);

        let (body, body_range) = self.parse_body();
        if_range = if_range.cover(body_range);

        let elif_else_clauses = if matches!(
            self.current_token().kind(),
            TokenKind::Keyword(KeywordKind::Else | KeywordKind::Elif)
        ) {
            let (elif_else_clauses, range) = self.parse_elif_else_clauses();
            if_range = if_range.cover(range);

            elif_else_clauses
        } else {
            vec![]
        };

        self.clear_ctx(ParserCtxFlags::IF_STMT);
        (
            Statement::If(nodes::IfStmt {
                test: Box::new(test),
                body,
                elif_else_clauses,
                range: if_range,
            }),
            if_range,
        )
    }

    fn parse_elif_else_clauses(&mut self) -> (Vec<nodes::ElifElseClause<'src>>, TextRange) {
        let mut elif_else_stmts = vec![];
        let mut range = self.current_range();
        while self.at(TokenKind::Keyword(KeywordKind::Elif)) {
            let elif_range = self.current_range();
            self.eat(TokenKind::Keyword(KeywordKind::Elif));

            let (test, _) = self.parse_expr_or_add_error("expecting expression after `elif` keyword");
            self.expect_or_skip(TokenKind::Colon);

            let (body, body_range) = self.parse_body();
            range = body_range;
            elif_else_stmts.push(nodes::ElifElseClause {
                test: Some(test),
                body,
                range: elif_range.cover(body_range),
            });
        }

        if self.at(TokenKind::Keyword(KeywordKind::Else)) {
            let else_range = self.current_range();
            self.eat(TokenKind::Keyword(KeywordKind::Else));
            self.expect_or_skip(TokenKind::Colon);

            let (body, body_range) = self.parse_body();
            range = body_range;
            elif_else_stmts.push(nodes::ElifElseClause {
                test: None,
                body,
                range: else_range.cover(body_range),
            });
        }

        (elif_else_stmts, range)
    }

    fn parse_body(&mut self) -> (Vec<Statement<'src>>, TextRange) {
        let mut last_stmt_range = TextRange::default();
        let mut stmts = vec![];

        // Check if we are currently at a simple statement
        if !self.eat(TokenKind::NewLine) && self.at_simple_stmt() {
            return self.parse_simple_stmts();
        }

        if self.eat(TokenKind::Indent) {
            while !matches!(
                self.current_token().kind(),
                TokenKind::Eof | TokenKind::NewLine | TokenKind::Dedent
            ) {
                if self.at(TokenKind::Indent) {
                    last_stmt_range = self
                        .handle_unexpected_indentation(&mut stmts, "indentation doesn't match previous indentation");
                    continue;
                }
                let (stmt, stmt_range) = self.parse_statement();
                last_stmt_range = stmt_range;
                stmts.push(stmt);
            }

            self.eat(TokenKind::Dedent);
        } else {
            let ctx_str = match self.ctx {
                ParserCtxFlags::IF_STMT => Some("`if` statement"),
                ParserCtxFlags::FOR_STMT => Some("`for` statement"),
                ParserCtxFlags::WITH_STMT => Some("`with` statement"),
                ParserCtxFlags::WHILE_STMT => Some("`while` statement"),
                ParserCtxFlags::CLASS_DEF_STMT => Some("`class` definition"),
                ParserCtxFlags::FUNC_DEF_STMT => Some("function definition"),
                _ => None,
            };
            if let Some(ctx_str) = ctx_str {
                let range = self.current_range();
                self.add_error(
                    ParseErrorType::Other(format!("expected an indented block after {ctx_str}")),
                    range,
                );
            }
        }

        (stmts, last_stmt_range)
    }

    fn parse_expr_stmt(&mut self) -> StmtWithRange<'src> {
        let (expr, range) = self.parse_exprs();

        (
            Statement::Expression(nodes::ExpressionStmt {
                value: Box::new(expr),
                range,
            }),
            range,
        )
    }

    /// Parses every Python expression.
    fn parse_exprs(&mut self) -> ExprWithRange<'src> {
        let (expr, expr_range) = self.parse_expr();

        if self.at(TokenKind::Comma) {
            return self.parse_tuple_expr(expr, expr_range);
        }
        (expr, expr_range)
    }

    /// Parses every Python expression except unparenthesized tuple.
    ///
    /// If you have expressions separated by commas and want to parse then individually,
    /// instead of a tuple, use this function!
    fn parse_expr(&mut self) -> ExprWithRange<'src> {
        let (expr, expr_range) = self.parse_expr_simple();

        if self.at(TokenKind::Keyword(KeywordKind::If)) {
            return self.parse_if_expr(expr, expr_range);
        }

        (expr, expr_range)
    }

    /// Parses every Python expression except unparenthesized tuple and `if` expression.
    fn parse_expr_simple(&mut self) -> ExprWithRange<'src> {
        self.expr_bp(1)
    }

    fn parse_expr_simple_or_add_error(&mut self, error_msg: impl Display) -> ExprWithRange<'src> {
        if self.at_expr() {
            self.parse_expr_simple()
        } else {
            let range = self.current_range();
            self.add_error(ParseErrorType::Other(error_msg.to_string()), range);
            (Expression::Invalid(range), range)
        }
    }

    fn parse_expr_or_add_error(&mut self, error_msg: impl Display) -> ExprWithRange<'src> {
        if self.at_expr() {
            self.parse_expr()
        } else {
            let range = self.current_range();
            self.add_error(ParseErrorType::Other(error_msg.to_string()), range);
            (Expression::Invalid(range), range)
        }
    }

    fn parse_exprs_or_add_error(&mut self, error_msg: impl Display) -> ExprWithRange<'src> {
        if self.at_expr() {
            self.parse_exprs()
        } else {
            let range = self.current_range();
            self.add_error(ParseErrorType::Other(error_msg.to_string()), range);
            (Expression::Invalid(range), range)
        }
    }

    /// Binding powers of operators for a Pratt parser.
    ///
    /// See <https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html>
    fn current_op(&mut self) -> (u8, TokenKind, Associativity) {
        let kind = self.current_token().kind();
        const NOT_AN_OP: (u8, TokenKind, Associativity) = (0, TokenKind::Invalid, Associativity::Left);

        match kind {
            TokenKind::Keyword(KeywordKind::Or) => (4, kind, Associativity::Left),
            TokenKind::Keyword(KeywordKind::And) => (5, kind, Associativity::Left),
            TokenKind::Operator(OperatorKind::BitwiseOr) => (8, kind, Associativity::Left),
            TokenKind::Keyword(KeywordKind::Not) if self.lookahead(1).kind() == TokenKind::Keyword(KeywordKind::In) => {
                (7, kind, Associativity::Left)
            }
            TokenKind::Keyword(KeywordKind::Is | KeywordKind::In)
            | TokenKind::Operator(
                OperatorKind::Equals
                | OperatorKind::NotEquals
                | OperatorKind::LessThan
                | OperatorKind::LessThanOrEqual
                | OperatorKind::GreaterThan
                | OperatorKind::GreaterThanOrEqual,
            ) => (7, kind, Associativity::Left),
            TokenKind::Operator(OperatorKind::BitwiseXor) => (9, kind, Associativity::Left),
            TokenKind::Operator(OperatorKind::BitwiseAnd) => (10, kind, Associativity::Left),
            TokenKind::Operator(OperatorKind::BitwiseLeftShift | OperatorKind::BitwiseRightShift) => {
                (11, kind, Associativity::Left)
            }
            TokenKind::Operator(OperatorKind::Plus | OperatorKind::Minus) => (12, kind, Associativity::Left),
            TokenKind::Operator(
                OperatorKind::Asterisk
                | OperatorKind::Slash
                | OperatorKind::DoubleSlash
                | OperatorKind::Modulus
                | OperatorKind::At,
            ) => (14, kind, Associativity::Left),
            TokenKind::Operator(OperatorKind::Exponent) => (18, kind, Associativity::Right),
            _ => NOT_AN_OP,
        }
    }

    /// Parses expression with binding power of at least bp.
    ///
    /// Uses the Pratt parser algorithm.
    /// See <https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html>
    fn expr_bp(&mut self, bp: u8) -> ExprWithRange<'src> {
        let (mut lhs, mut lhs_range) = self.parse_lhs();

        loop {
            let (op_bp, op, associativity) = self.current_op();
            if op_bp < bp {
                break;
            }

            // Don't parse a `CompareExpr` if we are parsing a `Comprehension` or `ForStmt`
            if op.is_compare_op()
                && self.has_in_curr_or_parent_ctx(ParserCtxFlags::COMPREHENSION_TARGET | ParserCtxFlags::FOR_TARGET)
            {
                break;
            }

            let op_bp = match associativity {
                Associativity::Left => op_bp + 1,
                Associativity::Right => op_bp,
            };

            self.eat(op);

            // We need to create a dedicated node for boolean operations,
            // even though boolean operations are infix.
            if op.is_bool_op() {
                (lhs, lhs_range) = self.parse_bool_op_expr(lhs, lhs_range, op, op_bp);
                continue;
            }

            // Same here as well
            if op.is_compare_op() {
                (lhs, lhs_range) = self.parse_compare_op_expr(lhs, lhs_range, op, op_bp);
                continue;
            }

            let (rhs, rhs_range) = self.expr_bp(op_bp);
            lhs_range = lhs_range.cover(rhs_range);
            lhs = Expression::BinaryOp(nodes::BinaryOpExpr {
                left: Box::new(lhs),
                op: BinaryOp::try_from(op).expect("invalid binary operator"),
                right: Box::new(rhs),
                range: lhs_range,
            });
        }

        (lhs, lhs_range)
    }

    fn parse_lhs(&mut self) -> ExprWithRange<'src> {
        let token = self.current_token();
        let (mut lhs, mut lhs_range) = match token.kind() {
            TokenKind::Operator(OperatorKind::Plus | OperatorKind::Minus | OperatorKind::BitwiseNot)
            | TokenKind::Keyword(KeywordKind::Not) => self.parse_unary_expr(),
            TokenKind::Operator(OperatorKind::Asterisk) => self.parse_starred_expr(),
            TokenKind::Operator(OperatorKind::Exponent) if self.has_ctx(ParserCtxFlags::BRACESIZED_EXPR) => {
                self.set_ctx(ParserCtxFlags::DICT_EXPR);
                self.eat(TokenKind::Operator(OperatorKind::Exponent));

                let (value, _) = self.parse_expr();
                self.parse_dict_expr(None, value)
            }
            TokenKind::Keyword(KeywordKind::Await) => self.parse_await_expr(),
            TokenKind::Keyword(KeywordKind::Lambda) => self.parse_lambda_expr(),
            _ => self.parse_atom(),
        };

        if self.is_current_token_postfix() {
            (lhs, lhs_range) = self.parse_postfix_expr(lhs, lhs_range);
        }

        (lhs, lhs_range)
    }

    #[inline]
    fn parse_identifier(&mut self) -> nodes::MaybeIdentifier<'src> {
        let range = self.current_range();
        if self.expect(TokenKind::Id) {
            nodes::MaybeIdentifier::Valid(nodes::Identifier {
                id: self.src_text(range),
                range,
            })
        } else {
            self.next_token();
            nodes::MaybeIdentifier::Invalid(range)
        }
    }

    fn parse_id_expr(&mut self) -> ExprWithRange<'src> {
        let range = self.current_range();
        if self.expect(TokenKind::Id) {
            (
                Expression::Id(nodes::IdExpr {
                    id: self.src_text(range),
                    ctx: self.get_expr_ctx(),
                    range,
                }),
                range,
            )
        } else {
            (Expression::Invalid(range), range)
        }
    }

    fn parse_atom(&mut self) -> ExprWithRange<'src> {
        let token = self.current_token();
        let lhs = match token.kind() {
            TokenKind::Number(NumberKind::Float) => Expression::Literal(nodes::LiteralExpr {
                value: Literal::Float(self.src_text(token.range())),
                range: token.range(),
            }),
            TokenKind::Number(NumberKind::Complex) => Expression::Literal(nodes::LiteralExpr {
                value: Literal::Complex(self.src_text(token.range())),
                range: token.range(),
            }),
            TokenKind::Number(_) => Expression::Literal(nodes::LiteralExpr {
                value: Literal::Int(self.src_text(token.range())),
                range: token.range(),
            }),
            TokenKind::Keyword(KeywordKind::True) => Expression::Literal(nodes::LiteralExpr {
                value: Literal::Bool(BoolValue::True),
                range: token.range(),
            }),
            TokenKind::Keyword(KeywordKind::False) => Expression::Literal(nodes::LiteralExpr {
                value: Literal::Bool(BoolValue::False),
                range: token.range(),
            }),
            TokenKind::Keyword(KeywordKind::None) => Expression::Literal(nodes::LiteralExpr {
                value: Literal::None,
                range: token.range(),
            }),
            TokenKind::Ellipsis => Expression::Ellipsis(nodes::EllipsisExpr { range: token.range() }),
            TokenKind::Id => return self.parse_id_expr(),
            TokenKind::String { kind, is_triple_quote } => return self.parse_string_expr(kind, is_triple_quote),
            TokenKind::FStringStart => return self.parse_fstring_expr(),
            TokenKind::OpenParenthesis => return self.parse_parenthesized_expr(),
            TokenKind::OpenBracket => return self.parse_bracketsized_expr(),
            TokenKind::OpenBrace => return self.parse_bracesized_expr(),
            TokenKind::Keyword(KeywordKind::Yield) => return self.parse_yield_expr(),
            TokenKind::Colon if self.has_in_curr_or_parent_ctx(ParserCtxFlags::SUBSCRIPT_EXPR) => {
                return self.parse_slice(None, token.range())
            }
            _ => return self.handle_unexpected_token(token),
        };

        self.next_token();

        (lhs, token.range())
    }

    /// Handles all unexpected tokens found during the parsing. Depending on the
    /// current context and unexpected token received, a very specific error
    /// message can be created.
    fn handle_unexpected_token(&mut self, token: Token) -> ExprWithRange<'src> {
        self.add_error(
            ParseErrorType::Other(format!("unexpected token `{}`", self.src_text(token.range()))),
            token.range(),
        );
        self.next_token();

        (Expression::Invalid(token.range()), token.range())
    }

    fn parse_postfix_expr(&mut self, mut lhs: Expression<'src>, mut lhs_range: TextRange) -> ExprWithRange<'src> {
        loop {
            (lhs, lhs_range) = match self.current_token().kind() {
                TokenKind::OpenParenthesis => self.parse_call_expr(lhs, lhs_range),
                TokenKind::OpenBracket => self.parse_subscript_expr(lhs, lhs_range),
                TokenKind::Dot => self.parse_attribute_expr(lhs, lhs_range),
                TokenKind::Colon
                    if self.has_in_curr_or_parent_ctx(ParserCtxFlags::SUBSCRIPT_EXPR)
                        && !self.has_ctx(ParserCtxFlags::SLICE_EXPR) =>
                {
                    self.parse_slice(Some(lhs), lhs_range)
                }
                // Can't handle dict comprehension here due to the `parse_dict_comprehension_expr`
                // function requiring as parameters a `key` and `value`. The `value` parameter
                // cannot be obtained here.
                TokenKind::Keyword(KeywordKind::Async | KeywordKind::For)
                    if !self.has_ctx(ParserCtxFlags::DICT_EXPR) =>
                {
                    if self.has_ctx(ParserCtxFlags::ARGUMENTS | ParserCtxFlags::PARENTHESIZED_EXPR) {
                        self.parse_generator_expr(lhs, lhs_range)
                    } else if self.has_ctx(ParserCtxFlags::LIST_EXPR) {
                        self.parse_list_comprehension_expr(lhs, lhs_range)
                    } else if self.has_ctx(ParserCtxFlags::BRACESIZED_EXPR) {
                        self.parse_set_comprehension_expr(lhs, lhs_range)
                    } else {
                        break;
                    }
                }
                TokenKind::Operator(OperatorKind::ColonEqual) => self.parse_named_expr(lhs, lhs_range),
                _ => break,
            };
        }

        (lhs, lhs_range)
    }

    fn parse_call_expr(&mut self, lhs: Expression<'src>, lhs_range: TextRange) -> ExprWithRange<'src> {
        assert!(self.at(TokenKind::OpenParenthesis));
        let args = self.parse_arguments();

        let range = lhs_range.cover(args.range);

        (
            Expression::Call(nodes::CallExpr {
                func: Box::new(lhs),
                args,
                range,
            }),
            range,
        )
    }

    fn parse_arguments(&mut self) -> nodes::Arguments<'src> {
        self.set_ctx(ParserCtxFlags::ARGUMENTS);

        let mut args: Vec<Expression<'src>> = vec![];
        let mut kw_args: Vec<nodes::KeywordArg<'src>> = vec![];
        let mut is_keyword_unpack = false;
        let mut is_pos_arg = true;

        let range = parse_delimited!(
            self,
            openning = TokenKind::OpenParenthesis,
            closing = TokenKind::CloseParenthesis,
            delim = TokenKind::Comma,
            allow_trailing_delim = true,
            parsing = {
                if self.at(TokenKind::Operator(OperatorKind::Exponent)) {
                    let range = self.current_range();
                    self.eat(TokenKind::Operator(OperatorKind::Exponent));

                    let (expr, expr_range) = self.parse_expr();
                    kw_args.push(nodes::KeywordArg {
                        arg: None,
                        value: Box::new(expr),
                        range: range.cover(expr_range),
                    });

                    is_keyword_unpack = true;
                } else {
                    let (expr, expr_range) = self.parse_expr();

                    if is_keyword_unpack && matches!(expr, Expression::Starred(_)) {
                        self.add_error(ParseErrorType::IterableUnpackFollowsKeywordArg, expr_range);
                    }

                    if self.eat(TokenKind::Operator(OperatorKind::Assign)) {
                        is_pos_arg = false;
                        let arg = if let Expression::Id(ident_expr) = expr {
                            nodes::MaybeIdentifier::Valid(nodes::Identifier {
                                id: ident_expr.id,
                                range: ident_expr.range,
                            })
                        } else {
                            self.add_error(
                                ParseErrorType::Other(format!(
                                    "`{}` cannot be used as a keyword argument!",
                                    self.src_text(expr_range)
                                )),
                                expr_range,
                            );
                            nodes::MaybeIdentifier::Invalid(expr_range)
                        };

                        let (value, value_range) = self.parse_expr();

                        kw_args.push(nodes::KeywordArg {
                            arg: Some(arg),
                            value: Box::new(value),
                            range: expr_range.cover(value_range),
                        });
                    } else {
                        args.push(expr);
                    }

                    if is_keyword_unpack && is_pos_arg {
                        self.add_error(ParseErrorType::PosArgFollowsKeywordArgUnpack, expr_range);
                    }
                }
                is_pos_arg = true;
            }
        );
        self.clear_ctx(ParserCtxFlags::ARGUMENTS);

        nodes::Arguments { args, kw_args, range }
    }

    fn parse_subscript_expr(&mut self, value: Expression<'src>, value_range: TextRange) -> ExprWithRange<'src> {
        assert!(self.at(TokenKind::OpenBracket));
        self.set_ctx(ParserCtxFlags::SUBSCRIPT_EXPR);

        assert!(self.eat(TokenKind::OpenBracket));

        let is_current_token_colon = matches!(self.current_token().kind(), TokenKind::Colon);
        // Create an error when receiving a empty slice to parse, e.g. `l[]`
        if !is_current_token_colon && !self.at_expr() {
            let close_bracket_range = self.current_range();
            self.expect_or_skip(TokenKind::CloseBracket);

            let range = value_range.cover(close_bracket_range);
            self.add_error(ParseErrorType::EmptySubscript, range);
            self.clear_ctx(ParserCtxFlags::SUBSCRIPT_EXPR);
            return (
                Expression::Subscript(nodes::SubscriptExpr {
                    value: Box::new(value),
                    slice: Box::new(Expression::Invalid(close_bracket_range.sub_start(1.into()))),
                    ctx: self.get_expr_ctx(),
                    range,
                }),
                range,
            );
        }

        let (mut slice, slice_range) = if is_current_token_colon {
            let range = self.current_range();
            self.parse_slice(None, range)
        } else {
            self.parse_exprs()
        };

        if self.at(TokenKind::Comma) {
            (slice, _) = self.parse_tuple_expr(slice, slice_range);
        }

        let end_range = self.current_range();
        self.expect_or_skip(TokenKind::CloseBracket);

        let range = value_range.cover(end_range);

        self.clear_ctx(ParserCtxFlags::SUBSCRIPT_EXPR);

        (
            Expression::Subscript(nodes::SubscriptExpr {
                value: Box::new(value),
                slice: Box::new(slice),
                ctx: self.get_expr_ctx(),
                range,
            }),
            range,
        )
    }

    fn parse_slice(&mut self, lhs: Option<Expression<'src>>, lhs_range: TextRange) -> ExprWithRange<'src> {
        self.set_ctx(ParserCtxFlags::SLICE_EXPR);
        let mut slice_range = lhs_range;

        let slice = if self.eat(TokenKind::Colon) {
            let lower = lhs.map(Box::new);

            let upper = if matches!(
                self.current_token().kind(),
                TokenKind::Comma | TokenKind::Colon | TokenKind::CloseBracket | TokenKind::NewLine
            ) {
                None
            } else {
                let (upper, upper_range) = self.parse_expr();
                slice_range = slice_range.cover(upper_range);
                Some(Box::new(upper))
            };

            let step = if self.eat(TokenKind::Colon) {
                if matches!(
                    self.current_token().kind(),
                    TokenKind::Comma | TokenKind::CloseBracket | TokenKind::NewLine
                ) {
                    None
                } else {
                    let (step, step_range) = self.parse_expr();
                    slice_range = slice_range.cover(step_range);
                    Some(Box::new(step))
                }
            } else {
                None
            };

            Expression::Slice(nodes::SliceExpr {
                lower,
                upper,
                step,
                range: slice_range,
            })
        } else {
            lhs.unwrap()
        };

        self.clear_ctx(ParserCtxFlags::SLICE_EXPR);

        (slice, slice_range)
    }

    fn parse_unary_expr(&mut self) -> ExprWithRange<'src> {
        let op = self.next_token();
        let (rhs, rhs_range) = self.expr_bp(255);
        let new_range = op.range().cover(rhs_range);

        (
            Expression::UnaryOp(nodes::UnaryOpExpr {
                op: UnaryOp::try_from(op.kind()).expect("invalid unary operator"),
                operand: Box::new(rhs),
                range: new_range,
            }),
            new_range,
        )
    }

    fn parse_attribute_expr(&mut self, lhs: Expression<'src>, lhs_range: TextRange) -> ExprWithRange<'src> {
        assert!(self.eat(TokenKind::Dot));
        let attr = self.parse_identifier();

        let range = lhs_range.cover(attr.range());

        (
            Expression::Attribute(nodes::AttributeExpr {
                value: Box::new(lhs),
                attr,
                ctx: self.get_expr_ctx(),
                range,
            }),
            range,
        )
    }

    fn parse_bool_op_expr(
        &mut self,
        lhs: Expression<'src>,
        mut lhs_range: TextRange,
        op: TokenKind,
        op_bp: u8,
    ) -> ExprWithRange<'src> {
        let mut values = vec![lhs];

        // Keep adding `expr` to `values` until we see a different
        // boolean operation than `op`.
        loop {
            let (expr, expr_range) = self.expr_bp(op_bp);
            lhs_range = lhs_range.cover(expr_range);
            values.push(expr);

            if self.current_token().kind() != op {
                break;
            }

            self.next_token();
        }

        (
            Expression::BoolOp(nodes::BoolOpExpr {
                values,
                op: BoolOp::try_from(op).unwrap(),
                range: lhs_range,
            }),
            lhs_range,
        )
    }

    fn parse_compare_op_expr(
        &mut self,
        lhs: Expression<'src>,
        mut lhs_range: TextRange,
        op: TokenKind,
        op_bp: u8,
    ) -> ExprWithRange<'src> {
        let mut comparators = vec![];
        let op = CompareOp::try_from([op, self.current_token().kind()]).unwrap();
        let mut ops = vec![op];

        if matches!(op, CompareOp::IsNot | CompareOp::NotIn) {
            self.next_token();
        }

        loop {
            let (expr, expr_range) = self.expr_bp(op_bp);
            lhs_range = lhs_range.cover(expr_range);
            comparators.push(expr);

            if let Ok(op) = CompareOp::try_from([self.current_token().kind(), self.lookahead(1).kind()]) {
                if matches!(op, CompareOp::IsNot | CompareOp::NotIn) {
                    self.next_token();
                }

                ops.push(op);
            } else {
                break;
            }

            self.next_token();
        }

        (
            Expression::Compare(nodes::CompareExpr {
                left: Box::new(lhs),
                ops,
                comparators,
                range: lhs_range,
            }),
            lhs_range,
        )
    }

    fn parse_string_expr(&mut self, kind: StringKind, is_triple_quote: bool) -> ExprWithRange<'src> {
        let str_tok = self.next_token();
        let mut str_range = str_tok.range();

        // If another `String` token is found after consuming the current `String` token,
        // then we are parsing an implicit concatenated string.
        let str = if self.current_token().kind().is_str() {
            let mut implict_concatenated_str = String::from(self.src_text(remove_str_quotes(
                str_range,
                (kind.size() as u32).into(),
                is_triple_quote,
            )));

            while self.current_token().kind().is_str() {
                let str_tok = self.next_token();

                let TokenKind::String { is_triple_quote, .. } = str_tok.kind() else {
                    unreachable!()
                };

                str_range = str_range.cover(str_tok.range());

                implict_concatenated_str.push_str(self.src_text(remove_str_quotes(
                    str_tok.range(),
                    (kind.size() as u32).into(),
                    is_triple_quote,
                )));
            }

            Cow::Owned(implict_concatenated_str)
        } else {
            Cow::Borrowed(self.src_text(remove_str_quotes(
                str_range,
                (kind.size() as u32).into(),
                is_triple_quote,
            )))
        };

        let value = if kind == StringKind::Bytes {
            Literal::Bytes(str)
        } else {
            Literal::String(str)
        };

        (
            Expression::Literal(nodes::LiteralExpr {
                value,
                range: str_range,
            }),
            str_range,
        )
    }

    fn parse_fstring_expr(&mut self) -> ExprWithRange<'src> {
        let mut range = self.current_range();
        let mut values = vec![];

        self.eat(TokenKind::FStringStart);
        while !matches!(
            self.current_token().kind(),
            TokenKind::FStringEnd | TokenKind::CloseBrace | TokenKind::NewLine | TokenKind::Eof
        ) {
            let (expr, expr_range) = if self.at(TokenKind::OpenBrace) {
                self.parse_formatted_value_expr()
            } else if matches!(self.current_token().kind(), TokenKind::FStringMiddle { .. }) {
                self.parse_fstring_middle()
            } else if self.at_expr() {
                self.parse_expr()
            } else {
                let range = self.current_range();
                self.next_token();
                (Expression::Invalid(range), range)
            };
            range = range.cover(expr_range);
            values.push(expr);
        }

        if self.at(TokenKind::FStringEnd) {
            range = range.cover(self.current_range());
            self.eat(TokenKind::FStringEnd);
        }

        (Expression::FString(nodes::FStringExpr { values, range }), range)
    }

    fn parse_fstring_middle(&mut self) -> ExprWithRange<'src> {
        let range = self.next_token().range();

        (
            Expression::Literal(nodes::LiteralExpr {
                value: Literal::String(Cow::Borrowed(self.src_text(range))),
                range,
            }),
            range,
        )
    }

    fn parse_formatted_value_expr(&mut self) -> ExprWithRange<'src> {
        let mut range = self.current_range();

        self.eat(TokenKind::OpenBrace);
        let (value, value_range) = self.parse_expr_or_add_error("f-string: empty expression is not allowed");
        if !self.last_ctx.contains(ParserCtxFlags::PARENTHESIZED_EXPR) && matches!(value, Expression::Lambda(_)) {
            self.add_error(
                ParseErrorType::Other("f-string: unparenthesized `lambda` expression is not allowed".into()),
                value_range,
            )
        }
        let debug_text = if self.eat(TokenKind::Operator(OperatorKind::Assign)) {
            let leading_range = range.add_start(1.into()).cover_offset(value_range.start());
            let trailing_range = TextRange::new(value_range.end(), self.current_range().start());
            Some(nodes::DebugText {
                leading: self.src_text(leading_range),
                trailing: self.src_text(trailing_range),
            })
        } else {
            None
        };

        let conversion = if self.eat(TokenKind::Exclamation) {
            let token = self.next_token();
            match self.src_text(token.range()) {
                "s" => ConversionFlag::Str,
                "r" => ConversionFlag::Repr,
                "a" => ConversionFlag::Ascii,
                c => {
                    self.add_error(
                        ParseErrorType::Other(format!(
                            "f-string: invalid conversion character `{c}`: expected `s`, `r`, or `a`"
                        )),
                        token.range(),
                    );
                    ConversionFlag::None
                }
            }
        } else {
            ConversionFlag::None
        };

        let format_spec = if self.eat(TokenKind::Colon) {
            Some(Box::new(self.parse_fstring_expr().0))
        } else {
            None
        };

        range = range.cover(self.current_range());
        self.eat(TokenKind::CloseBrace);

        (
            Expression::FormattedValue(nodes::FormattedValueExpr {
                value: Box::new(value),
                debug_text,
                conversion,
                format_spec,
                range,
            }),
            range,
        )
    }

    fn parse_bracketsized_expr(&mut self) -> ExprWithRange<'src> {
        self.set_ctx(ParserCtxFlags::LIST_EXPR);
        let open_bracket_range = self.current_range();

        assert!(self.eat(TokenKind::OpenBracket));

        // Nice error message when having a unclosed open bracket `[`
        if matches!(self.current_token().kind(), TokenKind::NewLine | TokenKind::Eof) {
            let range = self.current_range();
            self.add_error(ParseErrorType::Other("missing closing bracket `]`".to_string()), range);
        }

        // Return an empty `ListExpr` when finding a `]` right after the `[`
        if self.at(TokenKind::CloseBracket) {
            let close_bracket_range = self.current_range();
            let range = open_bracket_range.cover(close_bracket_range);

            self.eat(TokenKind::CloseBracket);
            self.clear_ctx(ParserCtxFlags::LIST_EXPR);

            return (
                Expression::List(nodes::ListExpr {
                    elements: vec![],
                    ctx: self.get_expr_ctx(),
                    range,
                }),
                range,
            );
        }

        let (mut expr, _) = self.parse_expr();

        if !matches!(expr, Expression::ListComp(_)) {
            (expr, _) = self.parse_list_expr(expr);
        }

        let close_bracket_range = self.current_range();
        self.expect_or_skip(TokenKind::CloseBracket);

        let range = open_bracket_range.cover(close_bracket_range);

        // Update the range of `Expression::List` or `Expression::ListComp` to
        // include the parenthesis.
        if matches!(expr, Expression::List(_) | Expression::ListComp(_)) {
            helpers::set_expr_range(&mut expr, range);
        }

        self.clear_ctx(ParserCtxFlags::LIST_EXPR);

        (expr, range)
    }

    fn parse_bracesized_expr(&mut self) -> ExprWithRange<'src> {
        self.set_ctx(ParserCtxFlags::BRACESIZED_EXPR);
        let open_brace_range = self.current_range();

        assert!(self.eat(TokenKind::OpenBrace));

        // Nice error message when having a unclosed open brace `{`
        if matches!(self.current_token().kind(), TokenKind::NewLine | TokenKind::Eof) {
            let range = self.current_range();
            self.add_error(ParseErrorType::Other("missing closing brace `}`".to_string()), range);
        }

        // Return an empty `DictExpr` when finding a `}` right after the `{`
        if self.at(TokenKind::CloseBrace) {
            let close_brace_range = self.current_range();
            let range = open_brace_range.cover(close_brace_range);

            self.eat(TokenKind::CloseBrace);
            self.clear_ctx(ParserCtxFlags::BRACESIZED_EXPR);

            return (
                Expression::Dict(nodes::DictExpr {
                    keys: vec![],
                    values: vec![],
                    range,
                }),
                range,
            );
        }
        let (mut expr, expr_range) = self.parse_expr();

        // We can have a `DictExpr` at this point if we see this expression `{**d}`.
        if !matches!(expr, Expression::SetComp(_) | Expression::Dict(_)) && !self.at(TokenKind::Colon) {
            (expr, _) = self.parse_set_expr(expr);
        }

        if !matches!(expr, Expression::Dict(_)) && self.eat(TokenKind::Colon) {
            self.set_ctx(ParserCtxFlags::DICT_EXPR);
            let (value, value_range) = self.parse_expr();
            let range = expr_range.cover(value_range);

            (expr, _) = match self.current_token().kind() {
                TokenKind::Keyword(KeywordKind::For) | TokenKind::Keyword(KeywordKind::Async) => {
                    self.clear_ctx(ParserCtxFlags::DICT_EXPR);
                    self.parse_dict_comprehension_expr(expr, value, range)
                }
                _ => self.parse_dict_expr(Some(expr), value),
            };
        }

        let close_brace_range = self.current_range();
        self.expect_or_skip(TokenKind::CloseBrace);

        let range = open_brace_range.cover(close_brace_range);

        // Update the range of `Expression::Set`, `Expression::Dict` and
        // `Expression::DictComp` to include the parenthesis.
        if matches!(expr, Expression::Set(_) | Expression::Dict(_) | Expression::DictComp(_)) {
            helpers::set_expr_range(&mut expr, range);
        }

        self.clear_ctx(ParserCtxFlags::BRACESIZED_EXPR);

        (expr, range)
    }

    fn parse_parenthesized_expr(&mut self) -> ExprWithRange<'src> {
        self.set_ctx(ParserCtxFlags::PARENTHESIZED_EXPR);
        let open_paren_range = self.current_range();

        assert!(self.eat(TokenKind::OpenParenthesis));

        // Nice error message when having a unclosed open parenthesis `(`
        if matches!(self.current_token().kind(), TokenKind::NewLine | TokenKind::Eof) {
            let range = self.current_range();
            self.add_error(
                ParseErrorType::Other("missing closing parenthesis `)`".to_string()),
                range,
            );
        }

        // Return an empty `TupleExpr` when finding a `)` right after the `(`
        if self.at(TokenKind::CloseParenthesis) {
            let close_paren_range = self.current_range();
            let range = open_paren_range.cover(close_paren_range);

            self.eat(TokenKind::CloseParenthesis);
            self.clear_ctx(ParserCtxFlags::PARENTHESIZED_EXPR);

            return (
                Expression::Tuple(nodes::TupleExpr {
                    elements: vec![],
                    ctx: self.get_expr_ctx(),
                    range,
                }),
                range,
            );
        }

        let (mut expr, expr_range) = self.parse_expr();

        if self.at(TokenKind::Comma) {
            (expr, _) = self.parse_tuple_expr(expr, expr_range);
        }

        let close_paren_range = self.current_range();
        self.expect_or_skip(TokenKind::CloseParenthesis);

        let range = open_paren_range.cover(close_paren_range);

        // Update the range of `Expression::Tuple` or `Expression::Generator` to
        // include the parenthesis.
        if matches!(expr, Expression::Tuple(_) | Expression::Generator(_)) {
            helpers::set_expr_range(&mut expr, range);
        }

        self.clear_ctx(ParserCtxFlags::PARENTHESIZED_EXPR);

        (expr, range)
    }

    fn parse_tuple_expr(
        &mut self,
        first_element: Expression<'src>,
        first_element_range: TextRange,
    ) -> ExprWithRange<'src> {
        self.eat(TokenKind::Comma);

        let mut elements = vec![first_element];

        self.set_ctx(ParserCtxFlags::TUPLE_EXPR);

        while self.at_expr() {
            let (expr, _) = self.parse_expr();
            elements.push(expr);

            if !self.eat(TokenKind::Comma) {
                if self.at_expr() {
                    self.expect(TokenKind::Comma);
                } else {
                    break;
                }
            }
        }

        let range = first_element_range.cover_offset(self.current_range().start());
        self.clear_ctx(ParserCtxFlags::TUPLE_EXPR);

        (
            Expression::Tuple(nodes::TupleExpr {
                elements,
                ctx: self.get_expr_ctx(),
                range,
            }),
            range,
        )
    }

    fn parse_list_expr(&mut self, first_element: Expression<'src>) -> ExprWithRange<'src> {
        self.eat(TokenKind::Comma);
        let mut elements = vec![first_element];

        let range = parse_separated! {
            self,
            delim=TokenKind::Comma,
            ending=TokenKind::CloseBracket,
            allow_trailing_delim=true,
            parsing={
                let (expr, _) = self.parse_expr();
                elements.push(expr);
            }
        };

        (
            Expression::List(nodes::ListExpr {
                elements,
                ctx: self.get_expr_ctx(),
                range,
            }),
            range,
        )
    }

    fn parse_set_expr(&mut self, first_element: Expression<'src>) -> ExprWithRange<'src> {
        self.eat(TokenKind::Comma);
        let mut elements = vec![first_element];

        let range = parse_separated! {
            self,
            delim=TokenKind::Comma,
            ending=TokenKind::CloseBrace,
            allow_trailing_delim=true,
            parsing={
                let (expr, _) = self.parse_expr();
                elements.push(expr);
            }
        };

        (Expression::Set(nodes::SetExpr { elements, range }), range)
    }

    fn parse_dict_expr(&mut self, key: Option<Expression<'src>>, value: Expression<'src>) -> ExprWithRange<'src> {
        self.eat(TokenKind::Comma);

        let mut keys = vec![key];
        let mut values = vec![value];

        let range = parse_separated! {
            self,
            delim=TokenKind::Comma,
            ending=TokenKind::CloseBrace,
            allow_trailing_delim=true,
            parsing={
                if self.eat(TokenKind::Operator(OperatorKind::Exponent)) {
                    keys.push(None);
                } else {
                    let (key, _) = self.parse_expr();
                    keys.push(Some(key));

                    self.expect_or_skip(TokenKind::Colon);
                }
                let (value, _) = self.parse_expr();
                values.push(value);
            }
        };

        self.clear_ctx(ParserCtxFlags::DICT_EXPR);

        (Expression::Dict(nodes::DictExpr { keys, values, range }), range)
    }

    fn parse_comprehension(&mut self) -> nodes::Comprehension<'src> {
        assert!(self.at(TokenKind::Keyword(KeywordKind::For)) || self.at(TokenKind::Keyword(KeywordKind::Async)));

        let mut range = self.current_range();

        let is_async = self.eat(TokenKind::Keyword(KeywordKind::Async));
        self.expect_or_skip(TokenKind::Keyword(KeywordKind::For));

        self.set_ctx(ParserCtxFlags::COMPREHENSION_TARGET);
        let (target, _) = self.parse_exprs_or_add_error("expecting expression after `for` keyword");
        self.clear_ctx(ParserCtxFlags::COMPREHENSION_TARGET);

        self.expect_or_skip(TokenKind::Keyword(KeywordKind::In));

        let (iter, iter_expr) = self.parse_expr_simple_or_add_error("expecting expression after `in` keyword");
        range = range.cover(iter_expr);

        let mut ifs = vec![];
        while self.eat(TokenKind::Keyword(KeywordKind::If)) {
            let (if_expr, if_range) = self.parse_expr_simple();
            ifs.push(if_expr);
            range = range.cover(if_range);
        }

        nodes::Comprehension {
            target,
            iter,
            ifs,
            is_async,
            range,
        }
    }

    fn parse_generators(&mut self, mut range: TextRange) -> (Vec<nodes::Comprehension<'src>>, TextRange) {
        let mut generators = vec![];
        while matches!(
            self.current_token().kind(),
            TokenKind::Keyword(KeywordKind::For) | TokenKind::Keyword(KeywordKind::Async)
        ) {
            let comp = self.parse_comprehension();
            range = range.cover(comp.range);

            generators.push(comp);
        }

        (generators, range)
    }

    fn parse_generator_expr(&mut self, element: Expression<'src>, element_range: TextRange) -> ExprWithRange<'src> {
        self.set_ctx(ParserCtxFlags::GENERATOR_EXPR);
        let (generators, range) = self.parse_generators(element_range);
        self.clear_ctx(ParserCtxFlags::GENERATOR_EXPR);

        (
            Expression::Generator(nodes::GeneratorExpr {
                element: Box::new(element),
                generators,
                range,
            }),
            range,
        )
    }

    fn parse_list_comprehension_expr(
        &mut self,
        element: Expression<'src>,
        element_range: TextRange,
    ) -> ExprWithRange<'src> {
        self.set_ctx(ParserCtxFlags::LIST_COMP_EXPR);
        let (generators, range) = self.parse_generators(element_range);
        self.clear_ctx(ParserCtxFlags::LIST_COMP_EXPR);

        (
            Expression::ListComp(nodes::ListCompExpr {
                element: Box::new(element),
                generators,
                range,
            }),
            range,
        )
    }

    fn parse_dict_comprehension_expr(
        &mut self,
        key: Expression<'src>,
        value: Expression<'src>,
        range: TextRange,
    ) -> ExprWithRange<'src> {
        self.set_ctx(ParserCtxFlags::DICT_COMP_EXPR);
        let (generators, range) = self.parse_generators(range);
        self.clear_ctx(ParserCtxFlags::DICT_COMP_EXPR);

        (
            Expression::DictComp(nodes::DictCompExpr {
                key: Box::new(key),
                value: Box::new(value),
                generators,
                range,
            }),
            range,
        )
    }

    fn parse_set_comprehension_expr(
        &mut self,
        element: Expression<'src>,
        element_range: TextRange,
    ) -> ExprWithRange<'src> {
        self.set_ctx(ParserCtxFlags::SET_COMP_EXPR);
        let (generators, range) = self.parse_generators(element_range);
        self.clear_ctx(ParserCtxFlags::SET_COMP_EXPR);

        (
            Expression::SetComp(nodes::SetCompExpr {
                element: Box::new(element),
                generators,
                range,
            }),
            range,
        )
    }

    fn parse_starred_expr(&mut self) -> ExprWithRange<'src> {
        let mut star_range = self.next_token().range();
        let (expr, expr_range) = self.parse_expr();
        star_range = star_range.cover(expr_range);

        (
            Expression::Starred(nodes::StarredExpr {
                value: Box::new(expr),
                ctx: self.get_expr_ctx(),
                range: star_range,
            }),
            star_range,
        )
    }

    fn parse_await_expr(&mut self) -> ExprWithRange<'src> {
        self.set_ctx(ParserCtxFlags::AWAIT_EXPR);

        let mut await_range = self.current_range();
        self.eat(TokenKind::Keyword(KeywordKind::Await));

        let (expr, expr_range) = self.parse_expr_simple();
        await_range = await_range.cover(expr_range);

        if matches!(expr, Expression::Starred(_)) {
            self.add_error(
                ParseErrorType::Other(format!(
                    "starred expression `{}` is not allowed in an `await` statement",
                    self.src_text(expr_range)
                )),
                expr_range,
            );
        }

        self.clear_ctx(ParserCtxFlags::AWAIT_EXPR);
        (
            Expression::Await(nodes::AwaitExpr {
                value: Box::new(expr),
                range: await_range,
            }),
            await_range,
        )
    }

    fn parse_yield_expr(&mut self) -> ExprWithRange<'src> {
        let mut yield_range = self.current_range();
        self.eat(TokenKind::Keyword(KeywordKind::Yield));

        if self.eat(TokenKind::Keyword(KeywordKind::From)) {
            return self.parse_yield_from_expr(yield_range);
        }

        let value = if self.at_expr() {
            let (expr, expr_range) = self.parse_exprs();
            yield_range = yield_range.cover(expr_range);

            Some(Box::new(expr))
        } else {
            None
        };

        (
            Expression::Yield(nodes::YieldExpr {
                value,
                range: yield_range,
            }),
            yield_range,
        )
    }

    fn parse_yield_from_expr(&mut self, mut yield_range: TextRange) -> ExprWithRange<'src> {
        // TODO: don't allow unparenthesized tuples here
        let (expr, expr_range) = self.parse_exprs();
        yield_range = yield_range.cover(expr_range);

        if matches!(expr, Expression::Starred(_)) {
            // Should we make `expr` an `Expression::Invalid` here?
            self.add_error(
                ParseErrorType::Other(format!(
                    "starred expression `{}` is not allowed in a `yield from` statement",
                    self.src_text(expr_range)
                )),
                expr_range,
            );
        }

        (
            Expression::YieldFrom(nodes::YieldFromExpr {
                value: Box::new(expr),
                range: yield_range,
            }),
            yield_range,
        )
    }

    fn parse_if_expr(&mut self, body: Expression<'src>, body_range: TextRange) -> ExprWithRange<'src> {
        assert!(self.eat(TokenKind::Keyword(KeywordKind::If)));

        let (test, _) = self.parse_expr_simple();

        self.expect_or_skip(TokenKind::Keyword(KeywordKind::Else));

        let (orelse, orelse_range) = self.parse_expr();
        let if_range = body_range.cover(orelse_range);

        (
            Expression::IfElse(nodes::IfElseExpr {
                body: Box::new(body),
                test: Box::new(test),
                orelse: Box::new(orelse),
                range: if_range,
            }),
            if_range,
        )
    }

    fn parse_lambda_expr(&mut self) -> ExprWithRange<'src> {
        self.set_ctx(ParserCtxFlags::LAMBDA_EXPR);
        let mut range = self.current_range();

        assert!(self.eat(TokenKind::Keyword(KeywordKind::Lambda)));

        let parameters: Option<Box<nodes::Parameters>> = if self.at(TokenKind::Colon) {
            None
        } else {
            Some(Box::new(self.parse_parameters()))
        };

        self.expect_or_skip(TokenKind::Colon);

        // Check for forbidden tokens in the `lambda`'s body
        let token = self.current_token();
        match token.kind() {
            TokenKind::Keyword(KeywordKind::Yield) => self.add_error(
                ParseErrorType::Other("`yield` not allowed in a `lambda` expression".to_string()),
                token.range(),
            ),
            TokenKind::Operator(OperatorKind::Asterisk) => {
                self.add_error(
                    ParseErrorType::Other("starred expression not allowed in a `lambda` expression".to_string()),
                    token.range(),
                );
            }
            TokenKind::Operator(OperatorKind::Exponent) => {
                self.add_error(
                    ParseErrorType::Other("double starred expression not allowed in a `lambda` expression".to_string()),
                    token.range(),
                );
            }
            _ => {}
        }

        let (body, body_range) = self.parse_expr();
        range = range.cover(body_range);

        self.clear_ctx(ParserCtxFlags::LAMBDA_EXPR);
        (
            Expression::Lambda(nodes::LambdaExpr {
                body: Box::new(body),
                parameters,
                range,
            }),
            range,
        )
    }

    fn parse_parameter(&mut self) -> nodes::Parameter<'src> {
        let name = self.parse_identifier();
        let mut range = name.range();
        // If we are at a colon and we're currently parsing a `lambda` expression,
        // this is the `lambda`'s body, don't try to parse as an annotation.
        let annotation = if self.at(TokenKind::Colon) && !self.has_in_curr_or_parent_ctx(ParserCtxFlags::LAMBDA_EXPR) {
            self.eat(TokenKind::Colon);
            let (ann, ann_range) = self.parse_expr();
            range = range.cover(ann_range);
            Some(Box::new(ann))
        } else {
            None
        };

        nodes::Parameter {
            name,
            annotation,
            range,
        }
    }

    fn parse_parameter_with_default(&mut self) -> nodes::ParameterWithDefault<'src> {
        let parameter = self.parse_parameter();
        let mut range = parameter.range;

        let default = if self.eat(TokenKind::Operator(OperatorKind::Assign)) {
            let (expr, expr_range) = self.parse_expr();
            range = range.cover(expr_range);
            Some(Box::new(expr))
        } else {
            None
        };

        nodes::ParameterWithDefault {
            parameter,
            default,
            range,
        }
    }

    fn parse_parameters(&mut self) -> nodes::Parameters<'src> {
        let mut args = vec![];
        let mut posonlyargs = vec![];
        let mut kwonlyargs = vec![];
        let mut kwarg = None;
        let mut vararg = None;

        let mut has_seen_asterisk = false;
        let mut has_seen_vararg = false;
        let mut has_seen_default_param = false;
        let mut has_seen_non_default_param = false;

        let ending = if self.has_in_curr_or_parent_ctx(ParserCtxFlags::FUNC_DEF_STMT | ParserCtxFlags::CLASS_DEF_STMT) {
            TokenKind::CloseParenthesis
        } else if self.has_in_curr_or_parent_ctx(ParserCtxFlags::LAMBDA_EXPR) {
            TokenKind::Colon
        } else {
            TokenKind::NewLine
        };

        let range = parse_separated! {
            self,
            delim=TokenKind::Comma,
            ending=ending,
            allow_trailing_delim=true,
            parsing={
                // Don't allow any parameter after we have seen a vararg `**kwargs`
                if has_seen_vararg {
                    let range = self.current_range();
                    self.add_error(ParseErrorType::ParamFollowsVarKeywordParam, range);
                }

                if self.eat(TokenKind::Operator(OperatorKind::Asterisk)) {
                    if self.at(TokenKind::Comma) {
                        has_seen_asterisk = true;
                        has_seen_default_param = false;
                    } else if self.at_expr() {
                        kwarg = Some(Box::new(self.parse_parameter()));
                    }
                } else if self.eat(TokenKind::Operator(OperatorKind::Exponent)) {
                    has_seen_vararg = true;
                    vararg = Some(Box::new(self.parse_parameter()));
                } else if self.eat(TokenKind::Operator(OperatorKind::Slash)) {
                    // Don't allow `/` after a `*`
                    if has_seen_asterisk {
                        let range = self.current_range();
                        self.add_error(
                            ParseErrorType::Other("`/` must be ahead of `*`".to_string()),
                            range
                        );
                    }
                    std::mem::swap(&mut args, &mut posonlyargs);
                } else if self.at(TokenKind::Id) {
                    // Don't allow non-default paramaters after default parameters e.g. `a=1, b`,
                    // can't place `b` after `a=1`. Non-default parameters are only allowed after
                    // default parameters if we have a `*` before them, e.g. `a=1, *, b`.
                    if has_seen_default_param && has_seen_non_default_param {
                        let range = self.current_range();
                        self.add_error(ParseErrorType::NonDefaultParamFollowsDefaultParam, range);
                    }
                    let param = self.parse_parameter_with_default();
                    has_seen_default_param = param.default.is_some();
                    has_seen_non_default_param = param.default.is_none();

                    if has_seen_asterisk {
                        kwonlyargs.push(param);
                    } else {
                        args.push(param);
                    }
                } else {
                    if matches!(
                        self.current_token().kind(),
                        TokenKind::Indent | TokenKind::RightArrow | TokenKind::Colon
                    ) || self.at_compound_stmt()
                      || self.at_simple_stmt()
                    {
                        break;
                    }
                    let range = self.current_range();
                    self.add_error(ParseErrorType::Other("expected parameter".to_string()), range);
                    self.next_token();
                }
            }
        };

        nodes::Parameters {
            kwarg,
            vararg,
            args,
            posonlyargs,
            kwonlyargs,
            range,
        }
    }

    fn parse_named_expr(&mut self, mut target: Expression<'src>, target_range: TextRange) -> ExprWithRange<'src> {
        assert!(self.eat(TokenKind::Operator(OperatorKind::ColonEqual)));

        helpers::set_expr_ctx(&mut target, ContextExpr::Store);

        let (value, value_range) = self.parse_expr();
        let range = target_range.cover(value_range);

        (
            Expression::Named(nodes::NamedExpr {
                target: Box::new(target),
                value: Box::new(value),
                range,
            }),
            range,
        )
    }
}
