use lsp_types::{self as types, SemanticTokenType};
use python_ast::{
    self as ast,
    visitor::{self, Visitor},
    Expr, ModModule, Stmt,
};
use python_parser::{Parsed, Token, TokenKind, Tokens};
use ruff_text_size::{Ranged, TextRange, TextSize};

use crate::session::DocumentRef;

pub(crate) mod full;
pub(crate) mod range;

const SUPPORTED_SEMANTIC_TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::FUNCTION,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::STRING,
    SemanticTokenType::COMMENT,
    SemanticTokenType::NUMBER,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::PARAMETER,
    SemanticTokenType::DECORATOR,
];

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub(crate) enum SupportedSemanticTokens {
    Function,
    Variable,
    String,
    Comment,
    Number,
    Keyword,
    Operator,
    Parameter,
    Decorator,
}

impl SupportedSemanticTokens {
    pub(crate) fn all() -> Vec<SemanticTokenType> {
        SUPPORTED_SEMANTIC_TOKEN_TYPES.to_vec()
    }

    fn value(&self) -> u32 {
        *self as u32
    }
}

#[derive(Debug)]
struct SithSemanticToken {
    /// Byte offset where the token start.
    start: u32,
    /// The length of the token.
    length: u32,
    /// Index of the [`SemanticTokenType`] in the list defined in the server capabilities.
    /// See: [`SemanticTokensLegend#token_types`]
    token_type: u32,
}

struct SemanticTokenBuilder<'tokens> {
    tokens: &'tokens Tokens,
    semantic_tokens: Vec<SithSemanticToken>,
    filter_range: Option<TextRange>,
}

// TODO: use type inference
impl<'tokens> SemanticTokenBuilder<'tokens> {
    fn new(tokens: &'tokens Tokens) -> Self {
        Self {
            tokens,
            semantic_tokens: Vec::new(),
            filter_range: None,
        }
    }

    fn with_filter_range(mut self, range: TextRange) -> Self {
        self.filter_range = Some(range);
        self
    }

    fn build(mut self, suite: &[Stmt]) -> Vec<SithSemanticToken> {
        // Make a copy of `filter_range` so the borrow checker don't be angry
        let range = self.filter_range;
        let filter_range = |token: &&Token| {
            let Some(range) = range else {
                // if `filter_range` is None we are building for `semanticTokens/full`
                return true;
            };
            range.contains_range_with_partial_overlap(token.range())
        };
        for token in self.tokens.iter().filter(filter_range) {
            if token.is_keyword() {
                self.push_keyword_token(token.range());
            } else if token.is_operator() {
                self.push_token(token.range(), SupportedSemanticTokens::Operator);
            } else if token.kind() == TokenKind::Comment {
                self.push_token(token.range(), SupportedSemanticTokens::Comment);
            }
        }

        self.visit_body(suite);
        self.semantic_tokens
    }

    fn push_keyword_token(&mut self, range: TextRange) {
        self.push_token(range, SupportedSemanticTokens::Keyword);
    }

    fn push_token(&mut self, range: TextRange, typ: SupportedSemanticTokens) {
        self.semantic_tokens.push(SithSemanticToken {
            start: range.start().to_u32(),
            length: range.len().to_u32(),
            token_type: typ.value(),
        });
    }
}

impl Visitor<'_> for SemanticTokenBuilder<'_> {
    fn visit_stmt(&mut self, stmt: &python_ast::Stmt) {
        if self
            .filter_range
            .is_some_and(|range| !range.contains_range_with_partial_overlap(stmt.range()))
        {
            return;
        }
        match stmt {
            Stmt::FunctionDef(ast::FunctionDefStmt {
                decorator_list,
                name,
                parameters,
                body,
                ..
            }) => {
                for decorator in decorator_list {
                    self.visit_decorator(decorator);
                }
                self.push_token(name.range(), SupportedSemanticTokens::Function);

                self.visit_parameters(parameters);
                self.visit_body(body);
            }
            Stmt::ClassDef(ast::ClassDefStmt {
                decorator_list,
                name,
                arguments,
                body,
                ..
            }) => {
                for decorator in decorator_list {
                    self.visit_decorator(decorator);
                }
                self.push_token(name.range(), SupportedSemanticTokens::Function);
                if let Some(arguments) = arguments {
                    self.visit_arguments(arguments);
                }
                self.visit_body(body);
            }
            _ => visitor::walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &python_ast::Expr) {
        match expr {
            Expr::StringLiteral(ast::StringLiteralExpr { range, .. })
            | Expr::FString(ast::FStringExpr { range, .. }) => {
                self.push_token(*range, SupportedSemanticTokens::String)
            }
            Expr::NumberLiteral(ast::NumberLiteralExpr { range, .. }) => {
                self.push_token(*range, SupportedSemanticTokens::Number)
            }
            Expr::Name(ast::NameExpr { range, .. }) => {
                self.push_token(*range, SupportedSemanticTokens::Variable)
            }
            _ => visitor::walk_expr(self, expr),
        }
    }

    fn visit_parameter(&mut self, parameter: &python_ast::Parameter) {
        self.push_token(parameter.name.range(), SupportedSemanticTokens::Parameter);
    }

    fn visit_decorator(&mut self, decorator: &python_ast::Decorator) {
        self.push_token(
            decorator.expression.range(),
            SupportedSemanticTokens::Decorator,
        );
    }
}

enum ComputeSemanticTokenOptions {
    Full,
    InRange(TextRange),
}

fn compute_semantic_tokens(
    parsed_file: &Parsed<ModModule>,
    document: &DocumentRef,
    option: ComputeSemanticTokenOptions,
) -> types::SemanticTokens {
    let index = document.index();
    let mut tokens = match option {
        ComputeSemanticTokenOptions::Full => {
            SemanticTokenBuilder::new(parsed_file.tokens()).build(parsed_file.suite())
        }
        ComputeSemanticTokenOptions::InRange(range) => {
            SemanticTokenBuilder::new(parsed_file.tokens())
                .with_filter_range(range)
                .build(parsed_file.suite())
        }
    };
    tokens.sort_by_key(|t| t.start);

    let mut prev_line = 0;
    let mut prev_start = 0;
    let data = tokens
        .into_iter()
        .map(|token| {
            let location = index.source_location(TextSize::from(token.start), document.contents());
            let line = location.row.to_zero_indexed();
            let column = location.column.to_zero_indexed();

            let delta_line = line - prev_line;
            let delta_start = if delta_line == 0 {
                column - prev_start
            } else {
                column
            };
            let result = types::SemanticToken {
                delta_line: delta_line as u32,
                delta_start: delta_start as u32,
                length: token.length,
                token_type: token.token_type,
                token_modifiers_bitset: 0,
            };

            prev_line = line;
            prev_start = column;

            result
        })
        .collect::<Vec<_>>();

    types::SemanticTokens {
        result_id: None,
        data,
    }
}
