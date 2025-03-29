use lsp_types::SemanticTokenType;
use python_ast::{
    self as ast,
    visitor::{self, Visitor},
    Expr, Stmt,
};
use python_parser::{TokenKind, Tokens};
use ruff_text_size::{Ranged, TextRange};

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
struct SemanticToken {
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
    semantic_tokens: Vec<SemanticToken>,
}

// TODO: use type inference
impl<'tokens> SemanticTokenBuilder<'tokens> {
    fn new(tokens: &'tokens Tokens) -> Self {
        Self {
            tokens,
            semantic_tokens: Vec::new(),
        }
    }
    fn build(mut self, suite: &[Stmt]) -> Vec<SemanticToken> {
        for token in self.tokens {
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
        self.semantic_tokens.push(SemanticToken {
            start: range.start().to_u32(),
            length: range.len().to_u32(),
            token_type: typ.value(),
        });
    }
}

impl Visitor<'_> for SemanticTokenBuilder<'_> {
    fn visit_stmt(&mut self, stmt: &'_ python_ast::Stmt) {
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
