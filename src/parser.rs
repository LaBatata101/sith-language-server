use crate::lexer::{
    token::{
        types::{KeywordType, OperatorType, TokenType},
        Span, Token,
    },
    Lexer,
};

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Expression(Expression),
    Block(Block),
    FunctionDef,
    If,
    Assignment(Assignment, Expression),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    String(String, Span),
    Number(String, Span),
    Bool(bool, Span),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Assignment {
    name: String,
    span: Span,
}

impl Assignment {
    pub fn new(name: String, span: Span) -> Self {
        Self { name, span }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Block {
    stmts: Vec<Statement>,
}

impl Block {
    pub fn new() -> Self {
        Self { stmts: Vec::new() }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParsedFile {
    pub stmts: Vec<Statement>,
}

impl ParsedFile {
    pub fn new() -> Self {
        Self { stmts: Vec::new() }
    }
}

pub struct Parser {
    tokens: Vec<Token>,
}

impl Parser {
    pub fn new(text: &str) -> Self {
        let mut lexer = Lexer::new(text);
        lexer.tokenize();

        Self {
            tokens: lexer.tokens().to_vec(),
        }
    }

    pub fn parse(&self) -> ParsedFile {
        let mut parsed_file = ParsedFile::new();
        let mut index = 0;

        while index < self.tokens.len() {
            let token = self.tokens.get(index).unwrap();
            index += 1;

            match &token.kind {
                TokenType::Id(name) => {
                    if self
                        .tokens
                        .get(index)
                        .map_or(false, |token| token.kind == TokenType::Operator(OperatorType::Assign))
                    {
                        index += 1;
                        let (expr, expr_span) = self.parse_expression(&mut index);
                        let assign = Assignment::new(
                            name.clone(),
                            Span {
                                start: token.span.start,
                                end: expr_span.end,
                            },
                        );
                        parsed_file.stmts.push(Statement::Assignment(assign, expr));
                    }
                }
                TokenType::Eof | TokenType::NewLine => break,
                _ => panic!("ERROR: unexpected token {token:?}"),
            }
        }

        parsed_file
    }

    fn parse_expression(&self, index: &mut usize) -> (Expression, Span) {
        let token = self.tokens.get(*index).unwrap();
        let expr = match &token.kind {
            TokenType::String(str) => (Expression::String(str.to_string(), token.span), token.span),
            TokenType::Number(num) => (Expression::Number(num.to_string(), token.span), token.span),
            TokenType::Keyword(KeywordType::True) => (Expression::Bool(true, token.span), token.span),
            TokenType::Keyword(KeywordType::False) => (Expression::Bool(false, token.span), token.span),
            TokenType::NewLine => panic!("Invalid syntax!"),
            _ => panic!("ERROR: Unexpected token! {token:?}"),
        };

        *index += 1;
        let next_token = self.tokens.get(*index).unwrap();
        if matches!(next_token.kind, TokenType::NewLine | TokenType::SemiColon) {
            *index += 1;
            expr
        } else {
            self.parse_expression(index)
        }
    }
}
