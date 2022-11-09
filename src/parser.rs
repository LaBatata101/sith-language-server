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
    FunctionDef(Function),
    If(IfStmt),
    Assignment(Assignment, Expression),
    Pass(Span),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    String(String, Span),
    Number(String, Span),
    Bool(bool, Span),
}

#[derive(Debug, PartialEq, Eq)]
pub struct IfStmt {
    pub condition: Expression,
    pub block: Block,
    pub elif_stms: Vec<ElIfStmt>,
    pub else_stmt: Option<ElseStmt>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ElIfStmt {
    pub condition: Expression,
    pub block: Block,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ElseStmt {
    pub block: Block,
    pub span: Span,
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
pub struct Function {
    pub name: String,
    pub name_span: Span,
    pub block: Block,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Block {
    pub stmts: Vec<Statement>,
    pub span: Span,
}

impl Block {
    pub fn new() -> Self {
        Self {
            stmts: Vec::new(),
            span: Span { start: 0, end: 0 },
        }
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
            if self
                .tokens
                .get(index)
                .map_or(false, |token| token.kind == TokenType::Eof)
            {
                break;
            }
            let (stmt, _) = self.parse_statements(&mut index);
            parsed_file.stmts.push(stmt);
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
            TokenType::NewLine | TokenType::SemiColon | TokenType::Eof => panic!("Invalid syntax!"),
            _ => panic!("ERROR: Unexpected token! {token:?}"),
        };

        *index += 1;
        let next_token = self.tokens.get(*index).unwrap();
        if matches!(
            next_token.kind,
            TokenType::NewLine | TokenType::SemiColon | TokenType::Colon | TokenType::Eof
        ) {
            expr
        } else {
            self.parse_expression(index)
        }
    }

    fn parse_function(&self, index: &mut usize, name: String, name_span: Span) -> Function {
        let mut token = self.tokens.get(*index).unwrap();
        if !matches!(token.kind, TokenType::OpenParenthesis) {
            panic!("Invalid syntax: expecting '(' got {:?}", token.kind)
        }
        *index += 1;
        token = self.tokens.get(*index).unwrap();
        if !matches!(token.kind, TokenType::CloseParenthesis) {
            panic!("Invalid syntax: expecting ')' got {:?}", token.kind)
        }
        *index += 1;
        token = self.tokens.get(*index).unwrap();
        if !matches!(token.kind, TokenType::Colon) {
            panic!("Invalid syntax: expecting ':' got {:?}", token.kind)
        }
        *index += 1;
        let block = self.parse_block(index);

        Function {
            name,
            name_span,
            span: Span {
                start: 0,
                end: block.span.end,
            },
            block,
        }
    }

    fn parse_block(&self, index: &mut usize) -> Block {
        let mut token = self.tokens.get(*index).unwrap();
        if !matches!(token.kind, TokenType::NewLine) {
            panic!("Invalid syntax: expecting 'NEWLINE' got {:?}", token.kind)
        }
        *index += 1;
        token = self.tokens.get(*index).unwrap();
        if !matches!(token.kind, TokenType::Ident) {
            panic!("Expected a indented block!")
        }
        *index += 1;

        let mut block = Block::new();
        block.span.start = self.tokens.get(*index).map(|token| token.span.start).unwrap();

        while let Some(mut token_kind) = self.tokens.get(*index).map(|token| &token.kind) {
            // Not sure if this should be here or in the parse_statements
            if *token_kind == TokenType::NewLine {
                *index += 1;
                token_kind = self.tokens.get(*index).map(|token| &token.kind).unwrap();
            }

            if *token_kind == TokenType::Dedent {
                *index += 1;
                break;
            }

            let (statement, stmt_span) = self.parse_statements(index);
            block.span.end = stmt_span.end;
            block.stmts.push(statement);
        }

        block
    }

    fn parse_statements(&self, index: &mut usize) -> (Statement, Span) {
        let Token { kind, span } = self.tokens.get(*index).unwrap();
        *index += 1;

        if *kind == TokenType::NewLine {
            *index += 1;
        }

        match kind {
            TokenType::Id(name) => {
                let token = self.tokens.get(*index).unwrap();
                if !matches!(token.kind, TokenType::Operator(OperatorType::Assign)) {
                    panic!("Invalid syntax: expecting '=' got {:?}", token.kind);
                }

                *index += 1;
                let (expr, expr_span) = self.parse_expression(index);
                *index += 1;
                let assign_span = Span {
                    start: span.start,
                    end: expr_span.end,
                };
                let assign = Assignment::new(name.clone(), assign_span);

                (Statement::Assignment(assign, expr), assign_span)
            }
            TokenType::Keyword(KeywordType::Def) => {
                if let Some(Token {
                    kind: TokenType::Id(name),
                    span: func_name_span,
                }) = self.tokens.get(*index)
                {
                    *index += 1;
                    let mut func = self.parse_function(index, name.to_string(), *func_name_span);
                    func.span.start = span.start;
                    let func_span = func.span;

                    (Statement::FunctionDef(func), func_span)
                } else {
                    panic!("Invalid syntax for function definition!")
                }
            }
            TokenType::Keyword(KeywordType::If) => {
                let mut if_stmt = self.parse_if(index);
                if_stmt.span.start = span.start;
                let if_span = if_stmt.span;

                (Statement::If(if_stmt), if_span)
            }
            TokenType::Keyword(KeywordType::Pass) => (Statement::Pass(*span), *span),
            _ => panic!("ERROR: unexpected token {kind:?} at position {span:?}"),
        }
    }

    fn parse_if(&self, index: &mut usize) -> IfStmt {
        let (condition_expr, _) = self.parse_expression(index);
        let mut token = self.tokens.get(*index).unwrap();
        if !matches!(token.kind, TokenType::Colon) {
            panic!("Invalid syntax: expecting ':' got {:?}", token.kind)
        }
        *index += 1;

        let block = self.parse_block(index);

        let mut if_stmt = IfStmt {
            condition: condition_expr,
            elif_stms: vec![],
            else_stmt: None,
            span: Span {
                start: 0,
                end: block.span.end,
            },
            block,
        };

        token = self.tokens.get(*index).unwrap();
        if token.kind == TokenType::Keyword(KeywordType::Elif) {
            let mut elif_stms = Vec::new();

            while token.kind == TokenType::Keyword(KeywordType::Elif) {
                *index += 1;
                let (condition_expr, _) = self.parse_expression(index);

                let elif_start = token.span.start;
                token = self.tokens.get(*index).unwrap();
                if !matches!(token.kind, TokenType::Colon) {
                    panic!("Invalid syntax: expecting ':' got {:?}", token.kind)
                }
                *index += 1;

                let elif_block = self.parse_block(index);
                if_stmt.span.end = elif_block.span.end;
                elif_stms.push(ElIfStmt {
                    condition: condition_expr,
                    span: Span {
                        start: elif_start,
                        end: elif_block.span.end,
                    },
                    block: elif_block,
                });

                token = self.tokens.get(*index).unwrap();
            }

            if_stmt.elif_stms = elif_stms;
        }

        if token.kind == TokenType::Keyword(KeywordType::Else) {
            *index += 1;
            let else_start = token.span.start;
            token = self.tokens.get(*index).unwrap();
            if !matches!(token.kind, TokenType::Colon) {
                panic!("Invalid syntax: expecting ':' got {:?}", token.kind)
            }
            *index += 1;
            let else_block = self.parse_block(index);
            if_stmt.span.end = else_block.span.end;
            if_stmt.else_stmt = Some(ElseStmt {
                span: Span {
                    start: else_start,
                    end: else_block.span.end,
                },
                block: else_block,
            });
        }

        if_stmt
    }
}
