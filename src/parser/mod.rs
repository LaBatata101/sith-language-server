pub mod ast;
mod helpers;

use crate::lexer::{
    token::{
        types::{KeywordType, OperatorType, TokenType},
        Span, Token,
    },
    Lexer,
};
use ast::{
    BinaryOperator, Block, ElIfStmt, ElseStmt, Expression, Function, IfStmt, Operation, ParsedFile, Statement,
    UnaryOperator, VarAsgmt, While,
};
use helpers::{infix_binding_power, postfix_binding_power, prefix_binding_power};

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
        let expr = self.pratt_parsing(index, 0);

        if self.tokens.get(*index).map_or(false, |token| {
            matches!(
                &token.kind,
                TokenType::NewLine | TokenType::SemiColon | TokenType::Eof | TokenType::Comma
            )
        }) {
            *index += 1;
        }

        expr
    }

    /// Parse `expressions` using Pratt Parsing algorithm
    fn pratt_parsing(&self, index: &mut usize, min_precedence_weight: u8) -> (Expression, Span) {
        let mut token = self.tokens.get(*index).unwrap();
        let (mut lhs, mut lhs_span) = match &token.kind {
            TokenType::Id(name) => {
                *index += 1;
                (Expression::Id(name.to_string(), token.span), token.span)
            }
            TokenType::String(str) => {
                *index += 1;
                (Expression::String(str.to_string(), token.span), token.span)
            }
            TokenType::Number(num) => {
                *index += 1;
                (Expression::Number(num.to_string(), token.span), token.span)
            }
            TokenType::Keyword(KeywordType::True) => {
                *index += 1;
                (Expression::Bool(true, token.span), token.span)
            }
            TokenType::Keyword(KeywordType::False) => {
                *index += 1;
                (Expression::Bool(false, token.span), token.span)
            }
            TokenType::Operator(
                OperatorType::Plus | OperatorType::Minus | OperatorType::BitwiseNot | OperatorType::Asterisk,
            )
            | TokenType::Keyword(KeywordType::Not) => self.parse_unary_operator(index, token),
            TokenType::OpenParenthesis => self.parse_parenthesized_expr(index, token),
            TokenType::OpenBrackets => self.parse_list_expr(index),
            TokenType::OpenBrace => self.parse_bracesized_expr(index, token),
            TokenType::NewLine | TokenType::SemiColon | TokenType::Eof => panic!("Invalid syntax!"),
            _ => panic!("ERROR: Unexpected token! {token:?}"),
        };

        while self.tokens.get(*index).map_or(false, |token| {
            !matches!(
                &token.kind,
                // Tokens that can end an expression
                TokenType::NewLine
                    | TokenType::SemiColon
                    | TokenType::Colon
                    | TokenType::Eof
                    | TokenType::CloseBrace
                    | TokenType::CloseBrackets
                    | TokenType::CloseParenthesis
                    | TokenType::Comma
            )
        }) {
            token = self.tokens.get(*index).unwrap();

            let op = self.get_expr_operation(token, index);

            if let Some((lhs_bp, ())) = postfix_binding_power(op) {
                if lhs_bp < min_precedence_weight {
                    break;
                }

                *index += 1;

                lhs = match op {
                    Operation::Unary(UnaryOperator::OpenParenthesis) => {
                        // FIXME: Only parsing function calls with no arguments
                        let expr = Expression::Call(Box::new(lhs), lhs_span);
                        assert_eq!(
                            self.tokens.get(*index).map(|token| &token.kind),
                            Some(&TokenType::CloseParenthesis),
                            "Expecting a \")\"! at position: {}",
                            lhs_span.end + 1
                        );
                        *index += 1;
                        expr
                    }
                    Operation::Unary(UnaryOperator::OpenBrackets) => {
                        // FIXME: Only parsing slice with no start, stop or step attributes
                        let (rhs, rhs_span) = self.pratt_parsing(index, 0);
                        assert_eq!(
                            self.tokens.get(*index).map(|token| &token.kind),
                            Some(&TokenType::CloseBrackets),
                            "Expecting a \"]\"! at position: {}",
                            rhs_span.end + 1
                        );
                        *index += 1;

                        Expression::Slice(
                            Box::new(lhs),
                            Box::new(rhs),
                            Span {
                                start: lhs_span.start,
                                end: rhs_span.end + 1,
                            },
                        )
                    }
                    _ => panic!("Invalid postfix operator! {op:?}"),
                };

                continue;
            }

            if let Some((lhs_bp, rhs_bp)) = infix_binding_power(op) {
                if lhs_bp < min_precedence_weight {
                    break;
                }

                *index += 1;

                let (rhs, rhs_span) = self.pratt_parsing(index, rhs_bp);
                lhs_span = Span {
                    start: lhs_span.start,
                    end: rhs_span.end,
                };
                lhs = Expression::BinaryOp(Box::new(lhs), op.get_binary_op(), Box::new(rhs), lhs_span);
                continue;
            }
        }

        (lhs, lhs_span)
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
        if !matches!(token.kind, TokenType::Indent) {
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
        let mut token = self.tokens.get(*index).unwrap();
        *index += 1;

        if token.kind == TokenType::NewLine {
            token = self.tokens.get(*index).unwrap();
            *index += 1;
        }

        match &token.kind {
            TokenType::Id(name) => {
                if self
                    .tokens
                    .get(*index)
                    .map_or(false, |token| token.kind != TokenType::Operator(OperatorType::Assign))
                {
                    panic!("Invalid syntax: expecting '=' got {:?}", token.kind);
                }

                *index += 1;
                let (expr, expr_span) = self.parse_expression(index);

                let assign_span = Span {
                    start: token.span.start,
                    end: expr_span.end,
                };
                let assign = VarAsgmt::new(name.clone(), assign_span);

                (Statement::VarAsgmt(assign, expr), assign_span)
            }
            TokenType::Keyword(KeywordType::Def) => {
                if let Some(Token {
                    kind: TokenType::Id(name),
                    span: func_name_span,
                }) = self.tokens.get(*index)
                {
                    *index += 1;
                    let mut func = self.parse_function(index, name.to_string(), *func_name_span);
                    func.span.start = token.span.start;
                    let func_span = func.span;

                    (Statement::FunctionDef(func), func_span)
                } else {
                    panic!("Invalid syntax for function definition!")
                }
            }
            TokenType::Keyword(KeywordType::If) => {
                let mut if_stmt = self.parse_if(index);
                if_stmt.span.start = token.span.start;
                let if_span = if_stmt.span;

                (Statement::If(if_stmt), if_span)
            }
            TokenType::Keyword(KeywordType::While) => {
                let mut while_stmt = self.parse_while(index);
                while_stmt.span.start = token.span.start;
                let while_span = while_stmt.span;

                (Statement::While(while_stmt), while_span)
            }
            TokenType::Keyword(KeywordType::Pass) => (Statement::Pass(token.span), token.span),
            _ => panic!("ERROR: unexpected token {:?} at position {:?}", token.kind, token.span),
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

    fn parse_while(&self, index: &mut usize) -> While {
        let (condition_expr, _) = self.parse_expression(index);
        let mut token = self.tokens.get(*index).unwrap();
        if !matches!(token.kind, TokenType::Colon) {
            panic!("Invalid syntax: expecting ':' got {:?}", token.kind)
        }
        *index += 1;

        let while_block = self.parse_block(index);

        let mut while_stmt = While {
            condition: condition_expr,
            else_stmt: None,
            span: Span {
                start: 0,
                end: while_block.span.end,
            },
        };

        token = self.tokens.get(*index).unwrap();
        if token.kind == TokenType::Keyword(KeywordType::Else) {
            *index += 1;
            let else_start = token.span.start;
            token = self.tokens.get(*index).unwrap();
            if !matches!(token.kind, TokenType::Colon) {
                panic!("Invalid syntax: expecting ':' got {:?}", token.kind)
            }
            *index += 1;
            let else_block = self.parse_block(index);
            while_stmt.span.end = else_block.span.end;
            while_stmt.else_stmt = Some(ElseStmt {
                span: Span {
                    start: else_start,
                    end: else_block.span.end,
                },
                block: else_block,
            });
        }

        while_stmt
    }

    fn parse_unary_operator(&self, index: &mut usize, token: &Token) -> (Expression, Span) {
        let op = match token.kind {
            TokenType::Operator(OperatorType::BitwiseNot) => Operation::Unary(UnaryOperator::BitwiseNot),
            TokenType::Operator(OperatorType::Plus) => Operation::Unary(UnaryOperator::Plus),
            TokenType::Operator(OperatorType::Minus) => Operation::Unary(UnaryOperator::Minus),
            TokenType::Operator(OperatorType::Asterisk) => Operation::Unary(UnaryOperator::UnpackIterable),
            TokenType::Keyword(KeywordType::Not) => Operation::Unary(UnaryOperator::LogicalNot),
            _ => panic!("ERROR: Unexpected operator! {token:?}"),
        };
        *index += 1;

        let ((), r_bp) = prefix_binding_power(op).unwrap();
        let (rhs, mut rhs_span) = self.pratt_parsing(index, r_bp);
        rhs_span.start = token.span.start;
        (
            Expression::UnaryOp(Box::new(rhs), op.get_unary_op(), rhs_span),
            rhs_span,
        )
    }

    fn parse_parenthesized_expr(&self, index: &mut usize, token: &Token) -> (Expression, Span) {
        // Consume (
        *index += 1;
        let paren_span_start = token.span.start;
        let next_token = self.tokens.get(*index).unwrap();

        if next_token.kind == TokenType::Operator(OperatorType::Asterisk) {
            panic!("SyntaxError: cannot use starred expression inside parenthesis!");
        } else if next_token.kind == TokenType::Operator(OperatorType::Exponent) {
            panic!("SyntaxError: cannot use double starred expression inside parenthesis!");
        }

        let (lhs, lhs_span) = self.pratt_parsing(index, 0);
        if self
            .tokens
            .get(*index)
            .map_or(false, |token| token.kind == TokenType::Comma)
        {
            return self.parse_tuple_expression(index, lhs, paren_span_start);
        }
        assert_eq!(
            self.tokens.get(*index).map(|token| &token.kind),
            Some(&TokenType::CloseParenthesis),
            // FIXME: Showing incorrect position
            "Expecting a \")\"! at position: {}",
            lhs_span.end + 1
        );
        // Consume )
        *index += 1;

        (lhs, lhs_span)
    }

    fn parse_tuple_expression(
        &self,
        index: &mut usize,
        first_expr: Expression,
        tuple_span_start: usize,
    ) -> (Expression, Span) {
        let mut expressions = vec![first_expr];
        let mut tuple_span = Span {
            start: tuple_span_start,
            end: 0,
        };
        let mut last_expr_span = Span { start: 0, end: 0 };

        while self
            .tokens
            .get(*index)
            .map_or(false, |token| token.kind == TokenType::Comma)
        {
            *index += 1;
            let (expr, expr_span) = self.pratt_parsing(index, 0);
            last_expr_span = expr_span;
            expressions.push(expr);
        }

        assert_eq!(
            self.tokens.get(*index).map(|token| &token.kind),
            Some(&TokenType::CloseParenthesis),
            "Expecting a \")\"! at position: {}",
            // FIXME: Showing incorrect position
            last_expr_span.end + 1
        );

        tuple_span.end = self.tokens.get(*index).map(|token| token.span.end).unwrap();

        *index += 1;

        (Expression::Tuple(expressions, tuple_span), tuple_span)
    }

    fn parse_bracesized_expr(&self, index: &mut usize, token: &Token) -> (Expression, Span) {
        // Consume {
        *index += 1;
        let brace_span_start = token.span.start;

        let (lhs, lhs_span) = self.pratt_parsing(index, 0);

        if self
            .tokens
            .get(*index)
            .map_or(false, |token| token.kind == TokenType::Colon)
        {
            return self.parse_dict_expression(index, lhs, brace_span_start);
        }

        let mut expressions = vec![lhs];
        let mut last_expr_span = lhs_span;
        let mut set_span = Span {
            start: brace_span_start,
            end: 0,
        };

        while self
            .tokens
            .get(*index)
            .map_or(false, |token| token.kind == TokenType::Comma)
        {
            *index += 1;
            let (expr, expr_span) = self.pratt_parsing(index, 0);
            last_expr_span = expr_span;

            expressions.push(expr);
        }

        assert_eq!(
            self.tokens.get(*index).map(|token| &token.kind),
            Some(&TokenType::CloseBrace),
            "Expecting a \"}}\"! at position: {}",
            // FIXME: Showing incorrect position
            last_expr_span.end + 1
        );

        set_span.end = self.tokens.get(*index).map(|token| token.span.end).unwrap();
        // Consume }
        *index += 1;

        (Expression::Set(expressions, set_span), set_span)
    }
    fn parse_list_expr(&self, index: &mut usize) -> (Expression, Span) {
        // Consume [
        *index += 1;

        let (lhs, lhs_span) = self.pratt_parsing(index, 0);

        let mut expressions = vec![lhs];
        let mut last_expr_span = Span { start: 0, end: 0 };
        let mut list_span = Span {
            start: lhs_span.start - 1,
            end: 0,
        };

        while self
            .tokens
            .get(*index)
            .map_or(false, |token| token.kind == TokenType::Comma)
        {
            // consume the comma
            *index += 1;

            let (expr, expr_span) = self.pratt_parsing(index, 0);

            last_expr_span = expr_span;
            expressions.push(expr);
        }

        assert_eq!(
            self.tokens.get(*index).map(|token| &token.kind),
            Some(&TokenType::CloseBrackets),
            "Expecting a \"]\"! at position: {}",
            // FIXME: Showing incorrect position
            last_expr_span.end + 1
        );

        list_span.end = self.tokens.get(*index).map(|token| token.span.end).unwrap();

        // Consume ]
        *index += 1;

        (Expression::List(expressions, list_span), list_span)
    }

    fn get_expr_operation(&self, token: &Token, index: &mut usize) -> Operation {
        match token.kind {
            TokenType::Operator(OperatorType::Exponent) => Operation::Binary(BinaryOperator::Exponent),
            TokenType::Operator(OperatorType::Plus) => Operation::Binary(BinaryOperator::Add),
            TokenType::Operator(OperatorType::Minus) => Operation::Binary(BinaryOperator::Subtract),
            TokenType::Operator(OperatorType::Asterisk) => Operation::Binary(BinaryOperator::Multiply),
            TokenType::Operator(OperatorType::Divide) => Operation::Binary(BinaryOperator::Divide),
            TokenType::Operator(OperatorType::FloorDivision) => Operation::Binary(BinaryOperator::FloorDivision),
            TokenType::Operator(OperatorType::Modulus) => Operation::Binary(BinaryOperator::Modulo),
            TokenType::Operator(OperatorType::At) => Operation::Binary(BinaryOperator::At),
            TokenType::Operator(OperatorType::LessThan) => Operation::Binary(BinaryOperator::LessThan),
            TokenType::Operator(OperatorType::LessThanOrEqual) => Operation::Binary(BinaryOperator::LessThanOrEqual),
            TokenType::Operator(OperatorType::GreaterThan) => Operation::Binary(BinaryOperator::GreaterThan),
            TokenType::Operator(OperatorType::GreaterThanOrEqual) => {
                Operation::Binary(BinaryOperator::GreaterThanOrEqual)
            }
            TokenType::Operator(OperatorType::Equals) => Operation::Binary(BinaryOperator::Equals),
            TokenType::Operator(OperatorType::NotEquals) => Operation::Binary(BinaryOperator::NotEqual),
            TokenType::Operator(OperatorType::BitwiseOr) => Operation::Binary(BinaryOperator::BitwiseOr),
            TokenType::Operator(OperatorType::BitwiseAnd) => Operation::Binary(BinaryOperator::BitwiseAnd),
            TokenType::Operator(OperatorType::BitwiseXOR) => Operation::Binary(BinaryOperator::BitwiseXOR),
            TokenType::Operator(OperatorType::BitwiseLeftShift) => Operation::Binary(BinaryOperator::BitwiseLeftShift),
            TokenType::Operator(OperatorType::BitwiseRightShift) => {
                Operation::Binary(BinaryOperator::BitwiseRightShift)
            }
            TokenType::Dot => Operation::Binary(BinaryOperator::AttributeRef),
            TokenType::Keyword(KeywordType::And) => Operation::Binary(BinaryOperator::LogicalAnd),
            TokenType::Keyword(KeywordType::Or) => Operation::Binary(BinaryOperator::LogicalOr),
            TokenType::Keyword(KeywordType::In) => Operation::Binary(BinaryOperator::In),
            TokenType::Keyword(KeywordType::Is) => {
                if self
                    .tokens
                    .get(*index + 1)
                    .map_or(false, |token| token.kind == TokenType::Keyword(KeywordType::Not))
                {
                    *index += 1;
                    Operation::Binary(BinaryOperator::IsNot)
                } else {
                    Operation::Binary(BinaryOperator::Is)
                }
            }
            TokenType::Keyword(KeywordType::Not) => {
                if self
                    .tokens
                    .get(*index + 1)
                    .map_or(false, |token| token.kind == TokenType::Keyword(KeywordType::In))
                {
                    *index += 1;
                    Operation::Binary(BinaryOperator::NotIn)
                } else {
                    Operation::Unary(UnaryOperator::LogicalNot)
                }
            }
            TokenType::OpenParenthesis => Operation::Unary(UnaryOperator::OpenParenthesis),
            TokenType::OpenBrackets => Operation::Unary(UnaryOperator::OpenBrackets),
            _ => panic!("ERROR: Unexpected token! {token:?}"),
        }
    }
}
