pub mod ast;
mod helpers;

use crate::{
    lexer::{
        token::{
            types::{KeywordType, OperatorType, TokenType},
            Span, Token,
        },
        Lexer,
    },
    parser::ast::{DictItemType, IfElseExpr},
};
use ast::{
    BinaryOperator, Block, ElIfStmt, ElseStmt, Expression, Function, IfStmt, Operation, ParsedFile, Statement,
    UnaryOperator, VarAsgmt, While,
};
use helpers::{infix_binding_power, postfix_binding_power, prefix_binding_power};

use self::ast::{ClassStmt, FromImportStmt, FuncParameter, ImportModule, ImportStmt, LambdaExpr, StarParameterType};

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
            TokenType::Ellipsis => {
                *index += 1;
                (Expression::Ellipsis(token.span), token.span)
            }
            TokenType::Operator(
                OperatorType::Plus
                | OperatorType::Minus
                | OperatorType::BitwiseNot
                | OperatorType::Asterisk
                | OperatorType::Exponent,
            )
            | TokenType::Keyword(KeywordType::Not | KeywordType::Await | KeywordType::Lambda) => {
                self.parse_unary_operator(index, token)
            }
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
                    | TokenType::Dedent
                    | TokenType::Keyword(KeywordType::Else)
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

                if op == Operation::Binary(BinaryOperator::IfElse) {
                    lhs = self.parse_if_else_expr(index, lhs, lhs_span);
                    continue;
                }

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

    fn parse_function(&self, index: &mut usize, func_span_start: usize, name: String, name_span: Span) -> Function {
        let mut function = Function {
            name,
            name_span,
            span: Span {
                start: func_span_start,
                end: 0,
            },
            block: Block::default(),
            parameters: vec![],
        };

        let mut token = self.tokens.get(*index).unwrap();
        if !matches!(token.kind, TokenType::OpenParenthesis) {
            panic!("Invalid syntax: expecting '(' got {:?}", token.kind)
        }
        *index += 1;
        token = self.tokens.get(*index).unwrap();
        if token.kind != TokenType::CloseParenthesis {
            function.parameters = self.parse_function_parameters(index);
            token = self.tokens.get(*index).unwrap();
        }

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

        function.span.end = block.span.end;
        function.block = block;

        function
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
                let next_token = self.tokens.get(*index).unwrap();
                if next_token.kind == TokenType::Operator(OperatorType::Assign) {
                    *index += 1;
                    let (expr, expr_span) = self.parse_expression(index);

                    let assign_span = Span {
                        start: token.span.start,
                        end: expr_span.end,
                    };
                    let assign = VarAsgmt::new(name.clone(), assign_span);

                    (Statement::VarAsgmt(assign, expr), assign_span)
                } else if next_token.kind == TokenType::Operator(OperatorType::ColonEqual) {
                    panic!("Syntax Error: Invalid assignment statement!")
                } else {
                    *index -= 1;
                    let (expr, expr_span) = self.parse_expression(index);
                    (Statement::Expression(expr, expr_span), expr_span)
                }
            }
            TokenType::Keyword(KeywordType::Def) => {
                // TODO: move this check into `parse_function`
                if let Some(Token {
                    kind: TokenType::Id(name),
                    span: func_name_span,
                }) = self.tokens.get(*index)
                {
                    *index += 1;
                    let mut func = self.parse_function(index, token.span.start, name.to_string(), *func_name_span);
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
            TokenType::Keyword(KeywordType::Class) => {
                let mut class_stmt = self.parse_class(index);
                class_stmt.span.start = token.span.start;
                let class_span = class_stmt.span;

                (Statement::Class(class_stmt), class_span)
            }
            TokenType::Keyword(KeywordType::Import) => {
                let mut import_stmt = self.parse_import(index);
                import_stmt.span.start = token.span.start;
                let import_span = import_stmt.span;

                (Statement::Import(import_stmt), import_span)
            }
            TokenType::Keyword(KeywordType::From) => {
                let mut from_import_stmt = self.parse_from_import(index);
                from_import_stmt.span.start = token.span.start;
                let from_import_span = from_import_stmt.span;

                (Statement::FromImport(from_import_stmt), from_import_span)
            }
            TokenType::Keyword(KeywordType::Pass) => (Statement::Pass(token.span), token.span),
            TokenType::Keyword(KeywordType::Continue) => (Statement::Continue(token.span), token.span),
            TokenType::Keyword(KeywordType::Break) => (Statement::Break(token.span), token.span),
            _ => {
                *index -= 1;
                let (expr, expr_span) = self.parse_expression(index);
                (Statement::Expression(expr, expr_span), expr_span)
            }
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
            TokenType::Operator(OperatorType::Exponent) => Operation::Unary(UnaryOperator::UnpackDictionary),
            TokenType::Keyword(KeywordType::Not) => Operation::Unary(UnaryOperator::LogicalNot),
            TokenType::Keyword(KeywordType::Await) => Operation::Unary(UnaryOperator::Await),
            TokenType::Keyword(KeywordType::Lambda) => Operation::Unary(UnaryOperator::Lambda),
            _ => panic!("ERROR: Unexpected operator! {token:?}"),
        };
        *index += 1;

        let ((), r_bp) = prefix_binding_power(op).unwrap();

        if op == Operation::Unary(UnaryOperator::Lambda) {
            let parameters = self.parse_function_parameters(index);
            // Consume :
            *index += 1;
            let (expr, expr_span) = self.pratt_parsing(index, r_bp);
            let lambda_span = Span {
                start: token.span.start,
                end: expr_span.end,
            };

            return (
                Expression::Lambda(LambdaExpr {
                    parameters,
                    expression: Box::new(expr),
                    span: lambda_span,
                }),
                lambda_span,
            );
        }

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

            if self.tokens.get(*index).unwrap().kind == TokenType::CloseParenthesis {
                break;
            }

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

        // If we see the "**" operator that means we are unpacking a dictionary, therefore, we
        // should parse as a dictionary instead of a set.
        if self.tokens.get(*index).unwrap().kind == TokenType::Operator(OperatorType::Exponent) {
            let (lhs, lhs_span) = self.pratt_parsing(index, 0);
            assert_eq!(
                self.tokens.get(*index).map(|token| &token.kind),
                Some(&TokenType::Comma),
                "Expecting a \",\"! at position: {}",
                // FIXME: Showing incorrect position
                lhs_span.end + 1
            );
            return self.parse_dict_expression(index, DictItemType::DictUnpack(lhs), brace_span_start);
        }

        let (lhs, lhs_span) = self.pratt_parsing(index, 0);

        if self
            .tokens
            .get(*index)
            .map_or(false, |token| token.kind == TokenType::Colon)
        {
            // Consume :
            *index += 1;
            let (rhs, _) = self.pratt_parsing(index, 0);
            assert_eq!(
                self.tokens.get(*index).map(|token| &token.kind),
                Some(&TokenType::Comma),
                "Expecting a \",\"! at position: {}",
                // FIXME: Showing incorrect position
                lhs_span.end + 1
            );
            return self.parse_dict_expression(index, DictItemType::KeyValue(lhs, rhs), brace_span_start);
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

            if self.tokens.get(*index).unwrap().kind == TokenType::CloseBrace {
                break;
            }

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

    fn parse_dict_expression(
        &self,
        index: &mut usize,
        lhs: DictItemType,
        brace_span_start: usize,
    ) -> (Expression, Span) {
        let mut dict_items = vec![lhs];
        let mut last_expr_span = Span { start: 0, end: 0 };
        let mut dict_span = Span {
            start: brace_span_start,
            end: 0,
        };

        while self.tokens.get(*index).unwrap().kind == TokenType::Comma {
            *index += 1;
            if self.tokens.get(*index).unwrap().kind == TokenType::Operator(OperatorType::Asterisk) {
                panic!("Invalid Syntax: can't unpack iterable inside dictionary!")
            }

            if self.tokens.get(*index).unwrap().kind == TokenType::CloseBrace {
                break;
            }

            let (lhs, lhs_span) = self.pratt_parsing(index, 0);

            if self.tokens.get(*index).unwrap().kind == TokenType::CloseBrace {
                dict_items.push(DictItemType::DictUnpack(lhs));
                break;
            }

            if self.tokens.get(*index).unwrap().kind == TokenType::Comma {
                *index += 1;
                dict_items.push(DictItemType::DictUnpack(lhs));
                continue;
            }

            assert_eq!(
                self.tokens.get(*index).map(|token| &token.kind),
                Some(&TokenType::Colon),
                "Expecting a \":\"! at position: {}",
                // FIXME: Showing incorrect position
                lhs_span.end + 1
            );

            // Consume :
            *index += 1;
            let (rhs, rhs_span) = self.pratt_parsing(index, 0);
            last_expr_span = rhs_span;

            dict_items.push(DictItemType::KeyValue(lhs, rhs));
        }

        assert_eq!(
            self.tokens.get(*index).map(|token| &token.kind),
            Some(&TokenType::CloseBrace),
            "Expecting a \"}}\"! at position: {}",
            // FIXME: Showing incorrect position
            last_expr_span.end + 1
        );

        dict_span.end = self.tokens.get(*index).map(|token| token.span.end).unwrap();

        // Consume }
        *index += 1;

        (Expression::Dict(dict_items, dict_span), dict_span)
    }

    fn parse_list_expr(&self, index: &mut usize) -> (Expression, Span) {
        // Consume [
        *index += 1;

        if self.tokens.get(*index).unwrap().kind == TokenType::Operator(OperatorType::Exponent) {
            panic!("Invalid Syntax: can't unpack dictionary inside list!")
        }

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

            // allow trailing comma
            if self.tokens.get(*index).unwrap().kind == TokenType::CloseBrackets {
                break;
            }

            if self.tokens.get(*index).unwrap().kind == TokenType::Operator(OperatorType::Exponent) {
                panic!("Invalid Syntax: can't unpack dictionary inside list!")
            }

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

    /// This function assumes that `lhs` is already parsed and the "if" Token consumed.
    fn parse_if_else_expr(&self, index: &mut usize, lhs: Expression, lhs_span: Span) -> Expression {
        let (condition, _) = self.pratt_parsing(index, 0);
        assert_eq!(
            self.tokens.get(*index).map(|token| &token.kind),
            Some(&TokenType::Keyword(KeywordType::Else)),
            "Expecting \"else\" keyword!"
        );

        // Consume "else" keyword
        *index += 1;
        let (rhs, rhs_span) = self.pratt_parsing(index, 0);

        Expression::IfElse(IfElseExpr {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            condition: Box::new(condition),
            span: Span {
                start: lhs_span.start,
                end: rhs_span.end,
            },
        })
    }

    fn parse_function_parameters(&self, index: &mut usize) -> Vec<FuncParameter> {
        let mut parameters = vec![];

        loop {
            let mut func_parameter = FuncParameter::default();
            let token = self.tokens.get(*index).unwrap();
            match &token.kind {
                TokenType::Id(name) => {
                    func_parameter.name = name.to_string();
                    func_parameter.span = token.span;

                    // Consume Id
                    *index += 1;
                    // TODO: Maybe use the assignment parse function here
                    if self.tokens.get(*index).unwrap().kind == TokenType::Operator(OperatorType::Assign) {
                        *index += 1;
                        // TODO: use parse_expression instead
                        let (expr, expr_span) = self.pratt_parsing(index, 0);
                        func_parameter.default_value = Some(expr);
                        func_parameter.span.end = expr_span.end;
                    }

                    parameters.push(func_parameter);
                }
                TokenType::Operator(OperatorType::Asterisk) => {
                    // Consume *
                    *index += 1;
                    let next_token = self.tokens.get(*index).unwrap();
                    match &next_token.kind {
                        TokenType::Id(name) => {
                            // Consume Id
                            *index += 1;
                            func_parameter.name = name.to_string();
                            func_parameter.star_parameter_type = Some(StarParameterType::Kargs);
                            func_parameter.span = next_token.span;

                            parameters.push(func_parameter);
                        }
                        _ => panic!("Invalid Syntax: expecting identifier, got {next_token:?}"),
                    }
                }
                TokenType::Operator(OperatorType::Exponent) => {
                    // Consume **
                    *index += 1;
                    let next_token = self.tokens.get(*index).unwrap();
                    match &next_token.kind {
                        TokenType::Id(name) => {
                            // Consume Id
                            *index += 1;
                            func_parameter.name = name.to_string();
                            func_parameter.star_parameter_type = Some(StarParameterType::KWargs);
                            func_parameter.span = next_token.span;

                            parameters.push(func_parameter);
                        }
                        _ => panic!("Invalid Syntax: expecting identifier, got {next_token:?}"),
                    }
                }
                _ => panic!("Invalid syntax! {:?}", token),
            }

            if self.tokens.get(*index).unwrap().kind != TokenType::Comma {
                break;
            }

            // Consume ,
            *index += 1;
        }

        parameters
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
            TokenType::Operator(OperatorType::ColonEqual) => Operation::Binary(BinaryOperator::Walrus),
            TokenType::Dot => Operation::Binary(BinaryOperator::AttributeRef),
            TokenType::Keyword(KeywordType::And) => Operation::Binary(BinaryOperator::LogicalAnd),
            TokenType::Keyword(KeywordType::Or) => Operation::Binary(BinaryOperator::LogicalOr),
            TokenType::Keyword(KeywordType::In) => Operation::Binary(BinaryOperator::In),
            TokenType::Keyword(KeywordType::If) => Operation::Binary(BinaryOperator::IfElse),
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
            TokenType::Keyword(KeywordType::Await) => Operation::Unary(UnaryOperator::Await),
            TokenType::OpenParenthesis => Operation::Unary(UnaryOperator::OpenParenthesis),
            TokenType::OpenBrackets => Operation::Unary(UnaryOperator::OpenBrackets),
            _ => panic!("ERROR: Unexpected token! {token:?}"),
        }
    }

    fn parse_class(&self, index: &mut usize) -> ClassStmt {
        let mut class = ClassStmt::default();

        let mut token = self.tokens.get(*index).unwrap();
        match &token.kind {
            TokenType::Id(name) => {
                *index += 1;
                class.name = name.to_string();
            }
            _ => panic!("Syntax Error: expected identifier, got {token:?}"),
        }

        token = self.tokens.get(*index).unwrap();

        if token.kind == TokenType::OpenParenthesis {
            // Consume (
            *index += 1;
            class.super_classes = self.parse_function_parameters(index);

            assert_eq!(
                self.tokens.get(*index).map(|token| &token.kind),
                Some(&TokenType::CloseParenthesis),
                "Expecting a \")\"!",
            );
            // Consume )
            *index += 1;
        }

        // Consume :
        *index += 1;

        class.block = self.parse_block(index);
        class.span.end = class.block.span.end;

        class
    }

    fn parse_import(&self, index: &mut usize) -> ImportStmt {
        let mut import_stmt = ImportStmt::default();

        loop {
            let (name, span_end) = self.parse_import_module_name(index);
            let mut import_module = ImportModule { name, alias: None };
            import_stmt.span.end = span_end;

            if self.tokens.get(*index).unwrap().kind == TokenType::Keyword(KeywordType::As) {
                // Consume "as"
                *index += 1;
                import_module.alias = Some(match self.tokens.get(*index).unwrap() {
                    Token {
                        kind: TokenType::Id(alias_name),
                        span,
                    } => {
                        // Consume Id
                        *index += 1;
                        import_stmt.span.end = span.end;

                        alias_name.to_string()
                    }
                    token => panic!("Syntax Error: Expected identifier, got {token:?}"),
                });
            }

            import_stmt.modules.push(import_module);

            if self.tokens.get(*index).unwrap().kind != TokenType::Comma {
                break;
            }

            // Consume ,
            *index += 1;
        }

        import_stmt
    }

    fn parse_from_import(&self, index: &mut usize) -> FromImportStmt {
        let mut from_import_stmt = FromImportStmt::default();
        let mut token = self.tokens.get(*index).unwrap();

        // Get the relative part
        let mut has_dots = false;
        if let TokenType::Dot | TokenType::Ellipsis = token.kind {
            has_dots = true;
            let mut dots = vec![];

            while let Token {
                kind: TokenType::Dot | TokenType::Ellipsis,
                ..
            } = token
            {
                if token.kind == TokenType::Dot {
                    dots.push(".".to_string());
                } else {
                    dots.extend_from_slice(&[".".to_string(), ".".to_string(), ".".to_string()]);
                }

                *index += 1;
                token = self.tokens.get(*index).unwrap();
            }

            from_import_stmt.module.push(ImportModule {
                name: dots,
                alias: None,
            });
        }

        token = self.tokens.get(*index).unwrap();
        if let TokenType::Id(..) = token.kind {
            let (name, _) = self.parse_import_module_name(index);
            from_import_stmt.module.push(ImportModule { name, alias: None });
        } else if !has_dots {
            panic!("Syntax Error: expecting identifier, got {token:?}");
        }

        token = self.tokens.get(*index).unwrap();
        assert_eq!(
            token.kind,
            TokenType::Keyword(KeywordType::Import),
            "Syntax Error: expecting \"import\" keyword, got {token:?}"
        );
        // Consume "import"
        *index += 1;
        token = self.tokens.get(*index).unwrap();

        if token.kind == TokenType::Operator(OperatorType::Asterisk) {
            // Consume *
            *index += 1;
            from_import_stmt.targets.push(ImportModule {
                name: vec!["*".to_string()],
                alias: None,
            });
            from_import_stmt.span.end = token.span.end;

            return from_import_stmt;
        }

        let mut expect_close_paren = false;
        if token.kind == TokenType::OpenParenthesis {
            // Consume (
            *index += 1;
            expect_close_paren = true;
        }

        loop {
            token = self.tokens.get(*index).unwrap();
            let mut target = ImportModule::default();
            if let TokenType::Id(ref target_name) = token.kind {
                // Consume Id
                *index += 1;

                from_import_stmt.span.end = token.span.end;
                target.name = vec![target_name.to_string()];
            } else {
                panic!("Syntax Error: Expected identifier, got {token:?}")
            }

            if self.tokens.get(*index).unwrap().kind == TokenType::Keyword(KeywordType::As) {
                // Consume "as"
                *index += 1;
                target.alias = Some(match self.tokens.get(*index).unwrap() {
                    Token {
                        kind: TokenType::Id(alias_name),
                        span,
                    } => {
                        // Consume Id
                        *index += 1;
                        from_import_stmt.span.end = span.end;

                        alias_name.to_string()
                    }
                    token => panic!("Syntax Error: Expected identifier, got {token:?}"),
                });
            }

            from_import_stmt.targets.push(target);

            if self.tokens.get(*index).unwrap().kind != TokenType::Comma {
                break;
            }

            // Consume ,
            *index += 1;
        }

        token = self.tokens.get(*index).unwrap();
        if expect_close_paren {
            assert_eq!(
                token.kind,
                TokenType::CloseParenthesis,
                "Syntax Error: expecting \")\", got {token:?}"
            );
            // Consume )
            *index += 1;
        }

        from_import_stmt
    }

    fn parse_import_module_name(&self, index: &mut usize) -> (Vec<String>, usize) {
        let mut module_name = vec![];
        let mut span_end = 0;

        loop {
            let token = self.tokens.get(*index).unwrap();
            if let TokenType::Id(ref target) = token.kind {
                // Consume Id
                *index += 1;

                span_end = token.span.end;
                module_name.push(target.to_string());
            } else {
                panic!("Syntax Error: Expected identifier, got {token:?}")
            }

            if self.tokens.get(*index).unwrap().kind != TokenType::Dot {
                break;
            }

            // Consume .
            *index += 1;
        }

        (module_name, span_end)
    }
}
