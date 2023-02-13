pub mod ast;
mod helpers;

use crate::{
    error::{PythonError, PythonErrorType},
    lexer::{
        token::{
            types::{KeywordType, OperatorType, TokenType},
            Span, Token,
        },
        Lexer,
    },
    parser::ast::{DictItemType, ExceptBlock, ExceptBlockKind, FinallyBlock, IfElseExpr},
};
use ast::{
    BinaryOperator, Block, ElIfStmt, ElseStmt, Expression, Function, IfStmt, Operation, ParsedFile, Statement,
    UnaryOperator, VarAsgmt, While,
};
use helpers::{infix_binding_power, postfix_binding_power, prefix_binding_power};

use self::{
    ast::{
        ClassStmt, ForStmt, FromImportStmt, FuncParameter, ImportModule, ImportStmt, LambdaExpr, ReturnStmt,
        StarParameterType, TryStmt, WithItem, WithStmt,
    },
    helpers::AllowedExpr,
};

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

    pub fn parse(&self) -> (ParsedFile, Option<Vec<PythonError>>) {
        let mut parsed_file = ParsedFile::new();
        let mut index = 0;
        let mut parse_errors: Vec<PythonError> = vec![];

        while index < self.tokens.len() {
            if self
                .tokens
                .get(index)
                .map_or(false, |token| token.kind == TokenType::Eof)
            {
                break;
            }
            let (stmt, _, errors) = self.parse_statements(&mut index);
            parsed_file.stmts.push(stmt);
            if let Some(stmt_errors) = errors {
                parse_errors.extend(stmt_errors);
            }
        }

        if parse_errors.is_empty() {
            (parsed_file, None)
        } else {
            (parsed_file, Some(parse_errors))
        }
    }

    fn parse_expression(
        &self,
        index: &mut usize,
        allowed_expr: AllowedExpr,
    ) -> (Expression, Span, Option<Vec<PythonError>>) {
        let (expr, expr_span, expr_errors) = self.pratt_parsing(index, 0, allowed_expr);

        // TODO: Try to refactor this later
        if self
            .tokens
            .get(*index)
            .map_or(false, |token| token.kind == TokenType::Comma)
            && allowed_expr.contains(AllowedExpr::TUPLE)
        {
            // consume ,
            *index += 1;

            let (tuple_expr, tuple_span, mut tuple_errors) =
                self.parse_tuple_expr_with_no_parens(index, expr_span.start, allowed_expr);

            let mut items = vec![expr];

            if let Expression::Tuple(tuple_items, _) = tuple_expr {
                items.extend(tuple_items);
            }

            tuple_errors = match (expr_errors, tuple_errors) {
                (None, None) => None,
                (None, Some(tuple_errors)) => Some(tuple_errors),
                (Some(expr_errors), None) => Some(expr_errors),
                (Some(expr_errors), Some(mut tuple_errors)) => {
                    tuple_errors.extend(expr_errors);
                    Some(tuple_errors)
                }
            };

            return (Expression::Tuple(items, tuple_span), tuple_span, tuple_errors);
        }

        if self.tokens.get(*index).map_or(false, |token| {
            matches!(&token.kind, TokenType::NewLine | TokenType::SemiColon)
        }) {
            *index += 1;
        }

        (expr, expr_span, expr_errors)
    }

    /// Parse `expressions` using Pratt Parsing algorithm
    fn pratt_parsing(
        &self,
        index: &mut usize,
        min_precedence_weight: u8,
        allowed_expr: AllowedExpr,
    ) -> (Expression, Span, Option<Vec<PythonError>>) {
        let mut errors: Vec<PythonError> = Vec::new();
        let mut token = self.tokens.get(*index).unwrap();
        let (mut lhs, mut lhs_span, lhs_errors) = match &token.kind {
            TokenType::Id(name) if allowed_expr.contains(AllowedExpr::ID) => {
                *index += 1;
                (Expression::Id(name.to_string(), token.span), token.span, None)
            }
            TokenType::String(str) if allowed_expr.contains(AllowedExpr::STRING) => {
                *index += 1;
                (Expression::String(str.to_string(), token.span), token.span, None)
            }
            TokenType::Number(_, num) if allowed_expr.contains(AllowedExpr::NUMBER) => {
                *index += 1;
                (Expression::Number(num.to_string(), token.span), token.span, None)
            }
            TokenType::Keyword(KeywordType::True) if allowed_expr.contains(AllowedExpr::BOOL) => {
                *index += 1;
                (Expression::Bool(true, token.span), token.span, None)
            }
            TokenType::Keyword(KeywordType::False) if allowed_expr.contains(AllowedExpr::BOOL) => {
                *index += 1;
                (Expression::Bool(false, token.span), token.span, None)
            }
            TokenType::Ellipsis if allowed_expr.contains(AllowedExpr::ELLIPSIS) => {
                *index += 1;
                (Expression::Ellipsis(token.span), token.span, None)
            }
            TokenType::Keyword(KeywordType::None) if allowed_expr.contains(AllowedExpr::NONE) => {
                *index += 1;
                (Expression::None(token.span), token.span, None)
            }
            TokenType::Keyword(KeywordType::Yield) if allowed_expr.contains(AllowedExpr::YIELD) => {
                return self.parse_yield(index, token);
            }
            TokenType::Operator(
                OperatorType::Plus
                | OperatorType::Minus
                | OperatorType::BitwiseNot
                | OperatorType::Asterisk
                | OperatorType::Exponent,
            )
            | TokenType::Keyword(KeywordType::Not | KeywordType::Await | KeywordType::Lambda)
                if allowed_expr.contains(AllowedExpr::UNARY_OP) =>
            {
                self.parse_unary_operator(index, token)
            }
            TokenType::OpenParenthesis if allowed_expr.contains(AllowedExpr::PARENTHESIZED) => {
                self.parse_parenthesized_expr(index, token)
            }
            TokenType::OpenBrackets if allowed_expr.contains(AllowedExpr::LIST) => {
                self.parse_list_expr(index, token.span.start)
            }
            TokenType::OpenBrace if allowed_expr.contains(AllowedExpr::SET) => self.parse_bracesized_expr(index, token),
            _ => {
                *index += 1;

                (
                    Expression::Invalid(token.span),
                    token.span,
                    Some(vec![PythonError {
                        error: PythonErrorType::Syntax,
                        msg: format!("SyntaxError: unexpected token {:?}", token.kind),
                        span: token.span,
                    }]),
                )
            }
        };

        if let Some(lhs_errors) = lhs_errors {
            errors.extend(lhs_errors);
        }

        if !(allowed_expr.contains(AllowedExpr::BINARY_OP) && allowed_expr.contains(AllowedExpr::UNARY_OP)) {
            return (lhs, lhs_span, if errors.is_empty() { None } else { Some(errors) });
        }

        while self
            .tokens
            .get(*index)
            .map_or(false, |token| !helpers::is_token_end_of_expr(token))
        {
            token = self.tokens.get(*index).unwrap();

            let op = match self.get_expr_operation(token, index) {
                Ok(op) => op,
                Err(error) => {
                    errors.push(error);
                    *index += 1;
                    continue;
                }
            };

            if allowed_expr.contains(AllowedExpr::UNARY_OP) {
                if let Some((lhs_bp, ())) = postfix_binding_power(op) {
                    if lhs_bp < min_precedence_weight {
                        break;
                    }

                    *index += 1;

                    lhs = match op {
                        Operation::Unary(UnaryOperator::OpenParenthesis) => {
                            // FIXME: Only parsing function calls with no arguments
                            let expr = Expression::Call(Box::new(lhs), lhs_span);
                            if self
                                .tokens
                                .get(*index)
                                .map_or(false, |token| token.kind != TokenType::CloseParenthesis)
                            {
                                errors.push(PythonError {
                                    error: PythonErrorType::Syntax,
                                    msg: format!("SyntaxError: expecting a ')' at position: {}", lhs_span.end + 1),
                                    span: Span {
                                        start: lhs_span.end,
                                        end: lhs_span.end + 1,
                                    },
                                })
                            } else {
                                *index += 1;
                            }

                            expr
                        }
                        Operation::Unary(UnaryOperator::OpenBrackets) => {
                            // FIXME: Only parsing slice with no start, stop or step attributes
                            let (rhs, rhs_span, rhs_error) = self.pratt_parsing(index, 0, allowed_expr);

                            if let Some(rhs_error) = rhs_error {
                                errors.extend(rhs_error);
                            }

                            if self
                                .tokens
                                .get(*index)
                                .map_or(false, |token| token.kind != TokenType::CloseBrackets)
                            {
                                errors.push(PythonError {
                                    error: PythonErrorType::Syntax,
                                    msg: format!("SyntaxError: expecting a ']' at position: {}", rhs_span.end + 1),
                                    span: Span {
                                        start: rhs_span.end,
                                        end: rhs_span.end + 1,
                                    },
                                })
                            } else {
                                *index += 1;
                            }

                            Expression::Slice(
                                Box::new(lhs),
                                Box::new(rhs),
                                Span {
                                    start: lhs_span.start,
                                    end: rhs_span.end + 1,
                                },
                            )
                        }
                        _ => {
                            *index += 1;
                            errors.push(PythonError {
                                error: PythonErrorType::Syntax,
                                msg: format!("SyntaxError: invalid postfix operator! {:?}", op),
                                span: token.span,
                            });
                            Expression::Invalid(token.span)
                        }
                    };

                    continue;
                }
            }

            if allowed_expr.contains(AllowedExpr::BINARY_OP) {
                if let Some((lhs_bp, rhs_bp)) = infix_binding_power(op) {
                    if lhs_bp < min_precedence_weight {
                        break;
                    }

                    *index += 1;

                    if op == Operation::Binary(BinaryOperator::IfElse) {
                        let (expr, expr_errors) = self.parse_if_else_expr(index, lhs, lhs_span);
                        lhs = expr;

                        if let Some(expr_errors) = expr_errors {
                            errors.extend(expr_errors);
                        }

                        continue;
                    }

                    let next_token = self.tokens.get(*index).unwrap();
                    // This wont work if the rhs is another expression
                    if !helpers::is_token_start_of_expr(next_token) {
                        errors.push(PythonError {
                            error: PythonErrorType::Syntax,
                            msg: format!("SyntaxError: missing rhs in {:?} operation", op),
                            span: Span {
                                start: lhs_span.end,
                                end: next_token.span.end,
                            },
                        })
                    } else {
                        let (rhs, rhs_span, rhs_error) = self.pratt_parsing(index, rhs_bp, allowed_expr);
                        if let Some(rhs_error) = rhs_error {
                            errors.extend(rhs_error);
                        }
                        lhs_span = Span {
                            start: lhs_span.start,
                            end: rhs_span.end,
                        };
                        lhs = Expression::BinaryOp(Box::new(lhs), op.get_binary_op(), Box::new(rhs), lhs_span);
                    }

                    continue;
                }
            }
        }

        (lhs, lhs_span, if errors.is_empty() { None } else { Some(errors) })
    }

    fn parse_function(
        &self,
        index: &mut usize,
        func_span_start: usize,
        name: String,
        name_span: Span,
    ) -> (Function, Option<Vec<PythonError>>) {
        let mut errors = vec![];
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

        // Consume function identifier keyword
        *index += 1;
        // self.expect_next_token_or_error(index, TokenType::OpenParenthesis, "SyntaxError: expecting '(' got {:?}");
        let mut token = self.tokens.get(*index).unwrap();
        if token.kind != TokenType::OpenParenthesis {
            errors.push(PythonError {
                error: PythonErrorType::Syntax,
                msg: format!("SyntaxError: expecting '(' got {:?}", token.kind),
                span: token.span,
            });
        } else {
            *index += 1;
        }

        // check if we have arguments in the function/method
        token = self.tokens.get(*index).unwrap();
        if token.kind != TokenType::CloseParenthesis {
            let (parameters, parameters_errors) = self.parse_function_parameters(index);
            function.parameters = parameters;
            token = self.tokens.get(*index).unwrap();

            if let Some(parameters_errors) = parameters_errors {
                errors.extend(parameters_errors);
            }
        }

        if !matches!(token.kind, TokenType::CloseParenthesis) {
            errors.push(PythonError {
                error: PythonErrorType::Syntax,
                msg: format!("SyntaxError: expecting ')' got {:?}", token.kind),
                span: token.span,
            });
        }
        *index += 1;
        token = self.tokens.get(*index).unwrap();
        if !matches!(token.kind, TokenType::Colon) {
            errors.push(PythonError {
                error: PythonErrorType::Syntax,
                msg: format!("SyntaxError: expecting ':' got {:?}", token.kind),
                span: token.span,
            });
        } else {
            *index += 1;
        }
        let (block, block_errors) = self.parse_block(index);

        if let Some(block_errors) = block_errors {
            errors.extend(block_errors);
        }

        function.span.end = block.span.end;
        function.block = block;

        (function, if errors.is_empty() { None } else { Some(errors) })
    }

    // TODO: add suport for simple stmts
    fn parse_block(&self, index: &mut usize) -> (Block, Option<Vec<PythonError>>) {
        let mut errors = Vec::new();

        let mut token = self.tokens.get(*index).unwrap();
        if !matches!(token.kind, TokenType::NewLine) {
            errors.push(PythonError {
                error: PythonErrorType::Indentation,
                msg: format!("SyntaxError: expecting 'NEWLINE' got {:?}", token.kind),
                span: token.span,
            });
        }
        *index += 1;
        token = self.tokens.get(*index).unwrap();
        if !matches!(token.kind, TokenType::Indent) {
            errors.push(PythonError {
                error: PythonErrorType::Indentation,
                msg: format!("Expected an indented after function definition"),
                span: token.span,
            });
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

            let (statement, stmt_span, stmt_errors) = self.parse_statements(index);
            block.span.end = stmt_span.end;
            block.stmts.push(statement);

            if let Some(stmt_errors) = stmt_errors {
                errors.extend(stmt_errors);
            }
        }

        (block, if errors.is_empty() { None } else { Some(errors) })
    }

    fn parse_statements(&self, index: &mut usize) -> (Statement, Span, Option<Vec<PythonError>>) {
        let token = self.tokens.get(*index).unwrap();
        *index += 1;

        match &token.kind {
            TokenType::Id(name) => {
                let next_token = self.tokens.get(*index).unwrap();
                if next_token.kind == TokenType::Operator(OperatorType::Assign) {
                    *index += 1;
                    let (expr, expr_span, expr_error) = self.parse_expression(index, AllowedExpr::ALL);

                    let assign_span = Span {
                        start: token.span.start,
                        end: expr_span.end,
                    };
                    let assign = VarAsgmt::new(name.clone(), assign_span);

                    (Statement::VarAsgmt(assign, expr), assign_span, expr_error)
                } else if next_token.kind == TokenType::Operator(OperatorType::ColonEqual) {
                    *index += 1;
                    (
                        Statement::Expression(Expression::Invalid(next_token.span), next_token.span),
                        next_token.span,
                        Some(vec![PythonError {
                            error: PythonErrorType::Syntax,
                            msg: format!("SyntaxError: invalid assignment statement!"),
                            span: next_token.span,
                        }]),
                    )
                } else {
                    *index -= 1;
                    let (expr, expr_span, expr_error) = self.parse_expression(index, AllowedExpr::ALL);
                    (Statement::Expression(expr, expr_span), expr_span, expr_error)
                }
            }
            TokenType::Keyword(KeywordType::Def) => {
                // TODO: move this check into `parse_function`
                let next_token = self.tokens.get(*index).unwrap();
                if let Token {
                    kind: TokenType::Id(name),
                    span: func_name_span,
                } = next_token
                {
                    // *index += 1;
                    let (mut func, func_errors) =
                        self.parse_function(index, token.span.start, name.to_string(), *func_name_span);
                    func.span.start = token.span.start;
                    let func_span = func.span;

                    if func_errors.is_some() {
                        (Statement::FunctionDef(func), func_span, func_errors)
                    } else {
                        (Statement::FunctionDef(func), func_span, None)
                    }
                } else {
                    (
                        Statement::Invalid(next_token.span),
                        next_token.span,
                        Some(vec![PythonError {
                            error: PythonErrorType::Syntax,
                            msg: format!("SyntaxError: invalid function name"),
                            span: next_token.span,
                        }]),
                    )
                }
            }
            TokenType::Keyword(KeywordType::If) => {
                let (mut if_stmt, if_stmt_errors) = self.parse_if(index);
                if_stmt.span.start = token.span.start;
                let if_span = if_stmt.span;

                (Statement::If(if_stmt), if_span, if_stmt_errors)
            }
            TokenType::Keyword(KeywordType::While) => {
                let (mut while_stmt, while_stmt_errors) = self.parse_while(index);
                while_stmt.span.start = token.span.start;
                let while_span = while_stmt.span;

                (Statement::While(while_stmt), while_span, while_stmt_errors)
            }
            TokenType::Keyword(KeywordType::Class) => {
                let (mut class_stmt, class_stmt_errors) = self.parse_class(index);
                class_stmt.span.start = token.span.start;
                let class_span = class_stmt.span;

                (Statement::Class(class_stmt), class_span, class_stmt_errors)
            }
            TokenType::Keyword(KeywordType::Import) => {
                let (mut import_stmt, import_stmt_errors) = self.parse_import(index);
                import_stmt.span.start = token.span.start;
                let import_span = import_stmt.span;

                (Statement::Import(import_stmt), import_span, import_stmt_errors)
            }
            TokenType::Keyword(KeywordType::From) => {
                let (mut from_import_stmt, from_import_errors) = self.parse_from_import(index);
                from_import_stmt.span.start = token.span.start;
                let from_import_span = from_import_stmt.span;

                (
                    Statement::FromImport(from_import_stmt),
                    from_import_span,
                    from_import_errors,
                )
            }
            TokenType::Keyword(KeywordType::With) => {
                let (mut with_stmt, with_stmt_errors) = self.parse_with(index);
                with_stmt.span.start = token.span.start;
                let with_span = with_stmt.span;

                (Statement::With(with_stmt), with_span, with_stmt_errors)
            }
            TokenType::Keyword(KeywordType::Try) => {
                let (mut try_stmt, try_stmt_errors) = self.parse_try(index);
                try_stmt.span.start = token.span.start;
                let try_span = try_stmt.span;

                (Statement::Try(try_stmt), try_span, try_stmt_errors)
            }
            TokenType::Keyword(KeywordType::Return) => {
                let (mut return_stmt, return_stmt_errors) = self.parse_return(index);
                return_stmt.span.start = token.span.start;
                let return_span = return_stmt.span;

                (Statement::Return(return_stmt), return_span, return_stmt_errors)
            }
            TokenType::Keyword(KeywordType::For) => {
                let (mut for_stmt, for_stmt_errors) = self.parse_for_stmt(index);
                for_stmt.span.start = token.span.start;
                let for_span = for_stmt.span;

                (Statement::For(for_stmt), for_span, for_stmt_errors)
            }
            TokenType::Keyword(KeywordType::Pass) => (Statement::Pass(token.span), token.span, None),
            TokenType::Keyword(KeywordType::Continue) => (Statement::Continue(token.span), token.span, None),
            TokenType::Keyword(KeywordType::Break) => (Statement::Break(token.span), token.span, None),
            _ => {
                *index -= 1;
                let (expr, expr_span, expr_errors) = self.parse_expression(index, AllowedExpr::ALL);
                (Statement::Expression(expr, expr_span), expr_span, expr_errors)
            }
        }
    }

    fn parse_if(&self, index: &mut usize) -> (IfStmt, Option<Vec<PythonError>>) {
        let mut errors = Vec::new();
        let (condition_expr, _, condition_expr_errors) = self.parse_expression(index, AllowedExpr::ALL);

        if let Some(condition_expr_errors) = condition_expr_errors {
            errors.extend(condition_expr_errors);
        }

        let mut token = self.tokens.get(*index).unwrap();
        if token.kind != TokenType::Colon {
            errors.push(PythonError {
                error: PythonErrorType::Syntax,
                msg: format!("SyntaxError: expecting ':' got {:?}", token.kind),
                span: token.span,
            });
        } else {
            *index += 1;
        }

        let (block, block_errors) = self.parse_block(index);

        if let Some(block_errors) = block_errors {
            errors.extend(block_errors);
        }

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
                let (condition_expr, _, condition_expr_errors) = self.parse_expression(index, AllowedExpr::ALL);
                if let Some(condition_expr_errors) = condition_expr_errors {
                    errors.extend(condition_expr_errors);
                }

                let elif_start = token.span.start;
                token = self.tokens.get(*index).unwrap();
                if !matches!(token.kind, TokenType::Colon) {
                    errors.push(PythonError {
                        error: PythonErrorType::Syntax,
                        msg: format!("SyntaxError: expecting ':' got {:?}", token.kind),
                        span: token.span,
                    });
                } else {
                    *index += 1;
                }

                let (elif_block, elif_block_errors) = self.parse_block(index);

                if let Some(elif_block_errors) = elif_block_errors {
                    errors.extend(elif_block_errors);
                }

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
                errors.push(PythonError {
                    error: PythonErrorType::Syntax,
                    msg: format!("SyntaxError: expecting ':' got {:?}", token.kind),
                    span: token.span,
                });
            } else {
                *index += 1;
            }
            let (else_block, else_block_errors) = self.parse_block(index);

            if let Some(else_block_errors) = else_block_errors {
                errors.extend(else_block_errors);
            }

            if_stmt.span.end = else_block.span.end;
            if_stmt.else_stmt = Some(ElseStmt {
                span: Span {
                    start: else_start,
                    end: else_block.span.end,
                },
                block: else_block,
            });
        }

        (if_stmt, if errors.is_empty() { None } else { Some(errors) })
    }

    fn parse_while(&self, index: &mut usize) -> (While, Option<Vec<PythonError>>) {
        let mut errors = Vec::new();
        let (condition_expr, _, condition_expr_errors) = self.parse_expression(index, AllowedExpr::ALL);

        if let Some(condition_expr_errors) = condition_expr_errors {
            errors.extend(condition_expr_errors);
        }

        let mut token = self.tokens.get(*index).unwrap();
        if !matches!(token.kind, TokenType::Colon) {
            errors.push(PythonError {
                error: PythonErrorType::Syntax,
                msg: format!("SyntaxError: expecting ':' got {:?}", token.kind),
                span: token.span,
            });
        } else {
            *index += 1;
        }

        let (while_block, while_block_errors) = self.parse_block(index);
        if let Some(while_block_errors) = while_block_errors {
            errors.extend(while_block_errors);
        }

        let mut while_stmt = While {
            condition: condition_expr,
            else_stmt: None,
            span: Span {
                start: 0,
                end: while_block.span.end,
            },
            block: while_block,
        };

        token = self.tokens.get(*index).unwrap();
        if token.kind == TokenType::Keyword(KeywordType::Else) {
            *index += 1;
            let else_start = token.span.start;
            token = self.tokens.get(*index).unwrap();
            if !matches!(token.kind, TokenType::Colon) {
                errors.push(PythonError {
                    error: PythonErrorType::Syntax,
                    msg: format!("SyntaxError: expecting ':' got {:?}", token.kind),
                    span: token.span,
                });
            } else {
                *index += 1;
            }
            let (else_block, else_block_errors) = self.parse_block(index);

            if let Some(else_block_errors) = else_block_errors {
                errors.extend(else_block_errors);
            }

            while_stmt.span.end = else_block.span.end;
            while_stmt.else_stmt = Some(ElseStmt {
                span: Span {
                    start: else_start,
                    end: else_block.span.end,
                },
                block: else_block,
            });
        }

        (while_stmt, if errors.is_empty() { None } else { Some(errors) })
    }

    fn parse_unary_operator(&self, index: &mut usize, token: &Token) -> (Expression, Span, Option<Vec<PythonError>>) {
        let mut errors = Vec::new();

        let op = match token.kind {
            TokenType::Operator(OperatorType::BitwiseNot) => Operation::Unary(UnaryOperator::BitwiseNot),
            TokenType::Operator(OperatorType::Plus) => Operation::Unary(UnaryOperator::Plus),
            TokenType::Operator(OperatorType::Minus) => Operation::Unary(UnaryOperator::Minus),
            TokenType::Operator(OperatorType::Asterisk) => Operation::Unary(UnaryOperator::UnpackIterable),
            TokenType::Operator(OperatorType::Exponent) => Operation::Unary(UnaryOperator::UnpackDictionary),
            TokenType::Keyword(KeywordType::Not) => Operation::Unary(UnaryOperator::LogicalNot),
            TokenType::Keyword(KeywordType::Await) => Operation::Unary(UnaryOperator::Await),
            TokenType::Keyword(KeywordType::Lambda) => Operation::Unary(UnaryOperator::Lambda),
            _ => {
                *index += 1;
                errors.push(PythonError {
                    error: PythonErrorType::Syntax,
                    msg: format!("SyntaxError: unexpected operator {:?}", token.kind),
                    span: token.span,
                });
                return (Expression::Invalid(token.span), token.span, Some(errors));
            }
        };
        *index += 1;

        let ((), r_bp) = prefix_binding_power(op).unwrap();

        if op == Operation::Unary(UnaryOperator::Lambda) {
            let (parameters, parameters_errors) = self.parse_function_parameters(index);
            if let Some(parameters_errors) = parameters_errors {
                errors.extend(parameters_errors);
            }
            // Consume :
            *index += 1;
            let (expr, expr_span, expr_errors) = self.pratt_parsing(index, r_bp, AllowedExpr::ALL);
            let lambda_span = Span {
                start: token.span.start,
                end: expr_span.end,
            };

            if let Some(expr_errors) = expr_errors {
                errors.extend(expr_errors);
            }

            return (
                Expression::Lambda(LambdaExpr {
                    parameters,
                    expression: Box::new(expr),
                    span: lambda_span,
                }),
                lambda_span,
                if errors.is_empty() { None } else { Some(errors) },
            );
        }

        let (rhs, mut rhs_span, rhs_errors) = self.pratt_parsing(index, r_bp, AllowedExpr::ALL);
        rhs_span.start = token.span.start;

        if let Some(rhs_errors) = rhs_errors {
            errors.extend(rhs_errors);
        }

        (
            Expression::UnaryOp(Box::new(rhs), op.get_unary_op(), rhs_span),
            rhs_span,
            if errors.is_empty() { None } else { Some(errors) },
        )
    }

    fn parse_parenthesized_expr(
        &self,
        index: &mut usize,
        token: &Token,
    ) -> (Expression, Span, Option<Vec<PythonError>>) {
        let mut errors = Vec::new();

        // Consume (
        *index += 1;
        let paren_span_start = token.span.start;
        let next_token = self.tokens.get(*index).unwrap();

        if next_token.kind == TokenType::Operator(OperatorType::Asterisk) {
            errors.push(PythonError {
                error: PythonErrorType::Syntax,
                msg: String::from("SyntaxError: cannot use starred expression inside parenthesis!"),
                span: next_token.span,
            });
        } else if next_token.kind == TokenType::Operator(OperatorType::Exponent) {
            errors.push(PythonError {
                error: PythonErrorType::Syntax,
                msg: String::from("SyntaxError: cannot use double starred expression inside parenthesis!"),
                span: next_token.span,
            });
        }

        let (expr, expr_span, expr_errors) = if self
            .tokens
            .iter()
            .skip(*index)
            .find(|&token| token.kind == TokenType::Comma)
            .is_some()
        {
            self.parse_tuple_expr_with_no_parens(index, paren_span_start, AllowedExpr::ALL)
        } else {
            self.pratt_parsing(index, 0, AllowedExpr::ALL)
        };
        // FIXME: should be using this, but haven't found a way to set the right start of the tuple
        // yet.
        // let (expr, expr_span, expr_errors) = self.parse_expression(index);

        if let Some(expr_errors) = expr_errors {
            errors.extend(expr_errors);
        }

        let token = self.tokens.get(*index).unwrap();
        if token.kind != TokenType::CloseParenthesis {
            errors.push(PythonError {
                error: PythonErrorType::Syntax,
                msg: format!("SyntaxError: expecting ')' got {:?}", token.kind),
                span: token.span,
            });
        } else {
            // Consume )
            *index += 1;
        }

        (expr, expr_span, if errors.is_empty() { None } else { Some(errors) })
    }

    fn parse_tuple_expr_with_no_parens(
        &self,
        index: &mut usize,
        tuple_span_start: usize,
        allowed_expr: AllowedExpr,
    ) -> (Expression, Span, Option<Vec<PythonError>>) {
        let mut errors = Vec::new();
        let mut expressions = vec![];
        let mut tuple_span = Span {
            start: tuple_span_start,
            end: 0,
        };

        loop {
            let mut token = self.tokens.get(*index).unwrap();

            let expr_span = if helpers::is_token_start_of_expr(&token) {
                let (expr, expr_span, expr_errors) = self.pratt_parsing(index, 0, allowed_expr);
                if let Some(expr_errors) = expr_errors {
                    errors.extend(expr_errors);
                }
                expressions.push(expr);

                expr_span
            } else {
                break;
            };

            // allow tuples with trailing comma, e.g. "(1, 2, 3,)", "1, 2, 3,"
            token = self.tokens.get(*index).unwrap();
            if token.kind == TokenType::Comma {
                // Consume ,
                *index += 1;
            } else if !helpers::is_token_start_of_expr(&token) {
                break;
            } else {
                errors.push(PythonError {
                    error: PythonErrorType::Syntax,
                    msg: format!("SyntaxError: expected comma, got {:?}", token.kind),
                    span: Span {
                        start: expr_span.end,
                        end: expr_span.end + 1,
                    },
                });
            }
        }

        tuple_span.end = self.tokens.get(*index).map(|token| token.span.end).unwrap();

        (
            Expression::Tuple(expressions, tuple_span),
            tuple_span,
            if errors.is_empty() { None } else { Some(errors) },
        )
    }

    fn parse_bracesized_expr(&self, index: &mut usize, token: &Token) -> (Expression, Span, Option<Vec<PythonError>>) {
        let mut errors = Vec::new();
        // Consume {
        *index += 1;
        let brace_span_start = token.span.start;

        // If we see the "**" operator that means we are unpacking a dictionary, therefore, we
        // should parse as a dictionary instead of a set.
        if self.tokens.get(*index).unwrap().kind == TokenType::Operator(OperatorType::Exponent) {
            let (lhs, lhs_span, lhs_errors) = self.pratt_parsing(index, 0, AllowedExpr::ALL);

            if let Some(lhs_errors) = lhs_errors {
                errors.extend(lhs_errors);
            }

            let token = self.tokens.get(*index).unwrap();
            if token.kind != TokenType::Comma {
                errors.push(PythonError {
                    error: PythonErrorType::Syntax,
                    msg: format!("SyntaxError: expecting ',' got {:?}", token.kind),
                    span: token.span,
                });
            }

            return self.parse_dict_expression(index, DictItemType::DictUnpack(lhs), brace_span_start);
        }

        let (lhs, lhs_span, lhs_errors) = self.pratt_parsing(index, 0, AllowedExpr::ALL);

        if let Some(lhs_errors) = lhs_errors {
            errors.extend(lhs_errors);
        }

        if self
            .tokens
            .get(*index)
            .map_or(false, |token| token.kind == TokenType::Colon)
        {
            // Consume :
            *index += 1;
            let (rhs, _, rhs_errors) = self.pratt_parsing(index, 0, AllowedExpr::ALL);

            if let Some(rhs_errors) = rhs_errors {
                errors.extend(rhs_errors);
            }

            let token = self.tokens.get(*index).unwrap();
            if token.kind != TokenType::Comma {
                errors.push(PythonError {
                    error: PythonErrorType::Syntax,
                    msg: format!("SyntaxError: expecting ',' got {:?}", token.kind),
                    span: token.span,
                });
            }

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

            let (expr, expr_span, expr_errors) = self.pratt_parsing(index, 0, AllowedExpr::ALL);
            last_expr_span = expr_span;

            if let Some(expr_errors) = expr_errors {
                errors.extend(expr_errors);
            }

            expressions.push(expr);
        }

        let token = self.tokens.get(*index).unwrap();
        if token.kind != TokenType::CloseBrace {
            errors.push(PythonError {
                error: PythonErrorType::Syntax,
                msg: format!("SyntaxError: expecting '}}' got {:?}", token.kind),
                span: token.span,
            });
        }

        set_span.end = self.tokens.get(*index).map(|token| token.span.end).unwrap();
        // Consume }
        *index += 1;

        (
            Expression::Set(expressions, set_span),
            set_span,
            if errors.is_empty() { None } else { Some(errors) },
        )
    }

    fn parse_dict_expression(
        &self,
        index: &mut usize,
        lhs: DictItemType,
        brace_span_start: usize,
    ) -> (Expression, Span, Option<Vec<PythonError>>) {
        let mut errors = Vec::new();
        let mut dict_items = vec![lhs];
        let mut last_expr_span = Span { start: 0, end: 0 };
        let mut dict_span = Span {
            start: brace_span_start,
            end: 0,
        };

        while self.tokens.get(*index).unwrap().kind == TokenType::Comma {
            *index += 1;
            let mut token = self.tokens.get(*index).unwrap();
            if token.kind == TokenType::Operator(OperatorType::Asterisk) {
                errors.push(PythonError {
                    error: PythonErrorType::Syntax,
                    msg: "SyntaxError: can't unpack iterable inside dictionary!".to_string(),
                    span: token.span,
                });
            }

            if self.tokens.get(*index).unwrap().kind == TokenType::CloseBrace {
                break;
            }

            let (lhs, lhs_span, lhs_errors) = self.pratt_parsing(index, 0, AllowedExpr::ALL);

            if let Some(lhs_errors) = lhs_errors {
                errors.extend(lhs_errors);
            }

            if self.tokens.get(*index).unwrap().kind == TokenType::CloseBrace {
                dict_items.push(DictItemType::DictUnpack(lhs));
                break;
            }

            if self.tokens.get(*index).unwrap().kind == TokenType::Comma {
                *index += 1;
                dict_items.push(DictItemType::DictUnpack(lhs));
                continue;
            }

            token = self.tokens.get(*index).unwrap();
            if token.kind != TokenType::Colon {
                errors.push(PythonError {
                    error: PythonErrorType::Syntax,
                    msg: format!("SyntaxError: expecting ':' got {:?}", token.kind),
                    span: token.span,
                });
            }

            // Consume :
            *index += 1;

            let (rhs, rhs_span, rhs_errors) = self.pratt_parsing(index, 0, AllowedExpr::ALL);
            last_expr_span = rhs_span;

            if let Some(rhs_errors) = rhs_errors {
                errors.extend(rhs_errors);
            }

            dict_items.push(DictItemType::KeyValue(lhs, rhs));
        }

        let token = self.tokens.get(*index).unwrap();
        if token.kind != TokenType::CloseBrace {
            errors.push(PythonError {
                error: PythonErrorType::Syntax,
                msg: format!("SyntaxError: expecting '}}' got {:?}", token.kind),
                span: token.span,
            });
        }

        dict_span.end = self.tokens.get(*index).map(|token| token.span.end).unwrap();

        // Consume }
        *index += 1;

        (
            Expression::Dict(dict_items, dict_span),
            dict_span,
            if errors.is_empty() { None } else { Some(errors) },
        )
    }

    fn parse_list_expr(
        &self,
        index: &mut usize,
        list_expr_start: usize,
    ) -> (Expression, Span, Option<Vec<PythonError>>) {
        let mut errors = Vec::new();
        // Consume [
        *index += 1;

        let mut expressions = vec![];
        let mut list_span = Span {
            start: list_expr_start,
            end: 0,
        };

        loop {
            let token = self.tokens.get(*index).unwrap();

            // allow trailing comma
            if self.tokens.get(*index).unwrap().kind == TokenType::CloseBrackets {
                break;
            }

            if token.kind == TokenType::Operator(OperatorType::Exponent) {
                errors.push(PythonError {
                    error: PythonErrorType::Syntax,
                    msg: "SyntaxError: can't unpack dictionary inside list!".to_string(),
                    span: token.span,
                });
            }

            let (expr, _, expr_errors) = self.pratt_parsing(index, 0, AllowedExpr::ALL);

            if let Some(expr_errors) = expr_errors {
                errors.extend(expr_errors);
            }

            expressions.push(expr);

            if self.tokens.get(*index).unwrap().kind != TokenType::Comma {
                break;
            }

            // Consume ,
            *index += 1;
        }

        let token = self.tokens.get(*index).unwrap();
        if token.kind != TokenType::CloseBrackets {
            errors.push(PythonError {
                error: PythonErrorType::Syntax,
                msg: format!("SyntaxError: expecting ']' got {:?}", token.kind),
                span: token.span,
            });
        }

        list_span.end = self.tokens.get(*index).map(|token| token.span.end).unwrap();

        // Consume ]
        *index += 1;

        (
            Expression::List(expressions, list_span),
            list_span,
            if errors.is_empty() { None } else { Some(errors) },
        )
    }

    /// This function assumes that `lhs` is already parsed and the "if" Token consumed.
    fn parse_if_else_expr(
        &self,
        index: &mut usize,
        lhs: Expression,
        lhs_span: Span,
    ) -> (Expression, Option<Vec<PythonError>>) {
        let mut errors = Vec::new();
        let (condition, _, condition_errors) = self.pratt_parsing(index, 0, AllowedExpr::ALL);

        if let Some(condition_errors) = condition_errors {
            errors.extend(condition_errors);
        }

        let token = self.tokens.get(*index).unwrap();
        if token.kind != TokenType::Keyword(KeywordType::Else) {
            errors.push(PythonError {
                error: PythonErrorType::Syntax,
                msg: format!("SyntaxError: expecting \"else\" keyword, got {:?}", token.kind),
                span: token.span,
            });
        }

        // Consume "else" keyword
        *index += 1;
        let (rhs, rhs_span, rhs_errors) = self.pratt_parsing(index, 0, AllowedExpr::ALL);

        if let Some(rhs_errors) = rhs_errors {
            errors.extend(rhs_errors);
        }

        (
            Expression::IfElse(IfElseExpr {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                condition: Box::new(condition),
                span: Span {
                    start: lhs_span.start,
                    end: rhs_span.end,
                },
            }),
            if errors.is_empty() { None } else { Some(errors) },
        )
    }

    fn parse_function_parameters(&self, index: &mut usize) -> (Vec<FuncParameter>, Option<Vec<PythonError>>) {
        let mut errors = Vec::new();
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
                        let (expr, expr_span, expr_errors) = self.pratt_parsing(index, 0, AllowedExpr::ALL);
                        func_parameter.default_value = Some(expr);
                        func_parameter.span.end = expr_span.end;

                        if let Some(expr_errors) = expr_errors {
                            errors.extend(expr_errors);
                        }
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
                        _ => errors.push(PythonError {
                            error: PythonErrorType::Syntax,
                            msg: format!("SyntaxError: expecting identifier, got {:?}", next_token.kind),
                            span: next_token.span,
                        }),
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
                        _ => errors.push(PythonError {
                            error: PythonErrorType::Syntax,
                            msg: format!("SyntaxError: expecting identifier, got {:?}", next_token.kind),
                            span: next_token.span,
                        }),
                    }
                }
                _ => (),
            }

            if self.tokens.get(*index).unwrap().kind != TokenType::Comma {
                break;
            }

            // Consume ,
            *index += 1;
        }

        (parameters, if errors.is_empty() { None } else { Some(errors) })
    }

    fn get_expr_operation(&self, token: &Token, index: &mut usize) -> Result<Operation, PythonError> {
        Ok(match token.kind {
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
            _ => {
                return Err(PythonError {
                    error: PythonErrorType::Syntax,
                    msg: format!("SyntaxError: unexpected token {:?}", token.kind),
                    span: token.span,
                })
            }
        })
    }

    fn parse_class(&self, index: &mut usize) -> (ClassStmt, Option<Vec<PythonError>>) {
        let mut errors = Vec::new();
        let mut class = ClassStmt::default();

        let mut token = self.tokens.get(*index).unwrap();
        match &token.kind {
            TokenType::Id(name) => {
                *index += 1;
                class.name = name.to_string();
            }
            _ => errors.push(PythonError {
                error: PythonErrorType::Syntax,
                msg: format!("Syntax Error: expected identifier, got {token:?}"),
                span: token.span,
            }),
        }

        token = self.tokens.get(*index).unwrap();

        if token.kind == TokenType::OpenParenthesis {
            // Consume (
            *index += 1;
            let (super_classes, super_classes_errors) = self.parse_function_parameters(index);
            class.super_classes = super_classes;

            if let Some(super_classes_errors) = super_classes_errors {
                errors.extend(super_classes_errors);
            }

            token = self.tokens.get(*index).unwrap();
            if token.kind != TokenType::CloseParenthesis {
                errors.push(PythonError {
                    error: PythonErrorType::Syntax,
                    msg: format!("SyntaxError: expecting ')' got {:?}", token.kind),
                    span: token.span,
                });
            } else {
                // Consume )
                *index += 1;
            }
        }

        token = self.tokens.get(*index).unwrap();
        if token.kind != TokenType::Colon {
            errors.push(PythonError {
                error: PythonErrorType::Syntax,
                msg: format!("SyntaxError: expecting ':' got {:?}", token.kind),
                span: token.span,
            });
        } else {
            // Consume :
            *index += 1;
        }

        let (class_block, class_block_errors) = self.parse_block(index);

        if let Some(class_block_errors) = class_block_errors {
            errors.extend(class_block_errors);
        }

        class.block = class_block;
        class.span.end = class.block.span.end;

        (class, if errors.is_empty() { None } else { Some(errors) })
    }

    fn parse_import(&self, index: &mut usize) -> (ImportStmt, Option<Vec<PythonError>>) {
        let mut errors = Vec::new();
        let mut import_stmt = ImportStmt::default();

        loop {
            let (name, span_end, module_name_errors) = self.parse_import_module_name(index);
            let mut import_module = ImportModule { name, alias: None };
            import_stmt.span.end = span_end;

            if let Some(module_name_errors) = module_name_errors {
                errors.extend(module_name_errors);
            }

            if self.tokens.get(*index).unwrap().kind == TokenType::Keyword(KeywordType::As) {
                // Consume "as"
                *index += 1;
                let token = self.tokens.get(*index).unwrap();
                if let Token {
                    kind: TokenType::Id(alias_name),
                    span,
                } = token
                {
                    // Consume Id
                    *index += 1;
                    import_stmt.span.end = span.end;

                    import_module.alias = Some(alias_name.to_string());
                } else {
                    errors.push(PythonError {
                        error: PythonErrorType::Syntax,
                        msg: format!("SyntaxError: expected identifier, got {:?}", token.kind),
                        span: token.span,
                    })
                }
            }

            import_stmt.modules.push(import_module);

            if self.tokens.get(*index).unwrap().kind != TokenType::Comma {
                break;
            }

            // Consume ,
            *index += 1;
        }

        (import_stmt, if errors.is_empty() { None } else { Some(errors) })
    }

    fn parse_from_import(&self, index: &mut usize) -> (FromImportStmt, Option<Vec<PythonError>>) {
        let mut errors = Vec::new();
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
            let (name, _, module_name_errors) = self.parse_import_module_name(index);
            from_import_stmt.module.push(ImportModule { name, alias: None });

            if let Some(module_name_errors) = module_name_errors {
                errors.extend(module_name_errors);
            }
        } else if !has_dots {
            errors.push(PythonError {
                error: PythonErrorType::Syntax,
                msg: format!("Syntax Error: expecting identifier, got {:?}", token.kind),
                span: token.span,
            });
        }

        token = self.tokens.get(*index).unwrap();
        if token.kind != TokenType::Keyword(KeywordType::Import) {
            errors.push(PythonError {
                error: PythonErrorType::Syntax,
                msg: format!("SyntaxError: expecting \"import\" got {:?}", token.kind),
                span: token.span,
            });
        }

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

            return (from_import_stmt, if errors.is_empty() { None } else { Some(errors) });
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
                errors.push(PythonError {
                    error: PythonErrorType::Syntax,
                    msg: format!("SyntaxError: expected identifier, got {:?}", token.kind),
                    span: token.span,
                });
            }

            if self.tokens.get(*index).unwrap().kind == TokenType::Keyword(KeywordType::As) {
                // Consume "as"
                *index += 1;
                let token = self.tokens.get(*index).unwrap();
                if let Token {
                    kind: TokenType::Id(alias_name),
                    span,
                } = token
                {
                    // Consume Id
                    *index += 1;
                    from_import_stmt.span.end = span.end;

                    target.alias = Some(alias_name.to_string());
                } else {
                    errors.push(PythonError {
                        error: PythonErrorType::Syntax,
                        msg: format!("SyntaxError: expected identifier, got {:?}", token.kind),
                        span: token.span,
                    })
                }
            }

            from_import_stmt.targets.push(target);

            if self.tokens.get(*index).unwrap().kind != TokenType::Comma {
                break;
            }

            // Consume ,
            *index += 1;
        }

        if expect_close_paren {
            let token = self.tokens.get(*index).unwrap();
            if token.kind != TokenType::CloseParenthesis {
                errors.push(PythonError {
                    error: PythonErrorType::Syntax,
                    msg: format!("SyntaxError: expecting ')' got {:?}", token.kind),
                    span: token.span,
                });
            } else {
                *index += 1;
            }
        }

        (from_import_stmt, if errors.is_empty() { None } else { Some(errors) })
    }

    fn parse_import_module_name(&self, index: &mut usize) -> (Vec<String>, usize, Option<Vec<PythonError>>) {
        let mut errors = Vec::new();
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
                errors.push(PythonError {
                    error: PythonErrorType::Syntax,
                    msg: format!("SyntaxError: expected identifier, got {:?}", token.kind),
                    span: token.span,
                });
            }

            if self.tokens.get(*index).unwrap().kind != TokenType::Dot {
                break;
            }

            // Consume .
            *index += 1;
        }

        (
            module_name,
            span_end,
            if errors.is_empty() { None } else { Some(errors) },
        )
    }

    fn parse_with(&self, index: &mut usize) -> (WithStmt, Option<Vec<PythonError>>) {
        let mut errors = Vec::new();
        let mut with_stmt = WithStmt::default();
        let mut expect_close_paren = false;

        let mut token = self.tokens.get(*index).unwrap();
        if token.kind == TokenType::OpenParenthesis {
            // Consume (
            *index += 1;
            expect_close_paren = true;
        }

        loop {
            let (item, item_span, item_errors) = self.pratt_parsing(index, 0, AllowedExpr::ALL);

            if let Some(item_errors) = item_errors {
                errors.extend(item_errors);
            }

            let mut with_item = WithItem {
                item,
                target: None,
                span: item_span,
            };

            if self.tokens.get(*index).unwrap().kind == TokenType::Keyword(KeywordType::As) {
                // Consume "as"
                *index += 1;
                let (target, target_span, target_errors) = self.parse_primary(index);
                with_item.target = Some(target);
                with_item.span.end = target_span.end;

                if let Some(target_errors) = target_errors {
                    errors.extend(target_errors);
                }
            }

            with_stmt.items.push(with_item);

            if self.tokens.get(*index).unwrap().kind != TokenType::Comma {
                break;
            }

            // Consume ,
            *index += 1;
        }

        if expect_close_paren {
            token = self.tokens.get(*index).unwrap();
            if !matches!(token.kind, TokenType::CloseParenthesis) {
                errors.push(PythonError {
                    error: PythonErrorType::Syntax,
                    msg: format!("SyntaxError: expecting ')' got {:?}", token.kind),
                    span: token.span,
                });
            } else {
                // Consume )
                *index += 1;
            }
        }

        token = self.tokens.get(*index).unwrap();
        if !matches!(token.kind, TokenType::Colon) {
            errors.push(PythonError {
                error: PythonErrorType::Syntax,
                msg: format!("SyntaxError: expecting ':' got {:?}", token.kind),
                span: token.span,
            });
        } else {
            // Consume :
            *index += 1;
        }

        let (with_block, with_block_errors) = self.parse_block(index);

        if let Some(with_block_errors) = with_block_errors {
            errors.extend(with_block_errors);
        }

        with_stmt.block = with_block;
        with_stmt.span.end = with_stmt.block.span.end;

        (with_stmt, if errors.is_empty() { None } else { Some(errors) })
    }

    fn parse_primary(&self, index: &mut usize) -> (Expression, Span, Option<Vec<PythonError>>) {
        let mut errors = Vec::new();
        let mut token = self.tokens.get(*index).unwrap();
        let (mut lhs, mut lhs_span, lhs_errors) = match &token.kind {
            TokenType::Id(name) => {
                *index += 1;
                (Expression::Id(name.to_string(), token.span), token.span, None)
            }
            TokenType::OpenParenthesis => self.parse_parenthesized_expr(index, token),
            TokenType::OpenBrackets => self.parse_list_expr(index, token.span.start),
            TokenType::OpenBrace => self.parse_bracesized_expr(index, token),
            _ => (
                Expression::Invalid(token.span),
                token.span,
                Some(vec![PythonError {
                    error: PythonErrorType::Syntax,
                    msg: format!("SyntaxError: unexpected token {:?}", token.kind),
                    span: token.span,
                }]),
            ),
        };

        if let Some(lhs_errors) = lhs_errors {
            errors.extend(lhs_errors);
        }

        while self.tokens.get(*index).map_or(false, |token| {
            !matches!(
                &token.kind,
                // Tokens that can end a primary
                // TODO: Check which tokens are necessary
                TokenType::NewLine
                    | TokenType::SemiColon
                    | TokenType::Colon
                    | TokenType::CloseBrace
                    | TokenType::CloseBrackets
                    | TokenType::CloseParenthesis
                    | TokenType::Comma
            )
        }) {
            token = self.tokens.get(*index).unwrap();
            let op = match self.get_expr_operation(token, index) {
                Ok(op) => op,
                Err(error) => {
                    errors.push(error);
                    *index += 1;
                    continue;
                }
            };

            if let Some((lhs_bp, ())) = postfix_binding_power(op) {
                // if lhs_bp < min_precedence_weight {
                //     break;
                // }

                *index += 1;

                lhs = match op {
                    Operation::Unary(UnaryOperator::OpenParenthesis) => {
                        // FIXME: Only parsing function calls with no arguments
                        let expr = Expression::Call(Box::new(lhs), lhs_span);
                        if token.kind != TokenType::CloseParenthesis {
                            errors.push(PythonError {
                                error: PythonErrorType::Syntax,
                                msg: format!("SyntaxError: expecting ')' got {:?}", token.kind),
                                span: token.span,
                            });
                        } else {
                            *index += 1;
                        }
                        expr
                    }
                    Operation::Unary(UnaryOperator::OpenBrackets) => {
                        // FIXME: Only parsing slice with no start, stop or step attributes
                        let (rhs, rhs_span, rhs_errors) = self.pratt_parsing(index, 0, AllowedExpr::ALL);

                        if let Some(rhs_errors) = rhs_errors {
                            errors.extend(rhs_errors);
                        }

                        if token.kind != TokenType::CloseBrackets {
                            errors.push(PythonError {
                                error: PythonErrorType::Syntax,
                                msg: format!("SyntaxError: expecting ']' got {:?}", token.kind),
                                span: token.span,
                            });
                        } else {
                            *index += 1;
                        }

                        Expression::Slice(
                            Box::new(lhs),
                            Box::new(rhs),
                            Span {
                                start: lhs_span.start,
                                end: rhs_span.end + 1,
                            },
                        )
                    }
                    _ => {
                        *index += 1;
                        errors.push(PythonError {
                            error: PythonErrorType::Syntax,
                            msg: format!("SyntaxError: invalid postfix operator! {:?}", op),
                            span: token.span,
                        });
                        Expression::Invalid(token.span)
                    }
                };
                continue;
            }

            if let Some((lhs_bp, rhs_bp)) = infix_binding_power(op) {
                // if lhs_bp < min_precedence_weight {
                //     break;
                // }

                *index += 1;

                let (rhs, rhs_span, rhs_errors) = self.pratt_parsing(index, rhs_bp, AllowedExpr::ALL);

                if let Some(rhs_errors) = rhs_errors {
                    errors.extend(rhs_errors);
                }

                lhs_span = Span {
                    start: lhs_span.start,
                    end: rhs_span.end,
                };
                lhs = Expression::BinaryOp(Box::new(lhs), op.get_binary_op(), Box::new(rhs), lhs_span);
                continue;
            }
        }

        (lhs, lhs_span, if errors.is_empty() { None } else { Some(errors) })
    }

    fn parse_try(&self, index: &mut usize) -> (TryStmt, Option<Vec<PythonError>>) {
        let mut errors = Vec::new();
        // TODO: improve error checking
        let mut token = self.tokens.get(*index).unwrap();
        if !matches!(token.kind, TokenType::Colon) {
            errors.push(PythonError {
                error: PythonErrorType::Syntax,
                msg: format!("SyntaxError: expecting ':' got {:?}", token.kind),
                span: token.span,
            });
        } else {
            // Consume :
            *index += 1;
        }

        let (try_block, try_block_errors) = self.parse_block(index);

        if let Some(try_block_errors) = try_block_errors {
            errors.extend(try_block_errors);
        }

        let mut try_stmt = TryStmt {
            block: try_block,
            finally_block: None,
            except_blocks: vec![],
            else_stmt: None,
            ..Default::default()
        };

        token = self.tokens.get(*index).unwrap();
        let mut has_except = false;
        // TODO: parse more than one except block
        if token.kind == TokenType::Keyword(KeywordType::Except) {
            has_except = true;
            // Consume "except"
            *index += 1;
            let mut except_block = ExceptBlock {
                span: Span {
                    start: token.span.start,
                    end: 0,
                },
                ..Default::default()
            };

            if self.tokens.get(*index).unwrap().kind == TokenType::Operator(OperatorType::Asterisk) {
                // Consume *
                *index += 1;
                except_block.kind = ExceptBlockKind::ExceptStar;
            }

            if self.tokens.get(*index).unwrap().kind != TokenType::Colon {
                let (expr, _, expr_errors) = self.pratt_parsing(index, 0, AllowedExpr::ALL);
                except_block.expr = Some(expr);

                if let Some(expr_errors) = expr_errors {
                    errors.extend(expr_errors);
                }

                if self.tokens.get(*index).unwrap().kind == TokenType::Keyword(KeywordType::As) {
                    // Consume "as"
                    *index += 1;

                    match self.tokens.get(*index).unwrap() {
                        Token {
                            kind: TokenType::Id(name),
                            ..
                        } => {
                            // Consume Id
                            *index += 1;
                            except_block.expr_alias = Some(name.to_string());
                        }
                        token => errors.push(PythonError {
                            error: PythonErrorType::Syntax,
                            msg: format!("SyntaxError: expecting identifier, got {:?}", token.kind),
                            span: token.span,
                        }),
                    }
                }
            }

            token = self.tokens.get(*index).unwrap();
            if !matches!(token.kind, TokenType::Colon) {
                errors.push(PythonError {
                    error: PythonErrorType::Syntax,
                    msg: format!("SyntaxError: expecting ':' got {:?}", token.kind),
                    span: token.span,
                });
            } else {
                // Consume :
                *index += 1;
            }

            // TODO: rename this
            let (except, except_errors) = self.parse_block(index);

            if let Some(except_errors) = except_errors {
                errors.extend(except_errors);
            }

            except_block.block = except;
            except_block.span.end = except_block.block.span.end;

            try_stmt.span.end = except_block.span.end;
            try_stmt.except_blocks.push(except_block);

            token = self.tokens.get(*index).unwrap();
        }

        if has_except && token.kind == TokenType::Keyword(KeywordType::Else) {
            // Consume "else"
            *index += 1;
            let else_start = token.span.start;

            token = self.tokens.get(*index).unwrap();
            if !matches!(token.kind, TokenType::Colon) {
                errors.push(PythonError {
                    error: PythonErrorType::Syntax,
                    msg: format!("SyntaxError: expecting ':' got {:?}", token.kind),
                    span: token.span,
                });
            } else {
                // Consume :
                *index += 1;
            }

            let (else_block, else_block_errors) = self.parse_block(index);

            if let Some(else_block_errors) = else_block_errors {
                errors.extend(else_block_errors);
            }

            try_stmt.span.end = else_block.span.end;
            try_stmt.else_stmt = Some(ElseStmt {
                span: Span {
                    start: else_start,
                    end: else_block.span.end,
                },
                block: else_block,
            });

            token = self.tokens.get(*index).unwrap();
        }

        if token.kind == TokenType::Keyword(KeywordType::Finally) {
            // Consume "finally"
            *index += 1;
            let finally_start = token.span.start;

            token = self.tokens.get(*index).unwrap();
            if !matches!(token.kind, TokenType::Colon) {
                errors.push(PythonError {
                    error: PythonErrorType::Syntax,
                    msg: format!("SyntaxError: expecting ':' got {:?}", token.kind),
                    span: token.span,
                });
            } else {
                // Consume :
                *index += 1;
            }

            let (finally_block, finally_block_errors) = self.parse_block(index);

            if let Some(finally_block_errors) = finally_block_errors {
                errors.extend(finally_block_errors);
            }

            try_stmt.span.end = finally_block.span.end;
            try_stmt.finally_block = Some(FinallyBlock {
                span: Span {
                    start: finally_start,
                    end: finally_block.span.end,
                },
                block: finally_block,
            })
        }

        (try_stmt, if errors.is_empty() { None } else { Some(errors) })
    }

    fn parse_return(&self, index: &mut usize) -> (ReturnStmt, Option<Vec<PythonError>>) {
        let mut errors = Vec::new();
        let mut return_stmt = ReturnStmt::default();

        let token = self.tokens.get(*index).unwrap();
        return_stmt.span.end = token.span.end;

        let token = self.tokens.get(*index);
        if matches!(
            token.map(|next_token| &next_token.kind),
            Some(
                TokenType::Number(_, _)
                    | TokenType::Id(_)
                    | TokenType::String(_)
                    | TokenType::OpenParenthesis
                    | TokenType::Operator(OperatorType::Plus | OperatorType::Minus)
                    | TokenType::Keyword(KeywordType::Not | KeywordType::None)
            )
        ) {
            let (expr, expr_span, expr_errors) = self.pratt_parsing(index, 0, AllowedExpr::ALL);
            if let Some(expr_errors) = expr_errors {
                errors.extend(expr_errors);
            }
            return_stmt.value = Some(expr);
            return_stmt.span.end = expr_span.end;
        }

        (return_stmt, if errors.is_empty() { None } else { Some(errors) })
    }

    fn parse_yield(&self, index: &mut usize, token: &Token) -> (Expression, Span, Option<Vec<PythonError>>) {
        let mut yield_span = token.span;

        *index += 1;
        let token = self.tokens.get(*index).unwrap();

        if token.kind == TokenType::Keyword(KeywordType::From) {
            // consume "from"
            *index += 1;

            let (rhs, rhs_span, rhs_errors) = self.parse_expression(index, AllowedExpr::ALL);
            yield_span = Span {
                start: yield_span.start,
                end: rhs_span.end,
            };
            return (Expression::YieldFrom(Box::new(rhs), yield_span), yield_span, rhs_errors);
        }

        if helpers::is_token_start_of_expr(token) {
            let (rhs, rhs_span, rhs_errors) = self.parse_expression(index, AllowedExpr::ALL);
            yield_span = Span {
                start: yield_span.start,
                end: rhs_span.end,
            };
            return (
                Expression::Yield(Some(Box::new(rhs)), yield_span),
                yield_span,
                rhs_errors,
            );
        }

        return (Expression::Yield(None, yield_span), yield_span, None);
    }

    fn parse_for_stmt(&self, index: &mut usize) -> (ForStmt, Option<Vec<PythonError>>) {
        let mut errors = Vec::new();
        let mut for_stmt = ForStmt::default();

        let (target, _, target_errors) = self.parse_expression(
            index,
            AllowedExpr::ID | AllowedExpr::UNARY_OP | AllowedExpr::TUPLE | AllowedExpr::LIST,
        );
        for_stmt.target = target;

        if let Some(target_errors) = target_errors {
            errors.extend(target_errors);
        }

        let mut token = self.tokens.get(*index).unwrap();
        if token.kind != TokenType::Keyword(KeywordType::In) {
            errors.push(PythonError {
                error: PythonErrorType::Syntax,
                msg: format!("SyntaxError: expecting 'in' got {:?}", token.kind),
                span: token.span,
            });
        } else {
            // Consume "in"
            *index += 1;
        }

        let (iter, _, iter_errors) = self.parse_expression(index, AllowedExpr::ALL);
        for_stmt.iter = iter;

        if let Some(iter_errors) = iter_errors {
            errors.extend(iter_errors);
        }

        token = self.tokens.get(*index).unwrap();
        if token.kind != TokenType::Colon {
            errors.push(PythonError {
                error: PythonErrorType::Syntax,
                msg: format!("SyntaxError: expecting ':' got {:?}", token.kind),
                span: token.span,
            });
        } else {
            // Consume :
            *index += 1;
        }

        let (for_block, while_block_errors) = self.parse_block(index);
        for_stmt.span.end = for_block.span.end;
        for_stmt.block = for_block;

        if let Some(while_block_errors) = while_block_errors {
            errors.extend(while_block_errors);
        }

        token = self.tokens.get(*index).unwrap();
        if token.kind == TokenType::Keyword(KeywordType::Else) {
            *index += 1;
            let else_start = token.span.start;
            token = self.tokens.get(*index).unwrap();
            if token.kind != TokenType::Colon {
                errors.push(PythonError {
                    error: PythonErrorType::Syntax,
                    msg: format!("SyntaxError: expecting ':' got {:?}", token.kind),
                    span: token.span,
                });
            } else {
                *index += 1;
            }
            let (else_block, else_block_errors) = self.parse_block(index);

            if let Some(else_block_errors) = else_block_errors {
                errors.extend(else_block_errors);
            }

            for_stmt.span.end = else_block.span.end;
            for_stmt.else_stmt = Some(ElseStmt {
                span: Span {
                    start: else_start,
                    end: else_block.span.end,
                },
                block: else_block,
            });
        }

        (for_stmt, if errors.is_empty() { None } else { Some(errors) })
    }
}
