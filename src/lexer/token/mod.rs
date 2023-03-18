pub mod types;

use crate::parser::ast::{AugAssignType, Operation, UnaryOperator};

use self::types::{KeywordType, OperatorType, TokenType};

use super::span::Span;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Token<'a> {
    pub kind: TokenType<'a>,
    pub span: Span,
}

impl<'a> Token<'a> {
    pub fn is_augassign(&self) -> bool {
        matches!(
            self.kind,
            TokenType::Operator(
                OperatorType::PlusEqual
                    | OperatorType::AtEqual
                    | OperatorType::MinusEqual
                    | OperatorType::DivideEqual
                    | OperatorType::FloorDivisionEqual
                    | OperatorType::AsteriskEqual
                    | OperatorType::ModuloEqual
                    | OperatorType::ExponentEqual
                    | OperatorType::BitwiseAndEqual
                    | OperatorType::BitwiseOrEqual
                    | OperatorType::BitwiseXOrEqual
                    | OperatorType::BitwiseNotEqual
                    | OperatorType::BitwiseLeftShiftEqual
                    | OperatorType::BitwiseRightShiftEqual
            )
        )
    }

    pub fn is_assign(&self) -> bool {
        self.kind == TokenType::Operator(OperatorType::Assign)
    }

    pub fn to_augassign_type(&self) -> Result<AugAssignType, String> {
        Ok(match &self.kind {
            TokenType::Operator(OperatorType::AsteriskEqual) => AugAssignType::Asterisk,
            TokenType::Operator(OperatorType::AtEqual) => AugAssignType::At,
            TokenType::Operator(OperatorType::BitwiseAndEqual) => AugAssignType::BitwiseAnd,
            TokenType::Operator(OperatorType::BitwiseLeftShiftEqual) => AugAssignType::BitwiseLeftShift,
            TokenType::Operator(OperatorType::BitwiseNotEqual) => AugAssignType::BitwiseNot,
            TokenType::Operator(OperatorType::BitwiseOrEqual) => AugAssignType::BitwiseOr,
            TokenType::Operator(OperatorType::BitwiseRightShiftEqual) => AugAssignType::BitwiseRightShift,
            TokenType::Operator(OperatorType::BitwiseXOrEqual) => AugAssignType::BitwiseXOr,
            TokenType::Operator(OperatorType::DivideEqual) => AugAssignType::Divide,
            TokenType::Operator(OperatorType::ExponentEqual) => AugAssignType::Exponent,
            TokenType::Operator(OperatorType::FloorDivisionEqual) => AugAssignType::FloorDivision,
            TokenType::Operator(OperatorType::MinusEqual) => AugAssignType::Minus,
            TokenType::Operator(OperatorType::ModuloEqual) => AugAssignType::Modulus,
            TokenType::Operator(OperatorType::PlusEqual) => AugAssignType::Plus,
            t => return Err(format!("Invalid conversion of token {:?} to augassign type!", t)),
        })
    }

    pub fn is_start_of_expr(&self) -> bool {
        matches!(
            self.kind,
            TokenType::Number(_, _)
                | TokenType::Id(_)
                | TokenType::String(_)
                | TokenType::OpenParenthesis
                | TokenType::OpenBrackets
                | TokenType::OpenBrace
                | TokenType::Operator(
                    OperatorType::Plus
                        | OperatorType::Minus
                        | OperatorType::Asterisk
                        | OperatorType::BitwiseNot
                        | OperatorType::Exponent
                )
                | TokenType::Keyword(
                    KeywordType::Not
                        | KeywordType::None
                        | KeywordType::True
                        | KeywordType::False
                        | KeywordType::Await
                        | KeywordType::Lambda
                )
        )
    }

    pub fn is_end_of_expr(&self) -> bool {
        matches!(
            self.kind,
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
                | TokenType::Keyword(KeywordType::As)
                | TokenType::Keyword(KeywordType::From)
                | TokenType::Keyword(KeywordType::For)
        )
    }

    pub fn is_simple_stmt(&self) -> bool {
        self.is_start_of_expr()
            || matches!(
                self.kind,
                TokenType::Keyword(
                    KeywordType::Del
                        | KeywordType::Pass
                        | KeywordType::Import
                        | KeywordType::Global
                        | KeywordType::NonLocal
                        | KeywordType::Assert
                        | KeywordType::Break
                        | KeywordType::Continue
                        | KeywordType::Return
                        | KeywordType::Raise
                        | KeywordType::Yield
                ) 
                // Here we treat SemiColon as simple statement just in case we encounter something
                // like this while parsing:
                // def t(): ;
                // Then, in `parse_simple_stmts` we can generate a appropriate error message.
                | TokenType::SemiColon
            )
    }

    pub fn to_unary_operation(&self) -> Result<Operation, String> {
        Ok(match &self.kind {
            TokenType::Operator(OperatorType::BitwiseNot) => Operation::Unary(UnaryOperator::BitwiseNot),
            TokenType::Operator(OperatorType::Plus) => Operation::Unary(UnaryOperator::Plus),
            TokenType::Operator(OperatorType::Minus) => Operation::Unary(UnaryOperator::Minus),
            TokenType::Operator(OperatorType::Asterisk) => Operation::Unary(UnaryOperator::UnpackIterable),
            TokenType::Operator(OperatorType::Exponent) => Operation::Unary(UnaryOperator::UnpackDictionary),
            TokenType::Keyword(KeywordType::Not) => Operation::Unary(UnaryOperator::LogicalNot),
            TokenType::Keyword(KeywordType::Await) => Operation::Unary(UnaryOperator::Await),
            TokenType::Keyword(KeywordType::Lambda) => Operation::Unary(UnaryOperator::Lambda),
            // NOTE: this message probably shouldn't be here
            kind => return Err(format!("SyntaxError: unexpected operator {:?}", kind))
            }
        )
    }
}
