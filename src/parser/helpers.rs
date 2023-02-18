use bitflags::bitflags;

use crate::lexer::token::{
    types::{KeywordType, OperatorType, TokenType},
    Token,
};

use super::ast::{BinaryOperator, Operation, UnaryOperator};

bitflags! {
    #[derive(Default)]
    pub struct AllowedExpr: u32 {
        const BINARY_OP = 1 << 0;
        const BOOL = 1 << 1;
        const CALL = 1 << 2;
        const DICT = 1 << 3;
        const ELLIPSIS = 1 << 4;
        const ID = 1 << 5;
        const IF_ELSE = 1 << 6;
        const LAMBDA = 1 << 7;
        const LIST = 1 << 8;
        const NONE = 1 << 9;
        const NUMBER = 1 << 10;
        const PARENTHESIZED = 1<< 11;
        const SET = 1 << 12;
        const SLICE = 1 << 13;
        const STRING = 1 << 14;
        const TUPLE = 1 << 15;
        const UNARY_OP = 1 << 16;
        const YIELD = 1 << 17;
        const YIELD_FROM = 1 << 18;
        const ATTR_REF = 1 << 19;
        const ASSIGN = 1 << 20;
        /// Doesn't contain the ASSIGN
        const ALL = Self::BINARY_OP.bits
                    | Self::BOOL.bits | Self::CALL.bits | Self::DICT.bits | Self::ELLIPSIS.bits | Self::ID.bits
                    | Self::IF_ELSE.bits | Self::LAMBDA.bits | Self::LIST.bits | Self::NONE.bits | Self::NUMBER.bits
                    | Self::SET.bits | Self::SLICE.bits | Self::STRING.bits | Self::TUPLE.bits | Self::UNARY_OP.bits
                    | Self::YIELD.bits | Self::YIELD_FROM.bits | Self::PARENTHESIZED.bits | Self::ATTR_REF.bits;
    }
}

pub fn postfix_binding_power(op: Operation) -> Option<(u8, ())> {
    match op {
        Operation::Unary(UnaryOperator::OpenParenthesis) => Some((22, ())),
        Operation::Unary(UnaryOperator::OpenBrackets) => Some((22, ())),
        _ => None,
    }
}

pub fn prefix_binding_power(op: Operation) -> Option<((), u8)> {
    match op {
        Operation::Unary(UnaryOperator::Lambda) => Some(((), 2)),
        Operation::Unary(UnaryOperator::LogicalNot) => Some(((), 6)),
        Operation::Unary(
            UnaryOperator::Plus
            | UnaryOperator::Minus
            | UnaryOperator::BitwiseNot
            | UnaryOperator::UnpackIterable
            | UnaryOperator::UnpackDictionary,
        ) => Some(((), 16)),
        Operation::Unary(UnaryOperator::Await) => Some(((), 20)),
        _ => None,
    }
}

pub fn infix_binding_power(op: Operation) -> Option<(u8, u8)> {
    match op {
        Operation::Binary(BinaryOperator::Walrus) => Some((1, 1)),
        Operation::Binary(BinaryOperator::IfElse) => Some((3, 3)),
        Operation::Binary(BinaryOperator::LogicalOr) => Some((4, 4)),
        Operation::Binary(BinaryOperator::LogicalAnd) => Some((5, 5)),
        Operation::Binary(
            BinaryOperator::In
            | BinaryOperator::NotIn
            | BinaryOperator::Is
            | BinaryOperator::IsNot
            | BinaryOperator::LessThan
            | BinaryOperator::LessThanOrEqual
            | BinaryOperator::GreaterThan
            | BinaryOperator::GreaterThanOrEqual
            | BinaryOperator::Equals
            | BinaryOperator::NotEqual,
        ) => Some((7, 7)),
        Operation::Binary(BinaryOperator::BitwiseOr) => Some((8, 8)),
        Operation::Binary(BinaryOperator::BitwiseXOR) => Some((9, 9)),
        Operation::Binary(BinaryOperator::BitwiseAnd) => Some((10, 10)),
        Operation::Binary(BinaryOperator::BitwiseLeftShift | BinaryOperator::BitwiseRightShift) => Some((11, 11)),
        Operation::Binary(BinaryOperator::Add | BinaryOperator::Subtract) => Some((12, 13)),
        Operation::Binary(BinaryOperator::Divide | BinaryOperator::FloorDivision | BinaryOperator::Modulo) => {
            Some((14, 14))
        }
        Operation::Binary(BinaryOperator::Multiply) => Some((14, 15)),
        Operation::Binary(BinaryOperator::Exponent) => Some((18, 18)),
        Operation::Binary(BinaryOperator::AttributeRef) => Some((22, 22)),
        _ => None,
    }
}

pub fn is_token_start_of_expr(token: &Token) -> bool {
    matches!(
        token.kind,
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

pub fn is_token_end_of_expr(token: &Token) -> bool {
    matches!(
        token.kind,
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
    )
}
