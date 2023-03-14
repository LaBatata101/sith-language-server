pub mod types;

use crate::parser::ast::AugAssignType;

use self::types::{OperatorType, TokenType};

use super::span::Span;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenType,
    pub span: Span,
}

impl Token {
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
}
