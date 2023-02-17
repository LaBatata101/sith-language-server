pub mod types;

use crate::parser::ast::AugAssignType;

use self::types::{OperatorType, TokenType};

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default)]
pub struct Span {
    // TODO: add field to know which file the Span belongs to
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenType,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenType, start: usize, end: usize) -> Self {
        Self {
            kind,
            span: Span { start, end },
        }
    }

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
                    | OperatorType::ModulusEqual
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
        match &self.kind {
            TokenType::Operator(OperatorType::AsteriskEqual) => Ok(AugAssignType::Asterisk),
            TokenType::Operator(OperatorType::AtEqual) => Ok(AugAssignType::At),
            TokenType::Operator(OperatorType::BitwiseAndEqual) => Ok(AugAssignType::BitwiseAnd),
            TokenType::Operator(OperatorType::BitwiseLeftShiftEqual) => Ok(AugAssignType::BitwiseLeftShift),
            TokenType::Operator(OperatorType::BitwiseNotEqual) => Ok(AugAssignType::BitwiseNot),
            TokenType::Operator(OperatorType::BitwiseOrEqual) => Ok(AugAssignType::BitwiseOr),
            TokenType::Operator(OperatorType::BitwiseRightShiftEqual) => Ok(AugAssignType::BitwiseRightShift),
            TokenType::Operator(OperatorType::BitwiseXOrEqual) => Ok(AugAssignType::BitwiseXOr),
            TokenType::Operator(OperatorType::DivideEqual) => Ok(AugAssignType::Divide),
            TokenType::Operator(OperatorType::ExponentEqual) => Ok(AugAssignType::Exponent),
            TokenType::Operator(OperatorType::FloorDivisionEqual) => Ok(AugAssignType::FloorDivision),
            TokenType::Operator(OperatorType::MinusEqual) => Ok(AugAssignType::Minus),
            TokenType::Operator(OperatorType::ModulusEqual) => Ok(AugAssignType::Modulus),
            TokenType::Operator(OperatorType::PlusEqual) => Ok(AugAssignType::Plus),
            t => Err(format!("Invalid conversion of token {:?} to augassign type!", t)),
        }
    }
}
