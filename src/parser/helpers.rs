use super::ast::{BinaryOperator, Operation, UnaryOperator};

pub fn postfix_binding_power(op: Operation) -> Option<(u8, ())> {
    match op {
        Operation::Unary(UnaryOperator::OpenParenthesis) => Some((22, ())),
        Operation::Unary(UnaryOperator::OpenBrackets) => Some((22, ())),
        _ => None,
    }
}

pub fn prefix_binding_power(op: Operation) -> Option<((), u8)> {
    match op {
        Operation::Unary(UnaryOperator::LogicalNot) => Some(((), 6)),
        Operation::Unary(UnaryOperator::Await) => Some(((), 20)),
        Operation::Unary(
            UnaryOperator::Plus | UnaryOperator::Minus | UnaryOperator::BitwiseNot | UnaryOperator::UnpackIterable,
        ) => Some(((), 16)),
        _ => None,
    }
}

pub fn infix_binding_power(op: Operation) -> Option<(u8, u8)> {
    match op {
        Operation::Binary(BinaryOperator::Walrus) => Some((1, 1)),
        Operation::Binary(BinaryOperator::Lambda) => Some((2, 2)),
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
