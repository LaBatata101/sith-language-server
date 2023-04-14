use bitflags::bitflags;

use super::ast::{BinaryOperator, Operation, UnaryOperator};

bitflags! {
    #[derive(Default)]
    pub struct ExprBitflag: u32 {
        const BOOL = 1 << 0;
        const DICT = 1 << 1;
        const ELLIPSIS = 1 << 2;
        const ID = 1 << 3;
        const LIST = 1 << 4;
        const NONE = 1 << 5;
        const NUMBER = 1 << 6;
        const PARENTHESIZED = 1 << 7;
        const SET = 1 << 8;
        const STRING = 1 << 9;
        const TUPLE = 1 << 10;
        const YIELD = 1 << 11;
        const YIELD_FROM = 1 << 12;
        const ASSIGN = 1 << 13;
        const TUPLE_NO_PARENS = 1 << 14;
        /// Doesn't contain the ASSIGN
        const ALL = Self::BOOL.bits | Self::DICT.bits | Self::ELLIPSIS.bits | Self::ID.bits | Self::LIST.bits
                    | Self::NONE.bits | Self::NUMBER.bits | Self::SET.bits | Self::STRING.bits | Self::TUPLE.bits
                    | Self::YIELD.bits | Self::YIELD_FROM.bits | Self::PARENTHESIZED.bits | Self::TUPLE_NO_PARENS.bits;
    }

    #[derive(Default)]
    pub struct BinaryOperationsBitflag: u32 {
        const ADD = 1 << 0;
        const SUBTRACT = 1 << 1;
        const FLOOR_DIVISION = 1 << 2;
        const DIVISION = 1 << 3;
        const MULTIPLY = 1 << 4;
        const MODULO = 1 << 5;
        const EXPONENT = 1 << 6;

        const IN = 1 << 7;
        const IS = 1 << 8;
        const NOT_IN = 1 << 9;
        const IS_NOT = 1 << 10;
        const LESS_THAN = 1 << 11;
        const LESS_THAN_OR_EQUAL = 1 << 12;
        const GREATER_THAN = 1 << 13;
        const GREATER_THAN_OR_EQUAL = 1 << 14;
        const EQUALS = 1 << 15;
        const NOT_EQUAL = 1 << 16;
        const OR = 1 << 17;
        const AND = 1 << 18;

        const BITWISE_OR = 1 << 19;
        const BITWISE_XOR = 1 << 20;
        const BITWISE_AND = 1 << 22;
        const BITWISE_LEFT_SHIFT = 1 << 22;
        const BITWISE_RIGHT_SHIFT = 1 << 23;

        const WALRUS = 1 << 24;
        const IF_ELSE = 1 << 25;
        const ATTRIBUTE_REF = 1 << 26;

        const AT = 1 << 27;

        // This is a hacky way to avoid errors when parsing an "if ... else ..." inside of a comprehension.
        // For instance, the following code "[f for f in l if (a if b is None else c)]" without
        // this flag would not be able to parse the "if ... else ..." inside the parenthesis.
        const IF_ELSE_WITHIN_PARENS = 1 << 28;

        const ALL = Self::ADD.bits | Self::SUBTRACT.bits | Self::FLOOR_DIVISION.bits | Self::DIVISION.bits
        | Self::MULTIPLY.bits | Self::MODULO.bits | Self::EXPONENT.bits | Self::IN.bits | Self::IS.bits
        | Self::NOT_IN.bits | Self::IS_NOT.bits | Self::LESS_THAN.bits | Self::LESS_THAN_OR_EQUAL.bits
        | Self::GREATER_THAN.bits | Self::GREATER_THAN_OR_EQUAL.bits | Self::EQUALS.bits | Self::NOT_EQUAL.bits
        | Self::OR.bits | Self::AND.bits | Self::BITWISE_OR.bits | Self::BITWISE_XOR.bits | Self::BITWISE_AND.bits
        | Self::BITWISE_LEFT_SHIFT.bits | Self::BITWISE_RIGHT_SHIFT.bits | Self::WALRUS.bits | Self::IF_ELSE.bits
        | Self::ATTRIBUTE_REF.bits | Self::AT.bits;
    }

    #[derive(Default)]
    pub struct UnaryOperationsBitflag: u32 {
        const PLUS = 1 << 0;
        const MINUS = 1 << 1;
        const BITWISE_NOT = 1 << 2;
        const NOT = 1 << 3;
        const AWAIT = 1 << 4;
        const LAMBDA = 1 << 5;
        const FUNC_CALL = 1 << 6;
        const SUBSCRIPT = 1 << 7;
        const UNPACK_ITERABLE = 1 << 8;
        const UNPACK_DICT = 1 << 9;
        const ALL = Self::PLUS.bits | Self::MINUS.bits | Self::BITWISE_NOT.bits | Self::NOT.bits | Self::AWAIT.bits
                    | Self::LAMBDA.bits | Self::FUNC_CALL.bits | Self::SUBSCRIPT.bits | Self::UNPACK_ITERABLE.bits
                    | Self::UNPACK_DICT.bits;
    }
}

#[derive(Default, Clone, Copy, Debug)]
pub struct ParseExprBitflags {
    pub expressions: ExprBitflag,
    pub binary_op: BinaryOperationsBitflag,
    pub unary_op: UnaryOperationsBitflag,
}

impl ParseExprBitflags {
    pub fn empty() -> Self {
        Self { ..Default::default() }
    }

    pub fn all() -> Self {
        Self {
            expressions: ExprBitflag::ALL | ExprBitflag::ASSIGN,
            binary_op: BinaryOperationsBitflag::ALL,
            unary_op: UnaryOperationsBitflag::ALL,
        }
    }

    pub fn set_binary_op(mut self, binary_op: BinaryOperationsBitflag) -> Self {
        self.binary_op |= binary_op;
        self
    }

    pub fn set_unary_op(mut self, unary_op: UnaryOperationsBitflag) -> Self {
        self.unary_op |= unary_op;
        self
    }

    pub fn set_expressions(mut self, expr: ExprBitflag) -> Self {
        self.expressions |= expr;
        self
    }

    pub fn remove_expression(mut self, expr: ExprBitflag) -> Self {
        self.expressions.remove(expr);
        self
    }

    pub fn remove_binary_op(mut self, bin_op: BinaryOperationsBitflag) -> Self {
        self.binary_op.remove(bin_op);
        self
    }
}

pub fn postfix_binding_power(op: Operation, allowed_unary_op: UnaryOperationsBitflag) -> Option<(u8, ())> {
    match op {
        Operation::Unary(UnaryOperator::OpenParenthesis | UnaryOperator::OpenBrackets)
            if allowed_unary_op.intersects(UnaryOperationsBitflag::FUNC_CALL | UnaryOperationsBitflag::SUBSCRIPT) =>
        {
            Some((22, ()))
        }
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

pub fn infix_binding_power(op: Operation, allowed_bin_op: BinaryOperationsBitflag) -> Option<(u8, u8)> {
    match op {
        Operation::Binary(BinaryOperator::Walrus) if allowed_bin_op.intersects(BinaryOperationsBitflag::WALRUS) => {
            Some((1, 1))
        }
        Operation::Binary(BinaryOperator::IfElse) if allowed_bin_op.intersects(BinaryOperationsBitflag::IF_ELSE) => {
            Some((3, 3))
        }
        Operation::Binary(BinaryOperator::LogicalOr) if allowed_bin_op.intersects(BinaryOperationsBitflag::OR) => {
            Some((4, 4))
        }
        Operation::Binary(BinaryOperator::LogicalAnd) if allowed_bin_op.intersects(BinaryOperationsBitflag::AND) => {
            Some((5, 5))
        }
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
        ) if allowed_bin_op.intersects(
            BinaryOperationsBitflag::IN
                | BinaryOperationsBitflag::IS
                | BinaryOperationsBitflag::IS_NOT
                | BinaryOperationsBitflag::NOT_IN
                | BinaryOperationsBitflag::LESS_THAN
                | BinaryOperationsBitflag::LESS_THAN_OR_EQUAL
                | BinaryOperationsBitflag::GREATER_THAN
                | BinaryOperationsBitflag::GREATER_THAN_OR_EQUAL
                | BinaryOperationsBitflag::EQUALS
                | BinaryOperationsBitflag::NOT_EQUAL,
        ) =>
        {
            Some((7, 7))
        }
        Operation::Binary(BinaryOperator::BitwiseOr)
            if allowed_bin_op.intersects(BinaryOperationsBitflag::BITWISE_OR) =>
        {
            Some((8, 8))
        }
        Operation::Binary(BinaryOperator::BitwiseXOR)
            if allowed_bin_op.intersects(BinaryOperationsBitflag::BITWISE_XOR) =>
        {
            Some((9, 9))
        }
        Operation::Binary(BinaryOperator::BitwiseAnd)
            if allowed_bin_op.intersects(BinaryOperationsBitflag::BITWISE_AND) =>
        {
            Some((10, 10))
        }
        Operation::Binary(BinaryOperator::BitwiseLeftShift | BinaryOperator::BitwiseRightShift)
            if allowed_bin_op.intersects(
                BinaryOperationsBitflag::BITWISE_LEFT_SHIFT | BinaryOperationsBitflag::BITWISE_RIGHT_SHIFT,
            ) =>
        {
            Some((11, 11))
        }
        Operation::Binary(BinaryOperator::Add | BinaryOperator::Subtract)
            if allowed_bin_op.intersects(BinaryOperationsBitflag::ADD | BinaryOperationsBitflag::SUBTRACT) =>
        {
            Some((12, 13))
        }
        Operation::Binary(
            BinaryOperator::Divide | BinaryOperator::FloorDivision | BinaryOperator::Modulo | BinaryOperator::At,
        ) if allowed_bin_op.intersects(
            BinaryOperationsBitflag::DIVISION
                | BinaryOperationsBitflag::FLOOR_DIVISION
                | BinaryOperationsBitflag::MODULO
                | BinaryOperationsBitflag::AT,
        ) =>
        {
            Some((14, 14))
        }
        Operation::Binary(BinaryOperator::Multiply) if allowed_bin_op.intersects(BinaryOperationsBitflag::MULTIPLY) => {
            Some((14, 15))
        }
        Operation::Binary(BinaryOperator::Exponent) if allowed_bin_op.intersects(BinaryOperationsBitflag::EXPONENT) => {
            Some((18, 18))
        }
        Operation::Binary(BinaryOperator::AttributeRef)
            if allowed_bin_op.intersects(BinaryOperationsBitflag::ATTRIBUTE_REF) =>
        {
            Some((22, 22))
        }
        _ => None,
    }
}
