use std::borrow::Cow;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenType<'a> {
    CloseBrace,
    CloseBrackets,
    CloseParenthesis,
    Colon,
    Comma,
    Dedent,
    Dot,
    Eof,
    Ellipsis,
    Id(Cow<'a, str>),
    Indent,
    Invalid(char),
    Keyword(KeywordType),
    NewLine,
    Number(NumberType, String),
    OpenBrace,
    OpenBrackets,
    OpenParenthesis,
    Operator(OperatorType),
    SemiColon,
    /// These are only keywords under specific contexts
    SoftKeyword(SoftKeywordType),
    String(Cow<'a, str>),
    RightArrow,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum NumberType {
    Float,
    Integer(IntegerType),
    Imaginary,
    Invalid,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum IntegerType {
    Decimal,
    Binary,
    Octal,
    Hex,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum OperatorType {
    Assign,
    Asterisk,
    AsteriskEqual,
    At,
    AtEqual,
    BitwiseAnd,
    BitwiseAndEqual,
    BitwiseLeftShift,
    BitwiseLeftShiftEqual,
    BitwiseNot,
    BitwiseNotEqual,
    BitwiseOr,
    BitwiseOrEqual,
    BitwiseRightShift,
    BitwiseRightShiftEqual,
    BitwiseXOR,
    BitwiseXOrEqual,
    ColonEqual,
    Divide,
    DivideEqual,
    Equals,
    Exponent,
    ExponentEqual,
    FloorDivision,
    FloorDivisionEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Minus,
    MinusEqual,
    Modulo,
    ModuloEqual,
    NotEquals,
    Plus,
    PlusEqual,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum KeywordType {
    And,
    As,
    Async,
    Assert,
    Await,
    Break,
    Class,
    Continue,
    Def,
    Del,
    Elif,
    Else,
    Except,
    False,
    Finally,
    For,
    From,
    Global,
    If,
    Import,
    In,
    Is,
    Lambda,
    None,
    NonLocal,
    Not,
    Or,
    Pass,
    Raise,
    Return,
    True,
    Try,
    While,
    With,
    Yield,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SoftKeywordType {
    Match,
    Case,
    /// The char "_"
    Underscore,
}
