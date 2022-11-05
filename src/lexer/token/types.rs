#[derive(Debug, PartialEq, Eq)]
pub enum TokenType {
    At,
    CloseBrace,
    CloseBrackets,
    CloseParenthesis,
    Colon,
    Comma,
    Dedent,
    Dot,
    Eof,
    Ellipsis,
    Id(String),
    Ident,
    Invalid(char),
    Keyword(KeywordType),
    NewLine,
    Number(String), // TODO: maybe convert to an actual number in the Lexer
    OpenBrace,
    OpenBrackets,
    OpenParenthesis,
    Operator(OperatorType),
    SemiColon,
    /// These are only keywords under specific contexts
    SoftKeyword(SoftKeywordType),
    String(String),
    RightArrow,
}

#[derive(Debug, PartialEq, Eq)]
pub enum OperatorType {
    Assign,
    Asterisk,
    AsteriskEqual,
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
    BitwiseXOr,
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
    Modulus,
    ModulusEqual,
    NotEquals,
    Plus,
    PlusEqual,
}

#[derive(Debug, PartialEq, Eq)]
pub enum KeywordType {
    And,
    As,
    Assert,
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

#[derive(Debug, PartialEq, Eq)]
pub enum SoftKeywordType {
    Match,
    Case,
    /// The char "_"
    Underscore,
}
