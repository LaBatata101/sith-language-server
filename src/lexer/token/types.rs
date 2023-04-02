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
    Number(NumberType, Cow<'a, str>),
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

impl OperatorType {
    pub fn as_str(&self) -> &str {
        match self {
            OperatorType::Assign => "=",
            OperatorType::Asterisk => "*",
            OperatorType::AsteriskEqual => "*=",
            OperatorType::At => "@",
            OperatorType::AtEqual => "@=",
            OperatorType::BitwiseAnd => "&",
            OperatorType::BitwiseAndEqual => "&=",
            OperatorType::BitwiseLeftShift => "<<",
            OperatorType::BitwiseLeftShiftEqual => "<<=",
            OperatorType::BitwiseNot => "~",
            OperatorType::BitwiseNotEqual => "~=",
            OperatorType::BitwiseOr => "|",
            OperatorType::BitwiseOrEqual => "|=",
            OperatorType::BitwiseRightShift => ">>",
            OperatorType::BitwiseRightShiftEqual => ">>=",
            OperatorType::BitwiseXOR => "^",
            OperatorType::BitwiseXOrEqual => "^=",
            OperatorType::ColonEqual => ":=",
            OperatorType::Divide => "/",
            OperatorType::DivideEqual => "/=",
            OperatorType::Equals => "==",
            OperatorType::Exponent => "**",
            OperatorType::ExponentEqual => "**=",
            OperatorType::FloorDivision => "//",
            OperatorType::FloorDivisionEqual => "//=",
            OperatorType::GreaterThan => ">",
            OperatorType::GreaterThanOrEqual => ">=",
            OperatorType::LessThan => "<",
            OperatorType::LessThanOrEqual => "<=",
            OperatorType::Minus => "-",
            OperatorType::MinusEqual => "-=",
            OperatorType::Modulo => "%",
            OperatorType::ModuloEqual => "%=",
            OperatorType::NotEquals => "!=",
            OperatorType::Plus => "+",
            OperatorType::PlusEqual => "+=",
        }
    }
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

impl KeywordType {
    pub fn as_str(&self) -> &str {
        match self {
            KeywordType::And => "and",
            KeywordType::As => "as",
            KeywordType::Async => "async",
            KeywordType::Assert => "assert",
            KeywordType::Await => "await",
            KeywordType::Break => "break",
            KeywordType::Class => "class",
            KeywordType::Continue => "continue",
            KeywordType::Def => "def",
            KeywordType::Del => "del",
            KeywordType::Elif => "elif",
            KeywordType::Else => "else",
            KeywordType::Except => "except",
            KeywordType::True => "True",
            KeywordType::False => "False",
            KeywordType::Finally => "finally",
            KeywordType::For => "for",
            KeywordType::From => "from",
            KeywordType::Global => "global",
            KeywordType::If => "if",
            KeywordType::Import => "import",
            KeywordType::In => "in",
            KeywordType::Is => "is",
            KeywordType::Lambda => "lambda",
            KeywordType::None => "None",
            KeywordType::NonLocal => "nonlocal",
            KeywordType::Not => "not",
            KeywordType::Or => "or",
            KeywordType::Pass => "pass",
            KeywordType::Raise => "raise",
            KeywordType::Return => "return",
            KeywordType::Try => "try",
            KeywordType::While => "while",
            KeywordType::With => "with",
            KeywordType::Yield => "yield",
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SoftKeywordType {
    Match,
    Case,
    /// The char "_"
    Underscore,
}

impl SoftKeywordType {
    pub fn as_str(&self) -> &str {
        match self {
            SoftKeywordType::Match => "match",
            SoftKeywordType::Case => "case",
            SoftKeywordType::Underscore => "_",
        }
    }
}
