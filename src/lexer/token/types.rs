#[derive(Debug, PartialEq, Eq)]
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
    Id(&'a str),
    Indent,
    Invalid(char),
    Keyword(KeywordType),
    NewLine,
    Number(NumberType<'a>),
    OpenBrace,
    OpenBrackets,
    OpenParenthesis,
    Operator(OperatorType),
    SemiColon,
    String(&'a str),
    RightArrow,
    /// These are only keywords under specific contexts
    SoftKeyword(SoftKeywordType),
}

#[derive(Debug, PartialEq, Eq)]
pub enum NumberType<'a> {
    Float(&'a str),
    Integer(IntegerType<'a>),
    Imaginary(&'a str),
    Invalid,
}

#[derive(Debug, PartialEq, Eq)]
pub enum IntegerType<'a> {
    Hex(&'a str),
    Binary(&'a str),
    Octal(&'a str),
    Decimal(&'a str),
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
    Invalid,
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

impl SoftKeywordType {
    pub fn as_str(&self) -> &str {
        match self {
            SoftKeywordType::Match => "match",
            SoftKeywordType::Case => "case",
            SoftKeywordType::Underscore => "_",
        }
    }
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
            OperatorType::Invalid => "OP INVALID",
        }
    }
}

impl<'a> IntegerType<'a> {
    pub fn as_str(&self) -> &str {
        match self {
            IntegerType::Hex(hex) => hex,
            IntegerType::Binary(binary) => binary,
            IntegerType::Octal(octal) => octal,
            IntegerType::Decimal(decimal) => decimal,
        }
    }
}

impl<'a> NumberType<'a> {
    pub fn as_str(&self) -> &str {
        match self {
            NumberType::Float(float) => float,
            NumberType::Integer(int) => int.as_str(),
            NumberType::Imaginary(imaginary) => imaginary,
            NumberType::Invalid => "INVALID NUMBER",
        }
    }
}

impl<'a> TokenType<'a> {
    pub fn as_str(&self) -> &str {
        match self {
            TokenType::OpenBrace => "{",
            TokenType::OpenBrackets => "[",
            TokenType::OpenParenthesis => "(",
            TokenType::CloseBrace => "}",
            TokenType::CloseBrackets => "]",
            TokenType::CloseParenthesis => ")",
            TokenType::Colon => ":",
            TokenType::Comma => ",",
            TokenType::SemiColon => ";",
            TokenType::Dot => ".",
            TokenType::Eof => "EOF",
            TokenType::Ellipsis => "...",
            TokenType::RightArrow => "->",
            TokenType::Dedent => "",
            TokenType::Indent => "    ",
            TokenType::NewLine => "\n",
            TokenType::Invalid(_) => "INVALID TOKEN",
            TokenType::Id(name) => name,
            TokenType::String(str) => str,
            TokenType::Number(number) => number.as_str(),
            TokenType::Keyword(keyword) => keyword.as_str(),
            TokenType::Operator(operator) => operator.as_str(),
            TokenType::SoftKeyword(soft_keyword) => soft_keyword.as_str(),
        }
    }
}
