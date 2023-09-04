#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    CloseBrace,
    CloseBracket,
    CloseParenthesis,
    Colon,
    Comma,
    Dedent,
    Dot,
    Eof,
    Ellipsis,
    Id,
    Indent,
    Invalid,
    Keyword(KeywordKind),
    NewLine,
    Number(NumberKind),
    OpenBrace,
    OpenBracket,
    OpenParenthesis,
    Operator(OperatorKind),
    SemiColon,
    String {
        kind: StringKind,
        is_triple_quote: bool,
    },
    RightArrow,
    /// These are only keywords under specific contexts
    SoftKeyword(SoftKeywordKind),
    NonLogicalNewline,
    Comment,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum NumberKind {
    Binary,
    Decimal,
    Hex,
    Octal,
    Float,
    Complex,
}

impl TokenKind {
    pub fn is_start_of_expr(&self) -> bool {
        // Tokens that can be an expression or the start of one.
        matches!(
            self,
            Self::Id
                | Self::Number(_)
                | Self::Operator(
                    OperatorKind::Plus
                        | OperatorKind::Minus
                        | OperatorKind::Asterisk
                        | OperatorKind::Exponent
                        | OperatorKind::BitwiseNot
                )
                | Self::OpenBrace
                | Self::OpenParenthesis
                | Self::OpenBracket
                | Self::String { .. }
                | Self::Ellipsis
                | Self::Keyword(
                    KeywordKind::Lambda
                        | KeywordKind::Await
                        | KeywordKind::False
                        | KeywordKind::None
                        | KeywordKind::Not
                        | KeywordKind::True
                        | KeywordKind::Yield
                )
        )
    }

    pub fn is_end_of_expr(&self) -> bool {
        // Tokens that can come after an expression
        matches!(
            self,
            TokenKind::NewLine
                | TokenKind::SemiColon
                | TokenKind::Colon
                | TokenKind::Eof
                | TokenKind::CloseBrace
                | TokenKind::CloseBracket
                | TokenKind::CloseParenthesis
                | TokenKind::Comma
                | TokenKind::Dedent
                | TokenKind::Keyword(KeywordKind::Else)
                | TokenKind::Keyword(KeywordKind::As)
                | TokenKind::Keyword(KeywordKind::From)
                | TokenKind::Keyword(KeywordKind::For)
                | TokenKind::Keyword(KeywordKind::Async)
        )
    }

    pub fn is_bool_op(&self) -> bool {
        matches!(self, TokenKind::Keyword(KeywordKind::And | KeywordKind::Or))
    }

    /// Doesn't check for the `not in` and `is not` operator.
    pub fn is_compare_op(&self) -> bool {
        matches!(
            self,
            TokenKind::Keyword(KeywordKind::Not | KeywordKind::In | KeywordKind::Is)
                | TokenKind::Operator(
                    OperatorKind::Equals
                        | OperatorKind::NotEquals
                        | OperatorKind::LessThan
                        | OperatorKind::LessThanOrEqual
                        | OperatorKind::GreaterThan
                        | OperatorKind::GreaterThanOrEqual
                )
        )
    }

    pub fn is_str(&self) -> bool {
        matches!(self, TokenKind::String { .. })
    }

    pub fn is_simple_stmt(&self) -> bool {
        !self.is_compound_stmt() && !matches!(self, TokenKind::Eof | TokenKind::Comma | TokenKind::NewLine)
    }

    pub fn is_compound_stmt(&self) -> bool {
        matches!(
            self,
            TokenKind::SoftKeyword(SoftKeywordKind::Match)
                | TokenKind::Keyword(
                    KeywordKind::If
                        | KeywordKind::Else
                        | KeywordKind::Elif
                        | KeywordKind::With
                        | KeywordKind::While
                        | KeywordKind::For
                        | KeywordKind::Try
                        | KeywordKind::Def
                        | KeywordKind::Class
                        | KeywordKind::Async,
                )
        )
    }
}

/// The kind of string literal as described in the [String and Bytes literals]
/// section of the Python reference.
///
/// [String and Bytes literals]: https://docs.python.org/3/reference/lexical_analysis.html#string-and-bytes-literals
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum StringKind {
    /// A normal string literal with no prefix.
    String,
    /// A f-string literal, with a `f` or `F` prefix.
    FString,
    /// A byte string literal, with a `b` or `B` prefix.
    Bytes,
    /// A raw string literal, with a `r` or `R` prefix.
    RawString,
    /// A raw f-string literal, with a `rf`/`fr` or `rF`/`Fr` or `Rf`/`fR` or `RF`/`FR` prefix.
    RawFString,
    /// A raw byte string literal, with a `rb`/`br` or `rB`/`Br` or `Rb`/`bR` or `RB`/`BR` prefix.
    RawBytes,
    /// A unicode string literal, with a `u` or `U` prefix.
    Unicode,
}

impl StringKind {
    pub fn size(&self) -> usize {
        match self {
            Self::String => 0,
            Self::FString | Self::Bytes | Self::Unicode | Self::RawString => 1,
            Self::RawFString | Self::RawBytes => 2,
        }
    }
}

impl TryFrom<char> for StringKind {
    type Error = String;

    fn try_from(ch: char) -> Result<Self, String> {
        match ch {
            'r' | 'R' => Ok(StringKind::RawString),
            'f' | 'F' => Ok(StringKind::FString),
            'u' | 'U' => Ok(StringKind::Unicode),
            'b' | 'B' => Ok(StringKind::Bytes),
            c => Err(format!("Unexpected string prefix: {c}")),
        }
    }
}

impl TryFrom<[char; 2]> for StringKind {
    type Error = String;

    fn try_from(chars: [char; 2]) -> Result<Self, String> {
        match chars {
            ['r' | 'R', 'f' | 'F'] => Ok(StringKind::RawFString),
            ['f' | 'F', 'r' | 'R'] => Ok(StringKind::RawFString),
            ['r' | 'R', 'b' | 'B'] => Ok(StringKind::RawBytes),
            ['b' | 'B', 'r' | 'R'] => Ok(StringKind::RawBytes),
            [c1, c2] => Err(format!("Unexpected string prefix: {c1}{c2}")),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum OperatorKind {
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
    BitwiseXor,
    BitwiseXOREqual,
    ColonEqual,
    Slash,
    DivideEqual,
    Equals,
    Exponent,
    ExponentEqual,
    DoubleSlash,
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
    Invalid,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum KeywordKind {
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SoftKeywordKind {
    Match,
    Case,
    Type,
}

impl SoftKeywordKind {
    pub fn as_str(&self) -> &str {
        match self {
            SoftKeywordKind::Match => "match",
            SoftKeywordKind::Case => "case",
            SoftKeywordKind::Type => "type",
        }
    }
}

impl KeywordKind {
    pub fn as_str(&self) -> &str {
        match self {
            KeywordKind::And => "and",
            KeywordKind::As => "as",
            KeywordKind::Async => "async",
            KeywordKind::Assert => "assert",
            KeywordKind::Await => "await",
            KeywordKind::Break => "break",
            KeywordKind::Class => "class",
            KeywordKind::Continue => "continue",
            KeywordKind::Def => "def",
            KeywordKind::Del => "del",
            KeywordKind::Elif => "elif",
            KeywordKind::Else => "else",
            KeywordKind::Except => "except",
            KeywordKind::True => "True",
            KeywordKind::False => "False",
            KeywordKind::Finally => "finally",
            KeywordKind::For => "for",
            KeywordKind::From => "from",
            KeywordKind::Global => "global",
            KeywordKind::If => "if",
            KeywordKind::Import => "import",
            KeywordKind::In => "in",
            KeywordKind::Is => "is",
            KeywordKind::Lambda => "lambda",
            KeywordKind::None => "None",
            KeywordKind::NonLocal => "nonlocal",
            KeywordKind::Not => "not",
            KeywordKind::Or => "or",
            KeywordKind::Pass => "pass",
            KeywordKind::Raise => "raise",
            KeywordKind::Return => "return",
            KeywordKind::Try => "try",
            KeywordKind::While => "while",
            KeywordKind::With => "with",
            KeywordKind::Yield => "yield",
        }
    }
}

impl OperatorKind {
    pub fn as_str(&self) -> &str {
        match self {
            OperatorKind::Assign => "=",
            OperatorKind::Asterisk => "*",
            OperatorKind::AsteriskEqual => "*=",
            OperatorKind::At => "@",
            OperatorKind::AtEqual => "@=",
            OperatorKind::BitwiseAnd => "&",
            OperatorKind::BitwiseAndEqual => "&=",
            OperatorKind::BitwiseLeftShift => "<<",
            OperatorKind::BitwiseLeftShiftEqual => "<<=",
            OperatorKind::BitwiseNot => "~",
            OperatorKind::BitwiseNotEqual => "~=",
            OperatorKind::BitwiseOr => "|",
            OperatorKind::BitwiseOrEqual => "|=",
            OperatorKind::BitwiseRightShift => ">>",
            OperatorKind::BitwiseRightShiftEqual => ">>=",
            OperatorKind::BitwiseXor => "^",
            OperatorKind::BitwiseXOREqual => "^=",
            OperatorKind::ColonEqual => ":=",
            OperatorKind::Slash => "/",
            OperatorKind::DivideEqual => "/=",
            OperatorKind::Equals => "==",
            OperatorKind::Exponent => "**",
            OperatorKind::ExponentEqual => "**=",
            OperatorKind::DoubleSlash => "//",
            OperatorKind::FloorDivisionEqual => "//=",
            OperatorKind::GreaterThan => ">",
            OperatorKind::GreaterThanOrEqual => ">=",
            OperatorKind::LessThan => "<",
            OperatorKind::LessThanOrEqual => "<=",
            OperatorKind::Minus => "-",
            OperatorKind::MinusEqual => "-=",
            OperatorKind::Modulus => "%",
            OperatorKind::ModulusEqual => "%=",
            OperatorKind::NotEquals => "!=",
            OperatorKind::Plus => "+",
            OperatorKind::PlusEqual => "+=",
            OperatorKind::Invalid => "OP INVALID",
        }
    }
}
