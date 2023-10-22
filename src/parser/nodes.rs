use std::borrow::Cow;

use ruff_text_size::TextRange;

use crate::lexer::types::{KeywordKind, OperatorKind, TokenKind};

#[derive(Debug)]
pub struct Module<'a> {
    pub body: Vec<Statement<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement<'a> {
    Assign(AssignStmt<'a>),
    AugAssign(AugAssignStmt<'a>),
    AnnAssign(AnnAssignStmt<'a>),
    Assert(AssertStmt<'a>),
    AsyncFor(ForStmt<'a>),
    AsyncFunctionDef(FunctionDefStmt<'a>),
    AsyncWith(WithStmt<'a>),
    Break(BreakStmt),
    Class(ClassDefStmt<'a>),
    Continue(ContinueStmt),
    Del(DelStmt<'a>),
    Expression(ExpressionStmt<'a>),
    For(ForStmt<'a>),
    ImportFrom(ImportFromStmt<'a>),
    FunctionDef(FunctionDefStmt<'a>),
    Global(GlobalStmt<'a>),
    If(IfStmt<'a>),
    Import(ImportStmt<'a>),
    Match(MatchStmt<'a>),
    NonLocal(NonLocalStmt<'a>),
    Pass(PassStmt),
    Raise(RaiseStmt<'a>),
    Return(ReturnStmt<'a>),
    Try(TryStmt<'a>),
    While(WhileStmt<'a>),
    With(WithStmt<'a>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression<'a> {
    Attribute(AttributeExpr<'a>),
    Await(AwaitExpr<'a>),
    BinaryOp(BinaryOpExpr<'a>),
    BoolOp(BoolOpExpr<'a>),
    Call(CallExpr<'a>),
    Compare(CompareExpr<'a>),
    Dict(DictExpr<'a>),
    DictComp(DictCompExpr<'a>),
    Ellipsis(EllipsisExpr),
    FString(FStringExpr<'a>),
    Generator(GeneratorExpr<'a>),
    Id(IdExpr<'a>),
    IfElse(IfElseExpr<'a>),
    Invalid(TextRange),
    Lambda(LambdaExpr<'a>),
    List(ListExpr<'a>),
    ListComp(ListCompExpr<'a>),
    Literal(LiteralExpr<'a>),
    Set(SetExpr<'a>),
    SetComp(SetCompExpr<'a>),
    Slice(SliceExpr<'a>),
    Subscript(SubscriptExpr<'a>),
    Starred(StarredExpr<'a>),
    Tuple(TupleExpr<'a>),
    UnaryOp(UnaryOpExpr<'a>),
    Yield(YieldExpr<'a>),
    YieldFrom(YieldFromExpr<'a>),
    Named(NamedExpr<'a>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct LambdaExpr<'a> {
    pub parameters: Option<Box<Parameters<'a>>>,
    pub body: Box<Expression<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ClassDefStmt<'a> {
    pub decorators: Vec<Decorator<'a>>,
    pub name: MaybeIdentifier<'a>,
    pub arguments: Option<Box<Arguments<'a>>>,
    pub body: Vec<Statement<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct IfStmt<'a> {
    pub test: Box<Expression<'a>>,
    pub body: Vec<Statement<'a>>,
    pub elif_else_clauses: Vec<ElifElseClause<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ElifElseClause<'a> {
    pub test: Option<Expression<'a>>,
    pub body: Vec<Statement<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct WhileStmt<'a> {
    pub test: Box<Expression<'a>>,
    pub body: Vec<Statement<'a>>,
    pub orelse: Vec<Statement<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct AssignStmt<'a> {
    pub targets: Vec<Expression<'a>>,
    pub value: Box<Expression<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct AugAssignStmt<'a> {
    pub target: Box<Expression<'a>>,
    pub op: Operator,
    pub value: Box<Expression<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Operator {
    Add,
    Sub,
    Mult,
    MatMult,
    Div,
    Mod,
    Pow,
    LShift,
    RShift,
    BitOr,
    BitXor,
    BitAnd,
    FloorDiv,
}

impl TryFrom<TokenKind> for Operator {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        Ok(match value {
            TokenKind::Operator(OperatorKind::PlusEqual) => Self::Add,
            TokenKind::Operator(OperatorKind::MinusEqual) => Self::Sub,
            TokenKind::Operator(OperatorKind::AtEqual) => Self::MatMult,
            TokenKind::Operator(OperatorKind::DivideEqual) => Self::Div,
            TokenKind::Operator(OperatorKind::ModulusEqual) => Self::Mod,
            TokenKind::Operator(OperatorKind::ExponentEqual) => Self::Pow,
            TokenKind::Operator(OperatorKind::AsteriskEqual) => Self::Mult,
            TokenKind::Operator(OperatorKind::BitwiseOrEqual) => Self::BitOr,
            TokenKind::Operator(OperatorKind::BitwiseXOREqual) => Self::BitXor,
            TokenKind::Operator(OperatorKind::BitwiseAndEqual) => Self::BitAnd,
            TokenKind::Operator(OperatorKind::FloorDivisionEqual) => Self::FloorDiv,
            TokenKind::Operator(OperatorKind::BitwiseLeftShiftEqual) => Self::LShift,
            TokenKind::Operator(OperatorKind::BitwiseRightShiftEqual) => Self::RShift,
            _ => return Err(()),
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct AnnAssignStmt<'a> {
    pub target: Box<Expression<'a>>,
    pub annotation: Box<Expression<'a>>,
    pub value: Option<Box<Expression<'a>>>,
    pub simple: bool,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Decorator<'a> {
    pub expr: Expression<'a>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionDefStmt<'a> {
    pub name: MaybeIdentifier<'a>,
    pub parameters: Box<Parameters<'a>>,
    pub body: Vec<Statement<'a>>,
    pub decorators: Vec<Decorator<'a>>,
    pub returns: Option<Box<Expression<'a>>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct IfElseExpr<'a> {
    pub body: Box<Expression<'a>>,
    pub test: Box<Expression<'a>>,
    pub orelse: Box<Expression<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Alias<'a> {
    pub name: Identifier<'a>,
    pub asname: Option<MaybeIdentifier<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ImportStmt<'a> {
    pub names: Vec<Alias<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ImportFromStmt<'a> {
    pub module: Option<Identifier<'a>>,
    pub names: Vec<Alias<'a>>,
    pub level: u32,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct WithStmt<'a> {
    pub items: Vec<WithItem<'a>>,
    pub body: Vec<Statement<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct WithItem<'a> {
    pub item: Expression<'a>,
    pub target: Option<Expression<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ExceptHandler<'a> {
    pub ty: Option<Box<Expression<'a>>>,
    pub name: Option<MaybeIdentifier<'a>>,
    pub body: Vec<Statement<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TryStmt<'a> {
    pub body: Vec<Statement<'a>>,
    pub handlers: Vec<ExceptHandler<'a>>,
    pub orelse: Vec<Statement<'a>>,
    pub final_body: Vec<Statement<'a>>,
    pub is_star: bool,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ReturnStmt<'a> {
    pub value: Option<Box<Expression<'a>>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ForStmt<'a> {
    pub target: Box<Expression<'a>>,
    pub iter: Box<Expression<'a>>,
    pub body: Vec<Statement<'a>>,
    pub orelse: Vec<Statement<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct RaiseStmt<'a> {
    pub exc: Option<Box<Expression<'a>>>,
    pub cause: Option<Box<Expression<'a>>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct DelStmt<'a> {
    pub targets: Vec<Expression<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CallExpr<'a> {
    pub func: Box<Expression<'a>>,
    pub args: Arguments<'a>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Arguments<'a> {
    pub args: Vec<Expression<'a>>,
    pub kw_args: Vec<KeywordArg<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct KeywordArg<'a> {
    pub arg: Option<MaybeIdentifier<'a>>,
    pub value: Box<Expression<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SubscriptExpr<'a> {
    pub value: Box<Expression<'a>>,
    pub slice: Box<Expression<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct SliceExpr<'a> {
    pub lower: Option<Box<Expression<'a>>>,
    pub upper: Option<Box<Expression<'a>>>,
    pub step: Option<Box<Expression<'a>>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ListCompExpr<'a> {
    pub element: Box<Expression<'a>>,
    pub generators: Vec<Comprehension<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct GeneratorExpr<'a> {
    pub element: Box<Expression<'a>>,
    pub generators: Vec<Comprehension<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Comprehension<'a> {
    pub target: Expression<'a>,
    pub iter: Expression<'a>,
    pub ifs: Vec<Expression<'a>>,
    pub is_async: bool,
    pub range: TextRange,
}

/// The "if" that goes inside a comprehension e.g.: [i for i in range(10) if i % 2 == 0]
#[derive(Debug, PartialEq, Eq)]
pub struct IfComp<'a> {
    pub cond: Expression<'a>,
    pub range: TextRange,
}

/// The "for" that goes inside a comprehension e.g.: [i for i in range(10)]
#[derive(Debug, PartialEq, Eq)]
pub struct ForComp<'a> {
    pub target: Expression<'a>,
    pub iter: Expression<'a>,
    pub is_async: bool,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct AssertStmt<'a> {
    pub test: Box<Expression<'a>>,
    pub message: Option<Box<Expression<'a>>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct GlobalStmt<'a> {
    pub names: Vec<MaybeIdentifier<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct NonLocalStmt<'a> {
    pub names: Vec<MaybeIdentifier<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SetCompExpr<'a> {
    pub element: Box<Expression<'a>>,
    pub generators: Vec<Comprehension<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct DictCompExpr<'a> {
    pub key: Box<Expression<'a>>,
    pub value: Box<Expression<'a>>,
    pub generators: Vec<Comprehension<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct MatchStmt<'a> {
    pub subject: Box<Expression<'a>>,
    pub cases: Vec<MatchCase<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct MatchCase<'a> {
    pub pattern: Pattern<'a>,
    pub guard: Option<Box<Expression<'a>>>,
    pub body: Vec<Statement<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Pattern<'a> {
    Invalid,
    MatchAs(PatternMatchAs<'a>),
    MatchOr(PatternMatchOr<'a>),
    MatchStar(PatternMatchStar<'a>),
    MatchClass(PatternMatchClass<'a>),
    MatchValue(PatternMatchValue<'a>),
    MatchMapping(PatternMatchMapping<'a>),
    MatchSequence(PatternMatchSequence<'a>),
    MatchSingleton(PatternMatchSingleton<'a>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct PatternMatchValue<'a> {
    pub value: Box<Expression<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct PatternMatchAs<'a> {
    pub pattern: Option<Box<Pattern<'a>>>,
    pub name: Option<MaybeIdentifier<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct PatternMatchOr<'a> {
    pub patterns: Vec<Pattern<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct PatternMatchClass<'a> {
    pub cls: Box<Expression<'a>>,
    pub arguments: PatternArguments<'a>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct PatternArguments<'a> {
    pub patterns: Vec<Pattern<'a>>,
    pub keywords: Vec<PatternKeyword<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct PatternKeyword<'a> {
    pub attr: MaybeIdentifier<'a>,
    pub pattern: Pattern<'a>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct PatternMatchStar<'a> {
    pub value: Option<MaybeIdentifier<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct PatternMatchSequence<'a> {
    pub patterns: Vec<Pattern<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct PatternMatchMapping<'a> {
    pub keys: Vec<Expression<'a>>,
    pub patterns: Vec<Pattern<'a>>,
    pub rest: Option<MaybeIdentifier<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct PatternMatchSingleton<'a> {
    pub value: Literal<'a>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub enum MaybeIdentifier<'a> {
    Valid(Identifier<'a>),
    Invalid(TextRange),
}

impl<'a> MaybeIdentifier<'a> {
    pub fn into_id(self) -> Option<Identifier<'a>> {
        match self {
            MaybeIdentifier::Valid(ident) => Some(ident),
            MaybeIdentifier::Invalid(_) => None,
        }
    }

    pub fn as_id(&self) -> Option<&Identifier<'a>> {
        match self {
            MaybeIdentifier::Valid(ident) => Some(ident),
            MaybeIdentifier::Invalid(_) => None,
        }
    }

    pub fn range(&self) -> TextRange {
        match self {
            MaybeIdentifier::Valid(Identifier { range, .. }) | MaybeIdentifier::Invalid(range) => *range,
        }
    }

    pub fn is_valid_and(&self, f: impl Fn(&Identifier<'a>) -> bool) -> bool {
        match self {
            MaybeIdentifier::Valid(ident) => f(ident),
            MaybeIdentifier::Invalid(_) => false,
        }
    }

    pub fn is_valid(&self) -> bool {
        matches!(self, MaybeIdentifier::Valid(_))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Identifier<'a> {
    pub id: &'a str,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct PassStmt {
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ExpressionStmt<'a> {
    pub value: Box<Expression<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct BreakStmt {
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct ContinueStmt {
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct AwaitExpr<'a> {
    pub value: Box<Expression<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Literal<'a> {
    None,
    Bool(BoolValue),
    String(Cow<'a, str>),
    Bytes(Cow<'a, str>),
    Float(&'a str),
    Complex(&'a str),
    Int(&'a str),
}

#[derive(Debug, PartialEq, Eq)]
pub struct LiteralExpr<'a> {
    pub value: Literal<'a>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct EllipsisExpr {
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ListExpr<'a> {
    pub elements: Vec<Expression<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct DictExpr<'a> {
    pub keys: Vec<Option<Expression<'a>>>,
    pub values: Vec<Expression<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SetExpr<'a> {
    pub elements: Vec<Expression<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TupleExpr<'a> {
    pub elements: Vec<Expression<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct YieldExpr<'a> {
    pub value: Option<Box<Expression<'a>>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct YieldFromExpr<'a> {
    pub value: Box<Expression<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BoolValue {
    True,
    False,
}

#[derive(Debug, PartialEq, Eq)]
pub enum UnaryOp {
    // Logical `not` operator
    Not,
    // Unary `+` operator
    UAdd,
    // Unary `-` operator
    USub,
    // Bitwise not `~` operator
    Invert,
}

impl TryFrom<TokenKind> for UnaryOp {
    type Error = String;

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        Ok(match value {
            TokenKind::Operator(OperatorKind::Plus) => Self::UAdd,
            TokenKind::Operator(OperatorKind::Minus) => Self::USub,
            TokenKind::Operator(OperatorKind::BitwiseNot) => Self::Invert,
            TokenKind::Keyword(KeywordKind::Not) => Self::Not,
            _ => return Err(format!("invalid operator {:?}", value)),
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinaryOp {
    /// Addition (`+`)
    Add,
    /// Subtraction (`-`)
    Sub,
    /// Multiplication (`*`)
    Mult,
    /// Matrix Multiplication (`@`)
    MatMult,
    /// Division (`/`)
    Div,
    /// Floor Division (`//`)
    FloorDiv,
    /// Modulos (`%`)
    Mod,
    /// Exponentiation (`**`)
    Pow,
    /// Bitwise Left Shift (`<<`)
    LShift,
    /// Bitwise Right Shift (`>>`)
    RShift,
    /// Bitwise Or (`|`)
    BitOr,
    /// Bitwise Xor (`^`)
    BitXor,
    /// Bitwise And (`&`)
    BitAnd,
}

impl TryFrom<TokenKind> for BinaryOp {
    type Error = String;

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        Ok(match value {
            TokenKind::Operator(OperatorKind::At) => Self::MatMult,
            TokenKind::Operator(OperatorKind::Plus) => Self::Add,
            TokenKind::Operator(OperatorKind::Minus) => Self::Sub,
            TokenKind::Operator(OperatorKind::Slash) => Self::Div,
            TokenKind::Operator(OperatorKind::DoubleSlash) => Self::FloorDiv,
            TokenKind::Operator(OperatorKind::Asterisk) => Self::Mult,
            TokenKind::Operator(OperatorKind::Modulus) => Self::Mod,
            TokenKind::Operator(OperatorKind::Exponent) => Self::Pow,
            TokenKind::Operator(OperatorKind::BitwiseLeftShift) => Self::LShift,
            TokenKind::Operator(OperatorKind::BitwiseRightShift) => Self::RShift,
            TokenKind::Operator(OperatorKind::BitwiseOr) => Self::BitOr,
            TokenKind::Operator(OperatorKind::BitwiseXor) => Self::BitXor,
            TokenKind::Operator(OperatorKind::BitwiseAnd) => Self::BitAnd,
            _ => return Err(format!("invalid operator {:?}", value)),
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BinaryOpExpr<'a> {
    pub left: Box<Expression<'a>>,
    pub op: BinaryOp,
    pub right: Box<Expression<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct UnaryOpExpr<'a> {
    pub op: UnaryOp,
    pub operand: Box<Expression<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct AttributeExpr<'a> {
    pub value: Box<Expression<'a>>,
    pub attr: MaybeIdentifier<'a>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BoolOp {
    And,
    Or,
}

impl TryFrom<TokenKind> for BoolOp {
    type Error = String;

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        Ok(match value {
            TokenKind::Keyword(KeywordKind::And) => Self::And,
            TokenKind::Keyword(KeywordKind::Or) => Self::Or,
            _ => return Err(format!("invalid bool operator {:?}", value)),
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BoolOpExpr<'a> {
    pub op: BoolOp,
    pub values: Vec<Expression<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CompareExpr<'a> {
    pub left: Box<Expression<'a>>,
    pub ops: Vec<CompareOp>,
    pub comparators: Vec<Expression<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum CompareOp {
    Eq,
    NotEq,
    Lt,
    LtE,
    Gt,
    GtE,
    Is,
    IsNot,
    In,
    NotIn,
}

impl TryFrom<[TokenKind; 2]> for CompareOp {
    type Error = ();

    fn try_from(value: [TokenKind; 2]) -> Result<Self, Self::Error> {
        Ok(match value {
            [TokenKind::Keyword(KeywordKind::Is), TokenKind::Keyword(KeywordKind::Not)] => Self::IsNot,
            [TokenKind::Keyword(KeywordKind::Is), _] => Self::Is,
            [TokenKind::Keyword(KeywordKind::In), _] => Self::In,
            [TokenKind::Operator(OperatorKind::Equals), _] => Self::Eq,
            [TokenKind::Operator(OperatorKind::LessThan), _] => Self::Lt,
            [TokenKind::Operator(OperatorKind::GreaterThan), _] => Self::Gt,
            [TokenKind::Operator(OperatorKind::NotEquals), _] => Self::NotEq,
            [TokenKind::Operator(OperatorKind::LessThanOrEqual), _] => Self::LtE,
            [TokenKind::Operator(OperatorKind::GreaterThanOrEqual), _] => Self::GtE,
            [TokenKind::Keyword(KeywordKind::Not), TokenKind::Keyword(KeywordKind::In)] => Self::NotIn,
            _ => return Err(()),
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct StarredExpr<'a> {
    pub value: Box<Expression<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Parameter<'a> {
    pub name: MaybeIdentifier<'a>,
    pub annotation: Option<Box<Expression<'a>>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Parameters<'a> {
    pub kwarg: Option<Box<Parameter<'a>>>,
    pub vararg: Option<Box<Parameter<'a>>>,
    pub args: Vec<ParameterWithDefault<'a>>,
    pub posonlyargs: Vec<ParameterWithDefault<'a>>,
    pub kwonlyargs: Vec<ParameterWithDefault<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParameterWithDefault<'a> {
    pub parameter: Parameter<'a>,
    pub default: Option<Box<Expression<'a>>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct NamedExpr<'a> {
    pub target: Box<Expression<'a>>,
    pub value: Box<Expression<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Context {
    Load,
    Store,
    Del,
}

#[derive(Debug, PartialEq, Eq)]
pub struct IdExpr<'a> {
    pub id: &'a str,
    pub ctx: Context,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FStringExpr<'a> {
    pub values: Vec<Expression<'a>>,
    pub range: TextRange,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FormattedValueExpr<'a> {
    pub value: Box<Expression<'a>>,
    pub debug_text: Option<DebugText>,
    pub conversion: ConversionFlag,
    pub format_spec: Option<Box<Expression<'a>>>,
    pub range: TextRange,
}

#[repr(i8)]
#[derive(Debug, PartialEq, Eq)]
pub enum ConversionFlag {
    /// No conversion
    None = -1, // CPython uses -1
    /// Converts by calling `str(<value>)`.
    Str = b's' as i8,
    /// Converts by calling `ascii(<value>)`.
    Ascii = b'a' as i8,
    /// Converts by calling `repr(<value>)`.
    Repr = b'r' as i8,
}

#[derive(Debug, PartialEq, Eq)]
pub struct DebugText {}
