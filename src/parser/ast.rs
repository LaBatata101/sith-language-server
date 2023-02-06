use crate::lexer::token::Span;

#[derive(Debug, PartialEq, Eq, Default)]
pub enum Statement {
    Expression(Expression, Span),
    Block(Block),
    FunctionDef(Function),
    If(IfStmt),
    VarAsgmt(VarAsgmt, Expression),
    Pass(Span),
    While(While),
    Break(Span),
    Continue(Span),
    Class(ClassStmt),
    Import(ImportStmt),
    FromImport(FromImportStmt),
    With(WithStmt),
    Try(TryStmt),
    Return(ReturnStmt),
    Invalid(Span),

    #[default]
    None,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub enum Expression {
    String(String, Span),
    Number(String, Span),
    Bool(bool, Span),
    BinaryOp(Box<Expression>, BinaryOperator, Box<Expression>, Span),
    UnaryOp(Box<Expression>, UnaryOperator, Span),
    Id(String, Span),
    Call(Box<Expression>, Span),
    Slice(Box<Expression>, Box<Expression>, Span),
    List(Vec<Expression>, Span),
    Dict(Vec<DictItemType>, Span),
    Set(Vec<Expression>, Span),
    Tuple(Vec<Expression>, Span),
    IfElse(IfElseExpr),
    Lambda(LambdaExpr),
    Ellipsis(Span),
    Invalid(Span),
    None(Span),

    #[default]
    Empty,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    BitwiseAnd,
    BitwiseLeftShift,
    BitwiseOr,
    BitwiseRightShift,
    BitwiseXOR,
    Divide,
    Equals,
    Exponent,
    FloorDivision,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    LogicalAnd,
    LogicalOr,
    Modulo,
    Multiply,
    NotEqual,
    Subtract,
    At,
    In,
    NotIn,
    Is,
    IsNot,
    IfElse,
    Walrus,
    AttributeRef,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum UnaryOperator {
    Plus,
    Minus,
    BitwiseNot,
    LogicalNot,
    OpenParenthesis,
    OpenBrackets,
    UnpackIterable,
    UnpackDictionary,
    Await,
    Lambda,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Operation {
    Binary(BinaryOperator),
    Unary(UnaryOperator),
}

impl Operation {
    pub fn get_binary_op(&self) -> BinaryOperator {
        match self {
            Operation::Binary(op) => *op,
            op => panic!("Current Operation is not binary: {:?}", op),
        }
    }

    pub fn get_unary_op(&self) -> UnaryOperator {
        match self {
            Operation::Unary(op) => *op,
            op => panic!("Current Operation is not unary: {:?}", op),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum DictItemType {
    KeyValue(Expression, Expression),
    DictUnpack(Expression),
}

#[derive(Debug, PartialEq, Eq)]
pub enum StarParameterType {
    Kargs,
    KWargs,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub enum ExceptBlockKind {
    ExceptStar,
    #[default]
    Except,
}

#[derive(Debug, PartialEq, Eq)]
pub struct LambdaExpr {
    pub parameters: Vec<FuncParameter>,
    pub expression: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct FuncParameter {
    pub name: String,
    pub default_value: Option<Expression>,
    pub star_parameter_type: Option<StarParameterType>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CallExpr {
    pub name: String,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct ClassStmt {
    pub name: String,
    pub block: Block,
    pub super_classes: Vec<FuncParameter>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct IfStmt {
    pub condition: Expression,
    pub block: Block,
    pub elif_stms: Vec<ElIfStmt>,
    pub else_stmt: Option<ElseStmt>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ElIfStmt {
    pub condition: Expression,
    pub block: Block,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ElseStmt {
    pub block: Block,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct While {
    pub condition: Expression,
    pub else_stmt: Option<ElseStmt>,
    pub block: Block,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct VarAsgmt {
    name: String,
    span: Span,
}

impl VarAsgmt {
    pub fn new(name: String, span: Span) -> Self {
        Self { name, span }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Function {
    pub name: String,
    pub name_span: Span,
    pub parameters: Vec<FuncParameter>,
    pub block: Block,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct Block {
    pub stmts: Vec<Statement>,
    pub span: Span,
}

impl Block {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            stmts: Vec::new(),
            span: Span { start: 0, end: 0 },
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParsedFile {
    pub stmts: Vec<Statement>,
}

impl ParsedFile {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self { stmts: Vec::new() }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct IfElseExpr {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub condition: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct ImportStmt {
    pub modules: Vec<ImportModule>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct ImportModule {
    /// Store the module name and it's parents, e.g., "module1.module2.class.function" will become
    /// vec!["module1", "module2", "class", "function"]
    pub name: Vec<String>,
    pub alias: Option<String>,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct FromImportStmt {
    pub module: Vec<ImportModule>,
    pub targets: Vec<ImportModule>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct WithStmt {
    pub items: Vec<WithItem>,
    pub block: Block,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct WithItem {
    pub item: Expression,
    pub target: Option<Expression>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct TryStmt {
    pub block: Block,
    pub finally_block: Option<FinallyBlock>,
    pub except_blocks: Vec<ExceptBlock>,
    pub else_stmt: Option<ElseStmt>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct ExceptBlock {
    pub block: Block,
    pub kind: ExceptBlockKind,
    pub expr: Option<Expression>,
    pub expr_alias: Option<String>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct FinallyBlock {
    pub block: Block,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct ReturnStmt {
    pub value: Option<Expression>,
    pub span: Span,
}
