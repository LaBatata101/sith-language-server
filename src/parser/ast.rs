use crate::lexer::token::Span;

#[derive(Debug, PartialEq, Eq)]
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
}

#[derive(Debug, PartialEq, Eq)]
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
    Lambda,
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
pub struct CallExpr {
    pub name: String,
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
    pub block: Block,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
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
