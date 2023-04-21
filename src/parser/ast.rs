use std::borrow::Cow;

use crate::lexer::span::Span;

#[derive(Debug, PartialEq, Eq, Default)]
pub enum Statement<'a> {
    Assert(AssertStmt<'a>),
    Expression(Expression<'a>),
    Block(Block<'a>),
    FunctionDef(Function<'a>),
    If(IfStmt<'a>),
    Pass(Span),
    While(While<'a>),
    Break(Span),
    Continue(Span),
    Class(ClassStmt<'a>),
    Import(ImportStmt<'a>),
    FromImport(FromImportStmt<'a>),
    With(WithStmt<'a>),
    Try(TryStmt<'a>),
    Return(ReturnStmt<'a>),
    Raise(RaiseStmt<'a>),
    For(ForStmt<'a>),
    Del(DelStmt<'a>),
    Global(GlobalStmt<'a>),
    NonLocal(NonLocalStmt<'a>),
    AsyncFunctionDef(Function<'a>),
    AsyncWith(WithStmt<'a>),
    AsyncFor(ForStmt<'a>),
    Invalid(Span),

    #[default]
    None,
}

impl<'a> Statement<'a> {
    pub fn span(&self) -> Span {
        match self {
            Statement::Pass(span) | Statement::Break(span) | Statement::Continue(span) | Statement::Invalid(span) => {
                *span
            }
            Statement::Expression(expr) => expr.span(),
            Statement::Block(block) => block.span,
            Statement::FunctionDef(func) => func.span,
            Statement::If(if_stmt) => if_stmt.span,
            Statement::While(while_stmt) => while_stmt.span,
            Statement::Class(class) => class.span,
            Statement::Import(import) => import.span,
            Statement::FromImport(from_import) => from_import.span,
            Statement::With(with) => with.span,
            Statement::Try(try_stmt) => try_stmt.span,
            Statement::Return(return_stmt) => return_stmt.span,
            Statement::For(for_stmt) => for_stmt.span,
            Statement::Raise(raise) => raise.span,
            Statement::Del(del) => del.span,
            Statement::Assert(assert) => assert.span,
            Statement::Global(global) => global.span,
            Statement::NonLocal(nonlocal) => nonlocal.span,
            Statement::AsyncFunctionDef(async_func) => async_func.span,
            Statement::AsyncWith(async_with) => async_with.span,
            Statement::AsyncFor(async_for) => async_for.span,
            Statement::None => Span::default(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Default)]
pub enum Expression<'a> {
    Assign(Assign<'a>),
    AugAssing(AugAssign<'a>),
    Await(Box<Expression<'a>>, Span),
    String(Cow<'a, str>, Span),
    Number(Cow<'a, str>, Span),
    Bool(bool, Span),
    BinaryOp(Box<Expression<'a>>, BinaryOperator, Box<Expression<'a>>, Span),
    UnaryOp(Box<Expression<'a>>, UnaryOperator, Span),
    Id(Cow<'a, str>, Span),
    Call(FunctionCall<'a>),
    Subscript(Subscript<'a>),
    List(Vec<Expression<'a>>, Span),
    ListComp(ListComp<'a>),
    GeneratorComp(GeneratorComp<'a>),
    Dict(Vec<DictItemType<'a>>, Span),
    DictComp(DictComp<'a>),
    Set(Vec<Expression<'a>>, Span),
    SetComp(SetComp<'a>),
    Tuple(Vec<Expression<'a>>, Span),
    IfElse(IfElseExpr<'a>),
    Lambda(LambdaExpr<'a>),
    Ellipsis(Span),
    Invalid(Span),
    Yield(Option<Box<Expression<'a>>>, Span),
    YieldFrom(Box<Expression<'a>>, Span),
    AnnAssign(AnnAssign<'a>),
    None(Span),

    #[default]
    Empty,
}

impl<'a> Expression<'a> {
    pub fn span(&self) -> Span {
        match self {
            Expression::Assign(assign) => assign.span,
            Expression::AugAssing(aug_assign) => aug_assign.span,
            Expression::AnnAssign(ann_assign) => ann_assign.span,
            Expression::IfElse(if_else) => if_else.span,
            Expression::Lambda(lambda) => lambda.span,
            Expression::Call(func_call) => func_call.span,
            Expression::Subscript(subscript) => subscript.span,
            Expression::ListComp(list_comp) => list_comp.span,
            Expression::GeneratorComp(gen_comp) => gen_comp.span,
            Expression::SetComp(set_comp) => set_comp.span,
            Expression::DictComp(dict_comp) => dict_comp.span,
            Expression::String(_, span)
            | Expression::Number(_, span)
            | Expression::Bool(_, span)
            | Expression::BinaryOp(_, _, _, span)
            | Expression::UnaryOp(_, _, span)
            | Expression::Id(_, span)
            | Expression::List(_, span)
            | Expression::Dict(_, span)
            | Expression::Set(_, span)
            | Expression::Tuple(_, span)
            | Expression::Ellipsis(span)
            | Expression::Invalid(span)
            | Expression::Yield(_, span)
            | Expression::YieldFrom(_, span)
            | Expression::Await(_, span)
            | Expression::None(span) => *span,
            Expression::Empty => Span::default(),
        }
    }

    pub fn set_span(&mut self, new_span: Span) {
        match self {
            Expression::Assign(assign) => assign.span = new_span,
            Expression::AugAssing(aug_assign) => aug_assign.span = new_span,
            Expression::AnnAssign(ann_assign) => ann_assign.span = new_span,
            Expression::IfElse(if_else) => if_else.span = new_span,
            Expression::Lambda(lambda) => lambda.span = new_span,
            Expression::Call(func_call) => func_call.span = new_span,
            Expression::Subscript(subscript) => subscript.span = new_span,
            Expression::ListComp(list_comp) => list_comp.span = new_span,
            Expression::GeneratorComp(gen_comp) => gen_comp.span = new_span,
            Expression::SetComp(set_comp) => set_comp.span = new_span,
            Expression::DictComp(dict_comp) => dict_comp.span = new_span,
            Expression::String(_, span)
            | Expression::Number(_, span)
            | Expression::Bool(_, span)
            | Expression::BinaryOp(_, _, _, span)
            | Expression::UnaryOp(_, _, span)
            | Expression::Id(_, span)
            | Expression::List(_, span)
            | Expression::Dict(_, span)
            | Expression::Set(_, span)
            | Expression::Tuple(_, span)
            | Expression::Ellipsis(span)
            | Expression::Invalid(span)
            | Expression::Yield(_, span)
            | Expression::YieldFrom(_, span)
            | Expression::Await(_, span)
            | Expression::None(span) => *span = new_span,
            Expression::Empty => (),
        }
    }
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

    pub fn is_infix(&self) -> bool {
        matches!(self, Operation::Binary(_))
    }

    pub fn is_postfix(&self) -> bool {
        matches!(
            self,
            Operation::Unary(UnaryOperator::OpenParenthesis | UnaryOperator::OpenBrackets)
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum DictItemType<'a> {
    KeyValue(Expression<'a>, Expression<'a>),
    Unpack(Expression<'a>),
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
pub struct LambdaExpr<'a> {
    pub parameters: Vec<FuncParameter<'a>>,
    pub expression: Box<Expression<'a>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct FuncParameter<'a> {
    pub name: Cow<'a, str>,
    pub default_value: Option<Expression<'a>>,
    pub annotation: Option<Expression<'a>>,
    pub star_parameter_type: Option<StarParameterType>,
    pub is_kw_only: bool,
    pub is_pos_only: bool,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct ClassStmt<'a> {
    pub name: Cow<'a, str>,
    pub block: Block<'a>,
    pub base_classes: Vec<Expression<'a>>,
    pub keyword_args: Vec<ClassKeywordArg<'a>>,
    pub decorators: Vec<Expression<'a>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct ClassKeywordArg<'a> {
    pub arg: Expression<'a>,
    pub value: Expression<'a>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct IfStmt<'a> {
    pub condition: Expression<'a>,
    pub block: Block<'a>,
    pub elif_stms: Vec<ElIfStmt<'a>>,
    pub else_stmt: Option<ElseStmt<'a>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ElIfStmt<'a> {
    pub condition: Expression<'a>,
    pub block: Block<'a>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ElseStmt<'a> {
    pub block: Block<'a>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct While<'a> {
    pub condition: Expression<'a>,
    pub else_stmt: Option<ElseStmt<'a>>,
    pub block: Block<'a>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Assign<'a> {
    pub lhs: Box<Expression<'a>>,
    pub rhs: Box<Expression<'a>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct AugAssign<'a> {
    pub lhs: Box<Expression<'a>>,
    pub rhs: Box<Expression<'a>>,
    pub kind: AugAssignType,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub enum AugAssignType {
    Asterisk,
    At,
    BitwiseAnd,
    BitwiseLeftShift,
    BitwiseNot,
    BitwiseOr,
    BitwiseRightShift,
    BitwiseXOr,
    Divide,
    Exponent,
    FloorDivision,
    Minus,
    Modulus,
    Plus,
}

#[derive(Debug, PartialEq, Eq)]
pub struct AnnAssign<'a> {
    pub lhs: Box<Expression<'a>>,
    pub rhs: Option<Box<Expression<'a>>>,
    pub typehint: Box<Expression<'a>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct Function<'a> {
    pub name: Cow<'a, str>,
    pub parameters: Vec<FuncParameter<'a>>,
    pub block: Block<'a>,
    pub decorators: Vec<Expression<'a>>,
    pub returns: Option<Expression<'a>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct Block<'a> {
    pub stmts: Vec<Statement<'a>>,
    pub span: Span,
}

impl<'a> Block<'a> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            stmts: Vec::new(),
            span: Span::default(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParsedFile<'a> {
    pub stmts: Vec<Statement<'a>>,
}

impl<'a> ParsedFile<'a> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self { stmts: Vec::new() }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct IfElseExpr<'a> {
    pub lhs: Box<Expression<'a>>,
    pub rhs: Box<Expression<'a>>,
    pub condition: Box<Expression<'a>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct ImportStmt<'a> {
    pub modules: Vec<ImportModule<'a>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct ImportModule<'a> {
    pub name: Cow<'a, str>,
    pub alias: Option<Cow<'a, str>>,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct FromImportStmt<'a> {
    pub leading_dots: u32,
    pub module: Option<String>,
    pub targets: Vec<ImportModule<'a>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct WithStmt<'a> {
    pub items: Vec<WithItem<'a>>,
    pub block: Block<'a>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct WithItem<'a> {
    pub item: Expression<'a>,
    pub target: Option<Expression<'a>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct TryStmt<'a> {
    pub block: Block<'a>,
    pub finally_block: Option<FinallyBlock<'a>>,
    pub except_blocks: Vec<ExceptBlock<'a>>,
    pub else_stmt: Option<ElseStmt<'a>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct ExceptBlock<'a> {
    pub block: Block<'a>,
    pub kind: ExceptBlockKind,
    pub expr: Option<Expression<'a>>,
    pub expr_alias: Option<&'a str>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct FinallyBlock<'a> {
    pub block: Block<'a>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct ReturnStmt<'a> {
    pub value: Option<Expression<'a>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct ForStmt<'a> {
    pub target: Expression<'a>,
    pub iter: Expression<'a>,
    pub block: Block<'a>,
    pub else_stmt: Option<ElseStmt<'a>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct RaiseStmt<'a> {
    pub exc: Option<Expression<'a>>,
    pub from: Option<Expression<'a>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct DelStmt<'a> {
    pub expr: Expression<'a>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct FunctionCall<'a> {
    pub lhs: Box<Expression<'a>>,
    pub args: Vec<Expression<'a>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Subscript<'a> {
    pub lhs: Box<Expression<'a>>,
    pub slice: Box<SubscriptType<'a>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub enum SubscriptType<'a> {
    Slice {
        lower: Option<Expression<'a>>,
        upper: Option<Expression<'a>>,
        step: Option<Expression<'a>>,
    },
    Subscript(Expression<'a>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct ListComp<'a> {
    pub target: Box<Expression<'a>>,
    pub ifs: Vec<IfComp<'a>>,
    pub fors: Vec<ForComp<'a>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct GeneratorComp<'a> {
    pub target: Box<Expression<'a>>,
    pub ifs: Vec<IfComp<'a>>,
    pub fors: Vec<ForComp<'a>>,
    pub span: Span,
}

/// The "if" that goes inside a comprehension e.g.: [i for i in range(10) if i % 2 == 0]
#[derive(Debug, PartialEq, Eq)]
pub struct IfComp<'a> {
    pub cond: Expression<'a>,
    pub span: Span,
}

/// The "for" that goes inside a comprehension e.g.: [i for i in range(10)]
#[derive(Debug, PartialEq, Eq)]
pub struct ForComp<'a> {
    pub target: Expression<'a>,
    pub iter: Expression<'a>,
    pub is_async: bool,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct AssertStmt<'a> {
    pub expr: Expression<'a>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct GlobalStmt<'a> {
    pub names: Vec<Expression<'a>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct NonLocalStmt<'a> {
    pub names: Vec<Expression<'a>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SetComp<'a> {
    pub target: Box<Expression<'a>>,
    pub ifs: Vec<IfComp<'a>>,
    pub fors: Vec<ForComp<'a>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct DictComp<'a> {
    pub target: Box<Expression<'a>>,
    pub ifs: Vec<IfComp<'a>>,
    pub fors: Vec<ForComp<'a>>,
    pub span: Span,
}
