use super::nodes::{
    self, Alias, Arguments, BinaryOp, BoolOp, CompareOp, Comprehension, Decorator, ElifElseClause, ExceptHandler,
    Expression, KeywordArg, MatchCase, Module, Operator, Parameter, Parameters, Pattern, PatternArguments,
    PatternKeyword, Statement, TypeParam, TypeParams, UnaryOp, WithItem,
};

pub trait Visitor<'a> {
    fn visit_module(&mut self, module: &'a Module) {
        walk_module(self, module)
    }
    fn visit_stmt(&mut self, stmt: &'a Statement) {
        walk_stmt(self, stmt);
    }
    fn visit_annotation(&mut self, expr: &'a Expression) {
        walk_annotation(self, expr);
    }
    fn visit_decorator(&mut self, decorator: &'a Decorator) {
        walk_decorator(self, decorator);
    }
    fn visit_expr(&mut self, expr: &'a Expression) {
        walk_expr(self, expr);
    }
    fn visit_bool_op(&mut self, bool_op: &'a BoolOp) {
        walk_bool_op(self, bool_op);
    }
    fn visit_operator(&mut self, operator: &'a Operator) {
        walk_operator(self, operator);
    }
    fn visit_binary_operator(&mut self, operator: &'a BinaryOp) {
        walk_binary_operator(self, operator);
    }
    fn visit_unary_op(&mut self, unary_op: &'a UnaryOp) {
        walk_unary_op(self, unary_op);
    }
    fn visit_cmp_op(&mut self, cmp_op: &'a CompareOp) {
        walk_cmp_op(self, cmp_op);
    }
    fn visit_comprehension(&mut self, comprehension: &'a Comprehension) {
        walk_comprehension(self, comprehension);
    }
    fn visit_except_handler(&mut self, except_handler: &'a ExceptHandler) {
        walk_except_handler(self, except_handler);
    }
    fn visit_format_spec(&mut self, format_spec: &'a Expression) {
        walk_format_spec(self, format_spec);
    }
    fn visit_arguments(&mut self, arguments: &'a Arguments) {
        walk_arguments(self, arguments);
    }
    fn visit_parameters(&mut self, parameters: &'a Parameters) {
        walk_parameters(self, parameters);
    }
    fn visit_parameter(&mut self, parameter: &'a Parameter) {
        walk_parameter(self, parameter);
    }
    fn visit_keyword(&mut self, keyword: &'a KeywordArg) {
        walk_keyword(self, keyword);
    }
    fn visit_alias(&mut self, alias: &'a Alias) {
        walk_alias(self, alias);
    }
    fn visit_with_item(&mut self, with_item: &'a WithItem) {
        walk_with_item(self, with_item);
    }
    fn visit_type_params(&mut self, type_params: &'a TypeParams) {
        walk_type_params(self, type_params);
    }
    fn visit_type_param(&mut self, type_param: &'a TypeParam) {
        walk_type_param(self, type_param);
    }
    fn visit_match_case(&mut self, match_case: &'a MatchCase) {
        walk_match_case(self, match_case);
    }
    fn visit_pattern(&mut self, pattern: &'a Pattern) {
        walk_pattern(self, pattern);
    }
    fn visit_pattern_arguments(&mut self, pattern_arguments: &'a PatternArguments) {
        walk_pattern_arguments(self, pattern_arguments);
    }
    fn visit_pattern_keyword(&mut self, pattern_keyword: &'a PatternKeyword) {
        walk_pattern_keyword(self, pattern_keyword);
    }
    fn visit_body(&mut self, body: &'a [Statement]) {
        walk_body(self, body);
    }
    fn visit_elif_else_clause(&mut self, elif_else_clause: &'a ElifElseClause) {
        walk_elif_else_clause(self, elif_else_clause);
    }
}

pub fn walk_module<'a, V: Visitor<'a> + ?Sized>(visitor: &mut V, module: &'a Module) {
    walk_body(visitor, &module.body);
}

pub fn walk_body<'a, V: Visitor<'a> + ?Sized>(visitor: &mut V, body: &'a [Statement]) {
    for stmt in body {
        visitor.visit_stmt(stmt);
    }
}

pub fn walk_elif_else_clause<'a, V: Visitor<'a> + ?Sized>(visitor: &mut V, elif_else_clause: &'a ElifElseClause) {
    if let Some(test) = &elif_else_clause.test {
        visitor.visit_expr(test);
    }
    visitor.visit_body(&elif_else_clause.body);
}

pub fn walk_stmt<'a, V: Visitor<'a> + ?Sized>(visitor: &mut V, stmt: &'a Statement) {
    match stmt {
        Statement::FunctionDef(func) | Statement::AsyncFunctionDef(func) => {
            for decorator in &func.decorators {
                visitor.visit_decorator(decorator);
            }
            if let Some(type_params) = func.type_params.as_ref() {
                visitor.visit_type_params(type_params);
            }
            visitor.visit_parameters(&func.parameters);
            for expr in &func.returns {
                visitor.visit_annotation(expr);
            }
            visitor.visit_body(&func.body);
        }
        Statement::Class(nodes::ClassDefStmt {
            arguments,
            body,
            decorators,
            type_params,
            ..
        }) => {
            for decorator in decorators {
                visitor.visit_decorator(decorator);
            }
            if let Some(type_params) = type_params {
                visitor.visit_type_params(type_params);
            }
            if let Some(arguments) = arguments {
                visitor.visit_arguments(arguments);
            }
            visitor.visit_body(body);
        }
        Statement::Return(nodes::ReturnStmt { value, .. }) => {
            if let Some(expr) = value {
                visitor.visit_expr(expr);
            }
        }
        Statement::Del(nodes::DelStmt { targets, .. }) => {
            for expr in targets {
                visitor.visit_expr(expr);
            }
        }
        Statement::TypeAlias(nodes::TypeAlias {
            name,
            type_params,
            value,
            ..
        }) => {
            visitor.visit_expr(value);
            if let Some(type_params) = type_params {
                visitor.visit_type_params(type_params);
            }
            visitor.visit_expr(name);
        }
        Statement::Assign(nodes::AssignStmt { targets, value, .. }) => {
            visitor.visit_expr(value);
            for expr in targets {
                visitor.visit_expr(expr);
            }
        }
        Statement::AugAssign(nodes::AugAssignStmt { target, op, value, .. }) => {
            visitor.visit_expr(value);
            visitor.visit_operator(op);
            visitor.visit_expr(target);
        }
        Statement::AnnAssign(nodes::AnnAssignStmt {
            target,
            annotation,
            value,
            ..
        }) => {
            if let Some(expr) = value {
                visitor.visit_expr(expr);
            }
            visitor.visit_annotation(annotation);
            visitor.visit_expr(target);
        }
        Statement::For(for_stmt) | Statement::AsyncFor(for_stmt) => {
            visitor.visit_expr(&for_stmt.iter);
            visitor.visit_expr(&for_stmt.target);
            visitor.visit_body(&for_stmt.body);
            visitor.visit_body(&for_stmt.orelse);
        }
        Statement::While(nodes::WhileStmt { test, body, orelse, .. }) => {
            visitor.visit_expr(test);
            visitor.visit_body(body);
            visitor.visit_body(orelse);
        }
        Statement::If(nodes::IfStmt {
            test,
            body,
            elif_else_clauses,
            ..
        }) => {
            visitor.visit_expr(test);
            visitor.visit_body(body);
            for clause in elif_else_clauses {
                if let Some(test) = &clause.test {
                    visitor.visit_expr(test);
                }
                walk_elif_else_clause(visitor, clause);
            }
        }
        Statement::With(with_stmt) | Statement::AsyncWith(with_stmt) => {
            for with_item in &with_stmt.items {
                visitor.visit_with_item(with_item);
            }
            visitor.visit_body(&with_stmt.body);
        }
        Statement::Match(nodes::MatchStmt { subject, cases, .. }) => {
            visitor.visit_expr(subject);
            for match_case in cases {
                visitor.visit_match_case(match_case);
            }
        }
        Statement::Raise(nodes::RaiseStmt { exc, cause, .. }) => {
            if let Some(expr) = exc {
                visitor.visit_expr(expr);
            };
            if let Some(expr) = cause {
                visitor.visit_expr(expr);
            };
        }
        Statement::Try(nodes::TryStmt {
            body,
            handlers,
            orelse,
            final_body,
            is_star: _,
            ..
        }) => {
            visitor.visit_body(body);
            for except_handler in handlers {
                visitor.visit_except_handler(except_handler);
            }
            visitor.visit_body(orelse);
            visitor.visit_body(final_body);
        }
        Statement::Assert(nodes::AssertStmt { test, message, .. }) => {
            visitor.visit_expr(test);
            if let Some(expr) = message {
                visitor.visit_expr(expr);
            }
        }
        Statement::Import(nodes::ImportStmt { names, .. }) => {
            for alias in names {
                visitor.visit_alias(alias);
            }
        }
        Statement::ImportFrom(nodes::ImportFromStmt { names, .. }) => {
            for alias in names {
                visitor.visit_alias(alias);
            }
        }
        Statement::Global(_) => {}
        Statement::NonLocal(_) => {}
        Statement::Expression(nodes::ExpressionStmt { value, .. }) => visitor.visit_expr(value),
        Statement::Pass(_) | Statement::Break(_) | Statement::Continue(_) => {}
    }
}

pub fn walk_annotation<'a, V: Visitor<'a> + ?Sized>(visitor: &mut V, expr: &'a Expression) {
    visitor.visit_expr(expr);
}

pub fn walk_decorator<'a, V: Visitor<'a> + ?Sized>(visitor: &mut V, decorator: &'a Decorator) {
    visitor.visit_expr(&decorator.expr);
}

pub fn walk_expr<'a, V: Visitor<'a> + ?Sized>(visitor: &mut V, expr: &'a Expression) {
    match expr {
        Expression::BoolOp(nodes::BoolOpExpr { op, values, .. }) => {
            visitor.visit_bool_op(op);
            for expr in values {
                visitor.visit_expr(expr);
            }
        }
        Expression::Named(nodes::NamedExpr { target, value, .. }) => {
            visitor.visit_expr(value);
            visitor.visit_expr(target);
        }
        Expression::BinaryOp(nodes::BinaryOpExpr { left, op, right, .. }) => {
            visitor.visit_expr(left);
            visitor.visit_binary_operator(op);
            visitor.visit_expr(right);
        }
        Expression::UnaryOp(nodes::UnaryOpExpr { op, operand, .. }) => {
            visitor.visit_unary_op(op);
            visitor.visit_expr(operand);
        }
        Expression::Lambda(nodes::LambdaExpr { parameters, body, .. }) => {
            if let Some(parameters) = parameters {
                visitor.visit_parameters(parameters);
            }
            visitor.visit_expr(body);
        }
        Expression::IfElse(nodes::IfElseExpr { test, body, orelse, .. }) => {
            visitor.visit_expr(test);
            visitor.visit_expr(body);
            visitor.visit_expr(orelse);
        }
        Expression::Dict(nodes::DictExpr { keys, values, .. }) => {
            for expr in keys.iter().flatten() {
                visitor.visit_expr(expr);
            }
            for expr in values {
                visitor.visit_expr(expr);
            }
        }
        Expression::Set(nodes::SetExpr { elements, .. }) => {
            for expr in elements {
                visitor.visit_expr(expr);
            }
        }
        Expression::ListComp(nodes::ListCompExpr {
            element, generators, ..
        }) => {
            for comprehension in generators {
                visitor.visit_comprehension(comprehension);
            }
            visitor.visit_expr(element);
        }
        Expression::SetComp(nodes::SetCompExpr {
            element, generators, ..
        }) => {
            for comprehension in generators {
                visitor.visit_comprehension(comprehension);
            }
            visitor.visit_expr(element);
        }
        Expression::DictComp(nodes::DictCompExpr {
            key, value, generators, ..
        }) => {
            for comprehension in generators {
                visitor.visit_comprehension(comprehension);
            }
            visitor.visit_expr(key);
            visitor.visit_expr(value);
        }
        Expression::Generator(nodes::GeneratorExpr {
            element, generators, ..
        }) => {
            for comprehension in generators {
                visitor.visit_comprehension(comprehension);
            }
            visitor.visit_expr(element);
        }
        Expression::Await(nodes::AwaitExpr { value, .. }) => visitor.visit_expr(value),
        Expression::Yield(nodes::YieldExpr { value, .. }) => {
            if let Some(expr) = value {
                visitor.visit_expr(expr);
            }
        }
        Expression::YieldFrom(nodes::YieldFromExpr { value, .. }) => visitor.visit_expr(value),
        Expression::Compare(nodes::CompareExpr {
            left, ops, comparators, ..
        }) => {
            visitor.visit_expr(left);
            for cmp_op in ops {
                visitor.visit_cmp_op(cmp_op);
            }
            for expr in comparators {
                visitor.visit_expr(expr);
            }
        }
        Expression::Call(nodes::CallExpr { func, args, .. }) => {
            visitor.visit_expr(func);
            visitor.visit_arguments(args);
        }
        Expression::FormattedValue(nodes::FormattedValueExpr { value, format_spec, .. }) => {
            visitor.visit_expr(value);
            if let Some(expr) = format_spec {
                visitor.visit_format_spec(expr);
            }
        }
        Expression::FString(nodes::FStringExpr { values, .. }) => {
            for expr in values {
                visitor.visit_expr(expr);
            }
        }
        Expression::Literal(_) | Expression::Ellipsis(_) => {}
        Expression::Attribute(nodes::AttributeExpr { value, .. }) => {
            visitor.visit_expr(value);
        }
        Expression::Subscript(nodes::SubscriptExpr { value, slice, .. }) => {
            visitor.visit_expr(value);
            visitor.visit_expr(slice);
        }
        Expression::Starred(nodes::StarredExpr { value, .. }) => {
            visitor.visit_expr(value);
        }
        Expression::Id(_) => {}
        Expression::List(nodes::ListExpr { elements, .. }) => {
            for expr in elements {
                visitor.visit_expr(expr);
            }
        }
        Expression::Tuple(nodes::TupleExpr { elements, .. }) => {
            for expr in elements {
                visitor.visit_expr(expr);
            }
        }
        Expression::Slice(nodes::SliceExpr { lower, upper, step, .. }) => {
            if let Some(expr) = lower {
                visitor.visit_expr(expr);
            }
            if let Some(expr) = upper {
                visitor.visit_expr(expr);
            }
            if let Some(expr) = step {
                visitor.visit_expr(expr);
            }
        }
        Expression::Invalid(_) => {}
    }
}

pub fn walk_comprehension<'a, V: Visitor<'a> + ?Sized>(visitor: &mut V, comprehension: &'a Comprehension) {
    visitor.visit_expr(&comprehension.iter);
    visitor.visit_expr(&comprehension.target);
    for expr in &comprehension.ifs {
        visitor.visit_expr(expr);
    }
}

pub fn walk_except_handler<'a, V: Visitor<'a> + ?Sized>(visitor: &mut V, except_handler: &'a ExceptHandler) {
    if let Some(expr) = &except_handler.ty {
        visitor.visit_expr(expr);
    }
    visitor.visit_body(&except_handler.body);
}

pub fn walk_format_spec<'a, V: Visitor<'a> + ?Sized>(visitor: &mut V, format_spec: &'a Expression) {
    visitor.visit_expr(format_spec);
}

pub fn walk_arguments<'a, V: Visitor<'a> + ?Sized>(visitor: &mut V, arguments: &'a Arguments) {
    // Note that the there might be keywords before the last arg, e.g. in
    // f(*args, a=2, *args2, **kwargs)`, but we follow Python in evaluating first `args` and then
    // `keywords`. See also [Arguments::arguments_source_order`].
    for arg in &arguments.args {
        visitor.visit_expr(arg);
    }
    for keyword in &arguments.kw_args {
        visitor.visit_keyword(keyword);
    }
}

pub fn walk_parameters<'a, V: Visitor<'a> + ?Sized>(visitor: &mut V, parameters: &'a Parameters) {
    // Defaults are evaluated before annotations.
    for arg in &parameters.posonlyargs {
        if let Some(default) = &arg.default {
            visitor.visit_expr(default);
        }
    }
    for arg in &parameters.args {
        if let Some(default) = &arg.default {
            visitor.visit_expr(default);
        }
    }
    for arg in &parameters.kwonlyargs {
        if let Some(default) = &arg.default {
            visitor.visit_expr(default);
        }
    }

    for arg in &parameters.posonlyargs {
        visitor.visit_parameter(&arg.parameter);
    }
    for arg in &parameters.args {
        visitor.visit_parameter(&arg.parameter);
    }
    if let Some(arg) = &parameters.vararg {
        visitor.visit_parameter(arg);
    }
    for arg in &parameters.kwonlyargs {
        visitor.visit_parameter(&arg.parameter);
    }
    if let Some(arg) = &parameters.kwarg {
        visitor.visit_parameter(arg);
    }
}

pub fn walk_parameter<'a, V: Visitor<'a> + ?Sized>(visitor: &mut V, parameter: &'a Parameter) {
    if let Some(expr) = &parameter.annotation {
        visitor.visit_annotation(expr);
    }
}

pub fn walk_keyword<'a, V: Visitor<'a> + ?Sized>(visitor: &mut V, keyword: &'a KeywordArg) {
    visitor.visit_expr(&keyword.value);
}

pub fn walk_with_item<'a, V: Visitor<'a> + ?Sized>(visitor: &mut V, with_item: &'a WithItem) {
    visitor.visit_expr(&with_item.item);
    if let Some(expr) = &with_item.target {
        visitor.visit_expr(expr);
    }
}

pub fn walk_type_params<'a, V: Visitor<'a> + ?Sized>(visitor: &mut V, type_params: &'a TypeParams) {
    for type_param in &type_params.type_params {
        visitor.visit_type_param(type_param);
    }
}

pub fn walk_type_param<'a, V: Visitor<'a> + ?Sized>(visitor: &mut V, type_param: &'a TypeParam) {
    match type_param {
        TypeParam::TypeVar(nodes::TypeParamTypeVar { bound, name: _, .. }) => {
            if let Some(expr) = bound {
                visitor.visit_expr(expr);
            }
        }
        TypeParam::TypeVarTuple(_) | TypeParam::ParamSpec(_) => {}
    }
}

pub fn walk_match_case<'a, V: Visitor<'a> + ?Sized>(visitor: &mut V, match_case: &'a MatchCase) {
    visitor.visit_pattern(&match_case.pattern);
    if let Some(expr) = &match_case.guard {
        visitor.visit_expr(expr);
    }
    visitor.visit_body(&match_case.body);
}

pub fn walk_pattern<'a, V: Visitor<'a> + ?Sized>(visitor: &mut V, pattern: &'a Pattern) {
    match pattern {
        Pattern::MatchValue(nodes::PatternMatchValue { value, .. }) => {
            visitor.visit_expr(value);
        }
        Pattern::MatchSingleton(_) => {}
        Pattern::MatchSequence(nodes::PatternMatchSequence { patterns, .. }) => {
            for pattern in patterns {
                visitor.visit_pattern(pattern);
            }
        }
        Pattern::MatchMapping(nodes::PatternMatchMapping { keys, patterns, .. }) => {
            for expr in keys {
                visitor.visit_expr(expr);
            }
            for pattern in patterns {
                visitor.visit_pattern(pattern);
            }
        }
        Pattern::MatchClass(nodes::PatternMatchClass { cls, arguments, .. }) => {
            visitor.visit_expr(cls);
            visitor.visit_pattern_arguments(arguments);
        }
        Pattern::MatchStar(_) => {}
        Pattern::MatchAs(nodes::PatternMatchAs { pattern, .. }) => {
            if let Some(pattern) = pattern {
                visitor.visit_pattern(pattern);
            }
        }
        Pattern::MatchOr(nodes::PatternMatchOr { patterns, .. }) => {
            for pattern in patterns {
                visitor.visit_pattern(pattern);
            }
        }
        Pattern::Invalid => {}
    }
}

pub fn walk_pattern_arguments<'a, V: Visitor<'a> + ?Sized>(visitor: &mut V, pattern_arguments: &'a PatternArguments) {
    for pattern in &pattern_arguments.patterns {
        visitor.visit_pattern(pattern);
    }
    for keyword in &pattern_arguments.keywords {
        visitor.visit_pattern_keyword(keyword);
    }
}

pub fn walk_pattern_keyword<'a, V: Visitor<'a> + ?Sized>(visitor: &mut V, pattern_keyword: &'a PatternKeyword) {
    visitor.visit_pattern(&pattern_keyword.pattern);
}

#[allow(unused_variables)]
pub fn walk_bool_op<'a, V: Visitor<'a> + ?Sized>(visitor: &V, bool_op: &'a BoolOp) {}

#[allow(unused_variables)]
pub fn walk_operator<'a, V: Visitor<'a> + ?Sized>(visitor: &V, operator: &'a Operator) {}

#[allow(unused_variables)]
pub fn walk_binary_operator<'a, V: Visitor<'a> + ?Sized>(visitor: &V, operator: &'a BinaryOp) {}

#[allow(unused_variables)]
pub fn walk_unary_op<'a, V: Visitor<'a> + ?Sized>(visitor: &V, unary_op: &'a UnaryOp) {}

#[allow(unused_variables)]
pub fn walk_cmp_op<'a, V: Visitor<'a> + ?Sized>(visitor: &V, cmp_op: &'a CompareOp) {}

#[allow(unused_variables)]
pub fn walk_alias<'a, V: Visitor<'a> + ?Sized>(visitor: &V, alias: &'a Alias) {}
