use ruff_text_size::{TextRange, TextSize};

use super::nodes::{ContextExpr, Expression};

/// Return the range of the string token without the quotes
pub fn remove_str_quotes(str_range: TextRange, prefix_size: u32, is_triple_quote: bool) -> TextRange {
    let quote_size = if is_triple_quote { 3 } else { 1 };
    str_range
        .add_start(TextSize::from(quote_size + prefix_size))
        .sub_end(TextSize::from(quote_size))
}

/// Set the `ctx` for `Expression::Id`, `Expression::Attribute`, `Expression::Subscript`,
/// `Expression::Starred`, `Expression::Tuple` and `Expression::List`. If `expr` is either
/// `Expression::Tuple` or `Expression::List`, recursively sets the `ctx` for their elements.
pub fn set_expr_ctx(expr: &mut Expression, ctx: ContextExpr) {
    match expr {
        Expression::Id(ident) => ident.ctx = ctx,
        Expression::Attribute(attrib) => attrib.ctx = ctx,
        Expression::Subscript(subscript) => subscript.ctx = ctx,
        Expression::Starred(starred) => starred.ctx = ctx,
        Expression::List(list) => {
            list.ctx = ctx;
            list.elements.iter_mut().for_each(|element| set_expr_ctx(element, ctx));
        }
        Expression::Tuple(tuple) => {
            tuple.ctx = ctx;
            tuple.elements.iter_mut().for_each(|element| set_expr_ctx(element, ctx));
        }
        _ => {}
    }
}

pub fn set_expr_range(expr: &mut Expression, range: TextRange) {
    match expr {
        Expression::Invalid(r) => *r = range,
        Expression::Id(node) => node.range = range,
        Expression::Set(node) => node.range = range,
        Expression::Call(node) => node.range = range,
        Expression::Dict(node) => node.range = range,
        Expression::List(node) => node.range = range,
        Expression::Named(node) => node.range = range,
        Expression::Yield(node) => node.range = range,
        Expression::Await(node) => node.range = range,
        Expression::Slice(node) => node.range = range,
        Expression::Tuple(node) => node.range = range,
        Expression::Ellipsis(node) => node.range = range,
        Expression::BoolOp(node) => node.range = range,
        Expression::IfElse(node) => node.range = range,
        Expression::Lambda(node) => node.range = range,
        Expression::Compare(node) => node.range = range,
        Expression::UnaryOp(node) => node.range = range,
        Expression::FString(node) => node.range = range,
        Expression::Literal(node) => node.range = range,
        Expression::SetComp(node) => node.range = range,
        Expression::Starred(node) => node.range = range,
        Expression::BinaryOp(node) => node.range = range,
        Expression::DictComp(node) => node.range = range,
        Expression::ListComp(node) => node.range = range,
        Expression::Attribute(node) => node.range = range,
        Expression::Generator(node) => node.range = range,
        Expression::Subscript(node) => node.range = range,
        Expression::YieldFrom(node) => node.range = range,
    }
}
