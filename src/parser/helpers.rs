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
