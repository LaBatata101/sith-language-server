use ruff_text_size::{TextRange, TextSize};

use super::nodes::{Context, Expression};

/// Return the range of the string token without the quotes
pub fn remove_str_quotes(str_range: TextRange, prefix_size: u32, is_triple_quote: bool) -> TextRange {
    let quote_size = if is_triple_quote { 3 } else { 1 };
    str_range
        .add_start(TextSize::from(quote_size + prefix_size))
        .sub_end(TextSize::from(quote_size))
}

/// Set the `ctx` for `Expression::Id`. If `expr` is `Expression::Tuple` set the
/// `ctx` for every `Expression::Id` found in `Expression::Tuple`.
pub fn set_ctx_in_expr(expr: &mut Expression, ctx: Context) {
    match expr {
        Expression::Id(ident) => ident.ctx = ctx,
        Expression::Tuple(tuple) => {
            tuple
                .elements
                .iter_mut()
                .for_each(|element| set_ctx_in_expr(element, ctx));
        }
        _ => {}
    }
}
