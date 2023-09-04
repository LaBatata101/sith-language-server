use ruff_text_size::{TextRange, TextSize};

/// Return the range of the string token without the quotes
pub fn remove_str_quotes(str_range: TextRange, prefix_size: u32, is_triple_quote: bool) -> TextRange {
    let quote_size = if is_triple_quote { 3 } else { 1 };
    str_range
        .add_start(TextSize::from(quote_size + prefix_size))
        .sub_end(TextSize::from(quote_size))
}
