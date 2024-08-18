use python_ast::name::Name;
use python_ast::{self as ast, ContextExpr, Expr, Pattern};
use ruff_text_size::{Ranged, TextLen, TextRange};

/// Convert the given [`Pattern`] to an [`Expr`].
///
/// This is used to convert an invalid use of pattern to their equivalent expression
/// to preserve the structure of the pattern.
///
/// The conversion is done as follows:
/// - `PatternMatchSingleton`: Boolean and None literals
/// - `PatternMatchValue`: The value itself
/// - `PatternMatchSequence`: List literal
/// - `PatternMatchMapping`: Dictionary literal
/// - `PatternMatchClass`: Call expression
/// - `PatternMatchStar`: Starred expression
/// - `PatternMatchAs`: The pattern itself or the name
/// - `PatternMatchOr`: Binary expression with `|` operator
///
/// Note that the sequence pattern is always converted to a list literal even
/// if it was surrounded by parentheses.
///
/// # Note
///
/// This function returns an invalid [`ast::NameExpr`] if the given pattern is a [`Pattern::MatchAs`]
/// with both the pattern and name present. This is because it cannot be converted to an expression
/// without dropping one of them as there's no way to represent `x as y` as a valid expression.
pub(super) fn pattern_to_expr(pattern: Pattern) -> Expr {
    match pattern {
        Pattern::MatchSingleton(ast::PatternMatchSingleton { range, value }) => match value {
            ast::Singleton::True => {
                Expr::BooleanLiteral(ast::BooleanLiteralExpr { value: true, range })
            }
            ast::Singleton::False => Expr::BooleanLiteral(ast::BooleanLiteralExpr {
                value: false,
                range,
            }),
            ast::Singleton::None => Expr::NoneLiteral(ast::NoneLiteralExpr { range }),
        },
        Pattern::MatchValue(ast::PatternMatchValue { value, .. }) => *value,
        // We don't know which kind of sequence this is: `case [1, 2]:` or `case (1, 2):`.
        Pattern::MatchSequence(ast::PatternMatchSequence { range, patterns }) => {
            Expr::List(ast::ListExpr {
                elts: patterns.into_iter().map(pattern_to_expr).collect(),
                ctx: ContextExpr::Store,
                range,
            })
        }
        Pattern::MatchMapping(ast::PatternMatchMapping {
            range,
            keys,
            patterns,
            rest,
        }) => {
            let mut items: Vec<ast::DictItem> = keys
                .into_iter()
                .zip(patterns)
                .map(|(key, pattern)| ast::DictItem {
                    key: Some(key),
                    value: pattern_to_expr(pattern),
                })
                .collect();
            if let Some(rest) = rest {
                let value = Expr::Name(ast::NameExpr {
                    range: rest.range,
                    id: rest.id,
                    ctx: ContextExpr::Store,
                });
                items.push(ast::DictItem { key: None, value });
            }
            Expr::Dict(ast::DictExpr { range, items })
        }
        Pattern::MatchClass(ast::PatternMatchClass {
            range,
            cls,
            arguments,
        }) => Expr::Call(ast::CallExpr {
            range,
            func: cls,
            arguments: ast::Arguments {
                range: arguments.range,
                args: arguments
                    .patterns
                    .into_iter()
                    .map(pattern_to_expr)
                    .collect(),
                keywords: arguments
                    .keywords
                    .into_iter()
                    .map(|keyword_pattern| ast::Keyword {
                        range: keyword_pattern.range,
                        arg: Some(keyword_pattern.attr),
                        value: pattern_to_expr(keyword_pattern.pattern),
                    })
                    .collect(),
            },
        }),
        Pattern::MatchStar(ast::PatternMatchStar { range, name }) => {
            if let Some(name) = name {
                Expr::Starred(ast::StarredExpr {
                    range,
                    value: Box::new(Expr::Name(ast::NameExpr {
                        range: name.range,
                        id: name.id,
                        ctx: ContextExpr::Store,
                    })),
                    ctx: ContextExpr::Store,
                })
            } else {
                Expr::Starred(ast::StarredExpr {
                    range,
                    value: Box::new(Expr::Name(ast::NameExpr {
                        range: TextRange::new(range.end() - "_".text_len(), range.end()),
                        id: Name::new_static("_"),
                        ctx: ContextExpr::Store,
                    })),
                    ctx: ContextExpr::Store,
                })
            }
        }
        Pattern::MatchAs(ast::PatternMatchAs {
            range,
            pattern,
            name,
        }) => match (pattern, name) {
            (Some(_), Some(_)) => Expr::Name(ast::NameExpr {
                range,
                id: Name::empty(),
                ctx: ContextExpr::Invalid,
            }),
            (Some(pattern), None) => pattern_to_expr(*pattern),
            (None, Some(name)) => Expr::Name(ast::NameExpr {
                range: name.range,
                id: name.id,
                ctx: ContextExpr::Store,
            }),
            (None, None) => Expr::Name(ast::NameExpr {
                range,
                id: Name::new_static("_"),
                ctx: ContextExpr::Store,
            }),
        },
        Pattern::MatchOr(ast::PatternMatchOr { patterns, .. }) => {
            let to_bin_expr = |left: Pattern, right: Pattern| ast::BinOpExpr {
                range: TextRange::new(left.start(), right.end()),
                left: Box::new(pattern_to_expr(left)),
                op: ast::Operator::BitOr,
                right: Box::new(pattern_to_expr(right)),
            };

            let mut iter = patterns.into_iter();

            match (iter.next(), iter.next()) {
                (Some(left), Some(right)) => {
                    Expr::BinOp(iter.fold(to_bin_expr(left, right), |expr_bin_op, pattern| {
                        ast::BinOpExpr {
                            range: TextRange::new(expr_bin_op.start(), pattern.end()),
                            left: Box::new(Expr::BinOp(expr_bin_op)),
                            op: ast::Operator::BitOr,
                            right: Box::new(pattern_to_expr(pattern)),
                        }
                    }))
                }
                _ => unreachable!("Or patterns can only be formed with at least two patterns."),
            }
        }
    }
}
