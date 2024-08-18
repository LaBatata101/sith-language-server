use python_ast::{self as ast, CmpOp, ContextExpr, Expr};

use crate::TokenKind;

/// Set the `ctx` for `Expr::Id`, `Expr::Attribute`, `Expr::Subscript`, `Expr::Starred`,
/// `Expr::Tuple` and `Expr::List`. If `expr` is either `Expr::Tuple` or `Expr::List`,
/// recursively sets the `ctx` for their elements.
pub(super) fn set_expr_ctx(expr: &mut Expr, new_ctx: ContextExpr) {
    match expr {
        Expr::Name(ast::NameExpr { ctx, .. })
        | Expr::Attribute(ast::AttributeExpr { ctx, .. })
        | Expr::Subscript(ast::SubscriptExpr { ctx, .. }) => *ctx = new_ctx,
        Expr::Starred(ast::StarredExpr { value, ctx, .. }) => {
            *ctx = new_ctx;
            set_expr_ctx(value, new_ctx);
        }
        Expr::UnaryOp(ast::UnaryOpExpr { operand, .. }) => {
            set_expr_ctx(operand, new_ctx);
        }
        Expr::List(ast::ListExpr { elts, ctx, .. })
        | Expr::Tuple(ast::TupleExpr { elts, ctx, .. }) => {
            *ctx = new_ctx;
            elts.iter_mut()
                .for_each(|element| set_expr_ctx(element, new_ctx));
        }
        _ => {}
    }
}

/// Converts a [`TokenKind`] array of size 2 to its correspondent [`CmpOp`].
pub(super) const fn token_kind_to_cmp_op(tokens: [TokenKind; 2]) -> Option<CmpOp> {
    Some(match tokens {
        [TokenKind::Is, TokenKind::Not] => CmpOp::IsNot,
        [TokenKind::Is, _] => CmpOp::Is,
        [TokenKind::Not, TokenKind::In] => CmpOp::NotIn,
        [TokenKind::In, _] => CmpOp::In,
        [TokenKind::EqEqual, _] => CmpOp::Eq,
        [TokenKind::NotEqual, _] => CmpOp::NotEq,
        [TokenKind::Less, _] => CmpOp::Lt,
        [TokenKind::LessEqual, _] => CmpOp::LtE,
        [TokenKind::Greater, _] => CmpOp::Gt,
        [TokenKind::GreaterEqual, _] => CmpOp::GtE,
        _ => return None,
    })
}
