use std::sync::Arc;

use lsp_types::{self as types, request as req, HoverContents, MarkupContent, MarkupKind};
use ruff_text_size::{Ranged, TextRange, TextSize};
use sith_python_ast::{self as ast, AnyNodeRef, BoolOp, CmpOp, UnaryOp};
use sith_python_ast_utils::{
    node_at_offset, node_identifier_at_offset,
    nodes::{NodeId, NodeStack, NodeWithParent, Nodes},
};
use sith_python_utils::{get_python_doc, nodes::get_documentation_string_from_node};
use sith_semantic_model::{
    self as sm,
    db::SymbolTableDb,
    declaration::DeclarationQuery,
    indexer::FileId,
    type_inference::{PythonType, ResolvedType, TypeInferer},
    ScopeId,
};

use crate::{
    edit::position_to_offset,
    server::{client::Notifier, Result},
    session::DocumentSnapshot,
};

use super::completion::resolve::get_module_documentation;

pub(crate) struct Hover;

impl super::RequestHandler for Hover {
    type RequestType = req::HoverRequest;
}

impl super::BackgroundDocumentRequestHandler for Hover {
    fn document_url(params: &types::HoverParams) -> std::borrow::Cow<lsp_types::Url> {
        std::borrow::Cow::Borrowed(&params.text_document_position_params.text_document.uri)
    }

    fn run_with_snapshot(
        snapshot: DocumentSnapshot,
        _notifier: Notifier,
        params: types::HoverParams,
    ) -> Result<Option<types::Hover>> {
        Ok(hover(&snapshot, &params.text_document_position_params))
    }
}

// TODO: show variable type when hovering
pub(crate) fn hover(
    snapshot: &DocumentSnapshot,
    position: &types::TextDocumentPositionParams,
) -> Option<types::Hover> {
    let document_path = Arc::new(snapshot.url().to_file_path().ok()?);
    let db = snapshot.db();
    let node_stack = db.indexer().node_stack(&document_path);
    let nodes = node_stack.nodes();

    let document = snapshot.document();
    let position = position.position;
    let offset = position_to_offset(document.contents(), &position, document.index());

    let (scope_id, _) = db.find_enclosing_scope(&document_path, offset);

    let node = node_at_offset(nodes, offset)?;
    let hover_pos = HoverPosition::from_node(offset.into(), node, node_stack.nodes());

    if let HoverPosition::Keyword(keyword) = hover_pos {
        let doc_str = get_python_doc(snapshot.client_settings().interpreter()?, keyword)
            .unwrap_or_else(|err| {
                tracing::error!(
                    "Failed to get builtin symbol documentation with Python script: {err}"
                );
                None
            })?;
        return Some(types::Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: doc_str,
            }),
            range: None,
        });
    }

    if !should_show_docs(&hover_pos) {
        return None;
    }

    let identifier = node_identifier_at_offset(node, offset)?;
    let mut type_inferer = TypeInferer::new(db, scope_id, document_path);
    let doc_str = match type_inferer.infer_node(node, nodes) {
        ResolvedType::KnownType(PythonType::Class(class_type)) => get_doc_str(
            db,
            snapshot,
            class_type.file_id,
            class_type.node_id,
            identifier,
        )?,
        ResolvedType::KnownType(PythonType::Function {
            file_id, node_id, ..
        }) => get_doc_str(db, snapshot, file_id, node_id, identifier)?,
        ResolvedType::KnownType(PythonType::Module(import_source)) => {
            get_module_documentation(db, import_source.non_stub_path()?)?
        }
        _ => return None,
    };

    Some(types::Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: doc_str,
        }),
        range: None,
    })
}

fn should_show_docs(hover_pos: &HoverPosition) -> bool {
    matches!(
        hover_pos,
        HoverPosition::Id
            | HoverPosition::Attr
            | HoverPosition::FunctionName
            | HoverPosition::ClassName
            | HoverPosition::Call
    )
}

fn get_doc_str(
    db: &SymbolTableDb,
    snapshot: &DocumentSnapshot,
    file_id: FileId,
    node_id: NodeId,
    identifier: &str,
) -> Option<String> {
    let path = db.indexer().file_path(&file_id);

    // If the symbol being hovered is builtin we need to execute a python script to get the docs
    // for it.
    if path.ends_with("stdlib/builtins.pyi") {
        get_python_doc(snapshot.client_settings().interpreter()?, identifier).ok()?
    }
    // if the symbol came from a stub file we need to get the documentation from the non-stub
    // file. Usually stub files doesn't contain documentation.
    else if path.extension().is_some_and(|ext| ext == "pyi") {
        get_docs_for_non_stub(db, file_id, node_id, identifier)
    }
    // this case is for normal python files (.py)
    else {
        let node_stack = db.indexer().node_stack(path);
        get_documentation_string_from_node(node_stack.nodes().get(node_id).unwrap())
    }
}

fn get_docs_for_non_stub(
    db: &SymbolTableDb,
    file_id: FileId,
    node_id: NodeId,
    identifier: &str,
) -> Option<String> {
    let non_stub_path = db.indexer().non_stub_path(&file_id)?;
    if db.indexer().is_indexed(non_stub_path) {
        let node_stack = db.indexer().node_stack(non_stub_path);
        get_documentation_string_from_node(node_stack.nodes().get(node_id).unwrap())
    } else {
        let content = sm::util::read_to_string(non_stub_path.as_path()).ok()?;
        let (table, ast) = db
            .indexer()
            .build_symbol_table(non_stub_path, file_id, &content);
        let node_stack = NodeStack::default().is_thirdparty(true).build(ast.suite());

        let declaration =
            table.symbol_declaration(identifier, ScopeId::global(), DeclarationQuery::Last)?;

        get_documentation_string_from_node(node_stack.nodes().get(declaration.node_id).unwrap())
    }
}

#[derive(Debug)]
enum HoverPosition<'a> {
    /// Hovering over an identifier (e.g., variable name, function name)
    Id,
    /// Hovering over an assignment statement
    Assignment,
    /// Hovering over a parameter definition
    Param,
    /// Hovering over an attribute access (e.g., `object.attribute`)
    Attr,
    /// Hovering over a function definition
    FunctionName,
    /// Hovering over a class definition
    ClassName,
    /// Hovering over a function call expression
    Call,
    /// Hovering over a keyword
    Keyword(&'a str),
    /// Hovering over other things that we don't care about (e.g. number literals, strings, etc.)
    Other,
}

impl HoverPosition<'_> {
    fn from_node(offset: TextSize, node: &NodeWithParent, nodes: &Nodes) -> Self {
        match node.node() {
            AnyNodeRef::NameExpr(_) => {
                if let Some(parent_id) = node.parent_id() {
                    return HoverPosition::from_node(offset, nodes.get(parent_id).unwrap(), nodes);
                }
                Self::Id
            }
            AnyNodeRef::StmtAssign(assign_stmt)
                if assign_stmt
                    .targets
                    .iter()
                    .any(|target| target.range().contains(offset)) =>
            {
                Self::Assignment
            }
            AnyNodeRef::StmtAnnAssign(ann_assign_stmt)
                if ann_assign_stmt.target.range().contains(offset) =>
            {
                Self::Assignment
            }
            AnyNodeRef::StmtAnnAssign(ann_assign_stmt)
                if ann_assign_stmt.annotation.range().contains(offset) =>
            {
                Self::Id
            }
            AnyNodeRef::StmtImport(import)
                if import
                    .names
                    .iter()
                    .any(|name| name.range().contains(offset)) =>
            {
                Self::Id
            }
            AnyNodeRef::StmtImportFrom(ast::ImportFromStmt {
                module: Some(module),
                names,
                ..
            }) if module.range().contains(offset)
                || names.iter().any(|name| name.range().contains(offset)) =>
            {
                Self::Id
            }
            AnyNodeRef::Alias(_) => Self::Id,
            AnyNodeRef::AttributeExpr(_) => Self::Attr,
            AnyNodeRef::StmtClassDef(class) if class.name.range.contains(offset) => Self::ClassName,
            AnyNodeRef::StmtFunctionDef(func) if func.name.range.contains(offset) => {
                Self::FunctionName
            }
            AnyNodeRef::CallExpr(_) => Self::Call,
            AnyNodeRef::Parameter(_) => Self::Param,
            _ => Self::from_keyword(offset, node),
        }
    }

    fn from_keyword(offset: TextSize, node: &NodeWithParent) -> Self {
        match node.node() {
            AnyNodeRef::YieldExpr(_)
                if keyword_range(node.range().start(), "yield").contains(offset) =>
            {
                Self::Keyword("yield")
            }
            AnyNodeRef::YieldFromExpr(_) => {
                let yield_range = keyword_range(node.range().start(), "yield");
                let from_range = keyword_range(yield_range.range().add_end(1.into()).end(), "from");
                if yield_range.contains(offset) {
                    Self::Keyword("yield")
                } else if from_range.contains(offset) {
                    Self::Keyword("from")
                } else {
                    Self::Other
                }
            }
            AnyNodeRef::BooleanLiteralExpr(bool_literal) => {
                if bool_literal.value {
                    Self::Keyword("True")
                } else {
                    Self::Keyword("False")
                }
            }
            AnyNodeRef::NoneLiteralExpr(_) => Self::Keyword("None"),
            AnyNodeRef::BoolOpExpr(ast::BoolOpExpr {
                op: BoolOp::Or,
                values,
                ..
            }) if values.first().is_some_and(|expr| {
                keyword_range(expr.range().add_end(1.into()).end(), "or").contains(offset)
            }) =>
            {
                Self::Keyword("or")
            }
            AnyNodeRef::BoolOpExpr(ast::BoolOpExpr {
                op: BoolOp::And,
                values,
                ..
            }) if values.first().is_some_and(|expr| {
                keyword_range(expr.range().add_end(1.into()).end(), "and").contains(offset)
            }) =>
            {
                Self::Keyword("and")
            }
            AnyNodeRef::UnaryOpExpr(ast::UnaryOpExpr {
                op: UnaryOp::Not, ..
            }) if keyword_range(node.range().start(), "not").contains(offset) => {
                Self::Keyword("not")
            }
            AnyNodeRef::StmtBreak(_) => Self::Keyword("break"),
            AnyNodeRef::StmtContinue(_) => Self::Keyword("continue"),
            AnyNodeRef::StmtPass(_) => Self::Keyword("pass"),
            AnyNodeRef::StmtDelete(_)
                if keyword_range(node.range().start(), "del").contains(offset) =>
            {
                Self::Keyword("del")
            }
            AnyNodeRef::StmtGlobal(_)
                if keyword_range(node.range().start(), "global").contains(offset) =>
            {
                Self::Keyword("global")
            }
            AnyNodeRef::StmtNonlocal(_)
                if keyword_range(node.range().start(), "nonlocal").contains(offset) =>
            {
                Self::Keyword("nonlocal")
            }
            AnyNodeRef::StmtReturn(_)
                if keyword_range(node.range().start(), "return").contains(offset) =>
            {
                Self::Keyword("return")
            }
            AnyNodeRef::StmtClassDef(_)
                if keyword_range(node.range().start(), "class").contains(offset) =>
            {
                Self::Keyword("class")
            }
            AnyNodeRef::LambdaExpr(_)
                if keyword_range(node.range().start(), "lambda").contains(offset) =>
            {
                Self::Keyword("lambda")
            }
            AnyNodeRef::StmtRaise(_)
                if keyword_range(node.range().start(), "raise").contains(offset) =>
            {
                Self::Keyword("raise")
            }
            AnyNodeRef::StmtWhile(_)
                if keyword_range(node.range().start(), "while").contains(offset) =>
            {
                Self::Keyword("while")
            }
            AnyNodeRef::StmtFunctionDef(func) => {
                if func.is_async {
                    let async_range = keyword_range(node.range().start(), "async");
                    let def_range = keyword_range(async_range.add_end(1.into()).end(), "def");
                    if async_range.contains(offset) {
                        return Self::Keyword("async");
                    } else if def_range.contains(offset) {
                        return Self::Keyword("def");
                    }
                } else if keyword_range(node.range().start(), "def").contains(offset) {
                    return Self::Keyword("def");
                }
                Self::Other
            }
            AnyNodeRef::IfExpr(ast::IfExpr { body, test, .. }) => {
                let if_range = keyword_range(body.range().add_end(1.into()).end(), "if");
                let else_range = keyword_range(test.range().add_end(1.into()).end(), "else");
                if if_range.contains(offset) {
                    Self::Keyword("if")
                } else if else_range.contains(offset) {
                    Self::Keyword("else")
                } else {
                    Self::Other
                }
            }
            AnyNodeRef::StmtIf(ast::IfStmt {
                elif_else_clauses, ..
            }) => {
                // Here we are only checking for the "else" keyword but that's fine since "else"
                // and "elif" have the same length
                let is_in_clause = elif_else_clauses
                    .iter()
                    .any(|clause| keyword_range(clause.range().start(), "else").contains(offset));

                // For "else" and "elif" we return `Keyword("if")` because they all have the same
                // documentation.
                if keyword_range(node.range().start(), "if").contains(offset) || is_in_clause {
                    Self::Keyword("if")
                } else {
                    Self::Other
                }
            }
            AnyNodeRef::StmtTry(ast::TryStmt {
                handlers,
                finalbody,
                ..
            }) => {
                if keyword_range(node.range().start(), "try").contains(offset) {
                    Self::Keyword("try")
                } else if handlers.last().is_some_and(|last| {
                    keyword_range(last.range().end(), "finally").contains(offset)
                }) {
                    Self::Keyword("finally")
                } else if finalbody
                    .last()
                    .is_some_and(|last| keyword_range(last.range().end(), "else").contains(offset))
                {
                    Self::Keyword("try")
                } else {
                    Self::Other
                }
            }
            AnyNodeRef::ExceptHandlerExceptHandler(_)
                if keyword_range(node.range().start(), "except").contains(offset) =>
            {
                Self::Keyword("except")
            }
            AnyNodeRef::StmtFor(for_stmt) => {
                if for_stmt.is_async {
                    let async_range = keyword_range(node.range().start(), "async");
                    let def_range = keyword_range(async_range.add_end(1.into()).end(), "for");
                    if async_range.contains(offset) {
                        return Self::Keyword("async");
                    } else if def_range.contains(offset) {
                        return Self::Keyword("for");
                    }
                } else if keyword_range(node.range().start(), "for").contains(offset) {
                    return Self::Keyword("for");
                }
                Self::Other
            }
            AnyNodeRef::StmtImport(_)
                if keyword_range(node.range().start(), "import").contains(offset) =>
            {
                Self::Keyword("import")
            }
            AnyNodeRef::StmtImportFrom(ast::ImportFromStmt {
                module: Some(module),
                ..
            }) if keyword_range(module.range.add_end(1.into()).end(), "import")
                .contains(offset) =>
            {
                Self::Keyword("import")
            }
            AnyNodeRef::StmtImportFrom(_)
                if keyword_range(node.range().start(), "from").contains(offset) =>
            {
                Self::Keyword("from")
            }
            AnyNodeRef::StmtWith(with) => {
                if with.is_async {
                    let async_range = keyword_range(node.range().start(), "async");
                    let with_range = keyword_range(async_range.add_end(1.into()).end(), "with");
                    if async_range.contains(offset) {
                        return Self::Keyword("async");
                    } else if with_range.contains(offset) {
                        return Self::Keyword("with");
                    }
                } else if keyword_range(node.range().start(), "with").contains(offset) {
                    return Self::Keyword("with");
                }
                Self::Other
            }
            AnyNodeRef::StmtMatch(ast::MatchStmt { cases, .. }) => {
                if keyword_range(node.range().start(), "match").contains(offset) {
                    Self::Keyword("match")
                } else if cases
                    .iter()
                    .any(|case| keyword_range(case.range.start(), "case").contains(offset))
                {
                    Self::Keyword("case")
                } else {
                    Self::Other
                }
            }
            AnyNodeRef::StmtTypeAlias(_)
                if keyword_range(node.range().start(), "type").contains(offset) =>
            {
                Self::Keyword("type")
            }
            AnyNodeRef::CompareExpr(ast::CompareExpr {
                ops,
                left,
                comparators,
                ..
            }) if ops.iter().any(|op| matches!(op, CmpOp::In | CmpOp::Is)) => {
                // creates an iterator that yields a tuple with an operator and its precedent
                // expression.
                ops.iter()
                    .zip(
                        std::iter::once(left.as_ref())
                            .chain(comparators.iter().take(comparators.len() - 1)),
                    )
                    // Check if the `offset` is in the "in" or "is" keywords
                    .find(|(op, expr)| {
                        keyword_range(expr.range().add_end(1.into()).end(), op.as_str())
                            .contains(offset)
                    })
                    .map(|(op, _)| match op {
                        CmpOp::Is => Self::Keyword("is"),
                        CmpOp::In => Self::Keyword("in"),
                        _ => unreachable!(),
                    })
                    .unwrap_or(Self::Other)
            }
            _ => Self::Other,
        }
    }
}

fn keyword_range(start_offset: TextSize, keyword: &str) -> TextRange {
    TextRange::new(start_offset, start_offset + TextSize::of(keyword))
}
