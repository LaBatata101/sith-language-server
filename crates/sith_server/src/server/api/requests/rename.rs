use std::sync::Arc;

use crate::{
    edit::{position_to_offset, ToRangeExt, WorkspaceEditTracker},
    server::{
        api::requests::references::{DoGlobalSearch, IncludeDeclaration, ReferencesFinder},
        client::Notifier,
        Result,
    },
    session::DocumentSnapshot,
};
use lsp_types::{self as types, request as req, Url};
use python_ast_utils::{identifier_from_node, node_at_offset, nodes::NodeStack};
use ruff_source_file::LineIndex;
use semantic_model::{self as sm, declaration::DeclarationQuery};

pub(crate) struct Rename;

impl super::RequestHandler for Rename {
    type RequestType = req::Rename;
}

impl super::BackgroundDocumentRequestHandler for Rename {
    fn document_url(params: &types::RenameParams) -> std::borrow::Cow<lsp_types::Url> {
        std::borrow::Cow::Borrowed(&params.text_document_position.text_document.uri)
    }

    fn run_with_snapshot(
        snapshot: DocumentSnapshot,
        _notifier: Notifier,
        params: types::RenameParams,
    ) -> Result<Option<types::WorkspaceEdit>> {
        Ok(rename(&snapshot, &params))
    }
}

fn rename(
    snapshot: &DocumentSnapshot,
    params: &types::RenameParams,
) -> Option<types::WorkspaceEdit> {
    let current_file_path = Arc::new(snapshot.url().to_file_path().ok()?);

    let db = snapshot.db();
    let document = snapshot.document();

    let index = document.index();
    let position = params.text_document_position.position;
    let offset = position_to_offset(document.contents(), &position, index);
    let ast = db.indexer().ast_or_panic(&current_file_path);
    let node_stack = NodeStack::default().build(ast.suite());

    let symbol_node = node_at_offset(node_stack.nodes(), offset)?;
    let symbol_name = identifier_from_node(symbol_node, offset)?;

    let (scope_id, _) = db.find_enclosing_scope(&current_file_path, offset);
    let references = ReferencesFinder::new(db, &current_file_path).find(
        symbol_name,
        scope_id,
        symbol_node,
        ast.suite(),
        IncludeDeclaration::Yes,
        DoGlobalSearch::Yes,
    );

    let mut tracker = WorkspaceEditTracker::new(snapshot.resolved_client_capabilities());

    // ignore renaming if the symbol is a module
    if db
        .symbol_declaration(
            &current_file_path,
            symbol_name,
            scope_id,
            DeclarationQuery::Last,
        )
        .is_some_and(|decl| decl.is_imported_module(symbol_name))
    {
        return None;
    }

    for (file_id, reference) in references {
        let file_path = db.indexer().file_path(&file_id);
        let content = if file_path == &current_file_path {
            document.contents()
        } else {
            &sm::util::read_to_string(file_path.as_path()).ok()?
        };
        let index = LineIndex::from_source_text(content);
        let url = Url::from_file_path(file_path.as_path()).ok()?;
        let edits = reference
            .into_iter()
            .map(|range| types::TextEdit {
                new_text: params.new_name.clone(),
                range: range.to_range(content, &index, snapshot.encoding()),
            })
            .collect();
        tracker
            .set_edits_for_document(url, document.version(), edits)
            .unwrap();
    }

    Some(tracker.into_workspace_edit())
}
