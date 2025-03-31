use lsp_types::{self as types, request as req};

use crate::server::api::Result;
use crate::{
    server::{
        api::{traits::BackgroundDocumentRequestHandler, RequestHandler},
        client::Notifier,
    },
    session::DocumentSnapshot,
};

use super::{compute_semantic_tokens, ComputeSemanticTokenOptions};

pub(crate) struct SemanticTokensFull;

impl RequestHandler for SemanticTokensFull {
    type RequestType = req::SemanticTokensFullRequest;
}

impl BackgroundDocumentRequestHandler for SemanticTokensFull {
    fn document_url(params: &types::SemanticTokensParams) -> std::borrow::Cow<lsp_types::Url> {
        std::borrow::Cow::Borrowed(&params.text_document.uri)
    }

    fn run_with_snapshot(
        snapshot: DocumentSnapshot,
        _notifier: Notifier,
        params: types::SemanticTokensParams,
    ) -> Result<Option<types::SemanticTokensResult>> {
        Ok(semantic_tokens_full(&snapshot, params))
    }
}

pub(super) fn semantic_tokens_full(
    snapshot: &DocumentSnapshot,
    params: types::SemanticTokensParams,
) -> Option<types::SemanticTokensResult> {
    let document_path = params.text_document.uri.to_file_path().ok()?;

    let db = snapshot.db();
    let document = snapshot.document();
    let parsed_file = db.indexer().ast(&document_path)?;

    Some(types::SemanticTokensResult::Tokens(
        compute_semantic_tokens(parsed_file, document, ComputeSemanticTokenOptions::Full),
    ))
}
