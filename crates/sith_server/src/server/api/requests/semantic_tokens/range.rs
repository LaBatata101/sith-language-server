use lsp_types::{self as types, request as req};

use crate::edit::RangeExt;
use crate::server::api::Result;
use crate::{
    server::{
        api::{traits::BackgroundDocumentRequestHandler, RequestHandler},
        client::Notifier,
    },
    session::DocumentSnapshot,
};

use super::{compute_semantic_tokens, ComputeSemanticTokenOptions};

pub(crate) struct SemanticTokensRange;

impl RequestHandler for SemanticTokensRange {
    type RequestType = req::SemanticTokensRangeRequest;
}

impl BackgroundDocumentRequestHandler for SemanticTokensRange {
    fn document_url(params: &types::SemanticTokensRangeParams) -> std::borrow::Cow<lsp_types::Url> {
        std::borrow::Cow::Borrowed(&params.text_document.uri)
    }

    fn run_with_snapshot(
        snapshot: DocumentSnapshot,
        _notifier: Notifier,
        params: types::SemanticTokensRangeParams,
    ) -> Result<Option<types::SemanticTokensRangeResult>> {
        Ok(semantic_tokens_range(&snapshot, params))
    }
}

fn semantic_tokens_range(
    snapshot: &DocumentSnapshot,
    params: types::SemanticTokensRangeParams,
) -> Option<types::SemanticTokensRangeResult> {
    let document_path = params.text_document.uri.to_file_path().ok()?;
    let document = snapshot.document();
    let parsed_file = snapshot.db().indexer().ast(&document_path)?;
    let range =
        params
            .range
            .to_text_range(document.contents(), document.index(), snapshot.encoding());

    Some(types::SemanticTokensRangeResult::from(
        compute_semantic_tokens(
            parsed_file,
            document,
            ComputeSemanticTokenOptions::InRange(range),
        ),
    ))
}
