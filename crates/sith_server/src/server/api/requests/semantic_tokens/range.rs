use lsp_types::{self as types, request as req};

use crate::server::api::Result;
use crate::{
    server::{
        api::{traits::BackgroundDocumentRequestHandler, RequestHandler},
        client::Notifier,
    },
    session::DocumentSnapshot,
};

use super::full::semantic_tokens_full;

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
        // TODO: get the tokens and ast nodes that are contained in the range
        Ok(
            None, // semantic_tokens_full(&snapshot, params.text_document.uri).map(|result| {
                 //     let types::SemanticTokensResult::Tokens(semantic_tokens) = result else {
                 //         unreachable!()
                 //     };
                 //     types::SemanticTokensRangeResult::from(semantic_tokens)
                 // }),
        )
    }
}
