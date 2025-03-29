use lsp_types::{self as types, request as req, Url};
use ruff_text_size::TextSize;

use crate::server::api::requests::semantic_tokens::SemanticTokenBuilder;
use crate::server::api::Result;
use crate::{
    server::{
        api::{traits::BackgroundDocumentRequestHandler, RequestHandler},
        client::Notifier,
    },
    session::DocumentSnapshot,
};

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
        Ok(semantic_tokens_full(&snapshot, params.text_document.uri))
    }
}

pub(super) fn semantic_tokens_full(
    snapshot: &DocumentSnapshot,
    uri: Url,
) -> Option<types::SemanticTokensResult> {
    let document_path = uri.to_file_path().ok()?;

    let db = snapshot.db();
    let document_suite = db.indexer().ast(&document_path)?.suite();
    let document = snapshot.document();
    let index = document.index();

    let mut tokens =
        SemanticTokenBuilder::new(db.indexer().ast(&document_path)?.tokens()).build(document_suite);
    tokens.sort_by_key(|t| t.start);

    let mut prev_line = 0;
    let mut prev_start = 0;
    let data = tokens
        .into_iter()
        .map(|token| {
            let location = index.source_location(TextSize::from(token.start), document.contents());
            let line = location.row.to_zero_indexed();
            let column = location.column.to_zero_indexed();

            let delta_line = line - prev_line;
            let delta_start = if delta_line == 0 {
                column - prev_start
            } else {
                column
            };
            let result = types::SemanticToken {
                delta_line: delta_line as u32,
                delta_start: delta_start as u32,
                length: token.length,
                token_type: token.token_type,
                token_modifiers_bitset: 0,
            };

            prev_line = line;
            prev_start = column;

            result
        })
        .collect::<Vec<_>>();

    Some(types::SemanticTokensResult::Tokens(types::SemanticTokens {
        result_id: None,
        data,
    }))
}
