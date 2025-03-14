use std::path::PathBuf;

use lsp_types::{
    request::{self as req},
    CompletionItem, Documentation, MarkupContent, Url,
};
use python_ast_utils::nodes::NodeStack;
use python_parser::parse_module;
use python_utils::{get_python_doc, nodes::get_documentation_string_from_node};
use semantic_model::{self as sm, db::SymbolTableDb, declaration::DeclarationQuery, ScopeId};

use crate::server::Result;
use crate::{
    server::api::{
        requests::completion::CompletionItemData,
        traits::{BackgroundDocumentRequestHandler, RequestHandler},
    },
    session::DocumentSnapshot,
};

use super::{CompletionItemDataPayload, CompletionItemOrigin};

pub(crate) struct ResolveCompletionItem;
impl RequestHandler for ResolveCompletionItem {
    type RequestType = req::ResolveCompletionItem;
}

impl BackgroundDocumentRequestHandler for ResolveCompletionItem {
    fn document_url(params: &CompletionItem) -> std::borrow::Cow<lsp_types::Url> {
        let data = params.data.as_ref().expect("CompletionItem data");
        let url = Url::from_file_path(
            data["document_uri"]
                .as_str()
                .expect("document_uri to be a string"),
        )
        .expect("string to be a file path");
        std::borrow::Cow::Owned(url)
    }

    // TODO: check completionItem#resolveSupport client capability for properties that can be filled
    // in this request
    // TODO: show documentation for symbols annotated with `Annotated`
    fn run_with_snapshot(
        snapshot: super::DocumentSnapshot,
        _notifier: super::Notifier,
        mut original_completion: CompletionItem,
    ) -> Result<CompletionItem> {
        original_completion.documentation =
            get_completion_item_documentation(&snapshot, &mut original_completion).map(|doc| {
                Documentation::MarkupContent(MarkupContent {
                    kind: lsp_types::MarkupKind::Markdown,
                    value: doc,
                })
            });

        Ok(original_completion)
    }
}

fn get_completion_item_documentation(
    snapshot: &DocumentSnapshot,
    original_completion: &mut CompletionItem,
) -> Option<String> {
    let data = original_completion.data.take()?;
    let completion_item_data: CompletionItemData =
        serde_json::from_value(data).expect("no error when deserializing!");

    let db = snapshot.db();
    match completion_item_data.payload()? {
        CompletionItemDataPayload::Module(completion_item_module_data) => {
            get_module_documentation(db, completion_item_module_data.path())
        }
        CompletionItemDataPayload::Symbol(completion_item_symbol_data) => {
            match completion_item_symbol_data.origin() {
                CompletionItemOrigin::Builtin => {
                    let interpreter = snapshot.client_settings().interpreter()?;
                    get_python_doc(interpreter, &original_completion.label).unwrap_or_else(|err| {
                        tracing::error!(
                            "Failed to get builtin symbol documentation with Python script: {err}"
                        );
                        None
                    })
                }
                _ => {
                    let non_stub_path = db
                        .indexer()
                        .non_stub_path(&completion_item_symbol_data.file_id())?;
                    if db.indexer().is_indexed(non_stub_path) {
                        let node_stack = db.indexer().node_stack(non_stub_path);
                        let declaration = db.declaration(
                            non_stub_path,
                            completion_item_symbol_data.declaration_id(),
                        );
                        let completion_item_node = node_stack.get(declaration.node_id).unwrap();

                        get_documentation_string_from_node(completion_item_node)
                    } else {
                        let content = sm::util::read_to_string(non_stub_path.as_path()).ok()?;
                        let (table, ast) = db.indexer().symbol_table_builder(
                            non_stub_path,
                            completion_item_symbol_data.file_id(),
                            true,
                            &content,
                        );
                        let declaration = table.symbol_declaration(
                            &original_completion.label,
                            ScopeId::global(),
                            DeclarationQuery::Last,
                        )?;

                        let node_stack = NodeStack::default()
                            .is_thirdparty(
                                completion_item_symbol_data.origin()
                                    == CompletionItemOrigin::Module,
                            )
                            .build(ast.suite());
                        let completion_item_node = node_stack.get(declaration.node_id).unwrap();

                        get_documentation_string_from_node(completion_item_node)
                    }
                }
            }
        }
    }
}

pub(crate) fn get_module_documentation(db: &SymbolTableDb, path: &PathBuf) -> Option<String> {
    // Check if the module was already indexed and get the first string expression
    // statement. Otherwise, we need to read the file contents and parse it.
    let ast = if db.indexer().is_indexed(path) {
        db.indexer().ast_or_panic(path)
    } else {
        let contents = sm::util::read_to_string(path).ok()?;
        &parse_module(&contents)
    };
    ast.suite()
        .first()
        .and_then(|stmt| stmt.as_expr_stmt())
        .and_then(|expr_stmt| expr_stmt.value.as_string_literal_expr())
        .map(|str_expr| str_expr.value.to_string())
}
