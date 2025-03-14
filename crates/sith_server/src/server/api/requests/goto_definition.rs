use std::path::PathBuf;
use std::sync::Arc;

use lsp_types::{self as types, request as req, Url};
use python_ast::{AnyNodeRef, Arguments};
use python_ast_utils::nodes::Nodes;
use python_ast_utils::{identifier_from_node, node_at_offset};
use ruff_source_file::LineIndex;
use ruff_text_size::Ranged;
use semantic_model::declaration::{Declaration, DeclarationQuery, ImportSource};
use semantic_model::type_inference::TypeInferer;
use semantic_model::type_inference::{PythonType, ResolvedType};
use semantic_model::ScopeId;
use semantic_model::{self as sm, db::SymbolTableDb};
use types::GotoDefinitionResponse;

use crate::edit::{position_to_offset, ToLocation};
use crate::server::api::LSPResult;
use crate::server::{client::Notifier, Result};
use crate::session::DocumentSnapshot;

pub(crate) struct GotoDefinition;

impl super::RequestHandler for GotoDefinition {
    type RequestType = req::GotoDefinition;
}

impl super::BackgroundDocumentRequestHandler for GotoDefinition {
    fn document_url(params: &types::GotoDefinitionParams) -> std::borrow::Cow<lsp_types::Url> {
        std::borrow::Cow::Borrowed(&params.text_document_position_params.text_document.uri)
    }

    fn run_with_snapshot(
        snapshot: DocumentSnapshot,
        _: Notifier,
        params: types::GotoDefinitionParams,
    ) -> Result<Option<types::GotoDefinitionResponse>> {
        let document_path = Arc::new(
            snapshot
                .url()
                .to_file_path()
                .map_err(|_| anyhow::anyhow!("Failed to convert URL to file path"))
                .with_failure_code(lsp_server::ErrorCode::InternalError)?,
        );

        let db = snapshot.db();
        let node_stack = db.indexer().node_stack(&document_path);

        let document = snapshot.document();

        let position = params.text_document_position_params.position;
        let offset = position_to_offset(document.contents(), &position, document.index());
        let (scope, _) = db.find_enclosing_scope(&document_path, offset);

        let Some((is_python_module, declaration_path, declaration)) =
            find_declaration(db, &document_path, scope, offset, node_stack.nodes())
        else {
            return Ok(None);
        };

        let is_same_document = *declaration_path == *document_path;
        create_location_response(
            is_python_module,
            is_same_document,
            declaration_path,
            declaration,
            &snapshot,
        )
    }
}

enum IsPythonModule {
    Yes,
    No,
}

fn find_declaration<'a>(
    db: &'a SymbolTableDb,
    path: &'a Arc<PathBuf>,
    scope: ScopeId,
    offset: u32,
    nodes: &Nodes,
) -> Option<(IsPythonModule, &'a PathBuf, &'a Declaration)> {
    let node_with_parent = node_at_offset(nodes, offset)?;
    let mut is_python_module = IsPythonModule::No;

    let (path, declaration) = match node_with_parent.node() {
        AnyNodeRef::AttributeExpr(python_ast::AttributeExpr { value, attr, .. }) => {
            let mut type_inferer = TypeInferer::new(db, scope, path.clone());
            let (path, scope) = match type_inferer.infer_expr(value.as_ref(), nodes) {
                ResolvedType::KnownType(PythonType::Class(class)) => {
                    (db.indexer().file_path(&class.file_id), class.body_scope)
                }
                ResolvedType::KnownType(PythonType::Module(import_source)) => {
                    let file_id = db.indexer().file_id(import_source.any_path()?);
                    (db.indexer().file_path(&file_id), ScopeId::global())
                }
                _ => return None,
            };

            let declaration = db.symbol_declaration(path, attr, scope, DeclarationQuery::Last)?;
            (path, declaration)
        }
        // find the declaration of an argument in a call expression
        AnyNodeRef::CallExpr(python_ast::CallExpr {
            func,
            arguments: Arguments { keywords, .. },
            ..
        }) => {
            let arg_name = keywords.iter().find_map(|keyword| {
                let arg = keyword.arg.as_ref()?;
                arg.range().contains_inclusive(offset.into()).then_some(arg)
            })?;
            let mut type_inferer = TypeInferer::new(db, scope, path.clone());
            let (path, scope) = match type_inferer.infer_expr(func.as_ref(), nodes) {
                ResolvedType::KnownType(PythonType::Class(class)) => {
                    let path = db.indexer().file_path(&class.file_id);
                    let constructor_decl = db.symbol_declaration(
                        path,
                        "__init__",
                        class.body_scope,
                        DeclarationQuery::First,
                    )?;
                    (path, constructor_decl.body_scope().unwrap())
                }
                ResolvedType::KnownType(PythonType::Function {
                    file_id,
                    body_scope,
                    ..
                }) => (db.indexer().file_path(&file_id), body_scope),
                _ => return None,
            };
            let declaration =
                db.symbol_declaration(path, arg_name, scope, DeclarationQuery::First)?;
            (path, declaration)
        }
        _ => {
            let symbol_name = identifier_from_node(node_with_parent.node(), offset)?;
            let mut declaration = db.symbol_declaration(
                path,
                symbol_name,
                scope,
                DeclarationQuery::AtOffset(offset),
            )?;
            let mut final_path = db.indexer().file_path(&declaration.file_id);

            // Recursively resolve import chains to find the original symbol declaration.
            // If the symbol isn't found in an import path, assume it refers to a Python module.
            while let Some(source_path) =
                declaration.import_source().and_then(ImportSource::any_path)
            {
                final_path = source_path;
                let Some(imported_declaration) = db.symbol_declaration(
                    source_path,
                    symbol_name,
                    ScopeId::global(),
                    DeclarationQuery::Last,
                ) else {
                    is_python_module = IsPythonModule::Yes;
                    break;
                };
                declaration = imported_declaration;
            }
            (final_path, declaration)
        }
    };
    Some((is_python_module, path, declaration))
}

fn create_location_response(
    is_python_module: IsPythonModule,
    is_same_document: bool,
    path: &PathBuf,
    declaration: &Declaration,
    snapshot: &DocumentSnapshot,
) -> Result<Option<types::GotoDefinitionResponse>> {
    let url = Url::from_file_path(path.as_path())
        .map_err(|_| anyhow::anyhow!("Failed to convert file path to URL"))
        .with_failure_code(lsp_server::ErrorCode::InternalError)?;
    if matches!(is_python_module, IsPythonModule::Yes) {
        return Ok(Some(GotoDefinitionResponse::Scalar(lsp_types::Location {
            uri: url,
            range: lsp_types::Range::default(),
        })));
    }

    let content = if is_same_document {
        snapshot.document().contents()
    } else {
        &sm::util::read_to_string(path)
            .map_err(|e| anyhow::anyhow!("Failed to read {} contents: {e}", path.display()))
            .with_failure_code(lsp_server::ErrorCode::RequestFailed)?
    };
    let index = LineIndex::from_source_text(content);

    Ok(Some(GotoDefinitionResponse::Scalar(
        declaration
            .range
            .to_location(url, content, &index, snapshot.encoding()),
    )))
}
