use std::{
    hash::Hash,
    path::{Path, PathBuf},
    sync::Arc,
};

use compact_str::CompactString;
use lsp_types::{
    self as types,
    request::{self as req},
    CompletionItem, CompletionItemKind,
};
use ruff_source_file::LineIndex;
use ruff_text_size::Ranged;
use rustc_hash::FxHashSet;
use serde::{Deserialize, Serialize};
use sith_python_ast::{self as ast, AnyNodeRef, Expr};
use sith_python_ast_utils::{
    node_at_offset, node_at_row,
    nodes::{NodeId, NodeStack, NodeWithParent, Nodes},
};
use sith_python_parser::{TokenKind, Tokens};
use sith_python_utils::get_python_module_names_in_path;
use sith_semantic_model::{
    builtins::BUILTIN_KEYWORDS,
    db::SymbolTableDb,
    indexer::FileId,
    declaration::{DeclId, DeclStmt, DeclarationKind, DeclarationQuery},
    mro::compute_mro,
    type_inference::{PythonType, ResolvedType, TypeInferer},
    Scope, ScopeId, ScopeKind, Symbol,
};

use crate::{
    edit::position_to_offset,
    server::{client::Notifier, Result},
    session::DocumentSnapshot,
};

pub(super) mod resolve;

#[derive(Debug)]
struct PositionCtx<'a> {
    scope: ScopeId,
    kind: PositionCtxKind<'a>,
}

impl<'a> PositionCtx<'a> {
    fn new(scope: ScopeId, kind: PositionCtxKind<'a>) -> Self {
        Self { scope, kind }
    }

    fn module() -> Self {
        Self {
            scope: ScopeId::global(),
            kind: PositionCtxKind::Module,
        }
    }
}

#[derive(Debug)]
enum PositionCtxKind<'node> {
    Module,
    Class,
    Function,
    ParameterName,
    TypeParamAnnotation,
    Import {
        has_segments: bool,
        /// All import segments before the text cursor
        prev_segments: Vec<&'node str>,
    },
    ImportFromSegment {
        level: u32,
        /// All import segments before the text cursor
        prev_segments: Vec<&'node str>,
    },
    ImportFromName {
        level: u32,
        last_segment: Option<&'node str>,
    },
    AttrAccess(ResolvedType),
    Call {
        file_id: FileId,
        node_id: NodeId,
    },
    String,
    FString,
}

#[derive(Debug, Serialize, Deserialize)]
struct CompletionItemData {
    document_uri: PathBuf,
    payload: Option<CompletionItemDataPayload>,
}

impl CompletionItemData {
    fn payload(self) -> Option<CompletionItemDataPayload> {
        self.payload
    }
}

/// Type that represents where the completion item was originated.
#[derive(Debug, Serialize, Deserialize, Eq, PartialEq, Clone, Copy)]
enum CompletionItemOrigin {
    Class,
    Module,
    Function,
    Lambda,
    Comprehension,
    /// Special-case for builtin symbols
    Builtin,
}

impl From<ScopeKind> for CompletionItemOrigin {
    fn from(kind: ScopeKind) -> Self {
        match kind {
            ScopeKind::Module => Self::Module,
            ScopeKind::Class => Self::Class,
            ScopeKind::Function => Self::Function,
            ScopeKind::Lambda => Self::Lambda,
            ScopeKind::Comprehension => Self::Comprehension,
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Eq, PartialEq)]
struct CompletionItemSymbolData {
    file_id: FileId,
    declaration_id: DeclId,
    origin: CompletionItemOrigin,
}

impl CompletionItemSymbolData {
    fn origin(&self) -> CompletionItemOrigin {
        self.origin
    }

    fn file_id(&self) -> FileId {
        self.file_id
    }

    fn declaration_id(&self) -> DeclId {
        self.declaration_id
    }
}

#[derive(Debug, Serialize, Deserialize, Eq, PartialEq)]
struct CompletionItemModuleData {
    path: PathBuf,
}

impl CompletionItemModuleData {
    fn path(&self) -> &PathBuf {
        &self.path
    }
}

#[derive(Debug, Serialize, Deserialize, Eq, PartialEq)]
enum CompletionItemDataPayload {
    Module(CompletionItemModuleData),
    Symbol(CompletionItemSymbolData),
}

#[derive(Debug, Eq)]
struct CompletionItemCandidate {
    label: CompactString,
    kind: CompletionItemKind,
    data: Option<CompletionItemDataPayload>,
}

impl Hash for CompletionItemCandidate {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.label.hash(state);
        self.kind.hash(state);
    }
}

impl PartialEq for CompletionItemCandidate {
    fn eq(&self, other: &Self) -> bool {
        self.label == other.label && self.kind == other.kind
    }
}

impl CompletionItemCandidate {
    fn builtin(
        name: impl AsRef<str>,
        kind: CompletionItemKind,
        file_id: FileId,
        declaration_id: DeclId,
    ) -> Self {
        Self {
            label: CompactString::new(name),
            kind,
            data: Some(CompletionItemDataPayload::Symbol(
                CompletionItemSymbolData {
                    origin: CompletionItemOrigin::Builtin,
                    file_id,
                    declaration_id,
                },
            )),
        }
    }

    fn module(module_name: impl AsRef<str>, module_path: PathBuf) -> Self {
        Self {
            label: CompactString::new(module_name),
            kind: CompletionItemKind::MODULE,
            data: Some(CompletionItemDataPayload::Module(
                CompletionItemModuleData { path: module_path },
            )),
        }
    }

    fn keyword(keyword: impl AsRef<str>) -> Self {
        Self {
            label: CompactString::new(keyword),
            kind: CompletionItemKind::KEYWORD,
            data: None,
        }
    }

    fn parameter(name: impl AsRef<str>) -> Self {
        Self {
            label: CompactString::new(format!("{}=", name.as_ref())),
            kind: CompletionItemKind::VARIABLE,
            data: None,
        }
    }

    fn sort_text(&self) -> String {
        format!("{:?}", self.kind)
    }
}

fn position_context<'nodes>(
    db: &SymbolTableDb,
    path: &Arc<PathBuf>,
    scope_id: ScopeId,
    tokens: &Tokens,
    nodes: &'nodes Nodes,
    offset: u32,
    index: &LineIndex,
) -> PositionCtx<'nodes> {
    // When no node is found at the given offset, return `PositionCtx::Module` since the user
    // might be requesting completion on a blank line.
    let Some(node) = node_at_offset(nodes, offset).or_else(|| node_at_row(nodes, offset, index))
    else {
        return PositionCtx::module();
    };

    node_position_ctx(node, db, path, scope_id, tokens, nodes, offset)
}

fn node_position_ctx<'nodes>(
    node: &NodeWithParent<'nodes>,
    db: &SymbolTableDb,
    path: &Arc<PathBuf>,
    scope_id: ScopeId,
    tokens: &Tokens,
    nodes: &'nodes Nodes,
    offset: u32,
) -> PositionCtx<'nodes> {
    match node.node() {
        AnyNodeRef::StmtClassDef(ast::ClassDefStmt { name, .. }) => {
            let Some(declaration) = db.symbol_declaration(
                path,
                name,
                scope_id,
                DeclarationQuery::AtOffset(name.range.start().to_u32()),
            ) else {
                unreachable!()
            };

            PositionCtx::new(declaration.body_scope().unwrap(), PositionCtxKind::Class)
        }
        AnyNodeRef::StmtFunctionDef(ast::FunctionDefStmt { name, .. }) => {
            let Some(declaration) = db.symbol_declaration(
                path,
                name,
                scope_id,
                DeclarationQuery::AtOffset(name.range.start().to_u32()),
            ) else {
                unreachable!()
            };

            PositionCtx::new(declaration.body_scope().unwrap(), PositionCtxKind::Function)
        }
        AnyNodeRef::AttributeExpr(ast::AttributeExpr { value, .. }) => {
            let (scope_id, _) = db.find_enclosing_scope(path, offset);
            let mut type_inferer = TypeInferer::new(db, scope_id, path.clone());
            PositionCtx::new(
                scope_id,
                PositionCtxKind::AttrAccess(type_inferer.infer_expr(value.as_ref(), nodes)),
            )
        }
        AnyNodeRef::StmtImport(_) => PositionCtx::new(
            scope_id,
            PositionCtxKind::Import {
                has_segments: false,
                prev_segments: vec![],
            },
        ),
        AnyNodeRef::Alias(ast::Alias { name, .. }) => {
            let Some(parent_node) = node.parent_id().and_then(|id| nodes.get(id)) else {
                unreachable!()
            };
            match parent_node.node() {
                // Collects all segments before the cursor position
                // Example: in `import foo.bar.|` (where | is cursor)
                //   * prev_segments would contain ["foo", "bar"]
                //   * has_segments would be true due to dots
                AnyNodeRef::StmtImport(_) => {
                    let mut prev_segments = Vec::new();
                    let module_start = name.range.start().to_usize();
                    let mut position = 0;
                    for segment in name.split('.').filter(|s| !s.is_empty()) {
                        position += segment.len() + 1;
                        if module_start + position <= offset as usize {
                            prev_segments.push(segment);
                        }
                    }

                    PositionCtx::new(
                        scope_id,
                        PositionCtxKind::Import {
                            has_segments: name.contains('.'),
                            prev_segments,
                        },
                    )
                }
                AnyNodeRef::StmtImportFrom(ast::ImportFromStmt { level, module, .. }) => {
                    PositionCtx::new(
                        scope_id,
                        PositionCtxKind::ImportFromName {
                            level: *level,
                            last_segment: module
                                .as_ref()
                                .and_then(|module| module.split(".").last()),
                        },
                    )
                }
                _ => unreachable!(),
            }
        }
        AnyNodeRef::StmtImportFrom(ast::ImportFromStmt {
            module,
            level,
            range,
            ..
        }) => {
            let mut is_cursor_after_import = false;
            for token in tokens
                .iter()
                .filter(|token| range.contains_range(token.range()))
            {
                match token.kind() {
                    TokenKind::Newline => break,
                    TokenKind::Import if offset > token.range().end().to_u32() => {
                        is_cursor_after_import = true
                    }
                    _ => {}
                }
            }

            // Handles the case where the text cursor is after the `import` keyword, e.g.
            // `from foo import <cursor>`.
            if is_cursor_after_import {
                PositionCtx::new(
                    scope_id,
                    PositionCtxKind::ImportFromName {
                        level: *level,
                        last_segment: module.as_ref().and_then(|module| module.split(".").last()),
                    },
                )
            }
            // Handles the case where the text cursor is after an import segment, e.g.
            // `from foo.<cursor>`
            else {
                let mut prev_segments = Vec::new();
                if let Some(module) = module {
                    let module_start = module.range.start().to_usize();
                    let mut position = 0;
                    for segment in module.split('.').filter(|s| !s.is_empty()) {
                        position += segment.len() + 1;
                        if module_start + position <= offset as usize {
                            prev_segments.push(segment);
                        }
                    }
                }

                PositionCtx::new(
                    scope_id,
                    PositionCtxKind::ImportFromSegment {
                        level: *level,
                        prev_segments,
                    },
                )
            }
        }
        AnyNodeRef::Parameters(ast::Parameters { args, .. })
            if args
                .iter()
                .any(|pwd| is_in_type_param_annotation(offset, &pwd.parameter)) =>
        {
            PositionCtx::new(scope_id, PositionCtxKind::TypeParamAnnotation)
        }
        AnyNodeRef::Parameter(param) if is_in_type_param_annotation(offset, param) => {
            PositionCtx::new(scope_id, PositionCtxKind::TypeParamAnnotation)
        }
        AnyNodeRef::Parameter(_) => PositionCtx::new(scope_id, PositionCtxKind::ParameterName),
        AnyNodeRef::NameExpr(_) => {
            let Some(parent_node) = node.parent_id().and_then(|node_id| nodes.get(node_id)) else {
                return PositionCtx::module();
            };

            match parent_node.node() {
                AnyNodeRef::ExprStmt(_) => {
                    let Some(granparent_node) = parent_node
                        .parent_id()
                        .and_then(|node_id| nodes.get(node_id))
                    else {
                        return PositionCtx::module();
                    };
                    node_position_ctx(granparent_node, db, path, scope_id, tokens, nodes, offset)
                }
                _ => node_position_ctx(parent_node, db, path, scope_id, tokens, nodes, offset),
            }
        }
        AnyNodeRef::CallExpr(ast::CallExpr { func, .. }) => {
            get_call_expr_position_ctx(func, db, path, scope_id, nodes)
        }
        AnyNodeRef::FStringExpressionElement(_) => {
            PositionCtx::new(scope_id, PositionCtxKind::FString)
        }
        AnyNodeRef::StringLiteralExpr(_) | AnyNodeRef::FStringLiteralElement(_) => {
            PositionCtx::new(scope_id, PositionCtxKind::String)
        }
        _ => PositionCtx::module(),
    }
}

/// Check if the text cursor is in the type annotation part of the parameter.
/// The first part of the boolean expression checks if the text cursor is after
/// `:` character. The second part is to avoid returning `TypeParamAnnotation`
/// when the text cursor is the parameter name. The `Parameter` range will be
/// bigger because of the `:` character.
fn is_in_type_param_annotation(offset: u32, param: &ast::Parameter) -> bool {
    offset > param.name.range.end().to_u32() + 1 && param.name.range().end() < param.range.end()
}

fn get_call_expr_position_ctx<'nodes>(
    func: &Expr,
    db: &SymbolTableDb,
    path: &Arc<PathBuf>,
    scope_id: ScopeId,
    nodes: &'nodes Nodes,
) -> PositionCtx<'nodes> {
    let mut type_inferer = TypeInferer::new(db, scope_id, path.clone());
    match type_inferer.infer_expr(func, nodes) {
        ResolvedType::KnownType(PythonType::Class(class)) => {
            let Some((_, symbol)) = class.lookup(db, "__init__") else {
                return PositionCtx::module();
            };
            let class_file = db.indexer().file_path(&class.file_id);
            // TODO: handle multiple constructors
            let declaration = db.declaration(class_file, symbol.declarations().first());
            PositionCtx::new(
                scope_id,
                PositionCtxKind::Call {
                    file_id: class.file_id,
                    node_id: declaration.node_id,
                },
            )
        }
        ResolvedType::KnownType(PythonType::Function {
            file_id, node_id, ..
        }) => PositionCtx::new(scope_id, PositionCtxKind::Call { file_id, node_id }),
        _ => PositionCtx::module(),
    }
}

fn get_completion_item_kind(
    db: &SymbolTableDb,
    path: &PathBuf,
    symbol: &Symbol,
) -> Option<CompletionItemKind> {
    let decl_id = symbol.declarations().last();
    let declaration = db.declaration(path, decl_id);

    Some(match declaration.kind {
        DeclarationKind::Stmt(DeclStmt::Function(_)) => CompletionItemKind::FUNCTION,
        DeclarationKind::Stmt(DeclStmt::Method(_, _)) => CompletionItemKind::METHOD,
        DeclarationKind::Stmt(DeclStmt::Class(_)) => CompletionItemKind::CLASS,
        DeclarationKind::Stmt(
            DeclStmt::Assignment
            | DeclStmt::AnnAssign
            | DeclStmt::Import { .. }
            | DeclStmt::ImportAlias(..),
        ) => {
            if symbol.is_constant() {
                CompletionItemKind::CONSTANT
            } else {
                CompletionItemKind::VARIABLE
            }
        }
        DeclarationKind::Parameter
        | DeclarationKind::InstanceParameter(_)
        | DeclarationKind::WithItem
        | DeclarationKind::Exception
        | DeclarationKind::Named
        | DeclarationKind::PatternMatch
        | DeclarationKind::For => CompletionItemKind::VARIABLE,
        _ => return None,
    })
}

fn get_completion_candidates_from_scope<'a>(
    db: &'a SymbolTableDb,
    path: &'a PathBuf,
    scope: &'a Scope,
) -> impl Iterator<Item = CompletionItemCandidate> + use<'a> {
    // TODO: handle multiple symbol declarations
    let file_id = db.indexer().file_id(path);
    scope
        .symbols()
        .map(|(symbol_name, symbol_id)| (symbol_name, db.symbol(path, *symbol_id)))
        .filter_map(move |(symbol_name, symbol)| {
            if symbol.is_module() {
                let declaration = db.declaration(path, symbol.declarations().last());
                Some(CompletionItemCandidate::module(
                    symbol_name,
                    declaration.import_source()?.non_stub_path()?.to_path_buf(),
                ))
            } else {
                get_completion_item_kind(db, path, symbol).map(|kind| CompletionItemCandidate {
                    label: symbol_name.clone(),
                    kind,
                    data: Some(CompletionItemDataPayload::Symbol(
                        CompletionItemSymbolData {
                            file_id,
                            declaration_id: symbol.declarations().last(),
                            origin: CompletionItemOrigin::from(scope.kind()),
                        },
                    )),
                })
            }
        })
}

fn get_completion_candidates_from_scope_and_parents<'a>(
    db: &'a SymbolTableDb,
    path: &'a PathBuf,
    scope: &'a Scope,
) -> Vec<CompletionItemCandidate> {
    let mut candidates: Vec<_> = get_completion_candidates_from_scope(db, path, scope).collect();
    // add the symbols from the parent scope until it reaches the global scope
    for parent_scope in scope.parent_scopes(db, path) {
        candidates.extend(get_completion_candidates_from_scope(db, path, parent_scope));
    }
    candidates
}

fn get_thirdparty_and_builtin_modules_candidates(
    db: &SymbolTableDb,
) -> FxHashSet<CompletionItemCandidate> {
    let mut module_names = FxHashSet::default();
    for search_path in db.indexer().python_search_paths() {
        module_names.extend(
            get_python_module_names_in_path(search_path)
                .into_iter()
                .map(|(module_name, module_path)| {
                    CompletionItemCandidate::module(module_name, module_path)
                }),
        );
    }
    module_names
}

fn get_python_module_candidates(path: impl AsRef<Path>) -> FxHashSet<CompletionItemCandidate> {
    get_python_module_names_in_path(path)
        .into_iter()
        .map(|(module_name, module_path)| CompletionItemCandidate::module(module_name, module_path))
        .collect()
}

fn builtin_completion_candidates(
    db: &SymbolTableDb,
) -> impl Iterator<Item = CompletionItemCandidate> + use<'_> {
    let file_id = db.indexer().file_id(db.builtin_symbols().path());
    db.builtin_symbols()
        .scope()
        .symbols()
        .map(|(symbol_name, &symbol_id)| {
            (symbol_name, db.builtin_symbols().symbol(symbol_id).unwrap())
        })
        .filter(|(_, symbol)| !symbol.is_private())
        .filter_map(move |(symbol_name, symbol)| {
            // TODO: handle symbol with multiple declarations
            let declaration_id = symbol.declarations().last();
            let declaration = db.builtin_symbols().declaration(declaration_id).unwrap();

            // don't show imported symbols from the builtin.pyi file
            if declaration.is_import() {
                None
            } else {
                get_completion_item_kind(db, db.builtin_symbols().path(), symbol).map(|item_kind| {
                    CompletionItemCandidate::builtin(
                        symbol_name,
                        item_kind,
                        file_id,
                        declaration_id,
                    )
                })
            }
        })
}

fn get_completion_candidates(
    db: &SymbolTableDb,
    pos_ctx: PositionCtx,
    path: &Arc<PathBuf>,
    scope: ScopeId,
) -> Option<Vec<CompletionItem>> {
    let mut completion_candidates = FxHashSet::default();

    if matches!(pos_ctx.kind, PositionCtxKind::String) {
        return None;
    }

    // Don't show the bultin symbols in the following contexts
    if !matches!(
        pos_ctx.kind,
        PositionCtxKind::Import { .. }
            | PositionCtxKind::ImportFromName { .. }
            | PositionCtxKind::ImportFromSegment { .. }
            | PositionCtxKind::AttrAccess(_)
    ) {
        completion_candidates.extend(builtin_completion_candidates(db));
    }

    // Whether to show the keywords
    if matches!(
        pos_ctx.kind,
        PositionCtxKind::Module | PositionCtxKind::Class | PositionCtxKind::Function
    ) {
        completion_candidates.extend(
            BUILTIN_KEYWORDS
                .iter()
                .map(CompletionItemCandidate::keyword),
        );
    }

    match pos_ctx.kind {
        PositionCtxKind::Module => {
            completion_candidates.extend(get_completion_candidates_from_scope(
                db,
                path,
                db.global_scope(path),
            ));
        }
        PositionCtxKind::FString => {
            completion_candidates.extend(get_completion_candidates_from_scope_and_parents(
                db,
                path,
                db.scope(path, pos_ctx.scope),
            ));
        }
        PositionCtxKind::TypeParamAnnotation => {
            let scope = db.scope(path, pos_ctx.scope);
            // add the symbols from the parent scope until it reaches the global scope
            for parent_scope in scope.parent_scopes(db, path) {
                completion_candidates.extend(get_completion_candidates_from_scope(
                    db,
                    path,
                    parent_scope,
                ));
            }
        }
        PositionCtxKind::Function | PositionCtxKind::Class => {
            let func_scope = db.scope(path, pos_ctx.scope);
            completion_candidates.extend(get_completion_candidates_from_scope_and_parents(
                db, path, func_scope,
            ));
        }
        PositionCtxKind::Import {
            has_segments,
            prev_segments,
        } => {
            if has_segments {
                let last_segment = prev_segments.last().unwrap();
                let declaration =
                    db.symbol_declaration(path, last_segment, scope, DeclarationQuery::Last)?;
                let parent = declaration.import_source()?.any_path()?.parent()?;
                completion_candidates = get_python_module_candidates(parent);
            } else {
                completion_candidates = get_thirdparty_and_builtin_modules_candidates(db);
                // show the modules defined in the project
                completion_candidates
                    .extend(get_python_module_candidates(db.indexer().root_path()));
            }
        }
        PositionCtxKind::ImportFromSegment {
            level,
            prev_segments,
        } => {
            // The text cursor is in one of these cases:
            // Ex) Relative import: `from .foo.bar<cursor>` or `from .<cursor>`
            if level > 0 {
                let mut parent_dir = Path::new("");
                for _ in 0..level {
                    let Some(path) = path.parent() else {
                        break;
                    };
                    parent_dir = path;
                }

                if prev_segments.is_empty() {
                    completion_candidates = get_python_module_candidates(parent_dir);
                } else {
                    completion_candidates =
                        get_python_module_candidates(parent_dir.join(prev_segments.join("/")));
                }
            // Ex) Absolute import: `from foo.bar<cursor>`
            } else if !prev_segments.is_empty() {
                let segment = prev_segments.last()?;
                let declaration =
                    db.symbol_declaration(path, segment, scope, DeclarationQuery::Last)?;
                let source_path = declaration.import_source()?.any_path()?;

                if source_path.ends_with("__init__.py") || source_path.ends_with("__init__.pyi") {
                    completion_candidates = get_python_module_candidates(source_path.parent()?);
                } else {
                    return None;
                }
            // Ex) `from <cursor>`
            } else {
                completion_candidates = get_python_module_candidates(db.indexer().root_path());
                completion_candidates.extend(get_thirdparty_and_builtin_modules_candidates(db));
            }
        }
        PositionCtxKind::ImportFromName {
            level,
            last_segment,
        } => {
            // Handles relative imports:
            // Ex) `from . import foo<cursor>` or `from .foo import bar<cursor>`
            if last_segment.is_none() && level > 0 {
                let mut parent_dir = Path::new("");
                for _ in 0..level {
                    let Some(path) = path.parent() else {
                        break;
                    };
                    parent_dir = path;
                }

                completion_candidates = get_python_module_candidates(parent_dir);
            }
            // Handles absolute imports:
            // Ex) `from foo import bar<cursor>`
            else {
                let last_seg = last_segment.unwrap();
                let declaration =
                    db.symbol_declaration(path, last_seg, scope, DeclarationQuery::Last)?;
                let source = declaration.import_source()?.any_path()?;
                completion_candidates =
                    get_completion_candidates_from_scope(db, source, db.global_scope(source))
                        .collect();

                if source.ends_with("__init__.py") || source.ends_with("__init__.pyi") {
                    let parent_dir = source.parent().unwrap();
                    completion_candidates.extend(get_python_module_candidates(parent_dir));
                }
            }
        }
        PositionCtxKind::Call { file_id, node_id } => {
            let path = db.indexer().file_path(&file_id);
            let node_stack = db.indexer().node_stack(path);
            let Some(func_stmt) = node_stack
                .nodes()
                .get(node_id)
                .and_then(|node| node.as_stmt_function_def())
            else {
                unreachable!()
            };

            for param in func_stmt
                .parameters
                .iter_non_variadic_params()
                .map(|pwd| &pwd.parameter)
            {
                completion_candidates.insert(CompletionItemCandidate::parameter(&param.name));
            }

            let scope = db.scope(path, scope);
            completion_candidates.extend(get_completion_candidates_from_scope_and_parents(
                db, path, scope,
            ));
        }
        PositionCtxKind::AttrAccess(resolved_type) => match resolved_type {
            ResolvedType::KnownType(PythonType::Class(class)) => match compute_mro(db, class) {
                Ok(class_bases) => {
                    for class_base in class_bases
                        .into_iter()
                        .filter_map(|class_base| class_base.into_class())
                    {
                        let class_path = db.indexer().file_path(&class_base.file_id);
                        let body_scope = db.scope(class_path, class_base.body_scope);
                        completion_candidates.extend(get_completion_candidates_from_scope(
                            db, class_path, body_scope,
                        ));
                    }
                }
                Err(err_msg) => {
                    let path = db.indexer().file_path(&class.file_id);
                    tracing::error!(
                        "Failed to compute MRO of class ('{}'): {err_msg}",
                        db.symbol_name(path, class.symbol_id)
                    );
                }
            },
            ResolvedType::KnownType(PythonType::Module(import_source)) => {
                let module_path = import_source.any_path()?;
                let scope = db.scope(module_path, ScopeId::global());

                completion_candidates =
                    get_completion_candidates_from_scope(db, module_path, scope).collect();
            }
            _ => return None,
        },
        _ => return None,
    };

    Some(
        completion_candidates
            .into_iter()
            .map(|completion_item_candidate| CompletionItem {
                sort_text: Some(completion_item_candidate.sort_text()),
                label: completion_item_candidate.label.to_string(),
                kind: Some(completion_item_candidate.kind),
                data: Some(
                    serde_json::to_value(CompletionItemData {
                        document_uri: path.to_path_buf(),
                        payload: completion_item_candidate.data,
                    })
                    .expect("no error when serializing completion item data!"),
                ),
                ..Default::default()
            })
            .collect(),
    )
}

pub(crate) struct Completion;

impl super::RequestHandler for Completion {
    type RequestType = req::Completion;
}

impl super::BackgroundDocumentRequestHandler for Completion {
    fn document_url(params: &types::CompletionParams) -> std::borrow::Cow<lsp_types::Url> {
        std::borrow::Cow::Borrowed(&params.text_document_position.text_document.uri)
    }

    fn run_with_snapshot(
        snapshot: DocumentSnapshot,
        _: Notifier,
        params: types::CompletionParams,
    ) -> Result<Option<types::CompletionResponse>> {
        Ok(completion(snapshot, params))
    }
}

pub(crate) fn completion(
    snapshot: DocumentSnapshot,
    params: types::CompletionParams,
) -> Option<types::CompletionResponse> {
    let path = Arc::new(snapshot.url().to_file_path().ok()?);

    let db = snapshot.db();
    let document = snapshot.document();

    let index = document.index();
    let position = params.text_document_position.position;
    let offset = position_to_offset(document.contents(), &position, index);

    let parsed_file = db.indexer().ast_or_panic(&path);
    let node_stack = NodeStack::default().build(parsed_file.suite());

    let (scope, _) = db.find_enclosing_scope(&path, offset);
    let pos_ctx = position_context(
        db,
        &path,
        scope,
        parsed_file.tokens(),
        node_stack.nodes(),
        offset,
        index,
    );

    get_completion_candidates(db, pos_ctx, &path, scope).map(types::CompletionResponse::Array)
}
