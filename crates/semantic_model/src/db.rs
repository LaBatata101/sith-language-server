use std::{
    fs::{self},
    hash::BuildHasherDefault,
    ops::Deref,
    path::{Path, PathBuf},
    sync::Arc,
};

use bimap::BiHashMap;
use python_ast::{ModModule, Suite};
use python_ast_utils::nodes::NodeStack;
use python_parser::{parse_module, Parsed};
use python_utils::{PythonHost, ROOT_FILES};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use ruff_index::{newtype_index, Idx};
use ruff_python_resolver::{
    config::Config, execution_environment::ExecutionEnvironment, host::Host,
};
use rustc_hash::{FxHashMap, FxHashSet, FxHasher};
use serde::{Deserialize, Serialize};
use walkdir::WalkDir;

use crate::{
    declaration::{DeclId, Declaration, DeclarationQuery, ImportSource},
    symbol::{Symbol, SymbolId},
    symbol_table::{ImportResolverConfig, SymbolTable, SymbolTableBuilder},
    util,
    vendored::setup_typeshed,
    Scope, ScopeId,
};

pub enum Source<'a> {
    New(&'a str),
    Update { old: &'a str, new: &'a str },
}

type FxBiHashMap<L, R> =
    BiHashMap<L, R, BuildHasherDefault<FxHasher>, BuildHasherDefault<FxHasher>>;

#[newtype_index]
#[derive(Serialize, Deserialize)]
pub struct FileId;

impl Default for FileId {
    fn default() -> Self {
        FileId::new(0)
    }
}

#[derive(Debug, Default, Clone)]
pub struct Files {
    stub_paths: FxBiHashMap<Arc<PathBuf>, FileId>,
    non_stub_paths: FxBiHashMap<Arc<PathBuf>, FileId>,
    indexed: FxHashSet<Arc<PathBuf>>,
    next_id: FileId,
}

impl Files {
    fn contains_file(&self, path: &PathBuf) -> bool {
        self.stub_paths.contains_left(path) || self.non_stub_paths.contains_left(path)
    }

    fn push_file(&mut self, path: Arc<PathBuf>) -> FileId {
        if path.extension().is_some_and(|ext| ext == "pyi") {
            self.push_stub_file(path)
        } else {
            self.push_non_stub_file(path)
        }
    }

    fn push_stub_file(&mut self, path: Arc<PathBuf>) -> FileId {
        self.next_id = FileId::new(self.next_id.as_usize() + 1);
        self.stub_paths
            .insert_no_overwrite(path, self.next_id)
            .expect("Tried to insert repeated value");
        self.next_id
    }

    fn push_non_stub_file(&mut self, path: Arc<PathBuf>) -> FileId {
        self.next_id = FileId::new(self.next_id.as_usize() + 1);
        self.non_stub_paths
            .insert_no_overwrite(path.clone(), self.next_id)
            .expect("Tried to insert repeated value");
        self.next_id
    }

    fn any_path(&self, file_id: &FileId) -> Option<&Arc<PathBuf>> {
        self.stub_path(file_id).or(self.non_stub_path(file_id))
    }

    fn any_file_id(&self, path: &PathBuf) -> Option<FileId> {
        self.stub_file_id(path)
            .or(self.non_stub_file_id(path))
            .copied()
    }

    fn stub_path(&self, file_id: &FileId) -> Option<&Arc<PathBuf>> {
        self.stub_paths.get_by_right(file_id)
    }

    fn stub_file_id(&self, path: &PathBuf) -> Option<&FileId> {
        self.stub_paths.get_by_left(path)
    }

    fn non_stub_path(&self, file_id: &FileId) -> Option<&Arc<PathBuf>> {
        self.non_stub_paths.get_by_right(file_id)
    }

    fn non_stub_file_id(&self, path: &PathBuf) -> Option<&FileId> {
        self.non_stub_paths.get_by_left(path)
    }
}

#[derive(Debug, Clone)]
pub struct BuiltinSymbolTable {
    table: SymbolTable,
    ast: Parsed<ModModule>,
    path: Arc<PathBuf>,
}

impl BuiltinSymbolTable {
    pub fn path(&self) -> &Arc<PathBuf> {
        &self.path
    }

    pub fn suite(&self) -> &Suite {
        self.ast.suite()
    }

    pub fn symbol_table(&self) -> &SymbolTable {
        &self.table
    }

    pub fn scope_id(&self) -> ScopeId {
        ScopeId::global()
    }

    pub fn scope(&self) -> &Scope {
        self.table.scope(self.scope_id()).unwrap()
    }

    pub fn lookup_symbol(&self, name: &str, scope_id: ScopeId) -> Option<&Symbol> {
        self.table.lookup_symbol(name, scope_id).filter(|symbol| {
            self.table
                .declaration(symbol.declarations().last())
                .is_some_and(|declaration| !declaration.is_import())
        })
    }

    pub fn symbol_declaration(&self, name: &str) -> Option<&Declaration> {
        self.table
            .symbol_declaration(name, self.scope_id(), DeclarationQuery::Last)
            .filter(|&declaration| !declaration.is_import())
    }

    pub fn node_stack(&self) -> NodeStack {
        NodeStack::default().build(self.suite())
    }
}

impl Deref for BuiltinSymbolTable {
    type Target = SymbolTable;

    fn deref(&self) -> &Self::Target {
        &self.table
    }
}

impl Default for BuiltinSymbolTable {
    fn default() -> Self {
        Self {
            table: SymbolTable::default(),
            ast: Parsed::default(),
            path: Arc::new(PathBuf::new()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Indexer {
    tables: FxHashMap<FileId, SymbolTable>,
    asts: FxHashMap<FileId, Parsed<ModModule>>,
    files: Files,
    builtin_symbol_table: BuiltinSymbolTable,

    exec_env: ExecutionEnvironment,
    config: Config,
    host: PythonHost,

    was_root_path_indexed: bool,

    collection_stub_path: Arc<PathBuf>,
    builtins_stub_path: Arc<PathBuf>,
}

impl Indexer {
    fn new(root: PathBuf, python_host: PythonHost) -> Self {
        let typeshed_path = setup_typeshed();
        Self {
            tables: FxHashMap::default(),
            asts: FxHashMap::default(),
            files: Files::default(),
            builtin_symbol_table: BuiltinSymbolTable::default(),
            collection_stub_path: Arc::new(typeshed_path.join("stdlib/collections/__init__.pyi")),
            builtins_stub_path: Arc::new(typeshed_path.join("stdlib/builtins.pyi")),
            exec_env: ExecutionEnvironment {
                root,
                python_version: python_host.version,
                python_platform: python_host.platform,
                // TODO: add settings option in the LSP for this
                extra_paths: vec![],
            },
            config: Config {
                // TODO: add settings option in the LSP for this
                typeshed_path: Some(typeshed_path),
                stub_path: None,
                venv_path: None,
                venv: None,
            },
            host: python_host,
            was_root_path_indexed: false,
        }
    }

    /// Collects Python files (*.py) from a given directory path based on project indicators.
    ///
    /// This function scans a directory for Python files with the following behavior:
    /// - If the directory contains any of these project indicators:
    ///   * pyproject.toml
    ///   * requirements.txt
    ///   * .git directory
    ///   * Pipfile
    ///   * setup.py
    ///     then it will recursively scan all subdirectories for Python files.
    /// - If none of these indicators are present, it will only scan the immediate directory
    ///   (non-recursively) for Python files.
    fn scan_dir_for_python_files(root_dir: impl AsRef<Path>) -> Vec<PathBuf> {
        let root_dir = root_dir.as_ref();
        assert!(root_dir.is_dir());

        let mut python_files = Vec::new();

        let should_scan_subdirs = fs::read_dir(root_dir)
            .unwrap()
            .filter_map(|e| e.ok())
            .any(|e| {
                let path = e.path();
                let file_name = path.file_name().map(|f| f.to_string_lossy()).unwrap();
                ROOT_FILES.contains(file_name.as_ref())
            });
        let max_scan_depth = if should_scan_subdirs { usize::MAX } else { 1 };

        for entry in WalkDir::new(root_dir)
            .max_depth(max_scan_depth)
            .into_iter()
            .filter_entry(|e| !e.path().ends_with(".venv"))
            .filter_map(|e| e.ok())
        {
            let path = entry.path();
            if path.extension().is_some_and(|e| e == "py") {
                python_files.push(path.to_path_buf());
            }
        }

        python_files
    }

    fn index_content(
        &self,
        file_id: FileId,
        is_thirdparty: bool,
        content: &str,
    ) -> (SymbolTable, Parsed<ModModule>, Vec<ImportSource>) {
        let path = self.file_path(&file_id);
        let (table, parsed_file) = self.symbol_table_builder(path, file_id, is_thirdparty, content);

        let mut deferred_paths = Vec::new();
        for import_source in table
            .declarations()
            .filter_map(|declaration| declaration.import_source())
            .filter(|import_source| !import_source.is_unresolved())
        {
            deferred_paths.push(import_source.clone());
        }

        (table, parsed_file, deferred_paths)
    }

    pub fn symbol_table_builder(
        &self,
        path: &Path,
        file_id: FileId,
        is_thirdparty: bool,
        content: &str,
    ) -> (SymbolTable, Parsed<ModModule>) {
        let parsed_file = parse_module(content);
        (
            SymbolTableBuilder::new(
                path,
                file_id,
                is_thirdparty,
                ImportResolverConfig::new(&self.exec_env, &self.config, &self.host),
            )
            .build(parsed_file.suite()),
            parsed_file,
        )
    }

    fn index_files(&mut self, mut file_ids: Vec<(FileId, bool)>) {
        loop {
            self.files.indexed.extend(
                file_ids
                    .iter()
                    .map(|(file_id, _)| self.file_path(file_id).clone())
                    .collect::<Vec<_>>(),
            );

            let tables: Vec<_> = file_ids
                .into_par_iter()
                .filter_map(|(file_id, is_thirdparty)| {
                    let path = self.file_path(&file_id);
                    if path.is_file() && path.extension().is_some_and(|ext| ext != "so") {
                        let contents = util::read_to_string(path.as_path())
                            .map_err(|err| {
                                tracing::error!("Failed to read `{}`: {err}", path.display());
                            })
                            .ok()?;

                        let (table, parsed_file, deferred_paths) =
                            self.index_content(file_id, is_thirdparty, &contents);

                        Some((file_id, table, parsed_file, deferred_paths))
                    } else {
                        None
                    }
                })
                .collect();

            // files that will be parsed later
            let mut deferred_file_ids = Vec::new();
            for (file_id, table, parsed_file, deferred_paths) in tables {
                // collect the resolved import paths to be parsed later
                deferred_file_ids.extend(self.push_file_ids(deferred_paths));

                self.tables.insert(file_id, table);
                self.asts.insert(file_id, parsed_file);
            }

            if deferred_file_ids.is_empty() {
                break;
            } else {
                file_ids = deferred_file_ids;
            }
        }
    }

    fn index_builtin_symbols(&mut self) {
        let content = fs::read_to_string(self.builtins_stub_path.as_path())
            .expect("builtins.pyi file not found");

        self.files.indexed.insert(self.builtins_stub_path.clone());
        let file_id = self.push_file(self.builtins_stub_path.clone());
        let (table, parsed_file, _) = self.index_content(file_id, true, &content);

        self.builtin_symbol_table = BuiltinSymbolTable {
            table,
            ast: parsed_file,
            path: self.builtins_stub_path.clone(),
        };
    }

    fn index_collection_types(&mut self) {
        let content = fs::read_to_string(self.collection_stub_path.as_path())
            .expect("collections/__init__.pyi file not found");

        self.files.indexed.insert(self.collection_stub_path.clone());
        let file_id = self.push_file(self.collection_stub_path.clone());
        let (table, parsed_file, _) = self.index_content(file_id, true, &content);
        self.tables.insert(file_id, table);
        self.asts.insert(file_id, parsed_file);
    }

    pub fn tables(&self) -> impl Iterator<Item = (&FileId, &SymbolTable)> {
        self.tables.iter()
    }

    fn push_file(&mut self, file_path: Arc<PathBuf>) -> FileId {
        self.files.push_file(file_path)
    }

    pub fn typeshed_path(&self) -> &PathBuf {
        self.config.typeshed_path.as_ref().unwrap()
    }

    fn was_seen(&self, path: &PathBuf) -> bool {
        self.files.contains_file(path)
    }

    pub fn root_path(&self) -> &Path {
        &self.exec_env.root
    }

    pub fn python_search_paths(&self) -> Vec<PathBuf> {
        self.host.python_search_paths()
    }

    /// Returns the path [`PathBuf`] for a given `file_id`.
    /// # Panics
    /// If there's no [`PathBuf`] for a `file_id`.
    pub fn file_path(&self, file_id: &FileId) -> &Arc<PathBuf> {
        self.files
            .any_path(file_id)
            .unwrap_or_else(|| panic!("no file path for {:?}", file_id))
    }

    pub fn non_stub_path(&self, file_id: &FileId) -> Option<&Arc<PathBuf>> {
        self.files.non_stub_path(file_id)
    }

    /// Returns the [`FileId`] for a given `file_path`.
    /// # Panics
    /// If there's no [`FileId`] for a `file_path`.
    pub fn file_id(&self, file_path: &PathBuf) -> FileId {
        self.files
            .any_file_id(file_path)
            .unwrap_or_else(|| panic!("no `file_id` for {}", file_path.display()))
    }

    /// Returns the AST for the `file_path`, if `file_path` was not indexed
    /// returns `None`.
    pub fn ast(&self, file_path: &PathBuf) -> Option<&Parsed<ModModule>> {
        Some(
            match self
                .files
                .any_file_id(file_path)
                .and_then(|file_id| self.asts.get(&file_id))
            {
                Some(ast) => ast,
                None if file_path.ends_with("stdlib/builtins.pyi") => {
                    &self.builtin_symbol_table.ast
                }
                _ => return None,
            },
        )
    }

    /// Returns the AST for the `file_path`.
    ///
    /// # Panics
    /// If `file_path` was not indexed.
    pub fn ast_or_panic(&self, file_path: &PathBuf) -> &Parsed<ModModule> {
        self.ast(file_path)
            .unwrap_or_else(|| panic!("no ast for path: {}", file_path.display()))
    }

    pub fn node_stack(&self, file: &PathBuf) -> NodeStack {
        let suite = self.ast_or_panic(file).suite();
        NodeStack::default().build(suite)
    }

    pub fn is_indexed(&self, path: &PathBuf) -> bool {
        self.files.indexed.contains(path)
    }

    /// Registers a list of [`ImportSource`] in the indexer, creating the corresponding [`FileId`]
    /// for the stub or non-stub path. If [`ImportSource`] has both stub and non-stub path, the
    /// [`FileId`] created will be associated for both of them.
    ///
    /// Returns a list of `(FileId, bool)`, where:
    /// - `FileId` is the unique id for the registered path
    /// - `bool` indicates whether the path is third-party (from the stdlib or site-packages)
    fn push_file_ids(&mut self, paths: Vec<ImportSource>) -> Vec<(FileId, bool)> {
        paths
            .into_iter()
            .filter_map(|import_source| {
                // Skip if both paths are None
                if import_source.stub.is_none() && import_source.non_stub.is_none() {
                    return None;
                }

                // Case 1: We have a stub path (.pyi file)
                if let Some(stub_path) = import_source.stub {
                    // Skip if already indexed
                    if self.was_seen(&stub_path) {
                        return None;
                    }

                    // Register the stub path
                    let file_id = self.files.push_stub_file(stub_path);

                    // If we also have a non-stub path, associate it with the same file ID
                    if let Some(non_stub_path) = import_source.non_stub {
                        // Associate the non-stub path with the same file ID if not already indexed
                        if !self.was_seen(&non_stub_path) {
                            self.files
                                .non_stub_paths
                                .insert_no_overwrite(non_stub_path, file_id)
                                .expect("Tried to insert duplicated value");
                        }
                    }

                    return Some((file_id, import_source.is_thirdparty));
                }

                // Case 2: We only have a non-stub path (.py file)
                if let Some(non_stub_path) = import_source.non_stub {
                    // Skip if already indexed
                    if self.was_seen(&non_stub_path) {
                        return None;
                    }

                    // Register the non-stub path
                    let file_id = self.files.push_non_stub_file(non_stub_path);
                    return Some((file_id, import_source.is_thirdparty));
                }

                unreachable!()
            })
            .collect()
    }

    pub fn add_or_update_file(&mut self, file_path: PathBuf, source: Source) {
        let deferred_paths = match source {
            Source::New(source) => {
                if self.was_seen(&file_path) {
                    return;
                }

                let path = Arc::new(file_path);
                self.files.indexed.insert(path.clone());
                let file_id = self.push_file(path);
                let (table, parsed_file, mut deferred_paths) =
                    self.index_content(file_id, false, source);

                self.tables.insert(file_id, table);
                self.asts.insert(file_id, parsed_file);

                if !self.was_root_path_indexed {
                    self.was_root_path_indexed = true;

                    let python_files = Indexer::scan_dir_for_python_files(&self.exec_env.root);
                    deferred_paths.extend(python_files.into_iter().map(|path| ImportSource {
                        stub: None,
                        non_stub: Some(Arc::new(path)),
                        is_thirdparty: false,
                    }));
                }

                deferred_paths
            }
            Source::Update { new, .. } => {
                let file_id = self.file_id(&file_path);
                let (table, parsed_file, deferred_paths) = self.index_content(file_id, false, new);

                self.tables.insert(file_id, table);
                self.asts.insert(file_id, parsed_file);

                deferred_paths
            }
        };

        let file_ids = self.push_file_ids(deferred_paths);
        self.index_files(file_ids);
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTableDb {
    index: Indexer,
}

impl SymbolTableDb {
    pub fn new(root: PathBuf, python_host: PythonHost) -> Self {
        Self {
            index: Indexer::new(root, python_host),
        }
    }

    pub fn indexer(&self) -> &Indexer {
        &self.index
    }

    pub fn indexer_mut(&mut self) -> &mut Indexer {
        &mut self.index
    }

    pub fn with_builtin_symbols(mut self) -> Self {
        self.indexer_mut().index_builtin_symbols();
        self
    }

    pub fn with_collection_types(mut self) -> Self {
        self.indexer_mut().index_collection_types();
        self
    }

    pub fn builtin_symbols(&self) -> &BuiltinSymbolTable {
        &self.indexer().builtin_symbol_table
    }

    pub fn collection_stub_symbol_table(&self) -> &SymbolTable {
        self.table(self.collection_stub_path())
    }

    pub fn collection_stub_path(&self) -> &PathBuf {
        &self.index.collection_stub_path
    }

    pub fn table(&self, file: &PathBuf) -> &SymbolTable {
        match self.indexer().tables.get(&self.indexer().file_id(file)) {
            Some(table) => table,
            None if file.ends_with("stdlib/builtins.pyi") => &self.builtin_symbols().table,
            _ => panic!("no symbol table for path: {}", file.display()),
        }
    }

    pub fn symbol(&self, file: &PathBuf, symbol_id: SymbolId) -> &Symbol {
        self.table(file)
            .symbol(symbol_id)
            .or_else(|| self.builtin_symbols().symbol(symbol_id))
            .expect("symbol for id")
    }

    pub fn lookup_symbol(&self, file: &PathBuf, name: &str, scope: ScopeId) -> Option<&Symbol> {
        self.table(file).lookup_symbol(name, scope).or_else(|| {
            self.builtin_symbols()
                .lookup_symbol(name, ScopeId::global())
        })
    }

    pub fn symbol_name(&self, file: &PathBuf, symbol_id: SymbolId) -> &str {
        let symbol = self.symbol(file, symbol_id);
        let scope = self.scope(file, symbol.definition_scope());
        scope.symbol_name(symbol_id).unwrap()
    }

    pub fn declaration(&self, file: &PathBuf, decl_id: DeclId) -> &Declaration {
        self.table(file)
            .declaration(decl_id)
            .expect("expect declaration for id")
    }

    pub fn symbol_declaration(
        &self,
        file: &PathBuf,
        name: &str,
        scope_id: ScopeId,
        query: DeclarationQuery,
    ) -> Option<&Declaration> {
        self.table(file)
            .symbol_declaration(name, scope_id, query)
            .or_else(|| self.builtin_symbols().symbol_declaration(name))
    }

    pub fn find_enclosing_scope(&self, file: &PathBuf, offset: u32) -> (ScopeId, &Scope) {
        self.table(file).find_enclosing_scope(offset)
    }

    /// Returns the global [`Scope`] of a `file`.
    pub fn global_scope(&self, file: &PathBuf) -> &Scope {
        self.table(file).scope(ScopeId::global()).unwrap()
    }

    pub fn scope(&self, file: &PathBuf, scope_id: ScopeId) -> &Scope {
        self.table(file).scope(scope_id).expect("scope to exist")
    }

    pub fn scopes(&self, file: &PathBuf) -> impl Iterator<Item = &Scope> {
        self.table(file).scopes.iter()
    }
}
