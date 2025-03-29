//! Scheduling, I/O, and API endpoints.

use std::num::NonZeroUsize;
use std::str::FromStr;

use connection::Connection;
use connection::ConnectionInitializer;
use lsp_server as lsp;
use lsp_types as types;
use lsp_types::CodeActionKind;
use lsp_types::CodeActionOptions;
use lsp_types::CompletionOptions;
use lsp_types::HoverProviderCapability;
use lsp_types::SignatureHelpOptions;
use types::ClientCapabilities;
use types::DiagnosticOptions;
use types::DidChangeWatchedFilesRegistrationOptions;
use types::FileSystemWatcher;
use types::OneOf;
use types::TextDocumentSyncCapability;
use types::TextDocumentSyncKind;
use types::TextDocumentSyncOptions;
use types::WorkDoneProgressOptions;
use types::WorkspaceFoldersServerCapabilities;

use self::schedule::event_loop_thread;
use self::schedule::Scheduler;
use self::schedule::Task;
use crate::session::AllSettings;
use crate::session::ClientSettings;
use crate::session::Session;
use crate::PositionEncoding;

mod api;
mod client;
mod connection;
mod schedule;

pub(crate) use connection::ClientSender;

pub(crate) type Result<T> = std::result::Result<T, api::Error>;

pub struct Server {
    connection: Connection,
    client_capabilities: ClientCapabilities,
    worker_threads: NonZeroUsize,
    session: Session,
}

impl Server {
    pub fn new(worker_threads: NonZeroUsize) -> crate::Result<Self> {
        let connection = ConnectionInitializer::stdio();

        let (id, init_params) = connection.initialize_start()?;

        let client_capabilities = init_params.capabilities;
        let position_encoding = Self::find_best_position_encoding(&client_capabilities);
        let server_capabilities = Self::server_capabilities(&client_capabilities);
        let connection = connection.initialize_finish(
            id,
            &server_capabilities,
            crate::SERVER_NAME,
            crate::VERSION,
        )?;

        if let Some(trace) = init_params.trace {
            crate::trace::set_trace_value(trace);
        }

        crate::message::init_messenger(connection.make_sender());

        let AllSettings {
            global_settings,
            mut workspace_settings,
        } = AllSettings::from_value(init_params.initialization_options.unwrap_or_default());

        crate::trace::init_tracing(
            connection.make_sender(),
            global_settings
                .tracing
                .log_level
                .unwrap_or(crate::trace::LogLevel::Info),
            global_settings
                .tracing
                .log_file
                .as_deref()
                .filter(|path| path.is_file()),
            init_params.client_info.as_ref(),
        );

        let mut workspace_for_url = |url: lsp_types::Url| {
            let Some(workspace_settings) = workspace_settings.as_mut() else {
                return (url, ClientSettings::default());
            };
            let settings = workspace_settings.remove(&url).unwrap_or_else(|| {
                tracing::warn!("No workspace settings found for {}", url);
                ClientSettings::default()
            });
            (url, settings)
        };

        let workspaces = init_params
            .workspace_folders
            .filter(|folders| !folders.is_empty())
            .map(|folders| folders.into_iter().map(|folder| {
                workspace_for_url(folder.uri)
            }).collect())
            .or_else(|| {
                tracing::warn!("No workspace(s) were provided during initialization. Using the current working directory as a default workspace...");
                let uri = types::Url::from_file_path(std::env::current_dir().ok()?).ok()?;
                Some(vec![workspace_for_url(uri)])
            })
            .ok_or_else(|| {
                anyhow::anyhow!("Failed to get the current working directory while creating a default workspace.")
            })?;

        Ok(Self {
            connection,
            worker_threads,
            session: Session::new(
                &client_capabilities,
                position_encoding,
                global_settings,
                workspaces,
            )?,
            client_capabilities,
        })
    }

    pub fn run(self) -> crate::Result<()> {
        event_loop_thread(move || {
            Self::event_loop(
                &self.connection,
                &self.client_capabilities,
                self.session,
                self.worker_threads,
            )?;
            self.connection.close()?;
            Ok(())
        })?
        .join()
    }

    fn event_loop(
        connection: &Connection,
        client_capabilities: &ClientCapabilities,
        mut session: Session,
        worker_threads: NonZeroUsize,
    ) -> crate::Result<()> {
        let mut scheduler =
            schedule::Scheduler::new(&mut session, worker_threads, connection.make_sender());

        Self::try_register_capabilities(client_capabilities, &mut scheduler);
        for msg in connection.incoming() {
            if connection.handle_shutdown(&msg)? {
                break;
            }
            let task = match msg {
                lsp::Message::Request(req) => api::request(req),
                lsp::Message::Notification(notification) => api::notification(notification),
                lsp::Message::Response(response) => scheduler.response(response),
            };
            scheduler.dispatch(task);
        }
        Ok(())
    }

    fn try_register_capabilities(
        client_capabilities: &ClientCapabilities,
        scheduler: &mut Scheduler,
    ) {
        let dynamic_registration = client_capabilities
            .workspace
            .as_ref()
            .and_then(|workspace| workspace.did_change_watched_files)
            .and_then(|watched_files| watched_files.dynamic_registration)
            .unwrap_or_default();
        if dynamic_registration {
            // Register all dynamic capabilities here

            // `workspace/didChangeWatchedFiles`
            // (this registers the configuration file watcher)
            let params = lsp_types::RegistrationParams {
                registrations: vec![lsp_types::Registration {
                    id: "ruff-server-watch".into(),
                    method: "workspace/didChangeWatchedFiles".into(),
                    register_options: Some(
                        serde_json::to_value(DidChangeWatchedFilesRegistrationOptions {
                            watchers: vec![
                                FileSystemWatcher {
                                    glob_pattern: types::GlobPattern::String(
                                        "**/.?ruff.toml".into(),
                                    ),
                                    kind: None,
                                },
                                FileSystemWatcher {
                                    glob_pattern: types::GlobPattern::String(
                                        "**/pyproject.toml".into(),
                                    ),
                                    kind: None,
                                },
                            ],
                        })
                        .unwrap(),
                    ),
                }],
            };

            let response_handler = |()| {
                tracing::info!("Configuration file watcher successfully registered");
                Task::nothing()
            };

            if let Err(err) = scheduler
                .request::<lsp_types::request::RegisterCapability>(params, response_handler)
            {
                tracing::error!("An error occurred when trying to register the configuration file watcher: {err}");
            }
        } else {
            tracing::warn!("LSP client does not support dynamic capability registration - automatic configuration reloading will not be available.");
        }
    }

    fn find_best_position_encoding(client_capabilities: &ClientCapabilities) -> PositionEncoding {
        client_capabilities
            .general
            .as_ref()
            .and_then(|general_capabilities| general_capabilities.position_encodings.as_ref())
            .and_then(|encodings| {
                encodings
                    .iter()
                    .filter_map(|encoding| PositionEncoding::try_from(encoding).ok())
                    .max() // this selects the highest priority position encoding
            })
            .unwrap_or_default()
    }

    fn server_capabilities(client_capabilities: &ClientCapabilities) -> types::ServerCapabilities {
        let position_encoding = client_capabilities
            .general
            .as_ref()
            .and_then(|general_capabilities| general_capabilities.position_encodings.as_ref())
            .and_then(|encodings| {
                encodings
                    .iter()
                    .filter_map(|encoding| PositionEncoding::try_from(encoding).ok())
                    .max() // this selects the highest priority position encoding
            })
            .unwrap_or_default();
        types::ServerCapabilities {
            position_encoding: Some(position_encoding.into()),
            workspace: Some(types::WorkspaceServerCapabilities {
                workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                    supported: Some(true),
                    change_notifications: Some(OneOf::Left(true)),
                }),
                file_operations: None,
            }),
            text_document_sync: Some(TextDocumentSyncCapability::Options(
                TextDocumentSyncOptions {
                    open_close: Some(true),
                    change: Some(TextDocumentSyncKind::FULL),
                    will_save: Some(false),
                    will_save_wait_until: Some(false),
                    ..Default::default()
                },
            )),
            completion_provider: Some(CompletionOptions {
                resolve_provider: Some(true),
                trigger_characters: Some(vec![".".to_string()]),
                work_done_progress_options: Default::default(),
                all_commit_characters: None,
                completion_item: None,
            }),
            diagnostic_provider: Some(types::DiagnosticServerCapabilities::Options(
                DiagnosticOptions {
                    identifier: Some(crate::SERVER_NAME.into()),
                    inter_file_dependencies: false,
                    workspace_diagnostics: false,
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: Some(true),
                    },
                },
            )),
            document_formatting_provider: Some(OneOf::Left(true)),
            definition_provider: Some(OneOf::Left(true)),
            references_provider: Some(OneOf::Left(true)),
            rename_provider: Some(OneOf::Right(types::RenameOptions {
                prepare_provider: Some(true),
                work_done_progress_options: types::WorkDoneProgressOptions {
                    work_done_progress: None,
                },
            })),
            document_highlight_provider: Some(OneOf::Left(true)),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            code_action_provider: Some(types::CodeActionProviderCapability::Options(
                CodeActionOptions {
                    code_action_kinds: Some(
                        SupportedCodeAction::all()
                            .map(SupportedCodeAction::to_kind)
                            .collect(),
                    ),
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: Some(true),
                    },
                    resolve_provider: Some(true),
                },
            )),
            execute_command_provider: Some(types::ExecuteCommandOptions {
                commands: SupportedCommand::all()
                    .map(|command| command.identifier().to_string())
                    .collect(),
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: Some(false),
                },
            }),
            document_symbol_provider: Some(OneOf::Left(true)),
            signature_help_provider: Some(SignatureHelpOptions {
                trigger_characters: Some(vec!["(".to_owned(), ",".to_owned()]),
                retrigger_characters: None,
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: None,
                },
            }),
            semantic_tokens_provider: Some(
                types::SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                    types::SemanticTokensRegistrationOptions {
                        text_document_registration_options:
                            types::TextDocumentRegistrationOptions {
                                document_selector: Some(vec![types::DocumentFilter {
                                    language: Some("python".into()),
                                    scheme: Some("file".into()),
                                    pattern: None,
                                }]),
                            },
                        semantic_tokens_options: types::SemanticTokensOptions {
                            work_done_progress_options: WorkDoneProgressOptions::default(),
                            range: Some(true),
                            full: Some(types::SemanticTokensFullOptions::Bool(true)),
                            legend: types::SemanticTokensLegend {
                                token_modifiers: vec![],
                                token_types: api::SupportedSemanticTokens::all(),
                            },
                        },
                        static_registration_options: types::StaticRegistrationOptions::default(),
                    },
                ),
            ),
            ..Default::default()
        }
    }
}

/// The code actions we support.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum SupportedCodeAction {
    /// Maps to the `quickfix` code action kind. Quick fix code actions are shown under
    /// their respective diagnostics. Quick fixes are only created where the fix applicability is
    /// "safe" or "unsafe".
    QuickFix,
    /// Maps to `source.organizeImports` and `source.organizeImports.sith` code action kinds.
    /// This is a source action that applies import sorting fixes to the currently open document.
    SourceOrganizeImports,
    /// Maps to the `source.fixAll` and `source.fixAll.sith` code action kinds.
    /// This is a source action that applies all safe fixes to the currently open document.
    SourceFixAll,
}

impl SupportedCodeAction {
    /// Returns the LSP code action kind that map to this code action.
    fn to_kind(self) -> CodeActionKind {
        match self {
            Self::QuickFix => CodeActionKind::QUICKFIX,
            Self::SourceOrganizeImports => crate::SOURCE_ORGANIZE_IMPORTS_SITH,
            Self::SourceFixAll => crate::SOURCE_FIX_ALL_SITH,
        }
    }

    fn from_kind(kind: CodeActionKind) -> impl Iterator<Item = Self> {
        Self::all().filter(move |supported_kind| {
            supported_kind.to_kind().as_str().starts_with(kind.as_str())
        })
    }

    /// Returns all code actions kinds that the server currently supports.
    fn all() -> impl Iterator<Item = Self> {
        [
            Self::SourceOrganizeImports,
            Self::SourceFixAll,
            Self::QuickFix,
        ]
        .into_iter()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum SupportedCommand {
    Format,
    FixAll,
    OrganizeImports,
}

impl SupportedCommand {
    const fn label(self) -> &'static str {
        match self {
            Self::FixAll => "Fix all auto-fixable problems",
            Self::Format => "Format document",
            Self::OrganizeImports => "Format imports",
        }
    }

    /// Returns the identifier of the command.
    const fn identifier(self) -> &'static str {
        match self {
            SupportedCommand::Format => "sith.applyFormat",
            SupportedCommand::FixAll => "sith.applyAutofix",
            SupportedCommand::OrganizeImports => "sith.applyOrganizeImports",
        }
    }

    /// Returns all the commands that the server currently supports.
    fn all() -> impl Iterator<Item = Self> {
        [
            SupportedCommand::Format,
            SupportedCommand::FixAll,
            SupportedCommand::OrganizeImports,
        ]
        .into_iter()
    }
}

impl FromStr for SupportedCommand {
    type Err = anyhow::Error;

    fn from_str(name: &str) -> anyhow::Result<Self, Self::Err> {
        Ok(match name {
            "sith.applyAutofix" => Self::FixAll,
            "sith.applyFormat" => Self::Format,
            "sith.applyOrganizeImports" => Self::OrganizeImports,
            _ => return Err(anyhow::anyhow!("Invalid command `{name}`")),
        })
    }
}
