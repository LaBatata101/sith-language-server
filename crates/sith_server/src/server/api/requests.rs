mod code_action;
mod code_action_resolve;
mod completion;
mod diagnostic;
mod document_highlight;
mod document_symbol;
mod execute_command;
mod format;
mod goto_definition;
mod hover;
mod prepare_rename;
mod references;
mod rename;
mod semantic_tokens;
mod signature_help;

use super::{
    define_document_url,
    traits::{BackgroundDocumentRequestHandler, RequestHandler, SyncRequestHandler},
};
pub(super) use code_action::CodeActions;
pub(super) use code_action_resolve::CodeActionResolve;
pub(super) use completion::resolve::ResolveCompletionItem;
pub(super) use completion::Completion;
pub(super) use diagnostic::DocumentDiagnostic;
pub(super) use document_highlight::DocumentHighlight;
pub(super) use document_symbol::DocumentSymbol;
pub(super) use execute_command::ExecuteCommand;
pub(super) use format::Format;
pub(super) use goto_definition::GotoDefinition;
pub(super) use hover::Hover;
pub(super) use prepare_rename::PrepareRename;
pub(super) use references::References;
pub(super) use rename::Rename;
pub(super) use semantic_tokens::full::SemanticTokensFull;
pub(super) use semantic_tokens::range::SemanticTokensRange;
pub(crate) use semantic_tokens::SupportedSemanticTokens;
pub(super) use signature_help::SignatureHelp;

type FormatResponse = Option<Vec<lsp_types::TextEdit>>;
