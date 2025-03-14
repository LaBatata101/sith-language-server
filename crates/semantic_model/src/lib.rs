pub mod builtins;
pub mod db;
pub mod declaration;
pub mod mro;
mod scope;
mod symbol;
pub mod symbol_table;
pub mod type_inference;
pub mod util;
mod vendored;

pub use scope::*;
pub use symbol::{Symbol, SymbolId};

// The file path here is hardcoded in this crate's `build.rs` script.
// Luckily this crate will fail to build if this file isn't available at build time.
static TYPESHED_ZIP_BYTES: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/zipped_typeshed.zip"));
