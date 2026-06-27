mod anf;
mod ast;
pub mod build;
pub mod cek;
mod checker;
mod diagnostic;
mod location;
mod lsp;
mod parser;
pub mod syntax;
mod util;

use std::sync::LazyLock;

use lalrpop_util::lalrpop_mod;

pub use crate::lsp::LanguageServer;

lalrpop_mod!(
    #[allow(clippy::all)]
    #[allow(dead_code)]
    grammar
);

static INTERNER: LazyLock<lasso::ThreadedRodeo> = LazyLock::new(lasso::ThreadedRodeo::new);
