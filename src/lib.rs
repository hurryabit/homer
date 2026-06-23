use std::sync::LazyLock;

#[macro_use]
extern crate im;
#[macro_use]
extern crate lalrpop_util;
#[macro_use]
extern crate static_assertions;

pub mod anf;
#[macro_use]
pub mod ast;
pub mod build;
pub mod cek;
pub mod checker;
pub mod diagnostic;
pub mod location;
mod lsp;
pub mod parser;
pub mod syntax;
mod util;

lalrpop_mod!(
    #[allow(clippy::all)]
    #[allow(dead_code)]
    grammar
);

pub use lsp::LanguageServer;

static INTERNER: LazyLock<lasso::ThreadedRodeo> = LazyLock::new(lasso::ThreadedRodeo::new);

#[cfg(test)]
mod tests {
    mod anf;
    mod build;
    mod cek;
    mod checker;
    mod parser;
}
