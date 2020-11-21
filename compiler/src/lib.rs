#[macro_use]
extern crate im;
#[macro_use]
extern crate lalrpop_util;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate static_assertions;

pub mod anf;
pub mod build;
pub mod cek;
pub mod checker;
pub mod diagnostic;
pub mod location;
pub mod parser;
pub mod syntax;
mod util;

lalrpop_mod!(
    #[allow(clippy::all)]
    #[allow(dead_code)]
    grammar
);

lazy_static! {
    static ref INTERNER: lasso::ThreadedRodeo = lasso::ThreadedRodeo::new();
}

#[cfg(test)]
mod tests {
    mod anf;
    mod build;
    mod cek;
    mod checker;
    mod parser;
}
