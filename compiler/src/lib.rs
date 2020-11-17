#[macro_use]
extern crate lalrpop_util;
#[macro_use]
extern crate lazy_static;

pub mod build;
pub mod checker;
pub mod diagnostic;
pub mod location;
pub mod parser;
pub mod syntax;

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
    mod build;
    mod checker;
    mod parser;
}
