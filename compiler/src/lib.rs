#[macro_use]
extern crate lalrpop_util;

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

#[cfg(test)]
mod tests {
    mod checker;
    mod parser;
}
