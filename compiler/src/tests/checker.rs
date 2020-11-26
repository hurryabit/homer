use crate::*;
use checker::SymbolInfo;
use syntax::{Decl, Expr, FuncDecl, Module, Type};

mod decls;
mod expressions;
mod func_resolution;
mod shadowing;
mod symbols;
mod type_resolution;
mod types;

fn check_output_and_symbols(input: &str) -> (Module, Vec<SymbolInfo>) {
    let humanizer = location::Humanizer::new(input);
    let (result, diagnostics) = Module::parse(input, &humanizer);
    assert!(diagnostics.is_empty());
    let module = result.unwrap();
    match module.check(&humanizer) {
        Err(diagnostic) => panic!(
            "Expected module to type check but got error\n{:?}: {}",
            diagnostic.span, diagnostic.message
        ),
        Ok(module_and_symbols) => module_and_symbols,
    }
}

fn check_output(input: &str) -> Module {
    check_output_and_symbols(input).0
}

fn check_output_type(name: &str, input: &str) -> Type {
    check_output(input)
        .decls
        .into_iter()
        .find_map(|decl| match decl {
            Decl::Type(decl) if decl.name.locatee.as_str() == name => Some(decl.body.locatee),
            _ => None,
        })
        .unwrap()
}

fn check_output_func_decl(name: &str, input: &str) -> FuncDecl {
    check_output(input)
        .decls
        .into_iter()
        .find_map(|decl| match decl {
            Decl::Func(decl) if decl.name.locatee.as_str() == name => Some(decl),
            _ => None,
        })
        .unwrap()
}

fn check_output_func_body(name: &str, input: &str) -> Expr {
    check_output_func_decl(name, input).body.locatee
}

fn check_success(input: &str) {
    check_output(input);
}

fn check_symbols(input: &str) -> Vec<SymbolInfo> {
    check_output_and_symbols(input).1
}

fn check_error(input: &str) -> String {
    let humanizer = location::Humanizer::new(input);
    let (result, diagnostics) = Module::parse(input, &humanizer);
    assert!(diagnostics.is_empty());
    let module = result.unwrap();
    let diagnostic = module.check(&humanizer).unwrap_err();
    diagnostic.layout(input)
}
