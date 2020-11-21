use crate::*;
use diagnostic::Diagnostic;
use syntax::Module;

mod decl;
mod expr;
mod type_;

fn parse_output_impl<T, F>(f: F, input: &str) -> T
where
    F: Fn(&str) -> (Option<T>, Vec<Diagnostic>),
{
    let (result, diagnostics) = f(input);
    assert!(diagnostics.is_empty());
    result.unwrap()
}

fn parse_error_impl<T: std::fmt::Debug, F>(f: F, input: &str) -> String
where
    F: Fn(&str) -> (Option<T>, Vec<Diagnostic>),
{
    use std::fmt::Write;
    let (result, diagnostics) = f(input);
    assert!(!diagnostics.is_empty() || result.is_none());
    let mut output = String::new();
    if let Some(ast) = result {
        writeln!(output, "{:?}", ast).unwrap();
    } else {
        writeln!(output, "---").unwrap();
    }
    for diagnostic in diagnostics {
        write!(output, "{}\n{}\n", "-".repeat(50), diagnostic.layout(&input)).unwrap();
    }
    output
}

fn parse(input: &str) -> Module {
    parse_output_impl(Module::parse_test, input)
}

#[test]
fn module() {
    insta::assert_debug_snapshot!(parse(r#"
    type Mono = Int
    fn mono(x: Int) -> Mono { x }
    type Poly<A> = A
    fn poly<A>(x: A) -> Poly<A> { x }
    "#), @r###"
    MODULE
        decl: TYPEDECL
            name: Mono @ 10...14
            body: Int @ 17...20
        decl: FUNCDECL
            name: mono @ 28...32
            param: x @ 33...34
            type: Int @ 36...39
            result: Mono @ 44...48
            body: x @ 51...52
        decl: TYPEDECL
            name: Poly @ 64...68
            type_param: A @ 69...70
            body: A @ 74...75
        decl: FUNCDECL
            name: poly @ 83...87
            type_param: A @ 88...89
            param: x @ 91...92
            type: A @ 94...95
            result: APP @ 100...107
                syn: Poly @ 100...104
                type_arg: A @ 105...106
            body: x @ 110...111
    "###);
}
