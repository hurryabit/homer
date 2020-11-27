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
            name: Mono @ 2:10-2:14
            body: Int @ 2:17-2:20
        decl: FUNCDECL
            name: mono @ 3:8-3:12
            param: x @ 3:13-3:14
            type: Int @ 3:16-3:19
            result: Mono @ 3:24-3:28
            body: x @ 3:31-3:32
        decl: TYPEDECL
            name: Poly @ 4:10-4:14
            type_param: A @ 4:15-4:16
            body: A @ 4:20-4:21
        decl: FUNCDECL
            name: poly @ 5:8-5:12
            type_param: A @ 5:13-5:14
            param: x @ 5:16-5:17
            type: A @ 5:19-5:20
            result: APP @ 5:25-5:32
                syn: Poly @ 5:25-5:29
                type_arg: A @ 5:30-5:31
            body: x @ 5:35-5:36
    "###);
}
