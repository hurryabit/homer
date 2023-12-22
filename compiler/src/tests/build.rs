use crate::build::*;
use std::sync::Arc;

fn build_output(input: &str) -> String {
    use std::fmt::Write;
    let db = &mut CompilerDB::new();
    let uri = Uri::new("foo.homer");
    db.set_input(uri, Arc::new(input.to_owned()));

    let mut output = String::new();
    match db.anf_module(uri) {
        None => writeln!(output, "NONE").unwrap(),
        Some(module) => writeln!(output, "{:?}", module).unwrap(),
    }
    db.with_diagnostics(uri, |diagnostics| {
        for diagnostic in diagnostics {
            write!(output, "{}\n{}\n", "-".repeat(50), diagnostic.layout(input)).unwrap();
        }
    });
    output
}

#[test]
fn all_good() {
    insta::assert_snapshot!(build_output(r#"
    fn f(x: Int) -> Int { x }
    "#), @r###"
    MODULE
        decl: FUNCDECL
            name: f
            param: x
            body: EXPR
                binder: $result
                bindee: x/1
    "###);
}

#[test]
fn fatal_parse_error() {
    insta::assert_snapshot!(build_output(r#"
    fn f(x: Int) -> Int { x
    "#), @r###"
    NONE
    --------------------------------------------------
      2 |     fn f(x: Int) -> Int { x
                                     ~
    Unrecognized EOF found at 2:28
    Expected one of "!=", "(", "*", "+", "-", ".", "/", "<", "<=", "==", ">", ">=", "@" or "}"
    "###);
}

#[test]
fn recoverable_parse_error_types_good() {
    insta::assert_snapshot!(build_output(r#"
    fn f(x: Int) -> Int { + }
    "#), @r###"
    MODULE
        decl: FUNCDECL
            name: f
            param: x
            body: EXPR
                binder: $v1
                bindee: ERROR
                binder: $v2
                bindee: ERROR
                binder: $result
                bindee: BINOP
                    lhs: $v1/2
                    op: ADD
                    rhs: $v2/1
    --------------------------------------------------
      2 |     fn f(x: Int) -> Int { + }
                                    ~
    Unrecognized token `+` found at 2:27:2:28
    Expected one of "(", "false", "fn", "if", "let", "match", "true", "{", ID_LOWER, ID_UPPER or NUMBER
    --------------------------------------------------
      2 |     fn f(x: Int) -> Int { + }
                                      ~
    Unrecognized token `}` found at 2:29:2:30
    Expected one of "(", "false", "true", "{", ID_LOWER, ID_UPPER or NUMBER
    "###);
}

#[test]
fn recoverable_parse_error_types_bad() {
    insta::assert_snapshot!(build_output(r#"
    fn f(x: Int) -> Int { true + }
    "#), @r###"
    NONE
    --------------------------------------------------
      2 |     fn f(x: Int) -> Int { true + }
                                           ~
    Unrecognized token `}` found at 2:34:2:35
    Expected one of "(", "false", "true", "{", ID_LOWER, ID_UPPER or NUMBER
    --------------------------------------------------
      2 |     fn f(x: Int) -> Int { true + }
                                    ~~~~
    Expected an expression of type `Int` but found an expression of type `Bool`.
    "###);
}
