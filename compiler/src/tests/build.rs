use crate::build::*;

fn build_output(input: &str) -> String {
    use std::fmt::Write;
    let db = &mut CompilerDB::new();
    let uri = Uri::new("foo.homer");
    db.set_input(uri, input.to_owned());

    let mut output = String::new();
    if let Some(module) = db.best_module(uri).as_ref() {
        write!(output, "{:?}\n", module).unwrap();
    } else {
        write!(output, "NONE\n").unwrap();
    }
    db.with_diagnostics(uri, |diagnostics| {
        for diagnostic in diagnostics {
            write!(
                output,
                "{}\n{}\n",
                "-".repeat(50),
                diagnostic.layout(&input)
            )
            .unwrap();
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
            name: f @ 8...9
            param: x @ 10...11
            type: INT @ 13...16
            result: INT @ 21...24
            body: x @ 27...28
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
    Expected one of "!=", "(", ")", "*", "+", ",", "-", ".", "/", ":", ";", "<", "<=", "=", "==", ">", ">=", "@", "{" or "}"
    "###);
}

#[test]
fn recoverable_parse_error_types_good() {
    insta::assert_snapshot!(build_output(r#"
    fn f(x: Int) -> Int { + }
    "#), @r###"
    MODULE
        decl: FUNCDECL
            name: f @ 8...9
            param: x @ 10...11
            type: INT @ 13...16
            result: INT @ 21...24
            body: BINOP @ 26...29
                lhs: ERROR @ 26...27
                op: ADD
                rhs: ERROR @ 28...29
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
    MODULE
        decl: FUNCDECL
            name: f @ 8...9
            param: x @ 10...11
            type: Int @ 13...16
            result: Int @ 21...24
            body: BINOP @ 27...34
                lhs: true @ 27...31
                op: ADD
                rhs: ERROR @ 33...34
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
