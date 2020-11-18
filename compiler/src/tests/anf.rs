use crate::build::*;
use std::sync::Arc;

fn anf_output(input: &str) -> String {
    let db = &mut CompilerDB::new();
    let uri = Uri::new("foo.homer");
    db.set_input(uri, Arc::new(input.to_owned()));
    assert!(db.with_diagnostics(uri, |diagnostics| diagnostics.next().is_none()));
    format!("{:#?}", db.anf_module(uri).unwrap())
}

#[test]
fn simple() {
    insta::assert_snapshot!(anf_output(r#"
    fn f() -> Int { 1 + 2 }
    "#), @r###"
    MODULE
        decl: FUNCDECL
            name: f
            body: EXPR
                binder: $v1
                bindee: 1
                binder: $v2
                bindee: 2
                body: BINOP
                    lhs: $v1
                    op: ADD
                    rhs: $v2
    "###);
}

#[test]
fn lambda_shadowing() {
    insta::assert_snapshot!(anf_output(r#"
    fn f() -> Int {
        let x = 1;
        let f = fn (x: Int) { x };
        f(x)
    }
    "#), @r###"
    MODULE
        decl: FUNCDECL
            name: f
            body: EXPR
                binder: x
                bindee: 1
                binder: f
                bindee: LAM
                    param: $v1
                    body: EXPR
                        body: $v1
                body: APP
                    fun: f
                    arg: x
    "###);
}

#[test]
fn nested_shadowing() {
    insta::assert_snapshot!(anf_output(r#"
    fn f() -> Int {
        let x = 1;
        let x = {
            let x = x;
            x
        };
        x
    }
    "#), @r###"
    MODULE
        decl: FUNCDECL
            name: f
            body: EXPR
                binder: x
                bindee: 1
                binder: $v1
                bindee: x
                binder: $v2
                bindee: $v1
                body: $v2
    "###);
}
