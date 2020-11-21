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
                tail: BINOP
                    lhs: $v1/2
                    op: ADD
                    rhs: $v2/1
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
                bindee: MAKE_CLOSURE
                    param: $v1
                    body: EXPR
                        tail: $v1/1
                tail: APP
                    fun: f/1
                    arg: x/2
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
                bindee: x/1
                binder: $v2
                bindee: $v1/1
                tail: $v2/1
    "###);
}

#[test]
fn capture() {
    insta::assert_snapshot!(anf_output(r#"
    fn f() -> Int {
        let x = 1;
        let f = fn () { x };
        f()
    }
    "#), @r###"
    MODULE
        decl: FUNCDECL
            name: f
            body: EXPR
                binder: x
                bindee: 1
                binder: f
                bindee: MAKE_CLOSURE
                    captured: x/1
                    body: EXPR
                        tail: x/1
                tail: APP
                    fun: f/1
    "###);
}

#[test]
fn pattern_shadowing() {
    insta::assert_snapshot!(anf_output(r#"
    fn f(x: [C(Int)]) -> Int {
        match x {
            C(x) => x + x,
        }
    }
    "#), @r###"
    MODULE
        decl: FUNCDECL
            name: f
            param: x
            body: EXPR
                tail: MATCH
                    scrut: x/1
                    branch: BRANCH
                        pattern: PATTERN
                            constr: C/0
                            binder: $v1
                        rhs: EXPR
                            tail: BINOP
                                lhs: $v1/1
                                op: ADD
                                rhs: $v1/1
    "###);
}

#[test]
fn branch_sorting() {
    insta::assert_snapshot!(anf_output(r#"
    fn f(x: [A | B]) -> Int {
        match x {
            B => 0,
            A => 1,
        }
    }
    "#), @r###"
    MODULE
        decl: FUNCDECL
            name: f
            param: x
            body: EXPR
                tail: MATCH
                    scrut: x/1
                    branch: BRANCH
                        pattern: PATTERN
                            constr: A/0
                        rhs: EXPR
                            tail: 1
                    branch: BRANCH
                        pattern: PATTERN
                            constr: B/1
                        rhs: EXPR
                            tail: 0
    "###);
}
