use crate::*;
use syntax::*;

fn parse(input: &str) -> Decl {
    let (result, diagnostics) = Decl::parse_test(input);
    assert!(diagnostics.is_empty());
    result.unwrap()
}

#[test]
fn type_mono() {
    insta::assert_debug_snapshot!(parse("type T = Int"), @r###"
    TYPEDECL
        name: T @ 1:6-1:7
        body: Int @ 1:10-1:13
    "###);
}

#[test]
fn type_poly() {
    insta::assert_debug_snapshot!(parse("type T<A> = A"), @r###"
    TYPEDECL
        name: T @ 1:6-1:7
        type_param: A @ 1:8-1:9
        body: A @ 1:13-1:14
    "###);
}

#[test]
fn func_mono() {
    insta::assert_debug_snapshot!(parse("fn id(x: Int) -> Int { x }"), @r###"
    FUNCDECL
        name: id @ 1:4-1:6
        param: x @ 1:7-1:8
        type: Int @ 1:10-1:13
        result: Int @ 1:18-1:21
        body: x @ 1:24-1:25
    "###);
}

#[test]
fn func_poly() {
    insta::assert_debug_snapshot!(parse("fn id<A>(x: A) -> A { x }"), @r###"
    FUNCDECL
        name: id @ 1:4-1:6
        type_param: A @ 1:7-1:8
        param: x @ 1:10-1:11
        type: A @ 1:13-1:14
        result: A @ 1:19-1:20
        body: x @ 1:23-1:24
    "###);
}
