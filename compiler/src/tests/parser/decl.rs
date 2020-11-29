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
        body: VAR @ 1:10-1:13
            var: Int @ 1:10-1:13
    "###);
}

#[test]
fn type_poly() {
    insta::assert_debug_snapshot!(parse("type T<A> = A"), @r###"
    TYPEDECL
        name: T @ 1:6-1:7
        type_param: A @ 1:8-1:9
        body: VAR @ 1:13-1:14
            var: A @ 1:13-1:14
    "###);
}

#[test]
fn func_mono() {
    insta::assert_debug_snapshot!(parse("fn id(x: Int) -> Int { x }"), @r###"
    FUNCDECL
        name: id @ 1:4-1:6
        param: x @ 1:7-1:8
        type: VAR @ 1:10-1:13
            var: Int @ 1:10-1:13
        result: VAR @ 1:18-1:21
            var: Int @ 1:18-1:21
        body: VAR @ 1:24-1:25
            var: x @ 1:24-1:25
    "###);
}

#[test]
fn func_poly() {
    insta::assert_debug_snapshot!(parse("fn id<A>(x: A) -> A { x }"), @r###"
    FUNCDECL
        name: id @ 1:4-1:6
        type_param: A @ 1:7-1:8
        param: x @ 1:10-1:11
        type: VAR @ 1:13-1:14
            var: A @ 1:13-1:14
        result: VAR @ 1:19-1:20
            var: A @ 1:19-1:20
        body: VAR @ 1:23-1:24
            var: x @ 1:23-1:24
    "###);
}
