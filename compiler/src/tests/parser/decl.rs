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
        name: T @ 5...6
        type: Int @ 9...12
    "###);
}

#[test]
fn type_poly() {
    insta::assert_debug_snapshot!(parse("type T<A> = A"), @r###"
    TYPEDECL
        name: T @ 5...6
        type_param: A @ 7...8
        type: A @ 12...13
    "###);
}

#[test]
fn func_mono() {
    insta::assert_debug_snapshot!(parse("fn id(x: Int) -> Int { x }"), @r###"
    FUNCDECL
        name: id @ 3...5
        param: x @ 6...7
        type: Int @ 9...12
        result: Int @ 17...20
        body: x @ 23...24
    "###);
}

#[test]
fn func_poly() {
    insta::assert_debug_snapshot!(parse("fn id<A>(x: A) -> A { x }"), @r###"
    FUNCDECL
        name: id @ 3...5
        type_param: A @ 6...7
        param: x @ 9...10
        type: A @ 12...13
        result: A @ 18...19
        body: x @ 22...23
    "###);
}
