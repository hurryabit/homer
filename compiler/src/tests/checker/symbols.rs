use super::*;

#[test]
fn smoke_test() {
    insta::assert_debug_snapshot!(check_symbols(r#"
    fn f(x: Int) -> Int {
        let y = x;
        y
    }
    "#), @r###"
    [
        EXPR_BINDER
            var: x @ 2:10-2:11
            type: INT,
        EXPR_BINDER
            var: y @ 3:13-3:14
            type: INT,
        EXPR_VAR
            var: x @ 3:17-3:18
            type: INT
            def: 2:10-2:11,
        EXPR_VAR
            var: y @ 4:9-4:10
            type: INT
            def: 3:13-3:14,
    ]
    "###);
}

#[test]
fn func_ref() {
    insta::assert_debug_snapshot!(check_symbols(r#"
    fn f() -> Int { 0 }
    fn g() -> Int { f() }
    "#), @r###"
    [
        FUNC_REF
            var: f @ 3:21-3:22
            def: FUNC_SIG
                name: f @ 2:8-2:9
                result: INT,
    ]
    "###);
}
