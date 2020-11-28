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
        ExprBinder {
            var: Located {
                locatee: ExprVar("x"),
                span: 2:10-2:11,
            },
            typ: RcType(
                Int,
            ),
        },
        ExprBinder {
            var: Located {
                locatee: ExprVar("y"),
                span: 3:13-3:14,
            },
            typ: RcType(
                Int,
            ),
        },
        ExprVar {
            var: Located {
                locatee: ExprVar("x"),
                span: 3:17-3:18,
            },
            typ: RcType(
                Int,
            ),
            def: 2:10-2:11,
        },
        ExprVar {
            var: Located {
                locatee: ExprVar("y"),
                span: 4:9-4:10,
            },
            typ: RcType(
                Int,
            ),
            def: 3:13-3:14,
        },
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
        FuncRef {
            var: Located {
                locatee: ExprVar("f"),
                span: 3:21-3:22,
            },
            def: FuncSig {
                name: Located {
                    locatee: ExprVar("f"),
                    span: 2:8-2:9,
                },
                type_params: [],
                params: [],
                result: RcType(
                    Int,
                ),
            },
        },
    ]
    "###);
}
