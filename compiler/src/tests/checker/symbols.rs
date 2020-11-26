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
                span: 10-11,
            },
            typ: RcType(
                Int,
            ),
        },
        ExprBinder {
            var: Located {
                locatee: ExprVar("y"),
                span: 39-40,
            },
            typ: RcType(
                Int,
            ),
        },
        ExprVar {
            var: Located {
                locatee: ExprVar("x"),
                span: 43-44,
            },
            typ: RcType(
                Int,
            ),
            def: 10-11,
        },
        ExprVar {
            var: Located {
                locatee: ExprVar("y"),
                span: 54-55,
            },
            typ: RcType(
                Int,
            ),
            def: 39-40,
        },
    ]
    "###);
}
