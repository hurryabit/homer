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
