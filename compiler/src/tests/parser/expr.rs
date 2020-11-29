use crate::*;
use syntax::Expr;

fn parse_output(input: &str) -> Expr {
    super::parse_output_impl(Expr::parse_test, input)
}

fn parse_block_output(input: &str) -> Expr {
    super::parse_output_impl(Expr::parse_block_test, input)
}

fn parse_error(input: &'static str) -> String {
    super::parse_error_impl(Expr::parse_test, input)
}

#[test]
fn var() {
    insta::assert_debug_snapshot!(parse_output("x"), @r###"
    VAR
        var: x @ 1:1-1:2
    "###);
}

#[test]
fn num() {
    insta::assert_debug_snapshot!(parse_output("0"), @"0");
}

#[test]
fn bool_true() {
    insta::assert_debug_snapshot!(parse_output("true"), @"true");
}

#[test]
fn bool_false() {
    insta::assert_debug_snapshot!(parse_output("false"), @"false");
}

#[test]
fn app0() {
    insta::assert_debug_snapshot!(parse_output("f()"), @r###"
    APPCLO
        clo: f @ 1:1-1:2
    "###);
}

#[test]
fn app1() {
    insta::assert_debug_snapshot!(parse_output("f(1)"), @r###"
    APPCLO
        clo: f @ 1:1-1:2
        arg: 1 @ 1:3-1:4
    "###);
}

#[test]
fn app1_trailing() {
    insta::assert_debug_snapshot!(parse_output("f(1,)"), @r###"
    APPCLO
        clo: f @ 1:1-1:2
        arg: 1 @ 1:3-1:4
    "###);
}

#[test]
fn app2() {
    insta::assert_debug_snapshot!(parse_output("f(1, 2)"), @r###"
    APPCLO
        clo: f @ 1:1-1:2
        arg: 1 @ 1:3-1:4
        arg: 2 @ 1:6-1:7
    "###);
}
#[test]
fn app_ty() {
    insta::assert_debug_snapshot!(parse_output("f@<Int>(1)"), @r###"
    APPFUN
        fun: f @ 1:1-1:2
        type_arg: Int @ 1:4-1:7
        arg: 1 @ 1:9-1:10
    "###);
}

#[test]
fn app_ty_err() {
    insta::assert_snapshot!(parse_error("f<A>(1)"), @r###"
    BINOP
        lhs: ERROR @ 1:1-1:4
        op: GREATER
        rhs: 1 @ 1:5-1:8
    --------------------------------------------------
      1 | f<A>(1)
             ~
    Unrecognized token `>` found at 1:4:1:5
    Expected one of ")", "+", ",", "-", ";", "{" or "}"
    "###);
}

#[test]
fn record0() {
    insta::assert_debug_snapshot!(parse_output("{}"), @"RECORD");
}
#[test]
fn record1() {
    insta::assert_debug_snapshot!(parse_output("{x = 1}"), @r###"
    RECORD
        field: x @ 1:2-1:3
        value: 1 @ 1:6-1:7
    "###);
}

#[test]
fn record1_trailing() {
    insta::assert_debug_snapshot!(parse_output("{x = 1,}"), @r###"
    RECORD
        field: x @ 1:2-1:3
        value: 1 @ 1:6-1:7
    "###);
}

#[test]
fn record2() {
    insta::assert_debug_snapshot!(parse_output("{x = 1, y = 2}"), @r###"
    RECORD
        field: x @ 1:2-1:3
        value: 1 @ 1:6-1:7
        field: y @ 1:9-1:10
        value: 2 @ 1:13-1:14
    "###);
}

#[test]
fn proj1() {
    insta::assert_debug_snapshot!(parse_output("r.x"), @r###"
    PROJ
        record: VAR @ 1:1-1:2
            var: r @ 1:1-1:2
        field: x @ 1:3-1:4
    "###);
}

#[test]
fn proj2() {
    insta::assert_debug_snapshot!(parse_output("r.x.y"), @r###"
    PROJ
        record: PROJ @ 1:1-1:4
            record: VAR @ 1:1-1:2
                var: r @ 1:1-1:2
            field: x @ 1:3-1:4
        field: y @ 1:5-1:6
    "###);
}

#[test]
fn variant0() {
    insta::assert_debug_snapshot!(parse_output("A"), @r###"
    VARIANT
        constr: A
    "###);
}

#[test]
fn variant1() {
    insta::assert_debug_snapshot!(parse_output("A(0)"), @r###"
    VARIANT
        constr: A
        payload: 0 @ 1:3-1:4
    "###);
}

#[test]
fn variant_int() {
    insta::assert_debug_snapshot!(parse_output("Int"), @r###"
    VARIANT
        constr: Int
    "###);
}

#[test]
fn variant_bool() {
    insta::assert_debug_snapshot!(parse_output("Bool"), @r###"
    VARIANT
        constr: Bool
    "###);
}

#[test]
fn prod2() {
    insta::assert_debug_snapshot!(parse_output("a*b"), @r###"
    BINOP
        lhs: VAR @ 1:1-1:2
            var: a @ 1:1-1:2
        op: MUL
        rhs: VAR @ 1:3-1:4
            var: b @ 1:3-1:4
    "###);
}

#[test]
fn prod3() {
    insta::assert_debug_snapshot!(parse_output("a/b*c"), @r###"
    BINOP
        lhs: BINOP @ 1:1-1:4
            lhs: VAR @ 1:1-1:2
                var: a @ 1:1-1:2
            op: DIV
            rhs: VAR @ 1:3-1:4
                var: b @ 1:3-1:4
        op: MUL
        rhs: VAR @ 1:5-1:6
            var: c @ 1:5-1:6
    "###);
}

#[test]
fn sum2() {
    insta::assert_debug_snapshot!(parse_output("a+b"), @r###"
    BINOP
        lhs: VAR @ 1:1-1:2
            var: a @ 1:1-1:2
        op: ADD
        rhs: VAR @ 1:3-1:4
            var: b @ 1:3-1:4
    "###);
}

#[test]
fn sum3() {
    insta::assert_debug_snapshot!(parse_output("a-b+c"), @r###"
    BINOP
        lhs: BINOP @ 1:1-1:4
            lhs: VAR @ 1:1-1:2
                var: a @ 1:1-1:2
            op: SUB
            rhs: VAR @ 1:3-1:4
                var: b @ 1:3-1:4
        op: ADD
        rhs: VAR @ 1:5-1:6
            var: c @ 1:5-1:6
    "###);
}

#[test]
fn cmp_eq() {
    insta::assert_debug_snapshot!(parse_output("a == b"), @r###"
    BINOP
        lhs: VAR @ 1:1-1:2
            var: a @ 1:1-1:2
        op: EQUALS
        rhs: VAR @ 1:6-1:7
            var: b @ 1:6-1:7
    "###);
}

#[test]
fn cmp_neq() {
    insta::assert_debug_snapshot!(parse_output("a != b"), @r###"
    BINOP
        lhs: VAR @ 1:1-1:2
            var: a @ 1:1-1:2
        op: NOTEQ
        rhs: VAR @ 1:6-1:7
            var: b @ 1:6-1:7
    "###);
}

#[test]
fn cmp_lt() {
    insta::assert_debug_snapshot!(parse_output("a < b"), @r###"
    BINOP
        lhs: VAR @ 1:1-1:2
            var: a @ 1:1-1:2
        op: LESS
        rhs: VAR @ 1:5-1:6
            var: b @ 1:5-1:6
    "###);
}

#[test]
fn cmp_leq() {
    insta::assert_debug_snapshot!(parse_output("a <= b"), @r###"
    BINOP
        lhs: VAR @ 1:1-1:2
            var: a @ 1:1-1:2
        op: LESSEQ
        rhs: VAR @ 1:6-1:7
            var: b @ 1:6-1:7
    "###);
}

#[test]
fn cmp_gt() {
    insta::assert_debug_snapshot!(parse_output("a > b"), @r###"
    BINOP
        lhs: VAR @ 1:1-1:2
            var: a @ 1:1-1:2
        op: GREATER
        rhs: VAR @ 1:5-1:6
            var: b @ 1:5-1:6
    "###);
}

#[test]
fn cmp_geq() {
    insta::assert_debug_snapshot!(parse_output("a >= b"), @r###"
    BINOP
        lhs: VAR @ 1:1-1:2
            var: a @ 1:1-1:2
        op: GREATEREQ
        rhs: VAR @ 1:6-1:7
            var: b @ 1:6-1:7
    "###);
}

#[test]
fn cmp_mixed() {
    insta::assert_debug_snapshot!(parse_output("a + b == c * d"), @r###"
    BINOP
        lhs: BINOP @ 1:1-1:6
            lhs: VAR @ 1:1-1:2
                var: a @ 1:1-1:2
            op: ADD
            rhs: VAR @ 1:5-1:6
                var: b @ 1:5-1:6
        op: EQUALS
        rhs: BINOP @ 1:10-1:15
            lhs: VAR @ 1:10-1:11
                var: c @ 1:10-1:11
            op: MUL
            rhs: VAR @ 1:14-1:15
                var: d @ 1:14-1:15
    "###);
}

#[test]
fn cmp_many() {
    insta::assert_debug_snapshot!(parse_output("a == (b == c)"), @r###"
    BINOP
        lhs: VAR @ 1:1-1:2
            var: a @ 1:1-1:2
        op: EQUALS
        rhs: BINOP @ 1:6-1:14
            lhs: VAR @ 1:7-1:8
                var: b @ 1:7-1:8
            op: EQUALS
            rhs: VAR @ 1:12-1:13
                var: c @ 1:12-1:13
    "###);
}

#[test]
fn cmp_many_err() {
    insta::assert_snapshot!(parse_error("a == b == c"), @r###"
    BINOP
        lhs: ERROR @ 1:1-1:7
        op: EQUALS
        rhs: VAR @ 1:11-1:12
            var: c @ 1:11-1:12
    --------------------------------------------------
      1 | a == b == c
                 ~~
    Unrecognized token `==` found at 1:8:1:10
    Expected one of ")", "+", ",", "-", ";", "{" or "}"
    "###);
}

#[test]
fn sum_prod() {
    insta::assert_debug_snapshot!(parse_output("a+b*c"), @r###"
    BINOP
        lhs: VAR @ 1:1-1:2
            var: a @ 1:1-1:2
        op: ADD
        rhs: BINOP @ 1:3-1:6
            lhs: VAR @ 1:3-1:4
                var: b @ 1:3-1:4
            op: MUL
            rhs: VAR @ 1:5-1:6
                var: c @ 1:5-1:6
    "###);
}

#[test]
fn lam0() {
    insta::assert_debug_snapshot!(parse_output("fn() { 0 }"), @r###"
    LAM
        body: 0 @ 1:8-1:9
    "###);
}

#[test]
fn lam1() {
    insta::assert_debug_snapshot!(parse_output("fn(x) { x }"), @r###"
    LAM
        param: x @ 1:4-1:5
        body: VAR @ 1:9-1:10
            var: x @ 1:9-1:10
    "###);
}

#[test]
fn lam1_trailing() {
    insta::assert_debug_snapshot!(parse_output("fn(x,) { x }"), @r###"
    LAM
        param: x @ 1:4-1:5
        body: VAR @ 1:10-1:11
            var: x @ 1:10-1:11
    "###);
}

#[test]
fn lam2() {
    insta::assert_debug_snapshot!(parse_output("fn(x, y) { x }"), @r###"
    LAM
        param: x @ 1:4-1:5
        param: y @ 1:7-1:8
        body: VAR @ 1:12-1:13
            var: x @ 1:12-1:13
    "###);
}
#[test]
fn lam1_typed() {
    insta::assert_debug_snapshot!(parse_output("fn(x: Int) { x }"), @r###"
    LAM
        param: x @ 1:4-1:5
        type: Int @ 1:7-1:10
        body: VAR @ 1:14-1:15
            var: x @ 1:14-1:15
    "###);
}

#[test]
fn lam1_poly() {
    insta::assert_snapshot!(parse_error("fn<A>(x: A) { x }"), @r###"
    ERROR
    --------------------------------------------------
      1 | fn<A>(x: A) { x }
            ~
    Unrecognized token `<` found at 1:3:1:4
    Expected one of "("
    --------------------------------------------------
      1 | fn<A>(x: A) { x }
              ~
    Unrecognized token `>` found at 1:5:1:6
    Expected one of ")", "+", ",", "-", ";", "{" or "}"
    --------------------------------------------------
      1 | fn<A>(x: A) { x }
                 ~
    Unrecognized token `:` found at 1:8:1:9
    Expected one of "!=", "(", ")", "*", "+", ",", "-", ".", "/", ";", "<", "<=", "==", ">", ">=", "@", "{" or "}"
    --------------------------------------------------
      1 | fn<A>(x: A) { x }
                      ~
    Unrecognized token `{` found at 1:13:1:14
    "###);
}

#[test]
fn if_atom() {
    insta::assert_debug_snapshot!(parse_output("if true { 0 } else { 1 }"), @r###"
    IF
        cond: true @ 1:4-1:8
        then: 0 @ 1:11-1:12
        else: 1 @ 1:22-1:23
    "###);
}

#[test]
fn if_cmp() {
    insta::assert_debug_snapshot!(parse_output("if a == b { 0 } else { 1 }"), @r###"
    IF
        cond: BINOP @ 1:4-1:10
            lhs: VAR @ 1:4-1:5
                var: a @ 1:4-1:5
            op: EQUALS
            rhs: VAR @ 1:9-1:10
                var: b @ 1:9-1:10
        then: 0 @ 1:13-1:14
        else: 1 @ 1:24-1:25
    "###);
}

#[test]
fn if_else_if() {
    insta::assert_debug_snapshot!(parse_output("if true { 0 } else if false { 1 } else { 2 }"), @r###"
    IF
        cond: true @ 1:4-1:8
        then: 0 @ 1:11-1:12
        else: IF @ 1:20-1:45
            cond: false @ 1:23-1:28
            then: 1 @ 1:31-1:32
            else: 2 @ 1:42-1:43
    "###);
}

#[test]
fn block_atom() {
    insta::assert_debug_snapshot!(parse_block_output("{ a }"), @r###"
    VAR
        var: a @ 1:3-1:4
    "###);
}

#[test]
fn block_record() {
    insta::assert_debug_snapshot!(parse_block_output("{ {f = 1} }"), @r###"
    RECORD
        field: f @ 1:4-1:5
        value: 1 @ 1:8-1:9
    "###);
}

#[test]
fn let1_atom() {
    insta::assert_debug_snapshot!(parse_block_output("{ let x = 1; x }"), @r###"
    LET
        binder: x @ 1:7-1:8
        bindee: 1 @ 1:11-1:12
        tail: VAR @ 1:14-1:15
            var: x @ 1:14-1:15
    "###);
}

#[test]
fn let1_complex() {
    insta::assert_debug_snapshot!(parse_block_output("{ let x = 1 + 1; x }"), @r###"
    LET
        binder: x @ 1:7-1:8
        bindee: BINOP @ 1:11-1:16
            lhs: 1 @ 1:11-1:12
            op: ADD
            rhs: 1 @ 1:15-1:16
        tail: VAR @ 1:18-1:19
            var: x @ 1:18-1:19
    "###);
}

#[test]
fn let1_typed() {
    insta::assert_debug_snapshot!(parse_block_output("{ let x: Int = 1; x }"), @r###"
    LET
        binder: x @ 1:7-1:8
        type: Int @ 1:10-1:13
        bindee: 1 @ 1:16-1:17
        tail: VAR @ 1:19-1:20
            var: x @ 1:19-1:20
    "###);
}

#[test]
fn let1_block() {
    insta::assert_debug_snapshot!(parse_block_output("{ let x = { 1 }; x }"), @r###"
    LET
        binder: x @ 1:7-1:8
        bindee: 1 @ 1:13-1:14
        tail: VAR @ 1:18-1:19
            var: x @ 1:18-1:19
    "###);
}

#[test]
fn let2() {
    insta::assert_debug_snapshot!(parse_block_output("{ let x = 1; let y = x; y }"), @r###"
    LET
        binder: x @ 1:7-1:8
        bindee: 1 @ 1:11-1:12
        tail: LET @ 1:14-1:26
            binder: y @ 1:18-1:19
            bindee: VAR @ 1:22-1:23
                var: x @ 1:22-1:23
            tail: VAR @ 1:25-1:26
                var: y @ 1:25-1:26
    "###);
}

#[test]
fn match1_novar() {
    insta::assert_debug_snapshot!(parse_output("match x { A => 1, }"), @r###"
    MATCH
        scrut: VAR @ 1:7-1:8
            var: x @ 1:7-1:8
        branch: BRANCH
            pattern: PATTERN @ 1:11-1:12
                constr: A
            rhs: 1 @ 1:16-1:17
    "###);
}

#[test]
fn match1_var() {
    insta::assert_debug_snapshot!(parse_output("match x { A(y) => 1, }"), @r###"
    MATCH
        scrut: VAR @ 1:7-1:8
            var: x @ 1:7-1:8
        branch: BRANCH
            pattern: PATTERN @ 1:11-1:15
                constr: A
                binder: y @ 1:13-1:14
            rhs: 1 @ 1:19-1:20
    "###);
}

#[test]
fn match1_block() {
    insta::assert_debug_snapshot!(parse_output("match x { A => { 1 } }"), @r###"
    MATCH
        scrut: VAR @ 1:7-1:8
            var: x @ 1:7-1:8
        branch: BRANCH
            pattern: PATTERN @ 1:11-1:12
                constr: A
            rhs: 1 @ 1:18-1:19
    "###);
}

#[test]
fn match1_expr_nocomma() {
    insta::assert_snapshot!(parse_error("match x { A => 1 }"), @r###"
    ERROR
    --------------------------------------------------
      1 | match x { A => 1 }
                           ~
    Unrecognized token `}` found at 1:18:1:19
    Expected one of ","
    "###);
}

#[test]
fn match1_block_comma() {
    insta::assert_snapshot!(parse_error("match x { A => { 1 }, }"), @r###"
    MATCH
        scrut: VAR @ 1:7-1:8
            var: x @ 1:7-1:8
        branch: BRANCH
            pattern: PATTERN @ 1:11-1:12
                constr: A
            rhs: ERROR @ 1:16-1:21
    --------------------------------------------------
      1 | match x { A => { 1 }, }
                              ~
    Unrecognized token `,` found at 1:21:1:22
    Expected one of "}" or ID_UPPER
    "###);
}

#[test]
fn match2_exprs() {
    insta::assert_debug_snapshot!(parse_output("match x { A => 1, B => 2, }"), @r###"
    MATCH
        scrut: VAR @ 1:7-1:8
            var: x @ 1:7-1:8
        branch: BRANCH
            pattern: PATTERN @ 1:11-1:12
                constr: A
            rhs: 1 @ 1:16-1:17
        branch: BRANCH
            pattern: PATTERN @ 1:19-1:20
                constr: B
            rhs: 2 @ 1:24-1:25
    "###);
}

#[test]
fn match2_expr_block() {
    insta::assert_debug_snapshot!(parse_output("match x { A => 1, B => { 2 } }"), @r###"
    MATCH
        scrut: VAR @ 1:7-1:8
            var: x @ 1:7-1:8
        branch: BRANCH
            pattern: PATTERN @ 1:11-1:12
                constr: A
            rhs: 1 @ 1:16-1:17
        branch: BRANCH
            pattern: PATTERN @ 1:19-1:20
                constr: B
            rhs: 2 @ 1:26-1:27
    "###);
}

#[test]
fn match2_block_expr() {
    insta::assert_debug_snapshot!(parse_output("match x { A => { 1 } B => 2, }"), @r###"
    MATCH
        scrut: VAR @ 1:7-1:8
            var: x @ 1:7-1:8
        branch: BRANCH
            pattern: PATTERN @ 1:11-1:12
                constr: A
            rhs: 1 @ 1:18-1:19
        branch: BRANCH
            pattern: PATTERN @ 1:22-1:23
                constr: B
            rhs: 2 @ 1:27-1:28
    "###);
}

#[test]
fn match2_blocks() {
    insta::assert_debug_snapshot!(parse_output("match x { A => { 1 } B => { 2 } }"), @r###"
    MATCH
        scrut: VAR @ 1:7-1:8
            var: x @ 1:7-1:8
        branch: BRANCH
            pattern: PATTERN @ 1:11-1:12
                constr: A
            rhs: 1 @ 1:18-1:19
        branch: BRANCH
            pattern: PATTERN @ 1:22-1:23
                constr: B
            rhs: 2 @ 1:29-1:30
    "###);
}
