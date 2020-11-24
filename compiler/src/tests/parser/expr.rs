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
    insta::assert_debug_snapshot!(parse_output("x"), @"x");
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
    APP
        fun: f @ 0...1
    "###);
}

#[test]
fn app1() {
    insta::assert_debug_snapshot!(parse_output("f(1)"), @r###"
    APP
        fun: f @ 0...1
        arg: 1 @ 2...3
    "###);
}

#[test]
fn app1_trailing() {
    insta::assert_debug_snapshot!(parse_output("f(1,)"), @r###"
    APP
        fun: f @ 0...1
        arg: 1 @ 2...3
    "###);
}

#[test]
fn app2() {
    insta::assert_debug_snapshot!(parse_output("f(1, 2)"), @r###"
    APP
        fun: f @ 0...1
        arg: 1 @ 2...3
        arg: 2 @ 5...6
    "###);
}
#[test]
fn app_ty() {
    insta::assert_debug_snapshot!(parse_output("f@<Int>(1)"), @r###"
    APP
        fun: FUNCINST @ 0...7
            fun: f @ 0...1
            type_arg: Int @ 3...6
        arg: 1 @ 8...9
    "###);
}

#[test]
fn app_ty_err() {
    insta::assert_snapshot!(parse_error("f<A>(1)"), @r###"
    BINOP
        lhs: ERROR @ 0...3
        op: GREATER
        rhs: 1 @ 4...7
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
        field: x @ 1...2
        value: 1 @ 5...6
    "###);
}

#[test]
fn record1_trailing() {
    insta::assert_debug_snapshot!(parse_output("{x = 1,}"), @r###"
    RECORD
        field: x @ 1...2
        value: 1 @ 5...6
    "###);
}

#[test]
fn record2() {
    insta::assert_debug_snapshot!(parse_output("{x = 1, y = 2}"), @r###"
    RECORD
        field: x @ 1...2
        value: 1 @ 5...6
        field: y @ 8...9
        value: 2 @ 12...13
    "###);
}

#[test]
fn proj1() {
    insta::assert_debug_snapshot!(parse_output("r.x"), @r###"
    PROJ
        record: r @ 0...1
        field: x @ 2...3
    "###);
}

#[test]
fn proj2() {
    insta::assert_debug_snapshot!(parse_output("r.x.y"), @r###"
    PROJ
        record: PROJ @ 0...3
            record: r @ 0...1
            field: x @ 2...3
        field: y @ 4...5
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
        payload: 0 @ 2...3
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
        lhs: a @ 0...1
        op: MUL
        rhs: b @ 2...3
    "###);
}

#[test]
fn prod3() {
    insta::assert_debug_snapshot!(parse_output("a/b*c"), @r###"
    BINOP
        lhs: BINOP @ 0...3
            lhs: a @ 0...1
            op: DIV
            rhs: b @ 2...3
        op: MUL
        rhs: c @ 4...5
    "###);
}

#[test]
fn sum2() {
    insta::assert_debug_snapshot!(parse_output("a+b"), @r###"
    BINOP
        lhs: a @ 0...1
        op: ADD
        rhs: b @ 2...3
    "###);
}

#[test]
fn sum3() {
    insta::assert_debug_snapshot!(parse_output("a-b+c"), @r###"
    BINOP
        lhs: BINOP @ 0...3
            lhs: a @ 0...1
            op: SUB
            rhs: b @ 2...3
        op: ADD
        rhs: c @ 4...5
    "###);
}

#[test]
fn cmp_eq() {
    insta::assert_debug_snapshot!(parse_output("a == b"), @r###"
    BINOP
        lhs: a @ 0...1
        op: EQUALS
        rhs: b @ 5...6
    "###);
}

#[test]
fn cmp_neq() {
    insta::assert_debug_snapshot!(parse_output("a != b"), @r###"
    BINOP
        lhs: a @ 0...1
        op: NOTEQ
        rhs: b @ 5...6
    "###);
}

#[test]
fn cmp_lt() {
    insta::assert_debug_snapshot!(parse_output("a < b"), @r###"
    BINOP
        lhs: a @ 0...1
        op: LESS
        rhs: b @ 4...5
    "###);
}

#[test]
fn cmp_leq() {
    insta::assert_debug_snapshot!(parse_output("a <= b"), @r###"
    BINOP
        lhs: a @ 0...1
        op: LESSEQ
        rhs: b @ 5...6
    "###);
}

#[test]
fn cmp_gt() {
    insta::assert_debug_snapshot!(parse_output("a > b"), @r###"
    BINOP
        lhs: a @ 0...1
        op: GREATER
        rhs: b @ 4...5
    "###);
}

#[test]
fn cmp_geq() {
    insta::assert_debug_snapshot!(parse_output("a >= b"), @r###"
    BINOP
        lhs: a @ 0...1
        op: GREATEREQ
        rhs: b @ 5...6
    "###);
}

#[test]
fn cmp_mixed() {
    insta::assert_debug_snapshot!(parse_output("a + b == c * d"), @r###"
    BINOP
        lhs: BINOP @ 0...5
            lhs: a @ 0...1
            op: ADD
            rhs: b @ 4...5
        op: EQUALS
        rhs: BINOP @ 9...14
            lhs: c @ 9...10
            op: MUL
            rhs: d @ 13...14
    "###);
}

#[test]
fn cmp_many() {
    insta::assert_debug_snapshot!(parse_output("a == (b == c)"), @r###"
    BINOP
        lhs: a @ 0...1
        op: EQUALS
        rhs: BINOP @ 5...13
            lhs: b @ 6...7
            op: EQUALS
            rhs: c @ 11...12
    "###);
}

#[test]
fn cmp_many_err() {
    insta::assert_snapshot!(parse_error("a == b == c"), @r###"
    BINOP
        lhs: ERROR @ 0...6
        op: EQUALS
        rhs: c @ 10...11
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
        lhs: a @ 0...1
        op: ADD
        rhs: BINOP @ 2...5
            lhs: b @ 2...3
            op: MUL
            rhs: c @ 4...5
    "###);
}

#[test]
fn lam0() {
    insta::assert_debug_snapshot!(parse_output("fn() { 0 }"), @r###"
    LAM
        body: 0 @ 7...8
    "###);
}

#[test]
fn lam1() {
    insta::assert_debug_snapshot!(parse_output("fn(x) { x }"), @r###"
    LAM
        param: x @ 3...4
        body: x @ 8...9
    "###);
}

#[test]
fn lam1_trailing() {
    insta::assert_debug_snapshot!(parse_output("fn(x,) { x }"), @r###"
    LAM
        param: x @ 3...4
        body: x @ 9...10
    "###);
}

#[test]
fn lam2() {
    insta::assert_debug_snapshot!(parse_output("fn(x, y) { x }"), @r###"
    LAM
        param: x @ 3...4
        param: y @ 6...7
        body: x @ 11...12
    "###);
}
#[test]
fn lam1_typed() {
    insta::assert_debug_snapshot!(parse_output("fn(x: Int) { x }"), @r###"
    LAM
        param: x @ 3...4
        type: Int @ 6...9
        body: x @ 13...14
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
    Expected one of "!=", "(", ")", "*", "+", ",", "-", ".", "/", ";", "<", "<=", "=", "==", ">", ">=", "@", "{" or "}"
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
        cond: true @ 3...7
        then: 0 @ 10...11
        else: 1 @ 21...22
    "###);
}

#[test]
fn if_cmp() {
    insta::assert_debug_snapshot!(parse_output("if a == b { 0 } else { 1 }"), @r###"
    IF
        cond: BINOP @ 3...9
            lhs: a @ 3...4
            op: EQUALS
            rhs: b @ 8...9
        then: 0 @ 12...13
        else: 1 @ 23...24
    "###);
}

#[test]
fn if_else_if() {
    insta::assert_debug_snapshot!(parse_output("if true { 0 } else if false { 1 } else { 2 }"), @r###"
    IF
        cond: true @ 3...7
        then: 0 @ 10...11
        else: IF @ 19...44
            cond: false @ 22...27
            then: 1 @ 30...31
            else: 2 @ 41...42
    "###);
}

#[test]
fn block_atom() {
    insta::assert_debug_snapshot!(parse_block_output("{ a }"), @"a");
}

#[test]
fn block_record() {
    insta::assert_debug_snapshot!(parse_block_output("{ {f = 1} }"), @r###"
    RECORD
        field: f @ 3...4
        value: 1 @ 7...8
    "###);
}

#[test]
fn let1_atom() {
    insta::assert_debug_snapshot!(parse_block_output("{ let x = 1; x }"), @r###"
    LET
        binder: x @ 6...7
        bindee: 1 @ 10...11
        tail: x @ 13...14
    "###);
}

#[test]
fn let1_complex() {
    insta::assert_debug_snapshot!(parse_block_output("{ let x = 1 + 1; x }"), @r###"
    LET
        binder: x @ 6...7
        bindee: BINOP @ 10...15
            lhs: 1 @ 10...11
            op: ADD
            rhs: 1 @ 14...15
        tail: x @ 17...18
    "###);
}

#[test]
fn let1_typed() {
    insta::assert_debug_snapshot!(parse_block_output("{ let x: Int = 1; x }"), @r###"
    LET
        binder: x @ 6...7
        type: Int @ 9...12
        bindee: 1 @ 15...16
        tail: x @ 18...19
    "###);
}

#[test]
fn let1_block() {
    insta::assert_debug_snapshot!(parse_block_output("{ let x = { 1 }; x }"), @r###"
    LET
        binder: x @ 6...7
        bindee: 1 @ 12...13
        tail: x @ 17...18
    "###);
}

#[test]
fn let2() {
    insta::assert_debug_snapshot!(parse_block_output("{ let x = 1; let y = x; y }"), @r###"
    LET
        binder: x @ 6...7
        bindee: 1 @ 10...11
        tail: LET @ 13...25
            binder: y @ 17...18
            bindee: x @ 21...22
            tail: y @ 24...25
    "###);
}

#[test]
fn match1_novar() {
    insta::assert_debug_snapshot!(parse_output("match x { A => 1, }"), @r###"
    MATCH
        scrut: x @ 6...7
        branch: BRANCH
            pattern: PATTERN @ 10...11
                constr: A
            rhs: 1 @ 15...16
    "###);
}

#[test]
fn match1_var() {
    insta::assert_debug_snapshot!(parse_output("match x { A(y) => 1, }"), @r###"
    MATCH
        scrut: x @ 6...7
        branch: BRANCH
            pattern: PATTERN @ 10...14
                constr: A
                binder: y @ 12...13
            rhs: 1 @ 18...19
    "###);
}

#[test]
fn match1_block() {
    insta::assert_debug_snapshot!(parse_output("match x { A => { 1 } }"), @r###"
    MATCH
        scrut: x @ 6...7
        branch: BRANCH
            pattern: PATTERN @ 10...11
                constr: A
            rhs: 1 @ 17...18
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
        scrut: x @ 6...7
        branch: BRANCH
            pattern: PATTERN @ 10...11
                constr: A
            rhs: ERROR @ 15...20
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
        scrut: x @ 6...7
        branch: BRANCH
            pattern: PATTERN @ 10...11
                constr: A
            rhs: 1 @ 15...16
        branch: BRANCH
            pattern: PATTERN @ 18...19
                constr: B
            rhs: 2 @ 23...24
    "###);
}

#[test]
fn match2_expr_block() {
    insta::assert_debug_snapshot!(parse_output("match x { A => 1, B => { 2 } }"), @r###"
    MATCH
        scrut: x @ 6...7
        branch: BRANCH
            pattern: PATTERN @ 10...11
                constr: A
            rhs: 1 @ 15...16
        branch: BRANCH
            pattern: PATTERN @ 18...19
                constr: B
            rhs: 2 @ 25...26
    "###);
}

#[test]
fn match2_block_expr() {
    insta::assert_debug_snapshot!(parse_output("match x { A => { 1 } B => 2, }"), @r###"
    MATCH
        scrut: x @ 6...7
        branch: BRANCH
            pattern: PATTERN @ 10...11
                constr: A
            rhs: 1 @ 17...18
        branch: BRANCH
            pattern: PATTERN @ 21...22
                constr: B
            rhs: 2 @ 26...27
    "###);
}

#[test]
fn match2_blocks() {
    insta::assert_debug_snapshot!(parse_output("match x { A => { 1 } B => { 2 } }"), @r###"
    MATCH
        scrut: x @ 6...7
        branch: BRANCH
            pattern: PATTERN @ 10...11
                constr: A
            rhs: 1 @ 17...18
        branch: BRANCH
            pattern: PATTERN @ 21...22
                constr: B
            rhs: 2 @ 28...29
    "###);
}
