// Tests for variable vs function resolution.
use super::*;

#[test]
fn resolve_func_mono() {
    insta::assert_debug_snapshot!(check_output_func_body("f", r#"
    fn resolve_me() -> Int { 0 }
    fn f() -> Int { resolve_me() }
    "#), @r###"
    APPFUN
        fun: resolve_me @ 54...64
    "###);
}

#[test]
fn resolve_func_poly() {
    insta::assert_debug_snapshot!(check_output_func_body("f", r#"
    fn resolve_me<A>() -> Int { 0 }
    fn f() -> Int { resolve_me@<Int>() }
    "#), @r###"
    APPFUN
        fun: resolve_me @ 57...67
        type_arg: INT @ 69...72
    "###);
}

#[test]
fn resolve_func_param() {
    insta::assert_debug_snapshot!(check_output_func_body("f", r#"
    fn resolve_me() -> Bool { true }
    fn f(resolve_me: () -> Int) -> Int { resolve_me() }
    "#), @r###"
    APPCLO
        clo: resolve_me @ 79...89
    "###);
}

#[test]
fn resolve_let_infer_infer() {
    insta::assert_debug_snapshot!(check_output_func_body("f", r#"
    fn resolve_me() -> Bool { true }
    fn f() -> Int {
        let r = {
            let resolve_me = fn () { 0 };
            resolve_me()
        };
        r
    }
    "#), @r###"
    LET
        binder: r @ 70...71
        type: INT @ 70...71
        bindee: LET @ 88...142
            binder: resolve_me @ 92...102
            type: FUN @ 92...102
                result: INT @ 0...0
            bindee: LAM @ 105...116
                body: 0 @ 113...114
            tail: APPCLO @ 130...142
                clo: resolve_me @ 130...140
        tail: r @ 162...163
    "###);
}

#[test]
fn resolve_let_infer_check() {
    insta::assert_debug_snapshot!(check_output_func_body("f", r#"
    fn resolve_me() -> Bool { true }
    fn f() -> Int {
        let resolve_me = fn () { 0 };
        resolve_me()
    }
    "#), @r###"
    LET
        binder: resolve_me @ 70...80
        type: FUN @ 70...80
            result: INT @ 0...0
        bindee: LAM @ 83...94
            body: 0 @ 91...92
        tail: APPCLO @ 104...116
            clo: resolve_me @ 104...114
    "###);
}

#[test]
fn resolve_let_check_infer() {
    insta::assert_debug_snapshot!(check_output_func_body("f", r#"
    fn resolve_me() -> Bool { true }
    fn f() -> Int {
        let r = {
            let resolve_me: () -> Int = fn () { 0 };
            resolve_me()
        };
        r
    }
    "#), @r###"
    LET
        binder: r @ 70...71
        type: INT @ 70...71
        bindee: LET @ 88...153
            binder: resolve_me @ 92...102
            type: FUN @ 104...113
                result: INT @ 110...113
            bindee: LAM @ 116...127
                body: 0 @ 124...125
            tail: APPCLO @ 141...153
                clo: resolve_me @ 141...151
        tail: r @ 173...174
    "###);
}

#[test]
fn resolve_let_check_check() {
    insta::assert_debug_snapshot!(check_output_func_body("f", r#"
    fn resolve_me() -> Bool { true }
    fn f() -> Int {
        let resolve_me: () -> Int = fn () { 0 };
        resolve_me()
    }
    "#), @r###"
    LET
        binder: resolve_me @ 70...80
        type: FUN @ 82...91
            result: INT @ 88...91
        bindee: LAM @ 94...105
            body: 0 @ 102...103
        tail: APPCLO @ 115...127
            clo: resolve_me @ 115...125
    "###);
}

#[test]
fn resolve_lam_param_infer() {
    insta::assert_debug_snapshot!(check_output_func_body("f", r#"
    fn resolve_me() -> Bool { true }
    fn f() -> (() -> Int) -> Int {
        let f = fn (resolve_me: () -> Int) { resolve_me() };
        f
    }
    "#), @r###"
    LET
        binder: f @ 85...86
        type: FUN @ 85...86
            param: FUN @ 0...0
                result: INT @ 0...0
            result: INT @ 0...0
        bindee: LAM @ 89...132
            param: resolve_me @ 93...103
            type: FUN @ 105...114
                result: INT @ 111...114
            body: APPCLO @ 118...130
                clo: resolve_me @ 118...128
        tail: f @ 142...143
    "###);
}

#[test]
fn resolve_lam_param_check_unannotated() {
    insta::assert_debug_snapshot!(check_output_func_body("f", r#"
    fn resolve_me() -> Bool { true }
    fn f() -> (() -> Int) -> Int {
        fn (resolve_me) { resolve_me() }
    }
    "#), @r###"
    LAM
        param: resolve_me @ 85...95
        type: FUN @ 85...95
            result: INT @ 0...0
        body: APPCLO @ 99...111
            clo: resolve_me @ 99...109
    "###);
}

#[test]
fn resolve_lam_param_check_annotated() {
    insta::assert_debug_snapshot!(check_output_func_body("f", r#"
    fn resolve_me() -> Bool { true }
    fn f() -> (() -> Int) -> Int {
        fn (resolve_me: () -> Int) { resolve_me() }
    }
    "#), @r###"
    LAM
        param: resolve_me @ 85...95
        type: FUN @ 97...106
            result: INT @ 103...106
        body: APPCLO @ 110...122
            clo: resolve_me @ 110...120
    "###);
}

#[test]
fn resolve_match() {
    insta::assert_debug_snapshot!(check_output_func_body("f", r#"
    fn resolve_me() -> Bool { true }
    fn f() -> Int {
        let f: [F(() -> Int)] = F(fn () { 0 });
        match f {
            F(resolve_me) => resolve_me(),
        }
    }
    "#), @r###"
    LET
        binder: f @ 70...71
        type: VARIANT @ 73...87
            constr: F @ 74...75
            type: FUN @ 76...85
                result: INT @ 82...85
        bindee: VARIANT @ 90...104
            constr: F/0
            payload: LAM @ 92...103
                body: 0 @ 100...101
        tail: MATCH @ 114...176
            scrut: f @ 120...121
            branch: BRANCH
                pattern: PATTERN @ 136...149
                    constr: F/0
                    binder: resolve_me @ 138...148
                rhs: APPCLO @ 153...165
                    clo: resolve_me @ 153...163
    "###);
}

#[test]
fn resolve_implicit_closure_let() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int { 0 }
    fn g() -> Int {
        let h = f;
        h()
    }
    "#), @r###"
      4 |         let h = f;
                          ~
    Undeclared variable `f`. There is a function of the same name.
    If you want to use the function as a closure, you have to wrap it
    explicitly: `fn (...) { f(...) }`.
    "###);
}

#[test]
fn resolve_implicit_closure_arg() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int { 0 }
    fn g(h: (() -> Int) -> Int) -> Int {
        h(f)
    }
    "#), @r###"
      4 |         h(f)
                    ~
    Undeclared variable `f`. There is a function of the same name.
    If you want to use the function as a closure, you have to wrap it
    explicitly: `fn (...) { f(...) }`.
    "###);
}
