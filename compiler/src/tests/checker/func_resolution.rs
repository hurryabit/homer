// Tests for variable vs function resolution.
use super::*;

#[test]
fn resolve_func_mono() {
    insta::assert_debug_snapshot!(check_output_func_body("f", r#"
    fn resolve_me() -> Int { 0 }
    fn f() -> Int { resolve_me() }
    "#), @r###"
    APPFUN
        fun: resolve_me @ 3:21-3:31
    "###);
}

#[test]
fn resolve_func_poly() {
    insta::assert_debug_snapshot!(check_output_func_body("f", r#"
    fn resolve_me<A>() -> Int { 0 }
    fn f() -> Int { resolve_me@<Int>() }
    "#), @r###"
    APPFUN
        fun: resolve_me @ 3:21-3:31
        type_arg: INT @ 3:33-3:36
    "###);
}

#[test]
fn resolve_func_param() {
    insta::assert_debug_snapshot!(check_output_func_body("f", r#"
    fn resolve_me() -> Bool { true }
    fn f(resolve_me: () -> Int) -> Int { resolve_me() }
    "#), @r###"
    APPCLO
        clo: resolve_me @ 3:42-3:52
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
        binder: r @ 4:13-4:14
        type: INFERRED @ 4:13-4:14
            type: INT
        bindee: LET @ 5:13-6:25
            binder: resolve_me @ 5:17-5:27
            type: INFERRED @ 5:17-5:27
                type: FUN
                    result: INT
            bindee: LAM @ 5:30-5:41
                body: 0 @ 5:38-5:39
            tail: APPCLO @ 6:13-6:25
                clo: resolve_me @ 6:13-6:23
        tail: r @ 8:9-8:10
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
        binder: resolve_me @ 4:13-4:23
        type: INFERRED @ 4:13-4:23
            type: FUN
                result: INT
        bindee: LAM @ 4:26-4:37
            body: 0 @ 4:34-4:35
        tail: APPCLO @ 5:9-5:21
            clo: resolve_me @ 5:9-5:19
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
        binder: r @ 4:13-4:14
        type: INFERRED @ 4:13-4:14
            type: INT
        bindee: LET @ 5:13-6:25
            binder: resolve_me @ 5:17-5:27
            type: FUN @ 5:29-5:38
                result: INT @ 5:35-5:38
            bindee: LAM @ 5:41-5:52
                body: 0 @ 5:49-5:50
            tail: APPCLO @ 6:13-6:25
                clo: resolve_me @ 6:13-6:23
        tail: r @ 8:9-8:10
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
        binder: resolve_me @ 4:13-4:23
        type: FUN @ 4:25-4:34
            result: INT @ 4:31-4:34
        bindee: LAM @ 4:37-4:48
            body: 0 @ 4:45-4:46
        tail: APPCLO @ 5:9-5:21
            clo: resolve_me @ 5:9-5:19
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
        binder: f @ 4:13-4:14
        type: INFERRED @ 4:13-4:14
            type: FUN
                param: FUN
                    result: INT
                result: INT
        bindee: LAM @ 4:17-4:60
            param: resolve_me @ 4:21-4:31
            type: FUN @ 4:33-4:42
                result: INT @ 4:39-4:42
            body: APPCLO @ 4:46-4:58
                clo: resolve_me @ 4:46-4:56
        tail: f @ 5:9-5:10
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
        param: resolve_me @ 4:13-4:23
        type: INFERRED @ 4:13-4:23
            type: FUN
                result: INT
        body: APPCLO @ 4:27-4:39
            clo: resolve_me @ 4:27-4:37
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
        param: resolve_me @ 4:13-4:23
        type: FUN @ 4:25-4:34
            result: INT @ 4:31-4:34
        body: APPCLO @ 4:38-4:50
            clo: resolve_me @ 4:38-4:48
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
        binder: f @ 4:13-4:14
        type: VARIANT @ 4:16-4:30
            constr: F @ 4:17-4:18
            type: FUN @ 4:19-4:28
                result: INT @ 4:25-4:28
        bindee: VARIANT @ 4:33-4:47
            constr: F/0
            payload: LAM @ 4:35-4:46
                body: 0 @ 4:43-4:44
        tail: MATCH @ 5:9-7:10
            scrut: f @ 5:15-5:16
            branch: BRANCH
                pattern: PATTERN @ 6:13-6:26
                    constr: F/0
                    binder: resolve_me @ 6:15-6:25
                rhs: APPCLO @ 6:30-6:42
                    clo: resolve_me @ 6:30-6:40
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
