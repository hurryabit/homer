// Tests for variable vs function resolution.
use super::*;

#[test]
fn resolve_var_check_fun_param() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Bool { true }
    fn f(resolve_me: Int) -> Int {
        resolve_me
    }
    "#), @r###"
    [
        VAR "resolve_me" @ 4:9-4:19,
    ]
    "###);
}

#[test]
fn resolve_var_infer_fun_param() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Bool { true }
    fn f(resolve_me: Int) -> Int {
        let x = resolve_me;
        x
    }
    "#), @r###"
    [
        VAR "resolve_me" @ 4:17-4:27,
        VAR "x" @ 5:9-5:10,
    ]
    "###);
}

#[test]
fn resolve_var_check_lam_check_param() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Bool { true }
    fn f() -> (Int) -> Int {
        fn (resolve_me) { resolve_me }
    }
    "#), @r###"
    [
        VAR "resolve_me" @ 4:27-4:37,
    ]
    "###);
}

#[test]
fn resolve_var_infer_lam_check_param() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Bool { true }
    fn f() -> (Int) -> Int {
        fn (resolve_me) {
            let x = resolve_me;
            x
        }
    }
    "#), @r###"
    [
        VAR "resolve_me" @ 5:21-5:31,
        VAR "x" @ 6:13-6:14,
    ]
    "###);
}

#[test]
fn resolve_var_check_lam_infer_param() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Bool { true }
    fn f() -> (Int) -> Int {
        let f = fn (resolve_me: Int) { resolve_me };
        f
    }
    "#), @r###"
    [
        VAR "resolve_me" @ 4:40-4:50,
        VAR "f" @ 5:9-5:10,
    ]
    "###);
}

#[test]
fn resolve_var_infer_lam_infer_param() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Bool { true }
    fn f() -> (Int) -> Int {
        let f = fn (resolve_me: Int) {
            let x = resolve_me;
            x
        };
        f
    }
    "#), @r###"
    [
        VAR "resolve_me" @ 5:21-5:31,
        VAR "x" @ 6:13-6:14,
        VAR "f" @ 8:9-8:10,
    ]
    "###);
}

#[test]
fn resolve_var_check_let_check_bound() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Bool { true }
    fn f() -> Int {
        let resolve_me: Int = 0;
        resolve_me
    }
    "#), @r###"
    [
        VAR "resolve_me" @ 5:9-5:19,
    ]
    "###);
}

#[test]
fn resolve_var_infer_let_check_bound() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Bool { true }
    fn f() -> Int {
        let resolve_me: Int = 0;
        let x = resolve_me;
        x
    }
    "#), @r###"
    [
        VAR "resolve_me" @ 5:17-5:27,
        VAR "x" @ 6:9-6:10,
    ]
    "###);
}

#[test]
fn resolve_var_check_let_infer_bound() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Bool { true }
    fn f() -> Int {
        let resolve_me = 0;
        resolve_me
    }
    "#), @r###"
    [
        VAR "resolve_me" @ 5:9-5:19,
    ]
    "###);
}

#[test]
fn resolve_var_infer_let_infer_bound() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Bool { true }
    fn f() -> Int {
        let resolve_me = 0;
        let x = resolve_me;
        x
    }
    "#), @r###"
    [
        VAR "resolve_me" @ 5:17-5:27,
        VAR "x" @ 6:9-6:10,
    ]
    "###);
}

#[test]
fn resolve_var_check_branch_bound() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Bool { true }
    fn f(x: [C(Int)]) -> Int {
        match x {
            C(resolve_me) => resolve_me,
        }
    }
    "#), @r###"
    [
        VAR "x" @ 4:15-4:16,
        VAR "resolve_me" @ 5:30-5:40,
    ]
    "###);
}

#[test]
fn resolve_var_infer_branch_bound() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Bool { true }
    fn f(x: [C(Int)]) -> Int {
        match x {
            C(resolve_me) => {
                let x = resolve_me;
                x
            }
        }
    }
    "#), @r###"
    [
        VAR "x" @ 4:15-4:16,
        VAR "resolve_me" @ 6:25-6:35,
        VAR "x" @ 7:17-7:18,
    ]
    "###);
}

#[test]
fn resolve_var_check_unknown() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        resolve_me
    }
    "#), @r###"
      3 |         resolve_me
                  ~~~~~~~~~~
    Undeclared variable `resolve_me`.
    "###);
}

#[test]
fn resolve_var_infer_unknown() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let x = resolve_me;
        x
    }
    "#), @r###"
      3 |         let x = resolve_me;
                          ~~~~~~~~~~
    Undeclared variable `resolve_me`.
    "###);
}

#[test]
fn resolve_var_check_is_func() {
    insta::assert_snapshot!(check_error(r#"
    fn resolve_me() -> Int { 0 }
    fn g() -> () -> Int {
        resolve_me
    }
    "#), @r###"
      4 |         resolve_me
                  ~~~~~~~~~~
    Undeclared variable `resolve_me`. There is a function of the same name.
    If you want to use the function as a closure, you have to wrap it
    explicitly: `fn (...) { resolve_me(...) }`.
    "###);
}

#[test]
fn resolve_var_infer_is_func() {
    insta::assert_snapshot!(check_error(r#"
    fn resolve_me() -> Int { 0 }
    fn g() -> () -> Int {
        let x = resolve_me;
        x
    }
    "#), @r###"
      4 |         let x = resolve_me;
                          ~~~~~~~~~~
    Undeclared variable `resolve_me`. There is a function of the same name.
    If you want to use the function as a closure, you have to wrap it
    explicitly: `fn (...) { resolve_me(...) }`.
    "###);
}

#[test]
fn resolve_clo_check_fun_param() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Bool { true }
    fn f(resolve_me: () -> Int) -> Int {
        resolve_me()
    }
    "#), @r###"
    [
        CLO "resolve_me" @ 4:9-4:19,
    ]
    "###);
}

#[test]
fn resolve_clo_infer_fun_param() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Bool { true }
    fn f(resolve_me: () -> Int) -> Int {
        let x = resolve_me();
        x
    }
    "#), @r###"
    [
        CLO "resolve_me" @ 4:17-4:27,
        VAR "x" @ 5:9-5:10,
    ]
    "###);
}

#[test]
fn resolve_clo_check_lam_check_param() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Bool { true }
    fn f() -> (() -> Int) -> Int {
        fn (resolve_me) { resolve_me() }
    }
    "#), @r###"
    [
        CLO "resolve_me" @ 4:27-4:37,
    ]
    "###);
}

#[test]
fn resolve_clo_infer_lam_check_param() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Bool { true }
    fn f() -> (() -> Int) -> Int {
        fn (resolve_me) {
            let x = resolve_me();
            x
        }
    }
    "#), @r###"
    [
        CLO "resolve_me" @ 5:21-5:31,
        VAR "x" @ 6:13-6:14,
    ]
    "###);
}

#[test]
fn resolve_clo_check_lam_infer_param() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Bool { true }
    fn f() -> (() -> Int) -> Int {
        let f = fn (resolve_me: () -> Int) { resolve_me() };
        f
    }
    "#), @r###"
    [
        CLO "resolve_me" @ 4:46-4:56,
        VAR "f" @ 5:9-5:10,
    ]
    "###);
}

#[test]
fn resolve_clo_infer_lam_infer_param() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Bool { true }
    fn f() -> (() -> Int) -> Int {
        let f = fn (resolve_me: () -> Int) {
            let x = resolve_me();
            x
        };
        f
    }
    "#), @r###"
    [
        CLO "resolve_me" @ 5:21-5:31,
        VAR "x" @ 6:13-6:14,
        VAR "f" @ 8:9-8:10,
    ]
    "###);
}

#[test]
fn resolve_clo_check_let_check_bound() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Bool { true }
    fn f() -> Int {
        let resolve_me: () -> Int = fn () { 0 };
        resolve_me()
    }
    "#), @r###"
    [
        CLO "resolve_me" @ 5:9-5:19,
    ]
    "###);
}

#[test]
fn resolve_clo_infer_let_check_bound() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Bool { true }
    fn f() -> Int {
        let resolve_me: () -> Int = fn () { 0 };
        let x = resolve_me();
        x
    }
    "#), @r###"
    [
        CLO "resolve_me" @ 5:17-5:27,
        VAR "x" @ 6:9-6:10,
    ]
    "###);
}

#[test]
fn resolve_clo_check_let_infer_bound() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Bool { true }
    fn f() -> Int {
        let resolve_me = fn () { 0 };
        resolve_me()
    }
    "#), @r###"
    [
        CLO "resolve_me" @ 5:9-5:19,
    ]
    "###);
}

#[test]
fn resolve_clo_infer_let_infer_bound() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Bool { true }
    fn f() -> Int {
        let resolve_me = fn () { 0 };
        let x = resolve_me();
        x
    }
    "#), @r###"
    [
        CLO "resolve_me" @ 5:17-5:27,
        VAR "x" @ 6:9-6:10,
    ]
    "###);
}

#[test]
fn resolve_clo_check_branch_bound() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Bool { true }
    fn f(x: [C(() -> Int)]) -> Int {
        match x {
            C(resolve_me) => resolve_me(),
        }
    }
    "#), @r###"
    [
        VAR "x" @ 4:15-4:16,
        CLO "resolve_me" @ 5:30-5:40,
    ]
    "###);
}

#[test]
fn resolve_clo_infer_branch_bound() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Bool { true }
    fn f(x: [C(() -> Int)]) -> Int {
        match x {
            C(resolve_me) => {
                let x = resolve_me();
                x
            }
        }
    }
    "#), @r###"
    [
        VAR "x" @ 4:15-4:16,
        CLO "resolve_me" @ 6:25-6:35,
        VAR "x" @ 7:17-7:18,
    ]
    "###);
}

#[test]
fn resolve_clo_check_unknown() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        resolve_me()
    }
    "#), @r###"
      3 |         resolve_me()
                  ~~~~~~~~~~
    Undeclared variable `resolve_me`.
    "###);
}

#[test]
fn resolve_clo_infer_unknown() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let x = resolve_me();
        x
    }
    "#), @r###"
      3 |         let x = resolve_me();
                          ~~~~~~~~~~
    Undeclared variable `resolve_me`.
    "###);
}

#[test]
fn resolve_mono_fun_check() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Int { 0 }
    fn f() -> Int {
        resolve_me()
    }
    "#), @r###"
    [
        FUN "resolve_me" @ 4:9-4:19,
    ]
    "###);
}

#[test]
fn resolve_mono_fun_infer() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me() -> Int { 0 }
    fn f() -> Int {
        let x = resolve_me();
        x
    }
    "#), @r###"
    [
        FUN "resolve_me" @ 4:17-4:27,
        VAR "x" @ 5:9-5:10,
    ]
    "###);
}

#[test]
fn resolve_poly_fun_check_poly_fun() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me<A>() -> Int { 0 }
    fn f() -> Int {
        resolve_me@<Int>()
    }
    "#), @r###"
    [
        FUN "resolve_me" @ 4:9-4:19,
    ]
    "###);
}

#[test]
fn resolve_poly_fun_infer_poly_fun() {
    insta::assert_debug_snapshot!(check_output_func_refs("f", r#"
    fn resolve_me<A>() -> Int { 0 }
    fn f() -> Int {
        let x = resolve_me@<Int>();
        x
    }
    "#), @r###"
    [
        FUN "resolve_me" @ 4:17-4:27,
        VAR "x" @ 5:9-5:10,
    ]
    "###);
}

#[test]
fn resolve_poly_fun_check_unknown() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        resolve_me@<Int>()
    }
    "#), @r###"
      3 |         resolve_me@<Int>()
                  ~~~~~~~~~~
    Undeclared variable `resolve_me`.
    "###);
}

#[test]
fn resolve_poly_fun_infer_unknown() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let x = resolve_me@<Int>();
        x
    }
    "#), @r###"
      3 |         let x = resolve_me@<Int>();
                          ~~~~~~~~~~
    Undeclared variable `resolve_me`.
    "###);
}

#[test]
fn resolve_poly_fun_check_mono_fun() {
    insta::assert_snapshot!(check_error(r#"
    fn resolve_me() -> Int { 0 }
    fn f() -> Int {
        resolve_me@<Int>()
    }
    "#), @r###"
      4 |         resolve_me@<Int>()
                  ~~~~~~~~~~
    `resolve_me` is not a generic function and must be called as `resolve_me(...)`.
    "###);
}

#[test]
fn resolve_poly_fun_infer_mono_fun() {
    insta::assert_snapshot!(check_error(r#"
    fn resolve_me() -> Int { 0 }
    fn f() -> Int {
        let x = resolve_me@<Int>();
        x
    }
    "#), @r###"
      4 |         let x = resolve_me@<Int>();
                          ~~~~~~~~~~
    `resolve_me` is not a generic function and must be called as `resolve_me(...)`.
    "###);
}

#[test]
fn resolve_poly_fun_check_clo() {
    insta::assert_snapshot!(check_error(r#"
    fn resolve_me<A>() -> Int { 0 }
    fn f() -> Int {
        let resolve_me = fn() { 0 };
        resolve_me@<Int>()
    }
    "#), @r###"
      5 |         resolve_me@<Int>()
                  ~~~~~~~~~~
    `resolve_me` is not a generic function and must be called as `resolve_me(...)`.
    "###);
}

#[test]
fn resolve_poly_fun_infer_clo() {
    insta::assert_snapshot!(check_error(r#"
    fn resolve_me<A>() -> Int { 0 }
    fn f() -> Int {
        let x = {
            let resolve_me = fn () { 0 };
            resolve_me@<Int>()
        };
        x
    }
    "#), @r###"
      6 |             resolve_me@<Int>()
                      ~~~~~~~~~~
    `resolve_me` is not a generic function and must be called as `resolve_me(...)`.
    "###);
}

#[test]
fn resolve_poly0_fun_check_unknown() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        resolve_me@<>()
    }
    "#), @r###"
      3 |         resolve_me@<>()
                  ~~~~~~~~~~
    Undeclared variable `resolve_me`.
    "###);
}

#[test]
fn resolve_poly0_fun_infer_unknown() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let x = resolve_me@<>();
        x
    }
    "#), @r###"
      3 |         let x = resolve_me@<>();
                          ~~~~~~~~~~
    Undeclared variable `resolve_me`.
    "###);
}

#[test]
fn resolve_poly0_fun_check_mono_fun() {
    insta::assert_snapshot!(check_error(r#"
    fn resolve_me() -> Int { 0 }
    fn f() -> Int {
        resolve_me@<>()
    }
    "#), @r###"
      4 |         resolve_me@<>()
                  ~~~~~~~~~~
    `resolve_me` is not a generic function and must be called as `resolve_me(...)`.
    "###);
}

#[test]
fn resolve_poly0_fun_infer_mono_fun() {
    insta::assert_snapshot!(check_error(r#"
    fn resolve_me() -> Int { 0 }
    fn f() -> Int {
        let x = resolve_me@<>();
        x
    }
    "#), @r###"
      4 |         let x = resolve_me@<>();
                          ~~~~~~~~~~
    `resolve_me` is not a generic function and must be called as `resolve_me(...)`.
    "###);
}

#[test]
fn resolve_poly0_fun_check_clo() {
    insta::assert_snapshot!(check_error(r#"
    fn resolve_me<A>() -> Int { 0 }
    fn f() -> Int {
        let resolve_me = fn() { 0 };
        resolve_me@<>()
    }
    "#), @r###"
      5 |         resolve_me@<>()
                  ~~~~~~~~~~
    `resolve_me` is not a generic function and must be called as `resolve_me(...)`.
    "###);
}

#[test]
fn resolve_poly0_fun_infer_clo() {
    insta::assert_snapshot!(check_error(r#"
    fn resolve_me<A>() -> Int { 0 }
    fn f() -> Int {
        let x = {
            let resolve_me = fn () { 0 };
            resolve_me@<>()
        };
        x
    }
    "#), @r###"
      6 |             resolve_me@<>()
                      ~~~~~~~~~~
    `resolve_me` is not a generic function and must be called as `resolve_me(...)`.
    "###);
}
