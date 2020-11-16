// Test for shadowing variables.
use super::*;

#[test]
fn shadow_let_infer_infer() {
    check_success(r#"
    fn f(shadow_me: Bool) -> Int {
        let r = {
            let shadow_me = 0;
            shadow_me
        };
        r
    }
    "#);
}

#[test]
fn shadow_let_infer_check() {
    check_success(r#"
    fn f(shadow_me: Bool) -> Int {
        let shadow_me = 0;
        shadow_me
    }
    "#);
}

#[test]
fn shadow_let_check_infer() {
    check_success(r#"
    fn f(shadow_me: Bool) -> Int {
        let r = {
            let shadow_me: Int = 0;
            shadow_me
        };
        r
    }
    "#);
}

#[test]
fn shadow_let_check_check() {
    check_success(r#"
    fn f(shadow_me: Bool) -> Int {
        let shadow_me: Int = 0;
        shadow_me
    }
    "#);
}

#[test]
fn shadow_lam_infer() {
    check_success(r#"
    fn f(shadow_me: Bool) -> Int {
        let f = fn (shadow_me: Int) { shadow_me };
        f(0)
    }
    "#);
}

#[test]
fn shadow_lam_check_unannotated() {
    check_success(r#"
    fn f(shadow_me: Bool) -> Int {
        let f: (Int) -> Int = fn (shadow_me) { shadow_me };
        f(0)
    }
    "#);
}

#[test]
fn shadow_lam_check_annotated() {
    check_success(r#"
    fn f(shadow_me: Bool) -> Int {
        let f: (Int) -> Int = fn (shadow_me: Int) { shadow_me };
        f(0)
    }
    "#);
}

#[test]
fn shadow_match() {
    check_success(r#"
    fn f(shadow_me: Bool) -> Int {
        let f: [F(Int)] = F(0);
        match f {
            F(shadow_me) => shadow_me,
        }
    }
    "#);
}
