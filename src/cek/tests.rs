#![cfg(test)]
use std::sync::Arc;

use rstest::rstest;

use crate::build;
use crate::build::Compiler as _;
use crate::cek;
use crate::syntax;

fn with_cek_result<R, F>(main: &str, input: &str, f: F) -> R
where
    F: FnOnce(&cek::MachineResult) -> R,
{
    let db = &mut build::CompilerDB::new();
    let uri = build::Uri::new("test.doh");
    db.set_input(uri, Arc::new(input.to_string()));
    db.with_diagnostics(uri, |diagnostics| {
        let diagnostics = diagnostics.cloned().collect::<Vec<_>>();
        assert!(diagnostics.is_empty(), "parser/checker failed: {diagnostics:?}");
    });

    let module = db.anf_module(uri).expect("module could not be compiled");
    let machine = cek::Machine::new(&module, syntax::ExprVar::new(main));
    let result = machine.run();
    f(&result)
}

fn cek_value(main: &str, input: &str) -> String {
    with_cek_result(main, input, |result| result.value().to_string())
}

#[test]
fn bench() {
    let input = std::fs::read_to_string(std::path::Path::new("examples/bench.doh")).unwrap();
    with_cek_result("main", &input, |result| {
        assert_eq!(format!("{}", result.value()), "500500");
        assert_eq!(result.stack_capacity(), 8192);
    });
}

#[test]
fn output_int() {
    insta::assert_snapshot!(cek_value("f", r#"
    fn f() -> Int { 42 }
    "#), @"42");
}

#[test]
fn output_false() {
    insta::assert_snapshot!(cek_value("f", r#"
    fn f() -> Bool { false }
    "#), @"false");
}

#[test]
fn output_true() {
    insta::assert_snapshot!(cek_value("f", r#"
    fn f() -> Bool { true }
    "#), @"true");
}

#[test]
fn output_record() {
    insta::assert_snapshot!(cek_value("f", r#"
    fn f() -> {a: Int, b: Int} { {a = 5, b = 17} }
    "#), @"{_0 = 5, _1 = 17}");
}

#[test]
fn output_record_unordered() {
    insta::assert_snapshot!(cek_value("f", r#"
    fn f() -> {b: Int, a: Int} { {b = 5, a = 17} }
    "#), @"{_0 = 17, _1 = 5}");
}

#[test]
fn output_variant_without_payload() {
    insta::assert_snapshot!(cek_value("f", r#"
    fn f() -> [A | B(Int)] { A }
    "#), @"#0");
}

#[test]
fn output_variant_with_payload() {
    insta::assert_snapshot!(cek_value("f", r#"
    fn f() -> [A | B(Int)] { B(23) }
    "#), @"#1(23)");
}

#[test]
fn output_variant_unorderd_without_payload() {
    insta::assert_snapshot!(cek_value("f", r#"
    fn f() -> [B | A(Int)] { B }
    "#), @"#1");
}

#[test]
fn output_variant_unordered_with_payload() {
    insta::assert_snapshot!(cek_value("f", r#"
    fn f() -> [B | A(Int)] { A(23) }
    "#), @"#0(23)");
}

#[test]
fn output_closure() {
    insta::assert_snapshot!(cek_value("f", r#"
    fn f() -> (Int) -> Int {
        let x = 1;
        fn (y) { x + y }
    }
    "#), @"[x = 1; fn (y) { ... }]");
}

#[rstest]
#[case("{fst: Int, snd: Int}", "{fst = 5, snd = 17}", 5, 17)]
#[case("{fst: Int, snd: Int}", "{snd = 13, fst = 7}", 7, 13)]
#[case("{snd: Int, fst: Int}", "{fst = 17, snd = 5}", 17, 5)]
#[case("{snd: Int, fst: Int}", "{snd = 7, fst = 13}", 13, 7)]
fn record_proj(
    #[case] typ: &str,
    #[case] rec: &str,
    #[case] expected_fst: i64,
    #[case] expected_snd: i64,
) {
    let program = format!(
        r#"
        fn get_fst(rec: {typ}) -> Int {{ rec.fst }}
        fn get_snd(rec: {typ}) -> Int {{ rec.snd }}
        fn test_fst() -> Int {{ get_fst({rec}) }}
        fn test_snd() -> Int {{ get_snd({rec}) }}
        "#,
    );
    assert_eq!(cek_value("test_fst", &program).to_string(), expected_fst.to_string());
    assert_eq!(cek_value("test_snd", &program).to_string(), expected_snd.to_string());
}

#[rstest]
#[case("[Left(Int) | Right(Int)]", "[Left(Int) | Right(Int)]", 5, 17)]
#[case("[Left(Int) | Right(Int)]", "[Right(Int) | Left(Int)]", 7, 13)]
#[case("[Right(Int) | Left(Int)]", "[Left(Int) | Right(Int)]", 17, 5)]
#[case("[Right(Int) | Left(Int)]", "[Right(Int) | Left(Int)]", 13, 7)]
fn variant_match(
    #[case] typ1: &str,
    #[case] typ2: &str,
    #[case] expected_left: i64,
    #[case] expected_right: i64,
) {
    let program = format!(
        r#"
        fn get1(var: {typ1}) -> Int {{
            match var {{
                Left(x) => {expected_left},
                Right(y) => {expected_right},
            }}
        }}
        fn get2(var: {typ2}) -> Int {{ get1(var) }}
        fn test_left() -> Int {{ get2(Left(0)) }}
        fn test_right() -> Int {{ get2(Right(0)) }}
        "#
    );
    assert_eq!(cek_value("test_left", &program).to_string(), expected_left.to_string());
    assert_eq!(cek_value("test_right", &program).to_string(), expected_right.to_string());
}

#[test]
fn tail_call_optimization() {
    let input = r#"
    fn sum_up_to(s: Int, n: Int) -> Int {
        let b = n > 0;
        if b {
            sum_up_to(s + n, n - 1)
        } else {
            s
        }
    }

    fn f() -> Int {
        sum_up_to(0, 1000)
    }
    "#;
    with_cek_result("f", input, |result| {
        assert_eq!(&format!("{}", result.value()), "500500");
        assert_eq!(result.stack_capacity(), 8);
    });
}
