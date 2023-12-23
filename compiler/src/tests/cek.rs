use crate::*;
use build::Compiler;
use std::sync::Arc;

fn with_cek_result<R, F>(main: &str, input: &str, f: F) -> R
where
    F: FnOnce(&cek::MachineResult) -> R,
{
    let db = &mut build::CompilerDB::new();
    let uri = build::Uri::new("test.doh");
    db.set_input(uri, Arc::new(input.to_string()));

    assert_eq!(0, db.with_diagnostics(uri, |diagnostics| diagnostics.count()));
    let module = db.anf_module(uri).expect("module could not be compiled");
    let machine = cek::Machine::new(&module, syntax::ExprVar::new(main));
    let result = machine.run();
    f(&result)
}

fn cek_value(main: &str, input: &str) -> String {
    with_cek_result(main, input, |result| format!("{}", result.value()))
}

#[test]
fn bench() {
    let input = std::fs::read_to_string(std::path::Path::new("../examples/bench.doh")).unwrap();
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
fn output_closure() {
    insta::assert_snapshot!(cek_value("f", r#"
    fn f() -> (Int) -> Int {
        let x = 1;
        fn (y) { x + y }
    }
    "#), @"[x = 1; fn (y) { ... }]");
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
