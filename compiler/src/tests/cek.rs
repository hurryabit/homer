use crate::*;
use std::sync::Arc;

fn cek_output(main: &str, input: &str) -> String {
    let db = &mut build::CompilerDB::new();
    let uri = build::Uri::new("test.doh");
    db.set_input(uri, Arc::new(input.to_string()));

    assert_eq!(0, db.with_diagnostics(uri, |diagnostics| diagnostics.count()));
    let module = db.anf_module(uri).expect("module could not be compiled");
    let machine = cek::Machine::new(&module, syntax::ExprVar::new(main));
    let (addr, mem) = machine.run();
    format!("{}", mem.value_at(addr))
}

#[test]
fn sum_1_to_1000() {
    let db = &mut build::CompilerDB::new();
    let uri = build::Uri::new("test.doh");
    let input = r#"
    type Option<A> = [ None | Some(A) ]

    type List<A> = [ Nil | Cons({ head: A, tail: List<A> }) ]

    fn fold_left<A, B>(lst: List<A>, init: B, f: (B, A) -> B) -> B {
        match lst {
            Nil => init,
            Cons(lst) => fold_left@<A, B>(lst.tail, f(init, lst.head), f),
        }
    }

    type Pair<A, B> = {fst: A, snd: B}

    fn unfold_left<A, B>(f: (A) -> Option<Pair<A, B>>, init: A) -> List<B> {
        match f(init) {
            None => Nil,
            Some(pair) => Cons({head = pair.snd, tail = unfold_left@<A, B>(f, pair.fst)}),
        }
    }

    fn enumerate(n: Int) -> List<Int> {
        let f: (Int) -> Option<Pair<Int, Int>> = fn (k) {
            if (k >= 0) {
                Some({fst = k-1, snd = k})
            } else {
                None
            }
        };
        unfold_left@<Int, Int>(f, n)
    }

    fn main() -> Int {
        let xs = enumerate(1000);
        fold_left@<Int, Int>(xs, 0, fn (x, y) { x + y })
    }
    "#;
    db.set_input(uri, Arc::new(input.to_string()));

    let num_diagnostics = db.with_diagnostics(uri, |diagnostics| diagnostics.count());
    assert_eq!(num_diagnostics, 0);
    let module = db.anf_module(uri).expect("module could not be compiled");
    let machine = cek::Machine::new(&module, syntax::ExprVar::new("main"));
    let (addr, mem) = machine.run();
    let header = mem[addr].into_header();
    assert_eq!(header.tag, cek::mem::Tag::Int);
    assert_eq!(mem[addr + 1].into_int(), 500500);
}

#[test]
fn output_int() {
    insta::assert_snapshot!(cek_output("f", r#"
    fn f() -> Int { 42 }
    "#), @"42");
}

#[test]
fn output_false() {
    insta::assert_snapshot!(cek_output("f", r#"
    fn f() -> Bool { false }
    "#), @"false");
}

#[test]
fn output_true() {
    insta::assert_snapshot!(cek_output("f", r#"
    fn f() -> Bool { true }
    "#), @"true");
}

#[test]
fn output_record() {
    insta::assert_snapshot!(cek_output("f", r#"
    fn f() -> {a: Int, b: Int} { {a = 5, b = 17} }
    "#), @"{_0 = 5, _1 = 17}");
}

#[test]
fn output_variant_without_payload() {
    insta::assert_snapshot!(cek_output("f", r#"
    fn f() -> [A | B(Int)] { A }
    "#), @"#0");
}

#[test]
fn output_variant_with_payload() {
    insta::assert_snapshot!(cek_output("f", r#"
    fn f() -> [A | B(Int)] { B(23) }
    "#), @"#1(23)");
}

#[test]
fn output_closure() {
    insta::assert_snapshot!(cek_output("f", r#"
    fn f() -> (Int) -> Int {
        let x = 1;
        fn (y) { x + y }
    }
    "#), @"[x = 1; fn (y) { ... }]");
}
