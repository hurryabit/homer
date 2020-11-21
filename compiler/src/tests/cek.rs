use crate::*;
use std::sync::Arc;

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
    let value = machine.run();
    assert!(matches!(&*value, cek::Value::Int(500500)));
}
