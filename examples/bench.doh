type Option<A> = [ None | Some(A) ]

type List<A> = [ Nil | Cons({ head: A, tail: List<A> }) ]

fn fold_left<A, B>(lst: List<A>, init: B, f: (B, A) -> B) -> B {
    match lst {
        Nil => init,
        Cons(lst) => fold_left(lst.tail, f(init, lst.head), f),
    }
}

type Pair<A, B> = {fst: A, snd: B}

fn unfold_left<A, B>(f: (A) -> Option<Pair<A, B>>, init: A) -> List<B> {
    match f(init) {
        None => Nil,
        Some(pair) => Cons({head = pair.snd, tail = unfold_left(f, pair.fst)}),
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
    unfold_left(f, n)
}

fn main() -> Int {
    let xs = enumerate(1000);
    fold_left(xs, 0, fn (x, y) { x + y })
}
