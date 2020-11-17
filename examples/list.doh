type Option<A> = [ None | Some(A) ]

fn map_option<A, B>(opt_x: Option<A>, f: (A) -> B) -> Option<B> {
    match opt_x {
        None => None,
        Some(x) => Some(f(x)),
    }
}

fn and_then_option<A, B>(opt_x: Option<A>, f: (A) -> Option<B>) -> Option<B> {
    match opt_x {
        None => None,
        Some(x) => f(x),
    }
}

fn is_none<A>(opt: Option<A>) -> Bool {
    match opt {
        None => true,
        Some(x) => false,
    }
}

fn is_some<A>(opt: Option<A>) -> Bool {
    match opt {
        None => false,
        Some(x) => true,
    }
}

type List<A> = [ Nil | Cons({ head: A, tail: List<A> }) ]

fn map_list<A, B>(lst: List<A>, f: (A) -> B) -> List<B> {
    match lst {
        Nil => Nil,
        Cons(lst) => Cons({ head = f(lst.head), tail = map_list@<A, B>(lst.tail, f) }),
    }
}

fn head<A>(lst: List<A>) -> Option<A> {
    match lst {
        Nil => None,
        Cons(lst) => Some(lst.head),
    }
}

fn tail<A>(lst: List<A>) -> Option<List<A>> {
    match lst {
        Nil => None,
        Cons(lst) => Some(lst.tail),
    }
}

fn fold_left<A, B>(lst: List<A>, init: B, f: (B, A) -> B) -> B {
    match lst {
        Nil => init,
        Cons(lst) => fold_left@<A, B>(lst.tail, f(init, lst.head), f),
    }
}

fn fold_right<A, B>(lst: List<A>, f: (A, B) -> B, init: B) -> B {
    match lst {
        Nil => init,
        Cons(lst) => fold_right@<A, B>(lst.tail, f, f(lst.head, init)),
    }
}

fn append<A>(xs: List<A>, ys: List<A>) -> List<A> {
    fold_right@<A, List<A>>(xs, fn (z, zs) { Cons({head = z, tail = zs}) }, ys)
}

fn concat<A>(xss: List<List<A>>) -> List<A> {
    fold_right@<List<A>, List<A>>(xss, fn (ys, zs) { append@<A>(ys, zs) }, Nil)
}
