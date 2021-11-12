type List<A> = [Nil | Cons({head: A, tail: List<A>})]

fn divides(k: Int, n: Int) -> Bool {
    (n / k) * k == n
}

fn is_prime_aux(n: Int, k: Int) -> Bool {
    if k * k > n {
        true
    } else if divides(k, n) {
        false
    } else {
        is_prime_aux(n, k + 2)
    }
}

fn is_prime(n: Int) -> Bool {
    if n < 2 {
        false
    } else if n == 2 {
        true
    } else if divides(2, n) {
        false
    } else {
        is_prime_aux(n, 3)
    }
}

fn up_to_aux(k: Int, ns: List<Int>) -> List<Int> {
    if k <= 0 {
        ns
    } else {
        up_to_aux(k-1, Cons({head = k, tail = ns}))
    }
}

fn up_to(n: Int) -> List<Int> {
    up_to_aux(n, Nil)
}

fn select<A>(xs: List<A>, p: (A) -> Bool) -> List<A> {
    match xs {
        Nil => Nil,
        Cons(xs) => if p(xs.head) {
            Cons({head = xs.head, tail = select(xs.tail, p)})
        } else {
            select(xs.tail, p)
        },
    }
}


fn zip_with<A, B, C>(xs: List<A>, ys: List<B>, f: (A, B) -> C) -> List<C> {
    zip_with(xs, ys, f)
}
