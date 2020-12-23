
type Option<A> = [ None | Some({a: A}) ]

fn add(a: Int, b: Int) -> Int {
    a + b
}

fn main() -> Int {
    let a = 10;
    let foo: (Int) -> Option<Int> = fn(b: Int) { Some({ a = add(a, b) }) };
    match foo(20) {
        None => 1,
        Some(x) => if (x.a > 10) { x.a + 2 } else { x.a + 1 },
    }
}
