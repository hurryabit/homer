
type Option<A> = [ None | Some(A) ]

fn add(a: Int, b: Int) -> Int {
    a + b
}

fn main() -> Int {
    let a = 10;
    let foo: (Int) -> Option<Int> = fn(b: Int) { Some(add(a, b)) };
    match foo(20) {
        None => 1,
        Some(x) => if (x > 10) { x + 2 } else { x + 1 },
    }
}
