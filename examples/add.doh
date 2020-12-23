
type Option<A> = [ None | Some(A) ]

fn add(a: Int, b: Int) -> Int {
    a + b
}

fn three(a: Option<Int>, b: Int, c: Option<Int>) -> Int {
    match c {
        None => 0,
        Some(x) => x,
    }
}

fn double(a: Int) -> Int {
    a * 2
}

fn main() -> Int {
    let a = 10;
    let c = 77;
    let d = three(None,5,Some(9));
    let foo: (Int) -> Option<Int> = fn(b: Int) { Some(add(b + d, c - a - b)) };
    match foo(23) {
        None => d,
        Some(x) => if (x > 10) { x } else { x + 1 },
    }
}
