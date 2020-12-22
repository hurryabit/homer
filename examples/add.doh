
type Option<A> = [ None | Some(A) ]

fn main() -> Int {
    let a = 10;
    let foo: (Int) -> Option<Int> = fn(b: Int) { Some(a + b) };
    match foo(20) {
        None => 1,
        Some(x) => x,
    }
}
