
type Option<A> = [ None | Some(A) ]

//extern fn foo(a: Int) -> Int
//extern fn set_pixel(x: Int, y: Int, color: Int) -> Int

fn add(a: Int, b: Int) -> Int {
    a + b
}

fn three(a: Option<Int>, b: Int, c: Option<Int>) -> Int {
    match c {
        None => 0,
        Some(x) => x,
    }
}

fn sub(a: Int, b: Int) -> Int {
    if a > 0 {
        sub(a - b, b)        
    } else {
        b
    }
}

    /*
    let a = 10000;
    let b = 20;
    let c = 30;
    foo(b)
    */

    /*
    let c = 77;
    let d = three(None,5,Some(9));
    let foo: (Int) -> Option<Int> = fn(b: Int) { Some(add(b + d, c - a - b)) };
    match foo(23) {
        None => d,
        Some(x) => if (x > 10) { x } else { x + 1 },
    }
    */

/*
fn draw(x: Int, y: Int, color: Int, iter: Int) -> Int {
    let z = set_pixel(x, y, color);
    if y >= 1000 { 
        0
    } else { 
        if x >= 1000 {
            draw(0, y + 10, 1, iter)
        } else {
            draw(x + 10, y, (127 * (x+1) * (y+1) * iter) / ((y/3)+1), iter)
        }

    }
}
fn entry(iter: Int) -> Int {
    draw(0, 0, 0, iter)
}
*/

fn tail(a: Int, b: Int) -> Int {
    a + b
}

fn main() -> Int {
    let a = 1;
    let b = 2;
    let c = 77;
    let d = three(None,5,Some(9));
    let foo: (Int) -> Option<Int> = fn(b: Int) { Some(add(b + d, c - a - b)) };
    match foo(23) {
        None => d,
        Some(x) => tail(if (x > 10) { x } else { x + 1 }, 2),
    }
    //a + b
}


