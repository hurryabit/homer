fn f(x: Bool, y: Int, z: Int) -> Int {
    if x {
        y * z
    } else {
        y + z
    }
}

fn g() -> Int {
    let x = 1;
    let f = fn(y: Int) { x + y };
    f(2)
}

fn h() -> Int {
    let scrut: [None | Some(Int)] = Some(5);
    match scrut {
        None => 0,
        Some(x) => x,
    }
}
