type Unit = {}

type Option<T> = [None | Some(T)]

extern type Array<T>

extern fn empty<T>() -> Array<T>

extern fn is_empty<T>(self: Array<T>) -> Bool

extern fn len<T>(self: Array<T>) -> Int

extern fn get<T>(self: Array<T>, index: Int) -> T

extern fn set<T>(self: Array<T>, index: Int, value: T) -> Unit

extern fn push<T>(self: Array<T>, value: T) -> Unit

extern fn pop<T>(self: Array<T>) -> Option<T>

fn foldl_from<T, U>(self: Array<T>, start: Int, init: U, f: (U, T) -> U) -> U {
    if start < len(self) {
        foldl_from(self, start+1, f(init, get(self, start)), f)
    } else {
        init
    }
}

fn foldl<T, U>(self: Array<T>, init: U, f: (U, T) -> U) -> U {
    foldl_from(self, 0, init, f)
}

fn sum(self: Array<Int>) -> Int {
    foldl(self, 0, fn (x, y) { x + y })
}
