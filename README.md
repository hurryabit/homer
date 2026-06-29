# Homer

This is my attempt at a _very boring_ functional programming language. It is
boring on purpose. I am trying to explore the boundaries of how simple a
language can get before it gets unbearable. In order to stay above my personal
threshold for bearability, _Homer_ is statically typed, although structural, has
sum types and pattern matching, and supports parametric polymorphism and
_equi_-recursive types. These are the fanciest features the language offers.
The implementation comes with a compiler to
[WebAssembly](https://en.wikipedia.org/wiki/WebAssembly)
and an interpreter (based on a
[CEK machine](https://en.wikipedia.org/wiki/CEK_Machine)). ([Usage](#usage)
instructions for those can be found toward the bottom of the file.)

![Homer - Boring note](https://i.pinimg.com/736x/fa/e1/fa/fae1fa90b9d47809257a9d9b7ad5bbe8.jpg)

Although one might be tempted to think _Homer_ is
[lazy](https://en.wikipedia.org/wiki/Lazy_evaluation), that would be far too
complicated. Instead, it is as
[eager](https://en.wikipedia.org/wiki/Eager_evaluation) as if there were a donut
at stake. There are no Haskell-like type classes, no OCaml-like first-class
modules, and no Scala-like implicits. There are no higher-kinded types, no
subtyping, no existential types, and no GADTs. Monads are a no-no as well.
Polymorphism and recursion are only allowed at the top level. There is no
[Hindley-Milner type inference](https://en.wikipedia.org/wiki/Hindley–Milner_type_system),
only a
[bidirectional type checker](http://davidchristiansen.dk/tutorials/bidirectional.pdf)
(enriched with some lightweight unification-based inference for the type
arguments of generic functions). The syntax is not indentation sensitive but
rather uses good old curly braces and semicolons. There is no currying; function
arguments have to be separated by commas and wrapped in parentheses instead.
There really is nothing fancy.

As a side note, there are also no sum-of-product types. There are only sum types
and product types, but because the type system is structural, there's no real
need for sum-of-product types anyway.

Here's what the definition of a generic list type, the map function over it, and
a function doubling all values in a list look like:

```rust
type List<A> = [Nil | Cons({head: A, tail: List<A>})];

fn map<A, B>(xs: List<A>, f: (A) -> B) -> List<B> {
    match xs {
        Nil => Nil,
        Cons(xs) => {
            let y = f(xs.head);
            let ys = map(xs.tail, f);
            Cons({head = y, tail = ys})
        }
    }
}

fn double(xs: List<Int>) -> List<Int> {
    map(xs, fn (x) { 2 * x })
}
```

Nothing about this is surprising or exciting but it is hopefully quite easy to
understand. That's the beauty of boringness.

There is one thing in the example that sits right around my threshold for being
bearable: I'm not thrilled that I first have to match on `Cons(xs)` and then
project out `xs.head` and `xs.tail`. Instead, I would prefer to match on
`Cons({head = x, tail = xs})`. Such record patterns would give the compiler an
opportunity to warn me when I add a new field to a record type and don't (yet)
handle the new field in some places; very much like it can warn me that a
pattern match is not exhaustive because I forgot to mention a variant
constructor. Even though I'm not convinced that arbitrarily nested patterns
still qualify as boring, putting at least one _infallible_ record pattern into a
constructor pattern seems very reasonable from a "I _want_ my code to break
during refactoring" perspective. It also makes the deliberate absence of
sum-of-product types easier to deal with when it comes to pattern matching.
There's really nothing challenging behind implementing this, other than that I
might need to generate some fresh names in the compiler, it just hasn't been a
priority yet.

For those who think that the syntax above looks a lot like
[Rust](https://www.rust-lang.org): You're right, the syntax is heavily inspired
by Rust, except for the syntax for closures, which was taken from JavaScript --
and definitely _not_ from Go! Even more so, the whole langauge is implemented in
Rust. But nothing from Rust's ownership system has made it over. That would be
way too fancy for a boring language like _Homer_.

![Homer - Boring play](https://media1.giphy.com/media/3o6Mbjr8YBdQqKJh04/source.gif)

## Usage

To play with _Homer_, you need to
[install a Rust toolchain](https://rust-lang.org/learn/get-started/). Afterward,
you can run a simple example adding up the first 1000 numbers with the following
command:

```sh
cargo run -- interpet examples/bench.doh
```

If you're adventurous and want to use the compiler to WebAssembly, which is
still in a _very_ early stage, you first need to
[install `wasmtime`](https://github.com/bytecodealliance/wasmtime#installation).
Then, you can compile and run the same example via:

```sh
cargo run -- compile examples/bench.doh
wasmtime -W gc,function-references --invoke main examples/bench.wasm
```

If you want to sum to, say, 5000, replace the `wasmtime` command with

```sh
wasmtime -W gc,function-references --invoke sum_to examples/bench.wasm 5000
```

(I start getting stack overflows starting from around 5400 on my setup. This is
due to a tall of tail call optimization, which is on my TODO list.)

There is also a simple language server that can be started via

```sh
cargo run -- server
```

The repo also contains a [configuration file](.helix/languages.toml) which adds
support for _Homer_ to the [helix](https://helix-editor.com/) editor. This can
be used by running

```sh
cargo build
PATH="$PWD/target/debug:$PATH" hx examples/bench.doh
```

(If you have [direnv](https://direnv.net/) installed, you don't need to change
the path but can just run `direnv allow` followed by `hx`.)
