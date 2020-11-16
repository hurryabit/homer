# Homer

This is my attempt at a _very boring_ functional programming language. It is boring on purpose. I am trying to explore the boundaries on how simple a language can get before it gets unbearable. In order to stay above my personal bearableness threshold, _Homer_ is statically typed, although structural, has sum types and pattern matching, and allows for parametric polymorphism. These are the fanciest features the language offers.

![Homer - Boring note](https://i.pinimg.com/736x/fa/e1/fa/fae1fa90b9d47809257a9d9b7ad5bbe8.jpg)

Although you might be tempted to think _Homer_ is [lazy](https://en.wikipedia.org/wiki/Lazy_evaluation), that would be far too complicated. Instead, it is as [eager](https://en.wikipedia.org/wiki/Eager_evaluation) as if there were a donut at stake. There are no Haskell-like type classes, no OCaml-like first-class modules and no Scala-like implicits. There are no higher-kinded types, no subtyping, no existential types and no GADTs. Monads are a no-no as well. Polymorphism and recursion are only allowed at the top level. There is no [Hindley-Milner type inference](https://en.wikipedia.org/wiki/Hindley–Milner_type_system), only a [bidirectional type checker](http://davidchristiansen.dk/tutorials/bidirectional.pdf). The syntax is not indentation sensitive but rather uses good old curly braces and semicolons. There is no currying; function arguments have to be separated by commas and wrapped in parentheses instead. There is really nothing fancy.

As a side note, there are also no sum-of-product types. There are only sum types and product types, but because the type system is structural, there's no real need for sum-of-product types anyway.


Here's how the definition of a list type, the map function over it and a function doubling all values in a list look like:
```rust
type List<A> = [Nil | Cons({head: A, tail: List<A>})]

fn map<A, B>(xs: List<A>, f: (A) -> B) -> List<B> {
    match xs {
        Nil => Nil,
        Cons(xs) => {
            let y = f(xs.head);
            let ys = map@<A, B>(xs.tail, f);
            Cons({head = y, tail = ys})
        }
    }
}

fn double(xs: List<Int>) -> List<Int> {
    map@<Int, Int>(xs, fn (x) { 2 * x })
}
```

Nothing about this is surprising or exciting but it is hopefully quite easy to understand. That's the beauty of boringness.

There are two things in the example that actually fall below my threshold for being bearable:

1. Calls to polymorphic functions have to specify the type arguments explicitly. This degree of explicitness is too noisy even for my taste. I hope to find a _lightweight_ unification-based approach that allows for inferring these type arguments in most cases and hence frees the programmer from spelling them out.

2. I don't like that I have to match on `Cons(xs)` and then use `xs.head` and `xs.tail`. I would prefer to write `Cons({head = x, tail = xs})`. Such record patterns would give the compiler an opportunity to warn me when I add a new field to a record type and don't (yet) handle the new field in some places; very much like it can warn me that a pattern match is not exhaustive because I forgot to mention a variant constructor. Even though I'm not convinced that arbitrarily nested patterns still qualify as boring, putting at least one _infallible_ record pattern into a constructor pattern seems very reasonable from a "I _want_ my code to break during refactoring" perspective. It also makes the deliberate absence of sum-of-product types easier to deal with when it come to pattern matching. There's really nothing challenging behind implementing this, other than that I might need to generate some fresh names in the compiler, I just haven't gotten around to it yet.

For those who think that the syntax above looks a lot like [Rust](https://www.rust-lang.org): You're right, the syntax is heavily inspired by Rust. Even more so, the whole langauge is implemented in Rust. But nothing from Rust's ownership system has made it over. That would be too fancy for a boring language like _Homer_.

![Homer - Boring play](https://media1.giphy.com/media/3o6Mbjr8YBdQqKJh04/source.gif)
