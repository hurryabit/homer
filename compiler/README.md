# Compiler

This directory contains the compiler for the _Homer_ language. Currently, it only provides a parser and a bidirectional type checker.


## Typing rules

The typing rules and the implementation of the type checker are bidirectional, as described in [Bidirectional Typing Rules: A Tutorial](http://davidchristiansen.dk/tutorials/bidirectional.pdf). A few general notes on top:

1. The type system is structural. The only polymorphism is parametric polymorphism and it is only allowed at the top-level. There is no subtyping. Recursive types are _equi_-recursive.
1. Type declarations and (top-level) function declarations always have each other in scope and can be recursive.
1. The expansion of recursive types can still cause the compiler to loop indefinitely.


### Well-formed and well-typed declarations

```
  n >= 0
  A1, ..., An mutually distinct
  A1, ..., An |- t
-------------------------------------------------- TypeDecl
  type T<A1, ..., An> = t


  m >= 0
  n >= 0
  A1, ..., Am mutually distinct
  x1, ..., xn mutually distinct
  A1, ..., Am |- s1
  ...
  A1, ..., Am |- sn
  A1, ..., Am |- t
  A1, ..., Am, x1: s1, ..., xn: sn |- e <= t
-------------------------------------------------- FuncDecl
  fn f<A1, ..., Am>(x1: s1, ..., xn: sn) -> t { e }
```


### Well-formed types: `E |- t`

```
  A in E
-------------------------------------------------- TypeVar
  E |- A


  n >= 0
  T not in E
  type T<A1, ..., An> = ...
  E |- t1
  ...
  E |- tn
-------------------------------------------------- TypeSynApp
  E |- T<t1, ..., tn>


  .
-------------------------------------------------- TypeInt
  E |- Int


  .
-------------------------------------------------- TypeBool
  E |- Bool


  n >= 0
  E |- s1
  ...
  E |- sn
  E |- t
-------------------------------------------------- TypeFun
  E |- (s1, ..., sn) -> t


  n >= 0
  a1, ..., an mutually distinct (TODO)
  E |- t1
  ...
  E |- tn
-------------------------------------------------- TypeRecord
  E |- {a1: t1, ..., an: tn}


  n >= 1 (TODO)
  c1, ..., cn mutually distinct (TODO)
  p1 = c1  \/  p1 = c1(t1)  /\  E |- t1
  ...
  pn = cn  \/  pn = cn(tn)  /\  E |- tn
-------------------------------------------------- TypeVariant
  E |- [p1 | ... | p1]
```


### Well-typed expressions: `E |- e => t` and `E |- e <= t`

```
  E |- e => t
-------------------------------------------------- CheckInfer
  E |- e <= t


  type T<A1, ..., An> = t
  E |- e => T[s1, ..., sn]
-------------------------------------------------- SynInfer
  E |- e => t[s1/A1, ..., sn/An]


  type T<A1, ..., An> = t
  E |- e <= t[s1/A1, ..., sn/An]
-------------------------------------------------- SynCheck
  E |- e <= T[s1, ..., sn]


  (x: t) in E
-------------------------------------------------- Var
  E |- x => t


  n in {0, 1, -1, 2, -2, ...}
-------------------------------------------------- LitInt
  E |- n => Int


  b in {true, false}
-------------------------------------------------- LitBool
  E |- b => Bool


  n >= 0
  x1, ..., xn mutually distinct
  E |- s1
  ...
  E |- sn
  E, x1: s1, ..., xn: sn |- e => t
-------------------------------------------------- LamInfer
  E |- fn (x1: s1, ..., xn: sn) { e }
    => (s1, ..., sn) -> t


  n >= 0
  x1, ..., xn mutually distinct
  p1 = x1  /\  s1' = s1
    \/  p1 = x1: s1'  /\  E |- s1'  /\  s1' ~ s1
  ...
  pn = xn  /\  sn' = sn
    \/  pn = xn: sn'  /\  E |- sn'  /\  sn' ~ sn
  E, x1: s1, ..., xn: sn |- e <= t
-------------------------------------------------- LamCheck
  E |- fn (p1, ..., pn) { e }
    <= (s1, ..., sn) -> t

  m >= 0
  n >= 0
  f not in E
  fn f<A1, ..., Am>(x1: s1, ..., xn: sn) -> t { ... }
  E |- u1
  ...
  E |- um
-------------------------------------------------- FuncInst
  E |- f@<u1, ..., um>
    => ((s1, ..., sn) -> t)[u1/A1, ..., um/Am]


  E |- f => (s1, ..., sn) -> t
  E |- e1 <= s1
  ...
  E |- en <= sn
-------------------------------------------------- App
  E |- f(e1, ..., en) => t


  § in {+, -, *, /}
  E |- e1 <= Int
  E |- e2 <= Int
-------------------------------------------------- BinOpArith
  E |- e1 § e2 => Int


  § in {==, !=, <, <=, =>, >}
  E |- e1 => t
  E |- e2 <= t
-------------------------------------------------- BinOpCmp
  E |- e1 § e2 => Bool


  E |- e1 => s
  E, x: s |- e2 => t
-------------------------------------------------- LetInferInfer
  E |- let x = e1 in e2 => t


  E |- s
  E |- e1 <= s
  E, x: s |- e2 => t
-------------------------------------------------- LetCheckInfer
  E |- let x: s = e1 in e2 => t


  E |- e1 => s
  E, x: s |- e2 <= t
-------------------------------------------------- LetInferCheck
  E |- let x = e1 in e2 <= t


  E |- s
  E |- e1 <= s
  E, x: s |- e2 <= t
-------------------------------------------------- LetCheckCheck
  E |- let x: s = e1 in e2 <= t


  E |- e1 <= Bool
  E |- e2 => t
  E |- e3 <= t
-------------------------------------------------- IfInfer
  E |- if e1 { e2 } else { e3 } => t


  E |- e1 <= Bool
  E |- e2 <= t
  E |- e3 <= t
-------------------------------------------------- IfCheck
  E |- if e1 { e2 } else { e3 } <= t


  n >= 0
  a1, ..., an mutually distinct
  E |- e1 => t1
  ...
  E |- en => tn
-------------------------------------------------- RecordInfer
  E |- {a1 = e1, ..., an = en}
    => {a1: t1, ..., an: tn}


  n >= 0
  E |- e1 <= t1
  ...
  E |- en <= tn
-------------------------------------------------- RecordCheck (TODO)
  E |- {a1 = e1, ..., an = en}
    <= {a1: t1, ..., an: tn}


  E |- e => s
  s = {..., a: t, ...}
-------------------------------------------------- Proj
  E |- e.a => t


  .
-------------------------------------------------- VariantWithoutPayload
  E |- c <= [... | c | ...]


  E |- e <= t
-------------------------------------------------- VariantWithPayload
  E |- c(e) <= [... | c(t) | ...]


  E |- e => s
  s = [...]
  E |- branch s { p1 => e1 } => t
  E |- branch s { p2 => e2 } <= t
  ...
  E |- branch s { pn => en } <= t
-------------------------------------------------- MatchInfer
  E |- match e {p1 => e1, ..., pn -> en} => t


  E |- e => s
  s = [...]
  E |- branch s { p1 => e1 } <= t
  ...
  E |- branch s { pn => en } <= t
-------------------------------------------------- MatchCheck
  E |- match e {p1 -> e1, ..., pn -> en} <= t
```


### Well-typed branches: `E |- branch s {p => e} =>/<= t`
```
  E |- e => t
-------------------------------------------------- BranchWithoutPayloadInfer
  E |- branch [... | c | ...] { c => e }
    => t


  E |- e <= t
-------------------------------------------------- BranchWithoutPayloadCheck
  E |- branch [... | c | ...] { c => e }
    <= t


  E, x: s |- e => t
-------------------------------------------------- BranchWithPayloadInfer
  E |- branch [... | c(s) | ...] { c(x) => e }
    => t


  E, x: s |- e <= t
-------------------------------------------------- BranchWithPayloadCheck
  E |- branch [... | c(s) | ...] { c(x) => e }
    <= t
```
