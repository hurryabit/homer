xx

// Original

type T<X> = [ A(X) | B(S<X>) | C(T<X>) ]

type S<X> = [ D(X) | E(S<X>) | F(T<X>) ]

// With base functors

type T_F<X, TX, SX> = [ A(X) | B(SX) | C(TX) ]

type S_F<X, TX, SX> = [ D(X) | E(SX) | F(TX) ]

type T<X> = T_F<X, T<X>, S<X>>

type S<X> = S_F<X, T<X>, S<X>>
