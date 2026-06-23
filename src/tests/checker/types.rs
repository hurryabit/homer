use super::*;

#[test]
fn rule_type_var() {
    check_success(
        r#"
        type T<A> = A
        "#,
    );
}

#[test]
fn rule_type_var_unknown() {
    insta::assert_snapshot!(check_error(r#"
    type T = A
    "#), @r###"
      2 |     type T = A
                       ~
    Undeclared type variable `A`.
    "###);
}

#[test]
fn rule_type_synapp_0() {
    check_success(
        r#"
        type A = Int
        type T = A
        "#,
    );
}

#[test]
fn rule_type_synapp_1() {
    check_success(
        r#"
        type F<A> = A
        type T = F<Int>
        "#,
    );
}

#[test]
fn rule_type_synapp_2() {
    check_success(
        r#"
        type F<A, B> = {a: A, b: B}
        type T<A> = F<A, Int>
        "#,
    );
}

#[test]
fn rule_type_synapp_var() {
    insta::assert_snapshot!(check_error(r#"
    type T<F> = F<Int>
    "#), @r###"
      2 |     type T<F> = F<Int>
                          ~~~~~~
    Type `F` is not a generic type but is applied to 1 type argument(s).
    "###);
}

#[test]
fn rule_type_synapp_args_on_mono() {
    insta::assert_snapshot!(check_error(r#"
    type F = Int
    type T = F<Bool>
    "#), @r###"
      3 |     type T = F<Bool>
                       ~~~~~~~
    Type `F` is not a generic type but is applied to 1 type argument(s).
    "###);
}

#[test]
fn rule_type_synapp_no_args_on_poly() {
    insta::assert_snapshot!(check_error(r#"
    type F<A> = A
    type T = F
    "#), @r###"
      3 |     type T = F
                       ~
    Expected a type but found the generic type `F`.
    "###);
}

#[test]
fn rule_type_synapp_too_many_args() {
    insta::assert_snapshot!(check_error(r#"
    type F<A> = A
    type T = F<Int, Bool>
    "#), @r###"
      3 |     type T = F<Int, Bool>
                       ~~~~~~~~~~~~
    Generic type `F` expects 1 type argument(s) but is applied to 2 type argument(s).
    "###);
}

#[test]
fn rule_type_synapp_too_few_args() {
    insta::assert_snapshot!(check_error(r#"
    type F<A, B> = B
    type T = F<Int>
    "#), @r###"
      3 |     type T = F<Int>
                       ~~~~~~
    Generic type `F` expects 2 type argument(s) but is applied to 1 type argument(s).
    "###);
}

#[test]
fn rule_type_synapp_illformed_arg_1() {
    insta::assert_snapshot!(check_error(r#"
    type F<A> = A
    type G<B> = B
    type T = F<G>
    "#), @r###"
      4 |     type T = F<G>
                         ~
    Expected a type but found the generic type `G`.
    "###);
}

#[test]
fn rule_type_synapp_illformed_arg_2() {
    insta::assert_snapshot!(check_error(r#"
    type F<A, B> = A
    type T = F<Int, F>
    "#), @r###"
      3 |     type T = F<Int, F>
                              ~
    Expected a type but found the generic type `F`.
    "###);
}

#[test]
fn rule_type_int() {
    check_success(
        r#"
        type T = Int
        "#,
    );
}

#[test]
fn rule_type_bool() {
    check_success(
        r#"
        type T = Bool
        "#,
    );
}

#[test]
fn rule_type_fun_0() {
    check_success(
        r#"
        type T = () -> Bool
        "#,
    );
}

#[test]
fn rule_type_fun_1() {
    check_success(
        r#"
        type T = (Int) -> Int
        "#,
    );
}

#[test]
fn rule_type_fun_2() {
    check_success(
        r#"
        type B = Int
        type T<A> = (A, B) -> A
        "#,
    );
}

#[test]
fn rule_type_fun_illformed_param_1() {
    insta::assert_snapshot!(check_error(r#"
    type F<A> = A
    type T = (F) -> Int
    "#), @r###"
      3 |     type T = (F) -> Int
                        ~
    Expected a type but found the generic type `F`.
    "###);
}

#[test]
fn rule_type_fun_illformed_param_2() {
    insta::assert_snapshot!(check_error(r#"
    type F<A> = A
    type T = (Int, F) -> Bool
    "#), @r###"
      3 |     type T = (Int, F) -> Bool
                             ~
    Expected a type but found the generic type `F`.
    "###);
}

#[test]
fn rule_type_fun_illformed_result() {
    insta::assert_snapshot!(check_error(r#"
    type F<A> = A
    type T = (Int) -> F
    "#), @r###"
      3 |     type T = (Int) -> F
                                ~
    Expected a type but found the generic type `F`.
    "###);
}

#[test]
fn rule_type_record_0() {
    check_success(
        r#"
        type T = {}
        "#,
    );
}

#[test]
fn rule_type_record_1() {
    check_success(
        r#"
        type T<A> = {a: A}
        "#,
    );
}

#[test]
fn rule_type_record_2() {
    check_success(
        r#"
        type T = {y: Int, x: Int}
        "#,
    );
}

#[test]
fn rule_type_record_illformed_1() {
    insta::assert_snapshot!(check_error(r#"
  type F<A> = A
  type T = {f: F}
  "#), @r###"
    3 |   type T = {f: F}
                       ~
  Expected a type but found the generic type `F`.
  "###);
}

#[test]
fn rule_type_record_illformed_2() {
    insta::assert_snapshot!(check_error(r#"
  type F<A> = A
  type T = {x: Int, f: F}
  "#), @r###"
    3 |   type T = {x: Int, f: F}
                               ~
  Expected a type but found the generic type `F`.
  "###);
}

#[test]
fn rule_type_record_repeat_1_2() {
    insta::assert_snapshot!(check_error(r#"
    type T = {x: Int, x: Int}
    "#), @r###"
      2 |     type T = {x: Int, x: Int}
                                ~
    Duplicate record field `x`.
    "###);
}

#[test]
fn rule_type_record_repeat_2_4() {
    insta::assert_snapshot!(check_error(r#"
    type T = {x: Int, y: Int, z: Int, y: Int}
    "#), @r###"
      2 |     type T = {x: Int, y: Int, z: Int, y: Int}
                                                ~
    Duplicate record field `y`.
    "###);
}

#[test]
fn rule_variant_with() {
    check_success(
        r#"
        type T<A> = [A(A)]
        "#,
    );
}

#[test]
fn rule_variant_without() {
    check_success(
        r#"
        type T = [A]
        "#,
    );
}

#[test]
fn rule_variant_with_with() {
    check_success(
        r#"
        type T<A, B> = [A(A) | B(B)]
        "#,
    );
}

#[test]
fn rule_variant_with_without() {
    check_success(
        r#"
        type T = [A(Int) | B]
        "#,
    );
}

#[test]
fn rule_variant_without_with() {
    check_success(
        r#"
        type T = [C | D(Bool)]
        "#,
    );
}

#[test]
fn rule_variant_without_without() {
    check_success(
        r#"
        type T = [C | D]
        "#,
    );
}

#[test]
fn rule_type_variant_illformed_with_1() {
    insta::assert_snapshot!(check_error(r#"
    type F<A> = A
    type T = [A(F)]
    "#), @r###"
      3 |     type T = [A(F)]
                          ~
    Expected a type but found the generic type `F`.
    "###);
}

#[test]
fn rule_type_variant_illformed_with_with_1() {
    insta::assert_snapshot!(check_error(r#"
    type F<A> = A
    type T = [A(F) | B(Int)]
    "#), @r###"
      3 |     type T = [A(F) | B(Int)]
                          ~
    Expected a type but found the generic type `F`.
    "###);
}

#[test]
fn rule_type_variant_illformed_with_without_1() {
    insta::assert_snapshot!(check_error(r#"
    type F<A> = A
    type T = [A(F) | B]
    "#), @r###"
      3 |     type T = [A(F) | B]
                          ~
    Expected a type but found the generic type `F`.
    "###);
}

#[test]
fn rule_type_variant_illformed_without_with_2() {
    insta::assert_snapshot!(check_error(r#"
    type F<A> = A
    type T = [B | A(F)]
    "#), @r###"
      3 |     type T = [B | A(F)]
                              ~
    Expected a type but found the generic type `F`.
    "###);
}

#[test]
fn rule_type_variant_illformed_with_with_2() {
    insta::assert_snapshot!(check_error(r#"
    type F<A> = A
    type T = [A(Bool) | B(F)]
    "#), @r###"
      3 |     type T = [A(Bool) | B(F)]
                                    ~
    Expected a type but found the generic type `F`.
    "###);
}

#[test]
fn rule_type_variant_repeat_1_2() {
    insta::assert_snapshot!(check_error(r#"
    type T = [A | A]
    "#), @r###"
      2 |     type T = [A | A]
                            ~
    Duplicate variant constructor `A`.
    "###);
}

#[test]
fn rule_type_variant_repeat_2_4() {
    insta::assert_snapshot!(check_error(r#"
    type T = [A(Int) | B(Int) | C(Int) | B(Int)]
    "#), @r###"
      2 |     type T = [A(Int) | B(Int) | C(Int) | B(Int)]
                                                   ~
    Duplicate variant constructor `B`.
    "###);
}

#[test]
fn self_reference_not_contractive() {
    insta::assert_snapshot!(check_error(r#"
    type T = T
    "#), @r#"
      2 |     type T = T
                   ~
    Declaration of type `T` is not contractive.
    "#);
}

#[test]
fn two_cycle_not_contractive() {
    insta::assert_snapshot!(check_error(r#"
    type S = T
    type T = S
    "#), @r#"
      2 |     type S = T
                   ~
    Declaration of type `S` is not contractive.
    "#);
}

#[test]
fn three_cycle_not_contractive() {
    insta::assert_snapshot!(check_error(r#"
    type S = T
    type T = U
    type U = S
    "#), @r#"
      2 |     type S = T
                   ~
    Declaration of type `S` is not contractive.
    "#);
}

#[test]
fn lolly_not_contractive() {
    // We deliberately report `T` instead of `S` since fixing `T` would fix `S` as well.
    insta::assert_snapshot!(check_error(r#"
    type S = T
    type T = U
    type U = T
    "#), @r#"
      3 |     type T = U
                   ~
    Declaration of type `T` is not contractive.
    "#);
}

#[test]
fn generic_self_reference_not_contractive() {
    insta::assert_snapshot!(check_error(r#"
    type F<T> = F<T>
    "#), @r#"
      2 |     type F<T> = F<T>
                   ~
    Declaration of type `F` is not contractive.
    "#);
}

#[test]
fn generic_cycle_not_contractive() {
    insta::assert_snapshot!(check_error(r#"
    type F<S> = G<S, S>
    type G<T, U> = F<{x: T, y: U}>
    "#), @r#"
      2 |     type F<S> = G<S, S>
                   ~
    Declaration of type `F` is not contractive.
    "#);
}

#[test]
fn list_contractive() {
    check_success(
        r#"
        type List = [Nil | Cons({hd: Int, tl: List})]
        "#,
    );
}

#[test]
fn stream_contractive() {
    check_success(
        r#"
        type Stream<T> = () -> {value: T, next: Stream<T>}
        "#,
    );
}
