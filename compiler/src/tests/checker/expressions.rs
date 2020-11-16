/*
Some notes:

* To check that the of an expression `E` can be inferred and matches `T`, we
  structure the test as
  ```
  fn () -> T {
      let x = E;
      x
  }
  ```
  The unannotated `let`-binding ensure that the type of `E` gets definitely
  inferred rather than (accidentally) checked against `T`.

* If we want to check that type inference fails for an expression, we use a
  variant constructor like `InferMe` to signal that.

* If we want to test that the type of an expression gets checked and fails, we
  use a variant constructor `CheckMe` to signal that.
*/

use super::*;

#[test]
fn rule_check_infer() {
    check_success(r#"
    fn f() -> Int { 0 }
    "#);
}

#[test]
fn rule_check_infer_expected_syn() {
    check_success(r#"
    type A = Int
    fn f() -> A { 0 }
    "#);
}

#[test]
fn rule_check_infer_found_syn() {
    check_success(r#"
    type A = Int
    fn f(x: A) -> Int { x }
    "#);
}

#[test]
fn rule_check_infer_both_same_syn() {
    check_success(r#"
    type A = Int
    fn f(x: A) -> A { x }
    "#);
}

#[test]
fn rule_check_infer_both_same_diverging_syn() {
    check_success(r#"
    type A = A
    fn f(x: A) -> A { x }
    "#);
}

#[test]
fn rule_check_infer_different_syns() {
    check_success(r#"
    type A = Int
    type B = Int
    fn f(x: A) -> B { x }
    "#);
}

#[test]
fn rule_check_infer_expected_double_syn() {
    check_success(r#"
    type A = Int
    type B = A
    fn f(x: Int) -> B { x }
    "#);
}

#[test]
fn rule_check_infer_found_double_syn() {
    check_success(r#"
    type A = Int
    type B = A
    fn f(x: B) -> Int { x }
    "#);
}

#[test]
fn rule_check_infer_mismatch() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Bool { 0 }
    "#), @r###"
      2 |     fn f() -> Bool { 0 }
                               ~
    Expected an expression of type `Bool` but found an expression of type `Int`.
    "###);
}

#[test]
fn rule_var() {
    check_success(r#"
    fn f(x: Int) -> Int {
        let y = x;
        y
    }
    "#);
}

#[test]
fn rule_var_unknown() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        x
    }
    "#), @r###"
      3 |         x
                  ~
    Undeclared variable `x`.
    "###);
}

#[test]
fn rule_var_unknown_as_func() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        g(1)
    }
    "#), @r###"
      3 |         g(1)
                  ~
    Undeclared variable `g`.
    "###);
}

#[test]
fn rule_lit_int_0() {
    check_success(r#"
    fn f() -> Int {
        let x = 0;
        x
    }
    "#);
}

#[test]
fn rule_lit_int_1() {
    check_success(r#"
    fn f() -> Int {
        let x = 1;
        x
    }
    "#);
}

#[test]
fn rule_lit_bool_true() {
    check_success(r#"
    fn f() -> Bool {
        let x = true;
        x
    }
    "#);
}

#[test]
fn rule_lit_bool_false() {
    check_success(r#"
    fn f() -> Bool {
        let x = false;
        x
    }
    "#);
}

#[test]
fn rule_lam_infer_0() {
    check_success(r#"
    fn f() -> () -> Int {
        let f = fn () { 1 };
        f
    }
    "#);
}

#[test]
fn rule_lam_infer_1() {
    check_success(r#"
    fn f() -> (Int) -> Int {
        let f = fn (x: Int) { x };
        f
    }
    "#);
}

#[test]
fn rule_lam_infer_2() {
    check_success(r#"
    fn f<A>() -> (A, A) -> Bool {
        let f = fn (x: A, y: A) { x == y };
        f
    }
    "#);
}

#[test]
fn rule_lam_infer_duplicate_param() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> (Int) -> Bool {
        let f = fn (x: Int, x: Int) { x };
        f
    }
    "#), @r###"
      3 |         let f = fn (x: Int, x: Int) { x };
                                      ~
    Duplicate paramter `x`.
    "###);
}

#[test]
fn rule_lam_infer_unknown_type_ann() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let f = fn (x: Unknown) {
            0
        };
        0
    }
    "#), @r###"
      3 |         let f = fn (x: Unknown) {
                                 ~~~~~~~
    Undeclared type variable `Unknown`.
    "###);
}

#[test]
fn rule_lam_infer_illformed_type_ann() {
    insta::assert_snapshot!(check_error(r#"
    type Illformed<A> = A
    fn f() -> Int {
        let f = fn (x: Int, y: Illformed) {
            0
        };
        0
    }
    "#), @r###"
      4 |         let f = fn (x: Int, y: Illformed) {
                                         ~~~~~~~~~
    Expected a type but found the generic type `Illformed`.
    "###);
}

#[test]
fn rule_lam_infer_not_inferrable() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> (Int) -> Bool {
        let f = fn (x) {
            let y: Int = x;
            y
        };
        f
    }
    "#), @r###"
      3 |         let f = fn (x) {
                              ~
    Cannot infer the type of parameter `x`. A type annoation is needed.
    "###);
}

#[test]
fn rule_lam_check_0() {
    check_success(r#"
    fn f() -> () -> Int {
        fn () { 1 }
    }
    "#);
}

#[test]
fn rule_lam_check_1() {
    check_success(r#"
    fn f() -> (Int) -> Int {
        fn (x) { x }
    }
    "#);
}

#[test]
fn rule_lam_check_2() {
    check_success(r#"
    fn f<A>() -> (A, A) -> A {
        fn (x: A, y) { x }
    }
    "#);
}

#[test]
fn rule_lam_check_syn() {
    check_success(r#"
    type F = (Int) -> Int
    fn f() -> F {
        fn (x) { x }
    }
    "#);
}

#[test]
fn rule_lam_check_no_func() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        fn (x) { 0 }
    }
    "#), @r###"
      3 |         fn (x) { 0 }
                  ~~~~~~~~~~~~
    Expected an expression of type `Int` but found a lambda with 1 parameter.
    "###);
}

#[test]
fn rule_lam_check_duplicate_param_annotated() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> (Int, Int) -> Int {
        fn (x: Int, x: Int) { 0 }
    }
    "#), @r###"
      3 |         fn (x: Int, x: Int) { 0 }
                              ~
    Duplicate paramter `x`.
    "###);
}

#[test]
fn rule_lam_check_duplicate_param_not_annotated() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> (Int, Int) -> Int {
        fn (x, x) { 0 }
    }
    "#), @r###"
      3 |         fn (x, x) { 0 }
                         ~
    Duplicate paramter `x`.
    "###);
}

#[test]
fn rule_lam_check_too_many_params() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> () -> Int {
        fn (x) { 0 }
    }
    "#), @r###"
      3 |         fn (x) { 0 }
                  ~~~~~~~~~~~~
    Expected an expression of type `() -> Int` but found a lambda with 1 parameter.
    "###);
}

#[test]
fn rule_lam_check_too_few_params() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> (Int) -> Int {
        fn () { 0 }
    }
    "#), @r###"
      3 |         fn () { 0 }
                  ~~~~~~~~~~~
    Expected an expression of type `(Int) -> Int` but found a lambda with 0 parameters.
    "###);
}

#[test]
fn rule_lam_check_mismatch_param_1() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> (Int) -> Int {
        fn (x: Bool) { 0 }
    }
    "#), @r###"
      3 |         fn (x: Bool) { 0 }
                         ~~~~
    Expected parameter `x` to have type `Int` but found a type annotation `Bool`.
    "###);
}

#[test]
fn rule_lam_check_mismatch_param_2() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> (Int, Int) -> Int {
        fn (x, y: Bool) { 0 }
    }
    "#), @r###"
      3 |         fn (x, y: Bool) { 0 }
                            ~~~~
    Expected parameter `y` to have type `Int` but found a type annotation `Bool`.
    "###);
}

#[test]
fn rule_lam_check_mismatch_result() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> (Int, Int) -> Bool {
        fn (x, y: Int) { CheckMe }
    }
    "#), @r###"
      3 |         fn (x, y: Int) { CheckMe }
                                   ~~~~~~~
    Expected an expression of type `Bool` but found variant constructor `CheckMe`.
    "###);
}

#[test]
fn rule_lam_check_unknown_type_ann() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> (Int) -> Int {
        fn (x: Unknown) { 0 }
    }
    "#), @r###"
      3 |         fn (x: Unknown) { 0 }
                         ~~~~~~~
    Undeclared type variable `Unknown`.
    "###);
}

#[test]
fn rule_lam_check_illformed_type_ann() {
    insta::assert_snapshot!(check_error(r#"
    type Illformed<A> = A
    fn f() -> (Int, Int) -> Int {
        fn (x, y: Illformed) { 0 }
    }
    "#), @r###"
      4 |         fn (x, y: Illformed) { 0 }
                            ~~~~~~~~~
    Expected a type but found the generic type `Illformed`.
    "###);
}

#[test]
fn rule_func_inst_1() {
    check_success(r#"
    fn g<A>(x: A) -> A { x }
    fn f() -> Int {
        let x = g@<Int>(0);
        x
    }
    "#);
}

#[test]
fn rule_func_inst_2() {
    check_success(r#"
    fn g<A>(x: A, y: A) -> Bool { x == y }
    fn f<B>(b: B) -> Bool {
        let x = g@<B>(b, b);
        x
    }
    "#);
}

#[test]
fn rule_func_inst_unknown_0() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        g@<>()
    }
    "#), @r###"
      3 |         g@<>()
                  ~
    Undeclared variable `g`.
    "###);
}

#[test]
fn rule_func_inst_unknown_1() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        g@<Int>()
    }
    "#), @r###"
      3 |         g@<Int>()
                  ~
    Undeclared variable `g`.
    "###);
}

#[test]
fn rule_func_inst_on_var_0() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let g = fn () { 0 };
        g@<>()
    }
    "#), @r###"
      4 |         g@<>()
                  ~~~~
    `g` is not a generic function and must be called as `g(...)`.
    "###);
}

#[test]
fn rule_func_inst_on_var_1() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let g = fn () { 0 };
        g@<Int>()
    }
    "#), @r###"
      4 |         g@<Int>()
                  ~~~~~~~
    `g` is not a generic function but is applied to 1 type argument.
    "###);
}

#[test]
fn rule_func_inst_on_mono_func_0() {
    insta::assert_snapshot!(check_error(r#"
    fn g() -> Int { 0 }
    fn f() -> Int {
        g@<>()
    }
    "#), @r###"
      4 |         g@<>()
                  ~~~~
    `g` is not a generic function and must be called as `g(...)`.
    "###);
}

#[test]
fn rule_func_inst_on_mono_func_1() {
    insta::assert_snapshot!(check_error(r#"
    fn g() -> Int { 0 }
    fn f() -> Int {
        g@<Int>()
    }
    "#), @r###"
      4 |         g@<Int>()
                  ~~~~~~~
    `g` is not a generic function but is applied to 1 type argument.
    "###);
}

#[test]
fn rule_func_inst_no_types_on_poly_func() {
    insta::assert_snapshot!(check_error(r#"
    fn g<A>(x: A) -> A { x }
    fn f() -> Int {
        g(1)
    }
    "#), @r###"
      4 |         g(1)
                  ~
    Generic function `g` expects 1 type argument but is applied to 0 type arguments.
    "###);
}

#[test]
fn rule_func_inst_zero_types_on_poly_func() {
    insta::assert_snapshot!(check_error(r#"
    fn g<A>(x: A) -> A { x }
    fn f() -> Int {
        g@<>(1)
    }
    "#), @r###"
      4 |         g@<>(1)
                  ~~~~
    Generic function `g` expects 1 type argument but is applied to 0 type arguments.
    "###);
}

#[test]
fn rule_func_inst_too_many_types() {
    insta::assert_snapshot!(check_error(r#"
    fn g<A>(x: A) -> A { x }
    fn f() -> Int {
        g@<Int, Bool>(1)
    }
    "#), @r###"
      4 |         g@<Int, Bool>(1)
                  ~~~~~~~~~~~~~
    Generic function `g` expects 1 type argument but is applied to 2 type arguments.
    "###);
}

#[test]
fn rule_func_inst_too_few_types() {
    insta::assert_snapshot!(check_error(r#"
    fn g<A, B>(x: A, y: B) -> A { x }
    fn f() -> Int {
        g@<Int>(1)
    }
    "#), @r###"
      4 |         g@<Int>(1)
                  ~~~~~~~
    Generic function `g` expects 2 type arguments but is applied to 1 type argument.
    "###);
}

#[test]
fn rule_func_inst_mismatch_param() {
    insta::assert_snapshot!(check_error(r#"
    fn g<A>(x: A) -> Int { 0 }
    fn f() -> Int {
        g@<Int>(true)
    }
    "#), @r###"
      4 |         g@<Int>(true)
                          ~~~~
    Expected an expression of type `Int` but found an expression of type `Bool`.
    "###);
}

#[test]
fn rule_func_inst_mismatch_result() {
    insta::assert_snapshot!(check_error(r#"
    fn g<A>(x: A) -> A { x }
    fn f() -> Int {
        g@<Bool>(CheckMe)
    }
    "#), @r###"
      4 |         g@<Bool>(CheckMe)
                           ~~~~~~~
    Expected an expression of type `Bool` but found variant constructor `CheckMe`.
    "###);
}

#[test]
fn rule_func_inst_unknown_type_arg() {
    insta::assert_snapshot!(check_error(r#"
    fn g<A>(x: A) -> A { x }
    fn f() -> Int {
        g@<Unknown>(0)
    }
    "#), @r###"
      4 |         g@<Unknown>(0)
                     ~~~~~~~
    Undeclared type variable `Unknown`.
    "###);
}

#[test]
fn rule_func_inst_illformed_type_arg() {
    insta::assert_snapshot!(check_error(r#"
    type Illformed<A> = A
    fn g<A>(x: A) -> A { x }
    fn f() -> Int {
        g@<Illformed>(0)
    }
    "#), @r###"
      5 |         g@<Illformed>(0)
                     ~~~~~~~~~
    Expected a type but found the generic type `Illformed`.
    "###);
}

#[test]
fn rule_app_func_0() {
    check_success(r#"
    fn f() -> Int { 0 }
    fn g() -> Int {
        let x = f();
        x
    }
    "#);
}

#[test]
fn rule_app_func_1() {
    check_success(r#"
    fn f(x: Int) -> Int { x }
    fn g() -> Int {
        let x = f(1);
        x
    }
    "#);
}

#[test]
fn rule_app_func_2() {
    check_success(r#"
    fn f(x: Int, y: Int) -> Int { x + y }
    fn g() -> Int {
        let x = f(1, 2);
        x
    }
    "#);
}

#[test]
fn rule_app_func_poly() {
    check_success(r#"
    fn f<A>(x: A) -> A { x }
    fn g() -> Int {
        let x = f@<Int>(1);
        x
    }
    "#);
}

#[test]
fn rule_app_var() {
    check_success(r#"
    fn g() -> Int {
        let f = fn (x: Int) { x };
        let x = f(1);
        x
    }
    "#);
}

#[test]
fn rule_app_syn() {
    check_success(r#"
    type F = (Int) -> Int
    fn g(f: F) -> Int {
        let x = f(1);
        x
    }
    "#);
}

#[test]
fn rule_app_var_no_func() {
    insta::assert_snapshot!(check_error(r#"
    fn f(x: Int) -> Int {
        x()
    }
    "#), @r###"
      3 |         x()
                  ~~~
    `x` cannot be applied to 0 arguments because it has has type `Int`.
    "###);
}

#[test]
fn rule_app_var_too_many_args() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let g = fn () { 0 };
        g(1)
    }
    "#), @r###"
      4 |         g(1)
                  ~~~~
    `g` cannot be applied to 1 argument because it has has type `() -> Int`.
    "###);
}

#[test]
fn rule_app_var_too_few_args() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let g = fn (x: Int) { x };
        g()
    }
    "#), @r###"
      4 |         g()
                  ~~~
    `g` cannot be applied to 0 arguments because it has has type `(Int) -> Int`.
    "###);
}

#[test]
fn rule_app_func_too_many_args() {
    insta::assert_snapshot!(check_error(r#"
    fn g() -> Int { 0 }
    fn f() -> Int {
        g(1)
    }
    "#), @r###"
      4 |         g(1)
                  ~~~~
    `g` cannot be applied to 1 argument because it has has type `() -> Int`.
    "###);
}

#[test]
fn rule_app_func_too_few_args() {
    insta::assert_snapshot!(check_error(r#"
    fn g(x: Int) -> Int { x }
    fn f() -> Int {
        g()
    }
    "#), @r###"
      4 |         g()
                  ~~~
    `g` cannot be applied to 0 arguments because it has has type `(Int) -> Int`.
    "###);
}

#[test]
fn rule_app_func_mismatch_arg1() {
    insta::assert_snapshot!(check_error(r#"
    fn g(x: Int) -> Int { x }
    fn f() -> Int {
        g(CheckMe)
    }
    "#), @r###"
      4 |         g(CheckMe)
                    ~~~~~~~
    Expected an expression of type `Int` but found variant constructor `CheckMe`.
    "###);
}

#[test]
fn rule_app_var_mismatch_arg2() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let g = fn (x: Int, y: Bool) { x };
        g(1, CheckMe)
    }
    "#), @r###"
      4 |         g(1, CheckMe)
                       ~~~~~~~
    Expected an expression of type `Bool` but found variant constructor `CheckMe`.
    "###);
}

#[test]
fn rule_binop_arith() {
    check_success(r#"
    fn f() -> Int {
        let x = 1 + 1;
        x
    }
    "#);
}

#[test]
fn rule_binop_arith_mismatch_lhs() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        CheckMe - 0
    }
    "#), @r###"
      3 |         CheckMe - 0
                  ~~~~~~~
    Expected an expression of type `Int` but found variant constructor `CheckMe`.
    "###);
}

#[test]
fn rule_binop_arith_mismatch_rhs() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        0 * CheckMe
    }
    "#), @r###"
      3 |         0 * CheckMe
                      ~~~~~~~
    Expected an expression of type `Int` but found variant constructor `CheckMe`.
    "###);
}

#[test]
fn rule_binop_cmp() {
    check_success(r#"
    fn f() -> Bool {
        let x = 1 == 1;
        x
    }
    "#);
}

#[test]
fn rule_binop_cmp_lhs_not_inferrable() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Bool {
        InferMe < CheckMe
    }
    "#), @r###"
      3 |         InferMe < CheckMe
                  ~~~~~~~
    Cannot infer the type of the expression. Further type annotations are required.
    "###);
}

#[test]
fn rule_binop_cmp_mismatch() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Bool {
        0 >= CheckMe
    }
    "#), @r###"
      3 |         0 >= CheckMe
                       ~~~~~~~
    Expected an expression of type `Int` but found variant constructor `CheckMe`.
    "###);
}

#[test]
fn rule_let_infer_infer() {
    check_success(r#"
    fn f() -> Int {
        let x = {
            let y = 1;
            y
        };
        x
    }
    "#);
}

#[test]
fn rule_let_infer_infer_bindee_not_inferrable() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let x = {
            let y = InferMe;
            0
        };
        0
    }
    "#), @r###"
      4 |             let y = InferMe;
                              ~~~~~~~
    Cannot infer the type of the expression. Further type annotations are required.
    "###);
}

#[test]
fn rule_let_infer_infer_body_not_inferrable() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let x = {
            let y = 0;
            InferMe
        };
        0
    }
    "#), @r###"
      5 |             InferMe
                      ~~~~~~~
    Cannot infer the type of the expression. Further type annotations are required.
    "###);
}

#[test]
fn rule_let_check_infer() {
    check_success(r#"
    fn f() -> Int {
        let x = {
            let y: [CheckMe] = CheckMe;
            0
        };
        x
    }
    "#);
}

#[test]
fn rule_let_check_infer_poly() {
    check_success(r#"
    fn f<A>(a: A) -> Int {
        let x = {
            let y: A = a;
            0
        };
        x
    }
    "#);
}

#[test]
fn rule_let_check_infer_mismatch_bindee() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let x = {
            let y: Int = CheckMe;
            0
        };
        0
    }
    "#), @r###"
      4 |             let y: Int = CheckMe;
                                   ~~~~~~~
    Expected an expression of type `Int` but found variant constructor `CheckMe`.
    "###);
}

#[test]
fn rule_let_check_infer_body_not_inferrable() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let x = {
            let y: Int = 0;
            InferMe
        };
        0
    }
    "#), @r###"
      5 |             InferMe
                      ~~~~~~~
    Cannot infer the type of the expression. Further type annotations are required.
    "###);
}

#[test]
fn rule_let_check_infer_unknown_type_ann() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let x = {
            let y: Unknown = 0;
            0
        };
        0
    }
    "#), @r###"
      4 |             let y: Unknown = 0;
                             ~~~~~~~
    Undeclared type variable `Unknown`.
    "###);
}

#[test]
fn rule_let_check_infer_illformed_type_ann() {
    insta::assert_snapshot!(check_error(r#"
    type Illformed<A> = A
    fn f() -> Int {
        let x = {
            let y: Illformed = 0;
            0
        };
        0
    }
    "#), @r###"
      5 |             let y: Illformed = 0;
                             ~~~~~~~~~
    Expected a type but found the generic type `Illformed`.
    "###);
}

#[test]
fn rule_let_infer_check() {
    check_success(r#"
    fn f() -> [CheckMe] {
        let x = 0;
        CheckMe
    }
    "#);
}

#[test]
fn rule_let_infer_check_bindee_not_inferrable() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let x = InferMe;
        0
    }
    "#), @r###"
      3 |         let x = InferMe;
                          ~~~~~~~
    Cannot infer the type of the expression. Further type annotations are required.
    "###);
}

#[test]
fn rule_let_infer_check_mismatch_body() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let x = 0;
        CheckMe
    }
    "#), @r###"
      4 |         CheckMe
                  ~~~~~~~
    Expected an expression of type `Int` but found variant constructor `CheckMe`.
    "###);
}

#[test]
fn rule_let_check_check() {
    check_success(r#"
    fn f() -> [CheckMe1] {
        let x: [CheckMe2] = CheckMe2;
        CheckMe1
    }
    "#);
}

#[test]
fn rule_let_check_check_mismatch_bindee() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> [CheckMe1] {
        let x: Int = CheckMe2;
        CheckMe1
    }
    "#), @r###"
      3 |         let x: Int = CheckMe2;
                               ~~~~~~~~
    Expected an expression of type `Int` but found variant constructor `CheckMe2`.
    "###);
}

#[test]
fn rule_let_check_check_mismatch_body() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let x: [CheckMe2] = CheckMe2;
        CheckMe1
    }
    "#), @r###"
      4 |         CheckMe1
                  ~~~~~~~~
    Expected an expression of type `Int` but found variant constructor `CheckMe1`.
    "###);
}

#[test]
fn rule_let_check_check_unknown_type_ann() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let y: Unknown = 0;
        0
    }
    "#), @r###"
      3 |         let y: Unknown = 0;
                         ~~~~~~~
    Undeclared type variable `Unknown`.
    "###);
}

#[test]
fn rule_let_check_check_illformed_type_ann() {
    insta::assert_snapshot!(check_error(r#"
    type Illformed<A> = A
    fn f() -> Int {
        let y: Illformed = 0;
        0
    }
    "#), @r###"
      4 |         let y: Illformed = 0;
                         ~~~~~~~~~
    Expected a type but found the generic type `Illformed`.
    "###);
}

#[test]
fn rule_if_infer() {
    check_success(r#"
    fn check_me() -> [CheckMe] { CheckMe }
    fn f() -> [CheckMe] {
        let x = if true {
            check_me()
        } else {
            CheckMe
        };
        x
    }
    "#);
}

#[test]
fn rule_if_infer_cond_mismatch() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let x = if CheckMe { 1 } else { 2 };
        0
    }
    "#), @r###"
      3 |         let x = if CheckMe { 1 } else { 2 };
                             ~~~~~~~
    Expected an expression of type `Bool` but found variant constructor `CheckMe`.
    "###);
}

#[test]
fn rule_if_infer_then_not_inferrable() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let x = if true { InferMe } else { 1 };
        0
    }
    "#), @r###"
      3 |         let x = if true { InferMe } else { 1 };
                                    ~~~~~~~
    Cannot infer the type of the expression. Further type annotations are required.
    "###);
}

#[test]
fn rule_if_infer_else_mismatch() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let x = if true { 0 } else { CheckMe };
        x
    }
    "#), @r###"
      3 |         let x = if true { 0 } else { CheckMe };
                                               ~~~~~~~
    Expected an expression of type `Int` but found variant constructor `CheckMe`.
    "###);
}

#[test]
fn rule_if_check() {
    check_success(r#"
    fn f() -> [CheckMe] {
        if true {
            CheckMe
        } else {
            CheckMe
        }
    }
    "#);
}

#[test]
fn rule_if_check_cond_mismatch() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        if CheckMe { 1 } else { 2 }
    }
    "#), @r###"
      3 |         if CheckMe { 1 } else { 2 }
                     ~~~~~~~
    Expected an expression of type `Bool` but found variant constructor `CheckMe`.
    "###);
}

#[test]
fn rule_if_check_then_bad() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        if true { CheckMe } else { 1 }
    }
    "#), @r###"
      3 |         if true { CheckMe } else { 1 }
                            ~~~~~~~
    Expected an expression of type `Int` but found variant constructor `CheckMe`.
    "###);
}

#[test]
fn rule_if_check_else_bad() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        if true { 0 } else { CheckMe }
    }
    "#), @r###"
      3 |         if true { 0 } else { CheckMe }
                                       ~~~~~~~
    Expected an expression of type `Int` but found variant constructor `CheckMe`.
    "###);
}

#[test]
fn rule_record_infer() {
    check_success(r#"
    fn f() -> {a: Int, b: Bool} {
        let x = {a = 0, b = true};
        x
    }
    "#);
}

#[test]
fn rule_record_infer_field_1_not_inferrable() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let x = {a = InferMe};
        0
    }
    "#), @r###"
      3 |         let x = {a = InferMe};
                               ~~~~~~~
    Cannot infer the type of the expression. Further type annotations are required.
    "###);
}

#[test]
fn rule_record_infer_field_2_not_inferrable() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let x = {a = 0, b = InferMe};
        0
    }
    "#), @r###"
      3 |         let x = {a = 0, b = InferMe};
                                      ~~~~~~~
    Cannot infer the type of the expression. Further type annotations are required.
    "###);
}

#[test]
fn rule_proj() {
    check_success(r#"
    fn f() -> Int {
        let x = {
            let r = {a = 0, b = true};
            r.a
        };
        x
    }
    "#);
}

#[test]
fn rule_proj_syn() {
    check_success(r#"
    type R = {a: Int, b: Bool}
    fn f(r: R) -> Int {
        let x = {
            r.a
        };
        x
    }
    "#);
}

#[test]
fn rule_proj_record_not_inferrable() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        InferMe.a
    }
    "#), @r###"
      3 |         InferMe.a
                  ~~~~~~~
    Cannot infer the type of the expression. Further type annotations are required.
    "###);
}

#[test]
fn rule_proj_bad_field() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        {a = 1}.b
    }
    "#), @r###"
      3 |         {a = 1}.b
                  ~~~~~~~~~
    Expression of type `{a: Int}` do not contain a field named `b`.
    "###);
}

#[test]
fn rule_variant_without_payload_1() {
    check_success(r#"
    fn f() -> [CheckMe] {
        CheckMe
    }
    "#);
}

#[test]
fn rule_variant_without_payload_2() {
    check_success(r#"
    fn f() -> [IgnoreMe | CheckMe] {
        CheckMe
    }
    "#);
}

#[test]
fn rule_variant_without_payload_syn() {
    check_success(r#"
    type T = [CheckMe]
    fn f() -> T {
        CheckMe
    }
    "#);
}

#[test]
fn rule_variant_without_payload_not_inferrable() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let x = InferMe;
        0
    }
    "#), @r###"
      3 |         let x = InferMe;
                          ~~~~~~~
    Cannot infer the type of the expression. Further type annotations are required.
    "###);
}

#[test]
fn rule_variant_without_payload_no_variant_type() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        CheckMe
    }
    "#), @r###"
      3 |         CheckMe
                  ~~~~~~~
    Expected an expression of type `Int` but found variant constructor `CheckMe`.
    "###);
}

#[test]
fn rule_variant_without_payload_unknown_constructor() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> [NotCheckMe] {
        CheckMe
    }
    "#), @r###"
      3 |         CheckMe
                  ~~~~~~~
    `CheckMe` is not a possible constructor for variant type `[NotCheckMe]`.
    "###);
}

#[test]
fn rule_variant_without_payload_constructor_with_payload() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> [CheckMe(Int)] {
        CheckMe
    }
    "#), @r###"
      3 |         CheckMe
                  ~~~~~~~
    Constructor `CheckMe` of variant type `[CheckMe(Int)]` needs a payload.
    "###);
}

#[test]
fn rule_variant_with_payload_1() {
    check_success(r#"
    fn f() -> [CheckMe((Int) -> Int)] {
        CheckMe(fn (x) { x })
    }
    "#);
}

#[test]
fn rule_variant_with_payload_2() {
    check_success(r#"
    fn f() -> [IgnoreMe | CheckMe([CheckMeToo])] {
        CheckMe(CheckMeToo)
    }
    "#);
}

#[test]
fn rule_variant_with_payload_syn() {
    check_success(r#"
    type T = [CheckMe(Int)]
    fn f() -> T {
        CheckMe(0)
    }
    "#);
}

#[test]
fn rule_variant_with_payload_not_inferrable() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let x = InferMe(0);
        0
    }
    "#), @r###"
      3 |         let x = InferMe(0);
                          ~~~~~~~~~~
    Cannot infer the type of the expression. Further type annotations are required.
    "###);
}

#[test]
fn rule_variant_with_payload_no_variant_type() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        CheckMe(0)
    }
    "#), @r###"
      3 |         CheckMe(0)
                  ~~~~~~~~~~
    Expected an expression of type `Int` but found variant constructor `CheckMe`.
    "###);
}

#[test]
fn rule_variant_with_payload_unknown_constructor() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> [NotCheckMe] {
        CheckMe(0)
    }
    "#), @r###"
      3 |         CheckMe(0)
                  ~~~~~~~~~~
    `CheckMe` is not a possible constructor for variant type `[NotCheckMe]`.
    "###);
}

#[test]
fn rule_variant_with_payload_constructor_without_payload() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> [CheckMe] {
        CheckMe(0)
    }
    "#), @r###"
      3 |         CheckMe(0)
                  ~~~~~~~~~~
    Constructor `CheckMe` of variant type `[CheckMe]` does not take a payload.
    "###);
}

#[test]
fn rule_variant_with_payload_mismatch() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> [CheckMe(Int)] {
        CheckMe(CheckMeToo)
    }
    "#), @r###"
      3 |         CheckMe(CheckMeToo)
                          ~~~~~~~~~~
    Expected an expression of type `Int` but found variant constructor `CheckMeToo`.
    "###);
}

#[test]
fn rule_match_infer_without_without_payload() {
    check_success(r#"
    fn check_me() -> [CheckMe] { CheckMe }
    fn f(x: [A | B | C([CheckMe])]) -> [CheckMe] {
        let r = match x {
            A => check_me(),
            B => CheckMe,
        };
        r
    }
    "#);
}

#[test]
fn rule_match_infer_with_without_payload() {
    check_success(r#"
    fn f(x: [A | B | C([CheckMe])]) -> [CheckMe] {
        let r = match x {
            C(y) => y,
            B => CheckMe,
        };
        r
    }
    "#);
}

#[test]
fn rule_match_infer_without_with_payload() {
    check_success(r#"
    fn check_me() -> [CheckMe] { CheckMe }
    fn f(x: [A | B | C([CheckMe])]) -> [CheckMe] {
        let r = match x {
            A => check_me(),
            C(y) => {
                let u: [CheckMe] = y;
                CheckMe
            }
        };
        r
    }
    "#);
}

#[test]
fn rule_match_infer_with_with_payload() {
    check_success(r#"
    fn f(x: [A | B([CheckMe]) | C([CheckMe])]) -> [CheckMe] {
        let r = match x {
            B(y) => y,
            C(z) => {
                let u: [CheckMe] = z;
                CheckMe
            }
        };
        r
    }
    "#);
}

#[test]
fn rule_match_infer_syn() {
    check_success(r#"
    type A = [InferMe]
    fn f(x: A) -> Int {
        let r = match x {
            InferMe => 0,
        };
        r
    }
    "#);
}

#[test]
fn rule_match_infer_scrutinee_not_inferrable() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let r = match InferMe {
            A => 0,
        };
        r
    }
    "#), @r###"
      3 |         let r = match InferMe {
                                ~~~~~~~
    Cannot infer the type of the expression. Further type annotations are required.
    "###);
}

#[test]
fn rule_match_infer_scrutinee_not_variant() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        let r = match 0 {
            A => 0,
        };
        r
    }
    "#), @r###"
      3 |         let r = match 0 {
                                ~
    Cannot match on expressions of type `Int`.
    "###);
}

#[test]
fn rule_match_infer_no_branches() {
    insta::assert_snapshot!(check_error(r#"
    fn f(x: [A]) -> Int {
        let r = match x {
        };
        r
    }
    "#), @r###"
      3 |         let r = match x {
                                ~
    Match expressions must have at least one branch.
    "###);
}

#[test]
fn rule_match_infer_unknown_constructor_without_payload() {
    insta::assert_snapshot!(check_error(r#"
    fn f(x: [A]) -> Int {
        let r = match x {
            B => 0,
        };
        r
    }
    "#), @r###"
      4 |             B => 0,
                      ~
    `B` is not a possible constructor for variant type `[A]`.
    "###);
}

#[test]
fn rule_match_infer_unknown_constructor_with_payload() {
    insta::assert_snapshot!(check_error(r#"
    fn f(x: [A]) -> Int {
        let r = match x {
            B(y) => 0,
        };
        r
    }
    "#), @r###"
      4 |             B(y) => 0,
                      ~~~~
    `B` is not a possible constructor for variant type `[A]`.
    "###);
}

#[test]
fn rule_match_infer_unexpected_payload() {
    insta::assert_snapshot!(check_error(r#"
    fn f(x: [A]) -> Int {
        let r = match x {
            A(y) => 0,
        };
        r
    }
    "#), @r###"
      4 |             A(y) => 0,
                      ~~~~
    Constructor `A` of variant type `[A]` does not take a payload.
    "###);
}

#[test]
fn rule_match_infer_expected_payload() {
    insta::assert_snapshot!(check_error(r#"
    fn f(x: [A(Int)]) -> Int {
        let r = match x {
            A => 0,
        };
        r
    }
    "#), @r###"
      4 |             A => 0,
                      ~
    Constructor `A` of variant type `[A(Int)]` needs a payload.
    "###);
}

#[test]
fn rule_match_infer_branch1_not_inferrable() {
    insta::assert_snapshot!(check_error(r#"
    fn f(x: [A]) -> Int {
        let r = match x {
            A => InferMe,
        };
        r
    }
    "#), @r###"
      4 |             A => InferMe,
                           ~~~~~~~
    Cannot infer the type of the expression. Further type annotations are required.
    "###);
}

#[test]
fn rule_match_infer_branch2_mismatch() {
    insta::assert_snapshot!(check_error(r#"
    fn f(x: [A | B]) -> Int {
        let r = match x {
            A => 0,
            B => CheckMe,
        };
        r
    }
    "#), @r###"
      5 |             B => CheckMe,
                           ~~~~~~~
    Expected an expression of type `Int` but found variant constructor `CheckMe`.
    "###);
}

#[test]
fn rule_match_infer_unknown_constructor_after_not_inferrable() {
    insta::assert_snapshot!(check_error(r#"
    fn f(x: [A | B]) -> Int {
        let r = match x {
            A => InferMe,
            C => 0,
        };
        r
    }
    "#), @r###"
      5 |             C => 0,
                      ~
    `C` is not a possible constructor for variant type `[A | B]`.
    "###);
}

#[test]
fn rule_match_infer_unknown_constructor_after_mismatch() {
    insta::assert_snapshot!(check_error(r#"
    fn f(x: [A | B]) -> Int {
        let r = match x {
            A => 0,
            B => CheckMe,
            C => 0,
        };
        r
    }
    "#), @r###"
      6 |             C => 0,
                      ~
    `C` is not a possible constructor for variant type `[A | B]`.
    "###);
}

#[test]
fn rule_match_check_without_without_payload() {
    check_success(r#"
    fn f(x: [A | B | C([CheckMe])]) -> [CheckMe] {
        match x {
            A => CheckMe,
            B => CheckMe,
        }
    }
    "#);
}

#[test]
fn rule_match_check_with_without_payload() {
    check_success(r#"
    fn f(x: [A | B | C([CheckMe])]) -> [CheckMe] {
        match x {
            C(y) => y,
            B => CheckMe,
        }
    }
    "#);
}

#[test]
fn rule_match_check_without_with_payload() {
    check_success(r#"
    fn check_me() -> [CheckMe] { CheckMe }
    fn f(x: [A | B | C([CheckMe])]) -> [CheckMe] {
        match x {
            A => CheckMe,
            C(y) => y,
        }
    }
    "#);
}

#[test]
fn rule_match_check_with_with_payload() {
    check_success(r#"
    fn f(x: [A | B([CheckMe]) | C([CheckMe])]) -> [CheckMe] {
        let r = match x {
            B(y) => y,
            C(z) => z,
        };
        r
    }
    "#);
}

#[test]
fn rule_match_check_syn() {
    check_success(r#"
    type A = [InferMe]
    fn f(x: A) -> Int {
        match x {
            InferMe => 0,
        }
    }
    "#);
}

#[test]
fn rule_match_check_scrutinee_not_inferrable() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        match InferMe {
            A => 0,
        }
    }
    "#), @r###"
      3 |         match InferMe {
                        ~~~~~~~
    Cannot infer the type of the expression. Further type annotations are required.
    "###);
}

#[test]
fn rule_match_check_scrutinee_not_variant() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int {
        match 0 {
            A => 0,
        }
    }
    "#), @r###"
      3 |         match 0 {
                        ~
    Cannot match on expressions of type `Int`.
    "###);
}

#[test]
fn rule_match_check_no_branches() {
    insta::assert_snapshot!(check_error(r#"
    fn f(x: [A]) -> Int {
        match x {
        }
    }
    "#), @r###"
      3 |         match x {
                        ~
    Match expressions must have at least one branch.
    "###);
}

#[test]
fn rule_match_check_unknown_constructor_without_payload() {
    insta::assert_snapshot!(check_error(r#"
    fn f(x: [A]) -> Int {
        match x {
            B => 0,
        }
    }
    "#), @r###"
      4 |             B => 0,
                      ~
    `B` is not a possible constructor for variant type `[A]`.
    "###);
}

#[test]
fn rule_match_check_unknown_constructor_with_payload() {
    insta::assert_snapshot!(check_error(r#"
    fn f(x: [A]) -> Int {
        match x {
            B(y) => 0,
        }
    }
    "#), @r###"
      4 |             B(y) => 0,
                      ~~~~
    `B` is not a possible constructor for variant type `[A]`.
    "###);
}

#[test]
fn rule_match_check_unexpected_payload() {
    insta::assert_snapshot!(check_error(r#"
    fn f(x: [A]) -> Int {
        match x {
            A(y) => 0,
        }
    }
    "#), @r###"
      4 |             A(y) => 0,
                      ~~~~
    Constructor `A` of variant type `[A]` does not take a payload.
    "###);
}

#[test]
fn rule_match_check_expected_payload() {
    insta::assert_snapshot!(check_error(r#"
    fn f(x: [A(Int)]) -> Int {
        match x {
            A => 0,
        }
    }
    "#), @r###"
      4 |             A => 0,
                      ~
    Constructor `A` of variant type `[A(Int)]` needs a payload.
    "###);
}

#[test]
fn rule_match_check_branch1_mismatch() {
    insta::assert_snapshot!(check_error(r#"
    fn f(x: [A]) -> Int {
        match x {
            A => CheckMe,
        }
    }
    "#), @r###"
      4 |             A => CheckMe,
                           ~~~~~~~
    Expected an expression of type `Int` but found variant constructor `CheckMe`.
    "###);
}

#[test]
fn rule_match_check_branch2_mismatch() {
    insta::assert_snapshot!(check_error(r#"
    fn f(x: [A | B]) -> Int {
        let r = match x {
            A => 0,
            B => CheckMe,
        };
        r
    }
    "#), @r###"
      5 |             B => CheckMe,
                           ~~~~~~~
    Expected an expression of type `Int` but found variant constructor `CheckMe`.
    "###);
}

#[test]
fn rule_match_check_unknown_constructor_after_mismatch() {
    insta::assert_snapshot!(check_error(r#"
    fn f(x: [A | B]) -> Int {
        match x {
            A => CheckMe,
            C => 0,
        }
    }
    "#), @r###"
      5 |             C => 0,
                      ~
    `C` is not a possible constructor for variant type `[A | B]`.
    "###);
}
