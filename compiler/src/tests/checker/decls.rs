use super::*;

#[test]
fn type_decl_1() {
    check_success(r#"
    type List<A> = [Nil | Cons({head: A, tail: List<A>})]
    "#);
}

#[test]
fn type_decl_duplicate_decl() {
    insta::assert_snapshot!(check_error(r#"
    type T<Original> = Original
    type T<Duplicate> = Duplicate
    "#), @r###"
      3 |     type T<Duplicate> = Duplicate
                   ~
    Duplicate definition of type `T`.
    "###);
}

#[test]
fn type_decl_duplicate_type_var() {
    insta::assert_snapshot!(check_error(r#"
    type T<Duplicate, Duplicate> = Duplicate
    "#), @r###"
      2 |     type T<Duplicate, Duplicate> = Duplicate
                                ~~~~~~~~~
    Duplicate type variable `Duplicate`.
    "###);
}

#[test]
fn type_decl_unknown_in_body() {
    insta::assert_snapshot!(check_error(r#"
    type T = Unknown
    "#), @r###"
      2 |     type T = Unknown
                       ~~~~~~~
    Undeclared type variable `Unknown`.
    "###);
}

#[test]
fn type_decl_illformed_body() {
    insta::assert_snapshot!(check_error(r#"
    type Illformed<A> = A
    type T = Illformed
    "#), @r###"
      3 |     type T = Illformed
                       ~~~~~~~~~
    Expected a type but found the generic type `Illformed`.
    "###);
}

#[test]
fn func_decl_1() {
  check_success(r#"
  type List<A> = [Nil | Cons({head: A, tail: List<A>})]
  fn map<A, B>(xs: List<A>, f: (A) -> B) -> List<B> {
    let g = fn (xs: List<A>) { map@<A, B>(xs, f)};
    match xs {
      Nil => Nil,
      Cons(xs) => Cons({head = f(xs.head), tail = g(xs.tail)}),
    }
  }
  "#);
}

#[test]
fn func_decl_duplicate_decl() {
    insta::assert_snapshot!(check_error(r#"
    fn f(original: Int) -> Int { 0 }
    fn f(duplicate: Int) -> Int { 0 }
    "#), @r###"
      3 |     fn f(duplicate: Int) -> Int { 0 }
                 ~
    Duplicate definition of function `f`.
    "###);
}

#[test]
fn func_decl_duplicate_type_var() {
    insta::assert_snapshot!(check_error(r#"
    fn f<Duplicate, Duplicate>() -> Int { 0 }
    "#), @r###"
      2 |     fn f<Duplicate, Duplicate>() -> Int { 0 }
                              ~~~~~~~~~
    Duplicate type variable `Duplicate`.
    "###);
}

#[test]
fn func_decl_duplicate_param() {
    insta::assert_snapshot!(check_error(r#"
    fn f(duplicate: Int, duplicate: Int) -> Int { 0 }
    "#), @r###"
      2 |     fn f(duplicate: Int, duplicate: Int) -> Int { 0 }
                                   ~~~~~~~~~
    Duplicate paramter `duplicate`.
    "###);
}

#[test]
fn func_decl_unknown_type_in_param() {
    insta::assert_snapshot!(check_error(r#"
    fn f(x: Unknown) -> Int { 0 }
    "#), @r###"
      2 |     fn f(x: Unknown) -> Int { 0 }
                      ~~~~~~~
    Undeclared type variable `Unknown`.
    "###);
}

#[test]
fn func_decl_illformed_type_in_param() {
    insta::assert_snapshot!(check_error(r#"
    type Illformed<A> = A
    fn f(x: Illformed) -> Int { 0 }
    "#), @r###"
      3 |     fn f(x: Illformed) -> Int { 0 }
                      ~~~~~~~~~
    Expected a type but found the generic type `Illformed`.
    "###);
}

#[test]
fn func_decl_unknown_type_in_result() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Unknown { 0 }
    "#), @r###"
      2 |     fn f() -> Unknown { 0 }
                        ~~~~~~~
    Undeclared type variable `Unknown`.
    "###);
}

#[test]
fn func_decl_illformed_type_in_result() {
    insta::assert_snapshot!(check_error(r#"
    type Illformed<A> = A
    fn f() -> Illformed { 0 }
    "#), @r###"
      3 |     fn f() -> Illformed { 0 }
                        ~~~~~~~~~
    Expected a type but found the generic type `Illformed`.
    "###);
}

#[test]
fn func_decl_illtyped_body() {
    insta::assert_snapshot!(check_error(r#"
    fn f() -> Int { true }
    "#), @r###"
      2 |     fn f() -> Int { true }
                              ~~~~
    Expected an expression of type `Int` but found an expression of type `Bool`.
    "###);
}
