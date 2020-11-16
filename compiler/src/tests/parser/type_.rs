use crate::*;
use syntax::Type;

fn parse_output(input: &str) -> Type {
    super::parse_output_impl(Type::parse_test, input)
}

fn parse_error(input: &'static str) -> String {
    super::parse_error_impl(Type::parse_test, input)
}

#[test]
fn type_var() {
    insta::assert_debug_snapshot!(parse_output("A"), @"A");
}

#[test]
fn func0() {
    insta::assert_debug_snapshot!(parse_output("() -> Int"), @r###"
    FUN
        result: Int @ 6...9
    "###);
}

#[test]
fn func1() {
    insta::assert_debug_snapshot!(parse_output("(Int) -> Int"), @r###"
    FUN
        param: Int @ 1...4
        result: Int @ 9...12
    "###);
}

#[test]
fn func1_extra_comma() {
    insta::assert_debug_snapshot!(parse_output("(Int,) -> Int"), @r###"
    FUN
        param: Int @ 1...4
        result: Int @ 10...13
    "###);
}

#[test]
fn syn_app1() {
    insta::assert_debug_snapshot!(parse_output("A<Int>"), @r###"
    APP
        syn: A @ 0...1
        type_arg: Int @ 2...5
    "###);
}

#[test]
fn syn_app1_extra_comma() {
    insta::assert_debug_snapshot!(parse_output("A<Int,>"), @r###"
    APP
        syn: A @ 0...1
        type_arg: Int @ 2...5
    "###);
}

#[test]
fn syn_app2() {
    insta::assert_debug_snapshot!(parse_output("A<Int, Bool>"), @r###"
    APP
        syn: A @ 0...1
        type_arg: Int @ 2...5
        type_arg: Bool @ 7...11
    "###);
}

#[test]
fn record0() {
    insta::assert_debug_snapshot!(parse_output("{}"), @"RECORD");
}

#[test]
fn record1() {
    insta::assert_debug_snapshot!(parse_output("{x: Int}"), @r###"
    RECORD
        field: x @ 1...2
        type: Int @ 4...7
    "###);
}

#[test]
fn record1_extra_comma() {
    insta::assert_debug_snapshot!(parse_output("{x: Int,}"), @r###"
    RECORD
        field: x @ 1...2
        type: Int @ 4...7
    "###);
}

#[test]
fn variant1_unit() {
    insta::assert_debug_snapshot!(parse_output("[A]"), @r###"
    VARIANT
        constr: A @ 1...2
    "###);
}

#[test]
fn variant1_payload() {
    insta::assert_debug_snapshot!(parse_output("[A(Int)]"), @r###"
    VARIANT
        constr: A @ 1...2
        type: Int @ 3...6
    "###);
}

#[test]
fn variant2_units() {
    insta::assert_debug_snapshot!(parse_output("[A | B]"), @r###"
    VARIANT
        constr: A @ 1...2
        constr: B @ 5...6
    "###);
}

#[test]
fn variant2_unit_payload() {
    insta::assert_debug_snapshot!(parse_output("[A | B(Int)]"), @r###"
    VARIANT
        constr: A @ 1...2
        constr: B @ 5...6
        type: Int @ 7...10
    "###);
}

#[test]
fn variant2_payload_unit() {
    insta::assert_debug_snapshot!(parse_output("[A(Bool) | B]"), @r###"
    VARIANT
        constr: A @ 1...2
        type: Bool @ 3...7
        constr: B @ 11...12
    "###);
}

#[test]
fn variant2_payloads() {
    insta::assert_debug_snapshot!(parse_output("[A(Bool) | B(Int)]"), @r###"
    VARIANT
        constr: A @ 1...2
        type: Bool @ 3...7
        constr: B @ 11...12
        type: Int @ 13...16
    "###);
}

// TODO(MH): We want to allow an optional leading "|" rather
// than a trailing one.
#[test]
fn variant2_extra_bar() {
    insta::assert_debug_snapshot!(parse_output("[A | B(Int) |]"), @r###"
    VARIANT
        constr: A @ 1...2
        constr: B @ 5...6
        type: Int @ 7...10
    "###);
}

#[test]
fn func_type_zero_params_one_comma() {
    insta::assert_snapshot!(parse_error("(,) -> Int"), @r###"
    FUN
        param: ERROR @ 1...1
        result: Int @ 7...10
    --------------------------------------------------
      1 | (,) -> Int
           ~
    Unrecognized token `,` found at 1:2:1:3
    Expected one of "(", ")", "[", "{" or ID_UPPER
    "###);
}

#[test]
fn type_app_zero_args() {
    insta::assert_snapshot!(parse_error("A<>"), @r###"
    APP
        syn: A @ 0...1
        type_arg: ERROR @ 2...2
    --------------------------------------------------
      1 | A<>
            ~
    Unrecognized token `>` found at 1:3:1:4
    Expected one of "(", "[", "{" or ID_UPPER
    "###);
}

#[test]
fn record_zero_field_one_comma() {
    insta::assert_snapshot!(parse_error("{,}"), @r###"
    ERROR
    --------------------------------------------------
      1 | {,}
           ~
    Unrecognized token `,` found at 1:2:1:3
    Expected one of "}" or ID_LOWER
    "###);
}

#[test]
fn variant_zero_constructors() {
    insta::assert_snapshot!(parse_error("[]"), @r###"
    ERROR
    --------------------------------------------------
      1 | []
           ~
    Unrecognized token `]` found at 1:2:1:3
    Expected one of ID_UPPER
    "###);
}
