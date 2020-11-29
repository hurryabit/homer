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
    insta::assert_debug_snapshot!(parse_output("A"), @r###"
    VAR
        var: A @ 1:1-1:2
    "###);
}

#[test]
fn func0() {
    insta::assert_debug_snapshot!(parse_output("() -> Int"), @r###"
    FUN
        result: VAR @ 1:7-1:10
            var: Int @ 1:7-1:10
    "###);
}

#[test]
fn func1() {
    insta::assert_debug_snapshot!(parse_output("(Int) -> Int"), @r###"
    FUN
        param: VAR @ 1:2-1:5
            var: Int @ 1:2-1:5
        result: VAR @ 1:10-1:13
            var: Int @ 1:10-1:13
    "###);
}

#[test]
fn func1_extra_comma() {
    insta::assert_debug_snapshot!(parse_output("(Int,) -> Int"), @r###"
    FUN
        param: VAR @ 1:2-1:5
            var: Int @ 1:2-1:5
        result: VAR @ 1:11-1:14
            var: Int @ 1:11-1:14
    "###);
}

#[test]
fn syn_app1() {
    insta::assert_debug_snapshot!(parse_output("A<Int>"), @r###"
    APP
        syn: A @ 1:1-1:2
        type_arg: VAR @ 1:3-1:6
            var: Int @ 1:3-1:6
    "###);
}

#[test]
fn syn_app1_extra_comma() {
    insta::assert_debug_snapshot!(parse_output("A<Int,>"), @r###"
    APP
        syn: A @ 1:1-1:2
        type_arg: VAR @ 1:3-1:6
            var: Int @ 1:3-1:6
    "###);
}

#[test]
fn syn_app2() {
    insta::assert_debug_snapshot!(parse_output("A<Int, Bool>"), @r###"
    APP
        syn: A @ 1:1-1:2
        type_arg: VAR @ 1:3-1:6
            var: Int @ 1:3-1:6
        type_arg: VAR @ 1:8-1:12
            var: Bool @ 1:8-1:12
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
        field: x @ 1:2-1:3
        type: VAR @ 1:5-1:8
            var: Int @ 1:5-1:8
    "###);
}

#[test]
fn record1_extra_comma() {
    insta::assert_debug_snapshot!(parse_output("{x: Int,}"), @r###"
    RECORD
        field: x @ 1:2-1:3
        type: VAR @ 1:5-1:8
            var: Int @ 1:5-1:8
    "###);
}

#[test]
fn variant1_unit() {
    insta::assert_debug_snapshot!(parse_output("[A]"), @r###"
    VARIANT
        constr: A @ 1:2-1:3
    "###);
}

#[test]
fn variant1_payload() {
    insta::assert_debug_snapshot!(parse_output("[A(Int)]"), @r###"
    VARIANT
        constr: A @ 1:2-1:3
        type: VAR @ 1:4-1:7
            var: Int @ 1:4-1:7
    "###);
}

#[test]
fn variant2_units() {
    insta::assert_debug_snapshot!(parse_output("[A | B]"), @r###"
    VARIANT
        constr: A @ 1:2-1:3
        constr: B @ 1:6-1:7
    "###);
}

#[test]
fn variant2_unit_payload() {
    insta::assert_debug_snapshot!(parse_output("[A | B(Int)]"), @r###"
    VARIANT
        constr: A @ 1:2-1:3
        constr: B @ 1:6-1:7
        type: VAR @ 1:8-1:11
            var: Int @ 1:8-1:11
    "###);
}

#[test]
fn variant2_payload_unit() {
    insta::assert_debug_snapshot!(parse_output("[A(Bool) | B]"), @r###"
    VARIANT
        constr: A @ 1:2-1:3
        type: VAR @ 1:4-1:8
            var: Bool @ 1:4-1:8
        constr: B @ 1:12-1:13
    "###);
}

#[test]
fn variant2_payloads() {
    insta::assert_debug_snapshot!(parse_output("[A(Bool) | B(Int)]"), @r###"
    VARIANT
        constr: A @ 1:2-1:3
        type: VAR @ 1:4-1:8
            var: Bool @ 1:4-1:8
        constr: B @ 1:12-1:13
        type: VAR @ 1:14-1:17
            var: Int @ 1:14-1:17
    "###);
}

// TODO(MH): We want to allow an optional leading "|" rather
// than a trailing one.
#[test]
fn variant2_extra_bar() {
    insta::assert_debug_snapshot!(parse_output("[A | B(Int) |]"), @r###"
    VARIANT
        constr: A @ 1:2-1:3
        constr: B @ 1:6-1:7
        type: VAR @ 1:8-1:11
            var: Int @ 1:8-1:11
    "###);
}

#[test]
fn func_type_zero_params_one_comma() {
    insta::assert_snapshot!(parse_error("(,) -> Int"), @r###"
    FUN
        param: ERROR @ 1:2-1:2
        result: VAR @ 1:8-1:11
            var: Int @ 1:8-1:11
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
        syn: A @ 1:1-1:2
        type_arg: ERROR @ 1:3-1:3
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
