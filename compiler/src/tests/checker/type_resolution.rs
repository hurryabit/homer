/*
Tests for type variable vs type definition resolution.

We roughly test a matrix with one coordidinate being the position in which
should be resolved and the other coordindate being the type we want to resolve
in this position. We need to distinguish between monomorphic and polymorphic
types. We only test well-formed types here.

There are the following positions in type declarations:
* [type_top] Top-level: type X = ResolveMe
* [type_syn] Synonym position of a synonym application: type X = ResolveMe<A>
* [type_argN] Argument positions of a synonym application: type X = F<..., ResolveMe, ...>
* [type_paramN] Parameter positions of a function type: type X = (..., ResolveMe, ...) -> A
* [type_result] Result position of a function type: type X = (...) -> ResolveMe
* [type_fieldN] Field positions in a record: type X = {..., a: ResolveMe, ...}
* [type_constrN] Payload positions in a variant: type X = [... | C(ResolveMe) ...]

There are also the following positions in function declarations:
* [func_sign] Function signatures (parameters and result type)
* [func_let_lam] Lambda parameter types, these also show up in the binders if
  the lambda is let bound
* [func_inst] Polymorphic function instantiations

There are the following types:
- [def] A type defined by a type declaration
- [var] A type parameter of a polymorphic type
- [int] A builtin type
- [def_int] A type defined by a type declaration shadowing a builtin type
- [var_def] A type parameter of a polymorphic type shadowing a type declaration
- [var_int] A type parameter of a polymorphic type shadowing a builtin type
- [var_def_int] A type parameter of a polymorphic type shadowing a type declaration which
  itself shadows a builtin type

We don't test [def], [var] and [var_def_int] since all interesting cases are
covered by [int], [def_int], [var_def] and [var_int] already.
*/
use super::*;

#[test]
fn resolve_type_top_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X = Int
    "#), @"INT");
}

#[test]
fn resolve_type_top_def_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type Int = Bool
    type X = Int
    "#), @r###"
    APP
        syn: Int @ 34...37
    "###);
}

#[test]
fn resolve_type_top_var_def() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type ResolveMe = Int
    type X<ResolveMe> = ResolveMe
    "#), @"ResolveMe");
}

#[test]
fn resolve_type_top_var_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X<Int> = Int
    "#), @"Int");
}

#[test]
fn resolve_type_syn_def() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type ResolveMe<A> = A
    type X = ResolveMe<Int>
    "#), @r###"
    APP
        syn: ResolveMe @ 40...49
        type_arg: INT @ 50...53
    "###);
}

#[test]
fn resolve_type_syn_def_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type Int<A> = A
    type X = Int<Bool>
    "#), @r###"
    APP
        syn: Int @ 34...37
        type_arg: BOOL @ 38...42
    "###);
}

#[test]
fn resolve_type_arg1_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type F<A> = A
    type X = F<Int>
    "#), @r###"
    APP
        syn: F @ 32...33
        type_arg: INT @ 34...37
    "###);
}

#[test]
fn resolve_type_arg1_def_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type F<A> = A
    type Int = Bool
    type X = F<Int>
    "#), @r###"
    APP
        syn: F @ 52...53
        type_arg: APP @ 54...57
            syn: Int @ 54...57
    "###);
}

#[test]
fn resolve_type_arg1_var_def() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type F<A> = A
    type ResolveMe = Int
    type X<ResolveMe> = F<ResolveMe>
    "#), @r###"
    APP
        syn: F @ 68...69
        type_arg: ResolveMe @ 70...79
    "###);
}

#[test]
fn resolve_type_arg1_var_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type F<A> = A
    type X<Int> = F<Int>
    "#), @r###"
    APP
        syn: F @ 37...38
        type_arg: Int @ 39...42
    "###);
}

#[test]
fn resolve_type_arg2_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
  type F<A, B> = B
  type X = F<Bool, Int>
  "#), @r###"
  APP
      syn: F @ 31...32
      type_arg: BOOL @ 33...37
      type_arg: INT @ 39...42
  "###);
}

#[test]
fn resolve_type_arg2_def_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
  type F<A, B> = B
  type Int = Bool
  type X = F<Bool, Int>
  "#), @r###"
  APP
      syn: F @ 49...50
      type_arg: BOOL @ 51...55
      type_arg: APP @ 57...60
          syn: Int @ 57...60
  "###);
}

#[test]
fn resolve_type_arg2_var_def() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
  type F<A, B> = B
  type ResolveMe = Int
  type X<ResolveMe> = F<Bool, ResolveMe>
  "#), @r###"
  APP
      syn: F @ 65...66
      type_arg: BOOL @ 67...71
      type_arg: ResolveMe @ 73...82
  "###);
}

#[test]
fn resolve_type_arg2_var_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
  type F<A, B> = B
  type X<Int> = F<Bool, Int>
  "#), @r###"
  APP
      syn: F @ 36...37
      type_arg: BOOL @ 38...42
      type_arg: Int @ 44...47
  "###);
}

#[test]
fn resolve_type_param1_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X = (Int) -> Bool
    "#), @r###"
    FUN
        param: INT @ 15...18
        result: BOOL @ 23...27
    "###);
}

#[test]
fn resolve_type_param1_def_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type Int = Bool
    type X = (Int) -> Bool
    "#), @r###"
    FUN
        param: APP @ 35...38
            syn: Int @ 35...38
        result: BOOL @ 43...47
    "###);
}

#[test]
fn resolve_type_param1_var_def() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type ResolveMe = Int
    type X<ResolveMe> = (ResolveMe) -> Bool
    "#), @r###"
    FUN
        param: ResolveMe @ 51...60
        result: BOOL @ 65...69
    "###);
}

#[test]
fn resolve_type_param1_var_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X<Int> = (Int) -> Bool
    "#), @r###"
    FUN
        param: Int @ 20...23
        result: BOOL @ 28...32
    "###);
}

#[test]
fn resolve_type_param2_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X = ({}, Int) -> Bool
    "#), @r###"
    FUN
        param: RECORD @ 15...17
        param: INT @ 19...22
        result: BOOL @ 27...31
    "###);
}

#[test]
fn resolve_type_param2_def_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type Int = Bool
    type X = ({}, Int) -> Bool
    "#), @r###"
    FUN
        param: RECORD @ 35...37
        param: APP @ 39...42
            syn: Int @ 39...42
        result: BOOL @ 47...51
    "###);
}

#[test]
fn resolve_type_param2_var_def() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type ResolveMe = Int
    type X<ResolveMe> = ({}, ResolveMe) -> Bool
    "#), @r###"
    FUN
        param: RECORD @ 51...53
        param: ResolveMe @ 55...64
        result: BOOL @ 69...73
    "###);
}

#[test]
fn resolve_type_param2_var_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X<Int> = ({}, Int) -> Bool
    "#), @r###"
    FUN
        param: RECORD @ 20...22
        param: Int @ 24...27
        result: BOOL @ 32...36
    "###);
}

#[test]
fn resolve_type_result_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X = () -> Int
    "#), @r###"
    FUN
        result: INT @ 20...23
    "###);
}

#[test]
fn resolve_type_result_def_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type Int = Bool
    type X = () -> Int
    "#), @r###"
    FUN
        result: APP @ 40...43
            syn: Int @ 40...43
    "###);
}

#[test]
fn resolve_type_result_var_def() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type ResolveMe = Int
    type X<ResolveMe> = () -> ResolveMe
    "#), @r###"
    FUN
        result: ResolveMe @ 56...65
    "###);
}

#[test]
fn resolve_type_result_var_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X<Int> = () -> Int
    "#), @r###"
    FUN
        result: Int @ 25...28
    "###);
}

#[test]
fn resolve_type_field1_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X = {a: Int}
    "#), @r###"
    RECORD
        field: a @ 15...16
        type: INT @ 18...21
    "###);
}

#[test]
fn resolve_type_field1_def_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type Int = Bool
    type X = {a: Int}
    "#), @r###"
    RECORD
        field: a @ 35...36
        type: APP @ 38...41
            syn: Int @ 38...41
    "###);
}

#[test]
fn resolve_type_field1_var_def() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type ResolveMe = Int
    type X<ResolveMe> = {a: ResolveMe}
    "#), @r###"
    RECORD
        field: a @ 51...52
        type: ResolveMe @ 54...63
    "###);
}

#[test]
fn resolve_type_field1_var_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X<Int> = {a: Int}
    "#), @r###"
    RECORD
        field: a @ 20...21
        type: Int @ 23...26
    "###);
}

#[test]
fn resolve_type_field2_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X = {a: Bool, b: Int}
    "#), @r###"
    RECORD
        field: a @ 15...16
        type: BOOL @ 18...22
        field: b @ 24...25
        type: INT @ 27...30
    "###);
}

#[test]
fn resolve_type_field2_def_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type Int = Bool
    type X = {a: Bool, b: Int}
    "#), @r###"
    RECORD
        field: a @ 35...36
        type: BOOL @ 38...42
        field: b @ 44...45
        type: APP @ 47...50
            syn: Int @ 47...50
    "###);
}

#[test]
fn resolve_type_field2_var_def() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type ResolveMe = Int
    type X<ResolveMe> = {a: Bool, b: ResolveMe}
    "#), @r###"
    RECORD
        field: a @ 51...52
        type: BOOL @ 54...58
        field: b @ 60...61
        type: ResolveMe @ 63...72
    "###);
}

#[test]
fn resolve_type_field2_var_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X<Int> = {a: Bool, b: Int}
    "#), @r###"
    RECORD
        field: a @ 20...21
        type: BOOL @ 23...27
        field: b @ 29...30
        type: Int @ 32...35
    "###);
}

#[test]
fn resolve_type_constr1_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X = [C(Int)]
    "#), @r###"
    VARIANT
        constr: C @ 15...16
        type: INT @ 17...20
    "###);
}

#[test]
fn resolve_type_constr1_def_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type Int = Bool
    type X = [C(Int)]
    "#), @r###"
    VARIANT
        constr: C @ 35...36
        type: APP @ 37...40
            syn: Int @ 37...40
    "###);
}

#[test]
fn resolve_type_constr1_var_def() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type ResolveMe = Int
    type X<ResolveMe> = [C(ResolveMe)]
    "#), @r###"
    VARIANT
        constr: C @ 51...52
        type: ResolveMe @ 53...62
    "###);
}

#[test]
fn resolve_type_constr1_var_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X<Int> = [C(Int)]
    "#), @r###"
    VARIANT
        constr: C @ 20...21
        type: Int @ 22...25
    "###);
}

#[test]
fn resolve_type_constr2_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X = [B | C(Int)]
    "#), @r###"
    VARIANT
        constr: B @ 15...16
        constr: C @ 19...20
        type: INT @ 21...24
    "###);
}

#[test]
fn resolve_type_constr2_def_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type Int = Bool
    type X = [B | C(Int)]
    "#), @r###"
    VARIANT
        constr: B @ 35...36
        constr: C @ 39...40
        type: APP @ 41...44
            syn: Int @ 41...44
    "###);
}

#[test]
fn resolve_type_constr2_var_def() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type ResolveMe = Int
    type X<ResolveMe> = [B | C(ResolveMe)]
    "#), @r###"
    VARIANT
        constr: B @ 51...52
        constr: C @ 55...56
        type: ResolveMe @ 57...66
    "###);
}

#[test]
fn resolve_type_constr2_var_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X<Int> = [B | C(Int)]
    "#), @r###"
    VARIANT
        constr: B @ 20...21
        constr: C @ 24...25
        type: Int @ 26...29
    "###);
}

#[test]
fn resolve_func_sign_int() {
    insta::assert_debug_snapshot!(check_output_func_decl("f", r#"
    fn f(x: Int) -> Int { x }
    "#), @r###"
    FUNCDECL
        name: f @ 8...9
        param: x @ 10...11
        type: INT @ 13...16
        result: INT @ 21...24
        body: x @ 27...28
    "###);
}

#[test]
fn resolve_func_sign_def_int() {
    insta::assert_debug_snapshot!(check_output_func_decl("f", r#"
    type Int = Bool
    fn f(x: Int) -> Int { x }
    "#), @r###"
    FUNCDECL
        name: f @ 28...29
        param: x @ 30...31
        type: APP @ 33...36
            syn: Int @ 33...36
        result: APP @ 41...44
            syn: Int @ 41...44
        body: x @ 47...48
    "###);
}

#[test]
fn resolve_func_sign_var_def() {
    insta::assert_debug_snapshot!(check_output_func_decl("f", r#"
    type ResolveMe = Int
    fn f(x: ResolveMe) -> ResolveMe { x }
    "#), @r###"
    FUNCDECL
        name: f @ 33...34
        param: x @ 35...36
        type: APP @ 38...47
            syn: ResolveMe @ 38...47
        result: APP @ 52...61
            syn: ResolveMe @ 52...61
        body: x @ 64...65
    "###);
}

#[test]
fn resolve_func_sign_var_int() {
    insta::assert_debug_snapshot!(check_output_func_decl("f", r#"
    fn f<Int>(x: Int) -> Int { x }
    "#), @r###"
    FUNCDECL
        name: f @ 8...9
        type_param: Int @ 10...13
        param: x @ 15...16
        type: Int @ 18...21
        result: Int @ 26...29
        body: x @ 32...33
    "###);
}

#[test]
fn resolve_func_let_lam_int() {
    insta::assert_debug_snapshot!(check_output_func_decl("f", r#"
    fn f() -> Bool { let g = fn (x: Int) { false }; true }
    "#), @r###"
    FUNCDECL
        name: f @ 8...9
        result: BOOL @ 15...19
        body: LET @ 22...57
            binder: g @ 26...27
            type: FUN @ 26...27
                param: INT @ 0...0
                result: BOOL @ 0...0
            bindee: LAM @ 30...51
                param: x @ 34...35
                type: INT @ 37...40
                body: false @ 44...49
            body: true @ 53...57
    "###);
}

#[test]
fn resolve_func_let_lam_def_int() {
    insta::assert_debug_snapshot!(check_output_func_decl("f", r#"
    type Int = Bool
    fn f() -> Bool { let g = fn (x: Int) { false }; true }
    "#), @r###"
    FUNCDECL
        name: f @ 28...29
        result: BOOL @ 35...39
        body: LET @ 42...77
            binder: g @ 46...47
            type: FUN @ 46...47
                param: APP @ 0...0
                    syn: Int @ 0...0
                result: BOOL @ 0...0
            bindee: LAM @ 50...71
                param: x @ 54...55
                type: APP @ 57...60
                    syn: Int @ 57...60
                body: false @ 64...69
            body: true @ 73...77
    "###);
}

#[test]
fn resolve_func_let_lam_var_def() {
    insta::assert_debug_snapshot!(check_output_func_decl("f", r#"
    type ResolveMe = Int
    fn f() -> Bool { let g = fn (x: ResolveMe) { false }; true }
    "#), @r###"
    FUNCDECL
        name: f @ 33...34
        result: BOOL @ 40...44
        body: LET @ 47...88
            binder: g @ 51...52
            type: FUN @ 51...52
                param: APP @ 0...0
                    syn: ResolveMe @ 0...0
                result: BOOL @ 0...0
            bindee: LAM @ 55...82
                param: x @ 59...60
                type: APP @ 62...71
                    syn: ResolveMe @ 62...71
                body: false @ 75...80
            body: true @ 84...88
    "###);
}

#[test]
fn resolve_func_let_lam_var_int() {
    insta::assert_debug_snapshot!(check_output_func_decl("f", r#"
    fn f<Int>() -> Bool { let g = fn (x: Int) { false }; true }
    "#), @r###"
    FUNCDECL
        name: f @ 8...9
        type_param: Int @ 10...13
        result: BOOL @ 20...24
        body: LET @ 27...62
            binder: g @ 31...32
            type: FUN @ 31...32
                param: Int @ 0...0
                result: BOOL @ 0...0
            bindee: LAM @ 35...56
                param: x @ 39...40
                type: Int @ 42...45
                body: false @ 49...54
            body: true @ 58...62
    "###);
}

#[test]
fn resolve_func_inst_int() {
    insta::assert_debug_snapshot!(check_output_func_decl("f", r#"
    fn g<A>() -> Bool { true }
    fn f() -> Bool { g@<Int>() }
    "#), @r###"
    FUNCDECL
        name: f @ 39...40
        result: BOOL @ 46...50
        body: APP @ 53...62
            fun: FUNCINST @ 53...60
                fun: g @ 53...54
                type_arg: INT @ 56...59
    "###);
}

#[test]
fn resolve_func_inst_def_int() {
    insta::assert_debug_snapshot!(check_output_func_decl("f", r#"
    type Int = Bool
    fn g<A>() -> Bool { true }
    fn f() -> Bool { g@<Int>() }
    "#), @r###"
    FUNCDECL
        name: f @ 59...60
        result: BOOL @ 66...70
        body: APP @ 73...82
            fun: FUNCINST @ 73...80
                fun: g @ 73...74
                type_arg: APP @ 76...79
                    syn: Int @ 76...79
    "###);
}

#[test]
fn resolve_func_inst_var_def() {
    insta::assert_debug_snapshot!(check_output_func_decl("f", r#"
    type ResolveMe = Int
    fn g<A>() -> Bool { true }
    fn f() -> Bool { g@<ResolveMe>() }
    "#), @r###"
    FUNCDECL
        name: f @ 64...65
        result: BOOL @ 71...75
        body: APP @ 78...93
            fun: FUNCINST @ 78...91
                fun: g @ 78...79
                type_arg: APP @ 81...90
                    syn: ResolveMe @ 81...90
    "###);
}

#[test]
fn resolve_func_inst_var_int() {
    insta::assert_debug_snapshot!(check_output_func_decl("f", r#"
    fn g<A>() -> Bool { true }
    fn f<Int>() -> Bool { g@<Int>() }
    "#), @r###"
    FUNCDECL
        name: f @ 39...40
        type_param: Int @ 41...44
        result: BOOL @ 51...55
        body: APP @ 58...67
            fun: FUNCINST @ 58...65
                fun: g @ 58...59
                type_arg: Int @ 61...64
    "###);
}
