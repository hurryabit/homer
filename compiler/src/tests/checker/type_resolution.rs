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
        syn: Int @ 3:14-3:17
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
        syn: ResolveMe @ 3:14-3:23
        type_arg: INT @ 3:24-3:27
    "###);
}

#[test]
fn resolve_type_syn_def_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type Int<A> = A
    type X = Int<Bool>
    "#), @r###"
    APP
        syn: Int @ 3:14-3:17
        type_arg: BOOL @ 3:18-3:22
    "###);
}

#[test]
fn resolve_type_arg1_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type F<A> = A
    type X = F<Int>
    "#), @r###"
    APP
        syn: F @ 3:14-3:15
        type_arg: INT @ 3:16-3:19
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
        syn: F @ 4:14-4:15
        type_arg: APP @ 4:16-4:19
            syn: Int @ 4:16-4:19
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
        syn: F @ 4:25-4:26
        type_arg: ResolveMe @ 4:27-4:36
    "###);
}

#[test]
fn resolve_type_arg1_var_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type F<A> = A
    type X<Int> = F<Int>
    "#), @r###"
    APP
        syn: F @ 3:19-3:20
        type_arg: Int @ 3:21-3:24
    "###);
}

#[test]
fn resolve_type_arg2_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type F<A, B> = B
    type X = F<Bool, Int>
    "#), @r###"
    APP
        syn: F @ 3:14-3:15
        type_arg: BOOL @ 3:16-3:20
        type_arg: INT @ 3:22-3:25
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
        syn: F @ 4:14-4:15
        type_arg: BOOL @ 4:16-4:20
        type_arg: APP @ 4:22-4:25
            syn: Int @ 4:22-4:25
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
        syn: F @ 4:25-4:26
        type_arg: BOOL @ 4:27-4:31
        type_arg: ResolveMe @ 4:33-4:42
    "###);
}

#[test]
fn resolve_type_arg2_var_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type F<A, B> = B
    type X<Int> = F<Bool, Int>
    "#), @r###"
    APP
        syn: F @ 3:19-3:20
        type_arg: BOOL @ 3:21-3:25
        type_arg: Int @ 3:27-3:30
    "###);
}

#[test]
fn resolve_type_param1_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X = (Int) -> Bool
    "#), @r###"
    FUN
        param: INT @ 2:15-2:18
        result: BOOL @ 2:23-2:27
    "###);
}

#[test]
fn resolve_type_param1_def_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type Int = Bool
    type X = (Int) -> Bool
    "#), @r###"
    FUN
        param: APP @ 3:15-3:18
            syn: Int @ 3:15-3:18
        result: BOOL @ 3:23-3:27
    "###);
}

#[test]
fn resolve_type_param1_var_def() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type ResolveMe = Int
    type X<ResolveMe> = (ResolveMe) -> Bool
    "#), @r###"
    FUN
        param: ResolveMe @ 3:26-3:35
        result: BOOL @ 3:40-3:44
    "###);
}

#[test]
fn resolve_type_param1_var_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X<Int> = (Int) -> Bool
    "#), @r###"
    FUN
        param: Int @ 2:20-2:23
        result: BOOL @ 2:28-2:32
    "###);
}

#[test]
fn resolve_type_param2_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X = ({}, Int) -> Bool
    "#), @r###"
    FUN
        param: RECORD @ 2:15-2:17
        param: INT @ 2:19-2:22
        result: BOOL @ 2:27-2:31
    "###);
}

#[test]
fn resolve_type_param2_def_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type Int = Bool
    type X = ({}, Int) -> Bool
    "#), @r###"
    FUN
        param: RECORD @ 3:15-3:17
        param: APP @ 3:19-3:22
            syn: Int @ 3:19-3:22
        result: BOOL @ 3:27-3:31
    "###);
}

#[test]
fn resolve_type_param2_var_def() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type ResolveMe = Int
    type X<ResolveMe> = ({}, ResolveMe) -> Bool
    "#), @r###"
    FUN
        param: RECORD @ 3:26-3:28
        param: ResolveMe @ 3:30-3:39
        result: BOOL @ 3:44-3:48
    "###);
}

#[test]
fn resolve_type_param2_var_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X<Int> = ({}, Int) -> Bool
    "#), @r###"
    FUN
        param: RECORD @ 2:20-2:22
        param: Int @ 2:24-2:27
        result: BOOL @ 2:32-2:36
    "###);
}

#[test]
fn resolve_type_result_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X = () -> Int
    "#), @r###"
    FUN
        result: INT @ 2:20-2:23
    "###);
}

#[test]
fn resolve_type_result_def_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type Int = Bool
    type X = () -> Int
    "#), @r###"
    FUN
        result: APP @ 3:20-3:23
            syn: Int @ 3:20-3:23
    "###);
}

#[test]
fn resolve_type_result_var_def() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type ResolveMe = Int
    type X<ResolveMe> = () -> ResolveMe
    "#), @r###"
    FUN
        result: ResolveMe @ 3:31-3:40
    "###);
}

#[test]
fn resolve_type_result_var_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X<Int> = () -> Int
    "#), @r###"
    FUN
        result: Int @ 2:25-2:28
    "###);
}

#[test]
fn resolve_type_field1_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X = {a: Int}
    "#), @r###"
    RECORD
        field: a @ 2:15-2:16
        type: INT @ 2:18-2:21
    "###);
}

#[test]
fn resolve_type_field1_def_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type Int = Bool
    type X = {a: Int}
    "#), @r###"
    RECORD
        field: a @ 3:15-3:16
        type: APP @ 3:18-3:21
            syn: Int @ 3:18-3:21
    "###);
}

#[test]
fn resolve_type_field1_var_def() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type ResolveMe = Int
    type X<ResolveMe> = {a: ResolveMe}
    "#), @r###"
    RECORD
        field: a @ 3:26-3:27
        type: ResolveMe @ 3:29-3:38
    "###);
}

#[test]
fn resolve_type_field1_var_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X<Int> = {a: Int}
    "#), @r###"
    RECORD
        field: a @ 2:20-2:21
        type: Int @ 2:23-2:26
    "###);
}

#[test]
fn resolve_type_field2_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X = {a: Bool, b: Int}
    "#), @r###"
    RECORD
        field: a @ 2:15-2:16
        type: BOOL @ 2:18-2:22
        field: b @ 2:24-2:25
        type: INT @ 2:27-2:30
    "###);
}

#[test]
fn resolve_type_field2_def_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type Int = Bool
    type X = {a: Bool, b: Int}
    "#), @r###"
    RECORD
        field: a @ 3:15-3:16
        type: BOOL @ 3:18-3:22
        field: b @ 3:24-3:25
        type: APP @ 3:27-3:30
            syn: Int @ 3:27-3:30
    "###);
}

#[test]
fn resolve_type_field2_var_def() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type ResolveMe = Int
    type X<ResolveMe> = {a: Bool, b: ResolveMe}
    "#), @r###"
    RECORD
        field: a @ 3:26-3:27
        type: BOOL @ 3:29-3:33
        field: b @ 3:35-3:36
        type: ResolveMe @ 3:38-3:47
    "###);
}

#[test]
fn resolve_type_field2_var_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X<Int> = {a: Bool, b: Int}
    "#), @r###"
    RECORD
        field: a @ 2:20-2:21
        type: BOOL @ 2:23-2:27
        field: b @ 2:29-2:30
        type: Int @ 2:32-2:35
    "###);
}

#[test]
fn resolve_type_constr1_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X = [C(Int)]
    "#), @r###"
    VARIANT
        constr: C @ 2:15-2:16
        type: INT @ 2:17-2:20
    "###);
}

#[test]
fn resolve_type_constr1_def_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type Int = Bool
    type X = [C(Int)]
    "#), @r###"
    VARIANT
        constr: C @ 3:15-3:16
        type: APP @ 3:17-3:20
            syn: Int @ 3:17-3:20
    "###);
}

#[test]
fn resolve_type_constr1_var_def() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type ResolveMe = Int
    type X<ResolveMe> = [C(ResolveMe)]
    "#), @r###"
    VARIANT
        constr: C @ 3:26-3:27
        type: ResolveMe @ 3:28-3:37
    "###);
}

#[test]
fn resolve_type_constr1_var_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X<Int> = [C(Int)]
    "#), @r###"
    VARIANT
        constr: C @ 2:20-2:21
        type: Int @ 2:22-2:25
    "###);
}

#[test]
fn resolve_type_constr2_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X = [B | C(Int)]
    "#), @r###"
    VARIANT
        constr: B @ 2:15-2:16
        constr: C @ 2:19-2:20
        type: INT @ 2:21-2:24
    "###);
}

#[test]
fn resolve_type_constr2_def_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type Int = Bool
    type X = [B | C(Int)]
    "#), @r###"
    VARIANT
        constr: B @ 3:15-3:16
        constr: C @ 3:19-3:20
        type: APP @ 3:21-3:24
            syn: Int @ 3:21-3:24
    "###);
}

#[test]
fn resolve_type_constr2_var_def() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type ResolveMe = Int
    type X<ResolveMe> = [B | C(ResolveMe)]
    "#), @r###"
    VARIANT
        constr: B @ 3:26-3:27
        constr: C @ 3:30-3:31
        type: ResolveMe @ 3:32-3:41
    "###);
}

#[test]
fn resolve_type_constr2_var_int() {
    insta::assert_debug_snapshot!(check_output_type("X", r#"
    type X<Int> = [B | C(Int)]
    "#), @r###"
    VARIANT
        constr: B @ 2:20-2:21
        constr: C @ 2:24-2:25
        type: Int @ 2:26-2:29
    "###);
}

#[test]
fn resolve_func_sign_int() {
    insta::assert_debug_snapshot!(check_output_func_decl("f", r#"
    fn f(x: Int) -> Int { x }
    "#), @r###"
    FUNCDECL
        name: f @ 2:8-2:9
        param: x @ 2:10-2:11
        type: INT @ 2:13-2:16
        result: INT @ 2:21-2:24
        body: VAR @ 2:27-2:28
            var: x @ 2:27-2:28
    "###);
}

#[test]
fn resolve_func_sign_def_int() {
    insta::assert_debug_snapshot!(check_output_func_decl("f", r#"
    type Int = Bool
    fn f(x: Int) -> Int { x }
    "#), @r###"
    FUNCDECL
        name: f @ 3:8-3:9
        param: x @ 3:10-3:11
        type: APP @ 3:13-3:16
            syn: Int @ 3:13-3:16
        result: APP @ 3:21-3:24
            syn: Int @ 3:21-3:24
        body: VAR @ 3:27-3:28
            var: x @ 3:27-3:28
    "###);
}

#[test]
fn resolve_func_sign_var_def() {
    insta::assert_debug_snapshot!(check_output_func_decl("f", r#"
    type ResolveMe = Int
    fn f(x: ResolveMe) -> ResolveMe { x }
    "#), @r###"
    FUNCDECL
        name: f @ 3:8-3:9
        param: x @ 3:10-3:11
        type: APP @ 3:13-3:22
            syn: ResolveMe @ 3:13-3:22
        result: APP @ 3:27-3:36
            syn: ResolveMe @ 3:27-3:36
        body: VAR @ 3:39-3:40
            var: x @ 3:39-3:40
    "###);
}

#[test]
fn resolve_func_sign_var_int() {
    insta::assert_debug_snapshot!(check_output_func_decl("f", r#"
    fn f<Int>(x: Int) -> Int { x }
    "#), @r###"
    FUNCDECL
        name: f @ 2:8-2:9
        type_param: Int @ 2:10-2:13
        param: x @ 2:15-2:16
        type: Int @ 2:18-2:21
        result: Int @ 2:26-2:29
        body: VAR @ 2:32-2:33
            var: x @ 2:32-2:33
    "###);
}

#[test]
fn resolve_func_let_lam_int() {
    insta::assert_debug_snapshot!(check_output_func_decl("f", r#"
    fn f() -> Bool { let g = fn (x: Int) { false }; true }
    "#), @r###"
    FUNCDECL
        name: f @ 2:8-2:9
        result: BOOL @ 2:15-2:19
        body: LET @ 2:22-2:57
            binder: g @ 2:26-2:27
            type: INFERRED @ 2:26-2:27
                type: FUN
                    param: INT
                    result: BOOL
            bindee: LAM @ 2:30-2:51
                param: x @ 2:34-2:35
                type: INT @ 2:37-2:40
                body: false @ 2:44-2:49
            tail: true @ 2:53-2:57
    "###);
}

#[test]
fn resolve_func_let_lam_def_int() {
    insta::assert_debug_snapshot!(check_output_func_decl("f", r#"
    type Int = Bool
    fn f() -> Bool { let g = fn (x: Int) { false }; true }
    "#), @r###"
    FUNCDECL
        name: f @ 3:8-3:9
        result: BOOL @ 3:15-3:19
        body: LET @ 3:22-3:57
            binder: g @ 3:26-3:27
            type: INFERRED @ 3:26-3:27
                type: FUN
                    param: APP
                        syn: Int
                    result: BOOL
            bindee: LAM @ 3:30-3:51
                param: x @ 3:34-3:35
                type: APP @ 3:37-3:40
                    syn: Int @ 3:37-3:40
                body: false @ 3:44-3:49
            tail: true @ 3:53-3:57
    "###);
}

#[test]
fn resolve_func_let_lam_var_def() {
    insta::assert_debug_snapshot!(check_output_func_decl("f", r#"
    type ResolveMe = Int
    fn f() -> Bool { let g = fn (x: ResolveMe) { false }; true }
    "#), @r###"
    FUNCDECL
        name: f @ 3:8-3:9
        result: BOOL @ 3:15-3:19
        body: LET @ 3:22-3:63
            binder: g @ 3:26-3:27
            type: INFERRED @ 3:26-3:27
                type: FUN
                    param: APP
                        syn: ResolveMe
                    result: BOOL
            bindee: LAM @ 3:30-3:57
                param: x @ 3:34-3:35
                type: APP @ 3:37-3:46
                    syn: ResolveMe @ 3:37-3:46
                body: false @ 3:50-3:55
            tail: true @ 3:59-3:63
    "###);
}

#[test]
fn resolve_func_let_lam_var_int() {
    insta::assert_debug_snapshot!(check_output_func_decl("f", r#"
    fn f<Int>() -> Bool { let g = fn (x: Int) { false }; true }
    "#), @r###"
    FUNCDECL
        name: f @ 2:8-2:9
        type_param: Int @ 2:10-2:13
        result: BOOL @ 2:20-2:24
        body: LET @ 2:27-2:62
            binder: g @ 2:31-2:32
            type: INFERRED @ 2:31-2:32
                type: FUN
                    param: Int
                    result: BOOL
            bindee: LAM @ 2:35-2:56
                param: x @ 2:39-2:40
                type: Int @ 2:42-2:45
                body: false @ 2:49-2:54
            tail: true @ 2:58-2:62
    "###);
}

#[test]
fn resolve_func_inst_int() {
    insta::assert_debug_snapshot!(check_output_func_decl("f", r#"
    fn g<A>() -> Bool { true }
    fn f() -> Bool { g@<Int>() }
    "#), @r###"
    FUNCDECL
        name: f @ 3:8-3:9
        result: BOOL @ 3:15-3:19
        body: APPFUN @ 3:22-3:31
            fun: g @ 3:22-3:23
            type_arg: INT @ 3:25-3:28
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
        name: f @ 4:8-4:9
        result: BOOL @ 4:15-4:19
        body: APPFUN @ 4:22-4:31
            fun: g @ 4:22-4:23
            type_arg: APP @ 4:25-4:28
                syn: Int @ 4:25-4:28
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
        name: f @ 4:8-4:9
        result: BOOL @ 4:15-4:19
        body: APPFUN @ 4:22-4:37
            fun: g @ 4:22-4:23
            type_arg: APP @ 4:25-4:34
                syn: ResolveMe @ 4:25-4:34
    "###);
}

#[test]
fn resolve_func_inst_var_int() {
    insta::assert_debug_snapshot!(check_output_func_decl("f", r#"
    fn g<A>() -> Bool { true }
    fn f<Int>() -> Bool { g@<Int>() }
    "#), @r###"
    FUNCDECL
        name: f @ 3:8-3:9
        type_param: Int @ 3:10-3:13
        result: BOOL @ 3:20-3:24
        body: APPFUN @ 3:27-3:36
            fun: g @ 3:27-3:28
            type_arg: Int @ 3:30-3:33
    "###);
}
