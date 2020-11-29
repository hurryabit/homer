
(module
  (import "runtime" "alloc_i32" (func $alloc_i32 (param i32)))
  (import "runtime" "pop" (func $pop (result i32)))
  (import "runtime" "pop_" (func $pop_))
  (import "runtime" "dup" (func $dup))
  (import "runtime" "push" (func $push (param i32)))
  (import "runtime" "add" (func $add))
  (import "runtime" "ret" (func $ret (param i32)))
  (import "runtime" "deref_i32" (func $deref_i32 (result i32)))
  (import "runtime" "loadenv" (func $loadenv (param i32) (param i32)))
  (import "runtime" "alloc_closure" (func $alloc_closure (param i32) (param i32)))
  (import "runtime" "gc" (func $gc))
  (import "runtime" "assert_stackempty" (func $assert_stackempty))
  (import "runtime" "assert_i32" (func $assert_i32 (param i32)))
  (import "runtime" "assert_boxed_i32" (func $assert_boxed_i32 (param i32)))

  ;; let x = ...
  ;; fn foo(a: Int) -> Int { a + x }
  (func $foo (result i32)
    (call $loadenv
      (i32.const 1)  ;; skip a
      (i32.const 0)) ;; load x
    ;; stack: <x> <a> <ptr to clo>
    call $add
    ;; stack: <a + x> <ptr to clo>
    call $deref_i32
  )

  (func $test_closure
    call $assert_stackempty
    (call $alloc_i32 (i32.const 123))
    (call $alloc_closure
       (i32.const 0)
       (i32.const 1))
    ;; stack: <clo>
    (call $loadenv
       (i32.const 0)
       (i32.const 0))
    ;; stack: 123 <clo>
    (call $assert_boxed_i32 (i32.const 123))
    call $pop_

    (call $push (i32.const 222))
    (call $loadenv
       (i32.const 1) ;; skip 222
       (i32.const 0))
    (call $assert_boxed_i32 (i32.const 123))

    call $pop_ ;; 123
    call $pop_ ;; 222
    call $pop_ ;; <clo>

    call $assert_stackempty
  )

  (func $test_stack
    call $assert_stackempty
    (call $push (i32.const 1))
    (call $push (i32.const 2))
    (call $push (i32.const 3))
    (call $push
      (i32.add
        (call $pop)
        (call $pop)))
    (call $assert_i32 (i32.const 5))
    call $pop_
    call $pop_
    call $assert_stackempty)

  (func $test_add
    call $assert_stackempty
    (call $alloc_i32 (i32.const 10))
    (call $alloc_i32 (i32.const 20))
    (call $add)
    (call $assert_boxed_i32 (i32.const 30))
    call $pop_
    call $assert_stackempty
  )

  (func $test_gc_i32
    call $assert_stackempty
    (call $alloc_i32 (i32.const 123))
    (call $alloc_i32 (i32.const 234))
    (call $assert_boxed_i32 (i32.const 234))
    call $gc
    (call $assert_boxed_i32 (i32.const 234))
    call $pop_
    (call $assert_boxed_i32 (i32.const 123))
    call $pop_
    call $assert_stackempty
  )

  (func $test_gc_closure
    call $assert_stackempty

    ;; allocate closure 1
    (call $alloc_i32 (i32.const 123))
    (call $alloc_closure
       (i32.const 0)
       (i32.const 1))

    (call $loadenv
       (i32.const 0)
       (i32.const 0))
    (call $assert_boxed_i32 (i32.const 123))
    call $pop_

    call $gc

    ;; check closure 1
    (call $loadenv
       (i32.const 0)
       (i32.const 0))
    (call $assert_boxed_i32 (i32.const 123))
    call $pop_

    ;; allocate closure 2
    (call $alloc_i32 (i32.const 256))
    (call $alloc_closure
       (i32.const 0)
       (i32.const 1))

    call $gc

    ;; check closure 2
    (call $loadenv
       (i32.const 0)
       (i32.const 0))
    (call $assert_boxed_i32 (i32.const 256))
    call $pop_ ;; 256
    call $pop_ ;; <clo2>

    ;; check closure 1
    (call $loadenv
       (i32.const 0)
       (i32.const 0))
    (call $assert_boxed_i32 (i32.const 123))
    call $pop_ ;; 123

    call $pop_ ;; <clo1>
    call $assert_stackempty
  )

)
