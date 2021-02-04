use crate::*;
use std::sync::Arc;
use wasmtime::*;

fn with_wasm_result<R>(main: &str, input: &str, f: fn(i64) -> R) -> R {
    let db = &mut build::CompilerDB::new();
    let uri = build::Uri::new("test.doh");
    db.set_input(uri, Arc::new(input.to_string()));

    db.with_diagnostics(uri, |diagnostics| {
        for d in diagnostics {
            panic!("diagnostics: {:?}", d);
        }
    });
    let module = db.anf_module(uri).expect("module could not be compiled");

    let wasm_module = match codegen_wasm::gen_module(&module) {
        Ok(wasm_module) => wasm_module,
        Err(err) => panic!("Error: {:?}", err),
    };
    let wasm_code = wasm_module.to_bytes().unwrap();
    let engine = Engine::default();
    let store = Store::new(&engine);
    let mut linker = Linker::new(&store);

    linker.func("host", "log_i32", |x: i32| println!("{}", x)).unwrap();
    linker.func("host", "abort", |x: i32| panic!("abort: {}", x)).unwrap();
    linker
        .func("host", "log_str", |caller: Caller, ptr: u64, len: i32| {
            let memory = caller.get_export("memory").unwrap().into_memory().unwrap();
            let str = unsafe {
                let bytes = &memory.data_unchecked()[ptr as usize..][..len as usize];
                match std::str::from_utf8(bytes) {
                    Ok(s) => s.to_string(),
                    Err(_) => panic!("not valid utf-8"),
                }
            };
            println!("LOG: {}", str)
        })
        .unwrap();

    let test = wasmtime::Module::from_binary(&engine, &wasm_code[0..]).unwrap();
    let test_inst = linker.instantiate(&test).unwrap();

    let init = test_inst.get_func("init").unwrap();
    init.get0::<()>().unwrap()().unwrap();

    let run = test_inst.get_func(format!("${}", main).as_str()).unwrap();
    let run = run.get0::<()>().unwrap();
    run().unwrap();

    let deref = test_inst.get_func("pop").unwrap().get0::<i64>().unwrap();
    let result = deref().unwrap();
    println!("result: {}", result);

    let assert_stackempty = test_inst.get_func("assert_stackempty").unwrap().get0::<()>().unwrap();
    assert_stackempty().unwrap();

    let assert_heapempty = test_inst.get_func("assert_heapempty").unwrap().get0::<()>().unwrap();
    assert_heapempty().unwrap();

    f(result)
}

fn wasm_value(main: &str, input: &str) -> i64 {
    with_wasm_result(main, input, |result| result)
}

#[test]
fn bench() {
    let input = std::fs::read_to_string(std::path::Path::new("../examples/bench.doh")).unwrap();
    with_wasm_result("main", &input, |result| {
        assert_eq!(result, 500500);
    });
}

#[test]
fn output_int() {
    assert_eq!(
        wasm_value(
            "f",
            r#"
    fn f() -> Int { 42 }
    "#
        ),
        42
    );
}

#[test]
fn test_if() {
    assert_eq!(
        wasm_value(
            "f",
            r#"
    fn f() -> Int {
        if 10 > 20 {
            1
        } else {
            2
        }
    }
    "#
        ),
        2
    );
}

#[test]
fn test_cmp() {
    assert_eq!(
        wasm_value(
            "f",
            r#"
    fn f() -> Int {
        let x = 10;
        let y = 0 - 5;
        if x >= y {
          if x < y {
            0
          } else {
            if x == 10 {
              if x != y {
                if x <= 10 {
                  1
                } else {
                  0
                }
              } else {
                0
              }
            } else {
              0
            }
        }
      } else {
        0
      }
    }
    "#),
      1
    );
}

#[test]
fn test_foo() {
    assert_eq!(
        wasm_value(
            "f",
            r#"

    type Option<A> = [ None | Some(A) ]
    type Pair<A, B> = {fst: A, snd: B}

    fn foo(k: Int) -> Option<Pair<Int, Int>> {
        if k >= 0 {
            Some({fst = k-1, snd = k})
        } else {
            None
        }
    }

    fn f() -> Int {
      match foo(10) {
        None => 1,
        Some(p) =>
          match foo(p.fst - 10) {
            None => 0,
            Some(x) => 2,
          },
      }
    }
    "#),
      0
    );
}

#[test]
fn test_recursive() {
    assert_eq!(
        wasm_value(
            "f",
            r#"

    fn f() -> Int {
      0
    }
    "#),
      0
    );


}

#[test]
fn test_false_true() {
    assert_eq!(
        wasm_value(
            "f",
            r#"
    fn f() -> Int { 
        if false == true { 
            0
        } else {
            if true {
                1
            } else {
                0
            }
        }
    }
    "#
        ),
        1
    );
}

#[test]
fn test_record() {
    assert_eq!(
        wasm_value(
            "f",
            r#"
    fn f() -> Int { 
        let r = {a = 5, b = 17};
        r.a + r.b
    }
    "#
        ),
        22
    );
}

#[test]
fn test_variant() {
    assert_eq!(
        wasm_value(
            "f",
            r#"
    fn f() -> Int { 
        let v1: [A | B(Int)] = B(23);
        let x = match v1 {
            A => 0,
            B(x) => x,
        };
        let v2: [A | B(Int)] = A;
        let y = match v2 {
            A => 1,
            B(x) => x,
        };
        x + y
    }"#
        ),
        23 + 1
    );
}

#[test]
fn test_closure() {
    assert_eq!(
        wasm_value(
            "f",
            r#"
    fn f() -> Int {
        let x = 11;
        let y = 22;
        let z = 33;
        let clo = fn (y: Int) { x + y };
        clo(z - y)
    }
    "#
        ),
        11 + 33 - 22
    );
}

#[test]
fn test_closure2() {
    assert_eq!(
        wasm_value(
            "f",
            r#"

    fn t(x: Int, y: Int) -> Int {
      x + x - y
    }

    fn f() -> Int {
        let x = 1;
        let y = 2;
        let z = 3;
        let clo = fn (y: Int) { x + z - y };
        let clo2 = fn (f: (Int) -> Int, g: Int) { f(t(g - z - x, 1)) };
        let h = 5;
        clo2(clo, h)
    }
    "#
        ),
        1 + 3 - (2*(5 - 3 - 1) - 1)
    );
}
#[test]
fn tail_call_optimization() {
    let input = r#"
    fn sum_up_to(s: Int, n: Int) -> Int {
        let b = n > 0;
        if b {
            sum_up_to(s + n, n - 1)
        } else {
            s
        }
    }

    fn f() -> Int {
        sum_up_to(0, 1000)
    }
    "#;
    with_wasm_result("f", input, |result| {
        // TODO: Read runtime.max_stack.
        assert_eq!(result, 500500);
    });
}
