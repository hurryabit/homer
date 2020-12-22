
use anyhow::Result;
use wasmtime::*;

fn main() -> Result<()> {
    let engine = Engine::default();
    let store = Store::new(&engine);
    let mut linker = Linker::new(&store);

    linker.func("host", "log_i32", |x: i32| println!("{}", x))?;
    linker.func("host", "abort", |x: i32| panic!("abort: {}", x))?;

    let runtime = Module::from_file(&engine, "runtime/runtime.wasm")?;
    let test = Module::from_file(&engine, "/tmp/out.wasm")?;
    let runtime_inst = linker.instantiate(&runtime)?;
    linker.instance("runtime", &runtime_inst)?;
    let test_inst = linker.instantiate(&test)?;

    let run = test_inst.get_func("main").unwrap();

    let run = run.get0::<()>()?;
    run()?;

    let deref = runtime_inst.get_func("deref_i64").unwrap().get0::<i64>()?;
    let result = deref()?;
    println!("result: {}", result);

    let assert_stackempty = runtime_inst.get_func("assert_stackempty").unwrap().get0::<()>()?;
    assert_stackempty()?;

    let assert_heapempty = runtime_inst.get_func("assert_heapempty").unwrap().get0::<()>()?;
    assert_heapempty()?;


    Ok(())
}
