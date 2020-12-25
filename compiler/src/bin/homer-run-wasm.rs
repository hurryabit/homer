
use anyhow::Result;
use wasmtime::*;

fn main() -> Result<()> {
    let engine = Engine::default();
    let store = Store::new(&engine);
    let mut linker = Linker::new(&store);

    linker.func("host", "log_i32", |x: i32| println!("{}", x))?;
    linker.func("host", "abort", |x: i32| panic!("abort: {}", x))?;
    linker.func("host", "log_str", |caller: Caller, ptr: u32, len: i32| {
        let memory = caller.get_export("memory").unwrap().into_memory().unwrap();
        let str = unsafe {
            let bytes = &memory.data_unchecked()[ptr as usize..][..len as usize];
            match std::str::from_utf8(bytes) {
                Ok(s) => s.to_string(),
                Err(_) => panic!("not valid utf-8"),
            }
        };
        println!("LOG: {}", str)
    })?;

    let test = Module::from_file(&engine, "/tmp/out.wasm")?;
    let test_inst = linker.instantiate(&test)?;

    let init = test_inst.get_func("init").unwrap();
    init.get0::<()>()?()?;

    
    let run = test_inst.get_func("$main").unwrap();
    let run = run.get0::<()>()?;
    run()?;

    let deref = test_inst.get_func("deref_i64").unwrap().get0::<i64>()?;
    let result = deref()?;
    println!("result: {}", result);

    let assert_stackempty = test_inst.get_func("assert_stackempty").unwrap().get0::<()>()?;
    assert_stackempty()?;

    let assert_heapempty = test_inst.get_func("assert_heapempty").unwrap().get0::<()>()?;
    assert_heapempty()?;

    Ok(())
}