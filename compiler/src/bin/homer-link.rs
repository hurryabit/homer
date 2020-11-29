
use anyhow::Result;
use wasmtime::*;

fn main() -> Result<()> {
    let engine = Engine::default();
    let store = Store::new(&engine);

    let mut linker = Linker::new(&store);
    let runtime = Module::from_file(&engine, "runtime/runtime.wasm")?;
    let test = Module::from_file(&engine, "runtime/test.wat")?;
    let runtime_inst = linker.instantiate(&runtime)?;
    linker.instance("runtime", &runtime_inst)?;

    // And with that we can perform the final link and the execute the module.
    let test_inst = linker.instantiate(&test)?;
    let run = test_inst.get_func("test_closure_apply").unwrap();
    let run = run.get0::<()>()?;
    run()?;
    Ok(())
}
