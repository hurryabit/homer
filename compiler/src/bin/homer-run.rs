use homer_compiler::{build, cek, syntax, backend_wasm};
use std::sync::Arc;

#[allow(clippy::iter_nth_zero)]
fn run() -> std::io::Result<bool> {
    let path = if let Some(path) = std::env::args().nth(1) {
        path
    } else {
        panic!("usage: {} <filename>", std::env::args().nth(0).unwrap())
    };
    let db = &mut build::CompilerDB::new();
    let uri = build::Uri::new(&path);
    let input = Arc::new(std::fs::read_to_string(path)?);
    db.set_input(uri, Arc::clone(&input));

    let mut success = true;
    db.with_diagnostics(uri, |diagnostics| {
        for diagnostic in diagnostics {
            success = false;
            eprintln!("{}\n{}", "-".repeat(50), diagnostic.layout(&input));
        }
    });


    if let Some(module) = db.anf_module(uri) {

        match backend_wasm::gen_module(&module) {
            Ok(wasm_module) =>
                parity_wasm::elements::serialize_to_file("/tmp/out.wasm", wasm_module).unwrap(),
            Err(err) =>
                eprintln!("Error: {:?}", err),
        }


        let machine = cek::Machine::new(&module, syntax::ExprVar::new("main"));
        let result = machine.run();
        println!("Result: {}", result.value());
    }
    Ok(success)
}

fn main() {
    match run() {
        Ok(true) => {}
        Ok(false) => {
            eprintln!("\nThere were errors.");
            std::process::exit(1);
        }
        Err(error) => {
            eprintln!("Error: {:?}", error);
            std::process::exit(1);
        }
    }
}
