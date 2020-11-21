use homer_compiler::{build, cek, syntax};
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
        let machine = cek::Machine::new(&module, syntax::ExprVar::new("main"));
        let (addr, mem) = machine.run();
        println!("Result: {}", mem.value_at(addr));
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
