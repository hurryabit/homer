use homer_compiler::build;
use std::sync::Arc;

#[allow(clippy::iter_nth_zero)]
fn run() -> std::io::Result<()> {
    let path = if let Some(path) = std::env::args().nth(1) {
        path
    } else {
        panic!("usage: {} <filename>", std::env::args().nth(0).unwrap())
    };
    let db = &mut build::CompilerDB::new();
    let uri = build::Uri::new(&path);
    let input = Arc::new(std::fs::read_to_string(path)?);
    db.set_input(uri, Arc::clone(&input));

    if let Some(module) = db.best_module(uri).as_ref() {
        println!("{:?}", module);
    }

    let mut failed = false;
    db.with_diagnostics(uri, |diagnostics| {
        for diagnostic in diagnostics {
            failed = true;
            eprintln!("{}\n{}", "-".repeat(50), diagnostic.layout(&input));
        }
    });
    Ok(())
}

fn main() {
    if let Err(error) = run() {
        eprintln!("Error: {:?}", error);
        std::process::exit(1);
    }
}
