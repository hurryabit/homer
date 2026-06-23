use build::Compiler;
use clap::Parser;
use homer_compiler::{build, cek, syntax};
use std::sync::Arc;

#[derive(Parser)]
struct Args {
    file: String,
    #[arg(long)]
    check_only: bool,
}

fn run(args: Args) -> std::io::Result<bool> {
    let db = &mut build::CompilerDB::new();
    let uri = build::Uri::new(&args.file);
    let input = Arc::new(std::fs::read_to_string(&args.file)?);
    db.set_input(uri, Arc::clone(&input));

    let mut success = true;
    db.with_diagnostics(uri, |diagnostics| {
        for diagnostic in diagnostics {
            success = false;
            eprintln!("{}\n{}", "-".repeat(50), diagnostic.layout(&input));
        }
    });

    if !args.check_only
        && let Some(module) = db.anf_module(uri)
    {
        let main = syntax::ExprVar::new("main");
        if module.func_decls.iter().any(|decl| decl.name == main) {
            let machine = cek::Machine::new(&module, main);
            let result = machine.run();
            println!("Result: {}", result.value());
        } else {
            println!("No function `main` to run.");
        }
    }
    Ok(success)
}

fn main() {
    let args = Args::parse();
    match run(args) {
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
