use std::path::PathBuf;
use std::sync::Arc;

use anyhow::anyhow;
use clap::Parser;
use clap::Subcommand;
use homer::build;
use homer::build::Compiler as _;
use homer::cek;
use homer::syntax;
use tower_lsp::LspService;
use tower_lsp::Server;

#[derive(Parser)]
struct Args {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    Check { file: PathBuf },
    Interpret { file: PathBuf },
    Compile { file: PathBuf },
    Server,
}

fn process_file(
    file: PathBuf,
    f: impl FnOnce(PathBuf, &mut build::CompilerDB, build::Uri) -> anyhow::Result<bool>,
) -> anyhow::Result<bool> {
    let uri =
        build::Uri::new(file.to_str().ok_or_else(|| anyhow!("non-UTF-8 paths are not supported"))?);
    let db = &mut build::CompilerDB::new();
    let input = Arc::new(std::fs::read_to_string(&file)?);
    db.set_input(uri, Arc::clone(&input));

    let mut success = true;
    db.with_diagnostics(uri, |diagnostics| {
        for diagnostic in diagnostics {
            success = false;
            eprintln!("{}\n{}", "-".repeat(50), diagnostic.layout(&input));
        }
    });
    if success {
        success = f(file, db, uri)?;
    }
    Ok(success)
}

fn interpret_file(
    _file: PathBuf,
    db: &mut build::CompilerDB,
    uri: build::Uri,
) -> anyhow::Result<bool> {
    let module = db.anf_module(uri).expect("conversion to ANF cannot fail when checks have passed");
    let main = syntax::ExprVar::new("main");
    if module.func_decls.iter().any(|decl| decl.name == main) {
        let machine = cek::Machine::new(&module, main);
        let result = machine.run();
        println!("Result: {}", result.value());
        Ok(true)
    } else {
        eprintln!("No function `main` to run.");
        Ok(false)
    }
}

fn compile_file(
    mut file: PathBuf,
    db: &mut build::CompilerDB,
    uri: build::Uri,
) -> anyhow::Result<bool> {
    assert!(file.set_extension("wasm"));
    let bytes =
        db.wasm_module(uri).expect("complication to WASM cannot fail when checks have passed");
    std::fs::write(file, &*bytes)?;
    Ok(true)
}

async fn language_server() -> anyhow::Result<bool> {
    simple_logger::SimpleLogger::new()
        .env()
        .with_level(log::LevelFilter::Info)
        .with_module_level("salsa", log::LevelFilter::Warn)
        .with_timestamp_format(time::macros::format_description!("[hour]:[minute]:[second]"))
        .init()?;
    log::info!("starting server");

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(homer::LanguageServer::new);
    Server::new(stdin, stdout, socket).serve(service).await;

    log::info!("server shut down");
    Ok(true)
}

#[tokio::main]
async fn main() {
    let args = Args::parse();
    let result = match args.command {
        Command::Check { file } => process_file(file, |_, _, _| Ok(true)),
        Command::Interpret { file } => process_file(file, interpret_file),
        Command::Compile { file } => process_file(file, compile_file),
        Command::Server => language_server().await,
    };
    match result {
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
