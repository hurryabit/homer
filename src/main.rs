use std::sync::Arc;

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
    Check { file: String },
    Run { file: String },
    Server,
}

fn check_and_run(file: String, run: bool) -> anyhow::Result<bool> {
    let db = &mut build::CompilerDB::new();
    let uri = build::Uri::new(&file);
    let input = Arc::new(std::fs::read_to_string(&file)?);
    db.set_input(uri, Arc::clone(&input));

    let mut success = true;
    db.with_diagnostics(uri, |diagnostics| {
        for diagnostic in diagnostics {
            success = false;
            eprintln!("{}\n{}", "-".repeat(50), diagnostic.layout(&input));
        }
    });

    if run && let Some(module) = db.anf_module(uri) {
        let main = syntax::ExprVar::new("main");
        if module.func_decls.iter().any(|decl| decl.name == main) {
            let machine = cek::Machine::new(&module, main);
            let result = machine.run();
            println!("Result: {}", result.value());
        } else {
            eprintln!("No function `main` to run.");
        }
    }
    Ok(success)
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
        Command::Check { file } => check_and_run(file, false),
        Command::Run { file } => check_and_run(file, true),
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
