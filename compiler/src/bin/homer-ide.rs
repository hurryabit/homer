use std::error::Error;

use log::info;
use lsp_types::{
    notification::{
        DidChangeTextDocument, DidOpenTextDocument, DidSaveTextDocument, Notification,
        PublishDiagnostics,
    },
    request::GotoDefinition,
    GotoDefinitionResponse, InitializeParams, PublishDiagnosticsParams, SaveOptions,
    ServerCapabilities, TextDocumentItem, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextDocumentSyncOptions, TextDocumentSyncSaveOptions, Url,
};

use lsp_server::{Connection, Message, Request, RequestId, Response};

use homer_compiler::build;

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    // Set up logging. Because `stdio_transport` gets a lock on stdout and stdin, we must have
    // our logging only write out to stderr.
    flexi_logger::Logger::with_str("info").start().unwrap();
    info!("starting generic LSP server");

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = Connection::stdio();

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let text_document_sync = Some(TextDocumentSyncCapability::Options(
        TextDocumentSyncOptions {
            open_close: Some(true),
            change: Some(TextDocumentSyncKind::Full),
            save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                include_text: Some(true),
            })),
            ..TextDocumentSyncOptions::default()
        },
    ));
    let server_capabilities = ServerCapabilities {
        text_document_sync,
        ..ServerCapabilities::default()
    };
    let initialization_params =
        connection.initialize(serde_json::to_value(server_capabilities).unwrap())?;
    let db = &mut build::CompilerDB::new();
    main_loop(&connection, initialization_params, db)?;
    io_threads.join()?;

    // Shut down gracefully.
    info!("shutting down server");
    Ok(())
}

fn main_loop(
    connection: &Connection,
    params: serde_json::Value,
    db: &mut build::CompilerDB,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    // info!("_params = {:?}", _params);
    // info!("starting example main loop");
    for msg in &connection.receiver {
        // info!("got msg: {:?}", msg);
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                info!("got request: {:?}", req);
                if let Ok((id, params)) = cast::<GotoDefinition>(req) {
                    info!("got gotoDefinition request #{}: {:?}", id, params);
                    let result = Some(GotoDefinitionResponse::Array(Vec::new()));
                    let result = serde_json::to_value(&result).unwrap();
                    let resp = Response {
                        id,
                        result: Some(result),
                        error: None,
                    };
                    connection.sender.send(Message::Response(resp))?;
                    continue;
                }
                // ...
            }
            Message::Response(resp) => {
                info!("got unhandled response {:?}", resp);
            }
            Message::Notification(not) => {
                match not.method.as_ref() {
                    DidOpenTextDocument::METHOD => {
                        let params = cast_notification::<DidOpenTextDocument>(not);
                        let TextDocumentItem { uri, text, .. } = params.text_document;
                        validate_document(connection, uri, text, true, db)?;
                    }
                    DidChangeTextDocument::METHOD => {
                        let params = cast_notification::<DidChangeTextDocument>(not);
                        let uri = params.text_document.uri;
                        let text = params.content_changes.into_iter().last().unwrap().text;
                        validate_document(connection, uri, text, false, db)?;
                    }
                    DidSaveTextDocument::METHOD => {
                        let params = cast_notification::<DidSaveTextDocument>(not);
                        let uri = params.text_document.uri;
                        match params.text {
                            Some(text) => {
                                validate_document(connection, uri, text, true, db)?;
                            }
                            None => {
                                info!("got save notification without text for {}", uri);
                            }
                        }
                    }
                    _ => {
                        info!("got unhandled notification: {:?}", not);
                    }
                };
            }
        }
    }
    Ok(())
}

fn validate_document(
    connection: &Connection,
    lsp_uri: Url,
    input: String,
    print_module: bool,
    db: &mut build::CompilerDB,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let uri = build::Uri::new(lsp_uri.as_str());
    info!("Received text for {:?}", uri);
    db.set_input(uri, input);
    let opt_module = db.best_module(uri);
    if print_module {
        if let Some(module) = opt_module.as_ref() {
            info!("{:?}", module);
        }
    }

    let diagnostics: Vec<_> = db.with_diagnostics(uri, |diagnostics| {
        diagnostics
            .map(homer_compiler::diagnostic::Diagnostic::to_lsp)
            .collect()
    });
    info!("Sending {} diagnostics", diagnostics.len());
    let params = PublishDiagnosticsParams {
        uri: lsp_uri,
        diagnostics,
        version: None,
    };
    let not = lsp_server::Notification::new(
        PublishDiagnostics::METHOD.to_string(),
        serde_json::to_value(params).unwrap(),
    );
    connection.sender.send(Message::from(not))?;

    Ok(())
}

fn cast_notification<N>(not: lsp_server::Notification) -> N::Params
where
    N: Notification,
    N::Params: serde::de::DeserializeOwned,
{
    not.extract(N::METHOD).unwrap()
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), Request>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

// impl From<std::io::Error> for Error {
//     fn from(err: std::io::Error) -> Self {
//         Self::Io(err)
//     }
// }

// impl From<lalrpop_util::ParseError<usize, parser::Token<'_>, &'static str>> for Error {
//     fn from(err: lalrpop_util::ParseError<usize, parser::Token<'_>, &'static str>) -> Self {
//         Self::Parse(err.map_token(|t| format!("{}", t)))
//     }
// }

// fn main() -> Result<(), Error> {
//     let path = if let Some(path) = std::env::args().nth(1) {
//         path
//     } else {
//         panic!("usage: {} <filename>", std::env::args().nth(0).unwrap())
//     };
//     let input = std::fs::read_to_string(path)?;
//     let parser = parser::ModuleParser::new();
//     let ast = parser.parse(&input)?;
//     serde_yaml::to_writer(std::io::stdout(), &ast)?;
//     Ok(())
// }
