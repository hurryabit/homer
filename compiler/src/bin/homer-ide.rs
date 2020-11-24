use std::error::Error;
use std::sync::Arc;

use log::info;
use lsp_types::{
    notification::{
        DidChangeTextDocument, DidOpenTextDocument, DidSaveTextDocument, Notification,
        PublishDiagnostics, ShowMessage,
    },
    request::{CodeLensRequest, ExecuteCommand, Request},
    CodeLens, CodeLensOptions, Command, ExecuteCommandOptions, InitializeParams, MessageType,
    PublishDiagnosticsParams, SaveOptions, ServerCapabilities, ShowMessageParams, TextDocumentItem,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    TextDocumentSyncSaveOptions, Url,
};

use lsp_server::{Connection, Message, RequestId, Response};

use homer_compiler::*;

#[derive(serde::Deserialize, serde::Serialize)]
struct RunFnParams {
    uri: String,
    fun: String,
}

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    // Set up logging. Because `stdio_transport` gets a lock on stdout and stdin, we must have
    // our logging only write out to stderr.
    flexi_logger::Logger::with_str("info").start().unwrap();
    info!("starting generic LSP server");

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = Connection::stdio();

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let text_document_sync = Some(TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
        open_close: Some(true),
        change: Some(TextDocumentSyncKind::Full),
        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
            include_text: Some(true),
        })),
        ..TextDocumentSyncOptions::default()
    }));
    let code_lens_provider = Some(CodeLensOptions { resolve_provider: Some(true) });
    let execute_command_provider = Some(ExecuteCommandOptions {
        commands: vec!["run_fn".to_string()],
        ..ExecuteCommandOptions::default()
    });
    let server_capabilities = ServerCapabilities {
        text_document_sync,
        code_lens_provider,
        execute_command_provider,
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
    for msg in &connection.receiver {
        // info!("got msg: {:?}", msg);
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                info!("got request: {:?}", req);
                match req.method.as_ref() {
                    CodeLensRequest::METHOD => {
                        let (id, params) = cast_request::<CodeLensRequest>(req);
                        let lsp_uri = params.text_document.uri;
                        info!("got code lens request for uri {:?} with id {:?}", lsp_uri, id);
                        let result = code_lenses(lsp_uri, db);
                        let result = serde_json::to_value(&result).unwrap();
                        let resp = Response { id, result: Some(result), error: None };
                        connection.sender.send(Message::Response(resp))?;
                    }
                    ExecuteCommand::METHOD => {
                        let (id, params) = cast_request::<ExecuteCommand>(req);
                        info!(
                            "got command execution request with params {:?} and id {:?}",
                            params, id
                        );
                        let result = run_function(params.arguments, db);
                        let result = serde_json::to_value(&result).unwrap();
                        let not = lsp_server::Notification {
                            method: ShowMessage::METHOD.to_owned(),
                            params: result,
                        };
                        connection.sender.send(Message::Notification(not))?;
                    }
                    _ => {
                        info!("got unhandled request: {:?}", req);
                    }
                }
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
    db.set_input(uri, Arc::new(input));
    if print_module {
        if let Some(module) = db.anf_module(uri) {
            info!("{:?}", module);
        }
    }

    let diagnostics: Vec<_> = db.with_diagnostics(uri, |diagnostics| {
        diagnostics.map(homer_compiler::diagnostic::Diagnostic::to_lsp).collect()
    });
    info!("Sending {} diagnostics", diagnostics.len());
    let params = PublishDiagnosticsParams { uri: lsp_uri, diagnostics, version: None };
    let not = lsp_server::Notification::new(
        PublishDiagnostics::METHOD.to_string(),
        serde_json::to_value(params).unwrap(),
    );
    connection.sender.send(Message::from(not))?;

    Ok(())
}

fn code_lenses(lsp_uri: Url, db: &mut build::CompilerDB) -> Option<Vec<CodeLens>> {
    let uri = build::Uri::new(lsp_uri.as_str());
    let humanizer = db.humanizer(uri);
    db.checked_module(uri).map(|module| {
        module
            .func_decls()
            .filter_map(|decl| {
                if decl.expr_params.is_empty() {
                    let range = decl.name.span.humanize(&humanizer).to_lsp();
                    let arg = serde_json::to_value(RunFnParams {
                        uri: uri.as_str().to_string(),
                        fun: decl.name.locatee.as_str().to_string(),
                    })
                    .unwrap();
                    let command = Some(Command {
                        title: "▶︎ Run Function".to_string(),
                        command: "run_fn".to_string(),
                        arguments: Some(vec![arg]),
                    });
                    let code_lens = CodeLens { range, command, data: None };
                    Some(code_lens)
                } else {
                    None
                }
            })
            .collect()
    })
}

fn run_function(
    arguments: Vec<serde_json::Value>,
    db: &mut build::CompilerDB,
) -> ShowMessageParams {
    assert_eq!(arguments.len(), 1);
    let argument = arguments.into_iter().next().unwrap();
    let args: RunFnParams = serde_json::from_value(argument).unwrap();
    if let Some(module) = db.anf_module(build::Uri::new(&args.uri)) {
        let machine = cek::Machine::new(&module, syntax::ExprVar::new(&args.fun));
        let result = machine.run();
        let message = format!("{}() = {}", args.fun, result.value());
        ShowMessageParams { typ: MessageType::Info, message }
    } else {
        ShowMessageParams {
            typ: MessageType::Error,
            message: "The module cannot be compiled.".to_string(),
        }
    }
}

fn cast_notification<N>(not: lsp_server::Notification) -> N::Params
where
    N: Notification,
    N::Params: serde::de::DeserializeOwned,
{
    not.extract(N::METHOD).unwrap()
}

fn cast_request<R>(req: lsp_server::Request) -> (RequestId, R::Params)
where
    R: Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD).unwrap()
}
