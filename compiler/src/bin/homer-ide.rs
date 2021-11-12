use std::error::Error;
use std::sync::Arc;

use log::info;
use lsp_server::{Connection, Message, Response};
use lsp_types::{notification::*, request::*, *};

use homer_compiler::*;

use checker::info::SymbolInfo;

type Result<T> = std::result::Result<T, Box<dyn Error + Sync + Send>>;

fn main() -> Result<()> {
    // Set up logging. Because `stdio_transport` gets a lock on stdout and stdin, we must have
    // our logging only write out to stderr.
    flexi_logger::Logger::with(flexi_logger::LogSpecification::info()).start()?;
    info!("starting generic LSP server");

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = Connection::stdio();

    let text_document_sync = Some(TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
        open_close: Some(true),
        change: Some(TextDocumentSyncKind::FULL),
        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
            include_text: Some(true),
        })),
        ..TextDocumentSyncOptions::default()
    }));
    let code_lens_provider = Some(CodeLensOptions { resolve_provider: Some(true) });
    let execute_command_provider = Some(ExecuteCommandOptions {
        commands: vec![String::from("run_fn")],
        ..ExecuteCommandOptions::default()
    });
    let hover_provider = Some(HoverProviderCapability::Simple(true));
    let definition_provider = Some(OneOf::Left(true));
    let server_capabilities = ServerCapabilities {
        text_document_sync,
        code_lens_provider,
        execute_command_provider,
        hover_provider,
        definition_provider,
        ..ServerCapabilities::default()
    };
    let initialize_params_json =
        connection.initialize(serde_json::to_value(server_capabilities)?)?;
    let _initialize_params: InitializeParams = serde_json::from_value(initialize_params_json)?;
    let db = build::CompilerDB::new();
    let mut server = Server::new(connection, db);
    server.run()?;
    io_threads.join()?;

    // Shut down gracefully.
    info!("shutting down server");
    Ok(())
}

struct Server {
    connection: Connection,
    db: build::CompilerDB,
}

impl Server {
    fn new(connection: Connection, db: build::CompilerDB) -> Self {
        Self { connection, db }
    }

    fn run(&mut self) -> Result<()> {
        // NOTE(MH): This is a hack to allow us to borrow `self` mutably
        // hereafter.
        let receiver =
            std::mem::replace(&mut self.connection.receiver, crossbeam::channel::never());
        for msg in &receiver {
            info!("Received message: {:?}", msg);
            match msg {
                Message::Request(request) => {
                    if self.connection.handle_shutdown(&request)? {
                        return Ok(());
                    }
                    self.handle_request(request)?;
                }
                Message::Response(_response) => info!("Received unhandled response"),
                Message::Notification(notification) => self.handle_notification(notification)?,
            }
        }
        Ok(())
    }

    fn did_open(&mut self, params: DidOpenTextDocumentParams) -> Result<()> {
        let TextDocumentItem { uri, text, .. } = params.text_document;
        self.validate_document(uri, text, true)
    }

    fn did_change(&mut self, params: DidChangeTextDocumentParams) -> Result<()> {
        let uri = params.text_document.uri;
        let text = params.content_changes.into_iter().last().unwrap().text;
        self.validate_document(uri, text, true)
    }

    fn did_save(&mut self, params: DidSaveTextDocumentParams) -> Result<()> {
        let uri = params.text_document.uri;
        if let Some(text) = params.text {
            self.validate_document(uri, text, true)
        } else {
            info!("got save notification without text for {}", uri);
            Ok(())
        }
    }

    fn did_close(&mut self, params: DidCloseTextDocumentParams) -> Result<()> {
        let uri = params.text_document.uri;
        let params = PublishDiagnosticsParams { uri, diagnostics: vec![], version: None };
        let notification = lsp_server::Notification::new(
            PublishDiagnostics::METHOD.to_owned(),
            serde_json::to_value(params)?,
        );
        self.connection.sender.send(Message::from(notification))?;
        Ok(())
    }

    fn validate_document(&mut self, lsp_uri: Url, input: String, print_module: bool) -> Result<()> {
        let uri = build::Uri::new(lsp_uri.as_str());
        info!("Received text for {:?}", uri);
        self.db.set_input(uri, Arc::new(input));

        let diagnostics: Vec<_> = self.db.with_diagnostics(uri, |diagnostics| {
            diagnostics.map(homer_compiler::diagnostic::Diagnostic::to_lsp).collect()
        });
        info!("Sending {} diagnostics", diagnostics.len());
        let params = PublishDiagnosticsParams { uri: lsp_uri, diagnostics, version: None };
        let notification = lsp_server::Notification::new(
            PublishDiagnostics::METHOD.to_owned(),
            serde_json::to_value(params)?,
        );
        self.connection.sender.send(Message::from(notification))?;

        if print_module {
            if let Some(module) = self.db.checked_module(uri) {
                info!("{:?}", module);
            }
        }
        Ok(())
    }

    fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let response = self.find_symbol(&params.text_document_position_params).map(|symbol| {
            let info = match &symbol {
                SymbolInfo::ExprBinder { typ, .. } | SymbolInfo::ExprVar { typ, .. } => {
                    format!("{}", typ)
                }
                SymbolInfo::FuncRef { def, .. } => format!("{}", def),
            };
            let range = Some(symbol.span().to_lsp());
            let contents = HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!("```homer\n{}\n```", info),
            });
            Hover { contents, range }
        });
        Ok(response)
    }

    fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let response = self
            .find_symbol(&params.text_document_position_params)
            .as_ref()
            .and_then(SymbolInfo::definition_span)
            .map(|span| {
                GotoDefinitionResponse::Scalar(Location {
                    uri: params.text_document_position_params.text_document.uri,
                    range: span.to_lsp(),
                })
            });
        Ok(response)
    }

    fn code_lenses(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        let lsp_uri = params.text_document.uri;
        info!("got code lens request for uri {:?}", lsp_uri);
        let uri = build::Uri::new(lsp_uri.as_str());
        let lenses = if let Some(module) = self.db.checked_module(uri) {
            let mut lenses = Vec::new();
            for decl in module.func_decls() {
                if decl.expr_params.is_empty() {
                    let range = decl.name.span.to_lsp();
                    let arg = serde_json::to_value(RunFnParams {
                        uri: uri.as_str().to_owned(),
                        fun: decl.name.locatee.as_str().to_owned(),
                    })?;
                    let command = Some(Command {
                        title: String::from("▶︎ Run Function"),
                        command: String::from("run_fn"),
                        arguments: Some(vec![arg]),
                    });
                    lenses.push(CodeLens { range, command, data: None });
                }
            }
            Some(lenses)
        } else {
            None
        };
        Ok(lenses)
    }

    fn execute_command(&self, params: ExecuteCommandParams) -> Result<Option<serde_json::Value>> {
        let arguments = params.arguments;
        assert_eq!(arguments.len(), 1);
        let argument = arguments.into_iter().next().unwrap();
        let args: RunFnParams = serde_json::from_value(argument)?;
        let message_params = if let Some(module) = self.db.anf_module(build::Uri::new(&args.uri)) {
            let machine = cek::Machine::new(&module, syntax::ExprVar::new(&args.fun));
            let result = machine.run();
            let message = format!("{}() = {}", args.fun, result.value());
            ShowMessageParams { typ: MessageType::INFO, message }
        } else {
            ShowMessageParams {
                typ: MessageType::ERROR,
                message: String::from("The module cannot be compiled."),
            }
        };
        let notification = lsp_server::Notification::new(
            ShowMessage::METHOD.to_owned(),
            serde_json::to_value(&message_params)?,
        );
        if let Err(error) = self.connection.sender.send(Message::from(notification)) {
            info!("Failed to send message: {:?}", error);
        }
        Ok(None)
    }

    fn find_symbol(&self, position_params: &TextDocumentPositionParams) -> Option<SymbolInfo> {
        let uri = build::Uri::new(position_params.text_document.uri.as_str());
        let symbols = self.db.symbols(uri);

        let loc = location::SourceLocation::from_lsp(position_params.position);
        // FIXME(MH): We should do a binary search here.
        symbols.iter().find_map(|symbol| {
            if symbol.span().contains(loc) {
                Some(symbol.clone())
            } else {
                None
            }
        })
    }

    fn handle_request(&self, request: lsp_server::Request) -> Result<()> {
        match request.method.as_ref() {
            HoverRequest::METHOD => self.dispatch_request::<HoverRequest>(request, &Self::hover),
            GotoDefinition::METHOD => {
                self.dispatch_request::<GotoDefinition>(request, &Self::goto_definition)
            }
            CodeLensRequest::METHOD => {
                self.dispatch_request::<CodeLensRequest>(request, &Self::code_lenses)
            }
            ExecuteCommand::METHOD => {
                self.dispatch_request::<ExecuteCommand>(request, &Self::execute_command)
            }
            other => {
                info!("Received unhandled request: {:?}", other);
                Ok(())
            }
        }
    }

    fn handle_notification(&mut self, notification: lsp_server::Notification) -> Result<()> {
        match notification.method.as_ref() {
            DidOpenTextDocument::METHOD => {
                self.dispatch_notification::<DidOpenTextDocument>(notification, &Self::did_open)
            }
            DidChangeTextDocument::METHOD => {
                self.dispatch_notification::<DidChangeTextDocument>(notification, &Self::did_change)
            }
            DidSaveTextDocument::METHOD => {
                self.dispatch_notification::<DidSaveTextDocument>(notification, &Self::did_save)
            }
            DidCloseTextDocument::METHOD => {
                self.dispatch_notification::<DidCloseTextDocument>(notification, &Self::did_close)
            }
            other => {
                info!("Received unhandled notification: {:?}", other);
                Ok(())
            }
        }
    }

    fn dispatch_request<R: Request>(
        &self,
        request: lsp_server::Request,
        f: &dyn Fn(&Self, R::Params) -> Result<R::Result>,
    ) -> Result<()> {
        let (id, params) = request.extract(R::METHOD).unwrap();
        let result = f(self, params)?;
        let result = serde_json::to_value(result)?;
        let response = Response::new_ok(id, result);
        self.connection.sender.send(Message::from(response))?;
        Ok(())
    }

    fn dispatch_notification<N: Notification>(
        &mut self,
        notification: lsp_server::Notification,
        f: &dyn Fn(&mut Self, N::Params) -> Result<()>,
    ) -> Result<()> {
        let params = notification.extract(N::METHOD).unwrap();
        f(self, params)
    }
}

#[derive(serde::Deserialize, serde::Serialize)]
struct RunFnParams {
    uri: String,
    fun: String,
}
