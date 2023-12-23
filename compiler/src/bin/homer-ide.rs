use std::sync::Arc;

use log::info;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use homer_compiler::{build, cek, checker, location, syntax};

use checker::SymbolInfo;

// #[derive(Debug)]
struct Backend {
    client: Client,
    db: Mutex<build::CompilerDB>,
}

#[derive(serde::Deserialize, serde::Serialize)]
struct RunFnParams {
    uri: String,
    fun: String,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        let text_document_sync =
            Some(TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
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

        Ok(InitializeResult { capabilities: server_capabilities, ..Default::default() })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client.log_message(MessageType::INFO, "server initialized!").await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let TextDocumentItem { uri, text, .. } = params.text_document;
        self.validate_document(uri, text, true).await
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.content_changes.into_iter().last().unwrap().text;
        self.validate_document(uri, text, true).await
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(text) = params.text {
            self.validate_document(uri, text, true).await
        } else {
            info!("got save notification without text for {}", uri);
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        self.client.publish_diagnostics(uri, vec![], None).await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let response =
            self.find_symbol(&params.text_document_position_params).await.map(|symbol| {
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

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let response = self
            .find_symbol(&params.text_document_position_params)
            .await
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

    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        let db = self.db.lock().await;
        let lsp_uri = params.text_document.uri;
        info!("got code lens request for uri {:?}", lsp_uri);
        let uri = build::Uri::new(lsp_uri.as_str());
        let lenses = if let Some(module) = db.checked_module(uri) {
            let mut lenses = Vec::new();
            for decl in module.func_decls() {
                if decl.expr_params.is_empty() {
                    let range = decl.name.span.to_lsp();
                    let arg = serde_json::to_value(RunFnParams {
                        uri: uri.as_str().to_owned(),
                        fun: decl.name.locatee.as_str().to_owned(),
                    })
                    .unwrap();
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

    async fn execute_command(
        &self,
        params: ExecuteCommandParams,
    ) -> Result<Option<serde_json::Value>> {
        let arguments = params.arguments;
        assert_eq!(arguments.len(), 1);
        let argument = arguments.into_iter().next().unwrap();
        let args: RunFnParams = serde_json::from_value(argument).unwrap();
        let db = self.db.lock().await;
        if let Some(module) = db.anf_module(build::Uri::new(&args.uri)) {
            let machine = cek::Machine::new(&module, syntax::ExprVar::new(&args.fun));
            let result = machine.run();
            let message = format!("{}() = {}", args.fun, result.value());
            self.client.show_message(MessageType::INFO, message).await;
        } else {
            self.client.show_message(MessageType::ERROR, "The module cannot be compiled.").await;
        };
        Ok(None)
    }
}

impl Backend {
    async fn validate_document(&self, lsp_uri: Url, input: String, print_module: bool) {
        let mut db = self.db.lock().await;
        let uri = build::Uri::new(lsp_uri.as_str());
        info!("Received text for {:?}", uri);
        db.set_input(uri, Arc::new(input));

        let diagnostics: Vec<_> = db.with_diagnostics(uri, |diagnostics| {
            diagnostics.map(homer_compiler::diagnostic::Diagnostic::to_lsp).collect()
        });
        info!("Sending {} diagnostics", diagnostics.len());
        self.client.publish_diagnostics(lsp_uri, diagnostics, None).await;

        if print_module {
            if let Some(module) = db.checked_module(uri) {
                info!("{:?}", module);
            }
        }
    }

    async fn find_symbol(
        &self,
        position_params: &TextDocumentPositionParams,
    ) -> Option<SymbolInfo> {
        let db = self.db.lock().await;
        let uri = build::Uri::new(position_params.text_document.uri.as_str());
        let symbols = db.symbols(uri);

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
}

#[tokio::main]
async fn main() {
    flexi_logger::Logger::with(flexi_logger::LogSpecification::info()).start().unwrap();
    info!("starting server");

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let db = Mutex::new(build::CompilerDB::new());

    let (service, socket) = LspService::new(|client| Backend { client, db });
    Server::new(stdin, stdout, socket).serve(service).await;

    info!("shutting down server");
}
