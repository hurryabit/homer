use std::sync::Arc;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use homer_compiler::*;

use checker::info::SymbolInfo;

fn main() {
    tokio::runtime::Builder::new().basic_scheduler().enable_all().build().unwrap().block_on(async {
        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();

        let (service, messages) = LspService::new(|client| {
            let db = Mutex::new(build::CompilerDB::new());
            Backend { client, db }
        });
        Server::new(stdin, stdout).interleave(messages).serve(service).await;
    })
}

struct Backend {
    client: Client,
    db: Mutex<build::CompilerDB>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        let text_document_sync =
            Some(TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::Full),
                save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                    include_text: Some(true),
                })),
                ..TextDocumentSyncOptions::default()
            }));
        let hover_provider = Some(HoverProviderCapability::Simple(true));
        let definition_provider = Some(true);
        let code_lens_provider = Some(CodeLensOptions { resolve_provider: Some(true) });
        let execute_command_provider = Some(ExecuteCommandOptions {
            commands: vec!["run_fn".to_string()],
            ..ExecuteCommandOptions::default()
        });
        let capabilities = ServerCapabilities {
            text_document_sync,
            hover_provider,
            definition_provider,
            code_lens_provider,
            execute_command_provider,
            ..ServerCapabilities::default()
        };
        Ok(InitializeResult { capabilities, ..InitializeResult::default() })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client.log_message(MessageType::Info, "Homer Language Server initialized.").await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let TextDocumentItem { uri, text, .. } = params.text_document;
        self.validate_document(uri, text, true).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.content_changes.into_iter().last().unwrap().text;
        self.validate_document(uri, text, false).await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri;
        match params.text {
            Some(text) => self.validate_document(uri, text, true).await,
            None => {
                self.client
                    .log_message(
                        MessageType::Log,
                        format!("Got save notification without text for {}.", uri),
                    )
                    .await
            }
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let opt_symbol = self.find_symbol(&params.text_document_position_params).await;
        let opt_hover = opt_symbol.map(|symbol| {
            let typ = match &symbol {
                SymbolInfo::ExprBinder { typ, .. } | SymbolInfo::ExprVar { typ, .. } => typ,
            };
            let range = Some(symbol.span().to_lsp());
            let contents = HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!("```homer\n{}\n```", typ),
            });
            Hover { contents, range }
        });
        Ok(opt_hover)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let opt_symbol = self.find_symbol(&params.text_document_position_params).await;
        let opt_response = opt_symbol.as_ref().and_then(SymbolInfo::definition_span).map(|span| {
            GotoDefinitionResponse::Scalar(Location {
                uri: params.text_document_position_params.text_document.uri,
                range: span.to_lsp(),
            })
        });
        Ok(opt_response)
    }

    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        let uri = build::Uri::new(params.text_document.uri.as_str());
        let db = self.db.lock().await;
        let opt_lenses = db.checked_module(uri).map(|module| {
            module
                .func_decls()
                .filter_map(|decl| {
                    if decl.expr_params.is_empty() {
                        let range = decl.name.span.to_lsp();
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
        });
        Ok(opt_lenses)
    }

    async fn execute_command(
        &self,
        params: ExecuteCommandParams,
    ) -> Result<Option<serde_json::Value>> {
        let arguments = params.arguments;
        assert_eq!(arguments.len(), 1);
        let db = self.db.lock().await;
        let argument = arguments.into_iter().next().unwrap();
        let args: RunFnParams = serde_json::from_value(argument).unwrap();
        let (typ, message) = if let Some(module) = db.anf_module(build::Uri::new(&args.uri)) {
            let machine = cek::Machine::new(&module, syntax::ExprVar::new(&args.fun));
            let result = machine.run();
            let message = format!("{}() = {}", args.fun, result.value());
            (MessageType::Info, message)
        } else {
            (MessageType::Error, "The module cannot be compiled.".to_string())
        };
        self.client.show_message(typ, message).await;
        Ok(None)
    }
}

#[derive(serde::Deserialize, serde::Serialize)]
struct RunFnParams {
    uri: String,
    fun: String,
}

impl Backend {
    async fn validate_document(&self, lsp_uri: Url, input: String, print_module: bool) {
        self.client.log_message(MessageType::Log, format!("Received text for {}.", lsp_uri)).await;
        let uri = build::Uri::new(lsp_uri.as_str());
        let mut db = self.db.lock().await;
        db.set_input(uri, Arc::new(input));

        let diagnostics: Vec<_> = db.with_diagnostics(uri, |diagnostics| {
            diagnostics.map(homer_compiler::diagnostic::Diagnostic::to_lsp).collect()
        });
        self.client
            .log_message(MessageType::Log, format!("Sending {} diagnostics.", diagnostics.len()))
            .await;
        self.client.publish_diagnostics(lsp_uri, diagnostics, None).await;

        let opt_module = if print_module {
            db.anf_module(uri).map(|module| format!("{:?}", module))
        } else {
            None
        };
        if let Some(module) = opt_module {
            self.client.log_message(MessageType::Log, module).await;
        }
    }

    async fn find_symbol(
        &self,
        position_params: &TextDocumentPositionParams,
    ) -> Option<SymbolInfo> {
        let uri = build::Uri::new(position_params.text_document.uri.as_str());
        let db = self.db.lock().await;
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
