use crate::{anf, checker, diagnostic, syntax};
use checker::SymbolInfo;
use diagnostic::Diagnostic;
use std::fmt;
use std::sync::Arc;
use syntax::Module;

#[derive(Copy, Clone, Eq, Hash, PartialEq)]
pub struct Uri(lasso::Spur);

impl Uri {
    pub fn new(uri: &str) -> Self {
        Uri(crate::INTERNER.get_or_intern(uri))
    }

    pub fn as_str(&self) -> &'static str {
        crate::INTERNER.resolve(&self.0)
    }
}

impl fmt::Debug for Uri {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

type CheckedModuleOutcome = (Option<Arc<Module>>, Arc<Vec<SymbolInfo>>, Arc<Vec<Diagnostic>>);

#[salsa::query_group(CompilerStorage)]
trait Compiler: salsa::Database {
    #[salsa::input]
    fn input(&self, uri: Uri) -> Arc<String>;

    fn parsed_module(&self, uri: Uri) -> (Option<Arc<Module>>, Arc<Vec<Diagnostic>>);

    fn checked_module(&self, uri: Uri) -> CheckedModuleOutcome;

    fn anf_module(&self, uri: Uri) -> Option<Arc<anf::Module>>;
}

fn parsed_module(db: &dyn Compiler, uri: Uri) -> (Option<Arc<Module>>, Arc<Vec<Diagnostic>>) {
    let input = db.input(uri);
    let (opt_parsed_module, diagnostics) = Module::parse(&input);
    (opt_parsed_module.map(Arc::new), Arc::new(diagnostics))
}

fn checked_module(db: &dyn Compiler, uri: Uri) -> CheckedModuleOutcome {
    let (opt_checked_module, symbols, diagnostics) = match db.parsed_module(uri).0 {
        None => (None, vec![], vec![]),
        Some(parsed_module) => match parsed_module.check() {
            Err(diagnostic) => (None, vec![], vec![diagnostic]),
            Ok((checked_module, symbols)) => (Some(checked_module), symbols, vec![]),
        },
    };
    (opt_checked_module.map(Arc::new), Arc::new(symbols), Arc::new(diagnostics))
}

fn anf_module(db: &dyn Compiler, uri: Uri) -> Option<Arc<anf::Module>> {
    db.checked_module(uri).0.map(|checked_module| Arc::new(checked_module.to_anf()))
}

#[salsa::database(CompilerStorage)]
pub struct CompilerDB {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for CompilerDB {}

impl CompilerDB {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        // NOTE(MH): We force the initialization of the interner to avoid races.
        crate::INTERNER.len();
        Self { storage: salsa::Storage::default() }
    }

    pub fn set_input(&mut self, uri: Uri, input: Arc<String>) {
        (self as &mut dyn Compiler).set_input(uri, input);
    }

    pub fn checked_module(&self, uri: Uri) -> Option<Arc<Module>> {
        (self as &dyn Compiler).checked_module(uri).0
    }

    pub fn symbols(&self, uri: Uri) -> Arc<Vec<SymbolInfo>> {
        (self as &dyn Compiler).checked_module(uri).1
    }

    pub fn anf_module(&self, uri: Uri) -> Option<Arc<anf::Module>> {
        (self as &dyn Compiler).anf_module(uri)
    }

    pub fn with_diagnostics<R, F>(&self, uri: Uri, f: F) -> R
    where
        F: FnOnce(&mut dyn Iterator<Item = &Diagnostic>) -> R,
    {
        let parser_diagnostics = self.parsed_module(uri).1;
        let checker_diagnostics = (self as &dyn Compiler).checked_module(uri).2;
        f(&mut parser_diagnostics.iter().chain(checker_diagnostics.iter()))
    }
}
