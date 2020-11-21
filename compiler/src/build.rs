use crate::*;
use diagnostic::Diagnostic;
use location::Humanizer;
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

#[salsa::query_group(CompilerStorage)]
trait Compiler: salsa::Database {
    #[salsa::input]
    fn input(&self, uri: Uri) -> Arc<String>;

    fn humanizer(&self, uri: Uri) -> Arc<Humanizer>;

    fn parsed_module(&self, uri: Uri) -> (Option<Arc<Module>>, Arc<Vec<Diagnostic>>);

    fn checked_module(&self, uri: Uri) -> (Option<Arc<Module>>, Arc<Vec<Diagnostic>>);

    fn anf_module(&self, uri: Uri) -> Option<Arc<anf::Module>>;
}

fn humanizer(db: &dyn Compiler, uri: Uri) -> Arc<Humanizer> {
    let input = db.input(uri);
    Arc::new(Humanizer::new(&input))
}

fn parsed_module(db: &dyn Compiler, uri: Uri) -> (Option<Arc<Module>>, Arc<Vec<Diagnostic>>) {
    let input = db.input(uri);
    let humanizer = db.humanizer(uri);
    let (opt_parsed_module, diagnostics) = Module::parse(&input, &humanizer);
    (opt_parsed_module.map(Arc::new), Arc::new(diagnostics))
}

fn checked_module(db: &dyn Compiler, uri: Uri) -> (Option<Arc<Module>>, Arc<Vec<Diagnostic>>) {
    let humanizer = db.humanizer(uri);
    let (opt_checked_module, diagnostics) = match db.parsed_module(uri).0 {
        None => (None, vec![]),
        Some(parsed_module) => match parsed_module.check(&humanizer) {
            Ok(checked_module) => (Some(checked_module), vec![]),
            Err(diagnostic) => (None, vec![diagnostic]),
        },
    };
    (opt_checked_module.map(Arc::new), Arc::new(diagnostics))
}

fn anf_module(db: &dyn Compiler, uri: Uri) -> Option<Arc<anf::Module>> {
    db.checked_module(uri)
        .0
        .map(|checked_module| Arc::new(checked_module.to_anf()))
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
        Self {
            storage: salsa::Storage::default(),
        }
    }

    pub fn set_input(&mut self, uri: Uri, input: Arc<String>) {
        (self as &mut dyn Compiler).set_input(uri, input);
    }

    pub fn anf_module(&self, uri: Uri) -> Option<Arc<anf::Module>> {
        (self as &dyn Compiler).anf_module(uri)
    }

    pub fn with_diagnostics<R, F>(&self, uri: Uri, f: F) -> R
    where
        F: FnOnce(&mut dyn Iterator<Item = &Diagnostic>) -> R,
    {
        let parser_diagnostics = self.parsed_module(uri).1;
        let checker_diagnostics = self.checked_module(uri).1;
        f(&mut parser_diagnostics.iter().chain(checker_diagnostics.iter()))
    }
}
