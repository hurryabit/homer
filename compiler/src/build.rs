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

    fn parsed_module(&self, uri: Uri) -> (Arc<Option<Module>>, Arc<Vec<Diagnostic>>);

    fn checked_module(&self, uri: Uri) -> (Arc<Option<Module>>, Arc<Vec<Diagnostic>>);
}

fn humanizer(db: &dyn Compiler, uri: Uri) -> Arc<Humanizer> {
    let input = db.input(uri);
    Arc::new(Humanizer::new(&input))
}

fn parsed_module(db: &dyn Compiler, uri: Uri) -> (Arc<Option<Module>>, Arc<Vec<Diagnostic>>) {
    let input = db.input(uri);
    let humanizer = db.humanizer(uri);
    let (opt_parsed_module, diagnostics) = Module::parse(&input, &humanizer);
    (Arc::new(opt_parsed_module), Arc::new(diagnostics))
}

fn checked_module(db: &dyn Compiler, uri: Uri) -> (Arc<Option<Module>>, Arc<Vec<Diagnostic>>) {
    let humanizer = db.humanizer(uri);
    let (opt_checked_module, diagnostics) = if let Some(parsed_module) = &*db.parsed_module(uri).0 {
        match parsed_module.check(&humanizer) {
            Ok(checked_module) => (Some(checked_module), vec![]),
            Err(diagnostic) => (None, vec![diagnostic]),
        }
    } else {
        (None, vec![])
    };
    (Arc::new(opt_checked_module), Arc::new(diagnostics))
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

    pub fn best_module(&self, uri: Uri) -> Arc<Option<Module>> {
        let opt_checked_module = self.checked_module(uri).0;
        if opt_checked_module.is_some() {
            opt_checked_module
        } else {
            self.parsed_module(uri).0
        }
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
