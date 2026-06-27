use std::fmt;
use std::sync::Arc;

use crate::anf;
use crate::checker::SymbolInfo;
use crate::diagnostic::Diagnostic;
use crate::syntax;

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

type CheckedModuleOutcome =
    (Option<Arc<syntax::Module>>, Arc<Vec<SymbolInfo>>, Arc<Vec<Diagnostic>>);

#[salsa::query_group(CompilerStorage)]
pub trait Compiler: salsa::Database {
    #[salsa::input]
    fn input(&self, uri: Uri) -> Arc<String>;

    fn parsed_module(&self, uri: Uri) -> (Option<Arc<syntax::Module>>, Arc<Vec<Diagnostic>>);

    fn checked_module(&self, uri: Uri) -> CheckedModuleOutcome;

    fn anf_module(&self, uri: Uri) -> Option<Arc<anf::Module>>;
}

fn parsed_module(
    db: &dyn Compiler,
    uri: Uri,
) -> (Option<Arc<syntax::Module>>, Arc<Vec<Diagnostic>>) {
    let input = db.input(uri);
    let (opt_parsed_module, diagnostics) = syntax::Module::parse(&input);
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
        Self { storage: salsa::Storage::default() }
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

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::build::*;

    fn build_output(input: &str) -> String {
        use std::fmt::Write as _;
        let db = &mut CompilerDB::new();
        let uri = Uri::new("foo.homer");
        db.set_input(uri, Arc::new(input.to_owned()));

        let mut output = String::new();
        match db.anf_module(uri) {
            None => writeln!(output, "NONE").unwrap(),
            Some(module) => writeln!(output, "{:?}", module).unwrap(),
        }
        db.with_diagnostics(uri, |diagnostics| {
            for diagnostic in diagnostics {
                write!(output, "{}\n{}\n", "-".repeat(50), diagnostic.layout(input)).unwrap();
            }
        });
        output
    }

    #[test]
    fn all_good() {
        insta::assert_snapshot!(build_output(r#"
        fn f(x: Int) -> Int { x }
        "#), @r###"
        MODULE
            decl: FUNCDECL
                name: f
                param: x
                body: EXPR
                    binder: $result
                    bindee: x/1
        "###);
    }

    #[test]
    fn fatal_parse_error() {
        insta::assert_snapshot!(build_output(r#"
        fn f(x: Int) -> Int { x
        "#), @r#"
        NONE
        --------------------------------------------------
          2 |         fn f(x: Int) -> Int { x
                                             ~
        Unrecognized EOF found at 2:32
        Expected one of "!=", "(", "*", "+", "-", ".", "/", "<", "<=", "==", ">", ">=", "@" or "}"
        "#);
    }

    #[test]
    fn recoverable_parse_error_types_good() {
        insta::assert_snapshot!(build_output(r#"
        fn f(x: Int) -> Int { + }
        "#), @r#"
        MODULE
            decl: FUNCDECL
                name: f
                param: x
                body: EXPR
                    binder: $v1
                    bindee: ERROR
                    binder: $v2
                    bindee: ERROR
                    binder: $result
                    bindee: BINOP
                        lhs: $v1/2
                        op: ADD
                        rhs: $v2/1
        --------------------------------------------------
          2 |         fn f(x: Int) -> Int { + }
                                            ~
        Unrecognized token `+` found at 2:31:2:32
        Expected one of NUMBER, ID_UPPER, ID_LOWER, "(", "false", "fn", "if", "let", "match", "true" or "{"
        --------------------------------------------------
          2 |         fn f(x: Int) -> Int { + }
                                              ~
        Unrecognized token `}` found at 2:33:2:34
        Expected one of NUMBER, ID_UPPER, ID_LOWER, "(", "false", "true" or "{"
        "#);
    }

    #[test]
    fn recoverable_parse_error_types_bad() {
        insta::assert_snapshot!(build_output(r#"
        fn f(x: Int) -> Int { true + }
        "#), @r#"
        NONE
        --------------------------------------------------
          2 |         fn f(x: Int) -> Int { true + }
                                                   ~
        Unrecognized token `}` found at 2:38:2:39
        Expected one of NUMBER, ID_UPPER, ID_LOWER, "(", "false", "true" or "{"
        --------------------------------------------------
          2 |         fn f(x: Int) -> Int { true + }
                                            ~~~~
        Expected an expression of type `Int` but found an expression of type `Bool`.
        "#);
    }
}
