use crate::location::*;

#[derive(Debug)]
pub enum Severity {
    Warning,
    Error,
}

#[derive(Debug)]
pub enum Source {
    Parser,
    Checker,
}

#[derive(Debug)]
pub struct Diagnostic {
    pub span: Span<HumanLoc>,
    pub severity: Severity,
    pub source: Source,
    pub message: String,
}

impl Diagnostic {
    pub fn to_lsp(self) -> lsp_types::Diagnostic {
        let Self {
            span,
            severity,
            source,
            message,
        } = self;
        use lsp_types::*;
        lsp_types::Diagnostic {
            range: span.to_lsp(),
            severity: Some(severity.to_lsp()),
            source: Some(source.to_lsp()),
            message,
            ..Diagnostic::default()
        }
    }
}

impl HumanLoc {
    fn to_lsp(self) -> lsp_types::Position {
        let Self { line, column } = self;
        lsp_types::Position::new(line, column)
    }
}

impl Span<HumanLoc> {
    fn to_lsp(self) -> lsp_types::Range {
        let Self { start, end } = self;
        lsp_types::Range::new(start.to_lsp(), end.to_lsp())
    }
}

impl Severity {
    fn to_lsp(self) -> lsp_types::DiagnosticSeverity {
        use lsp_types::DiagnosticSeverity;
        match self {
            Severity::Warning => DiagnosticSeverity::Warning,
            Severity::Error => DiagnosticSeverity::Error,
        }
    }
}

impl Source {
    fn to_lsp(self) -> String {
        let str = match self {
            Source::Parser => "parser",
            Source::Checker => "checker",
        };
        str.to_string()
    }
}

impl Diagnostic {
    pub fn layout(&self, input: &str) -> String {
        let Self { span, message, .. } = self;
        if span.start.line == span.end.line {
            let line = input.lines().nth(span.start.line as usize).unwrap();
            format!(
                "{:3} | {}\n{}{}\n{}",
                span.start.line + 1, // NOTE(MH): We're 0-base internal, and 1-based for users.
                line,
                " ".repeat((span.start.column + 6) as usize),
                "~".repeat((span.end.column - span.start.column) as usize),
                message
            )
        } else {
            format!("{}: {}", span, message)
        }
    }
}
