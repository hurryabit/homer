use crate::diagnostic::*;
use crate::grammar::*;
use crate::location::{Humanizer, ParserLoc, Span};
use crate::syntax::*;

impl Module {
    pub fn parse(input: &str, humanizer: &Humanizer) -> (Option<Self>, Vec<Diagnostic>) {
        parse_impl(humanizer, |errors| ModuleParser::new().parse(errors, input))
    }
}

type ParseError<'a, Loc> = lalrpop_util::ParseError<Loc, Token<'a>, &'static str>;

pub fn parse_impl<'a, T, F>(humanizer: &Humanizer, f: F) -> (Option<T>, Vec<Diagnostic>)
where
    F: FnOnce(&mut Vec<ParseError<'a, ParserLoc>>) -> Result<T, ParseError<'a, usize>>,
{
    let mut errors = Vec::new();
    let result = f(&mut errors);
    match result {
        Ok(module) => {
            let diagnostics = errors
                .into_iter()
                .map(|error| parse_error_to_diagnostic(error, humanizer))
                .collect::<Vec<_>>();
            (Some(module), diagnostics)
        }
        Err(fatal_error) => {
            let error = errors
                .into_iter()
                .next()
                .unwrap_or_else(|| fatal_error.map_location(ParserLoc::from_usize));
            let diagnostics = vec![parse_error_to_diagnostic(error, humanizer)];
            (None, diagnostics)
        }
    }
}

fn parse_error_to_diagnostic<'a>(
    error: ParseError<'a, ParserLoc>,
    humanizer: &Humanizer,
) -> Diagnostic {
    let error = error.map_location(|loc| loc.humanize(humanizer));
    let span = match error {
        ParseError::InvalidToken { location } | ParseError::UnrecognizedEOF { location, .. } => {
            Span::new(location, location)
        }
        ParseError::UnrecognizedToken {
            token: (start, _, end),
            ..
        }
        | ParseError::ExtraToken {
            token: (start, _, end),
        } => Span::new(start, end),
        ParseError::User { .. } => Span::default(),
    };
    Diagnostic {
        span,
        severity: Severity::Error,
        source: Source::Parser,
        message: format!("{}", error),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl Module {
        pub(crate) fn parse_test(input: &str) -> (Option<Self>, Vec<Diagnostic>) {
            parse_test_impl(input, |errors| ModuleParser::new().parse(errors, input))
        }
    }

    impl Decl {
        pub(crate) fn parse_test(input: &str) -> (Option<Self>, Vec<Diagnostic>) {
            parse_test_impl(input, |errors| DeclParser::new().parse(errors, input))
        }
    }

    impl Type {
        pub(crate) fn parse_test(input: &str) -> (Option<Self>, Vec<Diagnostic>) {
            parse_test_impl(input, |errors| TypeParser::new().parse(errors, input))
        }
    }

    impl Expr {
        pub(crate) fn parse_test(input: &str) -> (Option<Self>, Vec<Diagnostic>) {
            parse_test_impl(input, |errors| ExprParser::new().parse(errors, input))
        }

        pub(crate) fn parse_block_test(input: &str) -> (Option<Self>, Vec<Diagnostic>) {
            parse_test_impl(input, |errors| BlockExprParser::new().parse(errors, input))
        }
    }

    pub fn parse_test_impl<'a, T, F>(input: &str, f: F) -> (Option<T>, Vec<Diagnostic>)
    where
        F: FnOnce(&mut Vec<ParseError<'a, ParserLoc>>) -> Result<T, ParseError<'a, usize>>,
    {
        parse_impl(&Humanizer::new(input), f)
    }
}
