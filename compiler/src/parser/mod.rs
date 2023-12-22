use crate::diagnostic::*;
use crate::grammar::*;
use crate::location::SourceSpan;
use crate::syntax::*;

pub(crate) mod humanizer;
pub(crate) use humanizer::Humanizer;

impl Module {
    pub fn parse(input: &str) -> (Option<Self>, Vec<Diagnostic>) {
        parse_impl(input, |humanizer, errors| ModuleParser::new().parse(humanizer, errors, input))
    }
}

type ParseError<'a> = lalrpop_util::ParseError<usize, Token<'a>, &'static str>;

pub fn parse_impl<'a, T, F>(input: &str, f: F) -> (Option<T>, Vec<Diagnostic>)
where
    F: FnOnce(&Humanizer, &mut Vec<ParseError<'a>>) -> Result<T, ParseError<'a>>,
{
    let humanizer = Humanizer::new(input);
    let mut errors = Vec::new();
    let result = f(&humanizer, &mut errors);
    match result {
        Ok(module) => {
            let diagnostics = errors
                .into_iter()
                .map(|error| parse_error_to_diagnostic(error, &humanizer))
                .collect();
            (Some(module), diagnostics)
        }
        Err(fatal_error) => {
            let error = errors.into_iter().next().unwrap_or(fatal_error);
            let diagnostics = vec![parse_error_to_diagnostic(error, &humanizer)];
            (None, diagnostics)
        }
    }
}

fn parse_error_to_diagnostic(error: ParseError, humanizer: &Humanizer) -> Diagnostic {
    use lalrpop_util::ParseError;
    let error = error.map_location(|loc| humanizer.run(loc));
    let span = match error {
        ParseError::InvalidToken { location } | ParseError::UnrecognizedEof { location, .. } => {
            SourceSpan::new(location, location)
        }
        ParseError::UnrecognizedToken { token: (start, _, end), .. }
        | ParseError::ExtraToken { token: (start, _, end) } => SourceSpan::new(start, end),
        ParseError::User { .. } => SourceSpan::default(),
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
            parse_test_impl(input, |humanizer, errors| {
                ModuleParser::new().parse(humanizer, errors, input)
            })
        }
    }

    impl Decl {
        pub(crate) fn parse_test(input: &str) -> (Option<Self>, Vec<Diagnostic>) {
            parse_test_impl(input, |humanizer, errors| {
                DeclParser::new().parse(humanizer, errors, input)
            })
        }
    }

    impl Type {
        pub(crate) fn parse_test(input: &str) -> (Option<Self>, Vec<Diagnostic>) {
            parse_test_impl(input, |humanizer, errors| {
                TypeParser::new().parse(humanizer, errors, input)
            })
        }
    }

    impl Expr {
        pub(crate) fn parse_test(input: &str) -> (Option<Self>, Vec<Diagnostic>) {
            parse_test_impl(input, |humanizer, errors| {
                ExprParser::new().parse(humanizer, errors, input)
            })
        }

        pub(crate) fn parse_block_test(input: &str) -> (Option<Self>, Vec<Diagnostic>) {
            parse_test_impl(input, |humanizer, errors| {
                BlockExprParser::new().parse(humanizer, errors, input)
            })
        }
    }

    pub fn parse_test_impl<'a, T, F>(input: &str, f: F) -> (Option<T>, Vec<Diagnostic>)
    where
        F: FnOnce(&Humanizer, &mut Vec<ParseError<'a>>) -> Result<T, ParseError<'a>>,
    {
        parse_impl(input, f)
    }
}
