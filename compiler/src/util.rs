use std::fmt;

pub fn in_parens_if_some<T: fmt::Display>(opt: &Option<T>) -> OptionInParens<'_, T> {
    OptionInParens(opt)
}

pub struct OptionInParens<'a, T: fmt::Display>(&'a Option<T>);

impl<T: fmt::Display> fmt::Display for OptionInParens<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(x) = self.0 {
            write!(f, "({})", x)
        } else {
            Ok(())
        }
    }
}
