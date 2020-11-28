use std::fmt;

#[derive(Clone, Copy, Default, Eq, Ord, PartialEq, PartialOrd)]
pub struct SourceLocation {
    pub line: u32,
    pub column: u32,
}

#[derive(Clone, Copy, Default, Eq, PartialEq)]
pub struct SourceSpan {
    pub start: SourceLocation,
    pub end: SourceLocation,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Located<T> {
    pub locatee: T,
    pub span: SourceSpan,
}

impl SourceLocation {
    pub fn new(line: u32, column: u32) -> Self {
        Self { line, column }
    }
}

impl SourceSpan {
    pub fn new(start: SourceLocation, end: SourceLocation) -> Self {
        Self { start, end }
    }
}

impl SourceSpan {
    pub fn contains(&self, loc: SourceLocation) -> bool {
        self.start <= loc && loc < self.end
    }
}

impl<T> Located<T> {
    pub fn new(locatee: T, span: SourceSpan) -> Self {
        Self { locatee, span }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Located<U> {
        Located::new(f(self.locatee), self.span)
    }
}

impl<T> Located<T> {
    pub fn as_ref(&self) -> Located<&T> {
        Located { locatee: &self.locatee, span: self.span }
    }
}

// TODO(MH): Make this function obsolete by putting better location information
// instead.
impl<T> Located<T> {
    pub fn gen(locatee: T) -> Self {
        Self::new(locatee, SourceSpan::default())
    }
}
// `Display` implementations

impl fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // NOTE(MH): Internally, positions are zero-based. The user gets to see
        // them one-based though.
        write!(f, "{}:{}", self.line + 1, self.column + 1)
    }
}

impl fmt::Display for SourceSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

// `Debug` implementations

impl fmt::Debug for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Debug for SourceSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}-{:?}", self.start, self.end)
    }
}
