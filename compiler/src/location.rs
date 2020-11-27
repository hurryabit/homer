use std::fmt;

#[derive(Clone, Copy, Default, Eq, Ord, PartialEq, PartialOrd)]
pub struct HumanLoc {
    pub line: u32,
    pub column: u32,
}

#[derive(Clone, Copy, Default, Eq, PartialEq)]
pub struct Span<Loc> {
    pub start: Loc,
    pub end: Loc,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Located<T, Loc> {
    pub locatee: T,
    pub span: Span<Loc>,
}

impl HumanLoc {
    pub fn new(line: u32, column: u32) -> Self {
        Self { line, column }
    }
}

impl<Loc> Span<Loc> {
    pub fn new(start: Loc, end: Loc) -> Self {
        Self { start, end }
    }
}

impl<Loc: Ord> Span<Loc> {
    pub fn contains(&self, loc: Loc) -> bool {
        self.start <= loc && loc <= self.end
    }
}

impl<T, Loc> Located<T, Loc> {
    pub fn new(locatee: T, span: Span<Loc>) -> Self {
        Self { locatee, span }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Located<U, Loc> {
        Located::new(f(self.locatee), self.span)
    }
}

impl<T, Loc: Copy> Located<T, Loc> {
    pub fn as_ref(&self) -> Located<&T, Loc> {
        Located { locatee: &self.locatee, span: self.span }
    }
}

// TODO(MH): Make this function obsolete by putting better location information
// instead.
impl<T, Loc: Default> Located<T, Loc> {
    pub fn gen(locatee: T) -> Self {
        Self::new(locatee, Span::default())
    }
}

impl<Loc> Span<Loc> {
    pub fn map<Loc2, F: Fn(Loc) -> Loc2>(self, f: F) -> Span<Loc2> {
        Span { start: f(self.start), end: f(self.end) }
    }
}

// `Display` implementations

impl fmt::Display for HumanLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // NOTE(MH): Internally, positions are zero-based. The user gets to see
        // them one-based though.
        write!(f, "{}:{}", self.line + 1, self.column + 1)
    }
}

impl<Loc: fmt::Display> fmt::Display for Span<Loc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

// `Debug` implementations

impl fmt::Debug for HumanLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl<Loc: fmt::Debug> fmt::Debug for Span<Loc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}-{:?}", self.start, self.end)
    }
}
