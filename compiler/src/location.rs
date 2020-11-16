use std::fmt;

mod humanizer;
pub use humanizer::Humanizer;

// NOTE(MH): This type *must* not implement `Display` since parser locations
// are not meant to be shown to humans.
#[derive(Clone, Copy, Default, Eq, PartialEq, PartialOrd, Ord)]
pub struct ParserLoc(u32);

#[derive(Clone, Copy, Default, Eq, PartialEq)]
pub struct HumanLoc {
    pub line: u32,
    pub column: u32,
}

#[derive(Clone, Copy, Default)]
pub struct Span<Loc> {
    pub start: Loc,
    pub end: Loc,
}

#[derive(Clone, Copy, Debug)]
pub struct Located<T, Loc> {
    pub locatee: T,
    pub span: Span<Loc>,
}

impl ParserLoc {
    pub(crate) fn from_usize(loc: usize) -> Self {
        Self(loc as u32)
    }

    pub fn humanize(self, humanizer: &Humanizer) -> HumanLoc {
        humanizer.run(self)
    }
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

impl Span<ParserLoc> {
    pub fn humanize(self, humanizer: &Humanizer) -> Span<HumanLoc> {
        let Self { start, end } = self;
        Span {
            start: start.humanize(humanizer),
            end: end.humanize(humanizer),
        }
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
        Located {
            locatee: &self.locatee,
            span: self.span,
        }
    }
}

// TODO(MH): Make this function obsolete by putting better location information
// instead.
impl<T, Loc: Default> Located<T, Loc> {
    pub fn gen(locatee: T) -> Self {
        Self::new(locatee, Span::default())
    }
}

impl<Pos> Span<Pos> {
    pub fn map<Pos2, F: Fn(Pos) -> Pos2>(self, f: F) -> Span<Pos2> {
        Span {
            start: f(self.start),
            end: f(self.end),
        }
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

impl fmt::Debug for ParserLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

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
