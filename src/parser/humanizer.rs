use crate::location::SourceLocation;
use line_index::{LineIndex, TextSize, WideEncoding};

#[derive(Debug, Eq, PartialEq)]
pub struct Humanizer(LineIndex);

impl Humanizer {
    pub fn new(input: &str) -> Self {
        Self(LineIndex::new(input))
    }

    pub fn run(&self, loc: usize) -> SourceLocation {
        u32::try_from(loc)
            .ok()
            .and_then(|loc| self.0.try_line_col(TextSize::new(loc)))
            .and_then(|utf8_pos| self.0.to_wide(WideEncoding::Utf32, utf8_pos))
            .map(|utf16_pos| SourceLocation { line: utf16_pos.line, column: utf16_pos.col })
            .unwrap_or(SourceLocation { line: 0, column: 0 })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_translation() {
        let humanizer = Humanizer::new("ab\nc\nde\n\nf\r\ng\näß");
        let cases = vec![
            (0, 0, 0),  // ^|a
            (1, 0, 1),  // a|b
            (2, 0, 2),  // b|\n
            (3, 1, 0),  // \n|c
            (4, 1, 1),  // c|\n
            (5, 2, 0),  // \n|d
            (6, 2, 1),  // d|e
            (7, 2, 2),  // e|\n
            (8, 3, 0),  // \n|\n
            (9, 4, 0),  // \n|f
            (10, 4, 1), // f|\r
            (11, 4, 2), // \r|\n
            (12, 5, 0), // \n|g
            (13, 5, 1), // g|\n
            (14, 6, 0), // \n|ä
            (15, 0, 0), // in ä
            (16, 6, 1), // ä|ß
            (17, 0, 0), // in ß
            (18, 6, 2), // ß|$
            (19, 0, 0), // $|
            (100, 0, 0),
        ];
        for (loc, line, column) in cases {
            assert_eq!(humanizer.run(loc), SourceLocation { line, column });
        }
    }
}
