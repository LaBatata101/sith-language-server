use ruff_text_size::{TextLen, TextRange, TextSize};

pub(crate) struct Replacement {
    pub(crate) source_range: TextRange,
    pub(crate) modified_range: TextRange,
}

impl Replacement {
    /// Creates a [`Replacement`] that describes the `source_range` of `source` to replace
    /// with `modified` sliced by `modified_range`.
    pub(crate) fn between(
        source: &str,
        source_line_starts: &[TextSize],
        modified: &str,
        modified_line_starts: &[TextSize],
    ) -> Self {
        let mut source_start = TextSize::default();
        let mut replaced_start = TextSize::default();
        let mut source_end = source.text_len();
        let mut replaced_end = modified.text_len();
        let mut line_iter = source_line_starts
            .iter()
            .copied()
            .zip(modified_line_starts.iter().copied());
        for (source_line_start, modified_line_start) in line_iter.by_ref() {
            if source_line_start != modified_line_start
                || source[TextRange::new(source_start, source_line_start)]
                    != modified[TextRange::new(replaced_start, modified_line_start)]
            {
                break;
            }
            source_start = source_line_start;
            replaced_start = modified_line_start;
        }

        let mut line_iter = line_iter.rev();

        for (old_line_start, new_line_start) in line_iter.by_ref() {
            if old_line_start <= source_start
                || new_line_start <= replaced_start
                || source[TextRange::new(old_line_start, source_end)]
                    != modified[TextRange::new(new_line_start, replaced_end)]
            {
                break;
            }
            source_end = old_line_start;
            replaced_end = new_line_start;
        }

        Replacement {
            source_range: TextRange::new(source_start, source_end),
            modified_range: TextRange::new(replaced_start, replaced_end),
        }
    }
}

#[cfg(test)]
mod tests {
    use ruff_source_file::LineIndex;
    use ruff_text_size::TextRange;

    use super::Replacement;

    fn compute_replacement(source: &str, modified: &str) -> (Replacement, String) {
        let source_index = LineIndex::from_source_text(source);
        let modified_index = LineIndex::from_source_text(modified);
        let replacement = Replacement::between(
            source,
            source_index.line_starts(),
            modified,
            modified_index.line_starts(),
        );
        let mut expected = source.to_string();
        expected.replace_range(
            replacement.source_range.start().to_usize()..replacement.source_range.end().to_usize(),
            &modified[replacement.modified_range],
        );
        (replacement, expected)
    }

    #[test]
    fn delete_first_line() {
        let source = "aaaa
bbbb
cccc
";
        let modified = "bbbb
cccc
";
        let (replacement, expected) = compute_replacement(source, modified);
        assert_eq!(replacement.source_range, TextRange::new(0.into(), 5.into()));
        assert_eq!(replacement.modified_range, TextRange::empty(0.into()));
        assert_eq!(modified, &expected);
    }

    #[test]
    fn delete_middle_line() {
        let source = "aaaa
bbbb
cccc
dddd
";
        let modified = "aaaa
bbbb
dddd
";
        let (replacement, expected) = compute_replacement(source, modified);
        assert_eq!(
            replacement.source_range,
            TextRange::new(10.into(), 15.into())
        );
        assert_eq!(replacement.modified_range, TextRange::empty(10.into()));
        assert_eq!(modified, &expected);
    }

    #[test]
    fn delete_multiple_lines() {
        let source = "aaaa
bbbb
cccc
dddd
eeee
ffff
";
        let modified = "aaaa
cccc
dddd
ffff
";
        let (replacement, expected) = compute_replacement(source, modified);
        assert_eq!(
            replacement.source_range,
            TextRange::new(5.into(), 25.into())
        );
        assert_eq!(
            replacement.modified_range,
            TextRange::new(5.into(), 15.into())
        );
        assert_eq!(modified, &expected);
    }

    #[test]
    fn insert_first_line() {
        let source = "bbbb
cccc
";
        let modified = "aaaa
bbbb
cccc
";
        let (replacement, expected) = compute_replacement(source, modified);
        assert_eq!(replacement.source_range, TextRange::empty(0.into()));
        assert_eq!(
            replacement.modified_range,
            TextRange::new(0.into(), 5.into())
        );
        assert_eq!(modified, &expected);
    }

    #[test]
    fn insert_middle_line() {
        let source = "aaaa
cccc
";
        let modified = "aaaa
bbbb
cccc
";
        let (replacement, expected) = compute_replacement(source, modified);
        assert_eq!(replacement.source_range, TextRange::empty(5.into()));
        assert_eq!(
            replacement.modified_range,
            TextRange::new(5.into(), 10.into())
        );
        assert_eq!(modified, &expected);
    }

    #[test]
    fn insert_multiple_lines() {
        let source = "aaaa
cccc
eeee
";
        let modified = "aaaa
bbbb
cccc
dddd
";
        let (replacement, expected) = compute_replacement(source, modified);
        assert_eq!(
            replacement.source_range,
            TextRange::new(5.into(), 15.into())
        );
        assert_eq!(
            replacement.modified_range,
            TextRange::new(5.into(), 20.into())
        );
        assert_eq!(modified, &expected);
    }

    #[test]
    fn replace_lines() {
        let source = "aaaa
bbbb
cccc
";
        let modified = "aaaa
bbcb
cccc
";
        let (replacement, expected) = compute_replacement(source, modified);
        assert_eq!(
            replacement.source_range,
            TextRange::new(5.into(), 10.into())
        );
        assert_eq!(
            replacement.modified_range,
            TextRange::new(5.into(), 10.into())
        );
        assert_eq!(modified, &expected);
    }
}
