#[cfg(test)]
mod tests_parser {
    use python_parser::{
        lexer::token::Span,
        parser::{Assignment, Expression, ParsedFile, Parser, Statement},
    };

    #[test]
    fn test_parse_string_assignment() {
        let parser = Parser::new("test = \"Hello World!\"");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::Assignment(
                    Assignment::new("test".to_string(), Span { start: 0, end: 21 }),
                    Expression::String("Hello World!".to_string(), Span { start: 7, end: 21 })
                )]
            }
        );
    }

    #[test]
    fn test_parse_multiple_numbers_assignment() {
        let parser = Parser::new("test = 42; x = 12");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![
                    Statement::Assignment(
                        Assignment::new("test".to_string(), Span { start: 0, end: 9 }),
                        Expression::Number("42".to_string(), Span { start: 7, end: 9 })
                    ),
                    Statement::Assignment(
                        Assignment::new("x".to_string(), Span { start: 11, end: 17 }),
                        Expression::Number("12".to_string(), Span { start: 15, end: 17 })
                    )
                ]
            }
        );
    }

    #[test]
    fn test_parse_boolean_assignment() {
        let parser = Parser::new("x = True\ny = False");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![
                    Statement::Assignment(
                        Assignment::new("x".to_string(), Span { start: 0, end: 8 }),
                        Expression::Bool(true, Span { start: 4, end: 8 })
                    ),
                    Statement::Assignment(
                        Assignment::new("y".to_string(), Span { start: 9, end: 18 }),
                        Expression::Bool(false, Span { start: 13, end: 18 })
                    )
                ]
            }
        )
    }

    #[test]
    #[should_panic(expected = "Invalid syntax!")]
    fn test_parse_incorrect_assignment() {
        let parser = Parser::new("test =");
        parser.parse();
    }
}