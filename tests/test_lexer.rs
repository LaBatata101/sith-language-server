#[cfg(test)]
mod tests_lexer {
    use pretty_assertions::assert_eq;
    use python_parser::{
        error::{PythonError, PythonErrorType},
        lexer::{
            span::Span,
            token::{
                types::{IntegerType, KeywordType, NumberType, OperatorType, TokenType},
                Token,
            },
            Lexer,
        },
    };

    #[test]
    fn lex_var_assign() {
        let mut lexer = Lexer::new("test = 12");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("test".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::Assign),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 6
                    }
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal), "12".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 8,
                        column_end: 9
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 10,
                        column_end: 10
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_invalid_operator() {
        let mut lexer = Lexer::new("test +! 12");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("test".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::Plus),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 6
                    }
                },
                Token {
                    kind: TokenType::Invalid('!'),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 7,
                        column_end: 7
                    }
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal), "12".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 9,
                        column_end: 10
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 11,
                        column_end: 11
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_equals_operator() {
        let mut lexer = Lexer::new("test == 12");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("test".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::Equals),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 7
                    }
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal), "12".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 9,
                        column_end: 10
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 11,
                        column_end: 11
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_plus_equal_operator() {
        let mut lexer = Lexer::new("test += 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("test".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::PlusEqual),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 7
                    }
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 9,
                        column_end: 10
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 11,
                        column_end: 11
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_multiply_equal_operator() {
        let mut lexer = Lexer::new("test *= 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("test".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::AsteriskEqual),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 7
                    }
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 9,
                        column_end: 10
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 11,
                        column_end: 11
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_multiply_operator() {
        let mut lexer = Lexer::new("test * 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("test".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::Asterisk),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 6
                    }
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 8,
                        column_end: 9
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 10,
                        column_end: 10
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_plus_operator() {
        let mut lexer = Lexer::new("test + 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("test".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::Plus),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 6
                    }
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 8,
                        column_end: 9
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 10,
                        column_end: 10
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_minus_operator() {
        let mut lexer = Lexer::new("test - 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("test".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::Minus),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 6
                    }
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 8,
                        column_end: 9
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 10,
                        column_end: 10
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_minus_equal_operator() {
        let mut lexer = Lexer::new("test -= 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("test".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::MinusEqual),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 7
                    }
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 9,
                        column_end: 10
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 11,
                        column_end: 11
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_exponentiation_operator() {
        let mut lexer = Lexer::new("test ** 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("test".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::Exponent),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 7
                    }
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 9,
                        column_end: 10
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 11,
                        column_end: 11
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_lessthan_or_equal_operator() {
        let mut lexer = Lexer::new("test <= 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("test".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::LessThanOrEqual),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 7
                    }
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 9,
                        column_end: 10
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 11,
                        column_end: 11
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_lessthan_operator() {
        let mut lexer = Lexer::new("test < 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("test".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::LessThan),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 6
                    }
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 8,
                        column_end: 9
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 10,
                        column_end: 10
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_greaterthan_operator() {
        let mut lexer = Lexer::new("test > 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("test".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::GreaterThan),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 6
                    }
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 8,
                        column_end: 9
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 10,
                        column_end: 10
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_greaterthan_or_equal_operator() {
        let mut lexer = Lexer::new("test >= 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("test".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::GreaterThanOrEqual),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 7
                    }
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 9,
                        column_end: 10
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 11,
                        column_end: 11
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_bitwise_or_operator() {
        let mut lexer = Lexer::new("test | 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("test".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::BitwiseOr),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 6
                    }
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 8,
                        column_end: 9
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 10,
                        column_end: 10
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_bitwise_or_equal_operator() {
        let mut lexer = Lexer::new("test |= 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("test".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::BitwiseOrEqual),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 7
                    }
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 9,
                        column_end: 10
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 11,
                        column_end: 11
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_bitwise_and_operator() {
        let mut lexer = Lexer::new("test & 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("test".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::BitwiseAnd),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 6
                    }
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 8,
                        column_end: 9
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 10,
                        column_end: 10
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_bitwise_and_equal_operator() {
        let mut lexer = Lexer::new("test &= 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("test".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::BitwiseAndEqual),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 7
                    }
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 9,
                        column_end: 10
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 11,
                        column_end: 11
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_leftshift_operator() {
        let mut lexer = Lexer::new("test << 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("test".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::BitwiseLeftShift),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 7
                    }
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 9,
                        column_end: 10
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 11,
                        column_end: 11
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_leftshift_equal_operator() {
        let mut lexer = Lexer::new("test <<= 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("test".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::BitwiseLeftShiftEqual),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 8
                    }
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 10,
                        column_end: 11
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 12,
                        column_end: 12
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_rightshift_operator() {
        let mut lexer = Lexer::new("test >> 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("test".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::BitwiseRightShift),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 7
                    }
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 9,
                        column_end: 10
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 11,
                        column_end: 11
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_rightshift_equal_operator() {
        let mut lexer = Lexer::new("test >>= 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("test".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::BitwiseRightShiftEqual),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 8
                    }
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 10,
                        column_end: 11
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 12,
                        column_end: 12
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_bitwise_not_operator() {
        let mut lexer = Lexer::new("~test");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Operator(OperatorType::BitwiseNot),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 1
                    }
                },
                Token {
                    kind: TokenType::Id("test".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 2,
                        column_end: 5
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 6
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_bitwise_not_equal_operator() {
        let mut lexer = Lexer::new("test ~= 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("test".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::BitwiseNotEqual),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 7
                    }
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 9,
                        column_end: 10
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 11,
                        column_end: 11
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_at_operator() {
        let mut lexer = Lexer::new("matrix1 @ matrix2");
        lexer.tokenize();

        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("matrix1".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 7
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::At),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 9,
                        column_end: 9
                    }
                },
                Token {
                    kind: TokenType::Id("matrix2".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 11,
                        column_end: 17
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 18,
                        column_end: 18
                    }
                }
            ]
        )
    }

    #[test]
    fn lex_at_equal_operator() {
        let mut lexer = Lexer::new("matrix1 @= matrix2");
        lexer.tokenize();

        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("matrix1".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 7
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::AtEqual),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 9,
                        column_end: 10
                    }
                },
                Token {
                    kind: TokenType::Id("matrix2".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 12,
                        column_end: 18
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 19,
                        column_end: 19
                    }
                }
            ]
        )
    }

    #[test]
    fn lex_string() {
        let mut lexer = Lexer::new("\"Hello World!\"");
        lexer.tokenize();

        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::String("Hello World!".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 14
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 15,
                        column_end: 15
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_string2() {
        let mut lexer = Lexer::new("\"\\\"Hello World❤️\\\"\"");
        lexer.tokenize();

        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::String("\\\"Hello World❤️\\\"".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 23
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 24,
                        column_end: 24
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_string3() {
        let mut lexer = Lexer::new("\"\"\"\"\"\"\"\"\"\"\"\"\"\"");
        let errors = lexer.tokenize();

        assert!(errors.is_some());
        assert_eq!(
            errors.unwrap(),
            vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: unterminated string literal".into(),
                span: Span {
                    row_start: 1,
                    row_end: 1,
                    column_start: 1,
                    column_end: 14
                }
            }]
        );
    }

    #[test]
    fn lex_string4() {
        let mut lexer = Lexer::new("'Hello World!'");
        lexer.tokenize();

        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::String("Hello World!".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 14
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 15,
                        column_end: 15
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_string_with_prefix() {
        let mut lexer = Lexer::new("s = f'Hello World!'");
        lexer.tokenize();

        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("s".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 1
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::Assign),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 3,
                        column_end: 3
                    }
                },
                Token {
                    kind: TokenType::String("Hello World!".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 19
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 20,
                        column_end: 20
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_indentation() {
        let mut lexer = Lexer::new(
            "if True:
    print()
else:
    pass
",
        );
        lexer.tokenize();

        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Keyword(KeywordType::If),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 2
                    }
                },
                Token {
                    kind: TokenType::Keyword(KeywordType::True),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 4,
                        column_end: 7
                    }
                },
                Token {
                    kind: TokenType::Colon,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 8,
                        column_end: 8
                    }
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 9,
                        column_end: 9
                    }
                },
                Token {
                    kind: TokenType::Indent,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 1,
                        column_end: 1
                    }
                },
                Token {
                    kind: TokenType::Id("print".into()),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 5,
                        column_end: 9
                    }
                },
                Token {
                    kind: TokenType::OpenParenthesis,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 10,
                        column_end: 10
                    }
                },
                Token {
                    kind: TokenType::CloseParenthesis,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 11,
                        column_end: 11
                    }
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 12,
                        column_end: 12
                    }
                },
                Token {
                    kind: TokenType::Dedent,
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 1,
                        column_end: 1
                    }
                },
                Token {
                    kind: TokenType::Keyword(KeywordType::Else),
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Colon,
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 5,
                        column_end: 5
                    }
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 6,
                        column_end: 6
                    }
                },
                Token {
                    kind: TokenType::Indent,
                    span: Span {
                        row_start: 4,
                        row_end: 4,
                        column_start: 1,
                        column_end: 1
                    }
                },
                Token {
                    kind: TokenType::Keyword(KeywordType::Pass),
                    span: Span {
                        row_start: 4,
                        row_end: 4,
                        column_start: 5,
                        column_end: 8
                    }
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 4,
                        row_end: 4,
                        column_start: 9,
                        column_end: 9
                    }
                },
                Token {
                    kind: TokenType::Dedent,
                    span: Span {
                        row_start: 5,
                        row_end: 5,
                        column_start: 1,
                        column_end: 1
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 5,
                        row_end: 5,
                        column_start: 1,
                        column_end: 1
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_indentation2() {
        let mut lexer = Lexer::new(
            " def perm(l):          # error: first line indented
for i in range(len(l)):             # error: not indented
    s = l[:i] + l[i+1:]
        p = perm(l[:i] + l[i+1:])   # error: unexpected indent
        for x in p:
                r.append(l[i:i+1] + x)
            return r                # error: inconsistent dedent (this is the only error the lexer can find)
",
        );
        let errors = lexer.tokenize();
        assert!(errors.is_some());
        assert_eq!(
            errors.unwrap(),
            vec![PythonError {
                error: PythonErrorType::Indentation,
                msg: "IndentError: indent amount does not match previous indent".into(),
                span: Span {
                    row_start: 7,
                    row_end: 7,
                    column_start: 1,
                    column_end: 1
                }
            }]
        );
    }

    #[test]
    fn lex_indentation3() {
        let mut lexer = Lexer::new(
            "def test(x):
    if x:
        return True
    return False
",
        );
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Keyword(KeywordType::Def),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 3
                    }
                },
                Token {
                    kind: TokenType::Id("test".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 5,
                        column_end: 8
                    }
                },
                Token {
                    kind: TokenType::OpenParenthesis,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 9,
                        column_end: 9
                    }
                },
                Token {
                    kind: TokenType::Id("x".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 10,
                        column_end: 10
                    }
                },
                Token {
                    kind: TokenType::CloseParenthesis,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 11,
                        column_end: 11
                    }
                },
                Token {
                    kind: TokenType::Colon,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 12,
                        column_end: 12
                    }
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 13,
                        column_end: 13
                    }
                },
                Token {
                    kind: TokenType::Indent,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 1,
                        column_end: 1
                    }
                },
                Token {
                    kind: TokenType::Keyword(KeywordType::If),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 5,
                        column_end: 6
                    }
                },
                Token {
                    kind: TokenType::Id("x".into()),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 8,
                        column_end: 8
                    }
                },
                Token {
                    kind: TokenType::Colon,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 9,
                        column_end: 9
                    }
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 10,
                        column_end: 10
                    }
                },
                Token {
                    kind: TokenType::Indent,
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 1,
                        column_end: 1
                    }
                },
                Token {
                    kind: TokenType::Keyword(KeywordType::Return),
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 9,
                        column_end: 14
                    }
                },
                Token {
                    kind: TokenType::Keyword(KeywordType::True),
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 16,
                        column_end: 19
                    }
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 20,
                        column_end: 20
                    }
                },
                Token {
                    kind: TokenType::Dedent,
                    span: Span {
                        row_start: 4,
                        row_end: 4,
                        column_start: 1,
                        column_end: 1
                    }
                },
                Token {
                    kind: TokenType::Keyword(KeywordType::Return),
                    span: Span {
                        row_start: 4,
                        row_end: 4,
                        column_start: 5,
                        column_end: 10
                    }
                },
                Token {
                    kind: TokenType::Keyword(KeywordType::False),
                    span: Span {
                        row_start: 4,
                        row_end: 4,
                        column_start: 12,
                        column_end: 16
                    }
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 4,
                        row_end: 4,
                        column_start: 17,
                        column_end: 17
                    }
                },
                Token {
                    kind: TokenType::Dedent,
                    span: Span {
                        row_start: 5,
                        row_end: 5,
                        column_start: 1,
                        column_end: 1
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 5,
                        row_end: 5,
                        column_start: 1,
                        column_end: 1
                    }
                },
            ]
        );
    }

    #[test]
    #[ignore = "add support for \\n\\r, \\r"]
    fn lex_implicit_line_joining() {
        // FIXME: add support for \n\r, \r
        let mut lexer = Lexer::new("\n[\n(\n\r)\r{\n}\r\n]\r");

        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::OpenBrackets,
                    span: Span {
                        row_start: 1,
                        row_end: 2,
                        column_start: 2,
                        column_end: 1
                    }
                },
                Token {
                    kind: TokenType::OpenParenthesis,
                    span: Span {
                        row_start: 2,
                        row_end: 3,
                        column_start: 3,
                        column_end: 3
                    }
                },
                Token {
                    kind: TokenType::CloseParenthesis,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 6
                    }
                },
                Token {
                    kind: TokenType::OpenBrace,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 8,
                        column_end: 8
                    }
                },
                Token {
                    kind: TokenType::CloseBrace,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 10,
                        column_end: 10
                    }
                },
                Token {
                    kind: TokenType::CloseBrackets,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 13,
                        column_end: 13
                    }
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 14,
                        column_end: 14
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 15,
                        column_end: 16
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_explicit_line_joining() {
        let mut lexer = Lexer::new(
            "if True and \\
            True:\n    pass",
        );
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Keyword(KeywordType::If),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 2
                    }
                },
                Token {
                    kind: TokenType::Keyword(KeywordType::True),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 4,
                        column_end: 7
                    }
                },
                Token {
                    kind: TokenType::Keyword(KeywordType::And),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 9,
                        column_end: 11
                    }
                },
                Token {
                    kind: TokenType::Keyword(KeywordType::True),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 13,
                        column_end: 16
                    }
                },
                Token {
                    kind: TokenType::Colon,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 17,
                        column_end: 17
                    }
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 18,
                        column_end: 18
                    }
                },
                Token {
                    kind: TokenType::Indent,
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 1,
                        column_end: 1
                    }
                },
                Token {
                    kind: TokenType::Keyword(KeywordType::Pass),
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 5,
                        column_end: 8
                    }
                },
                Token {
                    kind: TokenType::Dedent,
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 1,
                        column_end: 1
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 9,
                        column_end: 9
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_explicit_line_joining_str() {
        let mut lexer = Lexer::new(
            "\"Hello \\
World!\"",
        );
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::String("Hello World!".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 2,
                        column_start: 1,
                        column_end: 7
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 8,
                        column_end: 8
                    }
                }
            ]
        );
    }

    #[test]
    fn lex_explicit_line_joining_str2() {
        let mut lexer = Lexer::new(
            "\"\\\"Hello \\
World!\\\"\"",
        );
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::String("\\\"Hello World!\\\"".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 2,
                        column_start: 1,
                        column_end: 9
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 10,
                        column_end: 10
                    }
                }
            ]
        );
    }

    #[test]
    fn lex_invalid_explicit_line_joining() {
        let mut lexer = Lexer::new(
            "\"Hello \\a
World!\"",
        );

        let errors = lexer.tokenize();
        assert!(errors.is_some());
        assert_eq!(
            errors.unwrap(),
            vec![
                PythonError {
                    error: PythonErrorType::Syntax,
                    msg: "SyntaxError: unterminated string literal".into(),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 8
                    }
                },
                PythonError {
                    error: PythonErrorType::Syntax,
                    msg: "SyntaxError: unterminated string literal".into(),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 7,
                        column_end: 7
                    }
                }
            ]
        );
    }

    #[test]
    fn lex_invalid_explicit_line_joining2() {
        let mut lexer = Lexer::new("if True and \\aa True:\n    pass");

        let errors = lexer.tokenize();
        assert!(errors.is_some());
        assert_eq!(
            errors.unwrap(),
            vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: unexpected characters after line continuation character".into(),
                span: Span {
                    row_start: 1,
                    row_end: 1,
                    column_start: 13,
                    column_end: 13
                }
            },]
        );
    }

    #[test]
    fn lex_binary_number() {
        let mut lexer = Lexer::new("0b_1_1_0");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Binary), "0b_1_1_0".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 8
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 9,
                        column_end: 9
                    }
                }
            ]
        )
    }

    #[test]
    fn lex_hex_number() {
        let mut lexer = Lexer::new("0xdeadbeef");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Hex), "0xdeadbeef".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 10
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 11,
                        column_end: 11
                    }
                }
            ]
        );
    }

    #[test]
    fn lex_octal_number() {
        let mut lexer = Lexer::new("0o177");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Octal), "0o177".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 5
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 6
                    }
                }
            ]
        );
    }

    #[test]
    fn lex_float_number() {
        let mut lexer = Lexer::new("3.14");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Number(NumberType::Float, "3.14".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 5,
                        column_end: 5
                    }
                }
            ]
        );
    }

    #[test]
    fn lex_float_number2() {
        let mut lexer = Lexer::new("3.");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Number(NumberType::Float, "3.".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 2
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 3,
                        column_end: 3
                    }
                }
            ]
        );
    }

    #[test]
    fn lex_float_number3() {
        let mut lexer = Lexer::new("1.001");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Number(NumberType::Float, "1.001".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 5
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 6
                    }
                }
            ]
        );
    }

    #[test]
    fn lex_float_number4() {
        let mut lexer = Lexer::new("1.001E-10");
        assert!(lexer.tokenize().is_none());

        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Number(NumberType::Float, "1.001E-10".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 9
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 10,
                        column_end: 10
                    }
                }
            ]
        );
    }

    #[test]
    fn lex_imaginary_number() {
        let mut lexer = Lexer::new(".001E-10j");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Number(NumberType::Imaginary, ".001E-10j".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 9
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 10,
                        column_end: 10
                    }
                }
            ]
        );
    }

    #[test]
    fn lex_incorrect_float_number() {
        let mut lexer = Lexer::new("1.123.21");
        let errors = lexer.tokenize();

        assert!(errors.is_some());
        assert_eq!(
            errors.unwrap(),
            vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: invalid float literal".into(),
                span: Span {
                    row_start: 1,
                    row_end: 1,
                    column_start: 1,
                    column_end: 8
                }
            }]
        );
    }

    #[test]
    fn lex_incorrect_float_number2() {
        let mut lexer = Lexer::new("112_3.23e4E-56");
        let errors = lexer.tokenize();

        assert!(errors.is_some());
        assert_eq!(
            errors.unwrap(),
            vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: invalid float literal".into(),
                span: Span {
                    row_start: 1,
                    row_end: 1,
                    column_start: 1,
                    column_end: 14
                }
            }]
        );
    }

    #[test]
    fn lex_incorrect_imaginary_number() {
        let mut lexer = Lexer::new("3.2_10jJj");
        let errors = lexer.tokenize();

        assert!(errors.is_some());
        assert_eq!(
            errors.unwrap(),
            vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: invalid imaginary literal".into(),
                span: Span {
                    row_start: 1,
                    row_end: 1,
                    column_start: 1,
                    column_end: 9
                }
            }]
        );
    }

    #[test]
    fn lex_decimal_number() {
        let mut lexer = Lexer::new("10_000_000");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal), "10_000_000".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 10
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 11,
                        column_end: 11
                    }
                }
            ]
        );
    }

    #[test]
    fn lex_invalid_decimal_number() {
        let mut lexer = Lexer::new("12_5__0");
        let errors = lexer.tokenize();
        assert!(errors.is_some());
        assert_eq!(
            errors.unwrap(),
            vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: invalid decimal literal".into(),
                span: Span {
                    row_start: 1,
                    row_end: 1,
                    column_start: 1,
                    column_end: 7
                }
            }]
        );
    }

    #[test]
    fn lex_invalid_decimal_number2() {
        let mut lexer = Lexer::new("10_");
        let errors = lexer.tokenize();
        assert!(errors.is_some());
        assert_eq!(
            errors.unwrap(),
            vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: invalid decimal literal".into(),
                span: Span {
                    row_start: 1,
                    row_end: 1,
                    column_start: 1,
                    column_end: 3
                }
            }]
        );
    }

    #[test]
    fn lex_invalid_decimal_number3() {
        let mut lexer = Lexer::new("10abc");
        let errors = lexer.tokenize();
        assert!(errors.is_some());
        assert_eq!(
            errors.unwrap(),
            vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: invalid decimal literal".into(),
                span: Span {
                    row_start: 1,
                    row_end: 1,
                    column_start: 1,
                    column_end: 5
                }
            }]
        );
    }

    #[test]
    fn lex_triple_quote_string() {
        let mut lexer = Lexer::new(
            "
r\"\"\"Multiline text
\\
Multiline text
Multiline text
\"\"\"
",
        );
        let errors = lexer.tokenize();

        assert!(errors.is_none());
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::String("Multiline text\n\\\nMultiline text\nMultiline text\n".into()),
                    span: Span {
                        row_start: 2,
                        row_end: 6,
                        column_start: 2,
                        column_end: 3
                    }
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 6,
                        row_end: 6,
                        column_start: 4,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 7,
                        row_end: 7,
                        column_start: 1,
                        column_end: 1
                    }
                }
            ]
        );
    }

    #[test]
    fn lex_parenthesized_string() {
        let mut lexer = Lexer::new(
            "
(\"Hello \"
 \"World\")",
        );
        let errors = lexer.tokenize();

        assert!(errors.is_none());
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::OpenParenthesis,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 1,
                        column_end: 1
                    }
                },
                Token {
                    kind: TokenType::String("Hello World".into()),
                    span: Span {
                        row_start: 2,
                        row_end: 3,
                        column_start: 2,
                        column_end: 8
                    }
                },
                Token {
                    kind: TokenType::CloseParenthesis,
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 9,
                        column_end: 9
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 10,
                        column_end: 10
                    }
                }
            ]
        );
    }

    #[test]
    fn lex_parenthesized_string2() {
        let mut lexer = Lexer::new("(\"Hello\" % world)");
        let errors = lexer.tokenize();

        assert!(errors.is_none());
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::OpenParenthesis,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 1
                    }
                },
                Token {
                    kind: TokenType::String("Hello".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 2,
                        column_end: 9
                    }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::Modulo),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 10,
                        column_end: 10
                    }
                },
                Token {
                    kind: TokenType::Id("world".into()),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 12,
                        column_end: 16
                    }
                },
                Token {
                    kind: TokenType::CloseParenthesis,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 17,
                        column_end: 17
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 18,
                        column_end: 18
                    }
                }
            ]
        )
    }

    #[test]
    fn lex_parenthesized_string3() {
        let mut lexer = Lexer::new(
            "
x(help='output only error messages; -qq will suppress '
     'the error messages as well.')
",
        );
        let errors = lexer.tokenize();

        assert!(errors.is_none());
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("x".into(),),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 1,
                        column_end: 1,
                    },
                },
                Token {
                    kind: TokenType::OpenParenthesis,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 2,
                        column_end: 2,
                    },
                },
                Token {
                    kind: TokenType::Id("help".into(),),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 3,
                        column_end: 6,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::Assign,),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 7,
                        column_end: 7,
                    },
                },
                Token {
                    kind: TokenType::String(
                        "output only error messages; -qq will suppress the error messages as well.".into(),
                    ),
                    span: Span {
                        row_start: 2,
                        row_end: 3,
                        column_start: 8,
                        column_end: 34,
                    },
                },
                Token {
                    kind: TokenType::CloseParenthesis,
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 35,
                        column_end: 35,
                    },
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 36,
                        column_end: 36,
                    },
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 4,
                        row_end: 4,
                        column_start: 1,
                        column_end: 1,
                    },
                },
            ]
        )
    }

    #[test]
    fn lex_parenthesized_string4() {
        let mut lexer = Lexer::new(
            "
(f'{}'
f'World {}')",
        );
        let errors = lexer.tokenize();

        assert!(errors.is_none());
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::OpenParenthesis,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 1,
                        column_end: 1,
                    },
                },
                Token {
                    kind: TokenType::String("{}World {}".into(),),
                    span: Span {
                        row_start: 2,
                        row_end: 3,
                        column_start: 3,
                        column_end: 11,
                    },
                },
                Token {
                    kind: TokenType::CloseParenthesis,
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 12,
                        column_end: 12,
                    },
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 13,
                        column_end: 13,
                    },
                },
            ]
        )
    }

    #[test]
    fn lex_parenthesized_string5() {
        let mut lexer = Lexer::new(
            "
(get_str() + 'hello ' \\
  # this is a comment
# this is a comment
            'world')
",
        );
        let errors = lexer.tokenize();

        assert!(errors.is_none());
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::OpenParenthesis,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 1,
                        column_end: 1,
                    },
                },
                Token {
                    kind: TokenType::Id("get_str".into(),),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 2,
                        column_end: 8,
                    },
                },
                Token {
                    kind: TokenType::OpenParenthesis,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 9,
                        column_end: 9,
                    },
                },
                Token {
                    kind: TokenType::CloseParenthesis,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 10,
                        column_end: 10,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::Plus,),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 12,
                        column_end: 12,
                    },
                },
                Token {
                    kind: TokenType::String("hello world".into(),),
                    span: Span {
                        row_start: 2,
                        row_end: 5,
                        column_start: 14,
                        column_end: 19,
                    },
                },
                Token {
                    kind: TokenType::CloseParenthesis,
                    span: Span {
                        row_start: 5,
                        row_end: 5,
                        column_start: 20,
                        column_end: 20,
                    },
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 5,
                        row_end: 5,
                        column_start: 21,
                        column_end: 21,
                    },
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 6,
                        row_end: 6,
                        column_start: 1,
                        column_end: 1,
                    },
                }
            ]
        )
    }

    #[test]
    fn lex_explict_splited_string() {
        let mut lexer = Lexer::new(
            "
'hello' \\ # this is a comment
  ' ' \\ 
'World' \\
fr'{x}' \\
+ '!'
",
        );
        let errors = lexer.tokenize();

        assert!(errors.is_none());
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::String("hello World{x}".into(),),
                    span: Span {
                        row_start: 2,
                        row_end: 5,
                        column_start: 1,
                        column_end: 7,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::Plus,),
                    span: Span {
                        row_start: 6,
                        row_end: 6,
                        column_start: 1,
                        column_end: 1,
                    },
                },
                Token {
                    kind: TokenType::String("!".into(),),
                    span: Span {
                        row_start: 6,
                        row_end: 6,
                        column_start: 3,
                        column_end: 5,
                    },
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 6,
                        row_end: 6,
                        column_start: 6,
                        column_end: 6,
                    },
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 7,
                        row_end: 7,
                        column_start: 1,
                        column_end: 1,
                    },
                },
            ]
        )
    }

    #[test]
    fn lex_empty_strings() {
        let mut lexer = Lexer::new(
            "
''
\"\"
''''''
\"\"\"\"\"\"
",
        );
        let errors = lexer.tokenize();

        assert!(errors.is_none());
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::String("".into()),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 1,
                        column_end: 2,
                    },
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 3,
                        column_end: 3,
                    },
                },
                Token {
                    kind: TokenType::String("".into()),
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 1,
                        column_end: 2,
                    },
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 3,
                        column_end: 3,
                    },
                },
                Token {
                    kind: TokenType::String("".into()),
                    span: Span {
                        row_start: 4,
                        row_end: 4,
                        column_start: 1,
                        column_end: 6,
                    },
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 4,
                        row_end: 4,
                        column_start: 7,
                        column_end: 7,
                    },
                },
                Token {
                    kind: TokenType::String("".into()),
                    span: Span {
                        row_start: 5,
                        row_end: 5,
                        column_start: 1,
                        column_end: 6,
                    },
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 5,
                        row_end: 5,
                        column_start: 7,
                        column_end: 7,
                    },
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 6,
                        row_end: 6,
                        column_start: 1,
                        column_end: 1,
                    },
                }
            ]
        );
    }

    #[test]
    fn lex_whitespaces() {
        let mut lexer = Lexer::new("a \tb\u{0c}c");
        let errors = lexer.tokenize();

        assert!(errors.is_none());
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::Id("a".into(),),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 1,
                    },
                },
                Token {
                    kind: TokenType::Id("b".into(),),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 4,
                        column_end: 4,
                    },
                },
                Token {
                    kind: TokenType::Id("c".into(),),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 6,
                    },
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 7,
                        column_end: 7,
                    },
                },
            ]
        );
    }

    #[test]
    fn lex_triple_quote_string_with_escaped_triple_quote() {
        let mut lexer = Lexer::new(
            "
r\"\"\"
\\\"\"\"
\"\"\"
",
        );
        let errors = lexer.tokenize();

        assert!(errors.is_none());
        assert_eq!(
            lexer.tokens(),
            vec![
                Token {
                    kind: TokenType::String("\n\\\"\"\"\n".into(),),
                    span: Span {
                        row_start: 2,
                        row_end: 4,
                        column_start: 2,
                        column_end: 3,
                    },
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 4,
                        row_end: 4,
                        column_start: 4,
                        column_end: 4,
                    },
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 5,
                        row_end: 5,
                        column_start: 1,
                        column_end: 1,
                    },
                },
            ]
        )
    }
}
