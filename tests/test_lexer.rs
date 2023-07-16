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
    fn lex_operators() {
        let (tokens, errors) = Lexer::new(
            "
= * *= @ @= & &= << <<= ~ ~= | |= >> >>= ^ ^= := / /= == ** **= // = > >= < <= - -= % %= != + +=",
        )
        .tokenize();

        assert!(errors.is_empty());
        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: TokenType::Operator(OperatorType::Assign),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 1,
                        column_end: 2,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::Asterisk),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 3,
                        column_end: 4,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::AsteriskEqual),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 5,
                        column_end: 7,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::At),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 8,
                        column_end: 9,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::AtEqual),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 10,
                        column_end: 12,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::BitwiseAnd),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 13,
                        column_end: 14,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::BitwiseAndEqual),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 15,
                        column_end: 17,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::BitwiseLeftShift),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 18,
                        column_end: 20,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::BitwiseLeftShiftEqual),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 21,
                        column_end: 24,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::BitwiseNot),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 25,
                        column_end: 26,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::BitwiseNotEqual),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 27,
                        column_end: 29,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::BitwiseOr),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 30,
                        column_end: 31,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::BitwiseOrEqual),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 32,
                        column_end: 34,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::BitwiseRightShift),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 35,
                        column_end: 37,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::BitwiseRightShiftEqual),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 38,
                        column_end: 41,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::BitwiseXOR),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 42,
                        column_end: 43,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::BitwiseXOrEqual),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 44,
                        column_end: 46,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::ColonEqual),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 47,
                        column_end: 49,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::Divide),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 50,
                        column_end: 51,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::DivideEqual),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 52,
                        column_end: 54,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::Equals),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 55,
                        column_end: 57,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::Exponent),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 58,
                        column_end: 60,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::ExponentEqual),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 61,
                        column_end: 64,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::FloorDivision),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 65,
                        column_end: 67,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::Assign),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 68,
                        column_end: 69,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::GreaterThan),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 70,
                        column_end: 71,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::GreaterThanOrEqual),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 72,
                        column_end: 74,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::LessThan),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 75,
                        column_end: 76,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::LessThanOrEqual),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 77,
                        column_end: 79,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::Minus),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 80,
                        column_end: 81,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::MinusEqual),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 82,
                        column_end: 84,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::Modulo),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 85,
                        column_end: 86,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::ModuloEqual),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 87,
                        column_end: 89,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::NotEquals),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 90,
                        column_end: 92,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::Plus),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 93,
                        column_end: 94,
                    },
                },
                Token {
                    kind: TokenType::Operator(OperatorType::PlusEqual),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 95,
                        column_end: 97,
                    },
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 97,
                        column_end: 97,
                    },
                },
            ]
        )
    }

    #[test]
    fn lex_identifiers() {
        let (tokens, errors) = Lexer::new("x hello_world y1 x_1 _id VAR VAR1 _VAR_2").tokenize();

        assert!(errors.is_empty());
        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: TokenType::Id("x"),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 2,
                    },
                },
                Token {
                    kind: TokenType::Id("hello_world"),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 3,
                        column_end: 14,
                    },
                },
                Token {
                    kind: TokenType::Id("y1"),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 15,
                        column_end: 17,
                    },
                },
                Token {
                    kind: TokenType::Id("x_1"),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 18,
                        column_end: 21,
                    },
                },
                Token {
                    kind: TokenType::Id("_id"),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 22,
                        column_end: 25,
                    },
                },
                Token {
                    kind: TokenType::Id("VAR"),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 26,
                        column_end: 29,
                    },
                },
                Token {
                    kind: TokenType::Id("VAR1"),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 30,
                        column_end: 34,
                    },
                },
                Token {
                    kind: TokenType::Id("_VAR_2"),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 35,
                        column_end: 41,
                    },
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 41,
                        column_end: 41,
                    },
                },
            ]
        );
    }

    #[test]
    fn lex_decimal_numbers_literals() {
        let (tokens, errors) = Lexer::new("10 1_000 0b10 0B1_1 0o77 0O1_6 0Xcafe_BABE 0xCAFE").tokenize();

        assert!(dbg!(errors).is_empty());
        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal("10"))),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 3,
                    },
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal("1_000"))),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 4,
                        column_end: 9,
                    },
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Binary("0b10"))),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 10,
                        column_end: 14,
                    },
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Binary("0B1_1"))),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 15,
                        column_end: 20,
                    },
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Octal("0o77"))),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 21,
                        column_end: 25,
                    },
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Octal("0O1_6"))),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 26,
                        column_end: 31,
                    },
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Hex("0Xcafe_BABE"))),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 32,
                        column_end: 43,
                    },
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Hex("0xCAFE"))),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 44,
                        column_end: 50,
                    },
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 50,
                        column_end: 50,
                    },
                },
            ]
        );
    }

    #[test]
    fn lex_float_numbers_literals() {
        let (tokens, errors) = Lexer::new("3.14 10. .001 1e100 3.14_15_93 1_000.95 3e-10 3.14E-10").tokenize();

        assert!(errors.is_empty());
        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: TokenType::Number(NumberType::Float("3.14")),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 5,
                    },
                },
                Token {
                    kind: TokenType::Number(NumberType::Float("10.")),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 6,
                        column_end: 9,
                    },
                },
                Token {
                    kind: TokenType::Number(NumberType::Float("1e100")),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 15,
                        column_end: 20,
                    },
                },
                Token {
                    kind: TokenType::Number(NumberType::Float("3.14_15_93")),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 21,
                        column_end: 31,
                    },
                },
                Token {
                    kind: TokenType::Number(NumberType::Float("1_000.95")),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 32,
                        column_end: 40,
                    },
                },
                Token {
                    kind: TokenType::Number(NumberType::Float("3e-10")),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 41,
                        column_end: 46,
                    },
                },
                Token {
                    kind: TokenType::Number(NumberType::Float("3.14E-10")),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 47,
                        column_end: 55,
                    },
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 55,
                        column_end: 55,
                    },
                },
            ]
        );
    }

    #[test]
    fn lex_imaginary_numbers_literals() {
        let (tokens, errors) =
            Lexer::new("3.14j 10.j .001j 1e100j 3.14_15_93j 1_000.95j 3e-10j 3.14E-10j 1J").tokenize();

        assert!(dbg!(errors).is_empty());
        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: TokenType::Number(NumberType::Imaginary("3.14j")),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 5,
                    },
                },
                Token {
                    kind: TokenType::Number(NumberType::Imaginary("10.j")),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 7,
                        column_end: 10,
                    },
                },
                Token {
                    kind: TokenType::Number(NumberType::Imaginary("1e100j")),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 18,
                        column_end: 23,
                    },
                },
                Token {
                    kind: TokenType::Number(NumberType::Imaginary("3.14_15_93j")),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 25,
                        column_end: 35,
                    },
                },
                Token {
                    kind: TokenType::Number(NumberType::Imaginary("1_000.95j")),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 37,
                        column_end: 45,
                    },
                },
                Token {
                    kind: TokenType::Number(NumberType::Imaginary("3e-10j")),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 47,
                        column_end: 52,
                    },
                },
                Token {
                    kind: TokenType::Number(NumberType::Imaginary("3.14E-10j")),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 54,
                        column_end: 62,
                    },
                },
                Token {
                    kind: TokenType::Number(NumberType::Imaginary("1J")),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 64,
                        column_end: 65,
                    },
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 66,
                        column_end: 66,
                    },
                },
            ]
        );
    }

    #[test]
    fn lex_single_quote_strings() {
        let (tokens, errors) = Lexer::new(
            "
''
\"\"
f'Hello {}'
F\"Hello {}\"
'Hello World'
\"Hello World\"
'Foo\\
Bar❤️'
",
        )
        .tokenize();

        assert!(errors.is_empty());
        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: TokenType::String("''"),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 1,
                        column_end: 3,
                    },
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 3,
                        column_end: 4,
                    },
                },
                Token {
                    kind: TokenType::String("\"\""),
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 1,
                        column_end: 3,
                    },
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 3,
                        column_end: 4,
                    },
                },
                Token {
                    kind: TokenType::String("'Hello {}'"),
                    span: Span {
                        row_start: 4,
                        row_end: 4,
                        column_start: 2,
                        column_end: 12,
                    },
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 4,
                        row_end: 4,
                        column_start: 12,
                        column_end: 13,
                    },
                },
                Token {
                    kind: TokenType::String("\"Hello {}\""),
                    span: Span {
                        row_start: 5,
                        row_end: 5,
                        column_start: 2,
                        column_end: 12,
                    },
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 5,
                        row_end: 5,
                        column_start: 12,
                        column_end: 13,
                    },
                },
                Token {
                    kind: TokenType::String("'Hello World'"),
                    span: Span {
                        row_start: 6,
                        row_end: 6,
                        column_start: 1,
                        column_end: 14,
                    },
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 6,
                        row_end: 6,
                        column_start: 14,
                        column_end: 15,
                    },
                },
                Token {
                    kind: TokenType::String("\"Hello World\""),
                    span: Span {
                        row_start: 7,
                        row_end: 7,
                        column_start: 1,
                        column_end: 14,
                    },
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 7,
                        row_end: 7,
                        column_start: 14,
                        column_end: 15,
                    },
                },
                Token {
                    kind: TokenType::String("'Foo\\\nBar❤️'"),
                    span: Span {
                        row_start: 8,
                        row_end: 9,
                        column_start: 1,
                        column_end: 11,
                    },
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 9,
                        row_end: 9,
                        column_start: 11,
                        column_end: 12,
                    },
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 10,
                        row_end: 10,
                        column_start: 1,
                        column_end: 1,
                    },
                },
            ]
        )
    }

    #[test]
    fn lex_triple_quote_strings() {
        let (tokens, errors) = Lexer::new(
            "
''''''
\"\"\"\"\"\"
'''Hello World'''
'''
Hello \\'World\\'
'''
'''
Foo\\
Bar
'''
'''
Foo\\'''Bar\\'''
'''
'''
Bar 'Foo'
'''
",
        )
        .tokenize();

        assert!(errors.is_empty());
        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: TokenType::String("''''''"),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 1,
                        column_end: 7,
                    },
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 7,
                        column_end: 8,
                    },
                },
                Token {
                    kind: TokenType::String("\"\"\"\"\"\""),
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 1,
                        column_end: 7,
                    },
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 7,
                        column_end: 8,
                    },
                },
                Token {
                    kind: TokenType::String("'''Hello World'''"),
                    span: Span {
                        row_start: 4,
                        row_end: 4,
                        column_start: 1,
                        column_end: 18,
                    },
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 4,
                        row_end: 4,
                        column_start: 18,
                        column_end: 19,
                    },
                },
                Token {
                    kind: TokenType::String("'''\nHello \\'World\\'\n'''"),
                    span: Span {
                        row_start: 5,
                        row_end: 7,
                        column_start: 1,
                        column_end: 4,
                    },
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 7,
                        row_end: 7,
                        column_start: 4,
                        column_end: 5,
                    },
                },
                Token {
                    kind: TokenType::String("'''\nFoo\\\nBar\n'''"),
                    span: Span {
                        row_start: 8,
                        row_end: 11,
                        column_start: 1,
                        column_end: 4,
                    },
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 11,
                        row_end: 11,
                        column_start: 4,
                        column_end: 5,
                    },
                },
                Token {
                    kind: TokenType::String("'''\nFoo\\'''Bar\\'''\n'''"),
                    span: Span {
                        row_start: 12,
                        row_end: 14,
                        column_start: 1,
                        column_end: 4,
                    },
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 14,
                        row_end: 14,
                        column_start: 4,
                        column_end: 5,
                    },
                },
                Token {
                    kind: TokenType::String("'''\nBar 'Foo'\n'''"),
                    span: Span {
                        row_start: 15,
                        row_end: 17,
                        column_start: 1,
                        column_end: 4,
                    },
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 17,
                        row_end: 17,
                        column_start: 4,
                        column_end: 5,
                    },
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 18,
                        row_end: 18,
                        column_start: 1,
                        column_end: 1,
                    },
                },
            ]
        );
    }

    #[test]
    fn lex_incorrect_indentation() {
        let lexer = Lexer::new(
            " 
 def perm(l):          # error: first line indented
for i in range(len(l)):             # error: not indented
    s = l[:i] + l[i+1:]
        p = perm(l[:i] + l[i+1:])   # error: unexpected indent
        for x in p:
                r.append(l[i:i+1] + x)
            return r                # error: inconsistent dedent (this is the only error the lexer can find)
",
        );
        let (_, errors) = lexer.tokenize();

        assert!(!errors.is_empty());
        assert_eq!(
            errors,
            vec![PythonError {
                error: PythonErrorType::Indentation,
                msg: "IndentError: current indent amount '12' does not match previous indent '8'".to_string(),
                span: Span {
                    row_start: 8,
                    row_end: 8,
                    column_start: 13,
                    column_end: 13
                }
            }]
        );
    }

    #[test]
    fn lex_indentation() {
        let lexer = Lexer::new(
            "
def test(x):
    if x:
        return True

    return False
",
        );
        let (tokens, errors) = lexer.tokenize();

        assert!(errors.is_empty());
        assert_eq!(
            dbg!(tokens),
            vec![
                Token {
                    kind: TokenType::Keyword(KeywordType::Def),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 1,
                        column_end: 4
                    }
                },
                Token {
                    kind: TokenType::Id("test"),
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
                        column_start: 9,
                        column_end: 10
                    }
                },
                Token {
                    kind: TokenType::Id("x"),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 10,
                        column_end: 11
                    }
                },
                Token {
                    kind: TokenType::CloseParenthesis,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 11,
                        column_end: 12
                    }
                },
                Token {
                    kind: TokenType::Colon,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 12,
                        column_end: 13
                    }
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 13,
                        column_end: 14
                    }
                },
                Token {
                    kind: TokenType::Indent,
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 5,
                        column_end: 5
                    }
                },
                Token {
                    kind: TokenType::Keyword(KeywordType::If),
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 5,
                        column_end: 7
                    }
                },
                Token {
                    kind: TokenType::Id("x"),
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 8,
                        column_end: 9
                    }
                },
                Token {
                    kind: TokenType::Colon,
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 9,
                        column_end: 10
                    }
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 10,
                        column_end: 11
                    }
                },
                Token {
                    kind: TokenType::Indent,
                    span: Span {
                        row_start: 4,
                        row_end: 4,
                        column_start: 9,
                        column_end: 9
                    }
                },
                Token {
                    kind: TokenType::Keyword(KeywordType::Return),
                    span: Span {
                        row_start: 4,
                        row_end: 4,
                        column_start: 9,
                        column_end: 15
                    }
                },
                Token {
                    kind: TokenType::Keyword(KeywordType::True),
                    span: Span {
                        row_start: 4,
                        row_end: 4,
                        column_start: 16,
                        column_end: 20
                    }
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 4,
                        row_end: 4,
                        column_start: 20,
                        column_end: 21
                    }
                },
                Token {
                    kind: TokenType::Dedent,
                    span: Span {
                        row_start: 6,
                        row_end: 6,
                        column_start: 5,
                        column_end: 5
                    }
                },
                Token {
                    kind: TokenType::Keyword(KeywordType::Return),
                    span: Span {
                        row_start: 6,
                        row_end: 6,
                        column_start: 5,
                        column_end: 11
                    }
                },
                Token {
                    kind: TokenType::Keyword(KeywordType::False),
                    span: Span {
                        row_start: 6,
                        row_end: 6,
                        column_start: 12,
                        column_end: 17
                    }
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 6,
                        row_end: 6,
                        column_start: 17,
                        column_end: 18
                    }
                },
                Token {
                    kind: TokenType::Dedent,
                    span: Span {
                        row_start: 8,
                        row_end: 8,
                        column_start: 1,
                        column_end: 1
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 8,
                        row_end: 8,
                        column_start: 1,
                        column_end: 1
                    }
                },
            ]
        );
    }

    #[test]
    fn lex_implicit_line_joining() {
        let lexer = Lexer::new("\n[\n(\n\r)\r{\n}\r\n]\r");

        let (tokens, errors) = lexer.tokenize();

        assert!(errors.is_empty());
        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: TokenType::OpenBrackets,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 1,
                        column_end: 2
                    }
                },
                Token {
                    kind: TokenType::OpenParenthesis,
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 1,
                        column_end: 2
                    }
                },
                Token {
                    kind: TokenType::CloseParenthesis,
                    span: Span {
                        row_start: 5,
                        row_end: 5,
                        column_start: 1,
                        column_end: 2
                    }
                },
                Token {
                    kind: TokenType::OpenBrace,
                    span: Span {
                        row_start: 6,
                        row_end: 6,
                        column_start: 1,
                        column_end: 2
                    }
                },
                Token {
                    kind: TokenType::CloseBrace,
                    span: Span {
                        row_start: 7,
                        row_end: 7,
                        column_start: 1,
                        column_end: 2
                    }
                },
                Token {
                    kind: TokenType::CloseBrackets,
                    span: Span {
                        row_start: 8,
                        row_end: 8,
                        column_start: 1,
                        column_end: 2
                    }
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 8,
                        row_end: 8,
                        column_start: 2,
                        column_end: 3
                    }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 9,
                        row_end: 9,
                        column_start: 1,
                        column_end: 1
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_explicit_line_joining() {
        let lexer = Lexer::new(
            "
if True and \\
 True:
    pass",
        );
        let (tokens, errors) = lexer.tokenize();
        assert!(errors.is_empty());
        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: TokenType::Keyword(KeywordType::If),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 1,
                        column_end: 3
                    }
                },
                Token {
                    kind: TokenType::Keyword(KeywordType::True),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 4,
                        column_end: 8
                    }
                },
                Token {
                    kind: TokenType::Keyword(KeywordType::And),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 9,
                        column_end: 12
                    }
                },
                Token {
                    kind: TokenType::Keyword(KeywordType::True),
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 2,
                        column_end: 6
                    }
                },
                Token {
                    kind: TokenType::Colon,
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 6,
                        column_end: 7
                    }
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 3,
                        row_end: 3,
                        column_start: 7,
                        column_end: 8
                    }
                },
                Token {
                    kind: TokenType::Indent,
                    span: Span {
                        row_start: 4,
                        row_end: 4,
                        column_start: 5,
                        column_end: 5
                    }
                },
                Token {
                    kind: TokenType::Keyword(KeywordType::Pass),
                    span: Span {
                        row_start: 4,
                        row_end: 4,
                        column_start: 5,
                        column_end: 9
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
                    kind: TokenType::Eof,
                    span: Span {
                        row_start: 4,
                        row_end: 4,
                        column_start: 9,
                        column_end: 9
                    }
                },
            ]
        )
    }

    #[test]
    fn lex_invalid_explicit_line_joining() {
        let lexer = Lexer::new("if True and \\aa True:\n    pass");

        let (_, errors) = lexer.tokenize();

        assert!(!errors.is_empty());
        assert_eq!(
            errors,
            vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: unexpected character 'a' after line continuation character".into(),
                span: Span {
                    row_start: 1,
                    row_end: 1,
                    column_start: 13,
                    column_end: 14
                }
            },]
        );
    }

    #[test]
    fn lex_whitespaces() {
        let lexer = Lexer::new(" \t\u{0c}");
        let (tokens, errors) = lexer.tokenize();

        assert!(errors.is_empty());
        assert_eq!(
            tokens,
            vec![Token {
                kind: TokenType::Eof,
                span: Span {
                    row_start: 1,
                    row_end: 1,
                    column_start: 4,
                    column_end: 4,
                },
            },]
        );
    }

    #[test]
    fn lex_comment() {
        let lexer = Lexer::new(
            "
if x:           # comment
                       # comment
    # comment
    return 42 # comment
",
        );
        let (tokens, errors) = lexer.tokenize();

        assert!(errors.is_empty());
        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: TokenType::Keyword(KeywordType::If),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 1,
                        column_end: 3,
                    },
                },
                Token {
                    kind: TokenType::Id("x"),
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 4,
                        column_end: 5,
                    },
                },
                Token {
                    kind: TokenType::Colon,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 5,
                        column_end: 6,
                    },
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 26,
                        column_end: 27,
                    },
                },
                Token {
                    kind: TokenType::Indent,
                    span: Span {
                        row_start: 5,
                        row_end: 5,
                        column_start: 5,
                        column_end: 5,
                    },
                },
                Token {
                    kind: TokenType::Keyword(KeywordType::Return),
                    span: Span {
                        row_start: 5,
                        row_end: 5,
                        column_start: 5,
                        column_end: 11,
                    },
                },
                Token {
                    kind: TokenType::Number(NumberType::Integer(IntegerType::Decimal("42"))),
                    span: Span {
                        row_start: 5,
                        row_end: 5,
                        column_start: 12,
                        column_end: 14,
                    },
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span {
                        row_start: 5,
                        row_end: 5,
                        column_start: 24,
                        column_end: 25,
                    },
                },
                Token {
                    kind: TokenType::Dedent,
                    span: Span {
                        row_start: 6,
                        row_end: 6,
                        column_start: 1,
                        column_end: 1,
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
                },
            ]
        );
    }

    #[test]
    fn lex_invalid_float_and_imaginary_numbers() {
        let (_, errors) = Lexer::new("3._14 3.__14 3E1_4_ 3.14e10e-10 3._14j 3.__14J 3E1_4_J 3.14e10e-10J").tokenize();
        assert_eq!(
            dbg!(errors),
            vec![
                PythonError {
                    error: PythonErrorType::Syntax,
                    msg: "SyntaxError: invalid float number literal \"3._14\"".to_string(),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 6,
                    },
                },
                PythonError {
                    error: PythonErrorType::Syntax,
                    msg: "SyntaxError: invalid float number literal \"3.__14\"".to_string(),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 7,
                        column_end: 13,
                    },
                },
                PythonError {
                    error: PythonErrorType::Syntax,
                    msg: "SyntaxError: invalid float number literal \"3E1_4_\"".to_string(),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 14,
                        column_end: 20,
                    },
                },
                PythonError {
                    error: PythonErrorType::Syntax,
                    msg: "SyntaxError: invalid imaginary number literal \"3._14j\"".to_string(),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 33,
                        column_end: 38,
                    },
                },
                PythonError {
                    error: PythonErrorType::Syntax,
                    msg: "SyntaxError: invalid imaginary number literal \"3.__14J\"".to_string(),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 40,
                        column_end: 46,
                    },
                },
                PythonError {
                    error: PythonErrorType::Syntax,
                    msg: "SyntaxError: invalid imaginary number literal \"3E1_4_J\"".to_string(),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 48,
                        column_end: 54,
                    },
                },
            ]
        );
    }
}
