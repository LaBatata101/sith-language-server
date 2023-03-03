#[cfg(test)]
mod tests_lexer {
    use pretty_assertions::assert_eq;
    use python_parser::{
        error::{PythonError, PythonErrorType},
        lexer::{
            token::{
                types::{IntegerType, KeywordType, NumberType, OperatorType, TokenType},
                Span, Token,
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
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::Assign), 5, 6),
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Decimal), "12".to_string()),
                    7,
                    9
                ),
                Token::new(TokenType::Eof, 9, 10),
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
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::Plus), 5, 6),
                Token::new(TokenType::Invalid('!'), 6, 7),
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Decimal), "12".to_string()),
                    8,
                    10
                ),
                Token::new(TokenType::Eof, 10, 11),
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
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::Equals), 5, 7),
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Decimal), "12".to_string()),
                    8,
                    10
                ),
                Token::new(TokenType::Eof, 10, 11),
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
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::PlusEqual), 5, 7),
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".to_string()),
                    8,
                    10
                ),
                Token::new(TokenType::Eof, 10, 11),
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
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::AsteriskEqual), 5, 7),
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".to_string()),
                    8,
                    10
                ),
                Token::new(TokenType::Eof, 10, 11),
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
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::Asterisk), 5, 6),
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".to_string()),
                    7,
                    9
                ),
                Token::new(TokenType::Eof, 9, 10),
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
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::Plus), 5, 6),
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".to_string()),
                    7,
                    9
                ),
                Token::new(TokenType::Eof, 9, 10),
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
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::Minus), 5, 6),
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".to_string()),
                    7,
                    9
                ),
                Token::new(TokenType::Eof, 9, 10),
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
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::MinusEqual), 5, 7),
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".to_string()),
                    8,
                    10
                ),
                Token::new(TokenType::Eof, 10, 11),
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
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::Exponent), 5, 7),
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".to_string()),
                    8,
                    10
                ),
                Token::new(TokenType::Eof, 10, 11),
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
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::LessThanOrEqual), 5, 7),
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".to_string()),
                    8,
                    10
                ),
                Token::new(TokenType::Eof, 10, 11),
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
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::LessThan), 5, 6),
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".to_string()),
                    7,
                    9
                ),
                Token::new(TokenType::Eof, 9, 10),
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
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::GreaterThan), 5, 6),
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".to_string()),
                    7,
                    9
                ),
                Token::new(TokenType::Eof, 9, 10),
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
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::GreaterThanOrEqual), 5, 7),
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".to_string()),
                    8,
                    10
                ),
                Token::new(TokenType::Eof, 10, 11),
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
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::BitwiseOr), 5, 6),
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".to_string()),
                    7,
                    9
                ),
                Token::new(TokenType::Eof, 9, 10),
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
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::BitwiseOrEqual), 5, 7),
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".to_string()),
                    8,
                    10
                ),
                Token::new(TokenType::Eof, 10, 11),
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
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::BitwiseAnd), 5, 6),
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".to_string()),
                    7,
                    9
                ),
                Token::new(TokenType::Eof, 9, 10),
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
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::BitwiseAndEqual), 5, 7),
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".to_string()),
                    8,
                    10
                ),
                Token::new(TokenType::Eof, 10, 11),
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
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::BitwiseLeftShift), 5, 7),
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".to_string()),
                    8,
                    10
                ),
                Token::new(TokenType::Eof, 10, 11),
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
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::BitwiseLeftShiftEqual), 5, 8),
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".to_string()),
                    9,
                    11
                ),
                Token::new(TokenType::Eof, 11, 12),
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
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::BitwiseRightShift), 5, 7),
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".to_string()),
                    8,
                    10
                ),
                Token::new(TokenType::Eof, 10, 11),
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
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::BitwiseRightShiftEqual), 5, 8),
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".to_string()),
                    9,
                    11
                ),
                Token::new(TokenType::Eof, 11, 12),
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
                Token::new(TokenType::Operator(OperatorType::BitwiseNot), 0, 1),
                Token::new(TokenType::Id("test".to_string()), 1, 5),
                Token::new(TokenType::Eof, 5, 6),
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
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::BitwiseNotEqual), 5, 7),
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Decimal), "42".to_string()),
                    8,
                    10
                ),
                Token::new(TokenType::Eof, 10, 11),
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
                Token::new(TokenType::Id("matrix1".to_string()), 0, 7),
                Token::new(TokenType::Operator(OperatorType::At), 8, 9),
                Token::new(TokenType::Id("matrix2".to_string()), 10, 17),
                Token::new(TokenType::Eof, 17, 18)
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
                Token::new(TokenType::Id("matrix1".to_string()), 0, 7),
                Token::new(TokenType::Operator(OperatorType::AtEqual), 8, 10),
                Token::new(TokenType::Id("matrix2".to_string()), 11, 18),
                Token::new(TokenType::Eof, 18, 19)
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
                Token::new(TokenType::String("Hello World!".to_string()), 0, 14),
                Token::new(TokenType::Eof, 14, 15),
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
                Token::new(TokenType::String("\\\"Hello World❤️\\\"".to_string()), 0, 23),
                Token::new(TokenType::Eof, 23, 24),
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
                msg: "SyntaxError: unterminated string literal".to_string(),
                span: Span { start: 0, end: 14 }
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
                Token::new(TokenType::String("Hello World!".to_string()), 0, 14),
                Token::new(TokenType::Eof, 14, 15),
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
                Token::new(TokenType::Id("s".to_string()), 0, 1),
                Token::new(TokenType::Operator(OperatorType::Assign), 2, 3),
                Token::new(TokenType::String("Hello World!".to_string()), 5, 19),
                Token::new(TokenType::Eof, 19, 20),
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
                Token::new(TokenType::Keyword(KeywordType::If), 0, 2),
                Token::new(TokenType::Keyword(KeywordType::True), 3, 7),
                Token::new(TokenType::Colon, 7, 8),
                Token::new(TokenType::NewLine, 8, 9),
                Token::new(TokenType::Indent, 0, 0),
                Token::new(TokenType::Id("print".to_string()), 13, 18),
                Token::new(TokenType::OpenParenthesis, 18, 19),
                Token::new(TokenType::CloseParenthesis, 19, 20),
                Token::new(TokenType::NewLine, 20, 21),
                Token::new(TokenType::Dedent, 0, 0),
                Token::new(TokenType::Keyword(KeywordType::Else), 21, 25),
                Token::new(TokenType::Colon, 25, 26),
                Token::new(TokenType::NewLine, 26, 27),
                Token::new(TokenType::Indent, 0, 0),
                Token::new(TokenType::Keyword(KeywordType::Pass), 31, 35),
                Token::new(TokenType::NewLine, 35, 36),
                Token::new(TokenType::Dedent, 0, 0),
                Token::new(TokenType::Eof, 36, 37),
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
                msg: "IndentError: indent amount does not match previous indent".to_string(),
                span: Span { start: 0, end: 0 }
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
                Token::new(TokenType::Keyword(KeywordType::Def), 0, 3),
                Token::new(TokenType::Id("test".to_string()), 4, 8),
                Token::new(TokenType::OpenParenthesis, 8, 9),
                Token::new(TokenType::Id("x".to_string()), 9, 10),
                Token::new(TokenType::CloseParenthesis, 10, 11),
                Token::new(TokenType::Colon, 11, 12),
                Token::new(TokenType::NewLine, 12, 13),
                Token::new(TokenType::Indent, 0, 0),
                Token::new(TokenType::Keyword(KeywordType::If), 17, 19),
                Token::new(TokenType::Id("x".to_string()), 20, 21),
                Token::new(TokenType::Colon, 21, 22),
                Token::new(TokenType::NewLine, 22, 23),
                Token::new(TokenType::Indent, 0, 0),
                Token::new(TokenType::Keyword(KeywordType::Return), 31, 37),
                Token::new(TokenType::Keyword(KeywordType::True), 38, 42),
                Token::new(TokenType::NewLine, 42, 43),
                Token::new(TokenType::Dedent, 0, 0),
                Token::new(TokenType::Keyword(KeywordType::Return), 47, 53),
                Token::new(TokenType::Keyword(KeywordType::False), 54, 59),
                Token::new(TokenType::NewLine, 59, 60),
                Token::new(TokenType::Dedent, 0, 0),
                Token::new(TokenType::Eof, 60, 61),
            ]
        );
    }

    #[test]
    fn lex_implicit_line_joining() {
        let mut lexer = Lexer::new("\n[\n(\n\r)\r{\n}\r\n]\r");

        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token::new(TokenType::OpenBrackets, 1, 2),
                Token::new(TokenType::OpenParenthesis, 3, 4),
                Token::new(TokenType::CloseParenthesis, 6, 7),
                Token::new(TokenType::OpenBrace, 8, 9),
                Token::new(TokenType::CloseBrace, 10, 11),
                Token::new(TokenType::CloseBrackets, 13, 14),
                Token::new(TokenType::NewLine, 14, 15),
                Token::new(TokenType::Eof, 15, 16),
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
                Token::new(TokenType::Keyword(KeywordType::If), 0, 2),
                Token::new(TokenType::Keyword(KeywordType::True), 3, 7),
                Token::new(TokenType::Keyword(KeywordType::And), 8, 11),
                Token::new(TokenType::Keyword(KeywordType::True), 26, 30),
                Token::new(TokenType::Colon, 30, 31),
                Token::new(TokenType::NewLine, 31, 32),
                Token::new(TokenType::Indent, 0, 0),
                Token::new(TokenType::Keyword(KeywordType::Pass), 36, 40),
                Token::new(TokenType::Dedent, 0, 0),
                Token::new(TokenType::Eof, 40, 41),
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
                Token::new(TokenType::String("Hello World!".to_string()), 0, 16),
                Token::new(TokenType::Eof, 16, 17)
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
                Token::new(TokenType::String("\\\"Hello World!\\\"".to_string()), 0, 20),
                Token::new(TokenType::Eof, 20, 21)
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
                    msg: "SyntaxError: unterminated string literal".to_string(),
                    span: Span { start: 0, end: 9 }
                },
                PythonError {
                    error: PythonErrorType::Syntax,
                    msg: "SyntaxError: unterminated string literal".to_string(),
                    span: Span { start: 16, end: 17 }
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
                msg: "SyntaxError: unexpected characters after line continuation character".to_string(),
                span: Span { start: 12, end: 13 }
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
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Binary), "0b_1_1_0".to_string()),
                    0,
                    8
                ),
                Token::new(TokenType::Eof, 8, 9)
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
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Hex), "0xdeadbeef".to_string()),
                    0,
                    10
                ),
                Token::new(TokenType::Eof, 10, 11)
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
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Octal), "0o177".to_string()),
                    0,
                    5
                ),
                Token::new(TokenType::Eof, 5, 6)
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
                Token::new(TokenType::Number(NumberType::Float, "3.14".to_string()), 0, 4),
                Token::new(TokenType::Eof, 4, 5)
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
                Token::new(TokenType::Number(NumberType::Float, "3.".to_string()), 0, 2),
                Token::new(TokenType::Eof, 2, 3)
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
                Token::new(TokenType::Number(NumberType::Float, "1.001".to_string()), 0, 5),
                Token::new(TokenType::Eof, 5, 6)
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
                Token::new(TokenType::Number(NumberType::Float, "1.001E-10".to_string()), 0, 9),
                Token::new(TokenType::Eof, 9, 10)
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
                Token::new(TokenType::Number(NumberType::Imaginary, ".001E-10j".to_string()), 0, 9),
                Token::new(TokenType::Eof, 9, 10)
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
                msg: "SyntaxError: invalid float literal".to_string(),
                span: Span { start: 0, end: 8 }
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
                msg: "SyntaxError: invalid float literal".to_string(),
                span: Span { start: 0, end: 14 }
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
                msg: "SyntaxError: invalid imaginary literal".to_string(),
                span: Span { start: 0, end: 9 }
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
                Token::new(
                    TokenType::Number(NumberType::Integer(IntegerType::Decimal), "10_000_000".to_string()),
                    0,
                    10
                ),
                Token::new(TokenType::Eof, 10, 11)
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
                msg: "SyntaxError: invalid decimal literal".to_string(),
                span: Span { start: 0, end: 7 }
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
                msg: "SyntaxError: invalid decimal literal".to_string(),
                span: Span { start: 0, end: 3 }
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
                msg: "SyntaxError: invalid decimal literal".to_string(),
                span: Span { start: 0, end: 5 }
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
                    kind: TokenType::String("Multiline text\n\\\nMultiline text\nMultiline text\n".to_string()),
                    span: Span { start: 2, end: 55 }
                },
                Token {
                    kind: TokenType::NewLine,
                    span: Span { start: 55, end: 56 }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span { start: 56, end: 57 }
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
                    span: Span { start: 1, end: 2 }
                },
                Token {
                    kind: TokenType::String("Hello World".to_string()),
                    span: Span { start: 2, end: 19 }
                },
                Token {
                    kind: TokenType::CloseParenthesis,
                    span: Span { start: 19, end: 20 }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span { start: 20, end: 21 }
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
                    span: Span { start: 0, end: 1 }
                },
                Token {
                    kind: TokenType::String("Hello".to_string()),
                    span: Span { start: 1, end: 8 }
                },
                Token {
                    kind: TokenType::Operator(OperatorType::Modulo),
                    span: Span { start: 9, end: 10 }
                },
                Token {
                    kind: TokenType::Id("world".to_string()),
                    span: Span { start: 11, end: 16 }
                },
                Token {
                    kind: TokenType::CloseParenthesis,
                    span: Span { start: 16, end: 17 }
                },
                Token {
                    kind: TokenType::Eof,
                    span: Span { start: 17, end: 18 }
                }
            ]
        )
    }
}
