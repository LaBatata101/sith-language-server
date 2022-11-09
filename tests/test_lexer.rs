#[cfg(test)]
mod test_lexer {
    use python_parser::lexer::{
        token::{
            types::{KeywordType, OperatorType, TokenType},
            Token,
        },
        Lexer,
    };

    #[test]
    fn test_lex_var_assign() {
        let mut lexer = Lexer::new("test = 12");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::Assign), 5, 6),
                Token::new(TokenType::Number("12".to_string()), 7, 9),
                Token::new(TokenType::Eof, 9, 10),
            ]
        )
    }

    #[test]
    fn test_lex_invalid_operator() {
        let mut lexer = Lexer::new("test +! 12");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::Plus), 5, 6),
                Token::new(TokenType::Invalid('!'), 6, 7),
                Token::new(TokenType::Number("12".to_string()), 8, 10),
                Token::new(TokenType::Eof, 10, 11),
            ]
        )
    }

    #[test]
    fn test_lex_equals_operator() {
        let mut lexer = Lexer::new("test == 12");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::Equals), 5, 7),
                Token::new(TokenType::Number("12".to_string()), 8, 10),
                Token::new(TokenType::Eof, 10, 11),
            ]
        )
    }

    #[test]
    fn test_lex_plus_equal_operator() {
        let mut lexer = Lexer::new("test += 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::PlusEqual), 5, 7),
                Token::new(TokenType::Number("42".to_string()), 8, 10),
                Token::new(TokenType::Eof, 10, 11),
            ]
        )
    }

    #[test]
    fn test_lex_multiply_equal_operator() {
        let mut lexer = Lexer::new("test *= 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::AsteriskEqual), 5, 7),
                Token::new(TokenType::Number("42".to_string()), 8, 10),
                Token::new(TokenType::Eof, 10, 11),
            ]
        )
    }

    #[test]
    fn test_lex_multiply_operator() {
        let mut lexer = Lexer::new("test * 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::Asterisk), 5, 6),
                Token::new(TokenType::Number("42".to_string()), 7, 9),
                Token::new(TokenType::Eof, 9, 10),
            ]
        )
    }

    #[test]
    fn test_lex_plus_operator() {
        let mut lexer = Lexer::new("test + 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::Plus), 5, 6),
                Token::new(TokenType::Number("42".to_string()), 7, 9),
                Token::new(TokenType::Eof, 9, 10),
            ]
        )
    }

    #[test]
    fn test_lex_minus_operator() {
        let mut lexer = Lexer::new("test - 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::Minus), 5, 6),
                Token::new(TokenType::Number("42".to_string()), 7, 9),
                Token::new(TokenType::Eof, 9, 10),
            ]
        )
    }

    #[test]
    fn test_lex_minus_equal_operator() {
        let mut lexer = Lexer::new("test -= 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::MinusEqual), 5, 7),
                Token::new(TokenType::Number("42".to_string()), 8, 10),
                Token::new(TokenType::Eof, 10, 11),
            ]
        )
    }

    #[test]
    fn test_lex_exponentiation_operator() {
        let mut lexer = Lexer::new("test ** 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::Exponent), 5, 7),
                Token::new(TokenType::Number("42".to_string()), 8, 10),
                Token::new(TokenType::Eof, 10, 11),
            ]
        )
    }

    #[test]
    fn test_lex_lessthan_or_equal_operator() {
        let mut lexer = Lexer::new("test <= 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::LessThanOrEqual), 5, 7),
                Token::new(TokenType::Number("42".to_string()), 8, 10),
                Token::new(TokenType::Eof, 10, 11),
            ]
        )
    }

    #[test]
    fn test_lex_lessthan_operator() {
        let mut lexer = Lexer::new("test < 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::LessThan), 5, 6),
                Token::new(TokenType::Number("42".to_string()), 7, 9),
                Token::new(TokenType::Eof, 9, 10),
            ]
        )
    }

    #[test]
    fn test_lex_greaterthan_operator() {
        let mut lexer = Lexer::new("test > 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::GreaterThan), 5, 6),
                Token::new(TokenType::Number("42".to_string()), 7, 9),
                Token::new(TokenType::Eof, 9, 10),
            ]
        )
    }

    #[test]
    fn test_lex_greaterthan_or_equal_operator() {
        let mut lexer = Lexer::new("test >= 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::GreaterThanOrEqual), 5, 7),
                Token::new(TokenType::Number("42".to_string()), 8, 10),
                Token::new(TokenType::Eof, 10, 11),
            ]
        )
    }

    #[test]
    fn test_lex_bitwise_or_operator() {
        let mut lexer = Lexer::new("test | 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::BitwiseOr), 5, 6),
                Token::new(TokenType::Number("42".to_string()), 7, 9),
                Token::new(TokenType::Eof, 9, 10),
            ]
        )
    }

    #[test]
    fn test_lex_bitwise_or_equal_operator() {
        let mut lexer = Lexer::new("test |= 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::BitwiseOrEqual), 5, 7),
                Token::new(TokenType::Number("42".to_string()), 8, 10),
                Token::new(TokenType::Eof, 10, 11),
            ]
        )
    }

    #[test]
    fn test_lex_bitwise_and_operator() {
        let mut lexer = Lexer::new("test & 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::BitwiseAnd), 5, 6),
                Token::new(TokenType::Number("42".to_string()), 7, 9),
                Token::new(TokenType::Eof, 9, 10),
            ]
        )
    }

    #[test]
    fn test_lex_bitwise_and_equal_operator() {
        let mut lexer = Lexer::new("test &= 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::BitwiseAndEqual), 5, 7),
                Token::new(TokenType::Number("42".to_string()), 8, 10),
                Token::new(TokenType::Eof, 10, 11),
            ]
        )
    }

    #[test]
    fn test_lex_leftshift_operator() {
        let mut lexer = Lexer::new("test << 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::BitwiseLeftShift), 5, 7),
                Token::new(TokenType::Number("42".to_string()), 8, 10),
                Token::new(TokenType::Eof, 10, 11),
            ]
        )
    }

    #[test]
    fn test_lex_leftshift_equal_operator() {
        let mut lexer = Lexer::new("test <<= 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::BitwiseLeftShiftEqual), 5, 8),
                Token::new(TokenType::Number("42".to_string()), 9, 11),
                Token::new(TokenType::Eof, 11, 12),
            ]
        )
    }

    #[test]
    fn test_lex_rightshift_operator() {
        let mut lexer = Lexer::new("test >> 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::BitwiseRightShift), 5, 7),
                Token::new(TokenType::Number("42".to_string()), 8, 10),
                Token::new(TokenType::Eof, 10, 11),
            ]
        )
    }

    #[test]
    fn test_lex_rightshift_equal_operator() {
        let mut lexer = Lexer::new("test >>= 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::BitwiseRightShiftEqual), 5, 8),
                Token::new(TokenType::Number("42".to_string()), 9, 11),
                Token::new(TokenType::Eof, 11, 12),
            ]
        )
    }

    #[test]
    fn test_lex_bitwise_not_operator() {
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
    fn test_lex_bitwise_not_equal_operator() {
        let mut lexer = Lexer::new("test ~= 42");
        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token::new(TokenType::Id("test".to_string()), 0, 4),
                Token::new(TokenType::Operator(OperatorType::BitwiseNotEqual), 5, 7),
                Token::new(TokenType::Number("42".to_string()), 8, 10),
                Token::new(TokenType::Eof, 10, 11),
            ]
        )
    }

    #[test]
    fn test_lex_at_operator() {
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
    fn test_lex_at_equal_operator() {
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
    fn test_lex_string() {
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
    fn test_lex_string2() {
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
    #[should_panic(expected = "Missing closing quote \"!")]
    fn test_lex_string3() {
        let mut lexer = Lexer::new("\"\"\"\"\"\"\"\"\"\"\"\"\"\"");
        lexer.tokenize();
    }

    #[test]
    fn test_lex_string4() {
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
    fn test_lex_string_with_prefix() {
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
    fn test_lex_indentation() {
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
                Token::new(TokenType::Ident, 0, 0),
                Token::new(TokenType::Id("print".to_string()), 13, 18),
                Token::new(TokenType::OpenParenthesis, 18, 19),
                Token::new(TokenType::CloseParenthesis, 19, 20),
                Token::new(TokenType::NewLine, 20, 21),
                Token::new(TokenType::Dedent, 0, 0),
                Token::new(TokenType::Keyword(KeywordType::Else), 21, 25),
                Token::new(TokenType::Colon, 25, 26),
                Token::new(TokenType::NewLine, 26, 27),
                Token::new(TokenType::Ident, 0, 0),
                Token::new(TokenType::Keyword(KeywordType::Pass), 31, 35),
                Token::new(TokenType::NewLine, 35, 36),
                Token::new(TokenType::Dedent, 0, 0),
                Token::new(TokenType::Eof, 36, 37),
            ]
        )
    }

    #[test]
    #[should_panic(expected = "IndentError!")]
    fn test_lex_indentation2() {
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
        lexer.tokenize();
    }

    #[test]
    fn test_lex_indentation3() {
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
                Token::new(TokenType::Ident, 0, 0),
                Token::new(TokenType::Keyword(KeywordType::If), 17, 19),
                Token::new(TokenType::Id("x".to_string()), 20, 21),
                Token::new(TokenType::Colon, 21, 22),
                Token::new(TokenType::NewLine, 22, 23),
                Token::new(TokenType::Ident, 0, 0),
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
    fn test_lex_implicit_line_joining() {
        let mut lexer = Lexer::new("\n[\n(\n)\n{\n}\n]\n");

        lexer.tokenize();
        assert_eq!(
            lexer.tokens(),
            vec![
                Token::new(TokenType::NewLine, 0, 1),
                Token::new(TokenType::OpenBrackets, 1, 2),
                Token::new(TokenType::OpenParenthesis, 3, 4),
                Token::new(TokenType::CloseParenthesis, 5, 6),
                Token::new(TokenType::OpenBrace, 7, 8),
                Token::new(TokenType::CloseBrace, 9, 10),
                Token::new(TokenType::CloseBrackets, 11, 12),
                Token::new(TokenType::NewLine, 12, 13),
                Token::new(TokenType::Eof, 13, 14),
            ]
        )
    }
}
