#[cfg(test)]
mod tests_parser {
    use python_parser::{
        lexer::token::Span,
        parser::{
            Assignment, BinaryOperator, Block, ElIfStmt, ElseStmt, Expression, Function, IfStmt, ParsedFile, Parser,
            Statement, UnaryOperator, While,
        },
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

    #[test]
    fn test_parse_function() {
        let parser = Parser::new(
            "def x():
    pass",
        );
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "x".to_string(),
                    name_span: Span { start: 4, end: 5 },
                    block: Block {
                        stmts: vec![Statement::Pass(Span { start: 13, end: 17 })],
                        span: Span { start: 13, end: 17 }
                    },
                    span: Span { start: 0, end: 17 }
                })]
            }
        )
    }

    #[test]
    fn test_parse_function2() {
        let parser = Parser::new(
            "def x():
    pass
    pass
    pass",
        );

        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "x".to_string(),
                    name_span: Span { start: 4, end: 5 },
                    block: Block {
                        stmts: vec![
                            Statement::Pass(Span { start: 13, end: 17 }),
                            Statement::Pass(Span { start: 22, end: 26 }),
                            Statement::Pass(Span { start: 31, end: 35 })
                        ],
                        span: Span { start: 13, end: 35 }
                    },
                    span: Span { start: 0, end: 35 }
                })]
            }
        )
    }

    #[test]
    fn test_parse_if() {
        let parser = Parser::new(
            "if True:
    pass
",
        );

        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::If(IfStmt {
                    condition: Expression::Bool(true, Span { start: 3, end: 7 }),
                    block: Block {
                        stmts: vec![Statement::Pass(Span { start: 13, end: 17 })],
                        span: Span { start: 13, end: 17 }
                    },
                    elif_stms: vec![],
                    else_stmt: None,
                    span: Span { start: 0, end: 17 }
                })]
            }
        )
    }

    #[test]
    fn test_parse_if_else() {
        let parser = Parser::new(
            "if True:
    pass
else:
    pass",
        );

        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::If(IfStmt {
                    condition: Expression::Bool(true, Span { start: 3, end: 7 }),
                    block: Block {
                        stmts: vec![Statement::Pass(Span { start: 13, end: 17 })],
                        span: Span { start: 13, end: 17 }
                    },
                    elif_stms: vec![],
                    else_stmt: Some(ElseStmt {
                        block: Block {
                            stmts: vec![Statement::Pass(Span { start: 28, end: 32 })],
                            span: Span { start: 28, end: 32 }
                        },
                        span: Span { start: 18, end: 32 },
                    }),
                    span: Span { start: 0, end: 32 }
                })]
            }
        )
    }

    #[test]
    fn test_parse_if_elif() {
        let parser = Parser::new(
            "if True:
    pass
elif True:
    pass
elif True:
    pass",
        );
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::If(IfStmt {
                    condition: Expression::Bool(true, Span { start: 3, end: 7 }),
                    block: Block {
                        stmts: vec![Statement::Pass(Span { start: 13, end: 17 })],
                        span: Span { start: 13, end: 17 },
                    },
                    elif_stms: vec![
                        ElIfStmt {
                            condition: Expression::Bool(true, Span { start: 23, end: 27 }),
                            block: Block {
                                stmts: vec![Statement::Pass(Span { start: 33, end: 37 })],
                                span: Span { start: 33, end: 37 },
                            },
                            span: Span { start: 18, end: 37 },
                        },
                        ElIfStmt {
                            condition: Expression::Bool(true, Span { start: 43, end: 47 }),
                            block: Block {
                                stmts: vec![Statement::Pass(Span { start: 53, end: 57 })],
                                span: Span { start: 53, end: 57 }
                            },
                            span: Span { start: 38, end: 57 },
                        },
                    ],
                    else_stmt: None,
                    span: Span { start: 0, end: 57 },
                })],
            }
        )
    }

    #[test]
    fn test_parse_if_elif_else() {
        let parser = Parser::new(
            "if True:
    pass
elif True:
    pass
elif True:
    pass
else:
    pass",
        );

        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::If(IfStmt {
                    condition: Expression::Bool(true, Span { start: 3, end: 7 }),
                    block: Block {
                        stmts: vec![Statement::Pass(Span { start: 13, end: 17 })],
                        span: Span { start: 13, end: 17 },
                    },
                    elif_stms: vec![
                        ElIfStmt {
                            condition: Expression::Bool(true, Span { start: 23, end: 27 }),
                            block: Block {
                                stmts: vec![Statement::Pass(Span { start: 33, end: 37 })],
                                span: Span { start: 33, end: 37 },
                            },
                            span: Span { start: 18, end: 37 },
                        },
                        ElIfStmt {
                            condition: Expression::Bool(true, Span { start: 43, end: 47 }),
                            block: Block {
                                stmts: vec![Statement::Pass(Span { start: 53, end: 57 })],
                                span: Span { start: 53, end: 57 }
                            },
                            span: Span { start: 38, end: 57 },
                        },
                    ],
                    else_stmt: Some(ElseStmt {
                        block: Block {
                            stmts: vec![Statement::Pass(Span { start: 68, end: 72 })],
                            span: Span { start: 68, end: 72 }
                        },
                        span: Span { start: 58, end: 72 },
                    }),
                    span: Span { start: 0, end: 72 },
                })],
            }
        )
    }

    #[test]
    fn test_parse_while() {
        let parser = Parser::new(
            "while True:
    pass
",
        );

        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::While(While {
                    condition: Expression::Bool(true, Span { start: 6, end: 10 }),
                    else_stmt: None,
                    span: Span { start: 0, end: 20 }
                })]
            }
        )
    }

    #[test]
    fn test_parse_while_else() {
        let parser = Parser::new(
            "while True:
    pass
else:
    pass",
        );

        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::While(While {
                    condition: Expression::Bool(true, Span { start: 6, end: 10 },),
                    else_stmt: Some(ElseStmt {
                        block: Block {
                            stmts: vec![Statement::Pass(Span { start: 31, end: 35 },),],
                            span: Span { start: 31, end: 35 },
                        },
                        span: Span { start: 21, end: 35 },
                    },),
                    span: Span { start: 0, end: 35 },
                })]
            }
        )
    }

    #[test]
    fn test_parse_expression() {
        let parser = Parser::new("x = 1 + 2 + 3");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::Assignment(
                    Assignment::new("x".to_string(), Span { start: 0, end: 13 }),
                    Expression::BinaryOp(
                        Box::new(Expression::BinaryOp(
                            Box::new(Expression::Number("1".to_string(), Span { start: 4, end: 5 })),
                            BinaryOperator::Add,
                            Box::new(Expression::Number("2".to_string(), Span { start: 8, end: 9 })),
                            Span { start: 4, end: 9 },
                        )),
                        BinaryOperator::Add,
                        Box::new(Expression::Number("3".to_string(), Span { start: 12, end: 13 })),
                        Span { start: 4, end: 13 },
                    )
                )]
            }
        )
    }

    #[test]
    fn test_parse_expression2() {
        let parser = Parser::new("x = 1 + 2 * 3 / 2");

        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::Assignment(
                    Assignment::new("x".to_string(), Span { start: 0, end: 17 }),
                    Expression::BinaryOp(
                        Box::new(Expression::Number("1".to_string(), Span { start: 4, end: 5 })),
                        BinaryOperator::Add,
                        Box::new(Expression::BinaryOp(
                            Box::new(Expression::BinaryOp(
                                Box::new(Expression::Number("2".to_string(), Span { start: 8, end: 9 })),
                                BinaryOperator::Multiply,
                                Box::new(Expression::Number("3".to_string(), Span { start: 12, end: 13 })),
                                Span { start: 8, end: 13 }
                            )),
                            BinaryOperator::Divide,
                            Box::new(Expression::Number("2".to_string(), Span { start: 16, end: 17 })),
                            Span { start: 8, end: 17 }
                        )),
                        Span { start: 4, end: 17 }
                    )
                )]
            }
        );
    }

    #[test]
    fn test_parse_expression3() {
        let parser = Parser::new("x = 3 + -5");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::Assignment(
                    Assignment::new("x".to_string(), Span { start: 0, end: 10 }),
                    Expression::BinaryOp(
                        Box::new(Expression::Number("3".to_string(), Span { start: 4, end: 5 })),
                        BinaryOperator::Add,
                        Box::new(Expression::UnaryOp(
                            Box::new(Expression::Number("5".to_string(), Span { start: 9, end: 10 })),
                            UnaryOperator::Minus,
                            Span { start: 8, end: 10 }
                        )),
                        Span { start: 4, end: 10 },
                    )
                )]
            }
        )
    }

    #[test]
    fn test_parse_expression4() {
        let parser = Parser::new("x = not 3 + -5");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::Assignment(
                    Assignment::new("x".to_string(), Span { start: 0, end: 14 }),
                    Expression::UnaryOp(
                        Box::new(Expression::BinaryOp(
                            Box::new(Expression::Number("3".to_string(), Span { start: 8, end: 9 })),
                            BinaryOperator::Add,
                            Box::new(Expression::UnaryOp(
                                Box::new(Expression::Number("5".to_string(), Span { start: 13, end: 14 })),
                                UnaryOperator::Minus,
                                Span { start: 12, end: 14 }
                            )),
                            Span { start: 8, end: 14 }
                        )),
                        UnaryOperator::LogicalNot,
                        Span { start: 4, end: 14 }
                    )
                )]
            }
        );
    }

    #[test]
    fn test_parse_expression5() {
        let parser = Parser::new("x = x + +5");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::Assignment(
                    Assignment::new("x".to_string(), Span { start: 0, end: 10 }),
                    Expression::BinaryOp(
                        Box::new(Expression::Id("x".to_string(), Span { start: 4, end: 5 })),
                        BinaryOperator::Add,
                        Box::new(Expression::UnaryOp(
                            Box::new(Expression::Number("5".to_string(), Span { start: 9, end: 10 })),
                            UnaryOperator::Plus,
                            Span { start: 8, end: 10 }
                        )),
                        Span { start: 4, end: 10 }
                    )
                )]
            }
        );
    }

    #[test]
    fn test_parse_expression6() {
        let parser = Parser::new("a = x < y or 69 > 9 and not 101 >> 666");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::Assignment(
                    Assignment::new("a".to_string(), Span { start: 0, end: 38 }),
                    Expression::BinaryOp(
                        Box::new(Expression::BinaryOp(
                            Box::new(Expression::Id("x".to_string(), Span { start: 4, end: 5 })),
                            BinaryOperator::LessThan,
                            Box::new(Expression::Id("y".to_string(), Span { start: 8, end: 9 })),
                            Span { start: 4, end: 9 }
                        )),
                        BinaryOperator::LogicalOr,
                        Box::new(Expression::BinaryOp(
                            Box::new(Expression::BinaryOp(
                                Box::new(Expression::Number("69".to_string(), Span { start: 13, end: 15 })),
                                BinaryOperator::GreaterThan,
                                Box::new(Expression::Number("9".to_string(), Span { start: 18, end: 19 })),
                                Span { start: 13, end: 19 }
                            )),
                            BinaryOperator::LogicalAnd,
                            Box::new(Expression::UnaryOp(
                                Box::new(Expression::BinaryOp(
                                    Box::new(Expression::Number("101".to_string(), Span { start: 28, end: 31 })),
                                    BinaryOperator::BitwiseRightShift,
                                    Box::new(Expression::Number("666".to_string(), Span { start: 35, end: 38 })),
                                    Span { start: 28, end: 38 }
                                )),
                                UnaryOperator::LogicalNot,
                                Span { start: 24, end: 38 }
                            )),
                            Span { start: 13, end: 38 }
                        )),
                        Span { start: 4, end: 38 }
                    )
                )]
            }
        );
    }

    #[test]
    fn test_parse_expression7() {
        let parser = Parser::new("x = a.b.c");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::Assignment(
                    Assignment::new("x".to_string(), Span { start: 0, end: 9 }),
                    Expression::BinaryOp(
                        Box::new(Expression::Id("a".to_string(), Span { start: 4, end: 5 })),
                        BinaryOperator::AttributeRef,
                        Box::new(Expression::BinaryOp(
                            Box::new(Expression::Id("b".to_string(), Span { start: 6, end: 7 })),
                            BinaryOperator::AttributeRef,
                            Box::new(Expression::Id("c".to_string(), Span { start: 8, end: 9 })),
                            Span { start: 6, end: 9 }
                        )),
                        Span { start: 4, end: 9 }
                    )
                )]
            }
        )
    }

    #[test]
    fn test_parse_expression8() {
        let parser = Parser::new("x = hello()");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::Assignment(
                    Assignment::new("x".to_string(), Span { start: 0, end: 9 }),
                    Expression::Call(
                        Box::new(Expression::Id("hello".to_string(), Span { start: 4, end: 9 })),
                        Span { start: 4, end: 9 }
                    )
                )]
            }
        )
    }

    #[test]
    fn test_parse_expression9() {
        let parser = Parser::new("x = l[1 + 2]");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::Assignment(
                    Assignment::new("x".to_string(), Span { start: 0, end: 5 }),
                    Expression::Slice(
                        Box::new(Expression::Id("l".to_string(), Span { start: 4, end: 5 })),
                        Box::new(Expression::BinaryOp(
                            Box::new(Expression::Number("1".to_string(), Span { start: 6, end: 7 })),
                            BinaryOperator::Add,
                            Box::new(Expression::Number("2".to_string(), Span { start: 10, end: 11 })),
                            Span { start: 6, end: 11 }
                        )),
                        Span { start: 4, end: 12 }
                    )
                )]
            }
        )
    }

    #[test]
    fn test_parse_expression10() {
        let parser = Parser::new("x = a not in b");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::Assignment(
                    Assignment::new("x".to_string(), Span { start: 0, end: 14 }),
                    Expression::BinaryOp(
                        Box::new(Expression::Id("a".to_string(), Span { start: 4, end: 5 })),
                        BinaryOperator::NotIn,
                        Box::new(Expression::Id("b".to_string(), Span { start: 13, end: 14 })),
                        Span { start: 4, end: 14 }
                    )
                )]
            }
        )
    }
}
