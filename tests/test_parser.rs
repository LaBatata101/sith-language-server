#[cfg(test)]
mod tests_parser {
    use python_parser::{
        lexer::token::Span,
        parser::{
            ast::{
                BinaryOperator, Block, DictItemType, ElIfStmt, ElseStmt, Expression, FuncParameter, Function,
                IfElseExpr, IfStmt, ParsedFile, StarParameterType, Statement, UnaryOperator, VarAsgmt, While,
            },
            Parser,
        },
    };

    #[test]
    fn test_parse_string_assignment() {
        let parser = Parser::new("test = \"Hello World!\"");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::VarAsgmt(
                    VarAsgmt::new("test".to_string(), Span { start: 0, end: 21 }),
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
                    Statement::VarAsgmt(
                        VarAsgmt::new("test".to_string(), Span { start: 0, end: 9 }),
                        Expression::Number("42".to_string(), Span { start: 7, end: 9 })
                    ),
                    Statement::VarAsgmt(
                        VarAsgmt::new("x".to_string(), Span { start: 11, end: 17 }),
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
                    Statement::VarAsgmt(
                        VarAsgmt::new("x".to_string(), Span { start: 0, end: 8 }),
                        Expression::Bool(true, Span { start: 4, end: 8 })
                    ),
                    Statement::VarAsgmt(
                        VarAsgmt::new("y".to_string(), Span { start: 9, end: 18 }),
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
                    span: Span { start: 0, end: 17 },
                    parameters: vec![]
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
                    parameters: vec![],
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
    fn test_parse_function3() {
        let parser = Parser::new(
            "def test(x, y = 42):
    pass",
        );

        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "test".to_string(),
                    name_span: Span { start: 4, end: 8 },
                    parameters: vec![
                        FuncParameter {
                            name: "x".to_string(),
                            default_value: None,
                            span: Span { start: 9, end: 10 },
                            star_parameter_type: None
                        },
                        FuncParameter {
                            name: "y".to_string(),
                            default_value: Some(Expression::Number("42".to_string(), Span { start: 16, end: 18 })),
                            span: Span { start: 12, end: 18 },
                            star_parameter_type: None
                        }
                    ],
                    block: Block {
                        stmts: vec![Statement::Pass(Span { start: 25, end: 29 })],
                        span: Span { start: 25, end: 29 }
                    },
                    span: Span { start: 0, end: 29 }
                })]
            }
        )
    }

    #[test]
    fn test_parse_function4() {
        let parser = Parser::new(
            "def test(*kargs, **kwargs):
    pass",
        );

        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "test".to_string(),
                    name_span: Span { start: 4, end: 8 },
                    parameters: vec![
                        FuncParameter {
                            name: "kargs".to_string(),
                            default_value: None,
                            span: Span { start: 10, end: 15 },
                            star_parameter_type: Some(StarParameterType::Kargs)
                        },
                        FuncParameter {
                            name: "kwargs".to_string(),
                            default_value: None,
                            span: Span { start: 19, end: 25 },
                            star_parameter_type: Some(StarParameterType::KWargs)
                        }
                    ],
                    block: Block {
                        stmts: vec![Statement::Pass(Span { start: 32, end: 36 })],
                        span: Span { start: 32, end: 36 }
                    },
                    span: Span { start: 0, end: 36 }
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
                stmts: vec![Statement::VarAsgmt(
                    VarAsgmt::new("x".to_string(), Span { start: 0, end: 13 }),
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
                stmts: vec![Statement::VarAsgmt(
                    VarAsgmt::new("x".to_string(), Span { start: 0, end: 17 }),
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
                stmts: vec![Statement::VarAsgmt(
                    VarAsgmt::new("x".to_string(), Span { start: 0, end: 10 }),
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
                stmts: vec![Statement::VarAsgmt(
                    VarAsgmt::new("x".to_string(), Span { start: 0, end: 14 }),
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
                stmts: vec![Statement::VarAsgmt(
                    VarAsgmt::new("x".to_string(), Span { start: 0, end: 10 }),
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
                stmts: vec![Statement::VarAsgmt(
                    VarAsgmt::new("a".to_string(), Span { start: 0, end: 38 }),
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
                stmts: vec![Statement::VarAsgmt(
                    VarAsgmt::new("x".to_string(), Span { start: 0, end: 9 }),
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
                stmts: vec![Statement::VarAsgmt(
                    VarAsgmt::new("x".to_string(), Span { start: 0, end: 9 }),
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
                stmts: vec![Statement::VarAsgmt(
                    VarAsgmt::new("x".to_string(), Span { start: 0, end: 5 }),
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
                stmts: vec![Statement::VarAsgmt(
                    VarAsgmt::new("x".to_string(), Span { start: 0, end: 14 }),
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

    #[test]
    fn test_parse_expression11() {
        let parser = Parser::new("x = ((1 + 2) * 54) / 3");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::VarAsgmt(
                    VarAsgmt::new("x".to_string(), Span { start: 0, end: 22 }),
                    Expression::BinaryOp(
                        Box::new(Expression::BinaryOp(
                            Box::new(Expression::BinaryOp(
                                Box::new(Expression::Number("1".to_string(), Span { start: 6, end: 7 })),
                                BinaryOperator::Add,
                                Box::new(Expression::Number("2".to_string(), Span { start: 10, end: 11 })),
                                Span { start: 6, end: 11 }
                            )),
                            BinaryOperator::Multiply,
                            Box::new(Expression::Number("54".to_string(), Span { start: 15, end: 17 })),
                            Span { start: 6, end: 17 }
                        )),
                        BinaryOperator::Divide,
                        Box::new(Expression::Number("3".to_string(), Span { start: 21, end: 22 })),
                        Span { start: 6, end: 22 }
                    )
                )]
            }
        );
    }

    #[test]
    fn test_parse_tuple_expression() {
        let parser = Parser::new("x = (1 + 2, True, y(), \"Hello\", l[i],)");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::VarAsgmt(
                    VarAsgmt::new("x".to_string(), Span { start: 0, end: 38 }),
                    Expression::Tuple(
                        vec![
                            Expression::BinaryOp(
                                Box::new(Expression::Number("1".to_string(), Span { start: 5, end: 6 })),
                                BinaryOperator::Add,
                                Box::new(Expression::Number("2".to_string(), Span { start: 9, end: 10 })),
                                Span { start: 5, end: 10 }
                            ),
                            Expression::Bool(true, Span { start: 12, end: 16 }),
                            Expression::Call(
                                Box::new(Expression::Id("y".to_string(), Span { start: 18, end: 19 })),
                                Span { start: 18, end: 19 }
                            ),
                            Expression::String("Hello".to_string(), Span { start: 23, end: 30 }),
                            Expression::Slice(
                                Box::new(Expression::Id("l".to_string(), Span { start: 32, end: 33 })),
                                Box::new(Expression::Id("i".to_string(), Span { start: 34, end: 35 })),
                                Span { start: 32, end: 36 }
                            )
                        ],
                        Span { start: 4, end: 38 }
                    )
                )]
            }
        );
    }

    #[test]
    fn test_parse_list_expression() {
        let parser = Parser::new("x = [1 + 2, True, y(), \"Hello\", l[i],]");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::VarAsgmt(
                    VarAsgmt::new("x".to_string(), Span { start: 0, end: 38 }),
                    Expression::List(
                        vec![
                            Expression::BinaryOp(
                                Box::new(Expression::Number("1".to_string(), Span { start: 5, end: 6 })),
                                BinaryOperator::Add,
                                Box::new(Expression::Number("2".to_string(), Span { start: 9, end: 10 })),
                                Span { start: 5, end: 10 }
                            ),
                            Expression::Bool(true, Span { start: 12, end: 16 }),
                            Expression::Call(
                                Box::new(Expression::Id("y".to_string(), Span { start: 18, end: 19 })),
                                Span { start: 18, end: 19 }
                            ),
                            Expression::String("Hello".to_string(), Span { start: 23, end: 30 }),
                            Expression::Slice(
                                Box::new(Expression::Id("l".to_string(), Span { start: 32, end: 33 })),
                                Box::new(Expression::Id("i".to_string(), Span { start: 34, end: 35 })),
                                Span { start: 32, end: 36 }
                            )
                        ],
                        Span { start: 4, end: 38 }
                    )
                )]
            }
        );
    }

    #[test]
    fn test_parse_list_expression2() {
        let parser = Parser::new("x = [*l, *[1,2,3], True]");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::VarAsgmt(
                    VarAsgmt::new("x".to_string(), Span { start: 0, end: 24 }),
                    Expression::List(
                        vec![
                            Expression::UnaryOp(
                                Box::new(Expression::Id("l".to_string(), Span { start: 6, end: 7 })),
                                UnaryOperator::UnpackIterable,
                                Span { start: 5, end: 7 }
                            ),
                            Expression::UnaryOp(
                                Box::new(Expression::List(
                                    vec![
                                        Expression::Number("1".to_string(), Span { start: 11, end: 12 }),
                                        Expression::Number("2".to_string(), Span { start: 13, end: 14 }),
                                        Expression::Number("3".to_string(), Span { start: 15, end: 16 })
                                    ],
                                    Span { start: 10, end: 17 }
                                )),
                                UnaryOperator::UnpackIterable,
                                Span { start: 9, end: 17 }
                            ),
                            Expression::Bool(true, Span { start: 19, end: 23 })
                        ],
                        Span { start: 4, end: 24 }
                    )
                )]
            }
        );
    }

    #[test]
    #[should_panic(expected = "Invalid Syntax: can't unpack dictionary inside list!")]
    fn test_parse_list_expression3() {
        let parser = Parser::new("[**d]");
        parser.parse();
    }

    #[test]
    fn test_parse_unpack_iterable_assignment() {
        let parser = Parser::new("x = *iterable");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::VarAsgmt(
                    VarAsgmt::new("x".to_string(), Span { start: 0, end: 13 }),
                    Expression::UnaryOp(
                        Box::new(Expression::Id("iterable".to_string(), Span { start: 5, end: 13 })),
                        UnaryOperator::UnpackIterable,
                        Span { start: 4, end: 13 }
                    )
                )]
            }
        )
    }

    #[test]
    fn test_parse_set_expression() {
        let parser = Parser::new("x = {1, True, \"hello\", *l,}");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::VarAsgmt(
                    VarAsgmt::new("x".to_string(), Span { start: 0, end: 27 }),
                    Expression::Set(
                        vec![
                            Expression::Number("1".to_string(), Span { start: 5, end: 6 }),
                            Expression::Bool(true, Span { start: 8, end: 12 }),
                            Expression::String("hello".to_string(), Span { start: 14, end: 21 }),
                            Expression::UnaryOp(
                                Box::new(Expression::Id("l".to_string(), Span { start: 24, end: 25 })),
                                UnaryOperator::UnpackIterable,
                                Span { start: 23, end: 25 }
                            )
                        ],
                        Span { start: 4, end: 27 }
                    )
                )]
            }
        )
    }

    #[test]
    fn test_parse_dict_expression() {
        let parser = Parser::new("x = {1: \"Hello\", 1 + 3: True, (6, 6): False,}");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::VarAsgmt(
                    VarAsgmt::new("x".to_string(), Span { start: 0, end: 45 }),
                    Expression::Dict(
                        vec![
                            DictItemType::KeyValue(
                                Expression::Number("1".to_string(), Span { start: 5, end: 6 }),
                                Expression::String("Hello".to_string(), Span { start: 8, end: 15 })
                            ),
                            DictItemType::KeyValue(
                                Expression::BinaryOp(
                                    Box::new(Expression::Number("1".to_string(), Span { start: 17, end: 18 })),
                                    BinaryOperator::Add,
                                    Box::new(Expression::Number("3".to_string(), Span { start: 21, end: 22 })),
                                    Span { start: 17, end: 22 }
                                ),
                                Expression::Bool(true, Span { start: 24, end: 28 })
                            ),
                            DictItemType::KeyValue(
                                Expression::Tuple(
                                    vec![
                                        Expression::Number("6".to_string(), Span { start: 31, end: 32 }),
                                        Expression::Number("6".to_string(), Span { start: 34, end: 35 })
                                    ],
                                    Span { start: 30, end: 36 }
                                ),
                                Expression::Bool(false, Span { start: 38, end: 43 })
                            )
                        ],
                        Span { start: 4, end: 45 }
                    )
                )]
            }
        )
    }

    #[test]
    fn test_parse_dict_expression2() {
        let parser = Parser::new("x = {**d, 2: 5, **x,}");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::VarAsgmt(
                    VarAsgmt::new("x".to_string(), Span { start: 0, end: 21 }),
                    Expression::Dict(
                        vec![
                            DictItemType::DictUnpack(Expression::UnaryOp(
                                Box::new(Expression::Id("d".to_string(), Span { start: 7, end: 8 })),
                                UnaryOperator::UnpackDictionary,
                                Span { start: 5, end: 8 }
                            )),
                            DictItemType::KeyValue(
                                Expression::Number("2".to_string(), Span { start: 10, end: 11 }),
                                Expression::Number("5".to_string(), Span { start: 13, end: 14 })
                            ),
                            DictItemType::DictUnpack(Expression::UnaryOp(
                                Box::new(Expression::Id("x".to_string(), Span { start: 18, end: 19 })),
                                UnaryOperator::UnpackDictionary,
                                Span { start: 16, end: 19 }
                            ))
                        ],
                        Span { start: 4, end: 21 }
                    )
                )]
            }
        )
    }

    #[test]
    #[should_panic(expected = "Invalid Syntax: can't unpack iterable inside dictionary!")]
    fn test_parse_dict_expression3() {
        let parser = Parser::new("x = {**d, *x}");
        parser.parse();
    }

    #[test]
    fn test_parse_if_else_expression() {
        let parser = Parser::new("x = 15 if 5 < x else 45");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::VarAsgmt(
                    VarAsgmt::new("x".to_string(), Span { start: 0, end: 6 }),
                    Expression::IfElse(IfElseExpr {
                        lhs: Box::new(Expression::Number("15".to_string(), Span { start: 4, end: 6 })),
                        rhs: Box::new(Expression::Number("45".to_string(), Span { start: 21, end: 23 })),
                        condition: Box::new(Expression::BinaryOp(
                            Box::new(Expression::Number("5".to_string(), Span { start: 10, end: 11 })),
                            BinaryOperator::LessThan,
                            Box::new(Expression::Id("x".to_string(), Span { start: 14, end: 15 })),
                            Span { start: 10, end: 15 }
                        )),
                        span: Span { start: 4, end: 23 }
                    })
                )]
            }
        )
    }

    #[test]
    fn test_parse_if_else_expression2() {
        let parser = Parser::new("x = func() if (5 < x or x >= y) and is_id else func2() * 5");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::VarAsgmt(
                    VarAsgmt::new("x".to_string(), Span { start: 0, end: 8 }),
                    Expression::IfElse(IfElseExpr {
                        lhs: Box::new(Expression::Call(
                            Box::new(Expression::Id("func".to_string(), Span { start: 4, end: 8 })),
                            Span { start: 4, end: 8 }
                        )),
                        rhs: Box::new(Expression::BinaryOp(
                            Box::new(Expression::Call(
                                Box::new(Expression::Id("func2".to_string(), Span { start: 47, end: 52 })),
                                Span { start: 47, end: 52 }
                            )),
                            BinaryOperator::Multiply,
                            Box::new(Expression::Number("5".to_string(), Span { start: 57, end: 58 })),
                            Span { start: 47, end: 58 }
                        )),
                        condition: Box::new(Expression::BinaryOp(
                            Box::new(Expression::BinaryOp(
                                Box::new(Expression::BinaryOp(
                                    Box::new(Expression::Number("5".to_string(), Span { start: 15, end: 16 })),
                                    BinaryOperator::LessThan,
                                    Box::new(Expression::Id("x".to_string(), Span { start: 19, end: 20 })),
                                    Span { start: 15, end: 20 }
                                )),
                                BinaryOperator::LogicalOr,
                                Box::new(Expression::BinaryOp(
                                    Box::new(Expression::Id("x".to_string(), Span { start: 24, end: 25 })),
                                    BinaryOperator::GreaterThanOrEqual,
                                    Box::new(Expression::Id("y".to_string(), Span { start: 29, end: 30 })),
                                    Span { start: 24, end: 30 }
                                )),
                                Span { start: 15, end: 30 }
                            )),
                            BinaryOperator::LogicalAnd,
                            Box::new(Expression::Id("is_id".to_string(), Span { start: 36, end: 41 })),
                            Span { start: 15, end: 41 }
                        )),
                        span: Span { start: 4, end: 58 }
                    })
                )]
            }
        )
    }

    #[test]
    fn test_parse_single_expression() {
        let parser = Parser::new("1 + 2; 3 + 4, 7, 8");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![
                    Statement::Expression(
                        Expression::BinaryOp(
                            Box::new(Expression::Number("1".to_string(), Span { start: 0, end: 1 })),
                            BinaryOperator::Add,
                            Box::new(Expression::Number("2".to_string(), Span { start: 4, end: 5 })),
                            Span { start: 0, end: 5 }
                        ),
                        Span { start: 0, end: 5 }
                    ),
                    Statement::Expression(
                        Expression::BinaryOp(
                            Box::new(Expression::Number("3".to_string(), Span { start: 7, end: 8 })),
                            BinaryOperator::Add,
                            Box::new(Expression::Number("4".to_string(), Span { start: 11, end: 12 })),
                            Span { start: 7, end: 12 }
                        ),
                        Span { start: 7, end: 12 }
                    ),
                    Statement::Expression(
                        Expression::Number("7".to_string(), Span { start: 14, end: 15 }),
                        Span { start: 14, end: 15 }
                    ),
                    Statement::Expression(
                        Expression::Number("8".to_string(), Span { start: 17, end: 18 }),
                        Span { start: 17, end: 18 }
                    )
                ]
            }
        )
    }

    #[test]
    fn test_parse_single_expression2() {
        let parser = Parser::new("x");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::Expression(
                    Expression::Id("x".to_string(), Span { start: 0, end: 1 }),
                    Span { start: 0, end: 1 }
                )]
            }
        )
    }

    #[test]
    fn test_parse_walrus_operator() {
        let parser = Parser::new(
            "if (x := 15) > 5:
    x",
        );
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::If(IfStmt {
                    condition: Expression::BinaryOp(
                        Box::new(Expression::BinaryOp(
                            Box::new(Expression::Id("x".to_string(), Span { start: 4, end: 5 })),
                            BinaryOperator::Walrus,
                            Box::new(Expression::Number("15".to_string(), Span { start: 9, end: 11 })),
                            Span { start: 4, end: 11 }
                        )),
                        BinaryOperator::GreaterThan,
                        Box::new(Expression::Number("5".to_string(), Span { start: 15, end: 16 })),
                        Span { start: 4, end: 16 }
                    ),
                    block: Block {
                        stmts: vec![Statement::Expression(
                            Expression::Id("x".to_string(), Span { start: 22, end: 23 }),
                            Span { start: 22, end: 23 }
                        )],
                        span: Span { start: 22, end: 23 }
                    },
                    elif_stms: vec![],
                    else_stmt: None,
                    span: Span { start: 0, end: 23 }
                })]
            }
        )
    }

    #[test]
    #[should_panic(expected = "Syntax Error: Invalid assignment statement!")]
    fn test_parse_walrus_operator2() {
        let parser = Parser::new("x := 5 + 5");
        parser.parse();
    }

    #[test]
    fn test_parse_await_operator() {
        let parser = Parser::new("await func() * x ** 5 / 3");
        assert_eq!(
            parser.parse(),
            ParsedFile {
                stmts: vec![Statement::Expression(
                    Expression::BinaryOp(
                        Box::new(Expression::BinaryOp(
                            Box::new(Expression::UnaryOp(
                                Box::new(Expression::Call(
                                    Box::new(Expression::Id("func".to_string(), Span { start: 6, end: 10 })),
                                    Span { start: 6, end: 10 }
                                )),
                                UnaryOperator::Await,
                                Span { start: 0, end: 10 }
                            )),
                            BinaryOperator::Multiply,
                            Box::new(Expression::BinaryOp(
                                Box::new(Expression::Id("x".to_string(), Span { start: 15, end: 16 })),
                                BinaryOperator::Exponent,
                                Box::new(Expression::Number("5".to_string(), Span { start: 20, end: 21 })),
                                Span { start: 15, end: 21 }
                            )),
                            Span { start: 0, end: 21 }
                        )),
                        BinaryOperator::Divide,
                        Box::new(Expression::Number("3".to_string(), Span { start: 24, end: 25 })),
                        Span { start: 0, end: 25 }
                    ),
                    Span { start: 0, end: 25 }
                )]
            }
        )
    }
}
