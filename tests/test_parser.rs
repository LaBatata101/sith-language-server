#[cfg(test)]
mod tests_parser {

    use python_parser::{
        error::{PythonError, PythonErrorType},
        lexer::token::Span,
        parser::{
            ast::{
                AnnAssign, AssertStmt, Assign, AugAssign, AugAssignType, BinaryOperator, Block, ClassStmt, DelStmt,
                DictItemType, ElIfStmt, ElseStmt, ExceptBlock, ExceptBlockKind, Expression, FinallyBlock,
                ForStmt, FromImportStmt, FuncParameter, Function, FunctionCall, IfElseExpr, IfStmt,
                ImportModule, ImportStmt, LambdaExpr, ParsedFile, RaiseStmt, ReturnStmt, StarParameterType,
                Statement, Subscript, SubscriptType, TryStmt, UnaryOperator, While, WithItem, WithStmt,
            },
            Parser,
        },
    };

    #[test]
    fn parse_string_assignment() {
        let parser = Parser::new("test = \"Hello World!\"");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id("test".to_string(), Span { start: 0, end: 4 })),
                    rhs: Box::new(Expression::String(
                        "Hello World!".to_string(),
                        Span { start: 7, end: 21 }
                    )),
                    span: Span { start: 0, end: 21 }
                },))]
            }
        );
    }

    #[test]
    fn parse_multiple_numbers_assignment() {
        let parser = Parser::new("test = 42; x = 12");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![
                    Statement::Expression(Expression::Assign(Assign {
                        lhs: Box::new(Expression::Id("test".to_string(), Span { start: 0, end: 4 })),
                        rhs: Box::new(Expression::Number("42".to_string(), Span { start: 7, end: 9 })),
                        span: Span { start: 0, end: 9 }
                    },)),
                    Statement::Expression(Expression::Assign(Assign {
                        lhs: Box::new(Expression::Id("x".to_string(), Span { start: 11, end: 12 })),
                        rhs: Box::new(Expression::Number("12".to_string(), Span { start: 15, end: 17 })),
                        span: Span { start: 11, end: 17 }
                    },))
                ]
            }
        );
    }

    #[test]
    fn parse_boolean_assignment() {
        let parser = Parser::new("x = True\ny = False");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![
                    Statement::Expression(Expression::Assign(Assign {
                        lhs: Box::new(Expression::Id("x".to_string(), Span { start: 0, end: 1 })),
                        rhs: Box::new(Expression::Bool(true, Span { start: 4, end: 8 })),
                        span: Span { start: 0, end: 8 }
                    },)),
                    Statement::Expression(Expression::Assign(Assign {
                        lhs: Box::new(Expression::Id("y".to_string(), Span { start: 9, end: 10 })),
                        rhs: Box::new(Expression::Bool(false, Span { start: 13, end: 18 })),
                        span: Span { start: 9, end: 18 }
                    },))
                ]
            }
        )
    }

    #[test]
    fn parse_incorrect_assignment() {
        let parser = Parser::new("test =");
        let (_, errors) = parser.parse();

        assert!(errors.is_some());
        assert_eq!(
            errors.unwrap(),
            vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: unexpected token Eof".to_string(),
                span: Span { start: 6, end: 7 },
            },]
        )
    }

    #[test]
    fn parse_assignment_with_typehint() {
        let parser = Parser::new("x: int = 0");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::AnnAssign(AnnAssign {
                    lhs: Box::new(Expression::Id("x".to_string(), Span { start: 0, end: 1 })),
                    rhs: Some(Box::new(Expression::Number(
                        "0".to_string(),
                        Span { start: 9, end: 10 }
                    ))),
                    typehint: Box::new(Expression::Id("int".to_string(), Span { start: 3, end: 6 })),
                    span: Span { start: 0, end: 10 }
                }))]
            }
        )
    }

    #[test]
    fn parse_tuple_unpacking_assignment() {
        let parser = Parser::new("a, b, c = 1, 2, 3");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Tuple(
                        vec![
                            Expression::Id("a".to_string(), Span { start: 0, end: 1 }),
                            Expression::Id("b".to_string(), Span { start: 3, end: 4 }),
                            Expression::Id("c".to_string(), Span { start: 6, end: 7 })
                        ],
                        Span { start: 0, end: 9 }
                    )),
                    rhs: Box::new(Expression::Tuple(
                        vec![
                            Expression::Number("1".to_string(), Span { start: 10, end: 11 }),
                            Expression::Number("2".to_string(), Span { start: 13, end: 14 }),
                            Expression::Number("3".to_string(), Span { start: 16, end: 17 })
                        ],
                        Span { start: 10, end: 18 }
                    )),
                    span: Span { start: 0, end: 18 }
                }))]
            }
        )
    }

    #[test]
    fn parse_invalid_tuple_assignment_with_typehint() {
        let parser = Parser::new("a, b, c: tuple[int, int, int] = 1, 2, 3");
        let (_, errors) = parser.parse();

        assert!(errors.is_some());
        assert_eq!(
            errors.unwrap(),
            vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: only single target (not tuple) can be annotated".to_string(),
                span: Span { start: 0, end: 8 },
            },]
        )
    }

    #[test]
    fn parse_list_unpacking_assignment() {
        let parser = Parser::new("[a, b, c] = 1, 2, 3");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::List(
                        vec![
                            Expression::Id("a".to_string(), Span { start: 1, end: 2 }),
                            Expression::Id("b".to_string(), Span { start: 4, end: 5 }),
                            Expression::Id("c".to_string(), Span { start: 7, end: 8 })
                        ],
                        Span { start: 0, end: 9 }
                    )),
                    rhs: Box::new(Expression::Tuple(
                        vec![
                            Expression::Number("1".to_string(), Span { start: 12, end: 13 }),
                            Expression::Number("2".to_string(), Span { start: 15, end: 16 }),
                            Expression::Number("3".to_string(), Span { start: 18, end: 19 })
                        ],
                        Span { start: 12, end: 20 }
                    )),
                    span: Span { start: 0, end: 20 }
                }))]
            }
        )
    }

    #[test]
    fn parse_assignment_wiht_only_typehint() {
        let parser = Parser::new("x: int");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::AnnAssign(AnnAssign {
                    lhs: Box::new(Expression::Id("x".to_string(), Span { start: 0, end: 1 })),
                    rhs: None,
                    typehint: Box::new(Expression::Id("int".to_string(), Span { start: 3, end: 6 })),
                    span: Span { start: 0, end: 6 }
                }))]
            }
        )
    }

    #[test]
    fn parse_aug_assignment() {
        let parser = Parser::new("x += 1");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::AugAssing(AugAssign {
                    lhs: Box::new(Expression::Id("x".to_string(), Span { start: 0, end: 1 })),
                    rhs: Box::new(Expression::Number("1".to_string(), Span { start: 5, end: 6 })),
                    kind: AugAssignType::Plus,
                    span: Span { start: 0, end: 6 }
                }))]
            }
        )
    }

    #[test]
    fn parse_invalid_aug_assignment() {
        let parser = Parser::new("x: int += 1");
        let (_, errors) = parser.parse();

        assert!(errors.is_some());
        assert_eq!(
            errors.unwrap(),
            vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: invalid syntax, typehint not allowed in this kind of expression".to_string(),
                span: Span { start: 0, end: 6 },
            }],
        )
    }

    #[test]
    fn parse_function() {
        let parser = Parser::new(
            "def x():
    pass",
        );
        let (parsed_file, parser_errors) = parser.parse();

        assert!(parser_errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "x".to_string(),
                    decorators: vec![],
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
    fn parse_function2() {
        let parser = Parser::new(
            "def x():
    pass
    pass
    pass",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "x".to_string(),
                    parameters: vec![],
                    decorators: vec![],
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
    fn parse_function3() {
        let parser = Parser::new(
            "def test(x, y = 42):
    pass",
        );

        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "test".to_string(),
                    decorators: vec![],
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
    fn parse_function4() {
        let parser = Parser::new(
            "def test(*kargs, **kwargs):
    pass",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "test".to_string(),
                    decorators: vec![],
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
    fn parse_if() {
        let parser = Parser::new(
            "if True:
    pass
    ",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
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
    fn parse_if_else() {
        let parser = Parser::new(
            "if True:
    pass
else:
    pass",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
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
    fn parse_if_elif() {
        let parser = Parser::new(
            "if True:
    pass
elif True:
    pass
elif True:
    pass",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
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
    fn parse_if_elif_else() {
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
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
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
    fn parse_while() {
        let parser = Parser::new(
            "while True:
    pass
",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::While(While {
                    condition: Expression::Bool(true, Span { start: 6, end: 10 }),
                    else_stmt: None,
                    block: Block {
                        stmts: vec![Statement::Pass(Span { start: 16, end: 20 })],
                        span: Span { start: 16, end: 20 }
                    },
                    span: Span { start: 0, end: 20 }
                })]
            }
        )
    }

    #[test]
    fn parse_while_else() {
        let parser = Parser::new(
            "while True:
    pass
else:
    pass",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
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
                    block: Block {
                        stmts: vec![Statement::Pass(Span { start: 16, end: 20 })],
                        span: Span { start: 16, end: 20 }
                    },
                    span: Span { start: 0, end: 35 },
                })]
            }
        )
    }

    #[test]
    fn parse_expression() {
        let parser = Parser::new("x = 1 + 2 + 3");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id("x".to_string(), Span { start: 0, end: 1 })),
                    rhs: Box::new(Expression::BinaryOp(
                        Box::new(Expression::BinaryOp(
                            Box::new(Expression::Number("1".to_string(), Span { start: 4, end: 5 })),
                            BinaryOperator::Add,
                            Box::new(Expression::Number("2".to_string(), Span { start: 8, end: 9 })),
                            Span { start: 4, end: 9 }
                        )),
                        BinaryOperator::Add,
                        Box::new(Expression::Number("3".to_string(), Span { start: 12, end: 13 })),
                        Span { start: 4, end: 13 }
                    )),
                    span: Span { start: 0, end: 13 }
                }))]
            }
        )
    }

    #[test]
    fn parse_expression2() {
        let parser = Parser::new("x = 1 + 2 * 3 / 2");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id("x".to_string(), Span { start: 0, end: 1 })),
                    rhs: Box::new(Expression::BinaryOp(
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
                    )),
                    span: Span { start: 0, end: 17 }
                }))]
            }
        );
    }

    #[test]
    fn parse_expression3() {
        let parser = Parser::new("x = 3 + -5");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id("x".to_string(), Span { start: 0, end: 1 })),
                    rhs: Box::new(Expression::BinaryOp(
                        Box::new(Expression::Number("3".to_string(), Span { start: 4, end: 5 })),
                        BinaryOperator::Add,
                        Box::new(Expression::UnaryOp(
                            Box::new(Expression::Number("5".to_string(), Span { start: 9, end: 10 })),
                            UnaryOperator::Minus,
                            Span { start: 8, end: 10 }
                        )),
                        Span { start: 4, end: 10 }
                    )),
                    span: Span { start: 0, end: 10 }
                }))]
            }
        )
    }

    #[test]
    fn parse_expression4() {
        let parser = Parser::new("x = not 3 + -5");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id("x".to_string(), Span { start: 0, end: 1 })),
                    rhs: Box::new(Expression::UnaryOp(
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
                    )),
                    span: Span { start: 0, end: 14 }
                }))]
            }
        );
    }

    #[test]
    fn parse_expression5() {
        let parser = Parser::new("x = x + +5");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id("x".to_string(), Span { start: 0, end: 1 })),
                    rhs: Box::new(Expression::BinaryOp(
                        Box::new(Expression::Id("x".to_string(), Span { start: 4, end: 5 })),
                        BinaryOperator::Add,
                        Box::new(Expression::UnaryOp(
                            Box::new(Expression::Number("5".to_string(), Span { start: 9, end: 10 })),
                            UnaryOperator::Plus,
                            Span { start: 8, end: 10 }
                        )),
                        Span { start: 4, end: 10 }
                    )),
                    span: Span { start: 0, end: 10 }
                }))]
            }
        );
    }

    #[test]
    fn parse_expression6() {
        let parser = Parser::new("a = x < y or 69 > 9 and not 101 >> 666");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id("a".to_string(), Span { start: 0, end: 1 })),
                    rhs: Box::new(Expression::BinaryOp(
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
                    )),
                    span: Span { start: 0, end: 38 }
                }))]
            }
        );
    }

    #[test]
    fn parse_expression7() {
        let parser = Parser::new("x = a.b.c");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id("x".to_string(), Span { start: 0, end: 1 })),
                    rhs: Box::new(Expression::BinaryOp(
                        Box::new(Expression::Id("a".to_string(), Span { start: 4, end: 5 })),
                        BinaryOperator::AttributeRef,
                        Box::new(Expression::BinaryOp(
                            Box::new(Expression::Id("b".to_string(), Span { start: 6, end: 7 })),
                            BinaryOperator::AttributeRef,
                            Box::new(Expression::Id("c".to_string(), Span { start: 8, end: 9 })),
                            Span { start: 6, end: 9 }
                        )),
                        Span { start: 4, end: 9 }
                    )),
                    span: Span { start: 0, end: 9 }
                }))],
            }
        )
    }

    #[test]
    fn parse_expression8() {
        let parser = Parser::new("x = hello()");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id("x".to_string(), Span { start: 0, end: 1 })),
                    rhs: Box::new(Expression::Call(FunctionCall {
                        lhs: Box::new(Expression::Id("hello".to_string(), Span { start: 4, end: 9 })),
                        args: vec![],
                        span: Span { start: 4, end: 12 }
                    })),
                    span: Span { start: 0, end: 12 }
                }))],
            }
        )
    }

    #[test]
    fn parse_expression9() {
        let parser = Parser::new("x = l[1 + 2]");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id("x".to_string(), Span { start: 0, end: 1 })),
                    rhs: Box::new(Expression::Subscript(Subscript {
                        lhs: Box::new(Expression::Id("l".to_string(), Span { start: 4, end: 5 })),
                        slice: Box::new(SubscriptType::Subscript(Expression::BinaryOp(
                            Box::new(Expression::Number("1".to_string(), Span { start: 6, end: 7 })),
                            BinaryOperator::Add,
                            Box::new(Expression::Number("2".to_string(), Span { start: 10, end: 11 })),
                            Span { start: 6, end: 11 }
                        ))),
                        span: Span { start: 4, end: 12 }
                    })),
                    span: Span { start: 0, end: 12 }
                }))]
            }
        )
    }

    #[test]
    fn parse_expression10() {
        let parser = Parser::new("x = a not in b");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id("x".to_string(), Span { start: 0, end: 1 })),
                    rhs: Box::new(Expression::BinaryOp(
                        Box::new(Expression::Id("a".to_string(), Span { start: 4, end: 5 })),
                        BinaryOperator::NotIn,
                        Box::new(Expression::Id("b".to_string(), Span { start: 13, end: 14 })),
                        Span { start: 4, end: 14 }
                    )),
                    span: Span { start: 0, end: 14 }
                }))],
            }
        )
    }

    #[test]
    fn parse_expression11() {
        let parser = Parser::new("x = ((1 + 2) * 54) / 3");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id("x".to_string(), Span { start: 0, end: 1 })),
                    rhs: Box::new(Expression::BinaryOp(
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
                    )),
                    span: Span { start: 0, end: 22 }
                }))],
            }
        );
    }

    #[test]
    fn parse_tuple_expression() {
        let parser = Parser::new("x = (1 + 2, True, y(), \"Hello\", l[i],)");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id("x".to_string(), Span { start: 0, end: 1 })),
                    rhs: Box::new(Expression::Tuple(
                        vec![
                            Expression::BinaryOp(
                                Box::new(Expression::Number("1".to_string(), Span { start: 5, end: 6 })),
                                BinaryOperator::Add,
                                Box::new(Expression::Number("2".to_string(), Span { start: 9, end: 10 })),
                                Span { start: 5, end: 10 }
                            ),
                            Expression::Bool(true, Span { start: 12, end: 16 }),
                            Expression::Call(FunctionCall {
                                lhs: Box::new(Expression::Id("y".to_string(), Span { start: 18, end: 19 })),
                                args: vec![],
                                span: Span { start: 18, end: 22 }
                            }),
                            Expression::String("Hello".to_string(), Span { start: 23, end: 30 }),
                            Expression::Subscript(Subscript {
                                lhs: Box::new(Expression::Id("l".to_string(), Span { start: 32, end: 33 })),
                                slice: Box::new(SubscriptType::Subscript(Expression::Id(
                                    "i".to_string(),
                                    Span { start: 34, end: 35 }
                                ))),
                                span: Span { start: 32, end: 36 }
                            })
                        ],
                        Span { start: 4, end: 38 }
                    )),
                    span: Span { start: 0, end: 38 }
                }))]
            }
        );
    }

    #[test]
    fn parse_tuple_expression_with_no_parens() {
        let parser = Parser::new("1 + 2, True, y(), \"Hello\", l[i],");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Tuple(
                    vec![
                        Expression::BinaryOp(
                            Box::new(Expression::Number("1".to_string(), Span { start: 0, end: 1 })),
                            BinaryOperator::Add,
                            Box::new(Expression::Number("2".to_string(), Span { start: 4, end: 5 })),
                            Span { start: 0, end: 5 }
                        ),
                        Expression::Bool(true, Span { start: 7, end: 11 }),
                        Expression::Call(FunctionCall {
                            lhs: Box::new(Expression::Id("y".to_string(), Span { start: 13, end: 14 })),
                            args: vec![],
                            span: Span { start: 13, end: 17 }
                        }),
                        Expression::String("Hello".to_string(), Span { start: 18, end: 25 }),
                        Expression::Subscript(Subscript {
                            lhs: Box::new(Expression::Id("l".to_string(), Span { start: 27, end: 28 })),
                            slice: Box::new(SubscriptType::Subscript(Expression::Id(
                                "i".to_string(),
                                Span { start: 29, end: 30 }
                            ))),
                            span: Span { start: 27, end: 31 }
                        })
                    ],
                    Span { start: 0, end: 33 }
                ),)]
            }
        )
    }

    #[test]
    fn parse_list_expression() {
        let parser = Parser::new("x = [1 + 2, True, y(), \"Hello\", l[i],]");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id("x".to_string(), Span { start: 0, end: 1 })),
                    rhs: Box::new(Expression::List(
                        vec![
                            Expression::BinaryOp(
                                Box::new(Expression::Number("1".to_string(), Span { start: 5, end: 6 })),
                                BinaryOperator::Add,
                                Box::new(Expression::Number("2".to_string(), Span { start: 9, end: 10 })),
                                Span { start: 5, end: 10 }
                            ),
                            Expression::Bool(true, Span { start: 12, end: 16 }),
                            Expression::Call(FunctionCall {
                                lhs: Box::new(Expression::Id("y".to_string(), Span { start: 18, end: 19 })),
                                args: vec![],
                                span: Span { start: 18, end: 22 }
                            }),
                            Expression::String("Hello".to_string(), Span { start: 23, end: 30 }),
                            Expression::Subscript(Subscript {
                                lhs: Box::new(Expression::Id("l".to_string(), Span { start: 32, end: 33 })),
                                slice: Box::new(SubscriptType::Subscript(Expression::Id(
                                    "i".to_string(),
                                    Span { start: 34, end: 35 }
                                ))),
                                span: Span { start: 32, end: 36 }
                            })
                        ],
                        Span { start: 4, end: 38 }
                    )),
                    span: Span { start: 0, end: 38 }
                }))]
            }
        );
    }

    #[test]
    fn parse_list_expression2() {
        let parser = Parser::new("x = [*l, *[1,2,3], True]");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id("x".to_string(), Span { start: 0, end: 1 })),
                    rhs: Box::new(Expression::List(
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
                    )),
                    span: Span { start: 0, end: 24 }
                }))]
            }
        );
    }

    #[test]
    fn parse_invalid_list_expression() {
        let parser = Parser::new("[**d, x]");
        let (_, errors) = parser.parse();

        assert!(errors.is_some());
        assert_eq!(
            errors,
            Some(vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: can't unpack dictionary inside list!".to_string(),
                span: Span { start: 1, end: 3 },
            },])
        )
    }

    #[test]
    fn parse_empty_list_expression() {
        let parser = Parser::new("[]");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::List(
                    vec![],
                    Span { start: 0, end: 2 }
                ),)]
            }
        );
    }

    #[test]
    fn parse_unpack_iterable_assignment() {
        let parser = Parser::new("x = *iterable");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id("x".to_string(), Span { start: 0, end: 1 })),
                    rhs: Box::new(Expression::UnaryOp(
                        Box::new(Expression::Id("iterable".to_string(), Span { start: 5, end: 13 })),
                        UnaryOperator::UnpackIterable,
                        Span { start: 4, end: 13 }
                    )),
                    span: Span { start: 0, end: 13 }
                }))]
            }
        )
    }

    #[test]
    fn parse_set_expression() {
        let parser = Parser::new("x = {1, True, \"hello\", *l,}");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id("x".to_string(), Span { start: 0, end: 1 })),
                    rhs: Box::new(Expression::Set(
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
                    )),
                    span: Span { start: 0, end: 27 }
                }))]
            }
        )
    }

    #[test]
    fn parse_dict_expression() {
        let parser = Parser::new("x = {1: \"Hello\", 1 + 3: True, (6, 6): False,}");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id("x".to_string(), Span { start: 0, end: 1 })),
                    rhs: Box::new(Expression::Dict(
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
                    )),
                    span: Span { start: 0, end: 45 }
                }))]
            }
        )
    }

    #[test]
    fn parse_dict_expression2() {
        let parser = Parser::new("x = {**d, 2: 5, **x,}");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id("x".to_string(), Span { start: 0, end: 1 })),
                    rhs: Box::new(Expression::Dict(
                        vec![
                            DictItemType::Unpack(Expression::UnaryOp(
                                Box::new(Expression::Id("d".to_string(), Span { start: 7, end: 8 })),
                                UnaryOperator::UnpackDictionary,
                                Span { start: 5, end: 8 }
                            )),
                            DictItemType::KeyValue(
                                Expression::Number("2".to_string(), Span { start: 10, end: 11 }),
                                Expression::Number("5".to_string(), Span { start: 13, end: 14 })
                            ),
                            DictItemType::Unpack(Expression::UnaryOp(
                                Box::new(Expression::Id("x".to_string(), Span { start: 18, end: 19 })),
                                UnaryOperator::UnpackDictionary,
                                Span { start: 16, end: 19 }
                            ))
                        ],
                        Span { start: 4, end: 21 }
                    )),
                    span: Span { start: 0, end: 21 }
                }))]
            }
        )
    }

    #[test]
    fn parse_invalid_dict_expression() {
        let parser = Parser::new("x = {**d, *x}");
        let (_, errors) = parser.parse();

        assert!(errors.is_some());
        assert_eq!(
            errors.unwrap(),
            vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: can't unpack iterable inside dictionary!".to_string(),
                span: Span { start: 10, end: 11 }
            }]
        );
    }

    #[test]
    fn parse_if_else_expression() {
        let parser = Parser::new("x = 15 if 5 < x else 45");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id("x".to_string(), Span { start: 0, end: 1 })),
                    rhs: Box::new(Expression::IfElse(IfElseExpr {
                        lhs: Box::new(Expression::Number("15".to_string(), Span { start: 4, end: 6 })),
                        rhs: Box::new(Expression::Number("45".to_string(), Span { start: 21, end: 23 })),
                        condition: Box::new(Expression::BinaryOp(
                            Box::new(Expression::Number("5".to_string(), Span { start: 10, end: 11 })),
                            BinaryOperator::LessThan,
                            Box::new(Expression::Id("x".to_string(), Span { start: 14, end: 15 })),
                            Span { start: 10, end: 15 }
                        )),
                        span: Span { start: 4, end: 23 }
                    })),
                    span: Span { start: 0, end: 23 }
                }))]
            }
        )
    }

    #[test]
    fn parse_if_else_expression2() {
        let parser = Parser::new("x = func() if (5 < x or x >= y) and is_id else func2() * 5");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id("x".to_string(), Span { start: 0, end: 1 })),
                    rhs: Box::new(Expression::IfElse(IfElseExpr {
                        lhs: Box::new(Expression::Call(FunctionCall {
                            lhs: Box::new(Expression::Id("func".to_string(), Span { start: 4, end: 8 })),
                            args: vec![],
                            span: Span { start: 4, end: 13 }
                        })),
                        rhs: Box::new(Expression::BinaryOp(
                            Box::new(Expression::Call(FunctionCall {
                                lhs: Box::new(Expression::Id("func2".to_string(), Span { start: 47, end: 52 })),
                                args: vec![],
                                span: Span { start: 47, end: 56 }
                            })),
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
                    })),
                    span: Span { start: 0, end: 58 }
                }))]
            }
        )
    }

    #[test]
    fn parse_single_expression() {
        let parser = Parser::new("1 + 2; 3 + 4, 7, 8");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![
                    Statement::Expression(Expression::BinaryOp(
                        Box::new(Expression::Number("1".to_string(), Span { start: 0, end: 1 })),
                        BinaryOperator::Add,
                        Box::new(Expression::Number("2".to_string(), Span { start: 4, end: 5 })),
                        Span { start: 0, end: 5 }
                    ),),
                    Statement::Expression(Expression::Tuple(
                        vec![
                            Expression::BinaryOp(
                                Box::new(Expression::Number("3".to_string(), Span { start: 7, end: 8 })),
                                BinaryOperator::Add,
                                Box::new(Expression::Number("4".to_string(), Span { start: 11, end: 12 })),
                                Span { start: 7, end: 12 }
                            ),
                            Expression::Number("7".to_string(), Span { start: 14, end: 15 }),
                            Expression::Number("8".to_string(), Span { start: 17, end: 18 }),
                        ],
                        Span { start: 7, end: 19 }
                    ),),
                ]
            }
        )
    }

    #[test]
    fn parse_single_expression2() {
        let parser = Parser::new("x");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Id(
                    "x".to_string(),
                    Span { start: 0, end: 1 }
                ),)]
            }
        )
    }

    #[test]
    fn parse_walrus_operator() {
        let parser = Parser::new(
            "if (x := 15) > 5:
    x",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
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
                        stmts: vec![Statement::Expression(Expression::Id(
                            "x".to_string(),
                            Span { start: 22, end: 23 }
                        ),)],
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
    #[ignore = "Handle invalid walrus assignment in statement"]
    fn parse_invalid_walrus_operator() {
        let parser = Parser::new("x := 5 + 5");
        let (_, errors) = parser.parse();

        assert!(errors.is_some());
        assert_eq!(
            errors.unwrap(),
            vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: invalid assignment statement!".to_string(),
                span: Span { start: 2, end: 4 }
            }]
        );
    }

    #[test]
    fn parse_await_operator() {
        let parser = Parser::new("await func() * x ** 5 / 3");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::BinaryOp(
                    Box::new(Expression::BinaryOp(
                        Box::new(Expression::UnaryOp(
                            Box::new(Expression::Call(FunctionCall {
                                lhs: Box::new(Expression::Id("func".to_string(), Span { start: 6, end: 10 })),
                                args: vec![],
                                span: Span { start: 6, end: 14 }
                            })),
                            UnaryOperator::Await,
                            Span { start: 0, end: 14 }
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
                ),)]
            }
        )
    }

    #[test]
    fn parse_lambda_expr() {
        let parser = Parser::new("lambda x: x + 1");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Lambda(LambdaExpr {
                    parameters: vec![FuncParameter {
                        name: "x".to_string(),
                        default_value: None,
                        star_parameter_type: None,
                        span: Span { start: 7, end: 8 }
                    }],
                    expression: Box::new(Expression::BinaryOp(
                        Box::new(Expression::Id("x".to_string(), Span { start: 10, end: 11 })),
                        BinaryOperator::Add,
                        Box::new(Expression::Number("1".to_string(), Span { start: 14, end: 15 })),
                        Span { start: 10, end: 15 }
                    )),
                    span: Span { start: 0, end: 15 }
                }),)]
            }
        )
    }

    #[test]
    fn parse_lambda_expr2() {
        let parser = Parser::new("(lambda x: x + 1)()");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Call(FunctionCall {
                    lhs: Box::new(Expression::Lambda(LambdaExpr {
                        parameters: vec![FuncParameter {
                            name: "x".to_string(),
                            default_value: None,
                            star_parameter_type: None,
                            span: Span { start: 8, end: 9 }
                        }],
                        expression: Box::new(Expression::BinaryOp(
                            Box::new(Expression::Id("x".to_string(), Span { start: 11, end: 12 })),
                            BinaryOperator::Add,
                            Box::new(Expression::Number("1".to_string(), Span { start: 15, end: 16 })),
                            Span { start: 11, end: 16 }
                        )),
                        span: Span { start: 1, end: 16 }
                    })),
                    args: vec![],
                    span: Span { start: 1, end: 20 }
                }),)]
            }
        )
    }

    #[test]
    fn parse_class() {
        let parser = Parser::new(
            "class Test:
    def __init__(self):
        pass",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Class(ClassStmt {
                    name: "Test".to_string(),
                    block: Block {
                        stmts: vec![Statement::FunctionDef(Function {
                            name: "__init__".to_string(),
                            decorators: vec![],
                            parameters: vec![FuncParameter {
                                name: "self".to_string(),
                                default_value: None,
                                star_parameter_type: None,
                                span: Span { start: 29, end: 33 }
                            }],
                            block: Block {
                                stmts: vec![Statement::Pass(Span { start: 44, end: 48 })],
                                span: Span { start: 44, end: 48 }
                            },
                            span: Span { start: 16, end: 48 }
                        })],
                        span: Span { start: 16, end: 48 }
                    },
                    super_classes: vec![],
                    span: Span { start: 0, end: 48 }
                })]
            }
        )
    }

    #[test]
    fn parse_class2() {
        let parser = Parser::new(
            "class Dog(Animal):
    def __init__(self):
        pass",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Class(ClassStmt {
                    name: "Dog".to_string(),
                    block: Block {
                        stmts: vec![Statement::FunctionDef(Function {
                            name: "__init__".to_string(),
                            decorators: vec![],
                            parameters: vec![FuncParameter {
                                name: "self".to_string(),
                                default_value: None,
                                star_parameter_type: None,
                                span: Span { start: 36, end: 40 }
                            }],
                            block: Block {
                                stmts: vec![Statement::Pass(Span { start: 51, end: 55 })],
                                span: Span { start: 51, end: 55 }
                            },
                            span: Span { start: 23, end: 55 }
                        })],
                        span: Span { start: 23, end: 55 }
                    },
                    base_classes: vec![Expression::Id("Animal".to_string(), Span { start: 10, end: 16 })],
                    span: Span { start: 0, end: 55 }
                })]
            }
        )
    }

    #[test]
    fn parse_import() {
        let parser = Parser::new("import os");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Import(ImportStmt {
                    modules: vec![ImportModule {
                        name: vec!["os".to_string()],
                        alias: None
                    }],
                    span: Span { start: 0, end: 9 }
                })]
            }
        )
    }

    #[test]
    fn parse_import2() {
        let parser = Parser::new("import os.walk as O, sys as S");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Import(ImportStmt {
                    modules: vec![
                        ImportModule {
                            name: vec!["os".to_string(), "walk".to_string()],
                            alias: Some("O".to_string())
                        },
                        ImportModule {
                            name: vec!["sys".to_string()],
                            alias: Some("S".to_string())
                        }
                    ],
                    span: Span { start: 0, end: 29 }
                })]
            }
        )
    }

    #[test]
    fn parse_from_import() {
        let parser = Parser::new("from os import *");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FromImport(FromImportStmt {
                    module: vec![ImportModule {
                        name: vec!["os".to_string()],
                        alias: None
                    }],
                    targets: vec![ImportModule {
                        name: vec!["*".to_string()],
                        alias: None
                    }],
                    span: Span { start: 0, end: 16 }
                })]
            }
        )
    }

    #[test]
    fn parse_from_import2() {
        let parser = Parser::new("from ... import *");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FromImport(FromImportStmt {
                    module: vec![ImportModule {
                        name: vec![".".to_string(), ".".to_string(), ".".to_string()],
                        alias: None
                    }],
                    targets: vec![ImportModule {
                        name: vec!["*".to_string()],
                        alias: None
                    }],
                    span: Span { start: 0, end: 17 }
                })]
            }
        )
    }

    #[test]
    fn parse_from_import3() {
        let parser = Parser::new("from .subpackage.module1 import func");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FromImport(FromImportStmt {
                    module: vec![
                        ImportModule {
                            name: vec![".".to_string()],
                            alias: None
                        },
                        ImportModule {
                            name: vec!["subpackage".to_string(), "module1".to_string()],
                            alias: None
                        }
                    ],
                    targets: vec![ImportModule {
                        name: vec!["func".to_string()],
                        alias: None
                    }],
                    span: Span { start: 0, end: 36 }
                })]
            }
        )
    }

    #[test]
    fn parse_from_import4() {
        let parser = Parser::new("from .subpackage.module1 import (func, func2, func3)");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FromImport(FromImportStmt {
                    module: vec![
                        ImportModule {
                            name: vec![".".to_string()],
                            alias: None
                        },
                        ImportModule {
                            name: vec!["subpackage".to_string(), "module1".to_string()],
                            alias: None
                        }
                    ],
                    targets: vec![
                        ImportModule {
                            name: vec!["func".to_string()],
                            alias: None
                        },
                        ImportModule {
                            name: vec!["func2".to_string()],
                            alias: None
                        },
                        ImportModule {
                            name: vec!["func3".to_string()],
                            alias: None
                        }
                    ],
                    span: Span { start: 0, end: 51 }
                })]
            }
        )
    }

    #[test]
    fn parse_with() {
        let parser = Parser::new(
            "with open() as file:
    pass",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::With(WithStmt {
                    items: vec![WithItem {
                        item: Expression::Call(FunctionCall {
                            lhs: Box::new(Expression::Id("open".to_string(), Span { start: 5, end: 9 })),
                            args: vec![],
                            span: Span { start: 5, end: 14 }
                        }),
                        target: Some(Expression::Id("file".to_string(), Span { start: 15, end: 19 })),
                        span: Span { start: 5, end: 19 }
                    }],
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
    fn parse_try_except() {
        let parser = Parser::new(
            "try:
    pass
except:
    pass",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Try(TryStmt {
                    block: Block {
                        stmts: vec![Statement::Pass(Span { start: 9, end: 13 })],
                        span: Span { start: 9, end: 13 }
                    },
                    finally_block: None,
                    except_blocks: vec![ExceptBlock {
                        block: Block {
                            stmts: vec![Statement::Pass(Span { start: 26, end: 30 })],
                            span: Span { start: 26, end: 30 }
                        },
                        kind: ExceptBlockKind::Except,
                        expr: None,
                        expr_alias: None,
                        span: Span { start: 14, end: 30 }
                    }],
                    else_stmt: None,
                    span: Span { start: 0, end: 30 }
                })]
            }
        )
    }

    #[test]
    fn parse_try_except_as() {
        let parser = Parser::new(
            "try:
    pass
except Except as e:
    pass",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Try(TryStmt {
                    block: Block {
                        stmts: vec![Statement::Pass(Span { start: 9, end: 13 })],
                        span: Span { start: 9, end: 13 }
                    },
                    finally_block: None,
                    except_blocks: vec![ExceptBlock {
                        block: Block {
                            stmts: vec![Statement::Pass(Span { start: 38, end: 42 })],
                            span: Span { start: 38, end: 42 }
                        },
                        kind: ExceptBlockKind::Except,
                        expr: Some(Expression::Id("Except".to_string(), Span { start: 21, end: 27 })),
                        expr_alias: Some("e".to_string()),
                        span: Span { start: 14, end: 42 }
                    }],
                    else_stmt: None,
                    span: Span { start: 0, end: 42 }
                })]
            }
        )
    }

    #[test]
    fn parse_try_finally() {
        let parser = Parser::new(
            "try:
    pass
finally:
    pass",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Try(TryStmt {
                    block: Block {
                        stmts: vec![Statement::Pass(Span { start: 9, end: 13 })],
                        span: Span { start: 9, end: 13 }
                    },
                    finally_block: Some(FinallyBlock {
                        block: Block {
                            stmts: vec![Statement::Pass(Span { start: 27, end: 31 })],
                            span: Span { start: 27, end: 31 }
                        },
                        span: Span { start: 14, end: 31 }
                    }),
                    except_blocks: vec![],
                    else_stmt: None,
                    span: Span { start: 0, end: 31 }
                })]
            }
        )
    }

    #[test]
    fn parse_try_except_finally() {
        let parser = Parser::new(
            "try:
    pass
except:
    pass
finally:
    pass",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Try(TryStmt {
                    block: Block {
                        stmts: vec![Statement::Pass(Span { start: 9, end: 13 })],
                        span: Span { start: 9, end: 13 }
                    },
                    finally_block: Some(FinallyBlock {
                        block: Block {
                            stmts: vec![Statement::Pass(Span { start: 44, end: 48 })],
                            span: Span { start: 44, end: 48 }
                        },
                        span: Span { start: 31, end: 48 }
                    }),
                    except_blocks: vec![ExceptBlock {
                        block: Block {
                            stmts: vec![Statement::Pass(Span { start: 26, end: 30 })],
                            span: Span { start: 26, end: 30 }
                        },
                        kind: ExceptBlockKind::Except,
                        expr: None,
                        expr_alias: None,
                        span: Span { start: 14, end: 30 }
                    }],
                    else_stmt: None,
                    span: Span { start: 0, end: 48 }
                })]
            }
        )
    }

    #[test]
    fn parse_try_except_else_finally() {
        let parser = Parser::new(
            "try:
    pass
except:
    pass
else:
    pass
finally:
    pass",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Try(TryStmt {
                    block: Block {
                        stmts: vec![Statement::Pass(Span { start: 9, end: 13 })],
                        span: Span { start: 9, end: 13 }
                    },
                    finally_block: Some(FinallyBlock {
                        block: Block {
                            stmts: vec![Statement::Pass(Span { start: 59, end: 63 })],
                            span: Span { start: 59, end: 63 }
                        },
                        span: Span { start: 46, end: 63 }
                    }),
                    except_blocks: vec![ExceptBlock {
                        block: Block {
                            stmts: vec![Statement::Pass(Span { start: 26, end: 30 })],
                            span: Span { start: 26, end: 30 }
                        },
                        kind: ExceptBlockKind::Except,
                        expr: None,
                        expr_alias: None,
                        span: Span { start: 14, end: 30 }
                    }],
                    else_stmt: Some(ElseStmt {
                        block: Block {
                            stmts: vec![Statement::Pass(Span { start: 41, end: 45 })],
                            span: Span { start: 41, end: 45 }
                        },
                        span: Span { start: 31, end: 45 }
                    }),
                    span: Span { start: 0, end: 63 }
                })]
            }
        )
    }

    #[test]
    fn parse_try_with_multiple_excepts() {
        let parser = Parser::new(
            "
try:
    pass
except:
    1 + 1
except:
    pass
",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Try(TryStmt {
                    block: Block {
                        stmts: vec![Statement::Pass(Span { start: 10, end: 14 })],
                        span: Span { start: 10, end: 14 }
                    },
                    finally_block: None,
                    except_blocks: vec![
                        ExceptBlock {
                            block: Block {
                                stmts: vec![Statement::Expression(Expression::BinaryOp(
                                    Box::new(Expression::Number("1".to_string(), Span { start: 27, end: 28 })),
                                    BinaryOperator::Add,
                                    Box::new(Expression::Number("1".to_string(), Span { start: 31, end: 32 })),
                                    Span { start: 27, end: 32 }
                                ))],
                                span: Span { start: 27, end: 32 }
                            },
                            kind: ExceptBlockKind::Except,
                            expr: None,
                            expr_alias: None,
                            span: Span { start: 15, end: 32 }
                        },
                        ExceptBlock {
                            block: Block {
                                stmts: vec![Statement::Pass(Span { start: 45, end: 49 })],
                                span: Span { start: 45, end: 49 }
                            },
                            kind: ExceptBlockKind::Except,
                            expr: None,
                            expr_alias: None,
                            span: Span { start: 33, end: 49 }
                        }
                    ],
                    else_stmt: None,
                    span: Span { start: 1, end: 49 }
                })]
            }
        )
    }

    #[test]
    fn parse_return_stmt() {
        let parser = Parser::new(
            "
def x():
    return
",
        );
        let (parsed_file, errors) = parser.parse();
        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "x".to_string(),
                    parameters: vec![],
                    decorators: vec![],
                    block: Block {
                        stmts: vec![Statement::Return(ReturnStmt {
                            value: None,
                            span: Span { start: 14, end: 21 }
                        })],
                        span: Span { start: 14, end: 21 }
                    },
                    span: Span { start: 1, end: 21 }
                })]
            }
        )
    }

    #[test]
    fn parse_return_stmt2() {
        let parser = Parser::new(
            "
def x():
    return 2 + 2
",
        );
        let (parsed_file, errors) = parser.parse();
        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "x".to_string(),
                    parameters: vec![],
                    decorators: vec![],
                    block: Block {
                        stmts: vec![Statement::Return(ReturnStmt {
                            value: Some(Expression::BinaryOp(
                                Box::new(Expression::Number("2".to_string(), Span { start: 21, end: 22 })),
                                BinaryOperator::Add,
                                Box::new(Expression::Number("2".to_string(), Span { start: 25, end: 26 })),
                                Span { start: 21, end: 26 }
                            )),
                            span: Span { start: 14, end: 26 }
                        })],
                        span: Span { start: 14, end: 26 }
                    },
                    span: Span { start: 1, end: 26 }
                })]
            }
        )
    }

    #[test]
    fn parse_yield_expr() {
        let parser = Parser::new(
            "
def test():
    yield 1
    yield 1, 2 + 3, abc
",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "test".to_string(),
                    parameters: vec![],
                    decorators: vec![],
                    block: Block {
                        stmts: vec![
                            Statement::Expression(Expression::Yield(
                                Some(Box::new(Expression::Number(
                                    "1".to_string(),
                                    Span { start: 23, end: 24 }
                                ))),
                                Span { start: 17, end: 24 }
                            ),),
                            Statement::Expression(Expression::Yield(
                                Some(Box::new(Expression::Tuple(
                                    vec![
                                        Expression::Number("1".to_string(), Span { start: 35, end: 36 }),
                                        Expression::BinaryOp(
                                            Box::new(Expression::Number("2".to_string(), Span { start: 38, end: 39 })),
                                            BinaryOperator::Add,
                                            Box::new(Expression::Number("3".to_string(), Span { start: 42, end: 43 })),
                                            Span { start: 38, end: 43 }
                                        ),
                                        Expression::Id("abc".to_string(), Span { start: 45, end: 48 })
                                    ],
                                    Span { start: 35, end: 49 }
                                ))),
                                Span { start: 29, end: 49 }
                            ),)
                        ],
                        span: Span { start: 17, end: 49 }
                    },
                    span: Span { start: 1, end: 49 }
                }),]
            }
        );
    }

    #[test]
    fn parse_yield_from() {
        let parser = Parser::new(
            "
def test():
    yield from func()
",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "test".to_string(),
                    parameters: vec![],
                    decorators: vec![],
                    block: Block {
                        stmts: vec![Statement::Expression(Expression::YieldFrom(
                            Box::new(Expression::Call(FunctionCall {
                                lhs: Box::new(Expression::Id("func".to_string(), Span { start: 28, end: 32 })),
                                args: vec![],
                                span: Span { start: 28, end: 35 }
                            })),
                            Span { start: 17, end: 35 }
                        ),)],
                        span: Span { start: 17, end: 35 }
                    },
                    span: Span { start: 1, end: 35 }
                })]
            }
        );
    }

    #[test]
    fn parse_for_stmt() {
        let parser = Parser::new(
            "
for i in [1, 2, 3]:
    yield i
    yield i + 1
",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::For(ForStmt {
                    target: Expression::Id("i".to_string(), Span { start: 5, end: 6 }),
                    iter: Expression::List(
                        vec![
                            Expression::Number("1".to_string(), Span { start: 11, end: 12 }),
                            Expression::Number("2".to_string(), Span { start: 14, end: 15 }),
                            Expression::Number("3".to_string(), Span { start: 17, end: 18 })
                        ],
                        Span { start: 10, end: 19 }
                    ),
                    block: Block {
                        stmts: vec![
                            Statement::Expression(Expression::Yield(
                                Some(Box::new(Expression::Id("i".to_string(), Span { start: 31, end: 32 }))),
                                Span { start: 25, end: 32 }
                            ),),
                            Statement::Expression(Expression::Yield(
                                Some(Box::new(Expression::BinaryOp(
                                    Box::new(Expression::Id("i".to_string(), Span { start: 43, end: 44 })),
                                    BinaryOperator::Add,
                                    Box::new(Expression::Number("1".to_string(), Span { start: 47, end: 48 })),
                                    Span { start: 43, end: 48 }
                                ))),
                                Span { start: 37, end: 48 }
                            ),)
                        ],
                        span: Span { start: 25, end: 48 }
                    },
                    else_stmt: None,
                    span: Span { start: 1, end: 48 }
                })]
            }
        );
    }

    #[test]
    fn parse_for_stmt2() {
        let parser = Parser::new(
            "
for a, b in ((1, 2, 3)):
    ...
",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::For(ForStmt {
                    target: Expression::Tuple(
                        vec![
                            Expression::Id("a".to_string(), Span { start: 5, end: 6 }),
                            Expression::Id("b".to_string(), Span { start: 8, end: 9 })
                        ],
                        Span { start: 5, end: 12 }
                    ),
                    iter: Expression::Tuple(
                        vec![Expression::Tuple(
                            vec![
                                Expression::Number("1".to_string(), Span { start: 15, end: 16 }),
                                Expression::Number("2".to_string(), Span { start: 18, end: 19 }),
                                Expression::Number("3".to_string(), Span { start: 21, end: 22 })
                            ],
                            Span { start: 14, end: 23 }
                        )],
                        Span { start: 13, end: 24 }
                    ),
                    block: Block {
                        stmts: vec![Statement::Expression(Expression::Ellipsis(Span { start: 30, end: 33 }),)],
                        span: Span { start: 30, end: 33 }
                    },
                    else_stmt: None,
                    span: Span { start: 1, end: 33 }
                })]
            }
        )
    }

    #[test]
    fn parse_for_stmt3() {
        let parser = Parser::new(
            "
for *a in ((1, 2, 3)):
    ...
",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::For(ForStmt {
                    target: Expression::UnaryOp(
                        Box::new(Expression::Id("a".to_string(), Span { start: 6, end: 7 })),
                        UnaryOperator::UnpackIterable,
                        Span { start: 5, end: 7 }
                    ),
                    iter: Expression::Tuple(
                        vec![Expression::Tuple(
                            vec![
                                Expression::Number("1".to_string(), Span { start: 13, end: 14 }),
                                Expression::Number("2".to_string(), Span { start: 16, end: 17 }),
                                Expression::Number("3".to_string(), Span { start: 19, end: 20 })
                            ],
                            Span { start: 12, end: 21 }
                        )],
                        Span { start: 11, end: 22 }
                    ),
                    block: Block {
                        stmts: vec![Statement::Expression(Expression::Ellipsis(Span { start: 28, end: 31 }),)],
                        span: Span { start: 28, end: 31 }
                    },
                    else_stmt: None,
                    span: Span { start: 1, end: 31 }
                })]
            }
        )
    }

    #[test]
    fn parse_raise_stmt() {
        let parser = Parser::new(
            "
raise
raise Exception
raise Exception from e
",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![
                    Statement::Raise(RaiseStmt {
                        exc: None,
                        from: None,
                        span: Span { start: 1, end: 0 }
                    }),
                    Statement::Raise(RaiseStmt {
                        exc: Some(Expression::Id("Exception".to_string(), Span { start: 13, end: 22 })),
                        from: None,
                        span: Span { start: 7, end: 22 }
                    }),
                    Statement::Raise(RaiseStmt {
                        exc: Some(Expression::Id("Exception".to_string(), Span { start: 29, end: 38 })),
                        from: Some(Expression::Id("e".to_string(), Span { start: 44, end: 45 })),
                        span: Span { start: 23, end: 45 }
                    })
                ]
            }
        );
    }

    #[test]
    fn parse_del_stmt() {
        let parser = Parser::new(
            "
del a
del b, c, d
",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![
                    Statement::Del(DelStmt {
                        expr: Expression::Id("a".to_string(), Span { start: 5, end: 6 }),
                        span: Span { start: 1, end: 6 }
                    }),
                    Statement::Del(DelStmt {
                        expr: Expression::Tuple(
                            vec![
                                Expression::Id("b".to_string(), Span { start: 11, end: 12 }),
                                Expression::Id("c".to_string(), Span { start: 14, end: 15 }),
                                Expression::Id("d".to_string(), Span { start: 17, end: 18 })
                            ],
                            Span { start: 11, end: 19 }
                        ),
                        span: Span { start: 7, end: 19 }
                    })
                ]
            }
        )
    }

    #[test]
    fn parse_function_call_with_arguments() {
        let parser = Parser::new("hello(1 + 2, True, y = None)");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Call(FunctionCall {
                    lhs: Box::new(Expression::Id("hello".to_string(), Span { start: 0, end: 5 })),
                    args: vec![
                        Expression::BinaryOp(
                            Box::new(Expression::Number("1".to_string(), Span { start: 6, end: 7 })),
                            BinaryOperator::Add,
                            Box::new(Expression::Number("2".to_string(), Span { start: 10, end: 11 })),
                            Span { start: 6, end: 11 }
                        ),
                        Expression::Bool(true, Span { start: 13, end: 17 }),
                        Expression::Assign(Assign {
                            lhs: Box::new(Expression::Id("y".to_string(), Span { start: 19, end: 20 })),
                            rhs: Box::new(Expression::None(Span { start: 23, end: 27 })),
                            span: Span { start: 19, end: 27 }
                        })
                    ],
                    span: Span { start: 0, end: 29 }
                }))]
            }
        )
    }

    #[test]
    fn parse_subscript_with_string() {
        let parser = Parser::new("l[\"x\"]");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Subscript(Subscript {
                    lhs: Box::new(Expression::Id("l".to_string(), Span { start: 0, end: 1 })),
                    slice: Box::new(SubscriptType::Subscript(Expression::String(
                        "x".to_string(),
                        Span { start: 2, end: 5 }
                    ))),
                    span: Span { start: 0, end: 6 }
                }))]
            }
        )
    }

    #[test]
    fn parse_subscript_with_slice() {
        let parser = Parser::new(
            "
l[1:]
l[:1]
l[1:2]
l[1:2:3]
l[::]
l[:]
",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![
                    Statement::Expression(Expression::Subscript(Subscript {
                        lhs: Box::new(Expression::Id("l".to_string(), Span { start: 1, end: 2 })),
                        slice: Box::new(SubscriptType::Slice {
                            lower: Some(Expression::Number("1".to_string(), Span { start: 3, end: 4 })),
                            upper: None,
                            step: None
                        }),
                        span: Span { start: 1, end: 6 }
                    })),
                    Statement::Expression(Expression::Subscript(Subscript {
                        lhs: Box::new(Expression::Id("l".to_string(), Span { start: 7, end: 8 })),
                        slice: Box::new(SubscriptType::Slice {
                            lower: None,
                            upper: Some(Expression::Number("1".to_string(), Span { start: 10, end: 11 })),
                            step: None
                        }),
                        span: Span { start: 7, end: 12 }
                    })),
                    Statement::Expression(Expression::Subscript(Subscript {
                        lhs: Box::new(Expression::Id("l".to_string(), Span { start: 13, end: 14 })),
                        slice: Box::new(SubscriptType::Slice {
                            lower: Some(Expression::Number("1".to_string(), Span { start: 15, end: 16 })),
                            upper: Some(Expression::Number("2".to_string(), Span { start: 17, end: 18 })),
                            step: None
                        }),
                        span: Span { start: 13, end: 19 }
                    })),
                    Statement::Expression(Expression::Subscript(Subscript {
                        lhs: Box::new(Expression::Id("l".to_string(), Span { start: 20, end: 21 })),
                        slice: Box::new(SubscriptType::Slice {
                            lower: Some(Expression::Number("1".to_string(), Span { start: 22, end: 23 })),
                            upper: Some(Expression::Number("2".to_string(), Span { start: 24, end: 25 })),
                            step: Some(Expression::Number("3".to_string(), Span { start: 26, end: 27 }))
                        }),
                        span: Span { start: 20, end: 28 }
                    })),
                    Statement::Expression(Expression::Subscript(Subscript {
                        lhs: Box::new(Expression::Id("l".to_string(), Span { start: 29, end: 30 })),
                        slice: Box::new(SubscriptType::Slice {
                            lower: None,
                            upper: None,
                            step: None
                        }),
                        span: Span { start: 29, end: 34 }
                    })),
                    Statement::Expression(Expression::Subscript(Subscript {
                        lhs: Box::new(Expression::Id("l".to_string(), Span { start: 35, end: 36 })),
                        slice: Box::new(SubscriptType::Slice {
                            lower: None,
                            upper: None,
                            step: None
                        }),
                        span: Span { start: 35, end: 39 }
                    }))
                ]
            }
        )
    }

    #[test]
    fn parse_function_with_decorators() {
        let parser = Parser::new(
            "
@abc
@abc.cde
def test():
   ...
",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "test".to_string(),
                    parameters: vec![],
                    block: Block {
                        stmts: vec![Statement::Expression(Expression::Ellipsis(Span { start: 30, end: 33 }))],
                        span: Span { start: 30, end: 33 }
                    },
                    decorators: vec![
                        Expression::Id("abc".to_string(), Span { start: 2, end: 5 }),
                        Expression::BinaryOp(
                            Box::new(Expression::Id("abc".to_string(), Span { start: 7, end: 10 })),
                            BinaryOperator::AttributeRef,
                            Box::new(Expression::Id("cde".to_string(), Span { start: 11, end: 14 })),
                            Span { start: 7, end: 14 }
                        )
                    ],
                    span: Span { start: 1, end: 33 }
                })]
            }
        )
    }

    #[test]
    fn parse_assert_stmt() {
        let parser = Parser::new("assert 1 > x");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Assert(AssertStmt {
                    expr: Expression::BinaryOp(
                        Box::new(Expression::Number("1".to_string(), Span { start: 7, end: 8 })),
                        BinaryOperator::GreaterThan,
                        Box::new(Expression::Id("x".to_string(), Span { start: 11, end: 12 })),
                        Span { start: 7, end: 12 }
                    ),
                    span: Span { start: 0, end: 12 }
                })]
            }
        )
    }
}
