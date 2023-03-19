#[cfg(test)]
mod tests_parser {
    use pretty_assertions::assert_eq;
    use python_parser::{
        error::{PythonError, PythonErrorType},
        lexer::{span::Span, Lexer},
        parser::{
            ast::{
                AnnAssign, AssertStmt, Assign, AugAssign, AugAssignType, BinaryOperator, Block, ClassStmt, DelStmt,
                DictItemType, ElIfStmt, ElseStmt, ExceptBlock, ExceptBlockKind, Expression, FinallyBlock, ForComp,
                ForStmt, FromImportStmt, FuncParameter, Function, FunctionCall, GeneratorComp, GlobalStmt, IfComp,
                IfElseExpr, IfStmt, ImportModule, ImportStmt, LambdaExpr, ListComp, NonLocalStmt, ParsedFile,
                RaiseStmt, ReturnStmt, StarParameterType, Statement, Subscript, SubscriptType, TryStmt, UnaryOperator,
                While, WithItem, WithStmt,
            },
            Parser,
        },
    };

    #[test]
    fn parse_string_assignment() {
        let mut lexer = Lexer::new("test = \"Hello World!\"");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "test".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 4
                        }
                    )),
                    rhs: Box::new(Expression::String(
                        "Hello World!".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 8,
                            column_end: 21
                        }
                    )),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 21
                    }
                },))]
            }
        );
    }

    #[test]
    fn parse_multiple_numbers_assignment() {
        let mut lexer = Lexer::new("test = 42; x = 12");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![
                    Statement::Expression(Expression::Assign(Assign {
                        lhs: Box::new(Expression::Id(
                            "test".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 1,
                                column_end: 4
                            }
                        )),
                        rhs: Box::new(Expression::Number(
                            "42".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 8,
                                column_end: 9
                            }
                        )),
                        span: Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 9
                        }
                    },)),
                    Statement::Expression(Expression::Assign(Assign {
                        lhs: Box::new(Expression::Id(
                            "x".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 12,
                                column_end: 12
                            }
                        )),
                        rhs: Box::new(Expression::Number(
                            "12".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 16,
                                column_end: 17
                            }
                        )),
                        span: Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 12,
                            column_end: 17
                        }
                    },))
                ]
            }
        );
    }

    #[test]
    fn parse_boolean_assignment() {
        let mut lexer = Lexer::new("x = True\ny = False");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![
                    Statement::Expression(Expression::Assign(Assign {
                        lhs: Box::new(Expression::Id(
                            "x".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 1,
                                column_end: 1
                            }
                        )),
                        rhs: Box::new(Expression::Bool(
                            true,
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 5,
                                column_end: 8
                            }
                        )),
                        span: Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 8
                        }
                    },)),
                    Statement::Expression(Expression::Assign(Assign {
                        lhs: Box::new(Expression::Id(
                            "y".into(),
                            Span {
                                row_start: 2,
                                row_end: 2,
                                column_start: 1,
                                column_end: 1
                            }
                        )),
                        rhs: Box::new(Expression::Bool(
                            false,
                            Span {
                                row_start: 2,
                                row_end: 2,
                                column_start: 5,
                                column_end: 9
                            }
                        )),
                        span: Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 1,
                            column_end: 9
                        }
                    },))
                ]
            }
        )
    }

    #[test]
    fn parse_incorrect_assignment() {
        let mut lexer = Lexer::new("test =");
        let parser = Parser::new(&mut lexer);
        let (_, errors) = parser.parse();

        assert!(errors.is_some());
        assert_eq!(
            errors.unwrap(),
            vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: unexpected token Eof".into(),
                span: Span {
                    row_start: 1,
                    row_end: 1,
                    column_start: 7,
                    column_end: 7
                },
            },]
        )
    }

    #[test]
    fn parse_assignment_with_typehint() {
        let mut lexer = Lexer::new("x: int = 0");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::AnnAssign(AnnAssign {
                    lhs: Box::new(Expression::Id(
                        "x".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Some(Box::new(Expression::Number(
                        "0".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 10,
                            column_end: 10
                        }
                    ))),
                    typehint: Box::new(Expression::Id(
                        "int".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 4,
                            column_end: 6
                        }
                    )),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 10
                    }
                }))]
            }
        )
    }

    #[test]
    fn parse_tuple_unpacking_assignment() {
        let mut lexer = Lexer::new("a, b, c = 1, 2, 3");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Tuple(
                        vec![
                            Expression::Id(
                                "a".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 1,
                                    column_end: 1
                                }
                            ),
                            Expression::Id(
                                "b".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 4,
                                    column_end: 4
                                }
                            ),
                            Expression::Id(
                                "c".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 7,
                                    column_end: 7
                                }
                            )
                        ],
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 7
                        }
                    )),
                    rhs: Box::new(Expression::Tuple(
                        vec![
                            Expression::Number(
                                "1".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 11,
                                    column_end: 11
                                }
                            ),
                            Expression::Number(
                                "2".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 14,
                                    column_end: 14
                                }
                            ),
                            Expression::Number(
                                "3".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 17,
                                    column_end: 17
                                }
                            )
                        ],
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 11,
                            column_end: 17
                        }
                    )),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 17
                    }
                }))]
            }
        )
    }

    #[test]
    fn parse_invalid_tuple_assignment_with_typehint() {
        let mut lexer = Lexer::new("a, b, c: tuple[int, int, int] = 1, 2, 3");
        let parser = Parser::new(&mut lexer);
        let (_, errors) = parser.parse();

        assert!(errors.is_some());
        assert_eq!(
            errors.unwrap(),
            vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: only single target (not tuple) can be annotated".into(),
                span: Span {
                    row_start: 1,
                    row_end: 1,
                    column_start: 1,
                    column_end: 8
                },
            },]
        )
    }

    #[test]
    fn parse_list_unpacking_assignment() {
        let mut lexer = Lexer::new("[a, b, c] = 1, 2, 3");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::List(
                        vec![
                            Expression::Id(
                                "a".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 2,
                                    column_end: 2
                                }
                            ),
                            Expression::Id(
                                "b".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 5,
                                    column_end: 5
                                }
                            ),
                            Expression::Id(
                                "c".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 8,
                                    column_end: 8
                                }
                            )
                        ],
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 9
                        }
                    )),
                    rhs: Box::new(Expression::Tuple(
                        vec![
                            Expression::Number(
                                "1".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 13,
                                    column_end: 13
                                }
                            ),
                            Expression::Number(
                                "2".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 16,
                                    column_end: 16
                                }
                            ),
                            Expression::Number(
                                "3".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 19,
                                    column_end: 19
                                }
                            )
                        ],
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 13,
                            column_end: 19
                        }
                    )),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 19
                    }
                }))]
            }
        )
    }

    #[test]
    fn parse_assignment_wiht_only_typehint() {
        let mut lexer = Lexer::new("x: int");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::AnnAssign(AnnAssign {
                    lhs: Box::new(Expression::Id(
                        "x".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: None,
                    typehint: Box::new(Expression::Id(
                        "int".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 4,
                            column_end: 6
                        }
                    )),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 6
                    }
                }))]
            }
        )
    }

    #[test]
    fn parse_aug_assignment() {
        let mut lexer = Lexer::new("x += 1");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::AugAssing(AugAssign {
                    lhs: Box::new(Expression::Id(
                        "x".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::Number(
                        "1".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 6,
                            column_end: 6
                        }
                    )),
                    kind: AugAssignType::Plus,
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 6
                    }
                }))]
            }
        )
    }

    #[test]
    fn parse_invalid_aug_assignment() {
        let mut lexer = Lexer::new("x: int += 1");
        let parser = Parser::new(&mut lexer);
        let (_, errors) = parser.parse();

        assert!(errors.is_some());
        assert_eq!(
            errors.unwrap(),
            vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: invalid syntax, typehint not allowed in this kind of expression".into(),
                span: Span {
                    row_start: 1,
                    row_end: 1,
                    column_start: 1,
                    column_end: 6
                },
            }],
        )
    }

    #[test]
    fn parse_function() {
        let mut lexer = Lexer::new(
            "def x():
    pass",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, parser_errors) = parser.parse();

        assert!(parser_errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "x".into(),
                    decorators: vec![],
                    block: Block {
                        stmts: vec![Statement::Pass(Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        })],
                        span: Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        }
                    },
                    span: Span {
                        row_start: 1,
                        row_end: 2,
                        column_start: 1,
                        column_end: 8
                    },
                    parameters: vec![]
                })]
            }
        )
    }

    #[test]
    fn parse_function2() {
        let mut lexer = Lexer::new(
            "def x():
    pass
    pass
    pass",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "x".into(),
                    parameters: vec![],
                    decorators: vec![],
                    block: Block {
                        stmts: vec![
                            Statement::Pass(Span {
                                row_start: 2,
                                row_end: 2,
                                column_start: 5,
                                column_end: 8
                            }),
                            Statement::Pass(Span {
                                row_start: 3,
                                row_end: 3,
                                column_start: 5,
                                column_end: 8
                            }),
                            Statement::Pass(Span {
                                row_start: 4,
                                row_end: 4,
                                column_start: 5,
                                column_end: 8
                            })
                        ],
                        span: Span {
                            row_start: 2,
                            row_end: 4,
                            column_start: 5,
                            column_end: 8
                        }
                    },
                    span: Span {
                        row_start: 1,
                        row_end: 4,
                        column_start: 1,
                        column_end: 8
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_function3() {
        let mut lexer = Lexer::new(
            "def test(x, y = 42):
    pass",
        );
        let parser = Parser::new(&mut lexer);

        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "test".into(),
                    decorators: vec![],
                    parameters: vec![
                        FuncParameter {
                            name: "x".into(),
                            default_value: None,
                            span: Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 10,
                                column_end: 10
                            },
                            star_parameter_type: None,
                            is_kw_only: false,
                            is_pos_only: false
                        },
                        FuncParameter {
                            name: "y".into(),
                            default_value: Some(Expression::Number(
                                "42".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 17,
                                    column_end: 18
                                }
                            )),
                            span: Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 13,
                                column_end: 18
                            },
                            star_parameter_type: None,
                            is_kw_only: false,
                            is_pos_only: false
                        }
                    ],
                    block: Block {
                        stmts: vec![Statement::Pass(Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        })],
                        span: Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        }
                    },
                    span: Span {
                        row_start: 1,
                        row_end: 2,
                        column_start: 1,
                        column_end: 8
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_function4() {
        let mut lexer = Lexer::new(
            "def test(*kargs, **kwargs):
    pass",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "test".into(),
                    decorators: vec![],
                    parameters: vec![
                        FuncParameter {
                            name: "kargs".into(),
                            default_value: None,
                            span: Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 11,
                                column_end: 15
                            },
                            star_parameter_type: Some(StarParameterType::Kargs),
                            is_kw_only: false,
                            is_pos_only: false
                        },
                        FuncParameter {
                            name: "kwargs".into(),
                            default_value: None,
                            span: Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 20,
                                column_end: 25
                            },
                            star_parameter_type: Some(StarParameterType::KWargs),
                            is_kw_only: false,
                            is_pos_only: false
                        }
                    ],
                    block: Block {
                        stmts: vec![Statement::Pass(Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        })],
                        span: Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        }
                    },
                    span: Span {
                        row_start: 1,
                        row_end: 2,
                        column_start: 1,
                        column_end: 8
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_if() {
        let mut lexer = Lexer::new(
            "if True:
    pass
    ",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::If(IfStmt {
                    condition: Expression::Bool(
                        true,
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 4,
                            column_end: 7
                        }
                    ),
                    block: Block {
                        stmts: vec![Statement::Pass(Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        })],
                        span: Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        }
                    },
                    elif_stms: vec![],
                    else_stmt: None,
                    span: Span {
                        row_start: 1,
                        row_end: 2,
                        column_start: 1,
                        column_end: 8
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_if_else() {
        let mut lexer = Lexer::new(
            "if True:
    pass
else:
    pass",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::If(IfStmt {
                    condition: Expression::Bool(
                        true,
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 4,
                            column_end: 7
                        }
                    ),
                    block: Block {
                        stmts: vec![Statement::Pass(Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        })],
                        span: Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        }
                    },
                    elif_stms: vec![],
                    else_stmt: Some(ElseStmt {
                        block: Block {
                            stmts: vec![Statement::Pass(Span {
                                row_start: 4,
                                row_end: 4,
                                column_start: 5,
                                column_end: 8
                            })],
                            span: Span {
                                row_start: 4,
                                row_end: 4,
                                column_start: 5,
                                column_end: 8
                            }
                        },
                        span: Span {
                            row_start: 3,
                            row_end: 4,
                            column_start: 1,
                            column_end: 8
                        },
                    }),
                    span: Span {
                        row_start: 1,
                        row_end: 4,
                        column_start: 1,
                        column_end: 8
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_if_elif() {
        let mut lexer = Lexer::new(
            "if True:
    pass
elif True:
    pass
elif True:
    pass",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::If(IfStmt {
                    condition: Expression::Bool(
                        true,
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 4,
                            column_end: 7
                        }
                    ),
                    block: Block {
                        stmts: vec![Statement::Pass(Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        })],
                        span: Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        },
                    },
                    elif_stms: vec![
                        ElIfStmt {
                            condition: Expression::Bool(
                                true,
                                Span {
                                    row_start: 3,
                                    row_end: 3,
                                    column_start: 6,
                                    column_end: 9
                                }
                            ),
                            block: Block {
                                stmts: vec![Statement::Pass(Span {
                                    row_start: 4,
                                    row_end: 4,
                                    column_start: 5,
                                    column_end: 8
                                })],
                                span: Span {
                                    row_start: 4,
                                    row_end: 4,
                                    column_start: 5,
                                    column_end: 8
                                },
                            },
                            span: Span {
                                row_start: 3,
                                row_end: 4,
                                column_start: 1,
                                column_end: 8
                            },
                        },
                        ElIfStmt {
                            condition: Expression::Bool(
                                true,
                                Span {
                                    row_start: 5,
                                    row_end: 5,
                                    column_start: 6,
                                    column_end: 9
                                }
                            ),
                            block: Block {
                                stmts: vec![Statement::Pass(Span {
                                    row_start: 6,
                                    row_end: 6,
                                    column_start: 5,
                                    column_end: 8
                                })],
                                span: Span {
                                    row_start: 6,
                                    row_end: 6,
                                    column_start: 5,
                                    column_end: 8
                                }
                            },
                            span: Span {
                                row_start: 5,
                                row_end: 6,
                                column_start: 1,
                                column_end: 8
                            },
                        },
                    ],
                    else_stmt: None,
                    span: Span {
                        row_start: 1,
                        row_end: 6,
                        column_start: 1,
                        column_end: 8
                    },
                })],
            }
        )
    }

    #[test]
    fn parse_if_elif_else() {
        let mut lexer = Lexer::new(
            "if True:
    pass
elif True:
    pass
elif True:
    pass
else:
    pass",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::If(IfStmt {
                    condition: Expression::Bool(
                        true,
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 4,
                            column_end: 7
                        }
                    ),
                    block: Block {
                        stmts: vec![Statement::Pass(Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        })],
                        span: Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        },
                    },
                    elif_stms: vec![
                        ElIfStmt {
                            condition: Expression::Bool(
                                true,
                                Span {
                                    row_start: 3,
                                    row_end: 3,
                                    column_start: 6,
                                    column_end: 9
                                }
                            ),
                            block: Block {
                                stmts: vec![Statement::Pass(Span {
                                    row_start: 4,
                                    row_end: 4,
                                    column_start: 5,
                                    column_end: 8
                                })],
                                span: Span {
                                    row_start: 4,
                                    row_end: 4,
                                    column_start: 5,
                                    column_end: 8
                                },
                            },
                            span: Span {
                                row_start: 3,
                                row_end: 4,
                                column_start: 1,
                                column_end: 8
                            },
                        },
                        ElIfStmt {
                            condition: Expression::Bool(
                                true,
                                Span {
                                    row_start: 5,
                                    row_end: 5,
                                    column_start: 6,
                                    column_end: 9
                                }
                            ),
                            block: Block {
                                stmts: vec![Statement::Pass(Span {
                                    row_start: 6,
                                    row_end: 6,
                                    column_start: 5,
                                    column_end: 8
                                })],
                                span: Span {
                                    row_start: 6,
                                    row_end: 6,
                                    column_start: 5,
                                    column_end: 8
                                }
                            },
                            span: Span {
                                row_start: 5,
                                row_end: 6,
                                column_start: 1,
                                column_end: 8
                            },
                        },
                    ],
                    else_stmt: Some(ElseStmt {
                        block: Block {
                            stmts: vec![Statement::Pass(Span {
                                row_start: 8,
                                row_end: 8,
                                column_start: 5,
                                column_end: 8
                            })],
                            span: Span {
                                row_start: 8,
                                row_end: 8,
                                column_start: 5,
                                column_end: 8
                            }
                        },
                        span: Span {
                            row_start: 7,
                            row_end: 8,
                            column_start: 1,
                            column_end: 8
                        },
                    }),
                    span: Span {
                        row_start: 1,
                        row_end: 8,
                        column_start: 1,
                        column_end: 8
                    },
                })],
            }
        )
    }

    #[test]
    fn parse_while() {
        let mut lexer = Lexer::new(
            "while True:
    pass
",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::While(While {
                    condition: Expression::Bool(
                        true,
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 7,
                            column_end: 10
                        }
                    ),
                    else_stmt: None,
                    block: Block {
                        stmts: vec![Statement::Pass(Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        })],
                        span: Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        }
                    },
                    span: Span {
                        row_start: 1,
                        row_end: 2,
                        column_start: 1,
                        column_end: 8
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_while_else() {
        let mut lexer = Lexer::new(
            "while True:
    pass
else:
    pass",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::While(While {
                    condition: Expression::Bool(
                        true,
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 7,
                            column_end: 10
                        },
                    ),
                    else_stmt: Some(ElseStmt {
                        block: Block {
                            stmts: vec![Statement::Pass(Span {
                                row_start: 4,
                                row_end: 4,
                                column_start: 5,
                                column_end: 8
                            },),],
                            span: Span {
                                row_start: 4,
                                row_end: 4,
                                column_start: 5,
                                column_end: 8
                            },
                        },
                        span: Span {
                            row_start: 3,
                            row_end: 4,
                            column_start: 1,
                            column_end: 8
                        },
                    },),
                    block: Block {
                        stmts: vec![Statement::Pass(Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        })],
                        span: Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        }
                    },
                    span: Span {
                        row_start: 1,
                        row_end: 4,
                        column_start: 1,
                        column_end: 8
                    },
                })]
            }
        )
    }

    #[test]
    fn parse_expression() {
        let mut lexer = Lexer::new("x = 1 + 2 + 3");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::BinaryOp(
                        Box::new(Expression::BinaryOp(
                            Box::new(Expression::Number(
                                "1".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 5,
                                    column_end: 5
                                }
                            )),
                            BinaryOperator::Add,
                            Box::new(Expression::Number(
                                "2".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 9,
                                    column_end: 9
                                }
                            )),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 5,
                                column_end: 9
                            }
                        )),
                        BinaryOperator::Add,
                        Box::new(Expression::Number(
                            "3".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 13,
                                column_end: 13
                            }
                        )),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 5,
                            column_end: 13
                        }
                    )),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 13
                    }
                }))]
            }
        )
    }

    #[test]
    fn parse_expression2() {
        let mut lexer = Lexer::new("x = 1 + 2 * 3 / 2");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::BinaryOp(
                        Box::new(Expression::Number(
                            "1".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 5,
                                column_end: 5
                            }
                        )),
                        BinaryOperator::Add,
                        Box::new(Expression::BinaryOp(
                            Box::new(Expression::BinaryOp(
                                Box::new(Expression::Number(
                                    "2".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 9,
                                        column_end: 9
                                    }
                                )),
                                BinaryOperator::Multiply,
                                Box::new(Expression::Number(
                                    "3".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 13,
                                        column_end: 13
                                    }
                                )),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 9,
                                    column_end: 13
                                }
                            )),
                            BinaryOperator::Divide,
                            Box::new(Expression::Number(
                                "2".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 17,
                                    column_end: 17
                                }
                            )),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 9,
                                column_end: 17
                            }
                        )),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 5,
                            column_end: 17
                        }
                    )),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 17
                    }
                }))]
            }
        );
    }

    #[test]
    fn parse_expression3() {
        let mut lexer = Lexer::new("x = 3 + -5");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::BinaryOp(
                        Box::new(Expression::Number(
                            "3".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 5,
                                column_end: 5
                            }
                        )),
                        BinaryOperator::Add,
                        Box::new(Expression::UnaryOp(
                            Box::new(Expression::Number(
                                "5".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 10,
                                    column_end: 10
                                }
                            )),
                            UnaryOperator::Minus,
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 9,
                                column_end: 10
                            }
                        )),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 5,
                            column_end: 10
                        }
                    )),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 10
                    }
                }))]
            }
        )
    }

    #[test]
    fn parse_expression4() {
        let mut lexer = Lexer::new("x = not 3 + -5");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::UnaryOp(
                        Box::new(Expression::BinaryOp(
                            Box::new(Expression::Number(
                                "3".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 9,
                                    column_end: 9
                                }
                            )),
                            BinaryOperator::Add,
                            Box::new(Expression::UnaryOp(
                                Box::new(Expression::Number(
                                    "5".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 14,
                                        column_end: 14
                                    }
                                )),
                                UnaryOperator::Minus,
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 13,
                                    column_end: 14
                                }
                            )),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 9,
                                column_end: 14
                            }
                        )),
                        UnaryOperator::LogicalNot,
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 5,
                            column_end: 14
                        }
                    )),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 14
                    }
                }))]
            }
        );
    }

    #[test]
    fn parse_expression5() {
        let mut lexer = Lexer::new("x = x + +5");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::BinaryOp(
                        Box::new(Expression::Id(
                            "x".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 5,
                                column_end: 5
                            }
                        )),
                        BinaryOperator::Add,
                        Box::new(Expression::UnaryOp(
                            Box::new(Expression::Number(
                                "5".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 10,
                                    column_end: 10
                                }
                            )),
                            UnaryOperator::Plus,
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 9,
                                column_end: 10
                            }
                        )),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 5,
                            column_end: 10
                        }
                    )),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 10
                    }
                }))]
            }
        );
    }

    #[test]
    fn parse_expression6() {
        let mut lexer = Lexer::new("a = x < y or 69 > 9 and not 101 >> 666");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "a".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::BinaryOp(
                        Box::new(Expression::BinaryOp(
                            Box::new(Expression::Id(
                                "x".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 5,
                                    column_end: 5
                                }
                            )),
                            BinaryOperator::LessThan,
                            Box::new(Expression::Id(
                                "y".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 9,
                                    column_end: 9
                                }
                            )),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 5,
                                column_end: 9
                            }
                        )),
                        BinaryOperator::LogicalOr,
                        Box::new(Expression::BinaryOp(
                            Box::new(Expression::BinaryOp(
                                Box::new(Expression::Number(
                                    "69".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 14,
                                        column_end: 15
                                    }
                                )),
                                BinaryOperator::GreaterThan,
                                Box::new(Expression::Number(
                                    "9".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 19,
                                        column_end: 19
                                    }
                                )),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 14,
                                    column_end: 19
                                }
                            )),
                            BinaryOperator::LogicalAnd,
                            Box::new(Expression::UnaryOp(
                                Box::new(Expression::BinaryOp(
                                    Box::new(Expression::Number(
                                        "101".into(),
                                        Span {
                                            row_start: 1,
                                            row_end: 1,
                                            column_start: 29,
                                            column_end: 31
                                        }
                                    )),
                                    BinaryOperator::BitwiseRightShift,
                                    Box::new(Expression::Number(
                                        "666".into(),
                                        Span {
                                            row_start: 1,
                                            row_end: 1,
                                            column_start: 36,
                                            column_end: 38
                                        }
                                    )),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 29,
                                        column_end: 38
                                    }
                                )),
                                UnaryOperator::LogicalNot,
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 25,
                                    column_end: 38
                                }
                            )),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 14,
                                column_end: 38
                            }
                        )),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 5,
                            column_end: 38
                        }
                    )),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 38
                    }
                }))]
            }
        );
    }

    #[test]
    fn parse_expression7() {
        let mut lexer = Lexer::new("x = a.b.c");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::BinaryOp(
                        Box::new(Expression::Id(
                            "a".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 5,
                                column_end: 5
                            }
                        )),
                        BinaryOperator::AttributeRef,
                        Box::new(Expression::BinaryOp(
                            Box::new(Expression::Id(
                                "b".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 7,
                                    column_end: 7
                                }
                            )),
                            BinaryOperator::AttributeRef,
                            Box::new(Expression::Id(
                                "c".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 9,
                                    column_end: 9
                                }
                            )),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 7,
                                column_end: 9
                            }
                        )),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 5,
                            column_end: 9
                        }
                    )),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 9
                    }
                }))],
            }
        )
    }

    #[test]
    fn parse_expression8() {
        let mut lexer = Lexer::new("x = hello();");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::Call(FunctionCall {
                        lhs: Box::new(Expression::Id(
                            "hello".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 5,
                                column_end: 9
                            }
                        )),
                        args: vec![],
                        span: Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 5,
                            column_end: 11
                        }
                    })),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 11
                    }
                }))],
            }
        )
    }

    #[test]
    fn parse_expression9() {
        let mut lexer = Lexer::new("x = l[1 + 2]");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::Subscript(Subscript {
                        lhs: Box::new(Expression::Id(
                            "l".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 5,
                                column_end: 5
                            }
                        )),
                        slice: Box::new(SubscriptType::Subscript(Expression::BinaryOp(
                            Box::new(Expression::Number(
                                "1".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 7,
                                    column_end: 7
                                }
                            )),
                            BinaryOperator::Add,
                            Box::new(Expression::Number(
                                "2".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 11,
                                    column_end: 11
                                }
                            )),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 7,
                                column_end: 11
                            }
                        ))),
                        span: Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 5,
                            column_end: 12
                        }
                    })),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 12
                    }
                }))]
            }
        )
    }

    #[test]
    fn parse_expression10() {
        let mut lexer = Lexer::new("x = a not in b");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::BinaryOp(
                        Box::new(Expression::Id(
                            "a".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 5,
                                column_end: 5
                            }
                        )),
                        BinaryOperator::NotIn,
                        Box::new(Expression::Id(
                            "b".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 14,
                                column_end: 14
                            }
                        )),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 5,
                            column_end: 14
                        }
                    )),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 14
                    }
                }))],
            }
        )
    }

    #[test]
    fn parse_expression11() {
        let mut lexer = Lexer::new("x = ((1 + 2) * 54) / 3");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::BinaryOp(
                        Box::new(Expression::BinaryOp(
                            Box::new(Expression::BinaryOp(
                                Box::new(Expression::Number(
                                    "1".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 7,
                                        column_end: 7
                                    }
                                )),
                                BinaryOperator::Add,
                                Box::new(Expression::Number(
                                    "2".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 11,
                                        column_end: 11
                                    }
                                )),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 6,
                                    column_end: 12
                                }
                            )),
                            BinaryOperator::Multiply,
                            Box::new(Expression::Number(
                                "54".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 16,
                                    column_end: 17
                                }
                            )),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 5,
                                column_end: 18
                            }
                        )),
                        BinaryOperator::Divide,
                        Box::new(Expression::Number(
                            "3".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 22,
                                column_end: 22
                            }
                        )),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 5,
                            column_end: 22
                        }
                    )),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 22
                    }
                }))],
            }
        );
    }

    #[test]
    fn parse_tuple_expression() {
        let mut lexer = Lexer::new("x = (1 + 2, True, y(), \"Hello\", l[i],)");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::Tuple(
                        vec![
                            Expression::BinaryOp(
                                Box::new(Expression::Number(
                                    "1".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 6,
                                        column_end: 6
                                    }
                                )),
                                BinaryOperator::Add,
                                Box::new(Expression::Number(
                                    "2".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 10,
                                        column_end: 10
                                    }
                                )),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 6,
                                    column_end: 10
                                }
                            ),
                            Expression::Bool(
                                true,
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 13,
                                    column_end: 16
                                }
                            ),
                            Expression::Call(FunctionCall {
                                lhs: Box::new(Expression::Id(
                                    "y".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 19,
                                        column_end: 19
                                    }
                                )),
                                args: vec![],
                                span: Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 19,
                                    column_end: 21
                                }
                            }),
                            Expression::String(
                                "Hello".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 24,
                                    column_end: 30
                                }
                            ),
                            Expression::Subscript(Subscript {
                                lhs: Box::new(Expression::Id(
                                    "l".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 33,
                                        column_end: 33
                                    }
                                )),
                                slice: Box::new(SubscriptType::Subscript(Expression::Id(
                                    "i".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 35,
                                        column_end: 35
                                    }
                                ))),
                                span: Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 33,
                                    column_end: 36
                                }
                            })
                        ],
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 5,
                            column_end: 38
                        }
                    )),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 38
                    }
                }))]
            }
        );
    }

    #[test]
    fn parse_tuple_expression_with_no_parens() {
        let mut lexer = Lexer::new("1 + 2, True, y(), \"Hello\", l[i],");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Tuple(
                    vec![
                        Expression::BinaryOp(
                            Box::new(Expression::Number(
                                "1".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 1,
                                    column_end: 1
                                }
                            )),
                            BinaryOperator::Add,
                            Box::new(Expression::Number(
                                "2".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 5,
                                    column_end: 5
                                }
                            )),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 1,
                                column_end: 5
                            }
                        ),
                        Expression::Bool(
                            true,
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 8,
                                column_end: 11
                            }
                        ),
                        Expression::Call(FunctionCall {
                            lhs: Box::new(Expression::Id(
                                "y".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 14,
                                    column_end: 14
                                }
                            )),
                            args: vec![],
                            span: Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 14,
                                column_end: 16
                            }
                        }),
                        Expression::String(
                            "Hello".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 19,
                                column_end: 25
                            }
                        ),
                        Expression::Subscript(Subscript {
                            lhs: Box::new(Expression::Id(
                                "l".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 28,
                                    column_end: 28
                                }
                            )),
                            slice: Box::new(SubscriptType::Subscript(Expression::Id(
                                "i".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 30,
                                    column_end: 30
                                }
                            ))),
                            span: Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 28,
                                column_end: 31
                            }
                        })
                    ],
                    Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 32
                    }
                ),)]
            }
        )
    }

    #[test]
    fn parse_list_expression() {
        let mut lexer = Lexer::new("x = [1 + 2, True, y(), \"Hello\", l[i],]");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::List(
                        vec![
                            Expression::BinaryOp(
                                Box::new(Expression::Number(
                                    "1".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 6,
                                        column_end: 6
                                    }
                                )),
                                BinaryOperator::Add,
                                Box::new(Expression::Number(
                                    "2".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 10,
                                        column_end: 10
                                    }
                                )),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 6,
                                    column_end: 10
                                }
                            ),
                            Expression::Bool(
                                true,
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 13,
                                    column_end: 16
                                }
                            ),
                            Expression::Call(FunctionCall {
                                lhs: Box::new(Expression::Id(
                                    "y".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 19,
                                        column_end: 19
                                    }
                                )),
                                args: vec![],
                                span: Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 19,
                                    column_end: 21
                                }
                            }),
                            Expression::String(
                                "Hello".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 24,
                                    column_end: 30
                                }
                            ),
                            Expression::Subscript(Subscript {
                                lhs: Box::new(Expression::Id(
                                    "l".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 33,
                                        column_end: 33
                                    }
                                )),
                                slice: Box::new(SubscriptType::Subscript(Expression::Id(
                                    "i".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 35,
                                        column_end: 35
                                    }
                                ))),
                                span: Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 33,
                                    column_end: 36
                                }
                            })
                        ],
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 5,
                            column_end: 38
                        }
                    )),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 38
                    }
                }))]
            }
        );
    }

    #[test]
    fn parse_list_expression2() {
        let mut lexer = Lexer::new("x = [*l, *[1,2,3], True]");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::List(
                        vec![
                            Expression::UnaryOp(
                                Box::new(Expression::Id(
                                    "l".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 7,
                                        column_end: 7
                                    }
                                )),
                                UnaryOperator::UnpackIterable,
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 6,
                                    column_end: 7
                                }
                            ),
                            Expression::UnaryOp(
                                Box::new(Expression::List(
                                    vec![
                                        Expression::Number(
                                            "1".into(),
                                            Span {
                                                row_start: 1,
                                                row_end: 1,
                                                column_start: 12,
                                                column_end: 12
                                            }
                                        ),
                                        Expression::Number(
                                            "2".into(),
                                            Span {
                                                row_start: 1,
                                                row_end: 1,
                                                column_start: 14,
                                                column_end: 14
                                            }
                                        ),
                                        Expression::Number(
                                            "3".into(),
                                            Span {
                                                row_start: 1,
                                                row_end: 1,
                                                column_start: 16,
                                                column_end: 16
                                            }
                                        )
                                    ],
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 11,
                                        column_end: 17
                                    }
                                )),
                                UnaryOperator::UnpackIterable,
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 10,
                                    column_end: 17
                                }
                            ),
                            Expression::Bool(
                                true,
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 20,
                                    column_end: 23
                                }
                            )
                        ],
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 5,
                            column_end: 24
                        }
                    )),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 24
                    }
                }))]
            }
        );
    }

    #[test]
    fn parse_invalid_list_expression() {
        let mut lexer = Lexer::new("[**d, x]");
        let parser = Parser::new(&mut lexer);
        let (_, errors) = parser.parse();

        assert!(errors.is_some());
        assert_eq!(
            errors,
            Some(vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: can't unpack dictionary inside list!".into(),
                span: Span {
                    row_start: 1,
                    row_end: 1,
                    column_start: 2,
                    column_end: 3
                },
            },])
        )
    }

    #[test]
    fn parse_empty_list_expression() {
        let mut lexer = Lexer::new("[]");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::List(
                    vec![],
                    Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 2
                    }
                ),)]
            }
        );
    }

    #[test]
    fn parse_unpack_iterable_assignment() {
        let mut lexer = Lexer::new("x = *iterable");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::UnaryOp(
                        Box::new(Expression::Id(
                            "iterable".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 6,
                                column_end: 13
                            }
                        )),
                        UnaryOperator::UnpackIterable,
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 5,
                            column_end: 13
                        }
                    )),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 13
                    }
                }))]
            }
        )
    }

    #[test]
    fn parse_set_expression() {
        let mut lexer = Lexer::new("x = {1, True, \"hello\", *l,}");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::Set(
                        vec![
                            Expression::Number(
                                "1".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 6,
                                    column_end: 6
                                }
                            ),
                            Expression::Bool(
                                true,
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 9,
                                    column_end: 12
                                }
                            ),
                            Expression::String(
                                "hello".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 15,
                                    column_end: 21
                                }
                            ),
                            Expression::UnaryOp(
                                Box::new(Expression::Id(
                                    "l".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 25,
                                        column_end: 25
                                    }
                                )),
                                UnaryOperator::UnpackIterable,
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 24,
                                    column_end: 25
                                }
                            )
                        ],
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 5,
                            column_end: 27
                        }
                    )),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 27
                    }
                }))]
            }
        )
    }

    #[test]
    fn parse_dict_expression() {
        let mut lexer = Lexer::new("x = {1: \"Hello\", 1 + 3: True, (6, 6): False,}");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::Dict(
                        vec![
                            DictItemType::KeyValue(
                                Expression::Number(
                                    "1".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 6,
                                        column_end: 6
                                    }
                                ),
                                Expression::String(
                                    "Hello".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 9,
                                        column_end: 15
                                    }
                                )
                            ),
                            DictItemType::KeyValue(
                                Expression::BinaryOp(
                                    Box::new(Expression::Number(
                                        "1".into(),
                                        Span {
                                            row_start: 1,
                                            row_end: 1,
                                            column_start: 18,
                                            column_end: 18
                                        }
                                    )),
                                    BinaryOperator::Add,
                                    Box::new(Expression::Number(
                                        "3".into(),
                                        Span {
                                            row_start: 1,
                                            row_end: 1,
                                            column_start: 22,
                                            column_end: 22
                                        }
                                    )),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 18,
                                        column_end: 22
                                    }
                                ),
                                Expression::Bool(
                                    true,
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 25,
                                        column_end: 28
                                    }
                                )
                            ),
                            DictItemType::KeyValue(
                                Expression::Tuple(
                                    vec![
                                        Expression::Number(
                                            "6".into(),
                                            Span {
                                                row_start: 1,
                                                row_end: 1,
                                                column_start: 32,
                                                column_end: 32
                                            }
                                        ),
                                        Expression::Number(
                                            "6".into(),
                                            Span {
                                                row_start: 1,
                                                row_end: 1,
                                                column_start: 35,
                                                column_end: 35
                                            }
                                        )
                                    ],
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 31,
                                        column_end: 36
                                    }
                                ),
                                Expression::Bool(
                                    false,
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 39,
                                        column_end: 43
                                    }
                                )
                            )
                        ],
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 5,
                            column_end: 45
                        }
                    )),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 45
                    }
                }))]
            }
        )
    }

    #[test]
    fn parse_dict_expression2() {
        let mut lexer = Lexer::new("x = {**d, 2: 5, **x,}");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::Dict(
                        vec![
                            DictItemType::Unpack(Expression::UnaryOp(
                                Box::new(Expression::Id(
                                    "d".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 8,
                                        column_end: 8
                                    }
                                )),
                                UnaryOperator::UnpackDictionary,
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 6,
                                    column_end: 8
                                }
                            )),
                            DictItemType::KeyValue(
                                Expression::Number(
                                    "2".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 11,
                                        column_end: 11
                                    }
                                ),
                                Expression::Number(
                                    "5".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 14,
                                        column_end: 14
                                    }
                                )
                            ),
                            DictItemType::Unpack(Expression::UnaryOp(
                                Box::new(Expression::Id(
                                    "x".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 19,
                                        column_end: 19
                                    }
                                )),
                                UnaryOperator::UnpackDictionary,
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 17,
                                    column_end: 19
                                }
                            ))
                        ],
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 5,
                            column_end: 21
                        }
                    )),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 21
                    }
                }))]
            }
        )
    }

    #[test]
    fn parse_invalid_dict_expression() {
        let mut lexer = Lexer::new("x = {**d, *x}");
        let parser = Parser::new(&mut lexer);
        let (_, errors) = parser.parse();

        assert!(errors.is_some());
        assert_eq!(
            errors.unwrap(),
            vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: can't unpack iterable inside dictionary!".into(),
                span: Span {
                    row_start: 1,
                    row_end: 1,
                    column_start: 11,
                    column_end: 11
                }
            }]
        );
    }

    #[test]
    fn parse_if_else_expression() {
        let mut lexer = Lexer::new("x = 15 if 5 < x else 45");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::IfElse(IfElseExpr {
                        lhs: Box::new(Expression::Number(
                            "15".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 5,
                                column_end: 6
                            }
                        )),
                        rhs: Box::new(Expression::Number(
                            "45".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 22,
                                column_end: 23
                            }
                        )),
                        condition: Box::new(Expression::BinaryOp(
                            Box::new(Expression::Number(
                                "5".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 11,
                                    column_end: 11
                                }
                            )),
                            BinaryOperator::LessThan,
                            Box::new(Expression::Id(
                                "x".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 15,
                                    column_end: 15
                                }
                            )),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 11,
                                column_end: 15
                            }
                        )),
                        span: Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 5,
                            column_end: 23
                        }
                    })),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 23
                    }
                }))]
            }
        )
    }

    #[test]
    fn parse_if_else_expression2() {
        let mut lexer = Lexer::new("x = func() if (5 < x or x >= y) and is_id else func2() * 5");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::IfElse(IfElseExpr {
                        lhs: Box::new(Expression::Call(FunctionCall {
                            lhs: Box::new(Expression::Id(
                                "func".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 5,
                                    column_end: 8
                                }
                            )),
                            args: vec![],
                            span: Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 5,
                                column_end: 10
                            }
                        })),
                        rhs: Box::new(Expression::BinaryOp(
                            Box::new(Expression::Call(FunctionCall {
                                lhs: Box::new(Expression::Id(
                                    "func2".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 48,
                                        column_end: 52
                                    }
                                )),
                                args: vec![],
                                span: Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 48,
                                    column_end: 54
                                }
                            })),
                            BinaryOperator::Multiply,
                            Box::new(Expression::Number(
                                "5".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 58,
                                    column_end: 58
                                }
                            )),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 48,
                                column_end: 58
                            }
                        )),
                        condition: Box::new(Expression::BinaryOp(
                            Box::new(Expression::BinaryOp(
                                Box::new(Expression::BinaryOp(
                                    Box::new(Expression::Number(
                                        "5".into(),
                                        Span {
                                            row_start: 1,
                                            row_end: 1,
                                            column_start: 16,
                                            column_end: 16
                                        }
                                    )),
                                    BinaryOperator::LessThan,
                                    Box::new(Expression::Id(
                                        "x".into(),
                                        Span {
                                            row_start: 1,
                                            row_end: 1,
                                            column_start: 20,
                                            column_end: 20
                                        }
                                    )),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 16,
                                        column_end: 20
                                    }
                                )),
                                BinaryOperator::LogicalOr,
                                Box::new(Expression::BinaryOp(
                                    Box::new(Expression::Id(
                                        "x".into(),
                                        Span {
                                            row_start: 1,
                                            row_end: 1,
                                            column_start: 25,
                                            column_end: 25
                                        }
                                    )),
                                    BinaryOperator::GreaterThanOrEqual,
                                    Box::new(Expression::Id(
                                        "y".into(),
                                        Span {
                                            row_start: 1,
                                            row_end: 1,
                                            column_start: 30,
                                            column_end: 30
                                        }
                                    )),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 25,
                                        column_end: 30
                                    }
                                )),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 15,
                                    column_end: 31
                                }
                            )),
                            BinaryOperator::LogicalAnd,
                            Box::new(Expression::Id(
                                "is_id".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 37,
                                    column_end: 41
                                }
                            )),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 15,
                                column_end: 41
                            }
                        )),
                        span: Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 5,
                            column_end: 58
                        }
                    })),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 58
                    }
                }))]
            }
        )
    }

    #[test]
    fn parse_single_expression() {
        let mut lexer = Lexer::new("1 + 2; 3 + 4, 7, 8");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![
                    Statement::Expression(Expression::BinaryOp(
                        Box::new(Expression::Number(
                            "1".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 1,
                                column_end: 1
                            }
                        )),
                        BinaryOperator::Add,
                        Box::new(Expression::Number(
                            "2".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 5,
                                column_end: 5
                            }
                        )),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 5
                        }
                    ),),
                    Statement::Expression(Expression::Tuple(
                        vec![
                            Expression::BinaryOp(
                                Box::new(Expression::Number(
                                    "3".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 8,
                                        column_end: 8
                                    }
                                )),
                                BinaryOperator::Add,
                                Box::new(Expression::Number(
                                    "4".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 12,
                                        column_end: 12
                                    }
                                )),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 8,
                                    column_end: 12
                                }
                            ),
                            Expression::Number(
                                "7".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 15,
                                    column_end: 15
                                }
                            ),
                            Expression::Number(
                                "8".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 18,
                                    column_end: 18
                                }
                            ),
                        ],
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 8,
                            column_end: 18
                        }
                    ),),
                ]
            }
        )
    }

    #[test]
    fn parse_single_expression2() {
        let mut lexer = Lexer::new("x");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Id(
                    "x".into(),
                    Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 1
                    }
                ),)]
            }
        )
    }

    #[test]
    fn parse_walrus_operator() {
        let mut lexer = Lexer::new(
            "if (x := 15) > 5:
    x",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::If(IfStmt {
                    condition: Expression::BinaryOp(
                        Box::new(Expression::BinaryOp(
                            Box::new(Expression::Id(
                                "x".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 5,
                                    column_end: 5
                                }
                            )),
                            BinaryOperator::Walrus,
                            Box::new(Expression::Number(
                                "15".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 10,
                                    column_end: 11
                                }
                            )),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 4,
                                column_end: 12
                            }
                        )),
                        BinaryOperator::GreaterThan,
                        Box::new(Expression::Number(
                            "5".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 16,
                                column_end: 16
                            }
                        )),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 4,
                            column_end: 16
                        }
                    ),
                    block: Block {
                        stmts: vec![Statement::Expression(Expression::Id(
                            "x".into(),
                            Span {
                                row_start: 2,
                                row_end: 2,
                                column_start: 5,
                                column_end: 5
                            }
                        ),)],
                        span: Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 5
                        }
                    },
                    elif_stms: vec![],
                    else_stmt: None,
                    span: Span {
                        row_start: 1,
                        row_end: 2,
                        column_start: 1,
                        column_end: 5
                    }
                })]
            }
        )
    }

    #[test]
    #[ignore = "Handle invalid walrus assignment in statement"]
    fn parse_invalid_walrus_operator() {
        let mut lexer = Lexer::new("x := 5 + 5");
        let parser = Parser::new(&mut lexer);
        let (_, errors) = parser.parse();

        assert!(errors.is_some());
        assert_eq!(
            errors.unwrap(),
            vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: invalid assignment statement!".into(),
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
    fn parse_await_expr() {
        let mut lexer = Lexer::new("await func() * x ** 5 / 3");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::BinaryOp(
                    Box::new(Expression::BinaryOp(
                        Box::new(Expression::Await(
                            Box::new(Expression::Call(FunctionCall {
                                lhs: Box::new(Expression::Id(
                                    "func".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 7,
                                        column_end: 10
                                    }
                                )),
                                args: vec![],
                                span: Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 7,
                                    column_end: 12
                                }
                            })),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 1,
                                column_end: 12
                            }
                        )),
                        BinaryOperator::Multiply,
                        Box::new(Expression::BinaryOp(
                            Box::new(Expression::Id(
                                "x".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 16,
                                    column_end: 16
                                }
                            )),
                            BinaryOperator::Exponent,
                            Box::new(Expression::Number(
                                "5".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 21,
                                    column_end: 21
                                }
                            )),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 16,
                                column_end: 21
                            }
                        )),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 21
                        }
                    )),
                    BinaryOperator::Divide,
                    Box::new(Expression::Number(
                        "3".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 25,
                            column_end: 25
                        }
                    )),
                    Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 25
                    }
                ),)]
            }
        )
    }

    #[test]
    fn parse_lambda_expr() {
        let mut lexer = Lexer::new("lambda x: x + 1");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Lambda(LambdaExpr {
                    parameters: vec![FuncParameter {
                        name: "x".into(),
                        default_value: None,
                        star_parameter_type: None,
                        span: Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 8,
                            column_end: 8
                        },
                        is_kw_only: false,
                        is_pos_only: false
                    }],
                    expression: Box::new(Expression::BinaryOp(
                        Box::new(Expression::Id(
                            "x".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 11,
                                column_end: 11
                            }
                        )),
                        BinaryOperator::Add,
                        Box::new(Expression::Number(
                            "1".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 15,
                                column_end: 15
                            }
                        )),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 11,
                            column_end: 15
                        }
                    )),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 15
                    }
                }),)]
            }
        )
    }

    #[test]
    fn parse_lambda_expr2() {
        let mut lexer = Lexer::new("(lambda x: x + 1)()");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Call(FunctionCall {
                    lhs: Box::new(Expression::Lambda(LambdaExpr {
                        parameters: vec![FuncParameter {
                            name: "x".into(),
                            default_value: None,
                            star_parameter_type: None,
                            span: Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 9,
                                column_end: 9
                            },
                            is_kw_only: false,
                            is_pos_only: false
                        }],
                        expression: Box::new(Expression::BinaryOp(
                            Box::new(Expression::Id(
                                "x".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 12,
                                    column_end: 12
                                }
                            )),
                            BinaryOperator::Add,
                            Box::new(Expression::Number(
                                "1".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 16,
                                    column_end: 16
                                }
                            )),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 12,
                                column_end: 16
                            }
                        )),
                        span: Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 17
                        }
                    })),
                    args: vec![],
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 19
                    }
                }),)]
            }
        )
    }

    #[test]
    fn parse_class() {
        let mut lexer = Lexer::new(
            "class Test:
    def __init__(self):
        pass",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Class(ClassStmt {
                    name: "Test".into(),
                    block: Block {
                        stmts: vec![Statement::FunctionDef(Function {
                            name: "__init__".into(),
                            decorators: vec![],
                            parameters: vec![FuncParameter {
                                name: "self".into(),
                                default_value: None,
                                star_parameter_type: None,
                                span: Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 18,
                                    column_end: 21
                                },
                                is_kw_only: false,
                                is_pos_only: false
                            }],
                            block: Block {
                                stmts: vec![Statement::Pass(Span {
                                    row_start: 3,
                                    row_end: 3,
                                    column_start: 9,
                                    column_end: 12
                                })],
                                span: Span {
                                    row_start: 3,
                                    row_end: 3,
                                    column_start: 9,
                                    column_end: 12
                                }
                            },
                            span: Span {
                                row_start: 2,
                                row_end: 3,
                                column_start: 5,
                                column_end: 12
                            }
                        })],
                        span: Span {
                            row_start: 2,
                            row_end: 3,
                            column_start: 5,
                            column_end: 12
                        }
                    },
                    base_classes: vec![],
                    span: Span {
                        row_start: 1,
                        row_end: 3,
                        column_start: 1,
                        column_end: 12
                    },
                    decorators: vec![]
                })]
            }
        )
    }

    #[test]
    fn parse_class2() {
        let mut lexer = Lexer::new(
            "class Dog(Animal):
    def __init__(self):
        pass",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Class(ClassStmt {
                    name: "Dog".into(),
                    block: Block {
                        stmts: vec![Statement::FunctionDef(Function {
                            name: "__init__".into(),
                            decorators: vec![],
                            parameters: vec![FuncParameter {
                                name: "self".into(),
                                default_value: None,
                                star_parameter_type: None,
                                span: Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 18,
                                    column_end: 21
                                },
                                is_kw_only: false,
                                is_pos_only: false
                            }],
                            block: Block {
                                stmts: vec![Statement::Pass(Span {
                                    row_start: 3,
                                    row_end: 3,
                                    column_start: 9,
                                    column_end: 12
                                })],
                                span: Span {
                                    row_start: 3,
                                    row_end: 3,
                                    column_start: 9,
                                    column_end: 12
                                }
                            },
                            span: Span {
                                row_start: 2,
                                row_end: 3,
                                column_start: 5,
                                column_end: 12
                            }
                        })],
                        span: Span {
                            row_start: 2,
                            row_end: 3,
                            column_start: 5,
                            column_end: 12
                        }
                    },
                    base_classes: vec![Expression::Id(
                        "Animal".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 11,
                            column_end: 16
                        }
                    )],
                    span: Span {
                        row_start: 1,
                        row_end: 3,
                        column_start: 1,
                        column_end: 12
                    },
                    decorators: vec![]
                })]
            }
        )
    }

    #[test]
    fn parse_import() {
        let mut lexer = Lexer::new("import os");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Import(ImportStmt {
                    modules: vec![ImportModule {
                        name: vec!["os".into()],
                        alias: None
                    }],
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 9
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_import2() {
        let mut lexer = Lexer::new("import os.walk as O, sys as S");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Import(ImportStmt {
                    modules: vec![
                        ImportModule {
                            name: vec!["os".into(), "walk".into()],
                            alias: Some("O".into())
                        },
                        ImportModule {
                            name: vec!["sys".into()],
                            alias: Some("S".into())
                        }
                    ],
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 29
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_from_import() {
        let mut lexer = Lexer::new("from os import *");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FromImport(FromImportStmt {
                    module: vec![ImportModule {
                        name: vec!["os".into()],
                        alias: None
                    }],
                    targets: vec![ImportModule {
                        name: vec!["*".into()],
                        alias: None
                    }],
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 16
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_from_import2() {
        let mut lexer = Lexer::new("from ... import *");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FromImport(FromImportStmt {
                    module: vec![ImportModule {
                        name: vec!["...".into()],
                        alias: None
                    }],
                    targets: vec![ImportModule {
                        name: vec!["*".into()],
                        alias: None
                    }],
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 17
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_from_import3() {
        let mut lexer = Lexer::new("from .subpackage.module1 import func");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FromImport(FromImportStmt {
                    module: vec![
                        ImportModule {
                            name: vec![".".into()],
                            alias: None
                        },
                        ImportModule {
                            name: vec!["subpackage".into(), "module1".into()],
                            alias: None
                        }
                    ],
                    targets: vec![ImportModule {
                        name: vec!["func".into()],
                        alias: None
                    }],
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 36
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_from_import4() {
        let mut lexer = Lexer::new("from .subpackage.module1 import (func, func2, func3)");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FromImport(FromImportStmt {
                    module: vec![
                        ImportModule {
                            name: vec![".".into()],
                            alias: None
                        },
                        ImportModule {
                            name: vec!["subpackage".into(), "module1".into()],
                            alias: None
                        }
                    ],
                    targets: vec![
                        ImportModule {
                            name: vec!["func".into()],
                            alias: None
                        },
                        ImportModule {
                            name: vec!["func2".into()],
                            alias: None
                        },
                        ImportModule {
                            name: vec!["func3".into()],
                            alias: None
                        }
                    ],
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 51
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_with() {
        let mut lexer = Lexer::new(
            "with open() as file:
    pass",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::With(WithStmt {
                    items: vec![WithItem {
                        item: Expression::Call(FunctionCall {
                            lhs: Box::new(Expression::Id(
                                "open".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 6,
                                    column_end: 9
                                }
                            )),
                            args: vec![],
                            span: Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 6,
                                column_end: 11
                            }
                        }),
                        target: Some(Expression::Id(
                            "file".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 16,
                                column_end: 19
                            }
                        )),
                        span: Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 6,
                            column_end: 19
                        }
                    }],
                    block: Block {
                        stmts: vec![Statement::Pass(Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        })],
                        span: Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        }
                    },
                    span: Span {
                        row_start: 1,
                        row_end: 2,
                        column_start: 1,
                        column_end: 8
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_try_except() {
        let mut lexer = Lexer::new(
            "try:
    pass
except:
    pass",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Try(TryStmt {
                    block: Block {
                        stmts: vec![Statement::Pass(Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        })],
                        span: Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        }
                    },
                    finally_block: None,
                    except_blocks: vec![ExceptBlock {
                        block: Block {
                            stmts: vec![Statement::Pass(Span {
                                row_start: 4,
                                row_end: 4,
                                column_start: 5,
                                column_end: 8
                            })],
                            span: Span {
                                row_start: 4,
                                row_end: 4,
                                column_start: 5,
                                column_end: 8
                            }
                        },
                        kind: ExceptBlockKind::Except,
                        expr: None,
                        expr_alias: None,
                        span: Span {
                            row_start: 3,
                            row_end: 4,
                            column_start: 1,
                            column_end: 8
                        }
                    }],
                    else_stmt: None,
                    span: Span {
                        row_start: 1,
                        row_end: 4,
                        column_start: 1,
                        column_end: 8
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_try_except_as() {
        let mut lexer = Lexer::new(
            "try:
    pass
except Except as e:
    pass",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Try(TryStmt {
                    block: Block {
                        stmts: vec![Statement::Pass(Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        })],
                        span: Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        }
                    },
                    finally_block: None,
                    except_blocks: vec![ExceptBlock {
                        block: Block {
                            stmts: vec![Statement::Pass(Span {
                                row_start: 4,
                                row_end: 4,
                                column_start: 5,
                                column_end: 8
                            })],
                            span: Span {
                                row_start: 4,
                                row_end: 4,
                                column_start: 5,
                                column_end: 8
                            }
                        },
                        kind: ExceptBlockKind::Except,
                        expr: Some(Expression::Id(
                            "Except".into(),
                            Span {
                                row_start: 3,
                                row_end: 3,
                                column_start: 8,
                                column_end: 13
                            }
                        )),
                        expr_alias: Some("e".into()),
                        span: Span {
                            row_start: 3,
                            row_end: 4,
                            column_start: 1,
                            column_end: 8
                        }
                    }],
                    else_stmt: None,
                    span: Span {
                        row_start: 1,
                        row_end: 4,
                        column_start: 1,
                        column_end: 8
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_try_finally() {
        let mut lexer = Lexer::new(
            "try:
    pass
finally:
    pass",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Try(TryStmt {
                    block: Block {
                        stmts: vec![Statement::Pass(Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        })],
                        span: Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        }
                    },
                    finally_block: Some(FinallyBlock {
                        block: Block {
                            stmts: vec![Statement::Pass(Span {
                                row_start: 4,
                                row_end: 4,
                                column_start: 5,
                                column_end: 8
                            })],
                            span: Span {
                                row_start: 4,
                                row_end: 4,
                                column_start: 5,
                                column_end: 8
                            }
                        },
                        span: Span {
                            row_start: 3,
                            row_end: 4,
                            column_start: 1,
                            column_end: 8
                        }
                    }),
                    except_blocks: vec![],
                    else_stmt: None,
                    span: Span {
                        row_start: 1,
                        row_end: 4,
                        column_start: 1,
                        column_end: 8
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_try_except_finally() {
        let mut lexer = Lexer::new(
            "try:
    pass
except:
    pass
finally:
    pass",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Try(TryStmt {
                    block: Block {
                        stmts: vec![Statement::Pass(Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        })],
                        span: Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        }
                    },
                    finally_block: Some(FinallyBlock {
                        block: Block {
                            stmts: vec![Statement::Pass(Span {
                                row_start: 6,
                                row_end: 6,
                                column_start: 5,
                                column_end: 8
                            })],
                            span: Span {
                                row_start: 6,
                                row_end: 6,
                                column_start: 5,
                                column_end: 8
                            }
                        },
                        span: Span {
                            row_start: 5,
                            row_end: 6,
                            column_start: 1,
                            column_end: 8
                        }
                    }),
                    except_blocks: vec![ExceptBlock {
                        block: Block {
                            stmts: vec![Statement::Pass(Span {
                                row_start: 4,
                                row_end: 4,
                                column_start: 5,
                                column_end: 8
                            })],
                            span: Span {
                                row_start: 4,
                                row_end: 4,
                                column_start: 5,
                                column_end: 8
                            }
                        },
                        kind: ExceptBlockKind::Except,
                        expr: None,
                        expr_alias: None,
                        span: Span {
                            row_start: 3,
                            row_end: 4,
                            column_start: 1,
                            column_end: 8
                        }
                    }],
                    else_stmt: None,
                    span: Span {
                        row_start: 1,
                        row_end: 6,
                        column_start: 1,
                        column_end: 8
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_try_except_else_finally() {
        let mut lexer = Lexer::new(
            "try:
    pass
except:
    pass
else:
    pass
finally:
    pass",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Try(TryStmt {
                    block: Block {
                        stmts: vec![Statement::Pass(Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        })],
                        span: Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        }
                    },
                    finally_block: Some(FinallyBlock {
                        block: Block {
                            stmts: vec![Statement::Pass(Span {
                                row_start: 8,
                                row_end: 8,
                                column_start: 5,
                                column_end: 8
                            })],
                            span: Span {
                                row_start: 8,
                                row_end: 8,
                                column_start: 5,
                                column_end: 8
                            }
                        },
                        span: Span {
                            row_start: 7,
                            row_end: 8,
                            column_start: 1,
                            column_end: 8
                        }
                    }),
                    except_blocks: vec![ExceptBlock {
                        block: Block {
                            stmts: vec![Statement::Pass(Span {
                                row_start: 4,
                                row_end: 4,
                                column_start: 5,
                                column_end: 8
                            })],
                            span: Span {
                                row_start: 4,
                                row_end: 4,
                                column_start: 5,
                                column_end: 8
                            }
                        },
                        kind: ExceptBlockKind::Except,
                        expr: None,
                        expr_alias: None,
                        span: Span {
                            row_start: 3,
                            row_end: 4,
                            column_start: 1,
                            column_end: 8
                        }
                    }],
                    else_stmt: Some(ElseStmt {
                        block: Block {
                            stmts: vec![Statement::Pass(Span {
                                row_start: 6,
                                row_end: 6,
                                column_start: 5,
                                column_end: 8
                            })],
                            span: Span {
                                row_start: 6,
                                row_end: 6,
                                column_start: 5,
                                column_end: 8
                            }
                        },
                        span: Span {
                            row_start: 5,
                            row_end: 6,
                            column_start: 1,
                            column_end: 8
                        }
                    }),
                    span: Span {
                        row_start: 1,
                        row_end: 8,
                        column_start: 1,
                        column_end: 8
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_try_with_multiple_excepts() {
        let mut lexer = Lexer::new(
            "
try:
    pass
except:
    1 + 1
except:
    pass
",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Try(TryStmt {
                    block: Block {
                        stmts: vec![Statement::Pass(Span {
                            row_start: 3,
                            row_end: 3,
                            column_start: 5,
                            column_end: 8
                        })],
                        span: Span {
                            row_start: 3,
                            row_end: 3,
                            column_start: 5,
                            column_end: 8
                        }
                    },
                    finally_block: None,
                    except_blocks: vec![
                        ExceptBlock {
                            block: Block {
                                stmts: vec![Statement::Expression(Expression::BinaryOp(
                                    Box::new(Expression::Number(
                                        "1".into(),
                                        Span {
                                            row_start: 5,
                                            row_end: 5,
                                            column_start: 5,
                                            column_end: 5
                                        }
                                    )),
                                    BinaryOperator::Add,
                                    Box::new(Expression::Number(
                                        "1".into(),
                                        Span {
                                            row_start: 5,
                                            row_end: 5,
                                            column_start: 9,
                                            column_end: 9
                                        }
                                    )),
                                    Span {
                                        row_start: 5,
                                        row_end: 5,
                                        column_start: 5,
                                        column_end: 9
                                    }
                                ))],
                                span: Span {
                                    row_start: 5,
                                    row_end: 5,
                                    column_start: 5,
                                    column_end: 9
                                }
                            },
                            kind: ExceptBlockKind::Except,
                            expr: None,
                            expr_alias: None,
                            span: Span {
                                row_start: 4,
                                row_end: 5,
                                column_start: 1,
                                column_end: 9
                            }
                        },
                        ExceptBlock {
                            block: Block {
                                stmts: vec![Statement::Pass(Span {
                                    row_start: 7,
                                    row_end: 7,
                                    column_start: 5,
                                    column_end: 8
                                })],
                                span: Span {
                                    row_start: 7,
                                    row_end: 7,
                                    column_start: 5,
                                    column_end: 8
                                }
                            },
                            kind: ExceptBlockKind::Except,
                            expr: None,
                            expr_alias: None,
                            span: Span {
                                row_start: 6,
                                row_end: 7,
                                column_start: 1,
                                column_end: 8
                            }
                        }
                    ],
                    else_stmt: None,
                    span: Span {
                        row_start: 2,
                        row_end: 7,
                        column_start: 1,
                        column_end: 8
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_return_stmt() {
        let mut lexer = Lexer::new(
            "
def x():
    return
",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();
        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "x".into(),
                    parameters: vec![],
                    decorators: vec![],
                    block: Block {
                        stmts: vec![Statement::Return(ReturnStmt {
                            value: None,
                            span: Span {
                                row_start: 3,
                                row_end: 3,
                                column_start: 5,
                                column_end: 11
                            }
                        })],
                        span: Span {
                            row_start: 3,
                            row_end: 3,
                            column_start: 5,
                            column_end: 11
                        }
                    },
                    span: Span {
                        row_start: 2,
                        row_end: 3,
                        column_start: 1,
                        column_end: 11
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_return_stmt2() {
        let mut lexer = Lexer::new(
            "
def x():
    return 2 + 2
",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();
        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "x".into(),
                    parameters: vec![],
                    decorators: vec![],
                    block: Block {
                        stmts: vec![Statement::Return(ReturnStmt {
                            value: Some(Expression::BinaryOp(
                                Box::new(Expression::Number(
                                    "2".into(),
                                    Span {
                                        row_start: 3,
                                        row_end: 3,
                                        column_start: 12,
                                        column_end: 12
                                    }
                                )),
                                BinaryOperator::Add,
                                Box::new(Expression::Number(
                                    "2".into(),
                                    Span {
                                        row_start: 3,
                                        row_end: 3,
                                        column_start: 16,
                                        column_end: 16
                                    }
                                )),
                                Span {
                                    row_start: 3,
                                    row_end: 3,
                                    column_start: 12,
                                    column_end: 16
                                }
                            )),
                            span: Span {
                                row_start: 3,
                                row_end: 3,
                                column_start: 5,
                                column_end: 16
                            }
                        })],
                        span: Span {
                            row_start: 3,
                            row_end: 3,
                            column_start: 5,
                            column_end: 16
                        }
                    },
                    span: Span {
                        row_start: 2,
                        row_end: 3,
                        column_start: 1,
                        column_end: 16
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_yield_expr() {
        let mut lexer = Lexer::new(
            "
def test():
    yield 1
    yield 1, 2 + 3, abc
",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "test".into(),
                    parameters: vec![],
                    decorators: vec![],
                    block: Block {
                        stmts: vec![
                            Statement::Expression(Expression::Yield(
                                Some(Box::new(Expression::Number(
                                    "1".into(),
                                    Span {
                                        row_start: 3,
                                        row_end: 3,
                                        column_start: 11,
                                        column_end: 11
                                    }
                                ))),
                                Span {
                                    row_start: 3,
                                    row_end: 3,
                                    column_start: 5,
                                    column_end: 11
                                }
                            ),),
                            Statement::Expression(Expression::Yield(
                                Some(Box::new(Expression::Tuple(
                                    vec![
                                        Expression::Number(
                                            "1".into(),
                                            Span {
                                                row_start: 4,
                                                row_end: 4,
                                                column_start: 11,
                                                column_end: 11
                                            }
                                        ),
                                        Expression::BinaryOp(
                                            Box::new(Expression::Number(
                                                "2".into(),
                                                Span {
                                                    row_start: 4,
                                                    row_end: 4,
                                                    column_start: 14,
                                                    column_end: 14
                                                }
                                            )),
                                            BinaryOperator::Add,
                                            Box::new(Expression::Number(
                                                "3".into(),
                                                Span {
                                                    row_start: 4,
                                                    row_end: 4,
                                                    column_start: 18,
                                                    column_end: 18
                                                }
                                            )),
                                            Span {
                                                row_start: 4,
                                                row_end: 4,
                                                column_start: 14,
                                                column_end: 18
                                            }
                                        ),
                                        Expression::Id(
                                            "abc".into(),
                                            Span {
                                                row_start: 4,
                                                row_end: 4,
                                                column_start: 21,
                                                column_end: 23
                                            }
                                        )
                                    ],
                                    Span {
                                        row_start: 4,
                                        row_end: 4,
                                        column_start: 11,
                                        column_end: 23
                                    }
                                ))),
                                Span {
                                    row_start: 4,
                                    row_end: 4,
                                    column_start: 5,
                                    column_end: 23
                                }
                            ),)
                        ],
                        span: Span {
                            row_start: 3,
                            row_end: 4,
                            column_start: 5,
                            column_end: 23
                        }
                    },
                    span: Span {
                        row_start: 2,
                        row_end: 4,
                        column_start: 1,
                        column_end: 23
                    }
                }),]
            }
        );
    }

    #[test]
    fn parse_yield_from() {
        let mut lexer = Lexer::new(
            "
def test():
    yield from func()
",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "test".into(),
                    parameters: vec![],
                    decorators: vec![],
                    block: Block {
                        stmts: vec![Statement::Expression(Expression::YieldFrom(
                            Box::new(Expression::Call(FunctionCall {
                                lhs: Box::new(Expression::Id(
                                    "func".into(),
                                    Span {
                                        row_start: 3,
                                        row_end: 3,
                                        column_start: 16,
                                        column_end: 19
                                    }
                                )),
                                args: vec![],
                                span: Span {
                                    row_start: 3,
                                    row_end: 3,
                                    column_start: 16,
                                    column_end: 21
                                }
                            })),
                            Span {
                                row_start: 3,
                                row_end: 3,
                                column_start: 5,
                                column_end: 21
                            }
                        ),)],
                        span: Span {
                            row_start: 3,
                            row_end: 3,
                            column_start: 5,
                            column_end: 21
                        }
                    },
                    span: Span {
                        row_start: 2,
                        row_end: 3,
                        column_start: 1,
                        column_end: 21
                    }
                })]
            }
        );
    }

    #[test]
    fn parse_for_stmt() {
        let mut lexer = Lexer::new(
            "
for i in [1, 2, 3]:
    yield i
    yield i + 1
",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::For(ForStmt {
                    target: Expression::Id(
                        "i".into(),
                        Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 5
                        }
                    ),
                    iter: Expression::List(
                        vec![
                            Expression::Number(
                                "1".into(),
                                Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 11,
                                    column_end: 11
                                }
                            ),
                            Expression::Number(
                                "2".into(),
                                Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 14,
                                    column_end: 14
                                }
                            ),
                            Expression::Number(
                                "3".into(),
                                Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 17,
                                    column_end: 17
                                }
                            )
                        ],
                        Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 10,
                            column_end: 18
                        }
                    ),
                    block: Block {
                        stmts: vec![
                            Statement::Expression(Expression::Yield(
                                Some(Box::new(Expression::Id(
                                    "i".into(),
                                    Span {
                                        row_start: 3,
                                        row_end: 3,
                                        column_start: 11,
                                        column_end: 11
                                    }
                                ))),
                                Span {
                                    row_start: 3,
                                    row_end: 3,
                                    column_start: 5,
                                    column_end: 11
                                }
                            ),),
                            Statement::Expression(Expression::Yield(
                                Some(Box::new(Expression::BinaryOp(
                                    Box::new(Expression::Id(
                                        "i".into(),
                                        Span {
                                            row_start: 4,
                                            row_end: 4,
                                            column_start: 11,
                                            column_end: 11
                                        }
                                    )),
                                    BinaryOperator::Add,
                                    Box::new(Expression::Number(
                                        "1".into(),
                                        Span {
                                            row_start: 4,
                                            row_end: 4,
                                            column_start: 15,
                                            column_end: 15
                                        }
                                    )),
                                    Span {
                                        row_start: 4,
                                        row_end: 4,
                                        column_start: 11,
                                        column_end: 15
                                    }
                                ))),
                                Span {
                                    row_start: 4,
                                    row_end: 4,
                                    column_start: 5,
                                    column_end: 15
                                }
                            ),)
                        ],
                        span: Span {
                            row_start: 3,
                            row_end: 4,
                            column_start: 5,
                            column_end: 15
                        }
                    },
                    else_stmt: None,
                    span: Span {
                        row_start: 2,
                        row_end: 4,
                        column_start: 1,
                        column_end: 15
                    }
                })]
            }
        );
    }

    #[test]
    fn parse_for_stmt2() {
        let mut lexer = Lexer::new(
            "
for a, b in ((1, 2, 3)):
    ...
",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::For(ForStmt {
                    target: Expression::Tuple(
                        vec![
                            Expression::Id(
                                "a".into(),
                                Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 5,
                                    column_end: 5
                                }
                            ),
                            Expression::Id(
                                "b".into(),
                                Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 8,
                                    column_end: 8
                                }
                            )
                        ],
                        Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 8
                        }
                    ),
                    iter: Expression::Tuple(
                        vec![
                            Expression::Number(
                                "1".into(),
                                Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 15,
                                    column_end: 15
                                }
                            ),
                            Expression::Number(
                                "2".into(),
                                Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 18,
                                    column_end: 18
                                }
                            ),
                            Expression::Number(
                                "3".into(),
                                Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 21,
                                    column_end: 21
                                }
                            )
                        ],
                        Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 13,
                            column_end: 23
                        }
                    ),
                    block: Block {
                        stmts: vec![Statement::Expression(Expression::Ellipsis(Span {
                            row_start: 3,
                            row_end: 3,
                            column_start: 5,
                            column_end: 7
                        }),)],
                        span: Span {
                            row_start: 3,
                            row_end: 3,
                            column_start: 5,
                            column_end: 7
                        }
                    },
                    else_stmt: None,
                    span: Span {
                        row_start: 2,
                        row_end: 3,
                        column_start: 1,
                        column_end: 7
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_for_stmt3() {
        let mut lexer = Lexer::new(
            "
for *a in ((1, 2, 3)):
    ...
",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::For(ForStmt {
                    target: Expression::UnaryOp(
                        Box::new(Expression::Id(
                            "a".into(),
                            Span {
                                row_start: 2,
                                row_end: 2,
                                column_start: 6,
                                column_end: 6
                            }
                        )),
                        UnaryOperator::UnpackIterable,
                        Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 5,
                            column_end: 6
                        }
                    ),
                    iter: Expression::Tuple(
                        vec![
                            Expression::Number(
                                "1".into(),
                                Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 13,
                                    column_end: 13
                                }
                            ),
                            Expression::Number(
                                "2".into(),
                                Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 16,
                                    column_end: 16
                                }
                            ),
                            Expression::Number(
                                "3".into(),
                                Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 19,
                                    column_end: 19
                                }
                            )
                        ],
                        Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 11,
                            column_end: 21
                        }
                    ),
                    block: Block {
                        stmts: vec![Statement::Expression(Expression::Ellipsis(Span {
                            row_start: 3,
                            row_end: 3,
                            column_start: 5,
                            column_end: 7
                        }),)],
                        span: Span {
                            row_start: 3,
                            row_end: 3,
                            column_start: 5,
                            column_end: 7
                        }
                    },
                    else_stmt: None,
                    span: Span {
                        row_start: 2,
                        row_end: 3,
                        column_start: 1,
                        column_end: 7
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_raise_stmt() {
        let mut lexer = Lexer::new(
            "
raise
raise Exception
raise Exception from e
",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![
                    Statement::Raise(RaiseStmt {
                        exc: None,
                        from: None,
                        span: Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 1,
                            column_end: 5
                        }
                    }),
                    Statement::Raise(RaiseStmt {
                        exc: Some(Expression::Id(
                            "Exception".into(),
                            Span {
                                row_start: 3,
                                row_end: 3,
                                column_start: 7,
                                column_end: 15
                            }
                        )),
                        from: None,
                        span: Span {
                            row_start: 3,
                            row_end: 3,
                            column_start: 1,
                            column_end: 15
                        }
                    }),
                    Statement::Raise(RaiseStmt {
                        exc: Some(Expression::Id(
                            "Exception".into(),
                            Span {
                                row_start: 4,
                                row_end: 4,
                                column_start: 7,
                                column_end: 15
                            }
                        )),
                        from: Some(Expression::Id(
                            "e".into(),
                            Span {
                                row_start: 4,
                                row_end: 4,
                                column_start: 22,
                                column_end: 22
                            }
                        )),
                        span: Span {
                            row_start: 4,
                            row_end: 4,
                            column_start: 1,
                            column_end: 22
                        }
                    })
                ]
            }
        );
    }

    #[test]
    fn parse_del_stmt() {
        let mut lexer = Lexer::new(
            "
del a
del b, c, d
",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![
                    Statement::Del(DelStmt {
                        expr: Expression::Id(
                            "a".into(),
                            Span {
                                row_start: 2,
                                row_end: 2,
                                column_start: 5,
                                column_end: 5
                            }
                        ),
                        span: Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 1,
                            column_end: 5
                        }
                    }),
                    Statement::Del(DelStmt {
                        expr: Expression::Tuple(
                            vec![
                                Expression::Id(
                                    "b".into(),
                                    Span {
                                        row_start: 3,
                                        row_end: 3,
                                        column_start: 5,
                                        column_end: 5
                                    }
                                ),
                                Expression::Id(
                                    "c".into(),
                                    Span {
                                        row_start: 3,
                                        row_end: 3,
                                        column_start: 8,
                                        column_end: 8
                                    }
                                ),
                                Expression::Id(
                                    "d".into(),
                                    Span {
                                        row_start: 3,
                                        row_end: 3,
                                        column_start: 11,
                                        column_end: 11
                                    }
                                )
                            ],
                            Span {
                                row_start: 3,
                                row_end: 3,
                                column_start: 5,
                                column_end: 11
                            }
                        ),
                        span: Span {
                            row_start: 3,
                            row_end: 3,
                            column_start: 1,
                            column_end: 11
                        }
                    })
                ]
            }
        )
    }

    #[test]
    fn parse_function_call_with_arguments() {
        let mut lexer = Lexer::new("hello(1 + 2, True, y = None)");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Call(FunctionCall {
                    lhs: Box::new(Expression::Id(
                        "hello".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 5
                        }
                    )),
                    args: vec![
                        Expression::BinaryOp(
                            Box::new(Expression::Number(
                                "1".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 7,
                                    column_end: 7
                                }
                            )),
                            BinaryOperator::Add,
                            Box::new(Expression::Number(
                                "2".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 11,
                                    column_end: 11
                                }
                            )),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 7,
                                column_end: 11
                            }
                        ),
                        Expression::Bool(
                            true,
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 14,
                                column_end: 17
                            }
                        ),
                        Expression::Assign(Assign {
                            lhs: Box::new(Expression::Id(
                                "y".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 20,
                                    column_end: 20
                                }
                            )),
                            rhs: Box::new(Expression::None(Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 24,
                                column_end: 27
                            })),
                            span: Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 20,
                                column_end: 27
                            }
                        })
                    ],
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 28
                    }
                }))]
            }
        )
    }

    #[test]
    fn parse_subscript_with_string() {
        let mut lexer = Lexer::new("l[\"x\"]");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Subscript(Subscript {
                    lhs: Box::new(Expression::Id(
                        "l".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    slice: Box::new(SubscriptType::Subscript(Expression::String(
                        "x".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 3,
                            column_end: 5
                        }
                    ))),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 6
                    }
                }))]
            }
        )
    }

    #[test]
    fn parse_subscript_with_slice() {
        let mut lexer = Lexer::new(
            "
l[1:]
l[:1]
l[1:2]
l[1:2:3]
l[::]
l[:]
",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![
                    Statement::Expression(Expression::Subscript(Subscript {
                        lhs: Box::new(Expression::Id(
                            "l".into(),
                            Span {
                                row_start: 2,
                                row_end: 2,
                                column_start: 1,
                                column_end: 1
                            }
                        )),
                        slice: Box::new(SubscriptType::Slice {
                            lower: Some(Expression::Number(
                                "1".into(),
                                Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 3,
                                    column_end: 3
                                }
                            )),
                            upper: None,
                            step: None
                        }),
                        span: Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 1,
                            column_end: 5
                        }
                    })),
                    Statement::Expression(Expression::Subscript(Subscript {
                        lhs: Box::new(Expression::Id(
                            "l".into(),
                            Span {
                                row_start: 3,
                                row_end: 3,
                                column_start: 1,
                                column_end: 1
                            }
                        )),
                        slice: Box::new(SubscriptType::Slice {
                            lower: None,
                            upper: Some(Expression::Number(
                                "1".into(),
                                Span {
                                    row_start: 3,
                                    row_end: 3,
                                    column_start: 4,
                                    column_end: 4
                                }
                            )),
                            step: None
                        }),
                        span: Span {
                            row_start: 3,
                            row_end: 3,
                            column_start: 1,
                            column_end: 5
                        }
                    })),
                    Statement::Expression(Expression::Subscript(Subscript {
                        lhs: Box::new(Expression::Id(
                            "l".into(),
                            Span {
                                row_start: 4,
                                row_end: 4,
                                column_start: 1,
                                column_end: 1
                            }
                        )),
                        slice: Box::new(SubscriptType::Slice {
                            lower: Some(Expression::Number(
                                "1".into(),
                                Span {
                                    row_start: 4,
                                    row_end: 4,
                                    column_start: 3,
                                    column_end: 3
                                }
                            )),
                            upper: Some(Expression::Number(
                                "2".into(),
                                Span {
                                    row_start: 4,
                                    row_end: 4,
                                    column_start: 5,
                                    column_end: 5
                                }
                            )),
                            step: None
                        }),
                        span: Span {
                            row_start: 4,
                            row_end: 4,
                            column_start: 1,
                            column_end: 6
                        }
                    })),
                    Statement::Expression(Expression::Subscript(Subscript {
                        lhs: Box::new(Expression::Id(
                            "l".into(),
                            Span {
                                row_start: 5,
                                row_end: 5,
                                column_start: 1,
                                column_end: 1
                            }
                        )),
                        slice: Box::new(SubscriptType::Slice {
                            lower: Some(Expression::Number(
                                "1".into(),
                                Span {
                                    row_start: 5,
                                    row_end: 5,
                                    column_start: 3,
                                    column_end: 3
                                }
                            )),
                            upper: Some(Expression::Number(
                                "2".into(),
                                Span {
                                    row_start: 5,
                                    row_end: 5,
                                    column_start: 5,
                                    column_end: 5
                                }
                            )),
                            step: Some(Expression::Number(
                                "3".into(),
                                Span {
                                    row_start: 5,
                                    row_end: 5,
                                    column_start: 7,
                                    column_end: 7
                                }
                            ))
                        }),
                        span: Span {
                            row_start: 5,
                            row_end: 5,
                            column_start: 1,
                            column_end: 8
                        }
                    })),
                    Statement::Expression(Expression::Subscript(Subscript {
                        lhs: Box::new(Expression::Id(
                            "l".into(),
                            Span {
                                row_start: 6,
                                row_end: 6,
                                column_start: 1,
                                column_end: 1
                            }
                        )),
                        slice: Box::new(SubscriptType::Slice {
                            lower: None,
                            upper: None,
                            step: None
                        }),
                        span: Span {
                            row_start: 6,
                            row_end: 6,
                            column_start: 1,
                            column_end: 5
                        }
                    })),
                    Statement::Expression(Expression::Subscript(Subscript {
                        lhs: Box::new(Expression::Id(
                            "l".into(),
                            Span {
                                row_start: 7,
                                row_end: 7,
                                column_start: 1,
                                column_end: 1
                            }
                        )),
                        slice: Box::new(SubscriptType::Slice {
                            lower: None,
                            upper: None,
                            step: None
                        }),
                        span: Span {
                            row_start: 7,
                            row_end: 7,
                            column_start: 1,
                            column_end: 4
                        }
                    }))
                ]
            }
        )
    }

    #[test]
    fn parse_function_with_decorators() {
        let mut lexer = Lexer::new(
            "
@abc
@abc.cde
def test():
   ...
",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "test".into(),
                    parameters: vec![],
                    block: Block {
                        stmts: vec![Statement::Expression(Expression::Ellipsis(Span {
                            row_start: 5,
                            row_end: 5,
                            column_start: 4,
                            column_end: 6
                        }))],
                        span: Span {
                            row_start: 5,
                            row_end: 5,
                            column_start: 4,
                            column_end: 6
                        }
                    },
                    decorators: vec![
                        Expression::Id(
                            "abc".into(),
                            Span {
                                row_start: 2,
                                row_end: 2,
                                column_start: 2,
                                column_end: 4
                            }
                        ),
                        Expression::BinaryOp(
                            Box::new(Expression::Id(
                                "abc".into(),
                                Span {
                                    row_start: 3,
                                    row_end: 3,
                                    column_start: 2,
                                    column_end: 4
                                }
                            )),
                            BinaryOperator::AttributeRef,
                            Box::new(Expression::Id(
                                "cde".into(),
                                Span {
                                    row_start: 3,
                                    row_end: 3,
                                    column_start: 6,
                                    column_end: 8
                                }
                            )),
                            Span {
                                row_start: 3,
                                row_end: 3,
                                column_start: 2,
                                column_end: 8
                            }
                        )
                    ],
                    span: Span {
                        row_start: 2,
                        row_end: 5,
                        column_start: 1,
                        column_end: 6
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_list_comprehension() {
        let mut lexer = Lexer::new("[n for n in dir(module) if n[0] != \"_\"]");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::ListComp(ListComp {
                    target: Box::new(Expression::Id(
                        "n".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 2,
                            column_end: 2
                        }
                    )),
                    ifs: vec![IfComp {
                        cond: Expression::BinaryOp(
                            Box::new(Expression::Subscript(Subscript {
                                lhs: Box::new(Expression::Id(
                                    "n".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 28,
                                        column_end: 28
                                    }
                                )),
                                slice: Box::new(SubscriptType::Subscript(Expression::Number(
                                    "0".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 30,
                                        column_end: 30
                                    }
                                ))),
                                span: Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 28,
                                    column_end: 31
                                }
                            })),
                            BinaryOperator::NotEqual,
                            Box::new(Expression::String(
                                "_".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 36,
                                    column_end: 38
                                }
                            )),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 28,
                                column_end: 38
                            }
                        ),
                        span: Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 25,
                            column_end: 38
                        }
                    }],
                    fors: vec![ForComp {
                        target: Expression::Id(
                            "n".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 8,
                                column_end: 8
                            }
                        ),
                        iter: Expression::Call(FunctionCall {
                            lhs: Box::new(Expression::Id(
                                "dir".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 13,
                                    column_end: 15
                                }
                            )),
                            args: vec![Expression::Id(
                                "module".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 17,
                                    column_end: 22
                                }
                            )],
                            span: Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 13,
                                column_end: 23
                            }
                        }),
                        span: Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 4,
                            column_end: 23
                        }
                    }],
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 39
                    }
                }))]
            }
        );
    }

    #[test]
    fn parse_list_comprehension2() {
        let mut lexer = Lexer::new("[i for i in l for j in l if i > 0 if j > 1 for x in l]");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::ListComp(ListComp {
                    target: Box::new(Expression::Id(
                        "i".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 2,
                            column_end: 2
                        }
                    )),
                    ifs: vec![
                        IfComp {
                            cond: Expression::BinaryOp(
                                Box::new(Expression::Id(
                                    "i".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 29,
                                        column_end: 29
                                    }
                                )),
                                BinaryOperator::GreaterThan,
                                Box::new(Expression::Number(
                                    "0".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 33,
                                        column_end: 33
                                    }
                                )),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 29,
                                    column_end: 33
                                }
                            ),
                            span: Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 26,
                                column_end: 33
                            }
                        },
                        IfComp {
                            cond: Expression::BinaryOp(
                                Box::new(Expression::Id(
                                    "j".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 38,
                                        column_end: 38
                                    }
                                )),
                                BinaryOperator::GreaterThan,
                                Box::new(Expression::Number(
                                    "1".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 42,
                                        column_end: 42
                                    }
                                )),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 38,
                                    column_end: 42
                                }
                            ),
                            span: Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 35,
                                column_end: 42
                            }
                        }
                    ],
                    fors: vec![
                        ForComp {
                            target: Expression::Id(
                                "i".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 8,
                                    column_end: 8
                                }
                            ),
                            iter: Expression::Id(
                                "l".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 13,
                                    column_end: 13
                                }
                            ),
                            span: Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 4,
                                column_end: 13
                            }
                        },
                        ForComp {
                            target: Expression::Id(
                                "j".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 19,
                                    column_end: 19
                                }
                            ),
                            iter: Expression::Id(
                                "l".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 24,
                                    column_end: 24
                                }
                            ),
                            span: Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 15,
                                column_end: 24
                            }
                        },
                        ForComp {
                            target: Expression::Id(
                                "x".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 48,
                                    column_end: 48
                                }
                            ),
                            iter: Expression::Id(
                                "l".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 53,
                                    column_end: 53
                                }
                            ),
                            span: Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 44,
                                column_end: 53
                            }
                        }
                    ],
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 54
                    }
                }))]
            }
        );
    }

    #[test]
    fn parse_function_with_kw_only_parameters() {
        let mut lexer = Lexer::new(
            "
def test(x=0, *, y=0):
    ...",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "test".into(),
                    parameters: vec![
                        FuncParameter {
                            name: "x".into(),
                            default_value: Some(Expression::Number(
                                "0".into(),
                                Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 12,
                                    column_end: 12
                                }
                            )),
                            star_parameter_type: None,
                            is_kw_only: false,
                            is_pos_only: false,
                            span: Span {
                                row_start: 2,
                                row_end: 2,
                                column_start: 10,
                                column_end: 12
                            }
                        },
                        FuncParameter {
                            name: "y".into(),
                            default_value: Some(Expression::Number(
                                "0".into(),
                                Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 20,
                                    column_end: 20
                                }
                            )),
                            star_parameter_type: None,
                            is_kw_only: true,
                            is_pos_only: false,
                            span: Span {
                                row_start: 2,
                                row_end: 2,
                                column_start: 18,
                                column_end: 20
                            }
                        }
                    ],
                    block: Block {
                        stmts: vec![Statement::Expression(Expression::Ellipsis(Span {
                            row_start: 3,
                            row_end: 3,
                            column_start: 5,
                            column_end: 7
                        }))],
                        span: Span {
                            row_start: 3,
                            row_end: 3,
                            column_start: 5,
                            column_end: 7
                        }
                    },
                    decorators: vec![],
                    span: Span {
                        row_start: 2,
                        row_end: 3,
                        column_start: 1,
                        column_end: 7
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_function_with_kw_only_and_pos_only_parameters() {
        let mut lexer = Lexer::new(
            "
def test(x, /, *, y):
    ...",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "test".into(),
                    parameters: vec![
                        FuncParameter {
                            name: "x".into(),
                            default_value: None,
                            star_parameter_type: None,
                            is_kw_only: false,
                            is_pos_only: true,
                            span: Span {
                                row_start: 2,
                                row_end: 2,
                                column_start: 10,
                                column_end: 10
                            }
                        },
                        FuncParameter {
                            name: "y".into(),
                            default_value: None,
                            star_parameter_type: None,
                            is_kw_only: true,
                            is_pos_only: false,
                            span: Span {
                                row_start: 2,
                                row_end: 2,
                                column_start: 19,
                                column_end: 19
                            }
                        }
                    ],
                    block: Block {
                        stmts: vec![Statement::Expression(Expression::Ellipsis(Span {
                            row_start: 3,
                            row_end: 3,
                            column_start: 5,
                            column_end: 7
                        }))],
                        span: Span {
                            row_start: 3,
                            row_end: 3,
                            column_start: 5,
                            column_end: 7
                        }
                    },
                    decorators: vec![],
                    span: Span {
                        row_start: 2,
                        row_end: 3,
                        column_start: 1,
                        column_end: 7
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_assert_stmt() {
        let mut lexer = Lexer::new("assert 1 > x");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Assert(AssertStmt {
                    expr: Expression::BinaryOp(
                        Box::new(Expression::Number(
                            "1".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 8,
                                column_end: 8
                            }
                        )),
                        BinaryOperator::GreaterThan,
                        Box::new(Expression::Id(
                            "x".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 12,
                                column_end: 12
                            }
                        )),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 8,
                            column_end: 12
                        }
                    ),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 12
                    }
                })]
            }
        )
    }

    #[test]
    fn parse_generator_comprehension() {
        let mut lexer = Lexer::new("(i for i in l if i % 2 == 0)");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::GeneratorComp(GeneratorComp {
                    target: Box::new(Expression::Id(
                        "i".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 2,
                            column_end: 2
                        }
                    )),
                    ifs: vec![IfComp {
                        cond: Expression::BinaryOp(
                            Box::new(Expression::BinaryOp(
                                Box::new(Expression::Id(
                                    "i".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 18,
                                        column_end: 18
                                    }
                                )),
                                BinaryOperator::Modulo,
                                Box::new(Expression::Number(
                                    "2".into(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 22,
                                        column_end: 22
                                    }
                                )),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 18,
                                    column_end: 22
                                }
                            )),
                            BinaryOperator::Equals,
                            Box::new(Expression::Number(
                                "0".into(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 27,
                                    column_end: 27
                                }
                            )),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 18,
                                column_end: 27
                            }
                        ),
                        span: Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 15,
                            column_end: 27
                        }
                    }],
                    fors: vec![ForComp {
                        target: Expression::Id(
                            "i".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 8,
                                column_end: 8
                            }
                        ),
                        iter: Expression::Id(
                            "l".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 13,
                                column_end: 13
                            }
                        ),
                        span: Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 4,
                            column_end: 13
                        }
                    }],
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 28
                    }
                }))]
            }
        )
    }

    #[test]
    fn parse_invalid_global_statement() {
        let mut lexer = Lexer::new("global 1 + 1");
        let parser = Parser::new(&mut lexer);
        let (_, errors) = parser.parse();

        assert!(errors.is_some());
        assert_eq!(
            errors,
            Some(vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: unexpected token Number(Integer(Decimal), \"1\")".into(),
                span: Span {
                    row_start: 1,
                    row_end: 1,
                    column_start: 8,
                    column_end: 8,
                },
            },])
        )
    }

    #[test]
    fn parse_global_statement() {
        let mut lexer = Lexer::new("global x");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Global(GlobalStmt {
                    name: Expression::Id(
                        "x".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 8,
                            column_end: 8,
                        },
                    ),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 8,
                    },
                },),]
            }
        )
    }

    #[test]
    fn parse_nonlocal_statement() {
        let mut lexer = Lexer::new("nonlocal x");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::NonLocal(NonLocalStmt {
                    name: Expression::Id(
                        "x".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 10,
                            column_end: 10,
                        },
                    ),
                    span: Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 10,
                    },
                })]
            }
        )
    }

    #[test]
    fn parse_function_with_simple_stmts() {
        let mut lexer = Lexer::new(
            "
def t(): x = 1; return x
",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "t".into(),
                    parameters: vec![],
                    block: Block {
                        stmts: vec![
                            Statement::Expression(Expression::Assign(Assign {
                                lhs: Box::new(Expression::Id(
                                    "x".into(),
                                    Span {
                                        row_start: 2,
                                        row_end: 2,
                                        column_start: 10,
                                        column_end: 10,
                                    },
                                )),
                                rhs: Box::new(Expression::Number(
                                    "1".into(),
                                    Span {
                                        row_start: 2,
                                        row_end: 2,
                                        column_start: 14,
                                        column_end: 14,
                                    },
                                )),
                                span: Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 10,
                                    column_end: 14,
                                },
                            })),
                            Statement::Return(ReturnStmt {
                                value: Some(Expression::Id(
                                    "x".into(),
                                    Span {
                                        row_start: 2,
                                        row_end: 2,
                                        column_start: 24,
                                        column_end: 24,
                                    },
                                ),),
                                span: Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 17,
                                    column_end: 24,
                                },
                            })
                        ],
                        span: Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 10,
                            column_end: 24,
                        },
                    },
                    decorators: vec![],
                    span: Span {
                        row_start: 2,
                        row_end: 2,
                        column_start: 1,
                        column_end: 24,
                    },
                })]
            }
        )
    }

    #[test]
    fn parse_invalid_function_with_simple_stmts() {
        // FIXME: the `parse_simple_stmts` function is creating the AST for the "while" statement,
        // when it should be creating an invalid AST node
        let mut lexer = Lexer::new(
            "
def t(): x = 1; while x > 1: x -= 1
",
        );
        let parser = Parser::new(&mut lexer);
        let (_, errors) = parser.parse();

        assert!(errors.is_some());
        assert_eq!(
            errors,
            Some(vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: invalid syntax".into(),
                span: Span {
                    row_start: 2,
                    row_end: 2,
                    column_start: 17,
                    column_end: 21,
                },
            },])
        )
    }

    #[test]
    fn parse_invalid_function_with_simple_stmts2() {
        let mut lexer = Lexer::new(
            "
def t(): ;
",
        );
        let parser = Parser::new(&mut lexer);
        let (_, errors) = parser.parse();

        assert!(errors.is_some());
        assert_eq!(
            errors,
            Some(vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: invalid syntax, expecting an simple statement before the ';'".into(),
                span: Span {
                    row_start: 2,
                    row_end: 2,
                    column_start: 10,
                    column_end: 10,
                },
            },])
        )
    }

    #[test]
    fn parse_invalid_await_expr() {
        let mut lexer = Lexer::new("await");
        let parser = Parser::new(&mut lexer);
        let (_, errors) = parser.parse();

        assert_eq!(
            errors,
            Some(vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: invalid syntax, expecting expression".into(),
                span: Span {
                    row_start: 1,
                    row_end: 1,
                    column_start: 1,
                    column_end: 5,
                },
            }])
        );
    }

    #[test]
    fn parse_class_with_decorators() {
        let mut lexer = Lexer::new(
            "
@abc
@abc.cde
class Test:
   ...
",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Class(ClassStmt {
                    name: "Test".into(),
                    block: Block {
                        stmts: vec![Statement::Expression(Expression::Ellipsis(Span {
                            row_start: 5,
                            row_end: 5,
                            column_start: 4,
                            column_end: 6,
                        },),),],
                        span: Span {
                            row_start: 5,
                            row_end: 5,
                            column_start: 4,
                            column_end: 6,
                        },
                    },
                    base_classes: vec![],
                    decorators: vec![
                        Expression::Id(
                            "abc".into(),
                            Span {
                                row_start: 2,
                                row_end: 2,
                                column_start: 2,
                                column_end: 4,
                            },
                        ),
                        Expression::BinaryOp(
                            Box::new(Expression::Id(
                                "abc".into(),
                                Span {
                                    row_start: 3,
                                    row_end: 3,
                                    column_start: 2,
                                    column_end: 4,
                                },
                            )),
                            BinaryOperator::AttributeRef,
                            Box::new(Expression::Id(
                                "cde".into(),
                                Span {
                                    row_start: 3,
                                    row_end: 3,
                                    column_start: 6,
                                    column_end: 8,
                                },
                            )),
                            Span {
                                row_start: 3,
                                row_end: 3,
                                column_start: 2,
                                column_end: 8,
                            },
                        ),
                    ],
                    span: Span {
                        row_start: 2,
                        row_end: 5,
                        column_start: 1,
                        column_end: 6,
                    },
                })]
            }
        )
    }

    #[test]
    fn parse_async_function() {
        let mut lexer = Lexer::new(
            "
async def t():
   ...
",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::AsyncFunctionDef(Function {
                    name: "t".into(),
                    parameters: vec![],
                    block: Block {
                        stmts: vec![Statement::Expression(Expression::Ellipsis(Span {
                            row_start: 3,
                            row_end: 3,
                            column_start: 4,
                            column_end: 6,
                        }))],
                        span: Span {
                            row_start: 3,
                            row_end: 3,
                            column_start: 4,
                            column_end: 6,
                        },
                    },
                    decorators: vec![],
                    span: Span {
                        row_start: 2,
                        row_end: 3,
                        column_start: 1,
                        column_end: 6,
                    },
                })]
            }
        )
    }

    #[test]
    fn parse_async_with() {
        let mut lexer = Lexer::new(
            "
async with T():
   ...
",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::AsyncWith(WithStmt {
                    items: vec![WithItem {
                        item: Expression::Call(FunctionCall {
                            lhs: Box::new(Expression::Id(
                                "T".into(),
                                Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 12,
                                    column_end: 12,
                                },
                            )),
                            args: vec![],
                            span: Span {
                                row_start: 2,
                                row_end: 2,
                                column_start: 12,
                                column_end: 14,
                            },
                        }),
                        target: None,
                        span: Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 12,
                            column_end: 14,
                        },
                    },],
                    block: Block {
                        stmts: vec![Statement::Expression(Expression::Ellipsis(Span {
                            row_start: 3,
                            row_end: 3,
                            column_start: 4,
                            column_end: 6,
                        }))],
                        span: Span {
                            row_start: 3,
                            row_end: 3,
                            column_start: 4,
                            column_end: 6,
                        },
                    },
                    span: Span {
                        row_start: 2,
                        row_end: 3,
                        column_start: 1,
                        column_end: 6,
                    },
                })]
            }
        )
    }

    #[test]
    fn parse_async_for() {
        let mut lexer = Lexer::new(
            "
async for i in l:
   ...
",
        );
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::AsyncFor(ForStmt {
                    target: Expression::Id(
                        "i".into(),
                        Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 11,
                            column_end: 11,
                        },
                    ),
                    iter: Expression::Id(
                        "l".into(),
                        Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 16,
                            column_end: 16,
                        },
                    ),
                    block: Block {
                        stmts: vec![Statement::Expression(Expression::Ellipsis(Span {
                            row_start: 3,
                            row_end: 3,
                            column_start: 4,
                            column_end: 6,
                        }))],
                        span: Span {
                            row_start: 3,
                            row_end: 3,
                            column_start: 4,
                            column_end: 6,
                        },
                    },
                    else_stmt: None,
                    span: Span {
                        row_start: 2,
                        row_end: 3,
                        column_start: 1,
                        column_end: 6,
                    },
                })]
            }
        )
    }

    #[test]
    fn parse_invalid_async() {
        let mut lexer = Lexer::new(
            "
async class Test:
   ...
",
        );
        let parser = Parser::new(&mut lexer);
        let (_, errors) = parser.parse();

        assert!(errors.is_some());
        assert_eq!(
            errors,
            Some(vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: Expected \"def\", \"with\" or \"for\" to follow \"async\"".into(),
                span: Span {
                    row_start: 2,
                    row_end: 2,
                    column_start: 1,
                    column_end: 11,
                },
            }])
        )
    }

    #[test]
    fn parse_is_not_operator() {
        let mut lexer = Lexer::new("object.attr is not None");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::BinaryOp(
                    Box::new(Expression::BinaryOp(
                        Box::new(Expression::Id(
                            "object".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 1,
                                column_end: 6,
                            },
                        )),
                        BinaryOperator::AttributeRef,
                        Box::new(Expression::Id(
                            "attr".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 8,
                                column_end: 11,
                            },
                        )),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 11,
                        },
                    )),
                    BinaryOperator::IsNot,
                    Box::new(Expression::None(Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 20,
                        column_end: 23,
                    })),
                    Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 23,
                    },
                ))]
            }
        )
    }

    #[test]
    fn parse_not_in_operator() {
        let mut lexer = Lexer::new("object.attr not in list");
        let parser = Parser::new(&mut lexer);
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::BinaryOp(
                    Box::new(Expression::BinaryOp(
                        Box::new(Expression::Id(
                            "object".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 1,
                                column_end: 6,
                            },
                        )),
                        BinaryOperator::AttributeRef,
                        Box::new(Expression::Id(
                            "attr".into(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 8,
                                column_end: 11,
                            },
                        )),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 11,
                        },
                    )),
                    BinaryOperator::NotIn,
                    Box::new(Expression::Id(
                        "list".into(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 20,
                            column_end: 23,
                        }
                    )),
                    Span {
                        row_start: 1,
                        row_end: 1,
                        column_start: 1,
                        column_end: 23,
                    },
                ))]
            }
        )
    }
}
