#[cfg(test)]
mod tests_parser {
    use pretty_assertions::assert_eq;
    use python_parser::{
        error::{PythonError, PythonErrorType},
        lexer::span::Span,
        parser::{
            ast::{
                AnnAssign, AssertStmt, Assign, AugAssign, AugAssignType, BinaryOperator, Block, ClassStmt, DelStmt,
                DictItemType, ElIfStmt, ElseStmt, ExceptBlock, ExceptBlockKind, Expression, FinallyBlock, ForComp,
                ForStmt, FromImportStmt, FuncParameter, Function, FunctionCall, GeneratorComp, GlobalStmt, IfComp,
                IfElseExpr, IfStmt, ImportModule, ImportStmt, LambdaExpr, ListComp, ParsedFile, RaiseStmt, ReturnStmt,
                StarParameterType, Statement, Subscript, SubscriptType, TryStmt, UnaryOperator, While, WithItem,
                WithStmt,
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
                    lhs: Box::new(Expression::Id(
                        "test".to_string(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 4
                        }
                    )),
                    rhs: Box::new(Expression::String(
                        "Hello World!".to_string(),
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
        let parser = Parser::new("test = 42; x = 12");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![
                    Statement::Expression(Expression::Assign(Assign {
                        lhs: Box::new(Expression::Id(
                            "test".to_string(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 1,
                                column_end: 4
                            }
                        )),
                        rhs: Box::new(Expression::Number(
                            "42".to_string(),
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
                            "x".to_string(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 12,
                                column_end: 12
                            }
                        )),
                        rhs: Box::new(Expression::Number(
                            "12".to_string(),
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
        let parser = Parser::new("x = True\ny = False");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![
                    Statement::Expression(Expression::Assign(Assign {
                        lhs: Box::new(Expression::Id(
                            "x".to_string(),
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
                            "y".to_string(),
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
        let parser = Parser::new("test =");
        let (_, errors) = parser.parse();

        assert!(errors.is_some());
        assert_eq!(
            errors.unwrap(),
            vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: unexpected token Eof".to_string(),
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
        let parser = Parser::new("x: int = 0");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::AnnAssign(AnnAssign {
                    lhs: Box::new(Expression::Id(
                        "x".to_string(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Some(Box::new(Expression::Number(
                        "0".to_string(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 10,
                            column_end: 10
                        }
                    ))),
                    typehint: Box::new(Expression::Id(
                        "int".to_string(),
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
        let parser = Parser::new("a, b, c = 1, 2, 3");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Tuple(
                        vec![
                            Expression::Id(
                                "a".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 1,
                                    column_end: 1
                                }
                            ),
                            Expression::Id(
                                "b".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 4,
                                    column_end: 4
                                }
                            ),
                            Expression::Id(
                                "c".to_string(),
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
                                "1".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 11,
                                    column_end: 11
                                }
                            ),
                            Expression::Number(
                                "2".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 14,
                                    column_end: 14
                                }
                            ),
                            Expression::Number(
                                "3".to_string(),
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
        let parser = Parser::new("a, b, c: tuple[int, int, int] = 1, 2, 3");
        let (_, errors) = parser.parse();

        assert!(errors.is_some());
        assert_eq!(
            errors.unwrap(),
            vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: only single target (not tuple) can be annotated".to_string(),
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
        let parser = Parser::new("[a, b, c] = 1, 2, 3");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::List(
                        vec![
                            Expression::Id(
                                "a".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 2,
                                    column_end: 2
                                }
                            ),
                            Expression::Id(
                                "b".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 5,
                                    column_end: 5
                                }
                            ),
                            Expression::Id(
                                "c".to_string(),
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
                                "1".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 13,
                                    column_end: 13
                                }
                            ),
                            Expression::Number(
                                "2".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 16,
                                    column_end: 16
                                }
                            ),
                            Expression::Number(
                                "3".to_string(),
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
        let parser = Parser::new("x: int");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::AnnAssign(AnnAssign {
                    lhs: Box::new(Expression::Id(
                        "x".to_string(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: None,
                    typehint: Box::new(Expression::Id(
                        "int".to_string(),
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
        let parser = Parser::new("x += 1");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::AugAssing(AugAssign {
                    lhs: Box::new(Expression::Id(
                        "x".to_string(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::Number(
                        "1".to_string(),
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
        let parser = Parser::new("x: int += 1");
        let (_, errors) = parser.parse();

        assert!(errors.is_some());
        assert_eq!(
            errors.unwrap(),
            vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: invalid syntax, typehint not allowed in this kind of expression".to_string(),
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
                            name: "y".to_string(),
                            default_value: Some(Expression::Number(
                                "42".to_string(),
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
                            name: "kwargs".to_string(),
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
        let parser = Parser::new("x = 1 + 2 + 3");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".to_string(),
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
                                "1".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 5,
                                    column_end: 5
                                }
                            )),
                            BinaryOperator::Add,
                            Box::new(Expression::Number(
                                "2".to_string(),
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
                            "3".to_string(),
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
        let parser = Parser::new("x = 1 + 2 * 3 / 2");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".to_string(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::BinaryOp(
                        Box::new(Expression::Number(
                            "1".to_string(),
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
                                    "2".to_string(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 9,
                                        column_end: 9
                                    }
                                )),
                                BinaryOperator::Multiply,
                                Box::new(Expression::Number(
                                    "3".to_string(),
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
                                "2".to_string(),
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
        let parser = Parser::new("x = 3 + -5");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".to_string(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::BinaryOp(
                        Box::new(Expression::Number(
                            "3".to_string(),
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
                                "5".to_string(),
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
        let parser = Parser::new("x = not 3 + -5");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".to_string(),
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
                                "3".to_string(),
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
                                    "5".to_string(),
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
        let parser = Parser::new("x = x + +5");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".to_string(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::BinaryOp(
                        Box::new(Expression::Id(
                            "x".to_string(),
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
                                "5".to_string(),
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
        let parser = Parser::new("a = x < y or 69 > 9 and not 101 >> 666");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "a".to_string(),
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
                                "x".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 5,
                                    column_end: 5
                                }
                            )),
                            BinaryOperator::LessThan,
                            Box::new(Expression::Id(
                                "y".to_string(),
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
                                    "69".to_string(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 14,
                                        column_end: 15
                                    }
                                )),
                                BinaryOperator::GreaterThan,
                                Box::new(Expression::Number(
                                    "9".to_string(),
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
                                        "101".to_string(),
                                        Span {
                                            row_start: 1,
                                            row_end: 1,
                                            column_start: 29,
                                            column_end: 31
                                        }
                                    )),
                                    BinaryOperator::BitwiseRightShift,
                                    Box::new(Expression::Number(
                                        "666".to_string(),
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
        let parser = Parser::new("x = a.b.c");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".to_string(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::BinaryOp(
                        Box::new(Expression::Id(
                            "a".to_string(),
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
                                "b".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 7,
                                    column_end: 7
                                }
                            )),
                            BinaryOperator::AttributeRef,
                            Box::new(Expression::Id(
                                "c".to_string(),
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
        let parser = Parser::new("x = hello()");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".to_string(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::Call(FunctionCall {
                        lhs: Box::new(Expression::Id(
                            "hello".to_string(),
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
        let parser = Parser::new("x = l[1 + 2]");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".to_string(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::Subscript(Subscript {
                        lhs: Box::new(Expression::Id(
                            "l".to_string(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 5,
                                column_end: 5
                            }
                        )),
                        slice: Box::new(SubscriptType::Subscript(Expression::BinaryOp(
                            Box::new(Expression::Number(
                                "1".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 7,
                                    column_end: 7
                                }
                            )),
                            BinaryOperator::Add,
                            Box::new(Expression::Number(
                                "2".to_string(),
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
        let parser = Parser::new("x = a not in b");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".to_string(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::BinaryOp(
                        Box::new(Expression::Id(
                            "a".to_string(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 5,
                                column_end: 5
                            }
                        )),
                        BinaryOperator::NotIn,
                        Box::new(Expression::Id(
                            "b".to_string(),
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
        let parser = Parser::new("x = ((1 + 2) * 54) / 3");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".to_string(),
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
                                    "1".to_string(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 7,
                                        column_end: 7
                                    }
                                )),
                                BinaryOperator::Add,
                                Box::new(Expression::Number(
                                    "2".to_string(),
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
                                "54".to_string(),
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
                            "3".to_string(),
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
        let parser = Parser::new("x = (1 + 2, True, y(), \"Hello\", l[i],)");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".to_string(),
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
                                    "1".to_string(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 6,
                                        column_end: 6
                                    }
                                )),
                                BinaryOperator::Add,
                                Box::new(Expression::Number(
                                    "2".to_string(),
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
                                    "y".to_string(),
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
                                "Hello".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 24,
                                    column_end: 30
                                }
                            ),
                            Expression::Subscript(Subscript {
                                lhs: Box::new(Expression::Id(
                                    "l".to_string(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 33,
                                        column_end: 33
                                    }
                                )),
                                slice: Box::new(SubscriptType::Subscript(Expression::Id(
                                    "i".to_string(),
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
        let parser = Parser::new("1 + 2, True, y(), \"Hello\", l[i],");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Tuple(
                    vec![
                        Expression::BinaryOp(
                            Box::new(Expression::Number(
                                "1".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 1,
                                    column_end: 1
                                }
                            )),
                            BinaryOperator::Add,
                            Box::new(Expression::Number(
                                "2".to_string(),
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
                                "y".to_string(),
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
                            "Hello".to_string(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 19,
                                column_end: 25
                            }
                        ),
                        Expression::Subscript(Subscript {
                            lhs: Box::new(Expression::Id(
                                "l".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 28,
                                    column_end: 28
                                }
                            )),
                            slice: Box::new(SubscriptType::Subscript(Expression::Id(
                                "i".to_string(),
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
        let parser = Parser::new("x = [1 + 2, True, y(), \"Hello\", l[i],]");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".to_string(),
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
                                    "1".to_string(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 6,
                                        column_end: 6
                                    }
                                )),
                                BinaryOperator::Add,
                                Box::new(Expression::Number(
                                    "2".to_string(),
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
                                    "y".to_string(),
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
                                "Hello".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 24,
                                    column_end: 30
                                }
                            ),
                            Expression::Subscript(Subscript {
                                lhs: Box::new(Expression::Id(
                                    "l".to_string(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 33,
                                        column_end: 33
                                    }
                                )),
                                slice: Box::new(SubscriptType::Subscript(Expression::Id(
                                    "i".to_string(),
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
        let parser = Parser::new("x = [*l, *[1,2,3], True]");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".to_string(),
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
                                    "l".to_string(),
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
                                            "1".to_string(),
                                            Span {
                                                row_start: 1,
                                                row_end: 1,
                                                column_start: 12,
                                                column_end: 12
                                            }
                                        ),
                                        Expression::Number(
                                            "2".to_string(),
                                            Span {
                                                row_start: 1,
                                                row_end: 1,
                                                column_start: 14,
                                                column_end: 14
                                            }
                                        ),
                                        Expression::Number(
                                            "3".to_string(),
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
        let parser = Parser::new("[**d, x]");
        let (_, errors) = parser.parse();

        assert!(errors.is_some());
        assert_eq!(
            errors,
            Some(vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: can't unpack dictionary inside list!".to_string(),
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
        let parser = Parser::new("[]");
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
        let parser = Parser::new("x = *iterable");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".to_string(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::UnaryOp(
                        Box::new(Expression::Id(
                            "iterable".to_string(),
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
        let parser = Parser::new("x = {1, True, \"hello\", *l,}");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".to_string(),
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
                                "1".to_string(),
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
                                "hello".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 15,
                                    column_end: 21
                                }
                            ),
                            Expression::UnaryOp(
                                Box::new(Expression::Id(
                                    "l".to_string(),
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
        let parser = Parser::new("x = {1: \"Hello\", 1 + 3: True, (6, 6): False,}");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".to_string(),
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
                                    "1".to_string(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 6,
                                        column_end: 6
                                    }
                                ),
                                Expression::String(
                                    "Hello".to_string(),
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
                                        "1".to_string(),
                                        Span {
                                            row_start: 1,
                                            row_end: 1,
                                            column_start: 18,
                                            column_end: 18
                                        }
                                    )),
                                    BinaryOperator::Add,
                                    Box::new(Expression::Number(
                                        "3".to_string(),
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
                                            "6".to_string(),
                                            Span {
                                                row_start: 1,
                                                row_end: 1,
                                                column_start: 32,
                                                column_end: 32
                                            }
                                        ),
                                        Expression::Number(
                                            "6".to_string(),
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
        let parser = Parser::new("x = {**d, 2: 5, **x,}");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".to_string(),
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
                                    "d".to_string(),
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
                                    "2".to_string(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 11,
                                        column_end: 11
                                    }
                                ),
                                Expression::Number(
                                    "5".to_string(),
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
                                    "x".to_string(),
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
        let parser = Parser::new("x = {**d, *x}");
        let (_, errors) = parser.parse();

        assert!(errors.is_some());
        assert_eq!(
            errors.unwrap(),
            vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: can't unpack iterable inside dictionary!".to_string(),
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
        let parser = Parser::new("x = 15 if 5 < x else 45");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".to_string(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    rhs: Box::new(Expression::IfElse(IfElseExpr {
                        lhs: Box::new(Expression::Number(
                            "15".to_string(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 5,
                                column_end: 6
                            }
                        )),
                        rhs: Box::new(Expression::Number(
                            "45".to_string(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 22,
                                column_end: 23
                            }
                        )),
                        condition: Box::new(Expression::BinaryOp(
                            Box::new(Expression::Number(
                                "5".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 11,
                                    column_end: 11
                                }
                            )),
                            BinaryOperator::LessThan,
                            Box::new(Expression::Id(
                                "x".to_string(),
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
        let parser = Parser::new("x = func() if (5 < x or x >= y) and is_id else func2() * 5");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Assign(Assign {
                    lhs: Box::new(Expression::Id(
                        "x".to_string(),
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
                                "func".to_string(),
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
                                    "func2".to_string(),
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
                                "5".to_string(),
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
                                        "5".to_string(),
                                        Span {
                                            row_start: 1,
                                            row_end: 1,
                                            column_start: 16,
                                            column_end: 16
                                        }
                                    )),
                                    BinaryOperator::LessThan,
                                    Box::new(Expression::Id(
                                        "x".to_string(),
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
                                        "x".to_string(),
                                        Span {
                                            row_start: 1,
                                            row_end: 1,
                                            column_start: 25,
                                            column_end: 25
                                        }
                                    )),
                                    BinaryOperator::GreaterThanOrEqual,
                                    Box::new(Expression::Id(
                                        "y".to_string(),
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
                                "is_id".to_string(),
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
        let parser = Parser::new("1 + 2; 3 + 4, 7, 8");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![
                    Statement::Expression(Expression::BinaryOp(
                        Box::new(Expression::Number(
                            "1".to_string(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 1,
                                column_end: 1
                            }
                        )),
                        BinaryOperator::Add,
                        Box::new(Expression::Number(
                            "2".to_string(),
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
                                    "3".to_string(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 8,
                                        column_end: 8
                                    }
                                )),
                                BinaryOperator::Add,
                                Box::new(Expression::Number(
                                    "4".to_string(),
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
                                "7".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 15,
                                    column_end: 15
                                }
                            ),
                            Expression::Number(
                                "8".to_string(),
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
        let parser = Parser::new("x");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Id(
                    "x".to_string(),
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
                            Box::new(Expression::Id(
                                "x".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 5,
                                    column_end: 5
                                }
                            )),
                            BinaryOperator::Walrus,
                            Box::new(Expression::Number(
                                "15".to_string(),
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
                            "5".to_string(),
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
                            "x".to_string(),
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
        let parser = Parser::new("x := 5 + 5");
        let (_, errors) = parser.parse();

        assert!(errors.is_some());
        assert_eq!(
            errors.unwrap(),
            vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: invalid assignment statement!".to_string(),
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
                                lhs: Box::new(Expression::Id(
                                    "func".to_string(),
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
                            UnaryOperator::Await,
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
                                "x".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 16,
                                    column_end: 16
                                }
                            )),
                            BinaryOperator::Exponent,
                            Box::new(Expression::Number(
                                "5".to_string(),
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
                        "3".to_string(),
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
                            "x".to_string(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 11,
                                column_end: 11
                            }
                        )),
                        BinaryOperator::Add,
                        Box::new(Expression::Number(
                            "1".to_string(),
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
                                "x".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 12,
                                    column_end: 12
                                }
                            )),
                            BinaryOperator::Add,
                            Box::new(Expression::Number(
                                "1".to_string(),
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
                    }
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
                        "Animal".to_string(),
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
                    }
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
                            lhs: Box::new(Expression::Id(
                                "open".to_string(),
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
                            "file".to_string(),
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
                            "Except".to_string(),
                            Span {
                                row_start: 3,
                                row_end: 3,
                                column_start: 8,
                                column_end: 13
                            }
                        )),
                        expr_alias: Some("e".to_string()),
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
                                        "1".to_string(),
                                        Span {
                                            row_start: 5,
                                            row_end: 5,
                                            column_start: 5,
                                            column_end: 5
                                        }
                                    )),
                                    BinaryOperator::Add,
                                    Box::new(Expression::Number(
                                        "1".to_string(),
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
                                Box::new(Expression::Number(
                                    "2".to_string(),
                                    Span {
                                        row_start: 3,
                                        row_end: 3,
                                        column_start: 12,
                                        column_end: 12
                                    }
                                )),
                                BinaryOperator::Add,
                                Box::new(Expression::Number(
                                    "2".to_string(),
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
                                            "1".to_string(),
                                            Span {
                                                row_start: 4,
                                                row_end: 4,
                                                column_start: 11,
                                                column_end: 11
                                            }
                                        ),
                                        Expression::BinaryOp(
                                            Box::new(Expression::Number(
                                                "2".to_string(),
                                                Span {
                                                    row_start: 4,
                                                    row_end: 4,
                                                    column_start: 14,
                                                    column_end: 14
                                                }
                                            )),
                                            BinaryOperator::Add,
                                            Box::new(Expression::Number(
                                                "3".to_string(),
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
                                            "abc".to_string(),
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
                                lhs: Box::new(Expression::Id(
                                    "func".to_string(),
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
                    target: Expression::Id(
                        "i".to_string(),
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
                                "1".to_string(),
                                Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 11,
                                    column_end: 11
                                }
                            ),
                            Expression::Number(
                                "2".to_string(),
                                Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 14,
                                    column_end: 14
                                }
                            ),
                            Expression::Number(
                                "3".to_string(),
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
                                    "i".to_string(),
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
                                        "i".to_string(),
                                        Span {
                                            row_start: 4,
                                            row_end: 4,
                                            column_start: 11,
                                            column_end: 11
                                        }
                                    )),
                                    BinaryOperator::Add,
                                    Box::new(Expression::Number(
                                        "1".to_string(),
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
                            Expression::Id(
                                "a".to_string(),
                                Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 5,
                                    column_end: 5
                                }
                            ),
                            Expression::Id(
                                "b".to_string(),
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
                                "1".to_string(),
                                Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 15,
                                    column_end: 15
                                }
                            ),
                            Expression::Number(
                                "2".to_string(),
                                Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 18,
                                    column_end: 18
                                }
                            ),
                            Expression::Number(
                                "3".to_string(),
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
                        Box::new(Expression::Id(
                            "a".to_string(),
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
                                "1".to_string(),
                                Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 13,
                                    column_end: 13
                                }
                            ),
                            Expression::Number(
                                "2".to_string(),
                                Span {
                                    row_start: 2,
                                    row_end: 2,
                                    column_start: 16,
                                    column_end: 16
                                }
                            ),
                            Expression::Number(
                                "3".to_string(),
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
                        span: Span {
                            row_start: 2,
                            row_end: 2,
                            column_start: 1,
                            column_end: 5
                        }
                    }),
                    Statement::Raise(RaiseStmt {
                        exc: Some(Expression::Id(
                            "Exception".to_string(),
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
                            "Exception".to_string(),
                            Span {
                                row_start: 4,
                                row_end: 4,
                                column_start: 7,
                                column_end: 15
                            }
                        )),
                        from: Some(Expression::Id(
                            "e".to_string(),
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
                        expr: Expression::Id(
                            "a".to_string(),
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
                                    "b".to_string(),
                                    Span {
                                        row_start: 3,
                                        row_end: 3,
                                        column_start: 5,
                                        column_end: 5
                                    }
                                ),
                                Expression::Id(
                                    "c".to_string(),
                                    Span {
                                        row_start: 3,
                                        row_end: 3,
                                        column_start: 8,
                                        column_end: 8
                                    }
                                ),
                                Expression::Id(
                                    "d".to_string(),
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
        let parser = Parser::new("hello(1 + 2, True, y = None)");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Call(FunctionCall {
                    lhs: Box::new(Expression::Id(
                        "hello".to_string(),
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
                                "1".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 7,
                                    column_end: 7
                                }
                            )),
                            BinaryOperator::Add,
                            Box::new(Expression::Number(
                                "2".to_string(),
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
                                "y".to_string(),
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
        let parser = Parser::new("l[\"x\"]");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::Subscript(Subscript {
                    lhs: Box::new(Expression::Id(
                        "l".to_string(),
                        Span {
                            row_start: 1,
                            row_end: 1,
                            column_start: 1,
                            column_end: 1
                        }
                    )),
                    slice: Box::new(SubscriptType::Subscript(Expression::String(
                        "x".to_string(),
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
                        lhs: Box::new(Expression::Id(
                            "l".to_string(),
                            Span {
                                row_start: 2,
                                row_end: 2,
                                column_start: 1,
                                column_end: 1
                            }
                        )),
                        slice: Box::new(SubscriptType::Slice {
                            lower: Some(Expression::Number(
                                "1".to_string(),
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
                            "l".to_string(),
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
                                "1".to_string(),
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
                            "l".to_string(),
                            Span {
                                row_start: 4,
                                row_end: 4,
                                column_start: 1,
                                column_end: 1
                            }
                        )),
                        slice: Box::new(SubscriptType::Slice {
                            lower: Some(Expression::Number(
                                "1".to_string(),
                                Span {
                                    row_start: 4,
                                    row_end: 4,
                                    column_start: 3,
                                    column_end: 3
                                }
                            )),
                            upper: Some(Expression::Number(
                                "2".to_string(),
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
                            "l".to_string(),
                            Span {
                                row_start: 5,
                                row_end: 5,
                                column_start: 1,
                                column_end: 1
                            }
                        )),
                        slice: Box::new(SubscriptType::Slice {
                            lower: Some(Expression::Number(
                                "1".to_string(),
                                Span {
                                    row_start: 5,
                                    row_end: 5,
                                    column_start: 3,
                                    column_end: 3
                                }
                            )),
                            upper: Some(Expression::Number(
                                "2".to_string(),
                                Span {
                                    row_start: 5,
                                    row_end: 5,
                                    column_start: 5,
                                    column_end: 5
                                }
                            )),
                            step: Some(Expression::Number(
                                "3".to_string(),
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
                            "l".to_string(),
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
                            "l".to_string(),
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
                            "abc".to_string(),
                            Span {
                                row_start: 2,
                                row_end: 2,
                                column_start: 2,
                                column_end: 4
                            }
                        ),
                        Expression::BinaryOp(
                            Box::new(Expression::Id(
                                "abc".to_string(),
                                Span {
                                    row_start: 3,
                                    row_end: 3,
                                    column_start: 2,
                                    column_end: 4
                                }
                            )),
                            BinaryOperator::AttributeRef,
                            Box::new(Expression::Id(
                                "cde".to_string(),
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
        let parser = Parser::new("[n for n in dir(module) if n[0] != \"_\"]");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::ListComp(ListComp {
                    target: Box::new(Expression::Id(
                        "n".to_string(),
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
                                    "n".to_string(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 28,
                                        column_end: 28
                                    }
                                )),
                                slice: Box::new(SubscriptType::Subscript(Expression::Number(
                                    "0".to_string(),
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
                                "_".to_string(),
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
                            "n".to_string(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 8,
                                column_end: 8
                            }
                        ),
                        iter: Expression::Call(FunctionCall {
                            lhs: Box::new(Expression::Id(
                                "dir".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 13,
                                    column_end: 15
                                }
                            )),
                            args: vec![Expression::Id(
                                "module".to_string(),
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
        let parser = Parser::new("[i for i in l for j in l if i > 0 if j > 1 for x in l]");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::ListComp(ListComp {
                    target: Box::new(Expression::Id(
                        "i".to_string(),
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
                                    "i".to_string(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 29,
                                        column_end: 29
                                    }
                                )),
                                BinaryOperator::GreaterThan,
                                Box::new(Expression::Number(
                                    "0".to_string(),
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
                                    "j".to_string(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 38,
                                        column_end: 38
                                    }
                                )),
                                BinaryOperator::GreaterThan,
                                Box::new(Expression::Number(
                                    "1".to_string(),
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
                                "i".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 8,
                                    column_end: 8
                                }
                            ),
                            iter: Expression::Id(
                                "l".to_string(),
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
                                "j".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 19,
                                    column_end: 19
                                }
                            ),
                            iter: Expression::Id(
                                "l".to_string(),
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
                                "x".to_string(),
                                Span {
                                    row_start: 1,
                                    row_end: 1,
                                    column_start: 48,
                                    column_end: 48
                                }
                            ),
                            iter: Expression::Id(
                                "l".to_string(),
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
        let parser = Parser::new(
            "
def test(x=0, *, y=0):
    ...",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "test".to_string(),
                    parameters: vec![
                        FuncParameter {
                            name: "x".to_string(),
                            default_value: Some(Expression::Number(
                                "0".to_string(),
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
                            name: "y".to_string(),
                            default_value: Some(Expression::Number(
                                "0".to_string(),
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
        let parser = Parser::new(
            "
def test(x, /, *, y):
    ...",
        );
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::FunctionDef(Function {
                    name: "test".to_string(),
                    parameters: vec![
                        FuncParameter {
                            name: "x".to_string(),
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
                            name: "y".to_string(),
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
        let parser = Parser::new("assert 1 > x");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Assert(AssertStmt {
                    expr: Expression::BinaryOp(
                        Box::new(Expression::Number(
                            "1".to_string(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 8,
                                column_end: 8
                            }
                        )),
                        BinaryOperator::GreaterThan,
                        Box::new(Expression::Id(
                            "x".to_string(),
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
        let parser = Parser::new("(i for i in l if i % 2 == 0)");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Expression(Expression::GeneratorComp(GeneratorComp {
                    target: Box::new(Expression::Id(
                        "i".to_string(),
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
                                    "i".to_string(),
                                    Span {
                                        row_start: 1,
                                        row_end: 1,
                                        column_start: 18,
                                        column_end: 18
                                    }
                                )),
                                BinaryOperator::Modulo,
                                Box::new(Expression::Number(
                                    "2".to_string(),
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
                                "0".to_string(),
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
                            "i".to_string(),
                            Span {
                                row_start: 1,
                                row_end: 1,
                                column_start: 8,
                                column_end: 8
                            }
                        ),
                        iter: Expression::Id(
                            "l".to_string(),
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
        let parser = Parser::new("global 1 + 1");
        let (_, errors) = parser.parse();

        assert!(errors.is_some());
        assert_eq!(
            errors,
            Some(vec![PythonError {
                error: PythonErrorType::Syntax,
                msg: "SyntaxError: unexpected token Number(Integer(Decimal), \"1\")".to_string(),
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
        let parser = Parser::new("global x");
        let (parsed_file, errors) = parser.parse();

        assert!(errors.is_none());
        assert_eq!(
            parsed_file,
            ParsedFile {
                stmts: vec![Statement::Global(GlobalStmt {
                    name: Expression::Id(
                        "x".to_string(),
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
}
