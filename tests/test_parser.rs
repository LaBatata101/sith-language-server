#[cfg(test)]
mod tests_parser {
    use python_parser::{
        lexer::token::Span,
        parser::{Assignment, Block, ElIfStmt, ElseStmt, Expression, Function, IfStmt, ParsedFile, Parser, Statement},
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
                        span: Span { start: 22, end: 32 },
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
                            span: Span { start: 27, end: 37 },
                        },
                        ElIfStmt {
                            condition: Expression::Bool(true, Span { start: 43, end: 47 }),
                            block: Block {
                                stmts: vec![Statement::Pass(Span { start: 53, end: 57 })],
                                span: Span { start: 53, end: 57 }
                            },
                            span: Span { start: 47, end: 57 },
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
                            span: Span { start: 27, end: 37 },
                        },
                        ElIfStmt {
                            condition: Expression::Bool(true, Span { start: 43, end: 47 }),
                            block: Block {
                                stmts: vec![Statement::Pass(Span { start: 53, end: 57 })],
                                span: Span { start: 53, end: 57 }
                            },
                            span: Span { start: 47, end: 57 },
                        },
                    ],
                    else_stmt: Some(ElseStmt {
                        block: Block {
                            stmts: vec![Statement::Pass(Span { start: 68, end: 72 })],
                            span: Span { start: 68, end: 72 }
                        },
                        span: Span { start: 62, end: 72 },
                    }),
                    span: Span { start: 0, end: 72 },
                })],
            }
        )
    }
}
