#![cfg(test)]

use super::{*,  Expr::*, Statement::*, Declaration::*, BlockItem::*, FunctionDeclaration::*, Program::*};
use scanner::{*, Keyword::*, Symbol::*, Operator::*, Token::*};

#[test]
fn simple_main_return_zero() {
    use scanner::*;
    use scanner::Keyword::*;
    let tokens = [
        TokenData {
            token: Keyword(
                Int,
            ),
            slice: "int",
            line: 1,
            col: 1,
        },
        TokenData {
            token: Id(
                "main".to_owned(),
            ),
            slice: "main",
            line: 1,
            col: 5,
        },
        TokenData {
            token: Symbol(
                LParen,
            ),
            slice: "(",
            line: 1,
            col: 10,
        },
        TokenData {
            token: Symbol(
                RParen,
            ),
            slice: ")",
            line: 1,
            col: 11,
        },
        TokenData {
            token: Symbol(
                LBrace,
            ),
            slice: "{",
            line: 1,
            col: 13,
        },
        TokenData {
            token: Keyword(
                Return,
            ),
            slice: "return",
            line: 2,
            col: 4,
        },
        TokenData {
            token: Integer(
                0,
            ),
            slice: "0",
            line: 2,
            col: 11,
        },
        TokenData {
            token: Symbol(
                Semicolon,
            ),
            slice: ";",
            line: 2,
            col: 13,
        },
        TokenData {
            token: Symbol(
                RBrace,
            ),
            slice: "}",
            line: 3,
            col: 1,
        },
    ];

    let ast = parse(&tokens);
    
    let expected_ast = Program::Function(FunctionDeclaration::Function("main".to_owned(), vec!(BlockItem::Statement(super::Statement::Return(super::Expr::Const(0))))));
    
    assert_eq!(ast, expected_ast);
}

#[test]
fn simple_variables() {
    use scanner::*;
    use scanner::Keyword::*;
    let tokens = [
        TokenData {
            token: Keyword(
                Int,
            ),
            slice: "int",
            line: 1,
            col: 1,
        },
        TokenData {
            token: Id(
                "main".to_owned(),
            ),
            slice: "main",
            line: 1,
            col: 5,
        },
        TokenData {
            token: Symbol(
                LParen,
            ),
            slice: "(",
            line: 1,
            col: 10,
        },
        TokenData {
            token: Symbol(
                RParen,
            ),
            slice: ")",
            line: 1,
            col: 11,
        },
        TokenData {
            token: Symbol(
                LBrace,
            ),
            slice: "{",
            line: 1,
            col: 12,
        },
        TokenData {
            token: Keyword(
                Int,
            ),
            slice: "int",
            line: 2,
            col: 4,
        },
        TokenData {
            token: Id(
                "a".to_owned(),
            ),
            slice: "a",
            line: 2,
            col: 8,
        },
        TokenData {
            token: Operator(
                Assignment,
            ),
            slice: "=",
            line: 2,
            col: 11,
        },
        TokenData {
            token: Integer(
                2,
            ),
            slice: "2",
            line: 2,
            col: 12,
        },
        TokenData {
            token: Symbol(
                Semicolon,
            ),
            slice: ";",
            line: 2,
            col: 14,
        },
        TokenData {
            token: Keyword(
                If,
            ),
            slice: "if",
            line: 3,
            col: 4,
        },
        TokenData {
            token: Symbol(
                LParen,
            ),
            slice: "(",
            line: 3,
            col: 8,
        },
        TokenData {
            token: Id(
                "a".to_owned(),
            ),
            slice: "a",
            line: 3,
            col: 8,
        },
        TokenData {
            token: Operator(
                LessThan,
            ),
            slice: "<",
            line: 3,
            col: 11,
        },
        TokenData {
            token: Integer(
                3,
            ),
            slice: "3",
            line: 3,
            col: 12,
        },
        TokenData {
            token: Symbol(
                RParen,
            ),
            slice: ")",
            line: 3,
            col: 14,
        },
        TokenData {
            token: Symbol(
                LBrace,
            ),
            slice: "{",
            line: 3,
            col: 16,
        },
        TokenData {
            token: Symbol(
                LBrace,
            ),
            slice: "{",
            line: 4,
            col: 9,
        },
        TokenData {
            token: Keyword(
                Int,
            ),
            slice: "int",
            line: 5,
            col: 12,
        },
        TokenData {
            token: Id(
                "a".to_owned(),
            ),
            slice: "a",
            line: 5,
            col: 16,
        },
        TokenData {
            token: Operator(
                Assignment,
            ),
            slice: "=",
            line: 5,
            col: 19,
        },
        TokenData {
            token: Integer(
                3,
            ),
            slice: "3",
            line: 5,
            col: 20,
        },
        TokenData {
            token: Symbol(
                Semicolon,
            ),
            slice: ";",
            line: 5,
            col: 22,
        },
        TokenData {
            token: Keyword(
                Return,
            ),
            slice: "return",
            line: 6,
            col: 12,
        },
        TokenData {
            token: Id(
                "a".to_owned(),
            ),
            slice: "a",
            line: 6,
            col: 19,
        },
        TokenData {
            token: Symbol(
                Semicolon,
            ),
            slice: ";",
            line: 6,
            col: 21,
        },
        TokenData {
            token: Symbol(
                RBrace,
            ),
            slice: "}",
            line: 7,
            col: 9,
        },
        TokenData {
            token: Keyword(
                Return,
            ),
            slice: "return",
            line: 8,
            col: 8,
        },
        TokenData {
            token: Id(
                "a".to_owned(),
            ),
            slice: "a",
            line: 8,
            col: 15,
        },
        TokenData {
            token: Symbol(
                Semicolon,
            ),
            slice: ";",
            line: 8,
            col: 17,
        },
        TokenData {
            token: Symbol(
                RBrace,
            ),
            slice: "}",
            line: 9,
            col: 5,
        },
        TokenData {
            token: Symbol(
                RBrace,
            ),
            slice: "}",
            line: 10,
            col: 1,
        },
    ];

    let ast = parse(&tokens);
    
    let expected_ast = Program::Function(
        FunctionDeclaration::Function(
            "main".to_owned(),
            vec![
                Declaration(
                    Declare(
                        "a".to_owned(),
                        Some(
                            Const(
                                2,
                            ),
                        ),
                    ),
                ),
                Statement(
                    super::Statement::Conditional(
                        BinOp(
                            LessThan,
                            Box::new(Var(
                                "a".to_owned(),
                            )),
                            Box::new(Const(
                                3,
                            )),
                        ),
                        Box::new(Compound(
                            vec![
                                Statement(
                                    Compound(
                                        vec![
                                            Declaration(
                                                Declare(
                                                    "a".to_owned(),
                                                    Some(
                                                        Const(
                                                            3,
                                                        ),
                                                    ),
                                                ),
                                            ),
                                            Statement(
                                                super::Statement::Return(
                                                    Var(
                                                        "a".to_owned(),
                                                    ),
                                                ),
                                            ),
                                        ],
                                    ),
                                ),
                                Statement(
                                    super::Statement::Return(
                                        Var(
                                            "a".to_owned(),
                                        ),
                                    ),
                                ),
                            ],
                        )),
                        None,
                    ),
                ),
            ],
        ),
    );
    
    assert_eq!(ast, expected_ast);
}
