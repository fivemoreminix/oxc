use scanner::*;
use std::slice::Iter;
use std::iter::Peekable;
use peek_nth::{PeekableNth, IteratorExt};

#[derive(Debug, Clone)]
pub enum Expr {
    Const(i32),
    Assign(String, Box<Expr>),
    Var(String),
    BinOp(Operator, Box<Expr>, Box<Expr>), // op, lhs, rhs
    UnaryOp(Operator, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Expr),                  // Return statement
    Declare(String, Option<Expr>), // Variable declaration
    Expr(Expr),                    // Any expression
}

#[derive(Debug, Clone)]
pub enum Program {
    Func(String, Vec<Statement>),
}

pub fn parse(tokens: &[Token]) -> Program {
    parse_function(&mut tokens.iter().peekable_nth())
}

fn parse_function(tokens: &mut PeekableNth<Iter<Token>>) -> Program {
    match tokens.next() {
        Some(tok_type) => {
            match tok_type {
                Token::Keyword(keyword) => {
                    match keyword {
                        Keyword::Int => { // Make sure the keyword is a type
                            match tokens.next() {
                                Some(tok_name) => {
                                    match tok_name {
                                        Token::Id(id) => { // Make sure the name is an identifier
                                            match tokens.next() { // Left paren ...
                                                Some(tok_open_paren) => {
                                                    match tok_open_paren {
                                                        Token::Symbol(Symbol::LParen) => { // ... Left paren
                                                            match tokens.next() { // Right paren ...
                                                                Some(tok_close_paren) => {
                                                                    match tok_close_paren {
                                                                        Token::Symbol(Symbol::RParen) => { // ... Right paren
                                                                            match tokens.next() {
                                                                                Some(tok_open_brace) => { // Open brace ...
                                                                                    match tok_open_brace {
                                                                                        Token::Symbol(Symbol::LBrace) => { // ... Open brace
                                                                                            // Parse multiple statements
                                                                                            let mut statements = Vec::<Statement>::new();
                                                                                            while tokens.peek_nth(0) != Some(&&Token::Symbol(Symbol::RBrace)) {
                                                                                                statements.push(parse_statement(tokens));
                                                                                            }

                                                                                            match tokens.next() {
                                                                                                Some(tok_close_brace) => {
                                                                                                    match tok_close_brace {
                                                                                                        Token::Symbol(Symbol::RBrace) => {
                                                                                                            return Program::Func(id.to_owned(), statements);
                                                                                                        }
                                                                                                        _ => panic!("Expected closing brace"),
                                                                                                    }
                                                                                                }
                                                                                                None => panic!("Expected closing brace"),
                                                                                            }
                                                                                        }
                                                                                        _ => panic!("Expected opening brace"),
                                                                                    }
                                                                                }
                                                                                None => panic!("Expected opening brace"),
                                                                            }
                                                                        }
                                                                        _ => panic!("Expected closing parenthesis"),
                                                                    }
                                                                }
                                                                None => panic!("Expected closing parenthesis"),
                                                            }
                                                        }
                                                        _ => panic!("Expected opening parenthesis"),
                                                    }
                                                }
                                                None => panic!("Expected opening parenthesis"),
                                            }
                                        }
                                        _ => panic!("Expected identifier for function name"),
                                    }
                                }
                                None => panic!("Expected name for function"),
                            }
                        }
                        _ => panic!("Expected type for function"),
                    }
                }
                _ => panic!("Expected type for function"),
            }
        }
        None => panic!("Expected function"),
    }
}

// statement = "return", expr, ";"
//           | "int", identifier, [ "=", expr ], ";"
//           | expr, ";" ;
fn parse_statement(tokens: &mut PeekableNth<Iter<Token>>) -> Statement {
    let statement: Statement;
    match tokens.peek_nth(0) {
        Some(Token::Keyword(Keyword::Return)) => {
            tokens.next();
            statement = Statement::Return(parse_expr(tokens));
        }
        Some(Token::Keyword(Keyword::Int)) => {
            tokens.next();
            match tokens.next() {
                Some(Token::Id(id)) => {
                    if tokens.peek_nth(0) == Some(&&Token::Operator(Operator::Assignment)) {
                        tokens.next(); // Consume '='
                        statement = Statement::Declare(id.clone(), Some(parse_expr(tokens)));
                    } else {
                        statement = Statement::Declare(id.clone(), None);
                    }
                }
                _ => panic!("Expected identifier for integer declaration"),
            }
        }
        None => panic!("Expected statement"),
        _ => statement = Statement::Expr(parse_expr(tokens)),
    }

    match tokens.next() {
        Some(Token::Symbol(Symbol::Semicolon)) => return statement,
        _ => panic!("Expected semicolon at end of statement: {:?}", tokens),
    }
}

// expr = identifier, "=", expr
//      | logical_or_expr ;
fn parse_expr(tokens: &mut PeekableNth<Iter<Token>>) -> Expr {
    match tokens.peek_nth(0) {
        Some(Token::Id(id)) => {
            if tokens.peek_nth(1) == Some(&&Token::Operator(Operator::Assignment)) {
                tokens.next(); // Consume identifier
                tokens.next(); // Consume assignment operator
                Expr::Assign(id.clone(), Box::new(parse_expr(tokens)))
            } else {
                parse_logical_or_expr(tokens)
            }
        }
        _ => parse_logical_or_expr(tokens),
    }
}

fn parse_logical_or_expr(tokens: &mut PeekableNth<Iter<Token>>) -> Expr { // logical_or_expr = logical_and_expr, { "||", logical_and_expr }
    let mut expr = parse_logical_and_expr(tokens);
    loop {
        match tokens.peek_nth(0) {
            Some(Token::Operator(peek)) if peek == &Operator::Or => {
                let mut op = match tokens.next().unwrap() {
                    Token::Operator(oper) => oper,
                    _ => unsafe { ::std::hint::unreachable_unchecked() } // impossible
                };

                let next_expr = parse_logical_and_expr(tokens);
                expr = Expr::BinOp(*op, Box::new(expr), Box::new(next_expr));
            }
            _ => break, // no more matches
        }
    }
    expr
}

fn parse_logical_and_expr(tokens: &mut PeekableNth<Iter<Token>>) -> Expr { // logical_and_expr = equality_expr, { "&&", equality_expr }
    let mut expr = parse_equality_expr(tokens);
    loop {
        match tokens.peek_nth(0) {
            Some(Token::Operator(peek)) if peek == &Operator::And => {
                let mut op = match tokens.next().unwrap() {
                    Token::Operator(oper) => oper,
                    _ => unsafe { ::std::hint::unreachable_unchecked() } // impossible
                };

                let next_expr = parse_equality_expr(tokens);
                expr = Expr::BinOp(*op, Box::new(expr), Box::new(next_expr));
            }
            _ => break, // no more matches
        }
    }
    expr
}

fn parse_equality_expr(tokens: &mut PeekableNth<Iter<Token>>) -> Expr { // equality_expr = relational_expr, { ("!=" | "=="), relational_expr }
    let mut expr = parse_relational_expr(tokens);
    loop {
        match tokens.peek_nth(0) {
            Some(Token::Operator(peek)) if peek == &Operator::NotEqual || peek == &Operator::EqualEqual => {
                let mut op = match tokens.next().unwrap() {
                    Token::Operator(oper) => oper,
                    _ => unsafe { ::std::hint::unreachable_unchecked() } // impossible
                };

                let next_expr = parse_relational_expr(tokens);
                expr = Expr::BinOp(*op, Box::new(expr), Box::new(next_expr));
            }
            _ => break, // no more matches
        }
    }
    expr
}

fn parse_relational_expr(tokens: &mut PeekableNth<Iter<Token>>) -> Expr { // relational_expr = bitwise_expr, { ("<" | ">" | "<=" | ">="), bitwise_expr }
    let mut expr = parse_bitwise_expr(tokens);
    loop {
        match tokens.peek_nth(0) {
            Some(Token::Operator(peek))
            if peek == &Operator::LessThan || peek == &Operator::GreaterThan ||
            peek == &Operator::LessEqual || peek == &Operator::GreaterEqual => {
                let mut op = match tokens.next().unwrap() {
                    Token::Operator(oper) => oper,
                    _ => unsafe { ::std::hint::unreachable_unchecked() } // impossible
                };

                let next_expr = parse_bitwise_expr(tokens);
                expr = Expr::BinOp(*op, Box::new(expr), Box::new(next_expr));
            }
            _ => break, // no more matches
        }
    }
    expr
}

fn parse_bitwise_expr(tokens: &mut PeekableNth<Iter<Token>>) -> Expr { // bitwise_expr = additive_expr, { ("&" | "|" | "^" | "<<" | ">>"), additive_expr }
    // Bitwise expressions like 2 & 1, 2 ^ 1, etc.
    let mut term = parse_additive_expr(tokens);
    loop {
        match tokens.peek_nth(0) {
            Some(Token::Operator(peek)) if peek.is_bitwise() => {
                let mut op = match tokens.next().unwrap() {
                    Token::Operator(oper) => oper,
                    _ => unsafe { ::std::hint::unreachable_unchecked() } // impossible
                };

                let next_term = parse_additive_expr(tokens);
                term = Expr::BinOp(*op, Box::new(term), Box::new(next_term));
            }
            _ => break, // no more matches
        }
    }
    term
}

fn parse_additive_expr(tokens: &mut PeekableNth<Iter<Token>>) -> Expr { // additive_expr = term, { ("+" | "-"), term }
    // Number expressions like 1+1 or 2+3*2 being 2+(3*2) using associativity
    let mut term = parse_term(tokens);
    loop {
        match tokens.peek_nth(0) {
            Some(Token::Operator(peek)) if peek == &Operator::Plus || peek == &Operator::Minus => {
                let mut op = match tokens.next().unwrap() {
                    Token::Operator(oper) => oper,
                    _ => unsafe { ::std::hint::unreachable_unchecked() } // impossible
                };

                let next_term = parse_term(tokens);
                term = Expr::BinOp(*op, Box::new(term), Box::new(next_term));
            }
            _ => break, // no more matches
        }
    }
    term
}

fn parse_term(tokens: &mut PeekableNth<Iter<Token>>) -> Expr { // term = factor, { ("*" | "/" | "%"), factor }
    let mut term = parse_factor(tokens);
    loop {
        match tokens.peek_nth(0) {
            Some(Token::Operator(peek)) if peek == &Operator::Star || peek == &Operator::Slash || peek == &Operator::Modulo => {
                // More terms
                let mut op = match tokens.next().unwrap() {
                    Token::Operator(oper) => oper,
                    _ => unsafe { ::std::hint::unreachable_unchecked() } // impossible
                };

                let next_term = parse_factor(tokens);
                term = Expr::BinOp(*op, Box::new(term), Box::new(next_term));
            }
            _ => break, // no more matches
        }
    }
    term
}

// factor = "(", expr, ")"
//        | unary_op, factor
//        | int
//        | identifier ;
fn parse_factor(tokens: &mut PeekableNth<Iter<Token>>) -> Expr {
    match tokens.next() {
        Some(Token::Symbol(Symbol::LParen)) => { // factor = "(", expression, ")"
            let expr = parse_expr(tokens); // parse expression in parenthesis
            match tokens.next() { // closing parenthesis
                Some(Token::Symbol(Symbol::RParen)) => return expr,
                _ => panic!("Missing closing parenthesis on expression"),
            }
        }
        Some(Token::Operator(op)) if op.is_unary() => { // factor = unary_op, factor
            let factor = parse_factor(tokens);
            return Expr::UnaryOp(*op, Box::new(factor));
        }
        Some(Token::Integer(num)) => return Expr::Const(*num), // factor = int
        Some(Token::Id(id)) => return Expr::Var(id.clone()),
        _ => panic!("Expected factor: {:?}", tokens),
    }
}
