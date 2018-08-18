use scanner::*;
use std::slice::Iter;
use std::iter::Peekable;

#[derive(Debug, Clone)]
pub enum Expr {
    Const(i32),
    BinOp(Operator, Box<Expr>, Box<Expr>), // op, lhs, rhs
    UnaryOp(Operator, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Expr),
}

#[derive(Debug, Clone)]
pub enum Program {
    Func(String, Statement),
}

pub fn parse(tokens: &[Token]) -> Program {
    let mut tok = tokens.iter().peekable();
    parse_function(&mut tok)
}

fn parse_function(tokens: &mut Peekable<Iter<Token>>) -> Program {
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
                                                                                            let statement = parse_statement(tokens); // Statement

                                                                                            match tokens.next() {
                                                                                                Some(tok_close_brace) => {
                                                                                                    match tok_close_brace {
                                                                                                        Token::Symbol(Symbol::RBrace) => {
                                                                                                            return Program::Func(id.to_owned(), statement);
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

fn parse_statement(tokens: &mut Peekable<Iter<Token>>) -> Statement {
    match tokens.next() {
        Some(t1) => {
            match t1 {
                Token::Keyword(Keyword::Return) => {
                    let expr = parse_expr(tokens);
                    match tokens.next() {
                        Some(tok_semicolon) => {
                            match tok_semicolon {
                                Token::Symbol(Symbol::Semicolon) => {
                                    return Statement::Return(expr);                                                    
                                }
                                _ => panic!("Expected semicolon at end of statement"),
                            }
                        }
                        None => panic!("Expected semicolon at end of statement"),
                    }
                }
                _ => panic!("Expected return statement"),
            }
        }
        None => panic!("Expected statement"),
    }
}

fn parse_expr(tokens: &mut Peekable<Iter<Token>>) -> Expr { // expr = logical_and_expr, { "||", logical_and_expr }
    let mut expr = parse_logical_and_expr(tokens);
    loop {
        match tokens.peek() {
            Some(Token::Operator(peek)) if peek == &Operator::Or => {
                let mut op = match tokens.next().unwrap() {
                    Token::Operator(oper) => *oper,
                    _ => unsafe { ::std::hint::unreachable_unchecked() } // impossible
                };

                let next_expr = parse_logical_and_expr(tokens);
                expr = Expr::BinOp(op, Box::new(expr), Box::new(next_expr));
            }
            _ => break, // no more matches
        }
    }
    expr
}

fn parse_logical_and_expr(tokens: &mut Peekable<Iter<Token>>) -> Expr { // logical_and_expr = equality_expr, { "&&", equality_expr }
    let mut expr = parse_equality_expr(tokens);
    loop {
        match tokens.peek() {
            Some(Token::Operator(peek)) if peek == &Operator::And => {
                let mut op = match tokens.next().unwrap() {
                    Token::Operator(oper) => *oper,
                    _ => unsafe { ::std::hint::unreachable_unchecked() } // impossible
                };

                let next_expr = parse_equality_expr(tokens);
                expr = Expr::BinOp(op, Box::new(expr), Box::new(next_expr));
            }
            _ => break, // no more matches
        }
    }
    expr
}

fn parse_equality_expr(tokens: &mut Peekable<Iter<Token>>) -> Expr { // equality_expr = relational_expr, { ("!=" | "=="), relational_expr }
    let mut expr = parse_relational_expr(tokens);
    loop {
        match tokens.peek() {
            Some(Token::Operator(peek)) if peek == &Operator::NotEqual || peek == &Operator::EqualEqual => {
                let mut op = match tokens.next().unwrap() {
                    Token::Operator(oper) => *oper,
                    _ => unsafe { ::std::hint::unreachable_unchecked() } // impossible
                };

                let next_expr = parse_relational_expr(tokens);
                expr = Expr::BinOp(op, Box::new(expr), Box::new(next_expr));
            }
            _ => break, // no more matches
        }
    }
    expr
}

fn parse_relational_expr(tokens: &mut Peekable<Iter<Token>>) -> Expr { // relational_expr = bitwise_expr, { ("<" | ">" | "<=" | ">="), bitwise_expr }
    let mut expr = parse_bitwise_expr(tokens);
    loop {
        match tokens.peek() {
            Some(Token::Operator(peek))
            if peek == &Operator::LessThan || peek == &Operator::GreaterThan ||
            peek == &Operator::LessEqual || peek == &Operator::GreaterEqual => {
                let mut op = match tokens.next().unwrap() {
                    Token::Operator(oper) => *oper,
                    _ => unsafe { ::std::hint::unreachable_unchecked() } // impossible
                };

                let next_expr = parse_bitwise_expr(tokens);
                expr = Expr::BinOp(op, Box::new(expr), Box::new(next_expr));
            }
            _ => break, // no more matches
        }
    }
    expr
}

fn parse_bitwise_expr(tokens: &mut Peekable<Iter<Token>>) -> Expr { // bitwise_expr = additive_expr, { ("&" | "|" | "^" | "<<" | ">>"), additive_expr }
    // Bitwise expressions like 2 & 1, 2 ^ 1, etc.
    let mut term = parse_additive_expr(tokens);
    loop {
        match tokens.peek() {
            Some(Token::Operator(peek)) if peek.is_bitwise() => {
                let mut op = match tokens.next().unwrap() {
                    Token::Operator(oper) => *oper,
                    _ => unsafe { ::std::hint::unreachable_unchecked() } // impossible
                };

                let next_term = parse_additive_expr(tokens);
                term = Expr::BinOp(op, Box::new(term), Box::new(next_term));
            }
            _ => break, // no more matches
        }
    }
    term
}

fn parse_additive_expr(tokens: &mut Peekable<Iter<Token>>) -> Expr { // additive_expr = term, { ("+" | "-"), term }
    // Number expressions like 1+1 or 2+3*2 being 2+(3*2) using associativity
    let mut term = parse_term(tokens);
    loop {
        match tokens.peek() {
            Some(Token::Operator(peek)) if peek == &Operator::Plus || peek == &Operator::Minus => {
                let mut op = match tokens.next().unwrap() {
                    Token::Operator(oper) => *oper,
                    _ => unsafe { ::std::hint::unreachable_unchecked() } // impossible
                };

                let next_term = parse_term(tokens);
                term = Expr::BinOp(op, Box::new(term), Box::new(next_term));
            }
            _ => break, // no more matches
        }
    }
    term
}

fn parse_term(tokens: &mut Peekable<Iter<Token>>) -> Expr { // term = factor, { ("*" | "/" | "%"), factor }
    let mut term = parse_factor(tokens);
    loop {
        match tokens.peek() {
            Some(Token::Operator(peek)) if peek == &Operator::Star || peek == &Operator::Slash || peek == &Operator::Modulo => {
                // More terms
                let mut op = match tokens.next().unwrap() {
                    Token::Operator(oper) => *oper,
                    _ => unsafe { ::std::hint::unreachable_unchecked() } // impossible
                };

                let next_term = parse_factor(tokens);
                term = Expr::BinOp(op, Box::new(term), Box::new(next_term));
            }
            _ => break, // no more matches
        }
    }
    term
}

// factor = "(", expr, ")"
//        | unary_op, factor
//        | int;
fn parse_factor(tokens: &mut Peekable<Iter<Token>>) -> Expr {
    match tokens.next() {
        Some(next) => {
            match next {
                Token::Symbol(Symbol::LParen) => { // factor = "(", expression, ")"
                    let expr = parse_expr(tokens); // parse expression in parenthesis
                    match tokens.next() { // closing parenthesis
                        Some(Token::Symbol(Symbol::RParen)) => return expr,
                        Some(_) | None => panic!("Missing closing parenthesis on expression"),
                    }
                }
                Token::Operator(op) if op.is_unary() => { // factor = unary_op, factor
                    let factor = parse_factor(tokens);
                    return Expr::UnaryOp(*op, Box::new(factor));
                }
                Token::Integer(num) => return Expr::Const(*num), // factor = int
                _ => panic!("Expected factor"),
            }
        }
        _ => panic!("Missing term"),
    }
}
