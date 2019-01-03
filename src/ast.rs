use scanner::*;
use std::slice::Iter;
use std::iter::Peekable;
use peek_nth::{PeekableNth, IteratorExt};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Const(i32),
    Assign(Operator, String, Box<Expr>),
    Var(String),
    BinOp(Operator, Box<Expr>, Box<Expr>), // op, lhs, rhs
    UnaryOp(Operator, Box<Expr>),
    Conditional(Box<Expr>, Box<Expr>, Box<Expr>), // ternary if: expr1 ? expr2 : expr3
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Return(Expr),                  // Return statement
    Expr(Option<Expr>),            // Any expression
    Conditional(Expr, Box<Statement>, Option<Box<Statement>>), // if (expr) statement1 else statement2
    Compound(Vec<BlockItem>),      // { int foo = 2; foo += 3; }
    // for (initial clause; controlling expression; post-expression)
    ForExpr(Option<Expr>, Expr, Option<Expr>, Box<Statement>), // for (;;) say_hello();
    ForDecl(Declaration, Expr, Option<Expr>, Box<Statement>), // for (int a = 0; a < 3; a++) say_hello();
    While(Expr, Box<Statement>),
    Do(Box<Statement>, Expr),
    Break,
    Continue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Declare(String, Option<Expr>), // Variable declaration
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockItem {
    Statement(Statement),
    Declaration(Declaration),
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionDeclaration {
    Function(String, Vec<BlockItem>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Program {
    Function(FunctionDeclaration),
}

// program = function;
pub fn parse(tokens: &[Token]) -> Program {
    parse_function(&mut tokens.iter().peekable_nth())
}

// function = "int", identifier, "(", ")", "{", { block_item }, "}";
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
                                                                                            // Parse multiple block items
                                                                                            let mut block_items = Vec::<BlockItem>::new();
                                                                                            while tokens.peek_nth(0) != Some(&&Token::Symbol(Symbol::RBrace)) {
                                                                                                block_items.push(parse_block_item(tokens));
                                                                                            }

                                                                                            match tokens.next() {
                                                                                                Some(tok_close_brace) => {
                                                                                                    match tok_close_brace {
                                                                                                        Token::Symbol(Symbol::RBrace) => {
                                                                                                            return Program::Function(FunctionDeclaration::Function(id.to_owned(), block_items));
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

// block_item = statement | declaration ;
fn parse_block_item(tokens: &mut PeekableNth<Iter<Token>>) -> BlockItem {
    match tokens.peek_nth(0) {
        Some(Token::Keyword(Keyword::Int)) => BlockItem::Declaration(parse_declaration(tokens)),
        _ => BlockItem::Statement(parse_statement(tokens)),
    }
}

// declaration = "int", identifier, [ "=", expr ], ";" ;
fn parse_declaration(tokens: &mut PeekableNth<Iter<Token>>) -> Declaration {
    match tokens.next() {
        Some(Token::Keyword(Keyword::Int)) => {
            match tokens.next() {
                Some(Token::Id(id)) => {
                    let declaration;

                    if tokens.peek_nth(0) == Some(&&Token::Operator(Operator::Assignment)) {
                        tokens.next(); // Consume '='
                        declaration = Declaration::Declare(id.clone(), Some(parse_expr(tokens)));
                    } else {
                        declaration = Declaration::Declare(id.clone(), None);
                    }

                    match tokens.next() {
                        Some(Token::Symbol(Symbol::Semicolon)) => return declaration,
                        _ => panic!("Expected semicolon at end of declaration: {:?}", tokens),
                    }
                }
                _ => panic!("Expected identifier for integer declaration"),
            }
        }
        _ => panic!("Expected type 'int' for declaration"),
    }
}

// statement = "return", expr, ";"
//           | expr, ";"
//           | "if", "(", expr, ")", statement, [ "else", statement ] ;
fn parse_statement(tokens: &mut PeekableNth<Iter<Token>>) -> Statement {
    let statement: Statement;
    match tokens.peek_nth(0) {
        Some(Token::Keyword(Keyword::Return)) => {
            tokens.next();
            statement = Statement::Return(parse_expr(tokens));

            match tokens.next() {
                Some(Token::Symbol(Symbol::Semicolon)) => return statement,
                _ => panic!("Expected semicolon at end of statement: {:?}", tokens),
            }
        }
        Some(Token::Keyword(Keyword::If)) => {
            tokens.next();

            match tokens.next() {
                Some(Token::Symbol(Symbol::LParen)) => {
                    let condition = parse_expr(tokens);
                    match tokens.next() {
                        Some(Token::Symbol(Symbol::RParen)) => {
                            let true_statement = parse_statement(tokens);
                            match tokens.peek_nth(0) {
                                Some(Token::Keyword(Keyword::Else)) => {
                                    tokens.next();
                                    return Statement::Conditional(condition, Box::new(true_statement), Some(Box::new(parse_statement(tokens))))
                                }
                                _ => return Statement::Conditional(condition, Box::new(true_statement), None),
                            }
                        }
                        _ => panic!("Expected closing parenthesis after condition on 'if' statement"),
                    }
                }
                _ => panic!("Expected opening parenthesis before condition on 'if' statement"),
            }
        }
        Some(Token::Symbol(Symbol::LBrace)) => {
            tokens.next(); // consume opening brace

            let mut block_items = Vec::<BlockItem>::new();
            while tokens.peek() != Some(&&Token::Symbol(Symbol::RBrace)) {
                block_items.push(parse_block_item(tokens));
            }

            if tokens.next() != Some(&Token::Symbol(Symbol::RBrace)) {
                panic!("Expected closing brace after compound statement");
            }

            return Statement::Compound(block_items);
        }
        Some(Token::Keyword(Keyword::For)) => {
            tokens.next(); // consume 'for'

            if tokens.next() != Some(&Token::Symbol(Symbol::LParen)) {
                panic!("Expected closing parenthesis to begin 'for' loop");
            }

            match tokens.peek() {
                Some(Token::Keyword(kwd)) if kwd.is_type() => { // ForDecl
                    let initial_clause = parse_declaration(tokens);
                    // no need to consume semicolon; declaration requires one
                    let controlling_expr = parse_expr_option(tokens);
                    
                    if tokens.next() != Some(&Token::Symbol(Symbol::Semicolon)) {
                        panic!("Expected semicolon after second expression in 'for' loop");
                    }

                    let post_expr = parse_expr_option(tokens);

                    if tokens.next() != Some(&Token::Symbol(Symbol::RParen)) {
                        panic!("Expected closing parenthesis after 'for' loop");
                    }

                    return Statement::ForDecl(initial_clause, match controlling_expr {
                        Some(expr) => expr,
                        None => Expr::Const(1),
                    }, post_expr, Box::new(parse_statement(tokens)));
                }
                _ => { // ForExpr
                    let initial_clause = parse_expr_option(tokens);
                    
                    if tokens.next() != Some(&Token::Symbol(Symbol::Semicolon)) {
                        panic!("Expected semicolon after first expression in 'for' loop");
                    }

                    let controlling_expr = parse_expr_option(tokens);

                    if tokens.next() != Some(&Token::Symbol(Symbol::Semicolon)) {
                        panic!("Expected semicolon after second expression in 'for' loop");
                    }
                    
                    let post_expr = parse_expr_option(tokens);

                    if tokens.next() != Some(&Token::Symbol(Symbol::RParen)) {
                        panic!("Expected closing parenthesis after 'for' loop");
                    }

                    return Statement::ForExpr(initial_clause, match controlling_expr {
                        Some(expr) => expr,
                        None => Expr::Const(1),
                    }, post_expr, Box::new(parse_statement(tokens)));
                }
            }
        }
        Some(Token::Keyword(Keyword::While)) => {
            tokens.next(); // consume 'while'

            if tokens.next() != Some(&Token::Symbol(Symbol::LParen)) {
                panic!("Expected opening parenthesis to begin 'while' loop");
            }

            let expr = parse_expr(tokens);

            if tokens.next() != Some(&Token::Symbol(Symbol::RParen)) {
                panic!("Expected closing parenthesis after 'while' loop");
            }

            return Statement::While(expr, Box::new(parse_statement(tokens)));
        }
        Some(Token::Keyword(Keyword::Do)) => {
            tokens.next(); // consume 'do'

            let statement = parse_statement(tokens);

            if tokens.next() != Some(&Token::Keyword(Keyword::While)) {
                panic!("Expected 'while' after 'do' expression");
            }

            let expr = parse_expr(tokens);

            if tokens.next() != Some(&Token::Symbol(Symbol::RParen)) {
                panic!("Expected closing parenthesis after 'while' loop");
            }

            return Statement::Do(Box::new(statement), expr);
        }
        Some(Token::Keyword(Keyword::Break)) => {
            tokens.next();

            if tokens.next() != Some(&Token::Symbol(Symbol::Semicolon)) {
                panic!("Expected semicolon after statement");
            }

            return Statement::Break;
        }
        Some(Token::Keyword(Keyword::Continue)) => {
            tokens.next();

            if tokens.next() != Some(&Token::Symbol(Symbol::Semicolon)) {
                panic!("Expected semicolon after statement");
            }

            return Statement::Continue;
        }
        None => panic!("Expected statement"),
        _ => {
            statement = Statement::Expr(parse_expr_option(tokens));

            match tokens.next() {
                Some(Token::Symbol(Symbol::Semicolon)) => return statement,
                _ => panic!("Expected semicolon at end of statement: {:?}", tokens),
            }
        }
    }
}

// expr_option = expr, ";" | ";" ;
fn parse_expr_option(tokens: &mut PeekableNth<Iter<Token>>) -> Option<Expr> {
    match tokens.peek() {
        Some(Token::Symbol(Symbol::Semicolon)) | Some(Token::Symbol(Symbol::RParen)) => None,
        _ => {
            let expression = parse_expr(tokens);
            match tokens.peek() {
                Some(Token::Symbol(Symbol::Semicolon)) | Some(Token::Symbol(Symbol::RParen)) => Some(expression),
                _ => panic!("Expected semicolon after optional expression"),
            }
        }
    }
}

// expr = identifier, assignment_operator, expr
//      | conditional_expr ;
fn parse_expr(tokens: &mut PeekableNth<Iter<Token>>) -> Expr {
    match tokens.peek_nth(0) {
        Some(Token::Id(id)) => {
            match tokens.peek_nth(1) {
                Some(Token::Operator(op)) if op.is_assignment() => {
                    tokens.next(); // Consume identifier
                    tokens.next(); // Consume assignment operator
                    Expr::Assign(*op, id.clone(), Box::new(parse_expr(tokens)))
                }
                _ => parse_conditional_expr(tokens),
            }
        }
        _ => parse_conditional_expr(tokens),
    }
}

// conditional_expr = logical_or_expr, [ "?", expr, ":", conditional_expr ] ;
fn parse_conditional_expr(tokens: &mut PeekableNth<Iter<Token>>) -> Expr {
    let expr1 = parse_logical_or_expr(tokens);
    match tokens.peek_nth(0) {
        Some(Token::Operator(Operator::QuestionMark)) => {
            tokens.next();
            let expr2 = parse_expr(tokens);
            match tokens.next() {
                Some(Token::Symbol(Symbol::Colon)) => {
                    Expr::Conditional(Box::new(expr1), Box::new(expr2), Box::new(parse_conditional_expr(tokens)))
                }
                _ => panic!("Expected colon in ternary expression"),
            }
        }
        _ => expr1,
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
