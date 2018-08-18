use ast::*;
use scanner::*;

fn generate_expression(expression: &Expr) -> String {
    match expression {
        Expr::Const(num) => return format!("  movl ${}, %eax\n", num),
        Expr::BinOp(op, lhs, rhs) => {
            let mut generated: String;
            match op {
                Operator::Plus | Operator::Minus | Operator::Star | Operator::Slash | Operator::Modulo => {
                    // We reverse who is in ecx register because subtraction is dst - src -> dst.
                    // Otherwise we'd have to `movl %ecx, %eax`. This is an optimization.
                    if *op == Operator::Minus || *op == Operator::Slash {
                        generated = generate_expression(rhs);
                        generated.push_str("  push %eax\n");
                        generated.push_str(&generate_expression(lhs));
                        generated.push_str("  pop %ecx\n");
                        // rhs is now in ecx register
                    } else {
                        generated = generate_expression(lhs);
                        generated.push_str("  push %eax\n");
                        generated.push_str(&generate_expression(rhs));
                        generated.push_str("  pop %ecx\n");
                        // lhs is now in ecx register
                    }

                    match op {
                        Operator::Plus => generated.push_str("  addl %ecx, %eax\n"),
                        Operator::Minus => generated.push_str("  subl %ecx, %eax\n"),
                        Operator::Star => generated.push_str("  imul %ecx, %eax\n"),
                        Operator::Slash => generated.push_str("  idivl %ecx\n  movl %ecx, %eax\n"),
                        Operator::Modulo => generated.push_str("  idivl %ecx\n  movl %edx, %eax\n"),
                        _ => unimplemented!()
                    }
                }
                Operator::EqualEqual  | Operator::NotEqual  | // Equality and comparison
                Operator::LessThan    | Operator::LessEqual |
                Operator::GreaterThan | Operator::GreaterEqual => {
                    generated = generate_expression(rhs);
                    generated.push_str("  push %eax\n");
                    generated.push_str(&generate_expression(lhs)); // lhs is now in eax register
                    generated.push_str("  pop %ecx\n"); // rhs is now in ecx register
                    generated.push_str("  cmpl %eax, %ecx\n  movl $0, %eax\n");
                    generated.push_str(match op {
                        Operator::EqualEqual   => "  sete %al\n",
                        Operator::NotEqual     => "  setne %al\n",
                        Operator::LessThan     => "  setl %al\n",
                        Operator::LessEqual    => "  setle %al\n",
                        Operator::GreaterThan  => "  setg %al\n",
                        Operator::GreaterEqual => "  setge %al\n",
                        _ => unimplemented!()
                    });
                }
                Operator::Or | Operator::And => {
                    generated = generate_expression(lhs);
                    generated.push_str("  push %eax\n");
                    generated.push_str(&generate_expression(rhs));
                    generated.push_str("  pop %ecx\n");
                    generated.push_str(match op {
                        Operator::Or  => "  orl %ecx, %eax\n  movl $0, %eax\n  setne %al\n",
                        /* 1. Set `cl` to 1 if lhs != 0
                         * 2. Set `al` to 1 if rhs != 0
                         * 3. Store `al` and `cl` in `al`
                         */
                        Operator::And => "  cmpl $0, %eax\n  setne %cl\n  cmpl $0, %eax\n  movl $0, %eax\n  setne %al\n  andb %cl, %al\n",
                        _ => unsafe { ::std::hint::unreachable_unchecked() },
                    });
                }
                _ if op.is_bitwise() => {
                    generated = generate_expression(lhs);
                    generated.push_str("  push %eax\n");
                    generated.push_str(&generate_expression(rhs));
                    generated.push_str("  pop %ebx\n");
                    match op {
                        Operator::BitwiseAND => generated.push_str("  and %eax, %ebx\n"),
                        Operator::BitwiseOR => generated.push_str("  or %eax, %ebx\n"),
                        Operator::BitwiseXOR => generated.push_str("  xor %eax, %ebx\n"),
                        Operator::BitwiseShiftLeft => generated.push_str("  shl %eax, %ebx\n"),
                        Operator::BitwiseShiftRight => generated.push_str("  shr %eax, %ebx\n"),
                        _ => unimplemented!(), // should be impossible
                    }
                }
                _ => unimplemented!()
            }
            return generated;
        }
        Expr::UnaryOp(op, expr) => {
            let mut generated_expr = generate_expression(expr);
            match op {
                Operator::LogicalNegation => {
                    generated_expr.push_str("  cmpl $0, %eax\n  sete %al\n");
                }
                Operator::Minus => {
                    generated_expr.push_str("  neg %eax\n");
                }
                Operator::BitwiseComplement => {
                    generated_expr.push_str("  not %eax\n");
                }
                _ => unimplemented!(),
            }
            return generated_expr
        }
        _ => unimplemented!()
    }
}

fn generate_statement(statement: &Statement) -> String {
    let mut output = String::new();
    match statement {
        Statement::Return(expr) => {
            let mut generated_expr = generate_expression(expr);
            output.push_str(&generated_expr);
            output.push_str("  ret\n");
        }
    }
    output
}

pub fn generate(ast: &Program) -> String {
    let mut output = String::new();
    match ast {
        Program::Func(name, statement) => { // Code generated when a function is made
            output.push_str(&format!("  .globl _{0}\n_{0}:\n", name));
            output.push_str(&generate_statement(statement));
        }
    }
    output
}
