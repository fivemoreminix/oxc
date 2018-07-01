use ast::*;
use scanner::*;

fn generate_expression(expression: &Expr) -> String {
    match expression {
        Expr::Const(num) => return format!("  movl ${}, %eax\n", num),
        Expr::BinOp(op, lhs, rhs) => {
            let mut generated: String;
            // We reverse who is in ecx register because subtraction is dst - src -> dst.
            // Otherwise we'd have to `movl %ecx, %eax`. This is an optimization.
            if *op == Operator::Minus || *op == Operator::Slash {
                generated = generate_expression(rhs);
                generated.push_str("  push %eax\n");
                generated.push_str(&generate_expression(lhs));
                generated.push_str("  pop %ecx\n"); // rhs is now in ecx register
            } else {
                generated = generate_expression(lhs);
                generated.push_str("  push %eax\n");
                generated.push_str(&generate_expression(rhs));
                generated.push_str("  pop %ecx\n"); // lhs is now in ecx register
            }

            match op {
                Operator::Plus => generated.push_str("  addl %ecx, %eax\n"),
                Operator::Minus => generated.push_str("  subl %ecx, %eax\n"),
                Operator::Star => generated.push_str("  imul %ecx, %eax\n"),
                Operator::Slash => generated.push_str("  xor %edx, %edx\n  idivl %ecx\n  movl %ecx, %eax\n"),
                _ => unimplemented!()
            }

            return generated;
        }
        Expr::UnaryOp(op, expr) => {
            let mut generated_expr = generate_expression(expr);
            match op {
                Operator::LogicalNegation => {
                    generated_expr.push_str("  cmpl $0, %eax\n  xor %eax, %eax\n  sete %al\n");
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
        Program::Func(name, statement) => {
            output.push_str(&format!("  .globl _{0}\n_{0}:\n", name));
            output.push_str(&generate_statement(statement));
        }
    }
    output
}
