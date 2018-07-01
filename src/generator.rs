use ast::*;
use scanner::*;

fn generate_expression(expression: &Expr) -> String {
    match expression {
        Expr::Const(num) => return format!("  movl ${}, %eax\n", num),
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
