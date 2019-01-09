use std::collections::HashMap;

use ast::*;
use scanner::*;

type VariableMap = HashMap<String, isize>; // name, location on stack

/// Gives priority to map2. map2 should always be the current scope.
fn merge_maps<'a>(map1: &'a VariableMap, map2: &'a VariableMap) -> VariableMap {
    let mut new_map = map1.clone();
    for (k, &v) in map2 {
        *new_map.entry(k.clone()).or_insert(0) = v;
    }
    new_map
}

#[derive(Debug, PartialEq)]
struct Context<'a> {
    function_name: String,
    outer_scope: &'a VariableMap,
    current_scope: &'a mut VariableMap,
    stack_index: &'a mut isize,
    counter: &'a mut u32,
    loop_label_begin: Option<String>,
    loop_label_end: Option<String>,
}

impl<'a> Context<'a> {
    pub fn new(function_name: String, outer_scope: &'a VariableMap, current_scope: &'a mut VariableMap, stack_index: &'a mut isize, counter: &'a mut u32, loop_label_begin: Option<String>, loop_label_end: Option<String>) -> Self {
        Context {
            function_name,
            outer_scope,
            current_scope,
            stack_index,
            counter,
            loop_label_begin,
            loop_label_end,
        }
    }
}

fn generate_expression(expression: &Option<&Expr>, context: &mut Context) -> String {
    match expression {
        Some(Expr::Const(num)) => return format!("  movl ${}, %eax\n", num),
        Some(Expr::BinOp(op, lhs, rhs)) => {
            let mut generated: String;
            match op {
                Operator::Plus | Operator::Minus | Operator::Star | Operator::Slash | Operator::Modulo => {
                    // We reverse who is in ecx register because subtraction is dst - src -> dst.
                    // Otherwise we'd have to `movl %ecx, %eax`. This is an optimization.
                    if *op == Operator::Minus || *op == Operator::Slash {
                        generated = generate_expression(&Some(&**rhs), context);
                        generated.push_str("  movl %eax, %ecx\n"); // rhs is now in ecx register
                        generated.push_str(&generate_expression(&Some(&**lhs), context));
                    } else {
                        generated = generate_expression(&Some(&**lhs), context);
                        generated.push_str("  movl %eax, %ecx\n"); // lhs is now in ecx register
                        generated.push_str(&generate_expression(&Some(&**rhs), context));
                    }

                    match op {
                        Operator::Plus => generated.push_str("  addl %ecx, %eax\n"),
                        Operator::Minus => generated.push_str("  subl %ecx, %eax\n"),
                        Operator::Star => generated.push_str("  imull %ecx\n"),
                        Operator::Slash => generated.push_str("  idivl %ecx\n  movl %ecx, %eax\n"),
                        Operator::Modulo => generated.push_str("  idivl %ecx\n  movl %edx, %eax\n"),
                        _ => unimplemented!()
                    }
                }
                Operator::EqualEqual  | Operator::NotEqual  | // Equality and comparison
                Operator::LessThan    | Operator::LessEqual |
                Operator::GreaterThan | Operator::GreaterEqual => {
                    generated = generate_expression(&Some(&**rhs), context);
                    generated.push_str("  push %eax\n");
                    generated.push_str(&generate_expression(&Some(&**lhs), context)); // lhs is now in eax register
                    generated.push_str("  pop %ecx\n"); // rhs is now in ecx register
                    generated.push_str("  cmpl %eax, %ecx\n");
                    generated.push_str("  xor %eax, %eax\n"); // zero eax register
                    generated.push_str(match op { // Ex. `sete %al` will set 1 on lower byte of eax if true
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
                    generated = generate_expression(&Some(&**lhs), context);
                    generated.push_str("  push %eax\n");
                    generated.push_str(&generate_expression(&Some(&**rhs), context));
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
                    generated = generate_expression(&Some(&**lhs), context);
                    generated.push_str("  push %eax\n");
                    generated.push_str(&generate_expression(&Some(&**rhs), context));
                    generated.push_str("  pop %ebx\n");
                    match op {
                        Operator::BitwiseAND => generated.push_str("  and %ebx, %eax\n"),
                        Operator::BitwiseOR => generated.push_str("  or %ebx, %eax\n"),
                        Operator::BitwiseXOR => generated.push_str("  xor %ebx, %eax\n"),
                        Operator::BitwiseShiftLeft => generated.push_str("  shl %ebx, %eax\n"),
                        Operator::BitwiseShiftRight => generated.push_str("  shr %ebx, %eax\n"),
                        _ => unimplemented!(), // should be impossible
                    }
                }
                _ => unimplemented!()
            }
            return generated;
        }
        Some(Expr::UnaryOp(op, expr)) => {
            let mut generated_expr = generate_expression(&Some(&**expr), context);
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
            return generated_expr;
        }
        Some(Expr::Assign(op, name, expr)) => { // `op` is guaranteed valid assignment operator by parser
            let mut output = generate_expression(&Some(&**expr), context);

            let offset: isize;
            if context.outer_scope.contains_key(name) {
                offset = *context.outer_scope.get(name).unwrap();
            } else if context.current_scope.contains_key(name) {
                offset = *context.current_scope.get(name).unwrap();
            } else {
                panic!("Attempting to assign to an undeclared variable");
            }

            match op {
                Operator::Assignment => output.push_str(&format!("  movl %eax, {}(%ebp)\n", offset)),
                Operator::PlusAssign => output.push_str(&format!("  addl %eax, {}(%ebp)\n", offset)),
                Operator::MinusAssign => output.push_str(&format!("  subl %eax, {}(%ebp)\n", offset)),
                Operator::StarAssign => output.push_str(&format!("  imull {}(%ebp)\n", offset)),
                Operator::SlashAssign => output.push_str(&format!("  movl %eax, %ecx\n  movl {0}(%ebp), %eax\n  idivl %ecx\n  movl %ecx, %eax\n  movl %eax, {0}(%ebp)\n", offset)),
                Operator::ModAssign => output.push_str(&format!("  movl %eax, %ecx\n  movl {0}(%ebp), %eax\n  idivl %ecx\n  movl %edx, %eax\n  movl %eax, {0}(%ebp)\n", offset)),
                // NOTE: Operators LeftShiftAssign, RightShiftAssign, ANDAssign, ORAssign, and XORAssign are all omitted until further development.
                _ => unimplemented!(),
            }

            return output;
        }
        Some(Expr::Var(name)) => {
            let offset: isize;
            if context.outer_scope.contains_key(name) {
                offset = *context.outer_scope.get(name).unwrap();
            } else if context.current_scope.contains_key(name) {
                offset = *context.current_scope.get(name).unwrap();
            } else {
                panic!("Attempting to assign to an undeclared variable {:?}", name);
            }

            return format!("  movl {}(%ebp), %eax\n", offset);
        }
        Some(Expr::Conditional(expr1, expr2, expr3)) => {
            // conditional
            let mut output = generate_expression(&Some(&**expr1), context);

            let label = format!("_t{}", *context.counter);
            *context.counter += 1;

            // if condition is false, jump to _tN_else
            output.push_str("  cmpl $0, %eax\n");
            output.push_str(&format!("  je {}_else\n", label));

            // condition is true
            output.push_str(&generate_expression(&Some(&**expr2), context));
            output.push_str(&format!("  jmp {}_end\n", label));

            // condition is false
            output.push_str(&format!("{}_else:\n", label));
            output.push_str(&generate_expression(&Some(&**expr3), context));

            output.push_str(&format!("{}_end:\n", label));

            return output;
        }
        None => return String::new(),
    }
}

fn generate_declaration(declaration: &Declaration, context: &mut Context) -> String {
    let mut output = String::new();
    match declaration {
        Declaration::Declare(name, value) => {
            if context.current_scope.contains_key(name) {
                panic!("Can't declare a variable twice in the same scope");
            }

            if let Some(expr) = value {
                output.push_str(&generate_expression(&Some(&*expr), context));
                //output.push_str("  pushl %eax\n");
                output.push_str(&format!("  movl %eax, {}(%ebp)\n", context.stack_index));
            } else {
                //output.push_str("  pushl $0\n");
                output.push_str(&format!("  movl $0, {}(%ebp)\n", context.stack_index));
            }

            context.current_scope.insert(name.clone(), *context.stack_index);
            *context.stack_index -= 4;
        }
    }
    output
}

fn generate_statement(statement: &Statement, context: &mut Context) -> String {
    let mut output = String::new();
    match statement {
        Statement::Return(expr) => {
            output.push_str(&generate_expression(&Some(&*expr), context));
            output.push_str(&format!("  jmp _{}_epilogue\n", context.function_name));
        }
        Statement::Expr(expr) => output.push_str(&generate_expression(&expr.as_ref(), context)),
        Statement::Conditional(condition, expr1, expr2) => {
            output.push_str(&generate_expression(&Some(&*condition), context));

            let label = format!("_c{}", *context.counter);
            *context.counter += 1;

            // if condition is false, jump to _cN_else
            output.push_str("  cmpl $0, %eax\n");
            output.push_str(&format!("  je {}_else\n", label));

            // condition is true
            output.push_str(&generate_statement(expr1, context));

            match expr2 {
                Some(else_expr) => {
                    // (for previous true statement)
                    output.push_str(&format!("  jmp {}_end\n", label));

                    // condition is false
                    output.push_str(&format!("{}_else:\n", label));
                    output.push_str(&generate_statement(else_expr, context));

                    output.push_str(&format!("{}_end:\n", label));
                }
                None => {
                    // no else statement
                    output.push_str(&format!("{}_else:\n", label));
                }
            }
        }
        Statement::Compound(block_items) => {
            return generate_block(block_items, context);
        }
        Statement::ForExpr(clause, control, post_expr, statement) => {
            let label = format!("_f{}", *context.counter);
            *context.counter += 1;

            output.push_str(&generate_expression(&clause.as_ref(), context));

            output.push_str(&format!("{}_begin:\n", label));

            output.push_str(&generate_expression(&Some(&control), context));

            output.push_str("  cmpl $0, %eax\n");
            output.push_str(&format!("  je {}_end\n", label));

            output.push_str(&generate_statement(statement, context));

            output.push_str(&generate_expression(&post_expr.as_ref(), context));

            output.push_str(&format!("  jmp {0}_begin\n{0}_end:\n", label));
        }
        Statement::ForDecl(clause, control, post_expr, statement) => {
            let label = format!("_f{}", *context.counter);
            *context.counter += 1;

            // output.push_str(&generate_declaration(clause, variables, &mut init_scope, stack_index, counter));
            let mut loop_scope = VariableMap::new();
            let mut loop_context = Context::new(context.function_name.clone(), context.current_scope, &mut loop_scope, context.stack_index, context.counter, context.loop_label_begin.clone(), context.loop_label_end.clone());
            output.push_str(&generate_declaration(clause, &mut loop_context));

            output.push_str(&format!("{}_begin:\n", label));

            output.push_str(&generate_expression(&Some(&control), &mut loop_context));

            output.push_str("  cmpl $0, %eax\n");
            output.push_str(&format!("  je {}_end\n", label));

            output.push_str(&generate_statement(statement, &mut loop_context));

            output.push_str(&generate_expression(&post_expr.as_ref(), &mut loop_context));

            output.push_str(&format!("  jmp {0}_begin\n{0}_end:\n", label));

            // deallocate initialization variable(s)
            if !loop_context.current_scope.is_empty() {
                let bytes_to_deallocate = 4 * loop_context.current_scope.len();
                output.push_str(&format!("  addl ${}, %esp\n", bytes_to_deallocate));
            }
        }
        Statement::While(expr, statement) => {
            let label = format!("_w{}", *context.counter);
            *context.counter += 1;

            output.push_str(&format!("{}_begin:\n", label));

            output.push_str(&generate_expression(&Some(&expr), context));

            output.push_str("  cmpl $0, %eax\n");
            output.push_str(&format!("  je {}_end\n", label));

            output.push_str(&generate_statement(statement, context));

            output.push_str(&format!("  jmp {0}_begin\n{0}_end:\n", label));
        }
        Statement::Do(statement, expr) => {
            let label = format!("_w{}", *context.counter);
            *context.counter += 1;

            output.push_str(&format!("{}_begin:\n", label));

            output.push_str(&generate_statement(statement, context));

            output.push_str(&generate_expression(&Some(&expr), context));

            output.push_str("  cmpl $0, %eax\n");
            output.push_str(&format!("  jne {}_begin\n", label));
        }
        Statement::Break => {
            match &context.loop_label_end {
                Some(label) => output.push_str(&format!("  jmp {}\n", label)),
                None => panic!("Cannot 'break' from this location"),
            }
        }
        Statement::Continue => {
            match &context.loop_label_begin {
                Some(label) => output.push_str(&format!("  jmp {}\n", label)),
                None => panic!("Cannot 'continue' from this location"),
            }
        }
    }
    output
}

fn generate_block_item(block_item: &BlockItem, context: &mut Context) -> String {
    match block_item {
        BlockItem::Declaration(declaration) => generate_declaration(declaration, context),
        BlockItem::Statement(statement) => generate_statement(statement, context),
    }
}

fn generate_block(block_items: &Vec<BlockItem>, context: &mut Context) -> String {
    let mut output = String::new();
    let mut inner_scope = merge_maps(&context.outer_scope, &context.current_scope);
    for block_item in block_items {
        output.push_str(&generate_block_item(block_item, {
            &mut Context::new(context.function_name.clone(), context.current_scope, &mut inner_scope, context.stack_index, context.counter, context.loop_label_begin.clone(), context.loop_label_end.clone())
        }));
    }

    // deallocate variables
    if !inner_scope.is_empty() {
        let bytes_to_deallocate = 4 * inner_scope.len();
        output.push_str(&format!("  addl ${}, %esp\n", bytes_to_deallocate));
    }

    output
}

pub fn generate_function(function: &FunctionDeclaration, outer_scope: &VariableMap, counter: &mut u32) -> String {
    let mut output = String::new();
    match function {
        FunctionDeclaration::Function(name, block_items) => {
            let mut variable_map = VariableMap::new();
            let mut stack_index = -4isize; // ESP - 4

            if name == "main" {
                if cfg!(target_os = "linux") {
                    output.push_str(&format!("  .globl main\nmain:\n"));
                } else if cfg!(target_os = "windows") || cfg!(target_os = "macos") {
                    output.push_str(&format!("  .globl _main\n_main:\n"));
                } else {
                    unimplemented!();
                }
            } else {
                output.push_str(&format!("  .globl _{0}\n_{0}:\n", name));
            }

            // Function prologue
            output.push_str("  push %ebp\n  movl %esp, %ebp\n");

            output.push_str(&generate_block(
                block_items,
                &mut Context {
                    function_name: name.clone(),
                    outer_scope: outer_scope,
                    current_scope: &mut variable_map,
                    stack_index: &mut stack_index,
                    counter: counter,
                    loop_label_end: None,
                    loop_label_begin: None,
                },
            ));

            if !output.ends_with(&format!("jmp _{}_epilogue\n", name)) {
                // No return issued, so we return zero by default
                output.push_str("  movl $0, %eax\n");
            } else {
                // Output ends with "jmp _{}_epilogue", which is dumb because we define the epilogue immediately after
                output = output[0..output.len() - format!("  jmp _{}_epilogue\n", name).len()].to_owned();
            }

            // Function epilogue
            output.push_str(&format!("_{}_epilogue:\n  movl %ebp, %esp\n  pop %ebp\n  ret\n", name));
        }
    }
    output
}

pub fn generate(ast: &Program) -> String {
    let mut output = String::new();
    let mut block_counter = 0u32;
    match ast {
        Program::Function(function) => { // Code generated when a function is made
            output.push_str(&generate_function(function, &VariableMap::new(), &mut block_counter));
        }
    }
    output
}
