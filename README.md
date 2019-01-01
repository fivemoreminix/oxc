# Summary
A C compiler written in Rust for experimentation and understanding compilers. Should be easy to fork from and work with.
Everything is handwritten and the compiler uses no third-party libraries.

Please keep in mind, **the compiler is currently EXPERIMENTAL, and is NOT PRODUCTION READY.**

The compiler is roughly following [this article](https://norasandler.com/2017/11/29/Write-a-Compiler.html).

# Backend
The compiler generates 32-bit AT&T-style assembly. The backend can be easily replaced with any other. The code generator uses a recursive descent style similar
to the parser.
See `src/generator.rs`.

# Where The Language is Right Now
All of tests/stage_1 through tests/stage_6.

# Testing
```rs
$ cargo build
...
$ .\target\debug\oxc.exe .\test\stage_6\valid\statement\if_nested.c
.\test\stage_6\valid\statement\if_nested.c:
int main() {
    int a = 1;
    int b = 0;
    if (a)
        b = 1;
    else if (b)
        b = 2;
    return b;
}

Scanner production:
[Keyword(Int), Id("main"), Symbol(LParen), Symbol(RParen), Symbol(LBrace), Keyword(Int), Id("a"), Operator(Assignment), Integer(1), Symbol(Semicolon), Keyword(Int), Id("b"), Operator(Assignment), Integer(0), Symbol(Semicolon), Keyword(If), Symbol(LParen), Id("a"), Symbol(RParen), Id("b"), Operator(Assignment), Integer(1), Symbol(Semicolon), Keyword(Else), Keyword(If), Symbol(LParen), Id("b"), Symbol(RParen), Id("b"), Operator(Assignment), Integer(2), Symbol(Semicolon), Keyword(Return), Id("b"), Symbol(Semicolon), Symbol(RBrace)]

Abstract syntax tree:
Function(
    Function(
        "main",
        [
            Declaration(
                Declare(
                    "a",
                    Some(
                        Const(
                            1
                        )
                    )
                )
            ),
            Declaration(
                Declare(
                    "b",
                    Some(
                        Const(
                            0
                        )
                    )
                )
            ),
            Statement(
                Conditional(
                    Var(
                        "a"
                    ),
                    Expr(
                        Assign(
                            Assignment,
                            "b",
                            Const(
                                1
                            )
                        )
                    ),
                    Some(
                        Conditional(
                            Var(
                                "b"
                            ),
                            Expr(
                                Assign(
                                    Assignment,
                                    "b",
                                    Const(
                                        2
                                    )
                                )
                            ),
                            None
                        )
                    )
                )
            ),
            Statement(
                Return(
                    Var(
                        "b"
                    )
                )
            )
        ]
    )
)

Generated assembly:
  .globl _main
_main:
  push %ebp
  movl %esp, %ebp
  movl $1, %eax
  pushl %eax
  movl $0, %eax
  pushl %eax
  movl -4(%ebp), %eax
  cmpl $0, %eax
  je _c0_else
  movl $1, %eax
  movl %eax, -8(%ebp)
  jmp _c0_end
_c0_else:
  movl -8(%ebp), %eax
  cmpl $0, %eax
  je _c1_else
  movl $2, %eax
  movl %eax, -8(%ebp)
_c1_else:
_c0_end:
  movl -8(%ebp), %eax
_main_epilogue:
  movl %ebp, %esp
  pop %ebp
  ret

$ ./if_nested
$ echo $?
1
```
