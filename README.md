# Summary
A C compiler written in Rust for understanding compilers and experimentation. Should be easy to fork from and work with.
Everything is handwritten and the compiler uses no third-party libraries.

Please keep in mind, **the compiler is currently EXPERIMENTAL, and is NOT PRODUCTION READY.**

The compiler is roughly following [this article](https://norasandler.com/2017/11/29/Write-a-Compiler.html).

# Backend
The compiler generates 32-bit AT&T-style assembly. The backend can be easily replaced with any other. The code generator uses a recursive descent style similar
to the parser.
See `src/generator.rs`.

# Where The Language is Right Now
All of tests/stage_1 through tests/stage_4.

# Testing
```rs
$ cargo build
...
$ .\target\debug\oxc.exe .\test\stage_5\valid\exp_return_val.c
.\test\stage_5\valid\exp_return_val.c:
int main() {
    int a;
    int b;
    a = b = 4;
    return a - b;
}

Scanner production:
[Keyword(Int), Id("main"), Symbol(LParen), Symbol(RParen), Symbol(LBrace), Keyword(Int), Id("a"), Symbol(Semicolon), Keyword(Int), Id("b"), Symbol(Semicolon), Id("a"), Operator(Assignment), Id("b"),
Operator(Assignment), Integer(4), Symbol(Semicolon), Keyword(Return), Id("a"), Operator(Minus), Id("b"), Symbol(Semicolon), Symbol(RBrace)]

Abstract syntax tree:
Func(
    "main",
    [
        Declare(
            "a",
            None
        ),
        Declare(
            "b",
            None
        ),
        Expr(
            Assign(
                "a",
                Assign(
                    "b",
                    Const(
                        4
                    )
                )
            )
        ),
        Return(
            BinOp(
                Minus,
                Var(
                    "a"
                ),
                Var(
                    "b"
                )
            )
        )
    ]
)

Generated assembly:
  .globl _main
_main:
  push %ebp
  movl %esp, %ebp
  pushl $0
  pushl $0
  movl $4, %eax
  movl %eax, -8(%ebp)
  movl %eax, -4(%ebp)
  movl -8(%ebp), %eax
  push %eax
  movl -4(%ebp), %eax
  pop %ecx
  subl %ecx, %eax
_main_epilogue:
  movl %ebp, %esp
  pop %ebp
  ret

$ ./exp_return_val.c
$ echo $?
0
```
