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
All valid tests/stage_1 through tests/stage_8.

# Testing
```rs
$ cargo build
...
$ .\target\debug\oxc.exe .\test\stage_7\valid\consecutive_declarations.c
.\test\stage_7\valid\consecutive_declarations.c:
int main() {
    int a = 0;
    {
        int b = 1;
        a = b;
    }
    {
        int b = 2;
        a = a + b;
    }
    return a;
}

Lexically analyzed in 0.000175057s: option 'lex' to print tokens

Parsed in 0.0018207730000000001s:
Function(
    Function(
        "main",
        [
            Declaration(
                Declare(
                    "a",
                    Some(
                        Const(
                            0
                        )
                    )
                )
            ),
            Statement(
                Compound(
                    [
                        Declaration(
                            Declare(
                                "b",
                                Some(
                                    Const(
                                        1
                                    )
                                )
                            )
                        ),
                        Statement(
                            Expr(
                                Some(
                                    Assign(
                                        Assignment,
                                        "a",
                                        Var(
                                            "b"
                                        )
                                    )
                                )
                            )
                        )
                    ]
                )
            ),
            Statement(
                Compound(
                    [
                        Declaration(
                            Declare(
                                "b",
                                Some(
                                    Const(
                                        2
                                    )
                                )
                            )
                        ),
                        Statement(
                            Expr(
                                Some(
                                    Assign(
                                        Assignment,
                                        "a",
                                        BinOp(
                                            Plus,
                                            Var(
                                                "a"
                                            ),
                                            Var(
                                                "b"
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    ]
                )
            ),
            Statement(
                Return(
                    Var(
                        "a"
                    )
                )
            )
        ]
    )
)

Generated assembly in 0.097695794s:
  .globl _main
_main:
  push %ebp
  movl %esp, %ebp
  movl $0, %eax
  movl %eax, -4(%ebp)
  movl $1, %eax
  movl %eax, -8(%ebp)
  movl -8(%ebp), %eax
  movl %eax, -4(%ebp)
  addl $4, %esp
  movl $2, %eax
  movl %eax, -12(%ebp)
  movl -4(%ebp), %eax
  movl %eax, %ecx
  movl -12(%ebp), %eax
  addl %ecx, %eax
  movl %eax, -4(%ebp)
  addl $4, %esp
  movl -4(%ebp), %eax
  jmp _main_epilogue
  addl $4, %esp
  movl $0, %eax
_main_epilogue:
  movl %ebp, %esp
  pop %ebp
  ret

Assembling...
Assembled in 0.218045083s
```
