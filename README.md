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
$ .\target\debug\oxc.exe .\test\stage_4\valid\precedence_2.c
.\test\stage_4\valid\precedence_2.c:
int main() {
    return (1 || 0) && 0;
}

Scanner production:
[Keyword(Int), Id("main"), Symbol(LParen), Symbol(RParen), Symbol(LBrace), Keyword(Return), Symbol(LParen), Integer(1), Operator(Or), Integer(0), Symbol(RParen), Operator(And), Integer(0), Symbol(Semicolon), Symbol(RBrace)]

Abstract syntax tree:
Func(
    "main",
    Return(
        BinOp(
            And,
            BinOp(
                Or,
                Const(
                    1
                ),
                Const(
                    0
                )
            ),
            Const(
                0
            )
        )
    )
)

Generated assembly:
  .globl _main
_main:
  movl $1, %eax
  push %eax
  movl $0, %eax
  pop %ecx
  orl %ecx, %eax
  movl $0, %eax
  setne %al
  push %eax
  movl $0, %eax
  pop %ecx
  cmpl $0, %eax
  setne %cl
  cmpl $0, %eax
  movl $0, %eax
  setne %al
  andb %cl, %al
  ret

$ ./precedence_2
$ echo $?
2
```
