# A Programming language

Making a programming language with a few backends.

The parser is a recursive descent parser, with an s-expression printer
to verify that the parsing was properly done.

There's a bytecode interpreter,
An x86_64 linux assembly interpreter,
And an AST traversing interpreter.

```sh
$ cargo r -- asm
$ 1 + 2 * 3
  .text
.LC0:
  .string "%d\n"
  .globl main
main:
  mov $3, %rax
  push %rax
  mov $2, %rax
  pop %rdi
  imul %rdi, %rax
  push %rax
  mov $1, %rax
  pop %rdi
  add %rdi, %rax
  push %rsp
  mov %rax, %rsi
  mov $.LC0, %rdi
  xor %rax, %rax
  call printf
  xor %rax, %rax
  pop %rsp
  ret
```

```sh
$ cargo r -- ast
$ 1 + 2 * 3
Output: (+ 1 (* 2 3))
```

```sh
$ cargo r -- bc
$ 1 + 2 * 3
Output: 7
```

```sh
$ cargo r
$ 1 + 2 * 3
Output: 7
```
