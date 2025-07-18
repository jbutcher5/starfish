# Starfish

A compiled Lisp designed to natively interoperate with C. This means there is a mature standard library out of the box along with a complementing, more functional, internal library to supplement libc.

## Features (18th July 2025)
- [x] Primitive Variables
- [x] Function Calling (upto 6 parameters)
- [ ] Strings
- [ ] Tail Recursion Elimination
- [ ] Metacircular Interpreter

## Syntax
```scheme
(asm "extern exit")
(asm "extern putchar")

(defun add-num (x y)
  (asm "add esi, edi")
  (asm "mov eax, esi"))

(defun _start ()
  (define my-number (add-num 64 4))
  (putchar my-number)
  (exit 0))
```
