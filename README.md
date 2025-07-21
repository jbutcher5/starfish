# Starfish

A compiled Lisp designed to natively interoperate with C. This means there is a mature standard library out of the box along with a complementing, more functional, internal library to supplement libc.

I hope to implement a metacircular evaluator to simplify the syntax and give it a more dynamic Lisp feel. The aim of this would be to remove typing and make lists easy to manipulate

## Syntax
```scheme
(asm "extern exit")
(asm "extern putchar")

(defun add-num (x y)
  (asm "add esi, edi")
  (asm "mov eax, esi"))

(defun _start ()
  (define Int my-number (add-num 64 4))
  (putchar my-number)
  (exit 0))
```
