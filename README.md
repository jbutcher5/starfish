# Starfish

A compiled Lisp designed to natively interoperate with C. This means there is a mature standard library out of the box along with a complementing, more functional, internal library to supplement libc.

I hope to implement a metacircular compiler to extend the programming language further. The aim of this would be to remove typing and make lists easy to manipulate.

## Syntax
```scheme
(ccall putchar Char (Char))

(defun main ()
  (define var "Hello, World!")
  (define first-letter-of-var (deref *Char var))
  (putchar first-letter-of-var)
  0)
```
