(asm "extern putchar")

(defun main ()
  (define Ptr var "Hello, World!")
  (define Int first (deref Char var))
  (putchar first)
  0)