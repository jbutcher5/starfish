(ccall putchar Char (Char))

(defun main ()
  (define var "Hello, World!")
  (define first (deref Char var))
  (putchar first)
  0)