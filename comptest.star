(ccall putchar Char (Char))

(defun main Int ()
  (define Ptr var "Hello, World!")
  (define Char first (deref Char var))
  (putchar first)
  0)