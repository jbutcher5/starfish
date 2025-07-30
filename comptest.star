(ccall putchar Char (Char))

(defun main ()
  (define var "Hello, World!")
  (define first-letter-of-var (deref *Char var))
  (putchar first-letter-of-var)
  0)