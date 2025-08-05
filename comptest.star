(ccall putchar Char (Char))
(ccall puts Int (*Char))

(defun main ()
  (define var "Hello, World!")
  (define first-letter-of-var (deref *Char var))
  (define x 0)
  (if x (putchar first-letter-of-var) (if 1 (puts "Hello!!!") (puts "Hello")))
  0)