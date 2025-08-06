(ccall putchar Char (Char))
(ccall puts Int (*Char))
(ccall scanf Int (*Char *Int))
(ccall atoi Int (*Char))

(defun main ()
  (define var "Hello, World!")

  (define user-input "This is a test")

  (scanf "%d" (ref user-input))

  (define Int k (atoi user-input))
  
  (if k (puts "Wow") (puts "Oh"))

  0)