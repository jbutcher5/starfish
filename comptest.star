(ccall putchar Char (Char))
(ccall puts Int (*Char))
(ccall scanf Int (*Char *Int))
(ccall atoi Int (*Char))

(defun main Int ()
  (define Int k 0)
  (scanf "%d" (ref k))

  (if k (puts "Wow") (puts "Oh"))

  0)