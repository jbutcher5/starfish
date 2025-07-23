(asm "extern puts")

(defun main ()
  (define Ptr var "Hello, World!")
  (puts var)
  0)