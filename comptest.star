(asm "extern exit")
(asm "extern putchar")

(defun addi (x y)
  (asm "add esi, edi")
  (asm "mov eax, esi"))

(defun _start ()
  (define Int my-number (addi 64 4))
  (define Ptr my-pointer (ref my-number))
  (define Int my-data (deref my-pointer))
  (putchar my-data)
  (exit 0))