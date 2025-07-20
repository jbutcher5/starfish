(asm "extern exit")
(asm "extern putchar")

(defun addi (x y)
  (asm "add esi, edi")
  (asm "mov eax, esi"))

(defun _start ()
  (define my-number (addi 64 4))
  (define my-pointer (ref my-number))
  (define _ 48)
  (define my-data (deref my-pointer))
  (putchar my-data)
  (exit 0))