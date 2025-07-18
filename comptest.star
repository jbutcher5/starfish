(asm "extern exit")
(asm "extern putchar")

(defun add-num (x y)
  (asm "add esi, edi")
  (asm "mov eax, esi"))

(defun _start ()
  (define my-number (add-num 64 4))
  (putchar my-number)
  (exit 0))