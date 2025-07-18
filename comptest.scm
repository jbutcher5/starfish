(asm "extern exit")
(asm "extern putchar")

(defun add (x y)
  (asm "add edi, esi")
  (asm "mov eax, edi"))

(defun _start ()
  (define my-number (add 64 4))
  (putchar my-number)
  (exit 0))

