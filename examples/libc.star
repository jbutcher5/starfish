(asm "extern exit")
(asm "extern putchar")

(defun _start ()
  (define my-char 68)
  (putchar my-char)
  (exit 0))