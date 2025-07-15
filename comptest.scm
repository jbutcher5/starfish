(asm "extern exit")

(defun _start
  (define x
    4)
  (asm "mov rdi, 4")
  (exit))
