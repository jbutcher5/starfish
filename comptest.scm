(defun _start
  (define x
    4)
  (asm "mov rax, 60
mov rdi, 0
syscall"))
