;;;; Test simple pour debugger
(load "compiler.lisp")

(format t "~%=== Test simple fonction ===~%")
(format t "Code ASM généré:~%~%")

(let ((code (compile-lisp '(progn
                             (defun double (x) (* x 2))
                             (double 5)))))
  (dolist (instr code)
    (format t "~A~%" instr)))
