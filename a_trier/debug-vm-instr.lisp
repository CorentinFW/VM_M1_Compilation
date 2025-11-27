;;;; Test de conversion en instructions VM
(load "compiler.lisp")
(load "loader.lisp")

(let* ((asm-code (compile-lisp-to-string '(progn
                                            (defun double (x) (* x 2))
                                            (double 5))))
       (code (load-asm-string asm-code)))
  
  (format t "~%Instructions VM:~%")
  (dotimes (i (length code))
    (let ((instr (aref code i)))
      (format t "~A: ~A~%" i (instruction-to-string instr)))))
