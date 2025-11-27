;;;; Test avec mode debug
(load "compiler.lisp")

(format t "~%=== Test avec mode debug ===~%")
(compile-and-run '(progn
                    (defun double (x) (* x 2))
                    (double 5))
                 :debug t)
