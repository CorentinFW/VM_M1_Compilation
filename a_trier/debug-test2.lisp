;;;; Test d'exécution simple
(load "compiler.lisp")

(format t "~%=== Test exécution fonction simple ===~%")
(compile-and-run '(progn
                    (defun double (x) (* x 2))
                    (double 5)))
