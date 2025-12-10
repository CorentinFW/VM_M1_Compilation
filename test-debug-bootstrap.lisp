;;;; Test debugging bootstrap

(load "compiler.lisp")

(format t "~%=== TEST 1: vm-stack seul ===~%")
(compile-and-run '(progn
                    (defun vm-stack (vm) (car vm))
                    (vm-stack (cons 10 20))))
(format t "Attendu: 10~%~%")

(format t "=== TEST 2: vm-set-stack seul ===~%")
(compile-and-run '(progn
                    (defun vm-set-stack (vm s) (cons s (cdr vm)))
                    (vm-set-stack (cons 10 20) 99)))
(format t "Attendu: (99 . 20)~%~%")

(format t "=== TEST 3: vm-push SANS appel interne ===~%")
(compile-and-run '(progn
                    (defun vm-push (vm v) (cons (cons v (car vm)) (cdr vm)))
                    (vm-push (cons 10 20) 42)))
(format t "Attendu: ((42 . 10) . 20)~%~%")

(format t "=== TEST 4: vm-push AVEC vm-stack inline ===~%")
(compile-and-run '(progn
                    (defun vm-stack (vm) (car vm))
                    (defun vm-push (vm v) (cons (cons v (vm-stack vm)) (cdr vm)))
                    (vm-push (cons 10 20) 42)))
(format t "Attendu: ((42 . 10) . 20)~%~%")

(format t "=== TEST 5: vm-push complet AVEC vm-set-stack ===~%")
(compile-and-run '(progn
                    (defun vm-stack (vm) (car vm))
                    (defun vm-set-stack (vm s) (cons s (cdr vm)))
                    (defun vm-push (vm v) (vm-set-stack vm (cons v (vm-stack vm))))
                    (vm-push (cons 10 20) 42)))
(format t "Attendu: ((42 . 10) . 20)~%~%")
