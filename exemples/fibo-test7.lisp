;;;; Test Fibonacci avec n=7
;;;; Résultat attendu: 13

(load "compiler.lisp")

(format t "~%=== Test Fibonacci(7) ===~%")
(compile-and-run '(progn
                    (defun fibo (n)
                      (if (<= n 1)
                          n
                          (+ (fibo (- n 1))
                             (fibo (- n 2)))))
                    (fibo 7)))

(quit)
