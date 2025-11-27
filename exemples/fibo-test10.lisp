;;;; Test Fibonacci avec n=10
;;;; Résultat attendu: 55

(load "compiler.lisp")

(format t "~%=== Test Fibonacci(10) ===~%")
(compile-and-run '(progn
                    (defun fibo (n)
                      (if (<= n 1)
                          n
                          (+ (fibo (- n 1))
                             (fibo (- n 2)))))
                    (fibo 10)))

(quit)
