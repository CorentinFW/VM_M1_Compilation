;;;; Test Fibonacci avec n=12
;;;; Résultat attendu: 144

(load "compiler.lisp")

(format t "~%=== Test Fibonacci(6) ===~%")
(compile-and-run '(progn
                    (defun fibo (n)
                      (if (<= n 1)
                          n
                          (+ (fibo (- n 1))
                             (fibo (- n 2)))))
                    (fibo 6)))

(quit)
