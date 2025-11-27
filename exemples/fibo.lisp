;;;; ============================================================================
;;;; FIBONACCI EN LISP - Définition de la fonction
;;;; ============================================================================
;;;; Ce fichier contient uniquement la définition de la fonction Fibonacci.
;;;; Pour les tests, voir les fichiers fibo-test*.lisp
;;;;
;;;; Usage:
;;;;   (load "compiler.lisp")
;;;;   (compile-and-run '(progn
;;;;                       (defun fibo (n)
;;;;                         (if (<= n 1)
;;;;                             n
;;;;                             (+ (fibo (- n 1))
;;;;                                (fibo (- n 2)))))
;;;;                       (fibo 7)))
;;;; ============================================================================

;;; Définition de la fonction fibonacci récursive
(defun fibo (n)
  "Calcule le n-ième nombre de Fibonacci récursivement.
   fibo(0) = 0, fibo(1) = 1
   fibo(n) = fibo(n-1) + fibo(n-2)"
  (if (<= n 1)
      n
      (+ (fibo (- n 1))
         (fibo (- n 2)))))
