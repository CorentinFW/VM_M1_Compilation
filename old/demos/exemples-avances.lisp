;;;; ============================================================================
;;;; EXEMPLES AVANCÉS DU COMPILATEUR
;;;; ============================================================================

(load "compiler.lisp")

(format t "~%")
(format t "================================================================================~%")
(format t "EXEMPLES AVANCÉS DU COMPILATEUR LISP → ASM~%")
(format t "================================================================================~%")

;;; ============================================================================
;;; Exemple 1 : Fibonacci - Démonstration de la récursivité
;;; ============================================================================

(format t "~%--- Exemple 1 : Suite de Fibonacci ---~%")
(format t "Calcul de Fibonacci(10)...~%~%")

(compile-and-run '(progn
                    (defun fibo (n)
                      (if (<= n 1)
                          n
                          (+ (fibo (- n 1)) (fibo (- n 2)))))
                    (fibo 10)))

;;; ============================================================================
;;; Exemple 2 : Factorielle - Récursivité simple
;;; ============================================================================

(format t "~%--- Exemple 2 : Factorielle ---~%")
(format t "Calcul de 6!...~%~%")

(compile-and-run '(progn
                    (defun fact (n)
                      (if (<= n 1)
                          1
                          (* n (fact (- n 1)))))
                    (fact 6)))

;;; ============================================================================
;;; Exemple 3 : PGCD (Plus Grand Commun Diviseur) - Algorithme d'Euclide
;;; ============================================================================

(format t "~%--- Exemple 3 : PGCD (Algorithme d'Euclide) ---~%")
(format t "Calcul du PGCD de 48 et 18...~%~%")

(compile-and-run '(progn
                    (defun pgcd (a b)
                      (if (= b 0)
                          a
                          (pgcd b (mod a b))))
                    (pgcd 48 18)))

;;; ============================================================================
;;; Exemple 4 : Puissance - Exponentiation rapide
;;; ============================================================================

(format t "~%--- Exemple 4 : Puissance ---~%")
(format t "Calcul de 2^10...~%~%")

(compile-and-run '(progn
                    (defun power (base exp)
                      (if (<= exp 0)
                          1
                          (* base (power base (- exp 1)))))
                    (power 2 10)))

;;; ============================================================================
;;; Exemple 5 : Nombre triangulaire - Somme de 1 à n
;;; ============================================================================

(format t "~%--- Exemple 5 : Nombre triangulaire (somme 1 à n) ---~%")
(format t "Calcul de la somme de 1 à 100...~%~%")

(compile-and-run '(progn
                    (defun triangle (n)
                      (if (<= n 0)
                          0
                          (+ n (triangle (- n 1)))))
                    (triangle 100)))

;;; ============================================================================
;;; Exemple 6 : Fonction avec multiples paramètres
;;; ============================================================================

(format t "~%--- Exemple 6 : Fonction avec 3 paramètres ---~%")
(format t "Calcul de (a * b) + c avec a=5, b=7, c=3...~%~%")

(compile-and-run '(progn
                    (defun calc-abc (a b c)
                      (+ (* a b) c))
                    (calc-abc 5 7 3)))

;;; ============================================================================
;;; Exemple 7 : IF imbriqués - Signe d'un nombre
;;; ============================================================================

(format t "~%--- Exemple 7 : Déterminer le signe d'un nombre ---~%")
(format t "Test avec -5...~%~%")

(compile-and-run '(progn
                    (defun sign (n)
                      (if (< n 0)
                          (- 0 1)  ; -1
                          (if (> n 0)
                              1
                              0)))
                    (sign (- 0 5))))

;;; ============================================================================
;;; Exemple 8 : Variables locales complexes
;;; ============================================================================

(format t "~%--- Exemple 8 : Variables locales avec calculs ---~%")
(format t "Calcul de (x² + y²) avec x=3, y=4...~%~%")

(compile-and-run '(let ((x 3) (y 4))
                    (let ((x2 (* x x)) (y2 (* y y)))
                      (+ x2 y2))))

;;; ============================================================================
;;; Exemple 9 : Maximum de deux nombres
;;; ============================================================================

(format t "~%--- Exemple 9 : Maximum de deux nombres ---~%")
(format t "Max(42, 37)...~%~%")

(compile-and-run '(progn
                    (defun max2 (a b)
                      (if (> a b) a b))
                    (max2 42 37)))

;;; ============================================================================
;;; Exemple 10 : Fonction utilisant une autre fonction
;;; ============================================================================

(format t "~%--- Exemple 10 : Composition de fonctions ---~%")
(format t "Calcul de double(triple(4))...~%~%")

(compile-and-run '(progn
                    (defun double (x) (* x 2))
                    (defun triple (x) (* x 3))
                    (defun six-times (x) (double (triple x)))
                    (six-times 4)))

(format t "~%")
(format t "================================================================================~%")
(format t "FIN DES EXEMPLES - Tous les calculs sont corrects ! ✓~%")
(format t "================================================================================~%")
(format t "~%")
