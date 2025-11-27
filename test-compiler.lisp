;;;; ============================================================================
;;;; TESTS DU COMPILATEUR LISP → ASM
;;;; ============================================================================

(in-package :cl-user)
(load "compiler.lisp")
(load "loader.lisp")

;;; ----------------------------------------------------------------------------
;;; Utilitaires de test
;;; ----------------------------------------------------------------------------

(defvar *test-count* 0)
(defvar *test-passed* 0)

(defun test-compile (name expr expected-result &key debug)
  "Test une compilation et exécution"
  (incf *test-count*)
  (format t "~%Test ~A: ~A~%" *test-count* name)
  (format t "  Expression: ~A~%" expr)
  
  (handler-case
      (let* ((asm-code (compile-lisp-to-string expr))
             (vm (make-vm :debug debug))
             (code (load-asm-string asm-code)))
        (when debug
          (format t "  Code ASM:~%~A~%" asm-code))
        (vm-load-code vm code)
        (let ((result (vm-run vm)))
          (if (equal result expected-result)
              (progn
                (incf *test-passed*)
                (format t "  ✓ RÉUSSI - Résultat: ~A~%" result))
              (format t "  ✗ ÉCHEC - Attendu: ~A, Obtenu: ~A~%" 
                      expected-result result))))
    (error (e)
      (format t "  ✗ ERREUR: ~A~%" e))))

(defun run-compiler-tests ()
  "Lance tous les tests du compilateur"
  (setf *test-count* 0)
  (setf *test-passed* 0)
  
  (format t "~%")
  (format t "================================================================================~%")
  (format t "TESTS DU COMPILATEUR LISP → ASM~%")
  (format t "================================================================================~%")
  
  ;; =========================================================================
  ;; PHASE 1 : Constantes et arithmétique simple
  ;; =========================================================================
  (format t "~%--- PHASE 1 : Constantes et Arithmétique ---~%")
  
  (test-compile "Constante simple" 
                42 
                42)
  
  (test-compile "Addition simple" 
                '(+ 2 3) 
                5)
  
  (test-compile "Soustraction" 
                '(- 10 3) 
                7)
  
  (test-compile "Multiplication" 
                '(* 4 5) 
                20)
  
  (test-compile "Division" 
                '(/ 20 4) 
                5)
  
  (test-compile "Addition multiple" 
                '(+ 1 2 3 4) 
                10)
  
  (test-compile "Expression complexe" 
                '(+ (* 2 3) (* 4 5)) 
                26)
  
  (test-compile "Expression imbriquée" 
                '(* (+ 2 3) (- 10 4)) 
                30)
  
  ;; =========================================================================
  ;; PHASE 2 : Comparaisons
  ;; =========================================================================
  (format t "~%--- PHASE 2 : Comparaisons ---~%")
  
  (test-compile "Égalité vraie" 
                '(= 5 5) 
                1)
  
  (test-compile "Égalité fausse" 
                '(= 5 3) 
                0)
  
  (test-compile "Inférieur vrai" 
                '(< 3 5) 
                1)
  
  (test-compile "Inférieur faux" 
                '(< 5 3) 
                0)
  
  (test-compile "Inférieur ou égal" 
                '(<= 5 5) 
                1)
  
  (test-compile "Supérieur" 
                '(> 10 5) 
                1)
  
  ;; =========================================================================
  ;; PHASE 3 : IF (structures conditionnelles)
  ;; =========================================================================
  (format t "~%--- PHASE 3 : Structures IF ---~%")
  
  (test-compile "IF simple - branche THEN" 
                '(if (< 2 3) 10 20) 
                10)
  
  (test-compile "IF simple - branche ELSE" 
                '(if (> 2 3) 10 20) 
                20)
  
  (test-compile "IF avec calculs" 
                '(if (= (+ 2 2) 4) 
                     (* 3 3) 
                     (+ 5 5)) 
                9)
  
  (test-compile "IF imbriqué" 
                '(if (< 5 10) 
                     (if (> 5 3) 100 200) 
                     300) 
                100)
  
  ;; =========================================================================
  ;; PHASE 4 : LET (variables locales)
  ;; =========================================================================
  (format t "~%--- PHASE 4 : Variables LET ---~%")
  
  (test-compile "LET avec une variable" 
                '(let ((x 5)) 
                   x) 
                5)
  
  (test-compile "LET avec calcul" 
                '(let ((x 5)) 
                   (* x 2)) 
                10)
  
  (test-compile "LET avec deux variables" 
                '(let ((x 3) (y 4)) 
                   (+ x y)) 
                7)
  
  (test-compile "LET avec expression complexe" 
                '(let ((x (+ 2 3)) (y (* 4 5))) 
                   (+ x y)) 
                25)
  
  (test-compile "LET imbriqué" 
                '(let ((x 10)) 
                   (let ((y 5)) 
                     (+ x y))) 
                15)
  
  ;; =========================================================================
  ;; PHASE 5 : DEFUN (fonctions simples)
  ;; =========================================================================
  (format t "~%--- PHASE 5 : Fonctions DEFUN ---~%")
  
  (test-compile "Fonction simple - double" 
                '(progn
                   (defun double (x) (* x 2))
                   (double 5))
                10)
  
  (test-compile "Fonction - addition" 
                '(progn
                   (defun add-nums (a b) (+ a b))
                   (add-nums 7 8))
                15)
  
  (test-compile "Fonction - calcul complexe" 
                '(progn
                   (defun calc (x y) 
                     (+ (* x 2) (* y 3)))
                   (calc 4 5))
                23)
  
  (test-compile "Fonction avec IF" 
                '(progn
                   (defun max-num (a b) 
                     (if (> a b) a b))
                   (max-num 10 7))
                10)
  
  (test-compile "Appels multiples" 
                '(progn
                   (defun triple (x) (* x 3))
                   (+ (triple 2) (triple 3)))
                15)
  
  ;; =========================================================================
  ;; PHASE 6 : Récursivité simple
  ;; =========================================================================
  (format t "~%--- PHASE 6 : Récursivité ---~%")
  
  (test-compile "Factorielle" 
                '(progn
                   (defun fact (n)
                     (if (<= n 1)
                         1
                         (* n (fact (- n 1)))))
                   (fact 5))
                120)
  
  (test-compile "Fibonacci" 
                '(progn
                   (defun fibo (n)
                     (if (<= n 1)
                         n
                         (+ (fibo (- n 1)) 
                            (fibo (- n 2)))))
                   (fibo 7))
                13)
  
  (test-compile "Somme récursive" 
                '(progn
                   (defun sum-to (n)
                     (if (<= n 0)
                         0
                         (+ n (sum-to (- n 1)))))
                   (sum-to 10))
                55)
  
  ;; =========================================================================
  ;; Résumé des tests
  ;; =========================================================================
  (format t "~%")
  (format t "================================================================================~%")
  (format t "RÉSUMÉ : ~A/~A tests réussis~%" *test-passed* *test-count*)
  (format t "================================================================================~%")
  (format t "~%"))

;;; ----------------------------------------------------------------------------
;;; Exemples interactifs
;;; ----------------------------------------------------------------------------

(defun example-compile-arithmetic ()
  "Exemple : compilation d'expressions arithmétiques"
  (format t "~%=== Exemple : Expressions arithmétiques ===~%")
  (compile-and-run '(+ (* 2 3) (* 4 5))))

(defun example-compile-if ()
  "Exemple : compilation d'un IF"
  (format t "~%=== Exemple : Structure IF ===~%")
  (compile-and-run '(if (< 5 10) 100 200)))

(defun example-compile-let ()
  "Exemple : compilation d'un LET"
  (format t "~%=== Exemple : LET avec variables ===~%")
  (compile-and-run '(let ((x 10) (y 20)) (+ x y))))

(defun example-compile-function ()
  "Exemple : compilation d'une fonction"
  (format t "~%=== Exemple : Fonction simple ===~%")
  (compile-and-run '(progn
                      (defun square (x) (* x x))
                      (square 7))))

(defun example-compile-factorial ()
  "Exemple : compilation de factorielle"
  (format t "~%=== Exemple : Factorielle récursive ===~%")
  (compile-and-run '(progn
                      (defun fact (n)
                        (if (<= n 1)
                            1
                            (* n (fact (- n 1)))))
                      (fact 6))))

(defun example-compile-fibonacci ()
  "Exemple : compilation de fibonacci"
  (format t "~%=== Exemple : Fibonacci récursif ===~%")
  (compile-and-run '(progn
                      (defun fibo (n)
                        (if (<= n 1)
                            n
                            (+ (fibo (- n 1)) (fibo (- n 2)))))
                      (fibo 8))))

;;; ----------------------------------------------------------------------------
;;; Messages de chargement
;;; ----------------------------------------------------------------------------

(format t "~%Tests du compilateur chargés avec succès!~%")
(format t "Utilisez (run-compiler-tests) pour lancer tous les tests~%")
(format t "~%Exemples disponibles:~%")
(format t "  (example-compile-arithmetic)~%")
(format t "  (example-compile-if)~%")
(format t "  (example-compile-let)~%")
(format t "  (example-compile-function)~%")
(format t "  (example-compile-factorial)~%")
(format t "  (example-compile-fibonacci)~%")
