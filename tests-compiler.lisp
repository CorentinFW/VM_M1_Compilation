;;;; Tests du Compilateur - Version simplifiée
;;;; Tests des fonctionnalités du compilateur LISP -> ASM

(load "compiler.lisp")

(defvar *test-count* 0)
(defvar *test-passed* 0)

(defun test-compile (name code expected)
  "Teste la compilation et l'exécution d'un code LISP"
  (incf *test-count*)
  (handler-case
      (let ((result (compile-and-run code)))
        (if (equal result expected)
            (progn
              (incf *test-passed*)
              (format t "✓ ~A~%" name))
            (format t "✗ ~A (attendu: ~A, obtenu: ~A)~%" name expected result)))
    (error (e)
      (format t "✗ ~A (erreur: ~A)~%" name e))))

(defun run-compiler-tests ()
  (format t "~%═══════════════════════════════════════════════════════════════~%")
  (format t "TESTS DU COMPILATEUR~%")
  (format t "═══════════════════════════════════════════════════════════════~%~%")
  
  (setf *test-count* 0)
  (setf *test-passed* 0)
  
  ;; ========== Phase 1: Arithmétique ==========
  (format t "Phase 1: Arithmétique~%")
  (test-compile "Addition" '(+ 2 3) 5)
  (test-compile "Soustraction" '(- 10 3) 7)
  (test-compile "Multiplication" '(* 4 5) 20)
  (test-compile "Division" '(/ 20 4) 5)
  (test-compile "Expression composée" '(+ (* 2 3) 4) 10)
  (test-compile "Expression complexe" '(* (+ 2 3) (- 10 6)) 20)
  (test-compile "Expression imbriquée" '(+ (* (+ 1 2) 3) (* 2 4)) 17)
  (test-compile "Modulo" '(mod 17 5) 2)
  (format t "~%")
  
  ;; ========== Phase 2: Comparaisons ==========
  (format t "Phase 2: Comparaisons~%")
  (test-compile "Égalité vrai" '(= 5 5) 1)
  (test-compile "Égalité faux" '(= 5 3) 0)
  (test-compile "Inférieur vrai" '(< 3 5) 1)
  (test-compile "Inférieur faux" '(< 5 3) 0)
  (test-compile "Inférieur ou égal" '(<= 5 5) 1)
  (test-compile "Supérieur" '(> 10 3) 1)
  (format t "~%")
  
  ;; ========== Phase 3: IF ==========
  (format t "Phase 3: Structures IF~%")
  (test-compile "IF simple vrai" '(if (> 5 3) 100 200) 100)
  (test-compile "IF simple faux" '(if (< 5 3) 100 200) 200)
  (test-compile "IF avec calculs" '(if (= (* 2 3) 6) (+ 10 5) (- 10 5)) 15)
  (test-compile "IF imbriqués" '(if (> 10 5) (if (< 3 5) 1 2) 3) 1)
  (format t "~%")
  
  ;; ========== Phase 4: LET ==========
  (format t "Phase 4: Variables LET~%")
  (test-compile "LET simple" '(let ((x 10)) x) 10)
  (test-compile "LET avec calcul" '(let ((x 5)) (* x 2)) 10)
  (test-compile "LET deux variables" '(let ((x 10) (y 20)) (+ x y)) 30)
  (test-compile "LET imbriqués" '(let ((x 5)) (let ((y 10)) (+ x y))) 15)
  (test-compile "SETQ modification" '(let ((x 10)) (setq x 20) x) 20)
  (format t "~%")
  
  ;; ========== Phase 5: PROGN ==========
  (format t "Phase 5: PROGN~%")
  (test-compile "PROGN simple" '(progn (+ 1 2) (+ 3 4)) 7)
  (test-compile "PROGN avec LET" '(progn (let ((x 5)) (* x 2)) (+ 10 5)) 15)
  (format t "~%")
  
  ;; ========== Phase 6: DEFUN ==========
  (format t "Phase 6: Fonctions DEFUN~%")
  (test-compile "Fonction simple" 
                '(progn (defun double (x) (* x 2)) (double 7))
                14)
  (test-compile "Fonction deux args" 
                '(progn (defun add (x y) (+ x y)) (add 3 5))
                8)
  (test-compile "Fonction avec IF" 
                '(progn (defun max2 (a b) (if (> a b) a b)) (max2 10 5))
                10)
  (test-compile "Deux fonctions" 
                '(progn 
                   (defun double (x) (* x 2))
                   (defun triple (x) (* x 3))
                   (+ (double 4) (triple 3)))
                17)
  (format t "~%")
  
  ;; ========== Phase 7: Récursion ==========
  (format t "Phase 7: Récursion~%")
  (test-compile "Factorielle(5)" 
                '(progn 
                   (defun fact (n) 
                     (if (<= n 1) 
                         1 
                         (* n (fact (- n 1)))))
                   (fact 5))
                120)
  (test-compile "Fibonacci(10)" 
                '(progn 
                   (defun fibo (n)
                     (if (< n 2)
                         n
                         (+ (fibo (- n 1)) (fibo (- n 2)))))
                   (fibo 10))
                55)
  (test-compile "Somme récursive" 
                '(progn 
                   (defun sum (n)
                     (if (<= n 0)
                         0
                         (+ n (sum (- n 1)))))
                   (sum 10))
                55)
  (format t "~%")
  
  (format t "═══════════════════════════════════════════════════════════════~%")
  (format t "RÉSULTAT: ~A/~A tests réussis~%" *test-passed* *test-count*)
  (format t "═══════════════════════════════════════════════════════════════~%~%")
  
  (= *test-passed* *test-count*))
