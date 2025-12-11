;;;; ============================================================================
;;;; TESTS POUR MINI-COMPILER
;;;; ============================================================================

(load "mini-compiler.lisp")
(load "compiler.lisp")
(load "loader.lisp")
(load "vm.lisp")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-mini-compiler (name expr expected)
  "Test le mini-compiler et compare le résultat avec le compiler natif"
  (format t "Test: ~A~%" name)
  (handler-case
      (let* ((mini-asm (mini-compile-lisp expr))
             (native-asm (compile-lisp-to-string expr))
             (mini-code (load-asm-string mini-asm))
             (native-code (load-asm-string native-asm))
             (mini-vm (make-vm))
             (native-vm (make-vm)))
        ;; Exécuter les deux
        (vm-load-code mini-vm mini-code)
        (vm-load-code native-vm native-code)
        (let ((mini-result (vm-run mini-vm))
              (native-result (vm-run native-vm)))
          (if (and (equal mini-result expected)
                   (equal native-result expected))
              (progn
                (format t "  ✓ PASS: résultat = ~A~%" mini-result)
                (incf *tests-passed*))
              (progn
                (format t "  ✗ FAIL~%")
                (format t "    Attendu: ~A~%" expected)
                (format t "    Mini:    ~A~%" mini-result)
                (format t "    Native:  ~A~%" native-result)
                (incf *tests-failed*)))))
    (error (e)
      (format t "  ✗ ERREUR: ~A~%" e)
      (incf *tests-failed*))))

(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "TESTS MINI-COMPILER~%")
(format t "═══════════════════════════════════════════════════════════════~%~%")

;; Test 1: Nombre simple
(test-mini-compiler
 "Constante"
 42
 42)

;; Test 2: Addition simple
(test-mini-compiler
 "Addition (+ 2 3)"
 '(+ 2 3)
 5)

;; Test 3: Soustraction
(test-mini-compiler
 "Soustraction (- 10 3)"
 '(- 10 3)
 7)

;; Test 4: Multiplication
(test-mini-compiler
 "Multiplication (* 4 5)"
 '(* 4 5)
 20)

;; Test 5: Division
(test-mini-compiler
 "Division (/ 20 4)"
 '(/ 20 4)
 5)

;; Test 6: Expression arithmétique complexe
(test-mini-compiler
 "Expression complexe (+ (* 2 3) (* 4 5))"
 '(+ (* 2 3) (* 4 5))
 26)

;; Test 7: Comparaison égalité
(test-mini-compiler
 "Égalité (= 5 5)"
 '(= 5 5)
 1)

;; Test 8: Comparaison inférieur
(test-mini-compiler
 "Inférieur (< 3 5)"
 '(< 3 5)
 1)

;; Test 9: IF simple vrai
(test-mini-compiler
 "IF vrai"
 '(if (< 5 10) 42 99)
 42)

;; Test 10: IF simple faux
(test-mini-compiler
 "IF faux"
 '(if (> 5 10) 42 99)
 99)

;; Test 11: LET avec une variable
(test-mini-compiler
 "LET simple"
 '(let ((x 10))
    x)
 10)

;; Test 12: LET avec calcul
(test-mini-compiler
 "LET avec calcul"
 '(let ((x 10)
        (y 20))
    (+ x y))
 30)

;; Test 13: DEFUN + appel simple
(test-mini-compiler
 "DEFUN + appel"
 '(progn
    (defun double (x)
      (* x 2))
    (double 5))
 10)

;; Test 14: Fonction récursive (factorielle)
(test-mini-compiler
 "Factorielle récursive"
 '(progn
    (defun fact (n)
      (if (<= n 1)
          1
          (* n (fact (- n 1)))))
    (fact 5))
 120)

;; Test 15: Fibonacci récursif
(test-mini-compiler
 "Fibonacci récursif"
 '(progn
    (defun fib (n)
      (if (<= n 1)
          n
          (+ (fib (- n 1))
             (fib (- n 2)))))
    (fib 7))
 13)

;; Test 16: Fonction avec plusieurs paramètres
(test-mini-compiler
 "Fonction avec 3 paramètres"
 '(progn
    (defun add3 (a b c)
      (+ (+ a b) c))
    (add3 1 2 3))
 6)

;; Test 17: LET imbriqué dans IF
(test-mini-compiler
 "LET dans IF"
 '(if (< 5 10)
      (let ((x 42))
        x)
      99)
 42)

;; Test 18: PROGN avec plusieurs expressions
(test-mini-compiler
 "PROGN"
 '(progn
    (+ 1 2)
    (+ 3 4)
    (+ 5 6))
 11)

;; Test 19: Fonction qui appelle une autre
(test-mini-compiler
 "Appel de fonction dans fonction"
 '(progn
    (defun double (x)
      (* x 2))
    (defun quadruple (x)
      (double (double x)))
    (quadruple 3))
 12)

;; Test 20: Expression complexe avec tout
(test-mini-compiler
 "Expression complexe complète"
 '(progn
    (defun max2 (a b)
      (if (> a b) a b))
    (let ((x 10)
          (y 20))
      (max2 x y)))
 20)

(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "RÉSULTATS: ~A tests réussis, ~A tests échoués~%" 
        *tests-passed* *tests-failed*)
(format t "═══════════════════════════════════════════════════════════════~%~%")

(if (= *tests-failed* 0)
    (format t "✓ Tous les tests MINI-COMPILER sont réussis!~%")
    (format t "✗ Certains tests ont échoué.~%"))
