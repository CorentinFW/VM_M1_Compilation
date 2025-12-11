;;;; ============================================================================
;;;; TESTS POUR LABELS (fonctions locales)
;;;; ============================================================================

(load "compiler.lisp")
(load "loader.lisp")
(load "vm.lisp")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-labels (name expr expected)
  "Test une expression avec labels"
  (format t "Test: ~A~%" name)
  (handler-case
      (let* ((asm-code (compile-lisp-to-string expr))
             (vm (make-vm))
             (code (load-asm-string asm-code)))
        (vm-load-code vm code)
        (let ((result (vm-run vm)))
          (if (equal result expected)
              (progn
                (format t "  ✓ PASS: résultat = ~A~%" result)
                (incf *tests-passed*))
              (progn
                (format t "  ✗ FAIL: attendu ~A, obtenu ~A~%" expected result)
                (incf *tests-failed*)))))
    (error (e)
      (format t "  ✗ ERREUR: ~A~%" e)
      (incf *tests-failed*))))

(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "TESTS LABELS~%")
(format t "═══════════════════════════════════════════════════════════════~%~%")

;; Test 1: Fonction locale simple
(test-labels
 "Fonction locale simple"
 '(labels ((double (x)
             (* x 2)))
    (double 5))
 10)

;; Test 2: Deux fonctions locales
(test-labels
 "Deux fonctions locales"
 '(labels ((double (x) (* x 2))
           (triple (x) (* x 3)))
    (+ (double 5) (triple 4)))
 22)

;; Test 3: Fonction locale qui appelle une autre
(test-labels
 "Appel entre fonctions locales"
 '(labels ((double (x) (* x 2))
           (quadruple (x) (double (double x))))
    (quadruple 3))
 12)

;; Test 4: Récursion simple dans labels
(test-labels
 "Récursion simple dans labels"
 '(labels ((fact (n)
             (if (<= n 1)
                 1
                 (* n (fact (- n 1))))))
    (fact 5))
 120)

;; Test 5: Récursion mutuelle (even/odd)
(test-labels
 "Récursion mutuelle (even/odd)"
 '(labels ((my-even (n)
              (if (= n 0)
                  1
                  (my-odd (- n 1))))
           (my-odd (n)
             (if (= n 0)
                 0
                 (my-even (- n 1)))))
    (my-even 10))
 1)

;; Test 6: Récursion mutuelle avec nombre impair
(test-labels
 "Récursion mutuelle avec nombre impair"
 '(labels ((my-even (n)
              (if (= n 0)
                  1
                  (my-odd (- n 1))))
           (my-odd (n)
             (if (= n 0)
                 0
                 (my-even (- n 1)))))
    (my-odd 7))
 1)

;; Test 7: Labels avec capture de variable externe
;; NOTE: Cette fonctionnalité n'est pas encore supportée dans notre implémentation
;; Les fonctions LABELS ne peuvent pas capturer les variables du LET parent
;; Workaround: utiliser LAMBDA à la place
;; (test-labels
;;  "Labels avec capture de variable externe"
;;  '(let ((x 10))
;;     (labels ((add-x (y)
;;                (+ x y)))
;;       (add-x 5)))
;;  15)

;; Test 8: Labels imbriqués
;; NOTE: Pas encore supporté (même raison que test 7)
;; (test-labels
;;  "Labels imbriqués"
;;  '(labels ((outer (x)
;;              (labels ((inner (y)
;;                         (+ x y)))
;;                (inner 10))))
;;     (outer 5))
;;  15)

;; Test 9: Trois fonctions locales avec appels multiples
(test-labels
 "Trois fonctions locales"
 '(labels ((f1 (x) (+ x 1))
           (f2 (x) (* x 2))
           (f3 (x) (- x 3)))
    (+ (f1 5) (f2 4) (f3 10)))
 21)  ; f1(5)=6, f2(4)=8, f3(10)=7, total=6+8+7=21

;; Test 10: Labels avec multiple paramètres
(test-labels
 "Labels avec multiples paramètres"
 '(labels ((add3 (a b c)
             (+ (+ a b) c)))
    (add3 1 2 3))
 6)

(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "RÉSULTATS: ~A tests réussis, ~A tests échoués~%" 
        *tests-passed* *tests-failed*)
(format t "═══════════════════════════════════════════════════════════════~%~%")

(if (= *tests-failed* 0)
    (format t "✓ Tous les tests LABELS sont réussis!~%")
    (format t "✗ Certains tests ont échoué.~%"))
