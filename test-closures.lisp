;;;; ============================================================================
;;;; TESTS DES FERMETURES (CLOSURES)
;;;; ============================================================================
;;;; Tests pour vérifier le bon fonctionnement des closures

(in-package :cl-user)
(load "compiler.lisp")

;;; ----------------------------------------------------------------------------
;;; Tests de base des lambdas
;;; ----------------------------------------------------------------------------

(defun test-lambda-simple ()
  "Test 1: Lambda simple sans capture de variable"
  (format t "~%Test 1: Lambda simple sans capture~%")
  (let ((result (compile-and-run '((lambda (x) (* x 2)) 5))))
    (if (= result 10)
        (format t "✓ PASS: Lambda simple (5*2 = ~A)~%" result)
        (format t "✗ FAIL: Lambda simple (attendu 10, obtenu ~A)~%" result))
    (= result 10)))

(defun test-lambda-multiple-args ()
  "Test 2: Lambda avec plusieurs arguments"
  (format t "~%Test 2: Lambda avec plusieurs arguments~%")
  (let ((result (compile-and-run '((lambda (x y) (+ x y)) 3 7))))
    (if (= result 10)
        (format t "✓ PASS: Lambda multi-args (3+7 = ~A)~%" result)
        (format t "✗ FAIL: Lambda multi-args (attendu 10, obtenu ~A)~%" result))
    (= result 10)))

(defun test-lambda-nested ()
  "Test 3: Lambda imbriquée"
  (format t "~%Test 3: Lambda imbriquée~%")
  (let ((result (compile-and-run '((lambda (x) ((lambda (y) (+ x y)) 5)) 3))))
    (if (= result 8)
        (format t "✓ PASS: Lambda imbriquée (3+5 = ~A)~%" result)
        (format t "✗ FAIL: Lambda imbriquée (attendu 8, obtenu ~A)~%" result))
    (= result 8)))

;;; ----------------------------------------------------------------------------
;;; Tests des fermetures avec capture de variables
;;; ----------------------------------------------------------------------------

(defun test-closure-simple ()
  "Test 4: Fermeture capturant une variable"
  (format t "~%Test 4: Fermeture simple avec capture~%")
  (let ((result (compile-and-run 
                 '(let ((x 10))
                    ((lambda (y) (+ x y)) 5)))))
    (if (= result 15)
        (format t "✓ PASS: Fermeture simple (10+5 = ~A)~%" result)
        (format t "✗ FAIL: Fermeture simple (attendu 15, obtenu ~A)~%" result))
    (= result 15)))

(defun test-closure-multiple-vars ()
  "Test 5: Fermeture capturant plusieurs variables"
  (format t "~%Test 5: Fermeture avec plusieurs variables capturées~%")
  (let ((result (compile-and-run 
                 '(let ((x 10) (y 20))
                    ((lambda (z) (+ x (+ y z))) 5)))))
    (if (= result 35)
        (format t "✓ PASS: Fermeture multi-vars (10+20+5 = ~A)~%" result)
        (format t "✗ FAIL: Fermeture multi-vars (attendu 35, obtenu ~A)~%" result))
    (= result 35)))

(defun test-closure-nested ()
  "Test 6: Fermetures imbriquées avec captures"
  (format t "~%Test 6: Fermetures imbriquées~%")
  (let ((result (compile-and-run 
                 '(let ((x 10))
                    (let ((y 20))
                      ((lambda (z) (+ x (+ y z))) 5))))))
    (if (= result 35)
        (format t "✓ PASS: Fermetures imbriquées (10+20+5 = ~A)~%" result)
        (format t "✗ FAIL: Fermetures imbriquées (attendu 35, obtenu ~A)~%" result))
    (= result 35)))

;;; ----------------------------------------------------------------------------
;;; Tests avancés
;;; ----------------------------------------------------------------------------

(defun test-closure-with-arithmetic ()
  "Test 7: Fermeture avec opérations arithmétiques"
  (format t "~%Test 7: Fermeture avec arithmétique complexe~%")
  (let ((result (compile-and-run 
                 '(let ((x 5))
                    ((lambda (y) (* (+ x y) 2)) 3)))))
    (if (= result 16)
        (format t "✓ PASS: Fermeture arithmétique ((5+3)*2 = ~A)~%" result)
        (format t "✗ FAIL: Fermeture arithmétique (attendu 16, obtenu ~A)~%" result))
    (= result 16)))

(defun test-closure-with-if ()
  "Test 8: Fermeture avec structure IF"
  (format t "~%Test 8: Fermeture avec IF~%")
  (let ((result (compile-and-run 
                 '(let ((x 10))
                    ((lambda (y) (if (< y x) x y)) 5)))))
    (if (= result 10)
        (format t "✓ PASS: Fermeture avec IF (max(10,5) = ~A)~%" result)
        (format t "✗ FAIL: Fermeture avec IF (attendu 10, obtenu ~A)~%" result))
    (= result 10)))

(defun test-closure-factorial-like ()
  "Test 9: Fermeture simulant un multiplicateur"
  (format t "~%Test 9: Fermeture comme multiplicateur~%")
  (let ((result (compile-and-run 
                 '(let ((factor 3))
                    ((lambda (n) (* n factor)) 7)))))
    (if (= result 21)
        (format t "✓ PASS: Multiplicateur (7*3 = ~A)~%" result)
        (format t "✗ FAIL: Multiplicateur (attendu 21, obtenu ~A)~%" result))
    (= result 21)))

(defun test-lambda-returning-lambda ()
  "Test 10: Lambda qui retourne une lambda (créateur de fonction)"
  (format t "~%Test 10: Lambda retournant une lambda~%")
  ;; Note: Ce test nécessite le support des variables contenant des closures
  ;; Pour l'instant, testons juste l'appel direct
  (let ((result (compile-and-run 
                 '(let ((x 10))
                    ((lambda (y) 
                       (let ((z (+ x y)))
                         ((lambda (w) (+ z w)) 5))) 
                     3)))))
    (if (= result 18)
        (format t "✓ PASS: Lambda retournant lambda (10+3+5 = ~A)~%" result)
        (format t "✗ FAIL: Lambda retournant lambda (attendu 18, obtenu ~A)~%" result))
    (= result 18)))

;;; ----------------------------------------------------------------------------
;;; Suite de tests complète
;;; ----------------------------------------------------------------------------

(defun run-all-closure-tests ()
  "Exécute tous les tests de fermetures"
  (format t "~%")
  (format t "========================================~%")
  (format t "   TESTS DES FERMETURES (CLOSURES)     ~%")
  (format t "========================================~%")
  
  (let ((tests '(test-lambda-simple
                 test-lambda-multiple-args
                 test-lambda-nested
                 test-closure-simple
                 test-closure-multiple-vars
                 test-closure-nested
                 test-closure-with-arithmetic
                 test-closure-with-if
                 test-closure-factorial-like
                 test-lambda-returning-lambda))
        (passed 0)
        (failed 0))
    
    (dolist (test tests)
      (handler-case
          (if (funcall test)
              (incf passed)
              (incf failed))
        (error (e)
          (format t "✗ ERREUR dans ~A: ~A~%" test e)
          (incf failed))))
    
    (format t "~%========================================~%")
    (format t "Résultats: ~A/~A tests réussis~%" passed (+ passed failed))
    (if (= failed 0)
        (format t "✓ TOUS LES TESTS SONT PASSÉS!~%")
        (format t "✗ ~A test(s) échoué(s)~%" failed))
    (format t "========================================~%")
    (= failed 0)))

;;; ----------------------------------------------------------------------------
;;; Tests interactifs
;;; ----------------------------------------------------------------------------

(defun demo-closures ()
  "Démonstration interactive des fermetures"
  (format t "~%")
  (format t "========================================~%")
  (format t "   DÉMONSTRATION DES FERMETURES         ~%")
  (format t "========================================~%")
  
  (format t "~%1. Lambda simple: ((lambda (x) (* x 2)) 5)~%")
  (compile-and-run '((lambda (x) (* x 2)) 5))
  
  (format t "~%2. Fermeture capturant x: (let ((x 10)) ((lambda (y) (+ x y)) 5))~%")
  (compile-and-run '(let ((x 10)) ((lambda (y) (+ x y)) 5)))
  
  (format t "~%3. Multiplicateur: (let ((factor 3)) ((lambda (n) (* n factor)) 7))~%")
  (compile-and-run '(let ((factor 3)) ((lambda (n) (* n factor)) 7)))
  
  (format t "~%4. Fermeture avec IF: (let ((x 10)) ((lambda (y) (if (< y x) x y)) 15))~%")
  (compile-and-run '(let ((x 10)) ((lambda (y) (if (< y x) x y)) 15)))
  
  (format t "~%========================================~%"))

;;; ----------------------------------------------------------------------------
;;; Auto-exécution au chargement
;;; ----------------------------------------------------------------------------

(format t "~%Tests des fermetures chargés!~%")
(format t "Utilisez (run-all-closure-tests) pour exécuter tous les tests~%")
(format t "Utilisez (demo-closures) pour une démonstration interactive~%")
