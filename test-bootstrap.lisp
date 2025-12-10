;;;; ============================================================================
;;;; TESTS DES PRIMITIVES POUR LE BOOTSTRAP
;;;; ============================================================================
;;;; Tests des listes, symboles et autres primitives nécessaires au bootstrap

(in-package :cl-user)
(load "compiler.lisp")

(format t "~%========================================~%")
(format t "   TESTS DES PRIMITIVES BOOTSTRAP       ~%")
(format t "========================================~%")

;;; ----------------------------------------------------------------------------
;;; Tests des listes
;;; ----------------------------------------------------------------------------

(format t "~%=== Tests des listes ===~%")

(format t "~%Test 1: CONS~%")
(format t "Code: (cons 1 2)~%")
(compile-and-run '(cons 1 2))

(format t "~%Test 2: CAR~%")
(format t "Code: (car (cons 1 2))~%")
(compile-and-run '(car (cons 1 2)))

(format t "~%Test 3: CDR~%")
(format t "Code: (cdr (cons 1 2))~%")
(compile-and-run '(cdr (cons 1 2)))

(format t "~%Test 4: LIST~%")
(format t "Code: (list 1 2 3)~%")
(compile-and-run '(list 1 2 3))

(format t "~%Test 5: CAR de LIST~%")
(format t "Code: (car (list 10 20 30))~%")
(compile-and-run '(car (list 10 20 30)))

(format t "~%Test 6: CDR de LIST~%")
(format t "Code: (cdr (list 10 20 30))~%")
(compile-and-run '(cdr (list 10 20 30)))

(format t "~%Test 7: CAR CDR imbriqués~%")
(format t "Code: (car (cdr (list 10 20 30)))~%")
(compile-and-run '(car (cdr (list 10 20 30))))

;;; ----------------------------------------------------------------------------
;;; Tests des prédicats
;;; ----------------------------------------------------------------------------

(format t "~%=== Tests des prédicats ===~%")

(format t "~%Test 8: NULL? avec NIL~%")
(format t "Code: (null 0)~%")
(compile-and-run '(null 0))

(format t "~%Test 9: NULL? avec non-NIL~%")
(format t "Code: (if (null 5) 0 1)~%")
(compile-and-run '(if (null 5) 0 1))

(format t "~%Test 10: LISTP~%")
(format t "Code: (if (listp (list 1 2)) 1 0)~%")
(compile-and-run '(if (listp (list 1 2)) 1 0))

;;; ----------------------------------------------------------------------------
;;; Tests des fonctions avec listes
;;; ----------------------------------------------------------------------------

(format t "~%=== Tests des fonctions avec listes ===~%")

(format t "~%Test 11: Fonction retournant une liste~%")
(format t "Code: (progn (defun make-pair (a b) (cons a b)) (make-pair 5 10))~%")
(compile-and-run '(progn 
                    (defun make-pair (a b) (cons a b))
                    (make-pair 5 10)))

(format t "~%Test 12: Fonction first~%")
(format t "Code: (progn (defun first (lst) (car lst)) (first (list 7 8 9)))~%")
(compile-and-run '(progn 
                    (defun first (lst) (car lst))
                    (first (list 7 8 9))))

(format t "~%Test 13: Fonction second~%")
(format t "Code: (progn (defun second (lst) (car (cdr lst))) (second (list 7 8 9)))~%")
(compile-and-run '(progn 
                    (defun second (lst) (car (cdr lst)))
                    (second (list 7 8 9))))

(format t "~%Test 14: Construction de liste récursive~%")
(format t "Code: (progn 
         (defun range (n) 
           (if (<= n 0) 
               0 
               (cons n (range (- n 1)))))
         (range 5))~%")
(compile-and-run '(progn 
                    (defun range (n) 
                      (if (<= n 0) 
                          0 
                          (cons n (range (- n 1)))))
                    (range 5)))

(format t "~%Test 15: Longueur de liste~%")
(format t "~Code: (progn 
         (defun length-list (lst) 
           (if (null lst) 
               0 
               (+ 1 (length-list (cdr lst)))))
         (length-list (list 1 2 3 4 5)))~%")
(compile-and-run '(progn 
                    (defun length-list (lst) 
                      (if (null lst) 
                          0 
                          (+ 1 (length-list (cdr lst)))))
                    (length-list (list 1 2 3 4 5))))

(format t "~%========================================~%")
(format t "   FIN DES TESTS                        ~%")
(format t "========================================~%")
