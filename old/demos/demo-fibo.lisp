;;;; ============================================================================
;;;; DÉMONSTRATION COMPLÈTE : LISP → ASM → VM
;;;; ============================================================================
;;;; Ce script montre le processus complet :
;;;; 1. Code LISP → 2. Compilation vers ASM → 3. Exécution sur la VM
;;;; ============================================================================

(load "compiler.lisp")

(format t "~%")
(format t "################################################################################~%")
(format t "# DÉMONSTRATION : FIBONACCI DE LISP À L'EXÉCUTION~%")
(format t "################################################################################~%")

;;; Programme à compiler
(defparameter fibo-program
  '(progn
     (defun fibo (n)
       (if (<= n 1)
           n
           (+ (fibo (- n 1))
              (fibo (- n 2)))))
     (fibo 7)))

;;; ----------------------------------------------------------------------------
;;; Étape 1 : Code LISP source
;;; ----------------------------------------------------------------------------
(format t "~%~%=== ÉTAPE 1 : Code LISP source ===~%~%")
(format t "(defun fibo (n)~%")
(format t "  (if (<= n 1)~%")
(format t "      n~%")
(format t "      (+ (fibo (- n 1))~%")
(format t "         (fibo (- n 2)))))~%~%")
(format t "(fibo 7)~%")

;;; ----------------------------------------------------------------------------
;;; Étape 2 : Compilation vers ASM
;;; ----------------------------------------------------------------------------
(format t "~%~%=== ÉTAPE 2 : Compilation vers Assembleur ===~%~%")
(let ((asm-code (compile-lisp-to-string fibo-program)))
  (format t "~A~%" asm-code)
  
  ;; Sauvegarder dans un fichier
  (compile-lisp-to-file fibo-program "exemples/fibo-compiled.asm")
  (format t "→ Sauvegardé dans: exemples/fibo-compiled.asm~%"))

;;; ----------------------------------------------------------------------------
;;; Étape 3 : Exécution sur la VM
;;; ----------------------------------------------------------------------------
(format t "~%~%=== ÉTAPE 3 : Chargement et Exécution sur la VM ===~%~%")
(format t "Chargement du bytecode...~%")

(let ((result (compile-and-run fibo-program)))
  (format t "~%~%→ Résultat final : ~A ✓~%" result))

;;; ----------------------------------------------------------------------------
;;; Tests supplémentaires
;;; ----------------------------------------------------------------------------
(format t "~%~%=== TESTS SUPPLÉMENTAIRES ===~%~%")

(format t "Test 1 : fibo(10)...~%")
(compile-and-run '(progn
                    (defun fibo (n)
                      (if (<= n 1)
                          n
                          (+ (fibo (- n 1))
                             (fibo (- n 2)))))
                    (fibo 10)))

(format t "~%Test 2 : fibo(12)...~%")
(compile-and-run '(progn
                    (defun fibo (n)
                      (if (<= n 1)
                          n
                          (+ (fibo (- n 1))
                             (fibo (- n 2)))))
                    (fibo 12)))

(format t "~%~%################################################################################~%")
(format t "# DÉMONSTRATION TERMINÉE AVEC SUCCÈS ! ✓~%")
(format t "################################################################################~%")
(format t "~%")

(quit)
