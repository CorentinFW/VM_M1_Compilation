;;;; Fichier principal pour orchestrer compilation et exécution
;;;; Charge tous les modules et fournit une interface simplifiée

(load "compiler.lisp")
(load "loader.lisp")
(load "vm.lisp")

(defpackage :main
  (:use :common-lisp :lisp-compiler :asm-loader :vm)
  (:export :compile-and-run
           :compile-and-run-with-call
           :run-tests
           :example-usage))

(in-package :main)

;;; Fonction principale pour compiler et exécuter un fichier Lisp
(defun compile-and-run (lisp-file &key (debug nil) (debug-vm nil))
  "Compile un fichier Lisp et l'exécute dans la VM"
  (format t "~%=== Compilation de ~A ===~%" lisp-file)
  
  ;; Activer le mode debug si demandé
  (setf lisp-compiler:*debug-mode* debug)
  (setf vm:*debug-vm* debug-vm)
  
  ;; Lire le fichier Lisp
  (with-open-file (in lisp-file :direction :input)
    (let ((expr (read in)))
      
      ;; Compiler l'expression
      (format t "Expression à compiler: ~A~%" expr)
      (let ((asm-code (lisp-compiler:compile-to-asm expr)))
        
        ;; Afficher le code ASM généré
        (format t "~%Code ASM généré:~%")
        (lisp-compiler:print-asm asm-code)
        
        ;; Charger le code dans la VM
        (format t "~%=== Chargement dans la VM ===~%")
        (multiple-value-bind (program labels)
            (asm-loader:load-asm-instructions asm-code)
          
          ;; Créer et configurer la VM
          (let ((machine (vm:make-vm)))
            (vm:vm-load-program machine program labels)
            
            ;; Exécuter le programme
            (format t "~%=== Exécution ===~%")
            (let ((result (vm:vm-run machine)))
              (format t "~%Résultat: ~A~%" result)
              result)))))))

;;; Fonction pour compiler et exécuter deux fichiers (fonction + appel)
(defun compile-and-run-with-call (function-file call-file &key (debug nil) (debug-vm nil))
  "Compile une fonction et son appel, puis exécute dans la VM"
  (format t "~%=== Compilation de ~A et ~A ===~%" function-file call-file)
  
  ;; Activer le mode debug si demandé
  (setf lisp-compiler:*debug-mode* debug)
  (setf vm:*debug-vm* debug-vm)
  
  ;; Lire et compiler la fonction
  (with-open-file (in function-file :direction :input)
    (let* ((fn-expr (read in))
           (fn-asm (lisp-compiler:compile-to-asm fn-expr)))
      
      ;; Lire et compiler l'appel
      (with-open-file (in2 call-file :direction :input)
        (let* ((call-expr (read in2))
               (call-asm (lisp-compiler:compile-to-asm call-expr)))
          
          ;; Combiner les deux programmes
          ;; On enlève le HALT de la fonction et on retire aussi le PUSH NIL final
          (let ((combined-asm (append (butlast fn-asm 2) call-asm)))
            
            ;; Afficher le code ASM généré
            (format t "~%Code ASM généré:~%")
            (lisp-compiler:print-asm combined-asm)
            
            ;; Charger et exécuter
            (format t "~%=== Chargement dans la VM ===~%")
            (multiple-value-bind (program labels)
                (asm-loader:load-asm-instructions combined-asm)
              
              (let ((machine (vm:make-vm)))
                (vm:vm-load-program machine program labels)
                
                ;; Exécuter le programme
                (format t "~%=== Exécution ===~%")
                (let ((result (vm:vm-run machine)))
                  (format t "~%Résultat: ~A~%" result)
                  result)))))))))

;;; Fonction de test interactive
(defun run-tests ()
  "Exécute une série de tests"
  (format t "~%========================================~%")
  (format t "Tests du compilateur Lisp vers ASM + VM~%")
  (format t "========================================~%")
  
  ;; Test 1: Let simple
  (format t "~%~%TEST 1: Variables locales (let)~%")
  (compile-and-run "test/let-test.lisp")
  
  ;; Test 2: If
  (format t "~%~%TEST 2: Condition if~%")
  (compile-and-run "test/if-test.lisp")
  
  ;; Test 3: Labels
  (format t "~%~%TEST 3: Fonctions locales (labels)~%")
  (compile-and-run "test/labels-test.lisp")
  
  ;; Test 4: Fibonacci
  (format t "~%~%TEST 4: Fibonacci (fonction + appel)~%")
  (compile-and-run-with-call "test/fibo.lisp" "test/call-fibo.lisp")
  
  (format t "~%~%========================================~%")
  (format t "Tests terminés!~%")
  (format t "========================================~%"))

;;; Exemple d'utilisation simple
(defun example-usage ()
  "Montre comment utiliser le système"
  (format t "~%Exemples d'utilisation:~%~%")
  (format t "1. Compiler et exécuter un fichier:~%")
  (format t "   (compile-and-run \"test/let-test.lisp\")~%~%")
  (format t "2. Compiler une fonction et son appel:~%")
  (format t "   (compile-and-run-with-call \"test/fibo.lisp\" \"test/call-fibo.lisp\")~%~%")
  (format t "3. Exécuter tous les tests:~%")
  (format t "   (run-tests)~%~%")
  (format t "4. Mode debug:~%")
  (format t "   (compile-and-run \"test/fibo.lisp\" :debug t :debug-vm t)~%~%"))

;; Afficher les instructions au démarrage
(format t "~%Système de compilation Lisp vers ASM chargé!~%")
(example-usage)
