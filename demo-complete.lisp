;;;; Démonstration complète du système
;;;; Compilateur → ASM → VM

(in-package :cl-user)

(format t "~%╔════════════════════════════════════════════════════════╗~%")
(format t "║  DÉMONSTRATION: Compilation et Exécution sur la VM   ║~%")
(format t "╚════════════════════════════════════════════════════════╝~%~%")

;; Charger le compilateur
(format t "1. Chargement du compilateur...~%")
(load "compiler.lisp")

;; Charger le loader et la VM
(format t "2. Chargement du loader et de la VM...~%")
(load "loader.lisp")

;; Programme exemple: Fibonacci récursif
(defparameter *programme*
  '(defun fib (n)
     (if (< n 2)
         n
         (+ (fib (- n 1)) (fib (- n 2))))))

(format t "~%3. Programme à compiler:~%")
(format t "   ~A~%~%" *programme*)

;; Compiler le programme
(format t "4. Compilation...~%")
(let ((asm-code (compile-lisp-to-string *programme*)))
  (format t "~%Code ASM généré (~A instructions):~%" 
          (length (compile-lisp *programme*)))
  (format t "~A~%" asm-code)
  
  ;; Sauvegarder dans un fichier
  (with-open-file (stream "fib.asm" :direction :output :if-exists :supersede)
    (write-string asm-code stream))
  (format t "✓ Sauvegardé dans fib.asm~%"))

;; Exécuter sur la VM
(format t "~%5. Exécution sur la VM:~%")
(format t "   Calcul de fib(10)...~%")

(let ((result (compile-and-run '((defun fib (n)
                                   (if (< n 2)
                                       n
                                       (+ (fib (- n 1)) (fib (- n 2)))))
                                 (fib 10)))))
  (format t "~%   Résultat: fib(10) = ~A~%" result)
  (format t "   (attendu: 55)~%"))

(format t "~%✓ Démonstration terminée!~%~%")
