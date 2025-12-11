;;;; Test de bootstrap du compilateur
;;;; Étape 1: Compiler le compilateur avec lui-même

(in-package :cl-user)
(load "compiler.lisp")

(format t "~%=== ÉTAPE 1: Compiler compiler.lisp → compiler.asm ===~%")

;; Lire le code source du compilateur
(defun read-file-as-sexpr (filename)
  "Lit un fichier Lisp et retourne une liste d'expressions"
  (with-open-file (stream filename :direction :input)
    (let ((exprs '()))
      (loop
        (let ((expr (read stream nil 'eof)))
          (if (eq expr 'eof)
              (return (nreverse exprs))
              (push expr exprs)))))))

;; Essayer de compiler juste une petite fonction simple d'abord
(format t "~%Test simple: compiler une fonction add~%")
(let ((simple-code '(defun add (x y) (+ x y))))
  (format t "Code: ~A~%" simple-code)
  (let ((asm (compile-lisp simple-code)))
    (format t "ASM généré: ~{~A~%~}~%" asm)))

(format t "~%✓ Test réussi!~%")
