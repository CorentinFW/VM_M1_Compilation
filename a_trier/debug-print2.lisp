;;;; Test avec PRINT mais pas de dépilage après
(load "loader.lisp")

(let ((asm "PUSH 5
PRINT
PUSH 1
PRINT
HALT"))
  (format t "~%=== Test PUSH + PRINT ===~%")
  (let ((result (load-and-run-asm-string asm)))
    (format t "Pile finale devrait contenir 5 et 1, sommet = ~A~%" result)))
