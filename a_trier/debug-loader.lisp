;;;; Debug du loader
(load "loader.lisp")

(format t "~%=== Test de parsing d'une chaîne ASM ===~%")
(let* ((asm "PUSH 5
PUSH 1
CALL FUNC_DOUBLE")
       (loader (make-loader))
       (lines (split-string asm #\Newline)))
  
  (format t "Lignes:~%")
  (dolist (line lines)
    (format t "  ~S~%" line))
  
  (format t "~%Parsing:~%")
  (dolist (line lines)
    (let ((parsed (parse-instruction-line line)))
      (format t "  ~S -> ~S~%" line parsed))))
