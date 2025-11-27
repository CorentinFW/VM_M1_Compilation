;;;; Debug complet
(load "compiler.lisp")
(load "loader.lisp")

(format t "~%=== Code généré ===~%")
(let ((asm-code (compile-lisp-to-string '(progn
                                           (defun double (x) (* x 2))
                                           (double 5)))))
  (format t "~A~%" asm-code)
  
  (format t "~%=== Chargement dans le loader ===~%")
  (let* ((loader (make-loader))
         (lines (split-string asm-code #\Newline)))
    
    ; Première passe
    (first-pass loader lines)
    (format t "Labels trouvés:~%")
    (maphash (lambda (k v) (format t "  ~A -> ~A~%" k v)) 
             (loader-labels loader))
    
    ; Deuxième passe
    (second-pass loader lines)
    (format t "~%Instructions parsées:~%")
    (dolist (instr (loader-instructions loader))
      (format t "  ~A~%" instr))
    
    ; Troisième passe
    (resolve-labels loader)
    (format t "~%Instructions après résolution:~%")
    (dolist (instr (loader-instructions loader))
      (format t "  ~A~%" instr))))
