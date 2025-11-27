;;;; Test complet step by step
(load "compiler.lisp")
(load "loader.lisp")

(format t "~%=== Génération du code ASM ===~%")
(let ((asm-code (compile-lisp-to-string '(progn
                                           (defun double (x) (* x 2))
                                           (double 5)))))
  (format t "~A~%" asm-code)
  
  (format t "~%=== Chargement via loader ===~%")
  (let ((code (load-asm-string asm-code)))
    (format t "Nombre d'instructions: ~A~%~%" (length code))
    
    (format t "Instructions:~%")
    (dotimes (i (length code))
      (format t "  ~A: ~A~%" i (instruction-to-string (aref code i))))
    
    (format t "~%=== Exécution ===~%")
    (let ((vm (make-vm)))
      (vm-load-code vm code)
      (format t "PC initial: ~A~%" (vm-pc vm))
      (format t "Pile initiale: ~A~%~%" (vm-stack vm))
      
      ; Exécuter instruction par instruction manuellement
      (dotimes (step 20)
        (when (not (vm-halt vm))
          (let* ((pc (vm-pc vm))
                 (instr (aref (vm-code vm) pc)))
            (format t "Step ~A - PC=~A: ~A~%" step pc (instruction-to-string instr))
            (format t "  Pile avant: ~A~%" (vm-stack vm))
            
            (handler-case
                (progn
                  (vm-execute-instruction vm instr)
                  (format t "  Pile après: ~A~%" (vm-stack vm))
                  (format t "  Nouveau PC: ~A~%~%" (vm-pc vm)))
              (error (e)
                (format t "  ERREUR: ~A~%~%" e)
                (setf (vm-halt vm) t)))))))))
