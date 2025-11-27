;;;; Trace complète test 26
(load "compiler.lisp")
(load "loader.lisp")

(let* ((asm (compile-lisp-to-string '(progn
                                       (defun calc (x y) (+ (* x 2) (* y 3)))
                                       (calc 4 5))))
       (code (load-asm-string asm))
       (vm (make-vm)))
  
  (format t "Instructions:~%")
  (dotimes (i (length code))
    (format t "  ~A: ~A~%" i (instruction-to-string (aref code i))))
  
  (vm-load-code vm code)
  
  (format t "~%Exécution pas à pas:~%")
  (dotimes (step 30)
    (when (not (vm-halt vm))
      (let* ((pc (vm-pc vm))
             (instr (aref (vm-code vm) pc)))
        (format t "~%Step ~A - PC=~A: ~A~%" step pc (instruction-to-string instr))
        (format t "  Pile: ~A~%" (vm-stack vm))
        (format t "  Call-stack: ~A frames~%" (length (vm-call-stack vm)))
        
        (handler-case
            (progn
              (vm-execute-instruction vm instr)
              (format t "  → Pile: ~A~%" (vm-stack vm)))
          (error (e)
            (format t "  ERREUR: ~A~%" e)
            (setf (vm-halt vm) t)))))))
