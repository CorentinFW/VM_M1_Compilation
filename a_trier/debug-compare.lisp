;;;; Comparer VM directe vs via loader
(load "loader.lisp")

(format t "~%=== Via loader ===~%")
(let ((code (load-asm-string "PUSH 42
HALT")))
  (format t "Nombre d'instructions: ~A~%" (length code))
  (dotimes (i (length code))
    (let ((instr (aref code i)))
      (format t "  ~A: opcode=~A operand=~A  (~A)~%" 
              i 
              (instruction-opcode instr)
              (instruction-operand instr)
              (instruction-to-string instr))))
  
  (let ((vm (make-vm)))
    (vm-load-code vm code)
    (format t "Avant: pile=~A~%" (vm-stack vm))
    (let ((result (vm-run vm)))
      (format t "Après: pile=~A~%" (vm-stack vm))
      (format t "Résultat: ~A~%" result))))
