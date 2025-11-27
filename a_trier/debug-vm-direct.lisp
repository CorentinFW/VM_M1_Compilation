;;;; Test ultra simple - juste PUSH
(load "vm.lisp")

(let ((vm (make-vm))
      (code (vector (make-instruction-from-mnemonic 'PUSH 42)
                    (make-instruction-from-mnemonic 'HALT))))
  (vm-load-code vm code)
  (format t "Avant exécution, pile: ~A~%" (vm-stack vm))
  (let ((result (vm-run vm)))
    (format t "Après exécution, pile: ~A~%" (vm-stack vm))
    (format t "Résultat: ~A~%" result)))
