;;;; Tests de la VM - Version simplifiée
;;;; Tests les fonctionnalités de base de la VM

(load "vm.lisp")
(load "loader.lisp")

(defvar *test-count* 0)
(defvar *test-passed* 0)

(defun test-vm (name asm-code expected)
  "Teste l'exécution d'un code ASM"
  (incf *test-count*)
  (handler-case
      (let* ((code (load-asm-string asm-code))
             (vm (make-vm :code code))
             (result (vm-run vm)))
        (if (equal result expected)
            (progn
              (incf *test-passed*)
              (format t "✓ ~A~%" name))
            (format t "✗ ~A (attendu: ~A, obtenu: ~A)~%" name expected result)))
    (error (e)
      (format t "✗ ~A (erreur: ~A)~%" name e))))

(defun run-all-vm-tests ()
  (format t "~%═══════════════════════════════════════════════════════════════~%")
  (format t "TESTS DE LA MACHINE VIRTUELLE~%")
  (format t "═══════════════════════════════════════════════════════════════~%~%")
  
  (setf *test-count* 0)
  (setf *test-passed* 0)
  
  ;; Test 1: Addition simple
  (test-vm "Addition 2+3"
           "PUSH 2
            PUSH 3
            ADD
            HALT"
           5)
  
  ;; Test 2: Multiplication
  (test-vm "Multiplication 4*5"
           "PUSH 4
            PUSH 5
            MUL
            HALT"
           20)
  
  ;; Test 3: Soustraction
  (test-vm "Soustraction 10-3"
           "PUSH 10
            PUSH 3
            SUB
            HALT"
           7)
  
  ;; Test 4: Division
  (test-vm "Division 20/4"
           "PUSH 20
            PUSH 4
            DIV
            HALT"
           5)
  
  ;; Test 5: Expression composée
  (test-vm "Expression (2+3)*4"
           "PUSH 2
            PUSH 3
            ADD
            PUSH 4
            MUL
            HALT"
           20)
  
  ;; Test 6: Comparaison égalité
  (test-vm "Égalité 5=5"
           "PUSH 5
            PUSH 5
            EQ
            HALT"
           1)
  
  ;; Test 7: Comparaison inégalité
  (test-vm "Inégalité 5=3"
           "PUSH 5
            PUSH 3
            EQ
            HALT"
           0)
  
  ;; Test 8: Comparaison inférieur
  (test-vm "Inférieur 3<5"
           "PUSH 3
            PUSH 5
            LT
            HALT"
           1)
  
  ;; Test 9: Saut conditionnel
  (test-vm "IF 5>3 then 100 else 200"
           "PUSH 5
            PUSH 3
            GT
            JUMPNIF ELSE
            PUSH 100
            JUMP END
            ELSE:
            PUSH 200
            END:
            HALT"
           100)
  
  ;; Test 10: Variables locales
  (test-vm "Variables LET x=10, y=20"
           "ALLOC 2
            PUSH 10
            STORE 0
            PUSH 20
            STORE 1
            LOAD 0
            LOAD 1
            ADD
            DEALLOC 2
            HALT"
           30)
  
  ;; Test 11: Fonction simple
  (test-vm "Fonction double(5)"
           "JUMP MAIN
            DOUBLE:
            LOADARG 0
            PUSH 2
            MUL
            RET
            MAIN:
            PUSH 5
            PUSH 1
            CALL DOUBLE
            HALT"
           10)
  
  (format t "~%═══════════════════════════════════════════════════════════════~%")
  (format t "RÉSULTAT: ~A/~A tests réussis~%" *test-passed* *test-count*)
  (format t "═══════════════════════════════════════════════════════════════~%~%")
  
  (= *test-passed* *test-count*))
