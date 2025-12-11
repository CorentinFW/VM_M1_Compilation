;;;; ============================================================================
;;;; TESTS POUR MINI-LOADER
;;;; ============================================================================

(load "mini-loader.lisp")
(load "loader.lisp")  ; Pour comparer avec le loader natif

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-mini-loader (name asm-code)
  "Test le mini-loader et compare avec le loader natif"
  (format t "Test: ~A~%" name)
  (handler-case
      (let* ((mini-bytecode (mini-load-asm-string asm-code))
             (native-code (load-asm-string asm-code))
             (native-bytecode (flatten-instructions native-code)))
        (if (equal mini-bytecode native-bytecode)
            (progn
              (format t "  ✓ PASS: bytecode identique~%")
              (format t "    → ~A~%" mini-bytecode)
              (incf *tests-passed*))
            (progn
              (format t "  ✗ FAIL: bytecode différent~%")
              (format t "    Mini:   ~A~%" mini-bytecode)
              (format t "    Native: ~A~%" native-bytecode)
              (incf *tests-failed*))))
    (error (e)
      (format t "  ✗ ERREUR: ~A~%" e)
      (incf *tests-failed*))))

(defun flatten-instructions (code-vector)
  "Convertit un vecteur d'instructions en liste plate d'entiers"
  (let ((result '()))
    (loop for i from 0 below (length code-vector) do
      (let ((instr (aref code-vector i)))
        (push (instruction-opcode instr) result)
        (when (instruction-operand instr)
          (push (instruction-operand instr) result))))
    (reverse result)))

(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "TESTS MINI-LOADER~%")
(format t "═══════════════════════════════════════════════════════════════~%~%")

;; Test 1: Programme simple sans labels
(test-mini-loader
 "Programme simple (PUSH, ADD, HALT)"
 "PUSH 10
PUSH 20
ADD
HALT")

;; Test 2: Programme avec soustraction
(test-mini-loader
 "Soustraction"
 "PUSH 30
PUSH 10
SUB
HALT")

;; Test 3: Programme avec multiplication
(test-mini-loader
 "Multiplication"
 "PUSH 5
PUSH 6
MUL
HALT")

;; Test 4: Programme avec labels et JUMP
(test-mini-loader
 "Programme avec JUMP"
 "JUMP END
PUSH 99
END:
PUSH 42
HALT")

;; Test 5: Programme avec CALL et RET
(test-mini-loader
 "Programme avec CALL/RET"
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
HALT")

;; Test 6: Programme avec variables locales
(test-mini-loader
 "Variables locales"
 "ALLOC 2
PUSH 10
STORE 0
PUSH 20
STORE 1
LOAD 0
LOAD 1
ADD
DEALLOC 2
HALT")

;; Test 7: Programme avec comparaisons
(test-mini-loader
 "Comparaisons"
 "PUSH 5
PUSH 10
LT
HALT")

;; Test 8: Programme avec JUMPIF
(test-mini-loader
 "Branchement conditionnel"
 "PUSH 1
JUMPIF TRUE_BRANCH
PUSH 0
JUMP END
TRUE_BRANCH:
PUSH 42
END:
HALT")

;; Test 9: Fibonacci itératif
(test-mini-loader
 "Programme plus complexe (fibonacci)"
 "JUMP MAIN
FIB:
LOADARG 0
PUSH 1
LE
JUMPIF BASE_CASE
LOADARG 0
PUSH 1
SUB
PUSH 1
CALL FIB
LOADARG 0
PUSH 2
SUB
PUSH 1
CALL FIB
ADD
RET
BASE_CASE:
LOADARG 0
RET
MAIN:
PUSH 5
PUSH 1
CALL FIB
HALT")

;; Test 10: Programme avec closures
(test-mini-loader
 "Programme avec MKCLOSURE"
 "JUMP MAIN
LAMBDA_0:
LOADARG 0
LOADCLOSURE 0
ADD
RET
MAIN:
ALLOC 1
PUSH 10
STORE 0
LOAD 0
PUSH 1
MKCLOSURE LAMBDA_0
DEALLOC 1
HALT")

(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "RÉSULTATS: ~A tests réussis, ~A tests échoués~%" 
        *tests-passed* *tests-failed*)
(format t "═══════════════════════════════════════════════════════════════~%~%")

(if (= *tests-failed* 0)
    (format t "✓ Tous les tests MINI-LOADER sont réussis!~%")
    (format t "✗ Certains tests ont échoué.~%"))
