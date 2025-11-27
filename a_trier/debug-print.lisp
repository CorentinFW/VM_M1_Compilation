;;;; Test simple avec PRINT
(load "compiler.lisp")
(load "loader.lisp")

(format t "~%=== Test avec instructions PRINT  pour debug ===~%")

(let ((asm "JUMP END_DEFUN_0
FUNC_DOUBLE:
LOADARG 0
PUSH 2
MUL
RET
END_DEFUN_0:
PUSH 5
PRINT
PUSH 1
PRINT
CALL FUNC_DOUBLE
HALT"))
  (load-and-run-asm-string asm))
