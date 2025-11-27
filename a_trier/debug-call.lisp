;;;; Test du CALL
(load "loader.lisp")

(let ((asm "JUMP main
func:
  PUSH 42
  RET
main:
  PUSH 100
  PUSH 0
  CALL func
  HALT"))
  (format t "~%=== Test CALL avec 0 arguments ===~%")
  (load-and-run-asm-string asm))
