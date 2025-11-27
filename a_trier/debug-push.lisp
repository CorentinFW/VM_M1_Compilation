;;;; Test minimal sans PRINT
(load "loader.lisp")

(let ((asm "PUSH 5
PUSH 1
HALT"))
  (format t "~%=== Test PUSH simple ===~%")
  (load-and-run-asm-string asm))
