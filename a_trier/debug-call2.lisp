;;;; Debug du CALL - voir les instructions
(load "loader.lisp")

(let* ((asm "JUMP main
func:
  PUSH 42
  RET
main:
  PUSH 100
  PUSH 0
  CALL func
  HALT")
       (code (load-asm-string asm)))
  (format t "~%Instructions chargées:~%")
  (dotimes (i (length code))
    (format t "~A: ~A~%" i (instruction-to-string (aref code i)))))
