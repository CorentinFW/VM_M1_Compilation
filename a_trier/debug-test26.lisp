;;;; Debug test 26
(load "compiler.lisp")

(format t "~%=== Test 26 : (+ (* x 2) (* y 3)) avec x=4, y=5 ===~%")
(format t "Attendu: (+ (* 4 2) (* 5 3)) = (+ 8 15) = 23~%~%")

(let ((asm (compile-lisp-to-string '(progn
                                      (defun calc (x y) (+ (* x 2) (* y 3)))
                                      (calc 4 5)))))
  (format t "Code ASM généré:~%~A~%" asm))

(format t "~%Exécution:~%")
(compile-and-run '(progn
                    (defun calc (x y) (+ (* x 2) (* y 3)))
                    (calc 4 5)))
