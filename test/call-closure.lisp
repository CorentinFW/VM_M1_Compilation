;;;; Test: Fermeture simple
;;;; La lambda capture x de let

(funcall (let ((x 10))
           (lambda (y)
             (+ x y)))
         5)
