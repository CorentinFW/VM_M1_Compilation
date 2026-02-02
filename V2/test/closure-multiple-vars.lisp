;;;; Test: Fermeture avec plusieurs variables capturées
;;;; Capture à la fois x et y

(funcall (let ((x 10)
               (y 20))
           (lambda (z)
             (+ x (+ y z))))
         5)
