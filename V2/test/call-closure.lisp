;;;; Test: Appel d'une fermeture
;;;; Appelle la fermeture créée avec l'argument 5

(funcall (let ((x 10))
           (lambda (y)
             (+ x y)))
         5)
