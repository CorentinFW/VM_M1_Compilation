;;;; Test: Fonctions locales avec labels

(labels ((helper (x)
           (if (<= x 0)
               1
               (* x (helper (- x 1))))))
  (helper 5))
