;;;; ============================================================================
;;;; EXEMPLES DE FERMETURES (CLOSURES)
;;;; ============================================================================
;;;; Exemples pratiques d'utilisation des closures

(load "compiler.lisp")

(format t "~%========================================~%")
(format t "   EXEMPLES DE FERMETURES               ~%")
(format t "========================================~%")

;;; ----------------------------------------------------------------------------
;;; Exemple 1 : Fonction multiplicatrice
;;; ----------------------------------------------------------------------------

(format t "~%=== Exemple 1 : Multiplicateur ===~%")
(format t "Code: (let ((factor 5)) ((lambda (n) (* n factor)) 7))~%")
(format t "Description: Crée une fermeture qui capture 'factor' et multiplie par 5~%")
(compile-and-run '(let ((factor 5)) 
                    ((lambda (n) (* n factor)) 7)))

;;; ----------------------------------------------------------------------------
;;; Exemple 2 : Fonction d'addition partielle
;;; ----------------------------------------------------------------------------

(format t "~%=== Exemple 2 : Additionneur ===~%")
(format t "Code: (let ((base 100)) ((lambda (x) (+ base x)) 23))~%")
(format t "Description: Crée une fermeture qui ajoute 100 à son argument~%")
(compile-and-run '(let ((base 100)) 
                    ((lambda (x) (+ base x)) 23)))

;;; ----------------------------------------------------------------------------
;;; Exemple 3 : Fonction avec plusieurs variables capturées
;;; ----------------------------------------------------------------------------

(format t "~%=== Exemple 3 : Calcul complexe ===~%")
(format t "Code: (let ((a 10) (b 20) (c 5)) ((lambda (x) (+ (* a x) (- b c))) 3))~%")
(format t "Description: Capture 3 variables et fait : (10*3) + (20-5)~%")
(compile-and-run '(let ((a 10) (b 20) (c 5)) 
                    ((lambda (x) (+ (* a x) (- b c))) 3)))

;;; ----------------------------------------------------------------------------
;;; Exemple 4 : Fonction avec condition
;;; ----------------------------------------------------------------------------

(format t "~%=== Exemple 4 : Maximum avec fermeture ===~%")
(format t "Code: (let ((threshold 50)) ((lambda (x) (if (> x threshold) x threshold)) 30))~%")
(format t "Description: Retourne le maximum entre x et le seuil capturé~%")
(compile-and-run '(let ((threshold 50)) 
                    ((lambda (x) (if (> x threshold) x threshold)) 30)))

(format t "~%Essayons avec un nombre plus grand:~%")
(compile-and-run '(let ((threshold 50)) 
                    ((lambda (x) (if (> x threshold) x threshold)) 75)))

;;; ----------------------------------------------------------------------------
;;; Exemple 5 : Lambda imbriquée avec capture
;;; ----------------------------------------------------------------------------

(format t "~%=== Exemple 5 : Lambdas imbriquées ===~%")
(format t "Code: ((lambda (x) ((lambda (y) (+ x y)) 5)) 10)~%")
(format t "Description: Lambda interne capture x de la lambda externe~%")
(compile-and-run '((lambda (x) ((lambda (y) (+ x y)) 5)) 10))

;;; ----------------------------------------------------------------------------
;;; Exemple 6 : Fermeture dans DEFUN
;;; ----------------------------------------------------------------------------

(format t "~%=== Exemple 6 : Fonction retournant une fermeture ===~%")
(format t "Code: (progn~%")
(format t "        (defun make-adder (n)~%")
(format t "          (lambda (x) (+ n x)))~%")
(format t "        ((make-adder 10) 5))~%")
(format t "Description: DEFUN qui retourne une lambda capturant n~%")
;; Note: Cet exemple nécessiterait le support des fonctions retournant des closures
;; Pour l'instant, montrons un exemple équivalent avec let
(format t "(Version simplifiée avec let)~%")
(compile-and-run '(let ((n 10))
                    ((lambda (x) (+ n x)) 5)))

;;; ----------------------------------------------------------------------------
;;; Exemple 7 : Compteur simple
;;; ----------------------------------------------------------------------------

(format t "~%=== Exemple 7 : Pattern du compteur ===~%")
(format t "Code: (let ((count 0)) ((lambda (inc) (+ count inc)) 5))~%")
(format t "Description: Simule un compteur qui capture son état~%")
(compile-and-run '(let ((count 0)) 
                    ((lambda (inc) (+ count inc)) 5)))

;;; ----------------------------------------------------------------------------
;;; Exemple 8 : Conversion de température
;;; ----------------------------------------------------------------------------

(format t "~%=== Exemple 8 : Convertisseur de température ===~%")
(format t "Code: (let ((offset 32)) ((lambda (celsius) (+ (* celsius 2) offset)) 20))~%")
(format t "Description: Conversion Celsius vers Fahrenheit (approximation)~%")
(format t "Formule: F = C*2 + 32~%")
(compile-and-run '(let ((offset 32)) 
                    ((lambda (celsius) (+ (* celsius 2) offset)) 20)))

;;; ----------------------------------------------------------------------------
;;; Exemple 9 : Calculateur de remise
;;; ----------------------------------------------------------------------------

(format t "~%=== Exemple 9 : Calculateur de remise ===~%")
(format t "Code: (let ((discount 10)) ((lambda (price) (- price discount)) 100))~%")
(format t "Description: Applique une remise fixe capturée~%")
(compile-and-run '(let ((discount 10)) 
                    ((lambda (price) (- price discount)) 100)))

;;; ----------------------------------------------------------------------------
;;; Exemple 10 : Vérificateur de plage
;;; ----------------------------------------------------------------------------

(format t "~%=== Exemple 10 : Vérificateur de plage ===~%")
(format t "Code: (let ((min 10) (max 100)) ~%")
(format t "        ((lambda (x) (if (< x min) 0 (if (> x max) 0 1))) 50))~%")
(format t "Description: Vérifie si x est dans [min, max], retourne 1 si oui, 0 sinon~%")
(compile-and-run '(let ((min 10) (max 100)) 
                    ((lambda (x) (if (< x min) 0 (if (> x max) 0 1))) 50)))

(format t "~%Testons avec une valeur hors plage:~%")
(compile-and-run '(let ((min 10) (max 100)) 
                    ((lambda (x) (if (< x min) 0 (if (> x max) 0 1))) 150)))

;;; ----------------------------------------------------------------------------
;;; Exemple 11 : Calcul de puissance
;;; ----------------------------------------------------------------------------

(format t "~%=== Exemple 11 : Élévation au carré avec offset ===~%")
(format t "Code: (let ((offset 5)) ((lambda (x) (+ (* x x) offset)) 3))~%")
(format t "Description: Calcule x² + offset~%")
(compile-and-run '(let ((offset 5)) 
                    ((lambda (x) (+ (* x x) offset)) 3)))

;;; ----------------------------------------------------------------------------
;;; Exemple 12 : Fonction d'échelle
;;; ----------------------------------------------------------------------------

(format t "~%=== Exemple 12 : Mise à l'échelle ===~%")
(format t "Code: (let ((scale 10) (offset 50)) ~%")
(format t "        ((lambda (x) (+ (* x scale) offset)) 3))~%")
(format t "Description: Applique une transformation linéaire: y = scale*x + offset~%")
(compile-and-run '(let ((scale 10) (offset 50)) 
                    ((lambda (x) (+ (* x scale) offset)) 3)))

(format t "~%========================================~%")
(format t "   FIN DES EXEMPLES                     ~%")
(format t "========================================~%")
