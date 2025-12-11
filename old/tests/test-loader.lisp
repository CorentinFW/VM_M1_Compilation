;;;; ============================================================================
;;;; TESTS DU LOADER
;;;; ============================================================================

(in-package :cl-user)
(load "loader.lisp")

;;; ----------------------------------------------------------------------------
;;; Utilitaires de test
;;; ----------------------------------------------------------------------------

(defvar *test-count* 0)
(defvar *test-passed* 0)
(defvar *test-failed* 0)

(defun reset-test-counters ()
  (setf *test-count* 0)
  (setf *test-passed* 0)
  (setf *test-failed* 0))

(defun assert-equal (name expected actual)
  (incf *test-count*)
  (if (equal expected actual)
      (progn
        (incf *test-passed*)
        (format t "✓ ~A~%" name))
      (progn
        (incf *test-failed*)
        (format t "✗ ~A~%  Attendu: ~A~%  Obtenu:  ~A~%" name expected actual))))

(defun print-test-summary ()
  (format t "~%========================================~%")
  (format t "Tests: ~A | Réussis: ~A | Échoués: ~A~%" 
          *test-count* *test-passed* *test-failed*)
  (format t "========================================~%"))

;;; ----------------------------------------------------------------------------
;;; Création de fichiers ASM de test
;;; ----------------------------------------------------------------------------

(defun create-test-file-simple ()
  "Crée un fichier ASM simple : 2 + 3"
  (save-asm-file "test_simple.asm"
                 '("PUSH 2"
                   "PUSH 3"
                   "ADD"
                   "HALT")))

(defun create-test-file-complex ()
  "Crée un fichier ASM : (10 + 5) * 2 - 3"
  (save-asm-file "test_complex.asm"
                 '("PUSH 10"
                   "PUSH 5"
                   "ADD"
                   "PUSH 2"
                   "MUL"
                   "PUSH 3"
                   "SUB"
                   "HALT")))

(defun create-test-file-with-labels ()
  "Crée un fichier ASM avec des labels et sauts"
  (save-asm-file "test_labels.asm"
                 '("; Test avec labels et sauts"
                   "PUSH 5"
                   "PUSH 3"
                   "LT          ; 5 < 3 ? => 0"
                   "JUMPIF then_branch"
                   "PUSH 100    ; else branch"
                   "JUMP end"
                   "then_branch:"
                   "PUSH 200"
                   "end:"
                   "HALT")))

(defun create-test-file-loop ()
  "Crée un fichier ASM avec une boucle : somme de 1 à 5"
  (save-asm-file "test_loop.asm"
                 '("; Calcul de 1+2+3+4+5 = 15"
                   "PUSH 0      ; somme"
                   "PUSH 1      ; compteur"
                   "loop_start:"
                   "DUP         ; dupliquer compteur"
                   "PUSH 6"
                   "LT          ; compteur < 6 ?"
                   "JUMPNIF loop_end"
                   "; Corps de la boucle"
                   "DUP         ; dupliquer compteur"
                   "PUSH 2      ; position de la somme dans la pile"
                   "LOAD 0      ; charger somme (on utilise une variable)"
                   "ADD"
                   "STORE 0     ; sauvegarder somme"
                   "PUSH 1"
                   "ADD         ; incrémenter compteur"
                   "JUMP loop_start"
                   "loop_end:"
                   "POP         ; enlever compteur"
                   "LOAD 0      ; charger résultat"
                   "HALT")))

(defun create-test-file-comments ()
  "Crée un fichier avec plein de commentaires"
  (save-asm-file "test_comments.asm"
                 '("; Fichier de test avec commentaires"
                   "; Ce programme calcule 7 * 8"
                   ""
                   "PUSH 7      ; premier nombre"
                   "PUSH 8      ; deuxième nombre"
                   ""
                   "; Multiplication"
                   "MUL         ; 7 * 8 = 56"
                   ""
                   "HALT        ; fin du programme")))

;;; ----------------------------------------------------------------------------
;;; Tests du loader
;;; ----------------------------------------------------------------------------

(defun test-load-simple ()
  "Test chargement fichier simple"
  (format t "~%--- Test Chargement Simple ---~%")
  (create-test-file-simple)
  (let ((result (load-and-run-asm-file "test_simple.asm")))
    (assert-equal "2 + 3 = 5" 5 result)))

(defun test-load-complex ()
  "Test chargement expression complexe"
  (format t "~%--- Test Chargement Complexe ---~%")
  (create-test-file-complex)
  (let ((result (load-and-run-asm-file "test_complex.asm")))
    (assert-equal "(10 + 5) * 2 - 3 = 27" 27 result)))

(defun test-load-with-labels ()
  "Test chargement avec labels"
  (format t "~%--- Test Labels et Sauts ---~%")
  (create-test-file-with-labels)
  (let ((result (load-and-run-asm-file "test_labels.asm")))
    (assert-equal "5 < 3 est faux => 100" 100 result)))

(defun test-load-comments ()
  "Test fichier avec commentaires"
  (format t "~%--- Test Commentaires ---~%")
  (create-test-file-comments)
  (let ((result (load-and-run-asm-file "test_comments.asm")))
    (assert-equal "7 * 8 = 56" 56 result)))

(defun test-load-string ()
  "Test chargement depuis une chaîne"
  (format t "~%--- Test Chargement String ---~%")
  (let* ((asm-code "PUSH 10
PUSH 20
ADD
HALT")
         (result (load-and-run-asm-string asm-code)))
    (assert-equal "10 + 20 = 30" 30 result)))

(defun test-string-with-labels ()
  "Test string avec labels"
  (format t "~%--- Test String avec Labels ---~%")
  (let* ((asm-code "PUSH 10
PUSH 5
GT
JUMPIF greater
PUSH 0
JUMP end
greater:
PUSH 1
end:
HALT")
         (result (load-and-run-asm-string asm-code)))
    (assert-equal "10 > 5 => 1" 1 result)))

;;; ----------------------------------------------------------------------------
;;; Tests avancés
;;; ----------------------------------------------------------------------------

(defun test-multiple-labels ()
  "Test avec plusieurs labels"
  (format t "~%--- Test Labels Multiples ---~%")
  (let* ((asm-code "JUMP start
unused:
PUSH 999
start:
PUSH 42
JUMP end
middle:
PUSH 888
end:
HALT")
         (result (load-and-run-asm-string asm-code)))
    (assert-equal "Sauts multiples => 42" 42 result)))

(defun test-comparison-jump ()
  "Test comparaison avec saut"
  (format t "~%--- Test Comparaison + Saut ---~%")
  (let* ((asm-code "; Test if 3 <= 5
PUSH 3
PUSH 5
LE
JUMPNIF else_part
PUSH 111
JUMP done
else_part:
PUSH 222
done:
HALT")
         (result (load-and-run-asm-string asm-code)))
    (assert-equal "3 <= 5 => 111" 111 result)))

;;; ----------------------------------------------------------------------------
;;; Fonction principale de test
;;; ----------------------------------------------------------------------------

(defun run-loader-tests ()
  "Lance tous les tests du loader"
  (reset-test-counters)
  (format t "~%========================================~%")
  (format t "TESTS DU LOADER~%")
  (format t "========================================~%")
  
  (test-load-simple)
  (test-load-complex)
  (test-load-with-labels)
  (test-load-comments)
  (test-load-string)
  (test-string-with-labels)
  (test-multiple-labels)
  (test-comparison-jump)
  
  (print-test-summary)
  
  ;; Nettoyer les fichiers de test
  (format t "~%Nettoyage des fichiers de test...~%")
  (ignore-errors (delete-file "test_simple.asm"))
  (ignore-errors (delete-file "test_complex.asm"))
  (ignore-errors (delete-file "test_labels.asm"))
  (ignore-errors (delete-file "test_loop.asm"))
  (ignore-errors (delete-file "test_comments.asm"))
  
  (= *test-failed* 0))

;;; ----------------------------------------------------------------------------
;;; Exemples interactifs
;;; ----------------------------------------------------------------------------

(defun example-loader-if ()
  "Exemple : structure IF"
  (format t "~%=== Exemple IF (x > 5 ? 100 : 200) ===~%")
  (let ((asm "PUSH 7          ; x = 7
PUSH 5
GT              ; x > 5 ?
JUMPNIF else_branch
PUSH 100        ; then
JUMP end_if
else_branch:
PUSH 200        ; else
end_if:
HALT"))
    (load-and-run-asm-string asm)))

(defun example-loader-max ()
  "Exemple : maximum de deux nombres"
  (format t "~%=== Exemple MAX(15, 22) ===~%")
  (let ((asm "; Calcul du maximum
PUSH 15
PUSH 22
; Comparer
DUP             ; dupliquer b
PUSH 3          
LOAD 0          ; on simule l'accès à 'a'
STORE 0         ; sauver a dans var 0
STORE 1         ; sauver b dans var 1
LOAD 0          ; charger a
LOAD 1          ; charger b
GT              ; a > b ?
JUMPIF a_greater
LOAD 1          ; retourner b
JUMP done
a_greater:
LOAD 0          ; retourner a
done:
HALT"))
    (load-and-run-asm-string asm)))

(defun example-loader-simple ()
  "Exemple très simple"
  (format t "~%=== Exemple Simple ===~%")
  (let ((asm "PUSH 100
PUSH 23
ADD
HALT"))
    (load-and-run-asm-string asm)))

;;; Afficher les instructions
(format t "~%Pour lancer les tests: (run-loader-tests)~%")
(format t "Pour des exemples: (example-loader-simple) (example-loader-if)~%")
