;;;; ============================================================================
;;;; TESTS DE LA MACHINE VIRTUELLE
;;;; ============================================================================
;;;; Tests unitaires et d'intégration pour la VM

(in-package :cl-user)
(load "vm.lisp")

;;; ----------------------------------------------------------------------------
;;; Utilitaires de test
;;; ----------------------------------------------------------------------------

(defvar *test-count* 0)
(defvar *test-passed* 0)
(defvar *test-failed* 0)

(defun reset-test-counters ()
  "Réinitialise les compteurs de tests"
  (setf *test-count* 0)
  (setf *test-passed* 0)
  (setf *test-failed* 0))

(defun assert-equal (name expected actual)
  "Vérifie que expected = actual"
  (incf *test-count*)
  (if (equal expected actual)
      (progn
        (incf *test-passed*)
        (format t "✓ ~A~%" name))
      (progn
        (incf *test-failed*)
        (format t "✗ ~A~%  Attendu: ~A~%  Obtenu:  ~A~%" name expected actual))))

(defun print-test-summary ()
  "Affiche le résumé des tests"
  (format t "~%========================================~%")
  (format t "Tests: ~A | Réussis: ~A | Échoués: ~A~%" 
          *test-count* *test-passed* *test-failed*)
  (format t "========================================~%"))

;;; ----------------------------------------------------------------------------
;;; Tests des instructions de base
;;; ----------------------------------------------------------------------------

(defun test-push-pop ()
  "Test PUSH et POP"
  (format t "~%--- Test PUSH/POP ---~%")
  (let* ((vm (make-vm))
         (code (list (make-instruction-from-mnemonic 'PUSH 42)
                     (make-instruction-from-mnemonic 'PUSH 10)
                     (make-instruction-from-mnemonic 'HALT))))
    (vm-load-code vm code)
    (vm-run vm)
    (assert-equal "PUSH 42, PUSH 10 => stack = (10 42)" 
                  '(10 42) 
                  (vm-stack vm))))

(defun test-arithmetic ()
  "Test des opérations arithmétiques"
  (format t "~%--- Test Arithmétique ---~%")
  
  ;; Test addition: 2 + 3 = 5
  (let ((result (vm-create-and-run
                 (list (make-instruction-from-mnemonic 'PUSH 2)
                       (make-instruction-from-mnemonic 'PUSH 3)
                       (make-instruction-from-mnemonic 'ADD)
                       (make-instruction-from-mnemonic 'HALT)))))
    (assert-equal "2 + 3 = 5" 5 result))
  
  ;; Test soustraction: 10 - 3 = 7
  (let ((result (vm-create-and-run
                 (list (make-instruction-from-mnemonic 'PUSH 10)
                       (make-instruction-from-mnemonic 'PUSH 3)
                       (make-instruction-from-mnemonic 'SUB)
                       (make-instruction-from-mnemonic 'HALT)))))
    (assert-equal "10 - 3 = 7" 7 result))
  
  ;; Test multiplication: 4 * 5 = 20
  (let ((result (vm-create-and-run
                 (list (make-instruction-from-mnemonic 'PUSH 4)
                       (make-instruction-from-mnemonic 'PUSH 5)
                       (make-instruction-from-mnemonic 'MUL)
                       (make-instruction-from-mnemonic 'HALT)))))
    (assert-equal "4 * 5 = 20" 20 result))
  
  ;; Test division: 20 / 4 = 5
  (let ((result (vm-create-and-run
                 (list (make-instruction-from-mnemonic 'PUSH 20)
                       (make-instruction-from-mnemonic 'PUSH 4)
                       (make-instruction-from-mnemonic 'DIV)
                       (make-instruction-from-mnemonic 'HALT)))))
    (assert-equal "20 / 4 = 5" 5 result)))

(defun test-complex-expression ()
  "Test d'une expression complexe: (2 + 3) * 4 = 20"
  (format t "~%--- Test Expression Complexe ---~%")
  (let ((result (vm-create-and-run
                 (list (make-instruction-from-mnemonic 'PUSH 2)
                       (make-instruction-from-mnemonic 'PUSH 3)
                       (make-instruction-from-mnemonic 'ADD)
                       (make-instruction-from-mnemonic 'PUSH 4)
                       (make-instruction-from-mnemonic 'MUL)
                       (make-instruction-from-mnemonic 'HALT)))))
    (assert-equal "(2 + 3) * 4 = 20" 20 result)))

(defun test-comparisons ()
  "Test des opérations de comparaison"
  (format t "~%--- Test Comparaisons ---~%")
  
  ;; Test égalité: 5 == 5 => 1
  (let ((result (vm-create-and-run
                 (list (make-instruction-from-mnemonic 'PUSH 5)
                       (make-instruction-from-mnemonic 'PUSH 5)
                       (make-instruction-from-mnemonic 'EQ)
                       (make-instruction-from-mnemonic 'HALT)))))
    (assert-equal "5 == 5 => 1" 1 result))
  
  ;; Test inférieur: 3 < 5 => 1
  (let ((result (vm-create-and-run
                 (list (make-instruction-from-mnemonic 'PUSH 3)
                       (make-instruction-from-mnemonic 'PUSH 5)
                       (make-instruction-from-mnemonic 'LT)
                       (make-instruction-from-mnemonic 'HALT)))))
    (assert-equal "3 < 5 => 1" 1 result))
  
  ;; Test inférieur ou égal: 5 <= 5 => 1
  (let ((result (vm-create-and-run
                 (list (make-instruction-from-mnemonic 'PUSH 5)
                       (make-instruction-from-mnemonic 'PUSH 5)
                       (make-instruction-from-mnemonic 'LE)
                       (make-instruction-from-mnemonic 'HALT)))))
    (assert-equal "5 <= 5 => 1" 1 result)))

(defun test-dup ()
  "Test de duplication"
  (format t "~%--- Test DUP ---~%")
  (let* ((vm (make-vm))
         (code (list (make-instruction-from-mnemonic 'PUSH 42)
                     (make-instruction-from-mnemonic 'DUP)
                     (make-instruction-from-mnemonic 'HALT))))
    (vm-load-code vm code)
    (vm-run vm)
    (assert-equal "DUP duplique le sommet" '(42 42) (vm-stack vm))))

(defun test-variables ()
  "Test des variables globales"
  (format t "~%--- Test Variables ---~%")
  ;; PUSH 100, STORE 0, PUSH 200, LOAD 0, ADD => 300
  (let ((result (vm-create-and-run
                 (list (make-instruction-from-mnemonic 'PUSH 100)
                       (make-instruction-from-mnemonic 'STORE 0)
                       (make-instruction-from-mnemonic 'PUSH 200)
                       (make-instruction-from-mnemonic 'LOAD 0)
                       (make-instruction-from-mnemonic 'ADD)
                       (make-instruction-from-mnemonic 'HALT)))))
    (assert-equal "Variable STORE/LOAD" 300 result)))

;;; ----------------------------------------------------------------------------
;;; Fonction principale de test
;;; ----------------------------------------------------------------------------

(defun run-all-tests ()
  "Lance tous les tests"
  (reset-test-counters)
  (format t "~%========================================~%")
  (format t "TESTS DE LA MACHINE VIRTUELLE~%")
  (format t "========================================~%")
  
  (test-push-pop)
  (test-arithmetic)
  (test-complex-expression)
  (test-comparisons)
  (test-dup)
  (test-variables)
  
  (print-test-summary)
  
  (= *test-failed* 0))

;;; ----------------------------------------------------------------------------
;;; Exemples interactifs
;;; ----------------------------------------------------------------------------

(defun example-simple ()
  "Exemple simple: 2 + 3"
  (format t "~%=== Exemple: 2 + 3 ===~%")
  (vm-create-and-run
   (list (make-instruction-from-mnemonic 'PUSH 2)
         (make-instruction-from-mnemonic 'PUSH 3)
         (make-instruction-from-mnemonic 'ADD)
         (make-instruction-from-mnemonic 'PRINT)
         (make-instruction-from-mnemonic 'HALT))))

(defun example-complex ()
  "Exemple complexe: (10 + 5) * 2 - 3"
  (format t "~%=== Exemple: (10 + 5) * 2 - 3 ===~%")
  (vm-create-and-run
   (list (make-instruction-from-mnemonic 'PUSH 10)
         (make-instruction-from-mnemonic 'PUSH 5)
         (make-instruction-from-mnemonic 'ADD)
         (make-instruction-from-mnemonic 'PUSH 2)
         (make-instruction-from-mnemonic 'MUL)
         (make-instruction-from-mnemonic 'PUSH 3)
         (make-instruction-from-mnemonic 'SUB)
         (make-instruction-from-mnemonic 'PRINT)
         (make-instruction-from-mnemonic 'HALT))))

(defun example-debug ()
  "Exemple en mode debug"
  (format t "~%=== Exemple en mode DEBUG: 5 * 6 ===~%")
  (vm-create-and-run
   (list (make-instruction-from-mnemonic 'PUSH 5)
         (make-instruction-from-mnemonic 'PUSH 6)
         (make-instruction-from-mnemonic 'MUL)
         (make-instruction-from-mnemonic 'HALT))
   :debug t))

;;; Lancer les tests automatiquement au chargement
(format t "~%Pour lancer les tests, exécutez: (run-all-tests)~%")
(format t "Pour des exemples: (example-simple) (example-complex) (example-debug)~%")
