;;;; ============================================================================
;;;; VM BOOTSTRAP - Version Compilable
;;;; ============================================================================
;;;; Version simplifiée de la VM écrite en LISP compilable par notre compilateur
;;;; 
;;;; Contraintes:
;;;; - Pas de defstruct → utiliser des listes
;;;; - Pas de hash-tables → utiliser des alists
;;;; - Pas de loop → utiliser la récursion
;;;; - Tout doit être compilable par compiler.lisp

;;; ----------------------------------------------------------------------------
;;; Constructeurs et accesseurs pour la VM (simulation de struct)
;;; ----------------------------------------------------------------------------

;; VM = (code pc stack halt)
;; Simplifié au maximum pour le bootstrap

(defun vm-new (code)
  "Crée une nouvelle VM avec le code donné"
  (list code 0 0 0))  ; code, pc, stack (0=NIL), halt

(defun vm-code (vm)
  "Retourne le code de la VM"
  (car vm))

(defun vm-pc (vm)
  "Retourne le PC de la VM"
  (car (cdr vm)))

(defun vm-stack (vm)
  "Retourne la pile de la VM"
  (car (cdr (cdr vm))))

(defun vm-halt (vm)
  "Retourne le flag halt de la VM"
  (car (cdr (cdr (cdr vm)))))

(defun vm-set-pc (vm new-pc)
  "Modifie le PC de la VM"
  (list (vm-code vm) new-pc (vm-stack vm) (vm-halt vm)))

(defun vm-set-stack (vm new-stack)
  "Modifie la pile de la VM"
  (list (vm-code vm) (vm-pc vm) new-stack (vm-halt vm)))

(defun vm-set-halt (vm new-halt)
  "Modifie le flag halt de la VM"
  (list (vm-code vm) (vm-pc vm) (vm-stack vm) new-halt))

;;; ----------------------------------------------------------------------------
;;; Opérations sur la pile
;;; ----------------------------------------------------------------------------

(defun vm-push (vm value)
  "Empile une valeur"
  (vm-set-stack vm (cons value (vm-stack vm))))

(defun vm-pop (vm)
  "Dépile et retourne (nouvelle-vm . valeur)"
  (if (null (vm-stack vm))
      (cons vm 0)  ; Erreur: retourne 0
      (cons (vm-set-stack vm (cdr (vm-stack vm)))
            (car (vm-stack vm)))))

(defun vm-peek (vm)
  "Regarde le sommet sans dépiler"
  (if (null (vm-stack vm))
      0
      (car (vm-stack vm))))

;;; ----------------------------------------------------------------------------
;;; Accès aux instructions
;;; ----------------------------------------------------------------------------

;; Instruction = (opcode . operand)
;; Opcode est un nombre, operand peut être 0 (NIL) ou une valeur

(defun instr-opcode (instr)
  "Retourne l'opcode d'une instruction"
  (car instr))

(defun instr-operand (instr)
  "Retourne l'opérande d'une instruction"
  (cdr instr))

(defun vm-fetch (vm)
  "Récupère l'instruction courante"
  (vm-nth (vm-pc vm) (vm-code vm)))

(defun vm-nth (n lst)
  "Retourne le n-ième élément d'une liste (0-indexé)"
  (if (<= n 0)
      (car lst)
      (vm-nth (- n 1) (cdr lst))))

;;; ----------------------------------------------------------------------------
;;; Exécution d'une instruction
;;; ----------------------------------------------------------------------------

(defun vm-exec-push (vm operand)
  "Exécute PUSH"
  (vm-set-pc (vm-push vm operand) (+ (vm-pc vm) 1)))

(defun vm-exec-pop (vm)
  "Exécute POP"
  (vm-set-pc (car (vm-pop vm)) (+ (vm-pc vm) 1)))

(defun vm-exec-add (vm)
  "Exécute ADD"
  (let ((pop1 (vm-pop vm)))
    (let ((vm1 (car pop1)))
      (let ((b (cdr pop1)))
        (let ((pop2 (vm-pop vm1)))
          (let ((vm2 (car pop2)))
            (let ((a (cdr pop2)))
              (vm-set-pc (vm-push vm2 (+ a b)) (+ (vm-pc vm) 1)))))))))

(defun vm-exec-sub (vm)
  "Exécute SUB"
  (let ((pop1 (vm-pop vm)))
    (let ((vm1 (car pop1)))
      (let ((b (cdr pop1)))
        (let ((pop2 (vm-pop vm1)))
          (let ((vm2 (car pop2)))
            (let ((a (cdr pop2)))
              (vm-set-pc (vm-push vm2 (- a b)) (+ (vm-pc vm) 1)))))))))

(defun vm-exec-mul (vm)
  "Exécute MUL"
  (let ((pop1 (vm-pop vm)))
    (let ((vm1 (car pop1)))
      (let ((b (cdr pop1)))
        (let ((pop2 (vm-pop vm1)))
          (let ((vm2 (car pop2)))
            (let ((a (cdr pop2)))
              (vm-set-pc (vm-push vm2 (* a b)) (+ (vm-pc vm) 1)))))))))

(defun vm-exec-halt (vm)
  "Exécute HALT"
  (vm-set-halt vm 1))

(defun vm-exec-jump (vm addr)
  "Exécute JUMP"
  (vm-set-pc vm addr))

(defun vm-exec-cons (vm)
  "Exécute CONS"
  (let ((pop1 (vm-pop vm)))
    (let ((vm1 (car pop1)))
      (let ((b (cdr pop1)))
        (let ((pop2 (vm-pop vm1)))
          (let ((vm2 (car pop2)))
            (let ((a (cdr pop2)))
              (vm-set-pc (vm-push vm2 (cons a b)) (+ (vm-pc vm) 1)))))))))

(defun vm-exec-car (vm)
  "Exécute CAR"
  (let ((pop1 (vm-pop vm)))
    (let ((vm1 (car pop1)))
      (let ((pair (cdr pop1)))
        (vm-set-pc (vm-push vm1 (car pair)) (+ (vm-pc vm) 1))))))

(defun vm-exec-cdr (vm)
  "Exécute CDR"
  (let ((pop1 (vm-pop vm)))
    (let ((vm1 (car pop1)))
      (let ((pair (cdr pop1)))
        (vm-set-pc (vm-push vm1 (cdr pair)) (+ (vm-pc vm) 1))))))

;;; ----------------------------------------------------------------------------
;;; Dispatch des instructions
;;; ----------------------------------------------------------------------------

(defun vm-execute (vm)
  "Exécute une instruction et retourne la nouvelle VM"
  (let ((instr (vm-fetch vm)))
    (let ((opcode (instr-opcode instr)))
      (let ((operand (instr-operand instr)))
        (if (= opcode 0)   ; HALT
            (vm-exec-halt vm)
            (if (= opcode 2)   ; PUSH
                (vm-exec-push vm operand)
                (if (= opcode 3)   ; POP
                    (vm-exec-pop vm)
                    (if (= opcode 10)  ; ADD
                        (vm-exec-add vm)
                        (if (= opcode 11)  ; SUB
                            (vm-exec-sub vm)
                            (if (= opcode 12)  ; MUL
                                (vm-exec-mul vm)
                                (if (= opcode 30)  ; JUMP
                                    (vm-exec-jump vm operand)
                                    (if (= opcode 70)  ; CONS
                                        (vm-exec-cons vm)
                                        (if (= opcode 71)  ; CAR
                                            (vm-exec-car vm)
                                            (if (= opcode 72)  ; CDR
                                                (vm-exec-cdr vm)
                                                vm))))))))))))))  ; Instruction inconnue: ne rien faire

;;; ----------------------------------------------------------------------------
;;; Boucle d'exécution
;;; ----------------------------------------------------------------------------

(defun vm-run-step (vm max-steps)
  "Exécute la VM pas à pas (récursivement)"
  (if (> (vm-halt vm) 0)
      vm  ; Halted
      (if (<= max-steps 0)
          vm  ; Max steps atteint
          (vm-run-step (vm-execute vm) (- max-steps 1)))))

(defun vm-run (vm)
  "Lance l'exécution de la VM (max 1000 steps)"
  (vm-run-step vm 1000))

(defun vm-result (vm)
  "Retourne le résultat (sommet de pile)"
  (vm-peek vm))

;;; ----------------------------------------------------------------------------
;;; Point d'entrée pour les tests
;;; ----------------------------------------------------------------------------

(defun test-vm-bootstrap ()
  "Test simple de la VM bootstrap"
  (let ((code (list (cons 2 5)      ; PUSH 5
                    (cons 2 3)      ; PUSH 3
                    (cons 10 0)     ; ADD
                    (cons 0 0))))   ; HALT
    (let ((vm (vm-new code)))
      (let ((result-vm (vm-run vm)))
        (vm-result result-vm)))))

;; Pour tester:
;; (test-vm-bootstrap)
;; Devrait retourner 8
