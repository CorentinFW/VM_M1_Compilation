;;;; ============================================================================
;;;; MACHINE VIRTUELLE (VM)
;;;; ============================================================================
;;;; Implémentation de la machine virtuelle qui exécute le bytecode

(in-package :cl-user)
(load "instructions.lisp")

;;; ----------------------------------------------------------------------------
;;; Structure de la machine virtuelle
;;; ----------------------------------------------------------------------------

(defstruct vm
  "Machine virtuelle avec pile, environnement et code"
  (code #() :type vector)           ; Tableau d'instructions
  (pc 0 :type integer)               ; Program Counter (pointeur d'instruction)
  (stack '())                        ; Pile d'exécution (liste)
  (env (make-hash-table))            ; Environnement (variables) - hash table
  (locals '())                       ; Variables locales (liste de frames)
  (call-stack '())                   ; Pile d'appels
  (heap '())                         ; Tas pour les fermetures
  (halt nil :type boolean)           ; Flag d'arrêt
  (debug nil :type boolean))         ; Mode debug

;;; ----------------------------------------------------------------------------
;;; Gestion de la pile
;;; ----------------------------------------------------------------------------

(defun vm-push (vm value)
  "Empile une valeur sur la pile de la VM"
  (push value (vm-stack vm)))

(defun vm-pop (vm)
  "Dépile et retourne la valeur au sommet de la pile"
  (if (null (vm-stack vm))
      (error "Stack underflow: tentative de dépiler une pile vide")
      (pop (vm-stack vm))))

(defun vm-peek (vm)
  "Retourne la valeur au sommet sans dépiler"
  (if (null (vm-stack vm))
      (error "Stack underflow: pile vide")
      (car (vm-stack vm))))

(defun vm-dup (vm)
  "Duplique le sommet de la pile"
  (let ((top (vm-peek vm)))
    (vm-push vm top)))

;;; ----------------------------------------------------------------------------
;;; Gestion de l'environnement et des variables
;;; ----------------------------------------------------------------------------

(defun vm-store-var (vm index value)
  "Sauvegarde une valeur dans une variable globale"
  (setf (gethash index (vm-env vm)) value))

(defun vm-load-var (vm index)
  "Charge une variable globale"
  (multiple-value-bind (value found) (gethash index (vm-env vm))
    (if found
        value
        (error "Variable non définie: ~A" index))))

(defun vm-alloc-locals (vm n)
  "Alloue un nouveau frame de n variables locales"
  (let ((frame (make-array n :initial-element nil)))
    (push frame (vm-locals vm))))

(defun vm-dealloc-locals (vm n)
  "Désalloue le frame de variables locales"
  (declare (ignore n))
  (if (vm-locals vm)
      (pop (vm-locals vm))
      (error "Aucun frame local à désallouer")))

(defun vm-load-local (vm index)
  "Charge une variable locale du frame courant"
  (if (vm-locals vm)
      (let ((frame (car (vm-locals vm))))
        (if (< index (length frame))
            (aref frame index)
            (error "Index local hors limite: ~A" index)))
      (error "Aucun frame local")))

(defun vm-store-local (vm index value)
  "Sauvegarde dans une variable locale du frame courant"
  (if (vm-locals vm)
      (let ((frame (car (vm-locals vm))))
        (if (< index (length frame))
            (setf (aref frame index) value)
            (error "Index local hors limite: ~A" index)))
      (error "Aucun frame local")))

;;; ----------------------------------------------------------------------------
;;; Gestion de la pile d'appels
;;; ----------------------------------------------------------------------------

(defstruct call-frame
  "Frame d'appel de fonction"
  (return-address 0 :type integer)   ; Adresse de retour
  (args '()))                        ; Arguments de la fonction

(defun vm-call (vm address args)
  "Appelle une fonction à l'adresse donnée avec des arguments"
  (let ((frame (make-call-frame 
                :return-address (1+ (vm-pc vm))  ; Sauvegarder PC+1 pour retourner après CALL
                :args args)))
    (push frame (vm-call-stack vm))
    (setf (vm-pc vm) address)))

(defun vm-return (vm)
  "Retourne d'un appel de fonction"
  (if (null (vm-call-stack vm))
      (progn
        (setf (vm-halt vm) t)  ; Plus d'appels, on arrête
        (vm-pc vm))
      (let* ((frame (pop (vm-call-stack vm)))
             (return-addr (call-frame-return-address frame)))
        (setf (vm-pc vm) return-addr))))

(defun vm-load-arg (vm index)
  "Charge un argument de la fonction courante"
  (if (null (vm-call-stack vm))
      (error "Aucun frame d'appel")
      (let* ((frame (car (vm-call-stack vm)))
             (args (call-frame-args frame)))
        (if (< index (length args))
            (nth index args)
            (error "Index d'argument hors limite: ~A" index)))))

;;; ----------------------------------------------------------------------------
;;; Exécution des instructions
;;; ----------------------------------------------------------------------------

(defun vm-execute-instruction (vm instr)
  "Exécute une instruction"
  (let ((opcode (instruction-opcode instr))
        (operand (instruction-operand instr)))
    
    (case (mnemonic-from-opcode opcode)
      ;; Contrôle
      (HALT (setf (vm-halt vm) t))
      (NOP nil)
      
      ;; Pile
      (PUSH (vm-push vm operand))
      (POP (vm-pop vm))
      (DUP (vm-dup vm))
      
      ;; Arithmétique
      (ADD (let ((b (vm-pop vm))
                 (a (vm-pop vm)))
             (vm-push vm (+ a b))))
      (SUB (let ((b (vm-pop vm))
                 (a (vm-pop vm)))
             (vm-push vm (- a b))))
      (MUL (let ((b (vm-pop vm))
                 (a (vm-pop vm)))
             (vm-push vm (* a b))))
      (DIV (let ((b (vm-pop vm))
                 (a (vm-pop vm)))
             (when (zerop b)
               (error "Division par zéro"))
             (vm-push vm (floor a b))))
      (MOD (let ((b (vm-pop vm))
                 (a (vm-pop vm)))
             (vm-push vm (mod a b))))
      
      ;; Comparaisons
      (EQ (let ((b (vm-pop vm))
                (a (vm-pop vm)))
            (vm-push vm (if (= a b) 1 0))))
      (LT (let ((b (vm-pop vm))
                (a (vm-pop vm)))
            (vm-push vm (if (< a b) 1 0))))
      (LE (let ((b (vm-pop vm))
                (a (vm-pop vm)))
            (vm-push vm (if (<= a b) 1 0))))
      (GT (let ((b (vm-pop vm))
                (a (vm-pop vm)))
            (vm-push vm (if (> a b) 1 0))))
      (GE (let ((b (vm-pop vm))
                (a (vm-pop vm)))
            (vm-push vm (if (>= a b) 1 0))))
      
      ;; Contrôle de flux
      (JUMP (setf (vm-pc vm) operand)
            (return-from vm-execute-instruction))
      (JUMPIF (let ((cond (vm-pop vm)))
                (when (/= cond 0)
                  (setf (vm-pc vm) operand)
                  (return-from vm-execute-instruction))))
      (JUMPNIF (let ((cond (vm-pop vm)))
                 (when (= cond 0)
                   (setf (vm-pc vm) operand)
                   (return-from vm-execute-instruction))))
      (CALL (let ((n-args (vm-pop vm))  ; Nombre d'arguments
                  (args '()))
              (dotimes (i n-args)
                (push (vm-pop vm) args))
              ;; Les arguments sont maintenant dans l'ordre inverse, on les remet dans le bon ordre
              (vm-call vm operand (nreverse args))
              (return-from vm-execute-instruction)))
      (RET (vm-return vm)
           (return-from vm-execute-instruction))
      
      ;; Variables
      (LOAD (vm-push vm (vm-load-var vm operand)))
      (STORE (let ((value (vm-pop vm)))
               (vm-store-var vm operand value)))
      (LOADARG (vm-push vm (vm-load-arg vm operand)))
      (ALLOC (vm-alloc-locals vm operand))
      (DEALLOC (vm-dealloc-locals vm operand))
      
      ;; Debug
      (PRINT (format t "=> ~A~%" (vm-peek vm)))
      
      (otherwise (error "Instruction inconnue: opcode ~A" opcode)))
    
    ;; Incrémenter le PC (sauf si déjà modifié par JUMP/CALL/RET)
    (incf (vm-pc vm))))

;;; ----------------------------------------------------------------------------
;;; Boucle d'exécution principale (Fetch-Decode-Execute)
;;; ----------------------------------------------------------------------------

(defun vm-print-state (vm)
  "Affiche l'état de la VM (mode debug)"
  (format t "~%--- État VM ---~%")
  (format t "PC: ~A~%" (vm-pc vm))
  (format t "Stack: ~A~%" (vm-stack vm))
  (format t "Locals: ~A frames~%" (length (vm-locals vm)))
  (format t "Call-stack: ~A frames~%" (length (vm-call-stack vm)))
  (when (< (vm-pc vm) (length (vm-code vm)))
    (format t "Next instruction: ~A~%" 
            (instruction-to-string (aref (vm-code vm) (vm-pc vm)))))
  (format t "---------------~%"))

(defun vm-run (vm)
  "Lance l'exécution de la VM"
  (setf (vm-halt vm) nil)
  (setf (vm-pc vm) 0)
  
  (loop while (and (not (vm-halt vm))
                   (< (vm-pc vm) (length (vm-code vm))))
        do (progn
             ;; Mode debug: afficher l'état avant chaque instruction
             (when (vm-debug vm)
               (vm-print-state vm)
               (format t "Appuyez sur Entrée pour continuer...")
               (read-line))
             
             ;; Fetch
             (let ((instruction (aref (vm-code vm) (vm-pc vm))))
               
               ;; Decode & Execute
               (handler-case
                   (vm-execute-instruction vm instruction)
                 (error (e)
                   (format t "~%ERREUR à PC=~A: ~A~%" (vm-pc vm) e)
                   (vm-print-state vm)
                   (setf (vm-halt vm) t))))))
  
  ;; Retourner le résultat (sommet de pile s'il existe)
  (if (vm-stack vm)
      (vm-peek vm)
      nil))

;;; ----------------------------------------------------------------------------
;;; Utilitaires pour créer et charger du code
;;; ----------------------------------------------------------------------------

(defun vm-load-code (vm instructions)
  "Charge un tableau d'instructions dans la VM"
  (setf (vm-code vm) (coerce instructions 'vector))
  (setf (vm-pc vm) 0)
  (setf (vm-halt vm) nil))

(defun vm-create-and-run (instructions &key debug)
  "Crée une VM, charge le code et l'exécute"
  (let ((vm (make-vm :debug debug)))
    (vm-load-code vm instructions)
    (let ((result (vm-run vm)))
      (format t "~%Résultat: ~A~%" result)
      result)))

;;; ----------------------------------------------------------------------------
;;; Export des symboles principaux
;;; ----------------------------------------------------------------------------

(export '(make-vm vm-run vm-load-code vm-create-and-run
          vm-push vm-pop vm-peek vm-print-state))
