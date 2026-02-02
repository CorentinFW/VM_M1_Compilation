;;;; Machine Virtuelle
;;;; Interprète les instructions ASM produites par le compilateur

(defpackage :vm
  (:use :common-lisp)
  (:export :make-vm
           :vm-run
           :vm-load-program
           :vm-reset
           :vm-step
           :*debug-vm*))

(in-package :vm)

(defvar *debug-vm* nil "Active le mode debug de la VM")

;;; Structure de la VM
(defstruct vm
  (stack nil)              ; Pile d'exécution
  (frames nil)             ; Pile des frames (environnements)
  (pc 0)                   ; Program counter
  (program nil)            ; Programme chargé
  (labels nil)             ; Table des labels
  (running t))             ; État d'exécution

;;; Structure pour les frames (environnements d'exécution)
(defstruct frame
  (vars nil)               ; Variables locales
  (return-pc nil))         ; Adresse de retour

;;; Opérations sur la pile
(defun vm-push (vm value)
  "Empile une valeur sur la pile"
  (push value (vm-stack vm)))

(defun vm-pop (vm)
  "Dépile une valeur de la pile"
  (if (vm-stack vm)
      (pop (vm-stack vm))
      (error "Stack underflow")))

(defun vm-top (vm)
  "Retourne la valeur au sommet de la pile sans la dépiler"
  (if (vm-stack vm)
      (car (vm-stack vm))
      (error "Stack empty")))

;;; Gestion des frames
(defun vm-push-frame (vm nvars)
  "Crée un nouveau frame avec nvars variables"
  (let ((frame (make-frame :vars (make-array nvars :initial-element nil)
                          :return-pc (vm-pc vm))))
    (push frame (vm-frames vm))))

(defun vm-pop-frame (vm)
  "Retire le frame courant"
  (if (vm-frames vm)
      (pop (vm-frames vm))
      (error "No frame to pop")))

(defun vm-current-frame (vm)
  "Retourne le frame courant"
  (car (vm-frames vm)))

(defun vm-get-frame (vm depth)
  "Récupère un frame à une profondeur donnée"
  (nth depth (vm-frames vm)))

;;; Accès aux variables
(defun vm-load-var (vm depth offset)
  "Charge une variable depuis un frame"
  (let ((frame (vm-get-frame vm depth)))
    (if frame
        (let ((vars (frame-vars frame)))
          (if (and (< offset (length vars)))
              (aref vars offset)
              (error "Variable offset out of bounds: ~A" offset)))
        (error "Frame depth out of bounds: ~A" depth))))

(defun vm-store-var (vm depth offset value)
  "Stocke une valeur dans une variable"
  (let ((frame (vm-get-frame vm depth)))
    (if frame
        (let ((vars (frame-vars frame)))
          (if (< offset (length vars))
              (setf (aref vars offset) value)
              (error "Variable offset out of bounds: ~A" offset)))
        (error "Frame depth out of bounds: ~A" depth))))

;;; Recherche de labels
(defun vm-find-label (vm label)
  "Trouve l'adresse d'un label"
  (let ((addr (gethash label (vm-labels vm))))
    (if addr
        addr
        (error "Label non trouvé: ~A" label))))

;;; Exécution des instructions
(defun vm-execute-instruction (vm instr)
  "Exécute une instruction"
  (when *debug-vm*
    (format t "PC=~A: ~A | Stack: ~A~%" 
            (vm-pc vm) instr (vm-stack vm)))
  
  (let ((opcode-name (symbol-name (car instr)))
        (operands (cdr instr)))
    (cond
      ;; Opérations de pile
      ((string= opcode-name "PUSH")
       (vm-push vm (car operands))
       (incf (vm-pc vm)))
      
      ((string= opcode-name "POP")
       (vm-pop vm)
       (incf (vm-pc vm)))
      
      ;; Opérations de mémoire
      ((string= opcode-name "LOAD")
       (let ((depth (first operands))
             (offset (second operands)))
         (vm-push vm (vm-load-var vm depth offset)))
       (incf (vm-pc vm)))
      
      ((string= opcode-name "STORE")
       (let ((depth (first operands))
             (offset (second operands))
             (value (vm-pop vm)))
         (vm-store-var vm depth offset value))
       (incf (vm-pc vm)))
      
      ;; Opérations arithmétiques
      ((string= opcode-name "ADD")
       (let ((b (vm-pop vm))
             (a (vm-pop vm)))
         (vm-push vm (+ a b)))
       (incf (vm-pc vm)))
      
      ((string= opcode-name "SUB")
       (let ((b (vm-pop vm))
             (a (vm-pop vm)))
         (vm-push vm (- a b)))
       (incf (vm-pc vm)))
      
      ((string= opcode-name "MUL")
       (let ((b (vm-pop vm))
             (a (vm-pop vm)))
         (vm-push vm (* a b)))
       (incf (vm-pc vm)))
      
      ((string= opcode-name "DIV")
       (let ((b (vm-pop vm))
             (a (vm-pop vm)))
         (if (zerop b)
             (error "Division par zéro")
             (vm-push vm (floor a b))))
       (incf (vm-pc vm)))
      
      ;; Opérations de comparaison
      ((string= opcode-name "EQ")
       (let ((b (vm-pop vm))
             (a (vm-pop vm)))
         (vm-push vm (if (= a b) 'T 'NIL)))
       (incf (vm-pc vm)))
      
      ((string= opcode-name "LT")
       (let ((b (vm-pop vm))
             (a (vm-pop vm)))
         (vm-push vm (if (< a b) 'T 'NIL)))
       (incf (vm-pc vm)))
      
      ((string= opcode-name "LE")
       (let ((b (vm-pop vm))
             (a (vm-pop vm)))
         (vm-push vm (if (<= a b) 'T 'NIL)))
       (incf (vm-pc vm)))
      
      ((string= opcode-name "GT")
       (let ((b (vm-pop vm))
             (a (vm-pop vm)))
         (vm-push vm (if (> a b) 'T 'NIL)))
       (incf (vm-pc vm)))
      
      ((string= opcode-name "GE")
       (let ((b (vm-pop vm))
             (a (vm-pop vm)))
         (vm-push vm (if (>= a b) 'T 'NIL)))
       (incf (vm-pc vm)))
      
      ;; Contrôle de flux
      ((string= opcode-name "JUMP")
       (setf (vm-pc vm) (vm-find-label vm (car operands))))
      
      ((string= opcode-name "JUMPNIL")
       (let ((value (vm-pop vm)))
         (if (or (null value) (eq value 'NIL))
             (setf (vm-pc vm) (vm-find-label vm (car operands)))
             (incf (vm-pc vm)))))
      
      ;; Contrôle de flux - appels de fonction
      ((string= opcode-name "CALL")
       (let* ((label (first operands))
              (nargs (second operands))
              (args (loop repeat nargs collect (vm-pop vm))))
         ;; Créer un nouveau frame pour l'appel
         (vm-push-frame vm nargs)
         ;; Stocker les arguments dans le nouveau frame
         (loop for arg in args
               for i from 0
               do (vm-store-var vm 0 i arg))
         ;; Sauvegarder l'adresse de retour
         (setf (frame-return-pc (vm-current-frame vm)) (1+ (vm-pc vm)))
         ;; Sauter à la fonction
         (setf (vm-pc vm) (vm-find-label vm label))))
      
      ((string= opcode-name "RETURN")
       (let ((return-value (if (vm-stack vm) (vm-pop vm) nil)))
         (if (vm-frames vm)
             ;; Il y a un frame, retour normal
             (let ((return-pc (frame-return-pc (vm-current-frame vm))))
               (vm-pop-frame vm)
               (when return-value
                 (vm-push vm return-value))
               (if return-pc
                   (setf (vm-pc vm) return-pc)
                   (setf (vm-running vm) nil)))
             ;; Pas de frame, arrêt de la VM
             (progn
               (when return-value
                 (vm-push vm return-value))
               (setf (vm-running vm) nil)))))
      
      ;; Gestion des frames
      ((string= opcode-name "MAKEFRAME")
       (vm-push-frame vm (car operands))
       (incf (vm-pc vm)))
      
      ((string= opcode-name "POPFRAME")
       ;; Sauvegarder la valeur de retour
       (let ((return-value (if (vm-stack vm) (vm-pop vm) nil)))
         (vm-pop-frame vm)
         (when return-value
           (vm-push vm return-value)))
       (incf (vm-pc vm)))
      
      ;; Fermetures
      ((string= opcode-name "MAKECLOSURE")
       (let ((label (first operands))
             (nvars (second operands)))
         ;; Pour simplifier, on pousse juste le label
         ;; Une implémentation complète devrait capturer l'environnement
         (vm-push vm (list 'CLOSURE label nvars)))
       (incf (vm-pc vm)))
      
      ;; Affichage
      ((string= opcode-name "PRINT")
       (format t "~A~%" (vm-pop vm))
       (incf (vm-pc vm)))
      
      ;; Arrêt
      ((string= opcode-name "HALT")
       (setf (vm-running vm) nil))
      
      (t
       (error "Instruction inconnue: ~A" opcode-name)))))

;;; Chargement et exécution
(defun vm-load-program (vm program labels)
  "Charge un programme dans la VM"
  (setf (vm-program vm) (coerce program 'vector))
  (setf (vm-labels vm) labels)
  (setf (vm-pc vm) 0)
  (setf (vm-running vm) t))

(defun vm-step (vm)
  "Exécute une instruction"
  (when (and (vm-running vm)
             (< (vm-pc vm) (length (vm-program vm))))
    (let ((instr (aref (vm-program vm) (vm-pc vm))))
      (vm-execute-instruction vm instr)
      t)))

(defun vm-run (vm)
  "Exécute le programme chargé dans la VM"
  (loop while (and (vm-running vm)
                   (< (vm-pc vm) (length (vm-program vm))))
        do (vm-step vm))
  
  ;; Retourner la valeur au sommet de la pile si elle existe
  (if (vm-stack vm)
      (vm-top vm)
      nil))

(defun vm-reset (vm)
  "Réinitialise la VM"
  (setf (vm-stack vm) nil)
  (setf (vm-frames vm) nil)
  (setf (vm-pc vm) 0)
  (setf (vm-running vm) t))

;;; Utilitaires de debug
(defun vm-print-state (vm)
  "Affiche l'état de la VM"
  (format t "~%=== État de la VM ===~%")
  (format t "PC: ~A~%" (vm-pc vm))
  (format t "Running: ~A~%" (vm-running vm))
  (format t "Stack: ~A~%" (vm-stack vm))
  (format t "Frames: ~A~%" (length (vm-frames vm)))
  (when (vm-frames vm)
    (format t "Current frame vars: ~A~%" 
            (frame-vars (vm-current-frame vm))))
  (format t "=====================~%"))
