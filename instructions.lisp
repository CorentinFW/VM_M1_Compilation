;;;; ============================================================================
;;;; DÉFINITION DES INSTRUCTIONS ET OPCODES
;;;; ============================================================================
;;;; Ce fichier définit le jeu d'instructions de la machine virtuelle

(in-package :cl-user)

;;; ----------------------------------------------------------------------------
;;; Définition des opcodes
;;; ----------------------------------------------------------------------------

(defparameter *opcodes*
  '(;; Manipulation de la pile
    (HALT    . 0)   ; Arrêt de la VM
    (NOP     . 1)   ; Pas d'opération
    (PUSH    . 2)   ; Empiler une valeur
    (POP     . 3)   ; Dépiler
    (DUP     . 4)   ; Dupliquer le sommet de la pile
    (PUSHSYM . 5)   ; Empiler un symbole
    
    ;; Opérations arithmétiques
    (ADD     . 10)  ; Addition
    (SUB     . 11)  ; Soustraction
    (MUL     . 12)  ; Multiplication
    (DIV     . 13)  ; Division
    (MOD     . 14)  ; Modulo
    
    ;; Opérations de comparaison
    (EQ      . 20)  ; Égalité
    (LT      . 21)  ; Inférieur
    (LE      . 22)  ; Inférieur ou égal
    (GT      . 23)  ; Supérieur
    (GE      . 24)  ; Supérieur ou égal
    
    ;; Contrôle de flux
    (JUMP    . 30)  ; Saut inconditionnel
    (JUMPIF  . 31)  ; Saut si vrai (sommet != 0)
    (JUMPNIF . 32)  ; Saut si faux (sommet == 0)
    (CALL    . 33)  ; Appel de fonction
    (RET     . 34)  ; Retour de fonction
    
    ;; Gestion mémoire/variables
    (LOAD    . 40)  ; Charger une variable
    (STORE   . 41)  ; Sauvegarder dans une variable
    (LOADARG . 42)  ; Charger un argument
    (STOREARG . 43) ; Sauvegarder un argument
    (ALLOC   . 44)  ; Allouer n cellules locales
    (DEALLOC . 45)  ; Désallouer n cellules
    
    ;; Gestion des fermetures
    (MKCLOSURE    . 50)  ; Créer une fermeture
    (LOADCLOSURE  . 51)  ; Charger variable de fermeture
    (STORECLOSURE . 52)  ; Sauvegarder dans fermeture
    (CALLCLOSURE  . 53)  ; Appeler une fermeture depuis la pile
    
    ;; Manipulation de listes
    (CONS    . 70)  ; Créer une paire (cons a b)
    (CAR     . 71)  ; Premier élément d'une paire
    (CDR     . 72)  ; Reste d'une paire
    (NULLP   . 73)  ; Test si NIL/null
    (LISTP   . 74)  ; Test si c'est une liste
    
    ;; Comparaison de symboles
    (SYMBOLP . 80)  ; Test si c'est un symbole
    (EQSYM   . 81)  ; Égalité de symboles (eq)
    
    ;; Utilitaires
    (PRINT   . 60)) ; Afficher le sommet de la pile (debug)
  "Table de correspondance mnémonique -> opcode")

;;; ----------------------------------------------------------------------------
;;; Fonctions utilitaires pour les opcodes
;;; ----------------------------------------------------------------------------

(defun opcode-from-mnemonic (mnemonic)
  "Retourne l'opcode numérique correspondant à un mnémonique"
  (cdr (assoc mnemonic *opcodes*)))

(defun mnemonic-from-opcode (opcode)
  "Retourne le mnémonique correspondant à un opcode numérique"
  (car (rassoc opcode *opcodes*)))

(defun instruction-has-operand-p (mnemonic)
  "Retourne T si l'instruction prend un opérande"
  (member mnemonic '(PUSH PUSHSYM JUMP JUMPIF JUMPNIF CALL LOAD STORE 
                     LOADARG STOREARG ALLOC DEALLOC MKCLOSURE 
                     LOADCLOSURE STORECLOSURE)))

;;; ----------------------------------------------------------------------------
;;; Structure d'une instruction
;;; ----------------------------------------------------------------------------

(defstruct instruction
  "Représente une instruction avec son opcode et son opérande optionnel"
  (opcode 0 :type integer)
  (operand nil))

(defun make-instruction-from-mnemonic (mnemonic &optional operand)
  "Crée une instruction à partir d'un mnémonique"
  (make-instruction 
   :opcode (opcode-from-mnemonic mnemonic)
   :operand operand))

(defun instruction-to-string (instr)
  "Convertit une instruction en chaîne lisible"
  (let ((mnemonic (mnemonic-from-opcode (instruction-opcode instr))))
    (if (instruction-operand instr)
        (format nil "~A ~A" mnemonic (instruction-operand instr))
        (format nil "~A" mnemonic))))

;;; ----------------------------------------------------------------------------
;;; Documentation des instructions
;;; ----------------------------------------------------------------------------

(defparameter *instruction-docs*
  '((HALT    . "Arrête l'exécution de la VM")
    (NOP     . "Aucune opération")
    (PUSH    . "Empile une valeur : PUSH <valeur>")
    (PUSHSYM . "Empile un symbole : PUSHSYM <symbole>")
    (POP     . "Dépile et ignore la valeur au sommet")
    (DUP     . "Duplique la valeur au sommet de la pile")
    
    (ADD     . "Dépile deux valeurs, empile leur somme")
    (SUB     . "Dépile b puis a, empile (a - b)")
    (MUL     . "Dépile deux valeurs, empile leur produit")
    (DIV     . "Dépile b puis a, empile (a / b)")
    (MOD     . "Dépile b puis a, empile (a mod b)")
    
    (EQ      . "Dépile deux valeurs, empile 1 si égales, 0 sinon")
    (LT      . "Dépile b puis a, empile 1 si a < b, 0 sinon")
    (LE      . "Dépile b puis a, empile 1 si a <= b, 0 sinon")
    (GT      . "Dépile b puis a, empile 1 si a > b, 0 sinon")
    (GE      . "Dépile b puis a, empile 1 si a >= b, 0 sinon")
    
    (JUMP    . "Saut inconditionnel : JUMP <adresse>")
    (JUMPIF  . "Dépile, saute si != 0 : JUMPIF <adresse>")
    (JUMPNIF . "Dépile, saute si == 0 : JUMPNIF <adresse>")
    (CALL    . "Appel de fonction : CALL <adresse>")
    (RET     . "Retour de fonction")
    
    (LOAD    . "Charge une variable : LOAD <index>")
    (STORE   . "Sauvegarde dans une variable : STORE <index>")
    (LOADARG . "Charge un argument : LOADARG <index>")
    (STOREARG . "Sauvegarde un argument : STOREARG <index>")
    (ALLOC   . "Alloue des cellules locales : ALLOC <n>")
    (DEALLOC . "Désalloue des cellules : DEALLOC <n>")
    
    (MKCLOSURE . "Crée une fermeture : MKCLOSURE <addr> <nvars>")
    (LOADCLOSURE . "Charge var de fermeture : LOADCLOSURE <index>")
    (STORECLOSURE . "Sauvegarde dans fermeture : STORECLOSURE <index>")
    (CALLCLOSURE . "Appelle une fermeture depuis la pile")
    
    (CONS    . "Dépile b puis a, empile (cons a b)")
    (CAR     . "Dépile une paire, empile son CAR")
    (CDR     . "Dépile une paire, empile son CDR")
    (NULLP   . "Dépile, empile 1 si NIL, 0 sinon")
    (LISTP   . "Dépile, empile 1 si liste, 0 sinon")
    
    (SYMBOLP . "Dépile, empile 1 si symbole, 0 sinon")
    (EQSYM   . "Dépile b puis a, empile 1 si eq, 0 sinon")
    
    (PRINT   . "Affiche le sommet de la pile (debug)"))
  "Documentation des instructions")

(defun print-instruction-help (&optional mnemonic)
  "Affiche l'aide pour une instruction ou toutes les instructions"
  (if mnemonic
      (format t "~A: ~A~%" mnemonic (cdr (assoc mnemonic *instruction-docs*)))
      (dolist (doc *instruction-docs*)
        (format t "~A: ~A~%" (car doc) (cdr doc)))))
