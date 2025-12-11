;;;; ============================================================================
;;;; MINI-LOADER EN LISP PUR
;;;; ============================================================================
;;;; Parser ASM et chargeur de bytecode écrit en LISP pur
;;;; Compatible avec le sous-ensemble minimal pour bootstrap
;;;;
;;;; CONTRAINTES:
;;;;   ✗ PAS de defstruct
;;;;   ✗ PAS de hash-table
;;;;   ✗ PAS de format avancé
;;;;   ✗ PAS de read-from-string
;;;;   ✓ Seulement: cons, car, cdr, list, append
;;;;   ✓ if, cond, let, defun, lambda
;;;;   ✓ +, -, *, /, =, <, >

;;; ----------------------------------------------------------------------------
;;; Table des opcodes (liste d'associations)
;;; ----------------------------------------------------------------------------

(defun make-opcode-table ()
  "Crée la table des opcodes en tant que liste d'associations"
  (list
   (cons 'HALT 0)
   (cons 'NOP 1)
   (cons 'PUSH 2)
   (cons 'POP 3)
   (cons 'DUP 4)
   (cons 'PUSHSYM 5)
   (cons 'ADD 10)
   (cons 'SUB 11)
   (cons 'MUL 12)
   (cons 'DIV 13)
   (cons 'MOD 14)
   (cons 'EQ 20)
   (cons 'LT 21)
   (cons 'LE 22)
   (cons 'GT 23)
   (cons 'GE 24)
   (cons 'JUMP 30)
   (cons 'JUMPIF 31)
   (cons 'JUMPNIF 32)
   (cons 'CALL 33)
   (cons 'RET 34)
   (cons 'LOAD 40)
   (cons 'STORE 41)
   (cons 'LOADARG 42)
   (cons 'STOREARG 43)
   (cons 'ALLOC 44)
   (cons 'DEALLOC 45)
   (cons 'MKCLOSURE 50)
   (cons 'LOADCLOSURE 51)
   (cons 'STORECLOSURE 52)
   (cons 'CALLCLOSURE 53)
   (cons 'PRINT 60)
   (cons 'CONS 70)
   (cons 'CAR 71)
   (cons 'CDR 72)
   (cons 'NULLP 73)
   (cons 'LISTP 74)
   (cons 'SYMBOLP 80)
   (cons 'EQSYM 81)))

(defun lookup-opcode (mnemonic opcode-table)
  "Cherche l'opcode d'un mnémonique dans la table"
  (if (null opcode-table)
      nil
      (if (eq (car (car opcode-table)) mnemonic)
          (cdr (car opcode-table))
          (lookup-opcode mnemonic (cdr opcode-table)))))

;;; ----------------------------------------------------------------------------
;;; Utilitaires de parsing de chaînes
;;; ----------------------------------------------------------------------------

(defun is-whitespace (char)
  "Teste si un caractère est un espace"
  (if (eq char 32)  ; espace
      t
      (if (eq char 9)  ; tab
          t
          (if (eq char 10)  ; newline
              t
              (if (eq char 13)  ; return
                  t
                  nil)))))

(defun skip-whitespace (chars)
  "Saute les espaces au début d'une liste de caractères"
  (if (null chars)
      nil
      (if (is-whitespace (car chars))
          (skip-whitespace (cdr chars))
          chars)))

(defun take-while-not-whitespace (chars)
  "Prend les caractères jusqu'au prochain espace"
  (if (null chars)
      nil
      (if (is-whitespace (car chars))
          nil
          (cons (car chars) (take-while-not-whitespace (cdr chars))))))

(defun string-to-list (str)
  "Convertit une chaîne en liste de caractères (codes ASCII)"
  (if (= (length str) 0)
      nil
      (cons (char-code (char str 0))
            (string-to-list (mini-substring str 1)))))

(defun list-to-string (chars)
  "Convertit une liste de caractères (codes ASCII) en chaîne"
  (if (null chars)
      ""
      (concatenate 'string
                   (string (code-char (car chars)))
                   (list-to-string (cdr chars)))))

(defun mini-substring (str start)
  "Extrait une sous-chaîne à partir de start"
  (if (>= start (length str))
      ""
      (subseq str start)))

(defun split-line (line)
  "Découpe une ligne en mots (séparés par des espaces)"
  (let ((chars (string-to-list line)))
    (split-line-chars (skip-whitespace chars) nil)))

(defun split-line-chars (chars acc)
  "Découpe les caractères en mots"
  (if (null chars)
      (if (null acc)
          nil
          (list acc))
      (let ((skipped (skip-whitespace chars)))
        (if (null skipped)
            (if (null acc)
                nil
                (list acc))
            (let ((word (take-while-not-whitespace skipped))
                  (rest (skip-whitespace (drop-while-not-whitespace skipped))))
              (cons word (split-line-chars rest nil)))))))

(defun drop-while-not-whitespace (chars)
  "Saute les caractères jusqu'au prochain espace"
  (if (null chars)
      nil
      (if (is-whitespace (car chars))
          chars
          (drop-while-not-whitespace (cdr chars)))))

;;; ----------------------------------------------------------------------------
;;; Parsing de nombres
;;; ----------------------------------------------------------------------------

(defun is-digit (char)
  "Teste si un caractère est un chiffre"
  (if (>= char 48)  ; '0'
      (if (<= char 57)  ; '9'
          t
          nil)
      nil))

(defun parse-number-chars (chars acc)
  "Parse une liste de caractères en nombre avec accumulateur"
  (if (null chars)
      acc
      (parse-number-chars (cdr chars)
                         (+ (* acc 10) (- (car chars) 48)))))

(defun all-digits (chars)
  "Vérifie si tous les caractères sont des chiffres"
  (if (null chars)
      t
      (if (is-digit (car chars))
          (all-digits (cdr chars))
          nil)))

(defun parse-number (word-chars)
  "Parse un mot (liste de chars) en nombre ou nil"
  (if (null word-chars)
      nil
      (if (= (car word-chars) 45)  ; '-'
          (if (all-digits (cdr word-chars))
              (- 0 (parse-number-chars (cdr word-chars) 0))
              nil)
          (if (all-digits word-chars)
              (parse-number-chars word-chars 0)
              nil))))

;;; ----------------------------------------------------------------------------
;;; Parsing de symboles
;;; ----------------------------------------------------------------------------

(defun char-to-upper (char)
  "Convertit un caractère minuscule en majuscule"
  (if (>= char 97)  ; 'a'
      (if (<= char 122)  ; 'z'
          (- char 32)
          char)
      char))

(defun chars-to-upper (chars)
  "Convertit une liste de caractères en majuscules"
  (if (null chars)
      nil
      (cons (char-to-upper (car chars))
            (chars-to-upper (cdr chars)))))

(defun parse-symbol (word-chars)
  "Parse un mot en symbole (intern)"
  (intern (list-to-string (chars-to-upper word-chars))))

;;; ----------------------------------------------------------------------------
;;; Première passe: collecter les labels
;;; ----------------------------------------------------------------------------

(defun is-label-line (word-chars)
  "Teste si c'est une ligne de label (se termine par :)"
  (if (null word-chars)
      nil
      (= (car (reverse word-chars)) 58)))  ; ':'

(defun extract-label-name (word-chars)
  "Extrait le nom du label (sans le :)"
  (parse-symbol (reverse (cdr (reverse word-chars)))))

(defun mini-first-pass (lines labels address)
  "Première passe: collecter tous les labels et leurs adresses
   lines: liste de lignes (chaînes)
   labels: liste d'associations (nom . adresse)
   address: adresse courante
   Retourne: nouvelle liste de labels"
  (if (null lines)
      labels
      (let* ((line (car lines))
             (words (split-line line)))
        (if (null words)
            ;; Ligne vide, continuer
            (mini-first-pass (cdr lines) labels address)
            (let ((first-word (car words)))
              (if (is-label-line first-word)
                  ;; C'est un label, l'enregistrer
                  (let ((label-name (extract-label-name first-word)))
                    (mini-first-pass (cdr lines)
                               (cons (cons label-name address) labels)
                               address))
                  ;; C'est une instruction, incrémenter l'adresse
                  (mini-first-pass (cdr lines) labels (+ address 1))))))))

;;; ----------------------------------------------------------------------------
;;; Deuxième passe: générer le bytecode
;;; ----------------------------------------------------------------------------

(defun is-label-instruction (mnemonic)
  "Teste si l'instruction prend un label comme opérande"
  (if (eq mnemonic 'JUMP)
      t
      (if (eq mnemonic 'JUMPIF)
          t
          (if (eq mnemonic 'JUMPNIF)
              t
              (if (eq mnemonic 'CALL)
                  t
                  (if (eq mnemonic 'MKCLOSURE)
                      t
                      nil))))))

(defun lookup-label (label-name labels)
  "Cherche l'adresse d'un label"
  (if (null labels)
      nil
      (if (eq (car (car labels)) label-name)
          (cdr (car labels))
          (lookup-label label-name (cdr labels)))))

(defun mini-parse-operand (word-chars mnemonic labels)
  "Parse un opérande selon le type d'instruction"
  (if (null word-chars)
      nil
      (if (is-label-instruction mnemonic)
          ;; C'est un label, le résoudre
          (lookup-label (parse-symbol word-chars) labels)
          ;; Sinon, parser comme nombre
          (parse-number word-chars))))

(defun mini-second-pass (lines labels opcode-table bytecode)
  "Deuxième passe: générer le bytecode
   lines: liste de lignes
   labels: liste de labels (de first-pass)
   opcode-table: table des opcodes
   bytecode: bytecode accumulé (liste d'entiers)
   Retourne: bytecode final"
  (if (null lines)
      (reverse bytecode)
      (let* ((line (car lines))
             (words (split-line line)))
        (if (null words)
            ;; Ligne vide
            (mini-second-pass (cdr lines) labels opcode-table bytecode)
            (let ((first-word (car words)))
              (if (is-label-line first-word)
                  ;; C'est un label, l'ignorer
                  (mini-second-pass (cdr lines) labels opcode-table bytecode)
                  ;; C'est une instruction
                  (let* ((mnemonic (parse-symbol first-word))
                         (opcode (lookup-opcode mnemonic opcode-table)))
                    (if (null opcode)
                        ;; Opcode inconnu, erreur
                        (mini-second-pass (cdr lines) labels opcode-table bytecode)
                        ;; Ajouter l'opcode au bytecode
                        (if (null (cdr words))
                            ;; Pas d'opérande
                            (mini-second-pass (cdr lines) labels opcode-table
                                       (cons opcode bytecode))
                            ;; Avec opérande
                            (let ((operand (mini-parse-operand (car (cdr words))
                                                        mnemonic labels)))
                              (mini-second-pass (cdr lines) labels opcode-table
                                         (cons operand (cons opcode bytecode)))))))))))))

;;; ----------------------------------------------------------------------------
;;; Fonction principale d'assemblage
;;; ----------------------------------------------------------------------------

(defun mini-assemble (lines)
  "Assemble une liste de lignes ASM en bytecode
   lines: liste de chaînes (lignes ASM)
   Retourne: liste d'entiers (bytecode)"
  (let* ((opcode-table (make-opcode-table))
         (labels (mini-first-pass lines nil 0))
         (bytecode (mini-second-pass lines labels opcode-table nil)))
    bytecode))

(defun mini-load-asm-string (asm-string)
  "Charge un programme ASM depuis une chaîne
   asm-string: chaîne contenant le code ASM
   Retourne: liste d'entiers (bytecode)"
  ;; Découper en lignes
  (let ((lines (split-string-newline asm-string)))
    (mini-assemble lines)))

(defun split-string-newline (str)
  "Découpe une chaîne en lignes"
  (split-string-newline-helper str 0 0 nil))

(defun split-string-newline-helper (str start current acc)
  "Helper pour découper par newline"
  (if (>= current (length str))
      (if (= start current)
          (reverse acc)
          (reverse (cons (subseq str start current) acc)))
      (if (char= (char str current) #\Newline)
          (split-string-newline-helper str (+ current 1) (+ current 1)
                                      (cons (subseq str start current) acc))
          (split-string-newline-helper str start (+ current 1) acc))))

;;; ----------------------------------------------------------------------------
;;; Tests et debug
;;; ----------------------------------------------------------------------------

(defun test-mini-loader ()
  "Test du mini-loader avec un programme simple"
  (let ((asm "PUSH 10
PUSH 20
ADD
HALT"))
    (mini-load-asm-string asm)))

;; Message de chargement
;; (format t "Mini-loader en LISP pur chargé!~%")
