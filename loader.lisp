;;;; ============================================================================
;;;; CHARGEUR (LOADER)
;;;; ============================================================================
;;;; Parse et charge des fichiers ASM dans la machine virtuelle

(in-package :cl-user)
(load "vm.lisp")

;;; ----------------------------------------------------------------------------
;;; Structure du loader
;;; ----------------------------------------------------------------------------

(defstruct loader
  "Chargeur de fichiers ASM"
  (instructions '())          ; Liste d'instructions parsées
  (labels (make-hash-table :test 'equal))  ; Table label -> adresse
  (symbols (make-hash-table :test 'equal)) ; Table symboles
  (constants '())             ; Liste des constantes
  (current-address 0))        ; Adresse courante pendant le parsing

;;; ----------------------------------------------------------------------------
;;; Utilitaires de parsing
;;; ----------------------------------------------------------------------------

(defun trim-string (str)
  "Enlève les espaces au début et à la fin"
  (string-trim '(#\Space #\Tab #\Newline #\Return) str))

(defun split-string (str delimiter)
  "Découpe une chaîne selon un délimiteur"
  (let ((result '())
        (current ""))
    (loop for char across str do
      (if (char= char delimiter)
          (when (> (length current) 0)
            (push current result)
            (setf current ""))
          (setf current (concatenate 'string current (string char)))))
    (when (> (length current) 0)
      (push current result))
    (nreverse result)))

(defun comment-line-p (line)
  "Vérifie si une ligne est un commentaire"
  (let ((trimmed (trim-string line)))
    (or (zerop (length trimmed))
        (char= (char trimmed 0) #\;))))

(defun label-line-p (line)
  "Vérifie si une ligne contient un label (se termine par :)"
  (let ((trimmed (trim-string line)))
    (and (> (length trimmed) 0)
         (char= (char trimmed (1- (length trimmed))) #\:))))

(defun extract-label (line)
  "Extrait le nom du label d'une ligne"
  (let ((trimmed (trim-string line)))
    (subseq trimmed 0 (1- (length trimmed)))))

(defun parse-instruction-line (line)
  "Parse une ligne d'instruction et retourne (mnémonique operand)"
  (let* ((trimmed (trim-string line))
         (comment-pos (position #\; trimmed)))
    ;; Enlever le commentaire s'il y en a un
    (when comment-pos
      (setf trimmed (subseq trimmed 0 comment-pos))
      (setf trimmed (trim-string trimmed)))
    
    ;; Séparer le mnémonique de l'opérande
    (let ((parts (split-string trimmed #\Space)))
      (if (null parts)
          nil
          (let ((mnemonic (intern (string-upcase (car parts)) :cl-user))
                (operand (if (cdr parts)
                            (cadr parts)
                            nil)))
            (list mnemonic operand))))))

(defun parse-operand (operand-str mnemonic)
  "Parse un opérande (peut être un nombre, un label, etc.)"
  (cond
    ;; Pas d'opérande
    ((null operand-str) nil)
    
    ;; Instructions qui prennent des labels (adresses)
    ((member mnemonic '(JUMP JUMPIF JUMPNIF CALL))
     ;; Retourner le label tel quel, sera résolu plus tard
     (intern (string-upcase operand-str) :cl-user))
    
    ;; Nombre
    ((every #'digit-char-p operand-str)
     (parse-integer operand-str))
    
    ;; Nombre négatif
    ((and (char= (char operand-str 0) #\-)
          (every #'digit-char-p (subseq operand-str 1)))
     (parse-integer operand-str))
    
    ;; Symbole/variable
    (t (intern (string-upcase operand-str) :cl-user))))

;;; ----------------------------------------------------------------------------
;;; Première passe : collecte des labels
;;; ----------------------------------------------------------------------------

(defun first-pass (loader lines)
  "Première passe : enregistre les labels et leur adresse"
  (let ((address 0))
    (dolist (line lines)
      (cond
        ;; Ignorer les commentaires et lignes vides
        ((comment-line-p line) nil)
        
        ;; Label
        ((label-line-p line)
         (let ((label (extract-label line)))
           (setf (gethash (intern (string-upcase label) :cl-user) 
                         (loader-labels loader))
                 address)))
        
        ;; Instruction
        (t (incf address))))))

;;; ----------------------------------------------------------------------------
;;; Deuxième passe : parsing des instructions
;;; ----------------------------------------------------------------------------

(defun second-pass (loader lines)
  "Deuxième passe : parse les instructions"
  (let ((instructions '()))
    (dolist (line lines)
      (unless (or (comment-line-p line) (label-line-p line))
        (let ((parsed (parse-instruction-line line)))
          (when parsed
            (let* ((mnemonic (car parsed))
                   (operand-str (cadr parsed))
                   (operand (parse-operand operand-str mnemonic)))
              (push (list mnemonic operand) instructions))))))
    (setf (loader-instructions loader) (nreverse instructions))))

;;; ----------------------------------------------------------------------------
;;; Troisième passe : résolution des labels
;;; ----------------------------------------------------------------------------

(defun resolve-labels (loader)
  "Résout les références aux labels"
  (let ((resolved '()))
    (dolist (instr (loader-instructions loader))
      (let* ((mnemonic (car instr))
             (operand (cadr instr))
             (new-operand operand))
        
        ;; Si l'opérande est un symbole et correspond à un label
        (when (and (symbolp operand)
                   (member mnemonic '(JUMP JUMPIF JUMPNIF CALL)))
          (multiple-value-bind (address found)
              (gethash operand (loader-labels loader))
            (if found
                (setf new-operand address)
                (error "Label non défini: ~A" operand))))
        
        (push (list mnemonic new-operand) resolved)))
    (setf (loader-instructions loader) (nreverse resolved))))

;;; ----------------------------------------------------------------------------
;;; Conversion en instructions VM
;;; ----------------------------------------------------------------------------

(defun convert-to-vm-instructions (loader)
  "Convertit les instructions parsées en instructions VM"
  (mapcar (lambda (instr)
            (let ((mnemonic (car instr))
                  (operand (cadr instr)))
              (make-instruction-from-mnemonic mnemonic operand)))
          (loader-instructions loader)))

;;; ----------------------------------------------------------------------------
;;; Fonction principale de chargement
;;; ----------------------------------------------------------------------------

(defun load-asm-file (filename)
  "Charge un fichier ASM et retourne un tableau d'instructions VM"
  (let ((loader (make-loader)))
    ;; Lire toutes les lignes du fichier
    (with-open-file (stream filename :direction :input)
      (let ((lines '()))
        (loop for line = (read-line stream nil)
              while line
              do (push line lines))
        (setf lines (nreverse lines))
        
        ;; Première passe : collecter les labels
        (first-pass loader lines)
        
        ;; Deuxième passe : parser les instructions
        (second-pass loader lines)
        
        ;; Troisième passe : résoudre les labels
        (resolve-labels loader)
        
        ;; Convertir en instructions VM
        (let ((vm-instructions (convert-to-vm-instructions loader)))
          (coerce vm-instructions 'vector))))))

(defun load-asm-string (asm-code)
  "Charge du code ASM depuis une chaîne"
  (let* ((loader (make-loader))
         (lines (split-string asm-code #\Newline)))
    
    ;; Première passe : collecter les labels
    (first-pass loader lines)
    
    ;; Deuxième passe : parser les instructions
    (second-pass loader lines)
    
    ;; Troisième passe : résoudre les labels
    (resolve-labels loader)
    
    ;; Convertir en instructions VM
    (let ((vm-instructions (convert-to-vm-instructions loader)))
      (coerce vm-instructions 'vector))))

;;; ----------------------------------------------------------------------------
;;; Fonction pratique : charger et exécuter
;;; ----------------------------------------------------------------------------

(defun load-and-run-asm-file (filename &key debug)
  "Charge un fichier ASM et l'exécute dans une nouvelle VM"
  (let ((vm (make-vm :debug debug))
        (code (load-asm-file filename)))
    (vm-load-code vm code)
    (format t "Code chargé : ~A instructions~%" (length code))
    (let ((result (vm-run vm)))
      (format t "~%Résultat: ~A~%" result)
      result)))

(defun load-and-run-asm-string (asm-code &key debug)
  "Charge du code ASM depuis une chaîne et l'exécute"
  (let ((vm (make-vm :debug debug))
        (code (load-asm-string asm-code)))
    (vm-load-code vm code)
    (format t "Code chargé : ~A instructions~%" (length code))
    (let ((result (vm-run vm)))
      (format t "~%Résultat: ~A~%" result)
      result)))

;;; ----------------------------------------------------------------------------
;;; Fonction utilitaire : sauvegarder du code ASM
;;; ----------------------------------------------------------------------------

(defun save-asm-file (filename instructions-list)
  "Sauvegarde une liste d'instructions sous forme de texte ASM"
  (with-open-file (stream filename 
                          :direction :output 
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (instr instructions-list)
      (format stream "~A~%" instr))))

;;; ----------------------------------------------------------------------------
;;; Affichage des labels
;;; ----------------------------------------------------------------------------

(defun print-labels (loader)
  "Affiche tous les labels et leurs adresses"
  (format t "~%=== Labels ===~%")
  (maphash (lambda (label address)
             (format t "~A: ~A~%" label address))
           (loader-labels loader))
  (format t "==============~%"))

;;; ----------------------------------------------------------------------------
;;; Export des symboles principaux
;;; ----------------------------------------------------------------------------

(export '(load-asm-file load-asm-string 
          load-and-run-asm-file load-and-run-asm-string
          save-asm-file))
