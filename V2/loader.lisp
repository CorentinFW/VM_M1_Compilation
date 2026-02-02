;;;; Chargeur ASM
;;;; Charge des instructions ASM dans la mémoire de la VM

(defpackage :asm-loader
  (:use :common-lisp)
  (:export :load-asm-file
           :load-asm-instructions
           :parse-asm-instruction))

(in-package :asm-loader)

(defun parse-asm-instruction (instr)
  "Parse une instruction ASM et retourne une structure de données"
  (cond
    ((not (listp instr))
     (error "Instruction invalide: ~A" instr))
    
    ((null instr)
     nil)
    
    (t
     ;; L'instruction est déjà sous forme de liste
     ;; On peut la retourner telle quelle ou la valider
     (let ((opcode-name (symbol-name (car instr)))
           (operands (cdr instr)))
       (cond
         ;; Instructions de pile
         ((member opcode-name '("PUSH" "POP") :test #'string=) 
          instr)
         
         ;; Instructions de mémoire
         ((member opcode-name '("LOAD" "STORE") :test #'string=)
          (unless (= (length operands) 2)
            (error "LOAD/STORE requiert 2 opérandes: ~A" instr))
          instr)
         
         ;; Instructions de contrôle
         ((string= opcode-name "CALL")
          (unless (= (length operands) 2)
            (error "CALL requiert 2 opérandes (label, nargs): ~A" instr))
          instr)
         
         ((member opcode-name '("JUMP" "JUMPNIL" "LABEL") :test #'string=)
          (unless (= (length operands) 1)
            (error "~A requiert 1 opérande: ~A" opcode-name instr))
          instr)
         
         ((member opcode-name '("RETURN" "HALT") :test #'string=) 
          instr)
         
         ;; Instructions arithmétiques
         ((member opcode-name '("ADD" "SUB" "MUL" "DIV") :test #'string=) 
          instr)
         
         ;; Instructions de comparaison
         ((member opcode-name '("EQ" "LT" "LE" "GT" "GE") :test #'string=) 
          instr)
         
         ;; Instructions d'affichage
         ((string= opcode-name "PRINT") 
          instr)
         
         ;; Gestion des frames
         ((member opcode-name '("MAKEFRAME" "POPFRAME") :test #'string=)
          instr)
         
         ;; Fermetures
         ((string= opcode-name "MAKECLOSURE")
          (unless (= (length operands) 2)
            (error "MAKECLOSURE requiert 2 opérandes: ~A" instr))
          instr)
         
         (t (error "Opcode inconnu: ~A" opcode-name)))))))

(defun load-asm-instructions (instructions)
  "Charge une liste d'instructions ASM et retourne un programme validé"
  (let ((program nil)
        (labels (make-hash-table :test 'equal)))
    ;; Premier passage: identifier les labels et construire le programme
    (loop for instr in instructions
          for original-pc from 0
          with actual-pc = 0
          do (when (listp instr)
               (if (and (symbolp (car instr))
                       (string= (symbol-name (car instr)) "LABEL"))
                   ;; Stocker l'adresse du label (position de la PROCHAINE instruction)
                   (setf (gethash (cadr instr) labels) actual-pc)
                   ;; Parser et ajouter l'instruction au programme
                   (progn
                     (let ((parsed (parse-asm-instruction instr)))
                       (when parsed
                         (push parsed program)))
                     (incf actual-pc)))))
    
    (values (nreverse program) labels)))

(defun load-asm-file (filename)
  "Charge un fichier ASM et retourne le programme et la table des labels"
  (with-open-file (in filename :direction :input)
    (let ((instructions nil))
      (loop for instr = (read in nil :eof)
            until (eq instr :eof)
            do (push instr instructions))
      (load-asm-instructions (nreverse instructions)))))

(defun print-program (program)
  "Affiche un programme de manière formatée"
  (loop for instr in program
        for i from 0
        do (format t "~4D: ~A~%" i instr)))

(defun save-asm-to-file (instructions filename)
  "Sauvegarde des instructions ASM dans un fichier"
  (with-open-file (out filename :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (dolist (instr instructions)
      (print instr out))))
