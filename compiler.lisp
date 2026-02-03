;;;; Compilateur Lisp vers ASM
;;;; Compile du code Lisp en instructions assembleur pour la VM

(defpackage :lisp-compiler
  (:use :common-lisp)
  (:export :compile-to-asm
           :compile-file-to-asm
           :print-asm
           :*debug-mode*))

(in-package :lisp-compiler)

(defvar *debug-mode* nil "Active les messages de débogage")
(defvar *label-counter* 0 "Compteur pour générer des labels uniques")
(defvar *current-env* nil "Environnement de compilation actuel")

;;; Structure pour représenter l'environnement de compilation
(defstruct env
  (vars nil)      ; Liste des variables locales
  (functions nil) ; Liste des fonctions locales
  (parent nil))   ; Environnement parent

;;; Génération de labels uniques
(defun gen-label (prefix)
  "Génère un label unique avec le préfixe donné"
  (format nil "~A_~D" prefix (incf *label-counter*)))

;;; Recherche de variable dans l'environnement
(defun lookup-var (var env)
  "Cherche une variable dans l'environnement et retourne son offset"
  (labels ((search-env (e depth offset)
             (cond
               ((null e) nil)
               ((member var (env-vars e))
                (list depth (position var (env-vars e))))
               (t (search-env (env-parent e) (1+ depth) 0)))))
    (search-env env 0 0)))

;;; Ajout de variables à l'environnement
(defun add-vars (vars env)
  "Ajoute des variables à l'environnement"
  (make-env :vars (append vars (env-vars env))
            :functions (env-functions env)
            :parent (env-parent env)))

;;; Création d'un nouvel environnement
(defun new-env (&optional parent)
  "Crée un nouvel environnement avec un parent optionnel"
  (make-env :parent parent))

;;; Instructions ASM
(defun emit-push (value)
  `(PUSH ,value))

(defun emit-pop ()
  `(POP))

(defun emit-load (depth offset)
  `(LOAD ,depth ,offset))

(defun emit-store (depth offset)
  `(STORE ,depth ,offset))

(defun emit-call (label nargs)
  `(CALL ,label ,nargs))

(defun emit-return ()
  `(RETURN))

(defun emit-label (label)
  `(LABEL ,label))

(defun emit-jump (label)
  `(JUMP ,label))

(defun emit-jump-if-nil (label)
  `(JUMPNIL ,label))

(defun emit-add ()
  `(ADD))

(defun emit-sub ()
  `(SUB))

(defun emit-mul ()
  `(MUL))

(defun emit-div ()
  `(DIV))

(defun emit-eq ()
  `(EQ))

(defun emit-lt ()
  `(LT))

(defun emit-le ()
  `(LE))

(defun emit-gt ()
  `(GT))

(defun emit-ge ()
  `(GE))

(defun emit-print ()
  `(PRINT))

(defun emit-halt ()
  `(HALT))

(defun emit-makeframe (nvars)
  `(MAKEFRAME ,nvars))

(defun emit-popframe ()
  `(POPFRAME))

(defun emit-makeclosure (label nvars)
  `(MAKECLOSURE ,label ,nvars))

;;; Compilation des expressions
(defun compile-expr (expr env)
  "Compile une expression Lisp en instructions ASM"
  (cond
    ;; Constantes
    ((numberp expr)
     (list (emit-push expr)))
    
    ((null expr)
     (list (emit-push 'NIL)))
    
    ((eq expr t)
     (list (emit-push 'T)))
    
    ;; Symboles (variables)
    ((symbolp expr)
     (let ((var-info (lookup-var expr env)))
       (if var-info
           (list (emit-load (first var-info) (second var-info)))
           (error "Variable non définie: ~A" expr))))
    
    ;; Listes (appels de fonction ou formes spéciales)
    ((listp expr)
     (compile-list expr env))
    
    (t (error "Expression non reconnue: ~A" expr))))

(defun compile-list (expr env)
  "Compile une liste (appel de fonction ou forme spéciale)"
  (let ((op (car expr)))
    (cond
      ;; Formes spéciales
      ((eq op 'quote)
       (compile-quote (cadr expr) env))
      
      ((eq op 'if)
       (compile-if (cadr expr) (caddr expr) (cadddr expr) env))
      
      ((eq op 'let)
       (compile-let (cadr expr) (cddr expr) env))
      
      ((eq op 'defun)
       (compile-defun (cadr expr) (caddr expr) (cadddr expr) env))
      
      ((eq op 'lambda)
       (compile-lambda (cadr expr) (cddr expr) env))
      
      ((eq op 'labels)
       (compile-labels (cadr expr) (cddr expr) env))
      
      ((eq op 'loop)
       (compile-loop (cdr expr) env))
      
      ((eq op 'return)
       (append (compile-expr (cadr expr) env)
               (list (emit-return))))
      
      ;; Opérateurs arithmétiques
      ((member op '(+ - * /))
       (compile-binop op (cadr expr) (caddr expr) env))
      
      ;; Opérateurs de comparaison
      ((member op '(= < <= > >=))
       (compile-comparison op (cadr expr) (caddr expr) env))
      
      ;; Print
      ((eq op 'print)
       (append (compile-expr (cadr expr) env)
               (list (emit-print))))
      
      ;; Funcall - appel d'une fermeture
      ((eq op 'funcall)
       (compile-funcall-closure (cadr expr) (cddr expr) env))
      
      ;; Appel de fonction
      (t (compile-funcall op (cdr expr) env)))))

(defun compile-quote (expr env)
  "Compile une expression quotée"
  (declare (ignore env))
  (list (emit-push expr)))

(defun compile-if (test then else env)
  "Compile une expression if"
  (let ((else-label (gen-label "ELSE"))
        (end-label (gen-label "ENDIF")))
    (append
     (compile-expr test env)
     (list (emit-jump-if-nil else-label))
     (compile-expr then env)
     (list (emit-jump end-label))
     (list (emit-label else-label))
     (if else
         (compile-expr else env)
         (list (emit-push 'NIL)))
     (list (emit-label end-label)))))

(defun compile-let (bindings body env)
  "Compile une expression let"
  (let* ((vars (mapcar #'car bindings))
         (vals (mapcar #'cadr bindings))
         (new-env (add-vars vars env))
         (nvars (length vars)))
    (append
     (list (emit-makeframe nvars))
     ;; Compiler les valeurs et les stocker
     (loop for val in vals
           for i from 0
           append (append (compile-expr val env)
                         (list (emit-store 0 i))))
     ;; Compiler le corps
     (loop for expr in body
           append (compile-expr expr new-env))
     (list (emit-popframe)))))

(defun compile-defun (name params body env)
  "Compile une définition de fonction"
  (let* ((label (format nil "FN_~A" name))
         (end-label (gen-label "END_DEFUN"))
         (new-env (add-vars params (new-env env)))
         (compiled-body (compile-expr body new-env)))
    (append
     ;; Sauter par-dessus le code de la fonction
     (list (emit-jump end-label))
     ;; Début de la fonction
     (list (emit-label label))
     compiled-body
     (list (emit-return))
     ;; Fin de la définition
     (list (emit-label end-label))
     ;; Retourner NIL pour la définition elle-même
     (list (emit-push 'NIL)))))

(defun compile-lambda (params body env)
  "Compile une expression lambda (fermeture)"
  (let* ((label (gen-label "LAMBDA"))
         (skip-label (gen-label "SKIP_LAMBDA"))
         ;; Identifier les variables libres
         (free-vars (find-free-vars body params env))
         (nfree (length free-vars))
         ;; Créer un nouvel environnement avec variables libres + paramètres
         (new-env (add-vars (append free-vars params) (new-env env))))
    (append
     ;; Pousser les valeurs des variables libres sur la pile
     (loop for var in free-vars
           append (compile-expr var env))
     ;; Créer la fermeture avec le label et le nombre de variables capturées
     (list (emit-makeclosure label nfree))
     ;; Sauter par-dessus le code de la fonction
     (list (emit-jump skip-label))
     ;; Code de la fonction
     (list (emit-label label))
     ;; Compiler le corps avec le nouvel environnement
     (loop for expr in body
           append (compile-expr expr new-env))
     (list (emit-return))
     (list (emit-label skip-label)))))

(defun find-free-vars (body params env)
  "Trouve les variables libres dans le corps d'une lambda"
  (labels ((find-vars (expr)
             (cond
               ((symbolp expr)
                (if (and (not (member expr params))
                        (lookup-var expr env))
                    (list expr)
                    nil))
               ((atom expr) nil)
               ((eq (car expr) 'quote) nil)
               ((eq (car expr) 'lambda)
                ;; Ne pas descendre dans les lambdas imbriquées
                nil)
               (t (remove-duplicates
                   (apply #'append (mapcar #'find-vars expr)))))))
    (remove-duplicates (apply #'append (mapcar #'find-vars body)))))

(defun compile-labels (bindings body env)
  "Compile des fonctions locales avec labels"
  (let* ((fn-names (mapcar #'car bindings))
         (new-env (make-env :functions fn-names :parent env)))
    (append
     ;; Compiler chaque fonction locale
     (loop for (name params . fn-body) in bindings
           append (compile-defun name params (car fn-body) new-env))
     ;; Compiler le corps
     (loop for expr in body
           append (compile-expr expr new-env)))))

(defun compile-loop (body env)
  "Compile une boucle loop"
  (let ((start-label (gen-label "LOOP_START"))
        (end-label (gen-label "LOOP_END")))
    (append
     (list (emit-label start-label))
     (loop for expr in body
           append (compile-expr expr env))
     (list (emit-jump start-label))
     (list (emit-label end-label)))))

(defun compile-binop (op arg1 arg2 env)
  "Compile une opération binaire"
  (append
   (compile-expr arg1 env)
   (compile-expr arg2 env)
   (list (case op
           (+ (emit-add))
           (- (emit-sub))
           (* (emit-mul))
           (/ (emit-div))))))

(defun compile-comparison (op arg1 arg2 env)
  "Compile une comparaison"
  (append
   (compile-expr arg1 env)
   (compile-expr arg2 env)
   (list (case op
           (= (emit-eq))
           (< (emit-lt))
           (<= (emit-le))
           (> (emit-gt))
           (>= (emit-ge))))))

(defun compile-funcall (fn args env)
  "Compile un appel de fonction"
  (let ((nargs (length args)))
    (append
     ;; Compiler les arguments (de droite à gauche)
     (loop for arg in (reverse args)
           append (compile-expr arg env))
     ;; Appeler la fonction
     (list (emit-call (if (symbolp fn)
                          (format nil "FN_~A" fn)
                          fn)
                      nargs)))))

(defun compile-funcall-closure (closure-expr args env)
  "Compile un appel de fermeture via funcall"
  (let ((nargs (length args)))
    (append
     ;; Compiler les arguments
     (loop for arg in (reverse args)
           append (compile-expr arg env))
     ;; Compiler l'expression de fermeture
     (compile-expr closure-expr env)
     ;; Appeler la fermeture
     (list (emit-callclosure nargs)))))

(defun emit-callclosure (nargs)
  `(CALLCLOSURE ,nargs))

;;; Interface principale
(defun compile-to-asm (expr)
  "Compile une expression Lisp en ASM"
  (setf *label-counter* 0)
  (let ((env (new-env)))
    (append (compile-expr expr env)
            (list (emit-halt)))))

(defun compile-file-to-asm (input-file output-file)
  "Compile un fichier Lisp en fichier ASM"
  (with-open-file (in input-file :direction :input)
    (with-open-file (out output-file :direction :output
                         :if-exists :supersede)
      (loop for expr = (read in nil :eof)
            until (eq expr :eof)
            do (let ((asm (compile-to-asm expr)))
                 (dolist (instr asm)
                   (print instr out)))))))

(defun print-asm (instructions)
  "Affiche les instructions ASM de manière lisible"
  (dolist (instr instructions)
    (format t "~A~%" instr)))
