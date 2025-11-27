;;;; ============================================================================
;;;; COMPILATEUR LISP → ASM
;;;; ============================================================================
;;;; Compile des expressions LISP vers le langage d'assemblage de la VM

(in-package :cl-user)
(load "instructions.lisp")

;;; ----------------------------------------------------------------------------
;;; Structures de données du compilateur
;;; ----------------------------------------------------------------------------

(defstruct compiler-env
  "Environnement de compilation"
  (variables '())              ; Liste des variables locales ((nom . index) ...)
  (functions (make-hash-table :test 'equal))  ; Table des fonctions
  (label-counter 0)            ; Compteur pour générer des labels uniques
  (current-function nil)       ; Fonction en cours de compilation
  (depth 0)                    ; Profondeur de la pile locale
  (in-function nil))           ; Indique si on compile dans une fonction (pour LOADARG)

;;; ----------------------------------------------------------------------------
;;; Génération de labels uniques
;;; ----------------------------------------------------------------------------

(defun generate-label (env prefix)
  "Génère un label unique avec un préfixe"
  (let ((counter (compiler-env-label-counter env)))
    (setf (compiler-env-label-counter env) (1+ counter))
    (intern (format nil "~A_~A" prefix counter) :cl-user)))

;;; ----------------------------------------------------------------------------
;;; Gestion de l'environnement
;;; ----------------------------------------------------------------------------

(defun env-lookup-var (env var-name)
  "Cherche une variable dans l'environnement, retourne son index ou NIL"
  (cdr (assoc var-name (compiler-env-variables env))))

(defun env-add-var (env var-name)
  "Ajoute une variable à l'environnement et retourne son index"
  (let ((index (length (compiler-env-variables env))))
    (push (cons var-name index) (compiler-env-variables env))
    index))

(defun env-add-vars (env var-names)
  "Ajoute plusieurs variables à l'environnement"
  (dolist (var var-names)
    (env-add-var env var)))

;;; ----------------------------------------------------------------------------
;;; ÉTAPE 7 : Compilation des expressions de base
;;; ----------------------------------------------------------------------------

(defun compile-expr (expr env)
  "Compile une expression LISP en une liste d'instructions ASM"
  (cond
    ;; Nombre
    ((numberp expr)
     (list (format nil "PUSH ~A" expr)))
    
    ;; Variable
    ((symbolp expr)
     (let ((index (env-lookup-var env expr)))
       (if index
           ;; Si on est dans une fonction, utiliser LOADARG pour les paramètres
           (if (compiler-env-in-function env)
               (list (format nil "LOADARG ~A" index))
               (list (format nil "LOAD ~A" index)))
           (error "Variable non définie: ~A" expr))))
    
    ;; Liste (appel de fonction ou forme spéciale)
    ((listp expr)
     (cond
       ;; Expression vide
       ((null expr) 
        (list "PUSH 0"))
       
       ;; Opérations arithmétiques
       ((member (car expr) '(+ - * / mod))
        (compile-arithmetic expr env))
       
       ;; Opérations de comparaison
       ((member (car expr) '(= < <= > >=))
        (compile-comparison expr env))
       
       ;; Structure IF
       ((eq (car expr) 'if)
        (compile-if expr env))
       
       ;; LET (variables locales)
       ((eq (car expr) 'let)
        (compile-let expr env))
       
       ;; PROGN (séquence d'expressions)
       ((eq (car expr) 'progn)
        (compile-progn expr env))
       
       ;; SETQ (affectation)
       ((eq (car expr) 'setq)
        (compile-setq expr env))
       
       ;; DEFUN (définition de fonction)
       ((eq (car expr) 'defun)
        (compile-defun expr env))
       
       ;; Appel de fonction
       (t (compile-call expr env))))
    
    ;; Type inconnu
    (t (error "Type d'expression non supporté: ~A" expr))))

;;; ----------------------------------------------------------------------------
;;; Compilation des opérations arithmétiques
;;; ----------------------------------------------------------------------------

(defun compile-arithmetic (expr env)
  "Compile une opération arithmétique: (+, -, *, /, mod)"
  (let ((op (car expr))
        (args (cdr expr)))
    (cond
      ;; Pas d'arguments
      ((null args)
       (error "Opération ~A sans arguments" op))
      
      ;; Un seul argument
      ((null (cdr args))
       (case op
         (+ (compile-expr (car args) env))
         (- (append (compile-expr (car args) env)
                   (list "PUSH 0"
                         "PUSH 0"
                         "ROT"
                         "SUB")))
         (* (compile-expr (car args) env))
         (/ (error "Division avec un seul argument non supportée"))
         (mod (error "Modulo avec un seul argument non supporté"))))
      
      ;; Deux arguments ou plus
      (t
       (let ((result (compile-expr (car args) env)))
         (dolist (arg (cdr args))
           (setf result (append result 
                               (compile-expr arg env)
                               (list (arithmetic-op-to-instruction op)))))
         result)))))

(defun arithmetic-op-to-instruction (op)
  "Convertit un opérateur arithmétique en instruction"
  (case op
    (+ "ADD")
    (- "SUB")
    (* "MUL")
    (/ "DIV")
    (mod "MOD")
    (t (error "Opérateur arithmétique inconnu: ~A" op))))

;;; ----------------------------------------------------------------------------
;;; Compilation des opérations de comparaison
;;; ----------------------------------------------------------------------------

(defun compile-comparison (expr env)
  "Compile une opération de comparaison: (=, <, <=, >, >=)"
  (let ((op (car expr))
        (args (cdr expr)))
    (unless (= (length args) 2)
      (error "L'opération ~A nécessite exactement 2 arguments" op))
    
    (append (compile-expr (car args) env)
            (compile-expr (cadr args) env)
            (list (comparison-op-to-instruction op)))))

(defun comparison-op-to-instruction (op)
  "Convertit un opérateur de comparaison en instruction"
  (case op
    (= "EQ")
    (< "LT")
    (<= "LE")
    (> "GT")
    (>= "GE")
    (t (error "Opérateur de comparaison inconnu: ~A" op))))

;;; ----------------------------------------------------------------------------
;;; ÉTAPE 8 : Compilation de IF
;;; ----------------------------------------------------------------------------

(defun compile-if (expr env)
  "Compile une expression IF: (if test then else)"
  (let ((test (cadr expr))
        (then-expr (caddr expr))
        (else-expr (cadddr expr))
        (label-else (generate-label env "ELSE"))
        (label-end (generate-label env "ENDIF")))
    
    (append
     ;; Compiler le test
     (compile-expr test env)
     ;; Sauter vers ELSE si faux (sommet == 0)
     (list (format nil "JUMPNIF ~A" label-else))
     ;; Compiler la branche THEN
     (compile-expr then-expr env)
     ;; Sauter vers la fin
     (list (format nil "JUMP ~A" label-end))
     ;; Label ELSE
     (list (format nil "~A:" label-else))
     ;; Compiler la branche ELSE (ou PUSH 0 si absent)
     (if else-expr
         (compile-expr else-expr env)
         (list "PUSH 0"))
     ;; Label END
     (list (format nil "~A:" label-end)))))

;;; ----------------------------------------------------------------------------
;;; ÉTAPE 8 : Compilation de LET
;;; ----------------------------------------------------------------------------

(defun compile-let (expr env)
  "Compile une expression LET: (let ((var1 val1) (var2 val2) ...) body...)"
  (let* ((bindings (cadr expr))
         (body (cddr expr))
         (new-env (copy-compiler-env env))
         (n-vars (length bindings)))
    
    ;; Allouer l'espace pour les variables locales
    (let ((result (if (> n-vars 0)
                      (list (format nil "ALLOC ~A" n-vars))
                      '())))
      
      ;; Compiler chaque binding et stocker dans les variables
      (dolist (binding bindings)
        (let* ((var-name (car binding))
               (var-value (cadr binding))
               (var-index (env-add-var new-env var-name)))
          (setf result (append result
                              (compile-expr var-value env)
                              (list (format nil "STORE ~A" var-index))))))
      
      ;; Compiler le corps
      (dolist (expr-body body)
        (setf result (append result (compile-expr expr-body new-env))))
      
      ;; Désallouer les variables locales
      (when (> n-vars 0)
        (setf result (append result (list (format nil "DEALLOC ~A" n-vars)))))
      
      result)))

;;; ----------------------------------------------------------------------------
;;; Compilation de PROGN
;;; ----------------------------------------------------------------------------

(defun compile-progn (expr env)
  "Compile une séquence d'expressions PROGN"
  (let ((result '())
        (body (cdr expr))
        (shared-env env))  ; Partager l'environnement entre toutes les expressions
    (dolist (e body)
      ;; Utiliser shared-env pour que les fonctions définies soient disponibles
      (setf result (append result (compile-expr e shared-env))))
    result))

;;; ----------------------------------------------------------------------------
;;; Compilation de SETQ
;;; ----------------------------------------------------------------------------

(defun compile-setq (expr env)
  "Compile une affectation SETQ: (setq var value)"
  (let ((var-name (cadr expr))
        (value (caddr expr)))
    (let ((index (env-lookup-var env var-name)))
      (unless index
        (error "Variable non définie: ~A" var-name))
      (append (compile-expr value env)
              (list (format nil "STORE ~A" index))))))

;;; ----------------------------------------------------------------------------
;;; ÉTAPE 9 : Compilation des fonctions (DEFUN)
;;; ----------------------------------------------------------------------------

(defun compile-defun (expr env)
  "Compile une définition de fonction: (defun nom (params...) body...)"
  (let* ((func-name (cadr expr))
         (params (caddr expr))
         (body (cdddr expr))
         (func-label (intern (format nil "FUNC_~A" func-name) :cl-user))
         (end-label (generate-label env "END_DEFUN"))
         (new-env (make-compiler-env :in-function t)))  ; Activer le mode fonction
    
    ;; Enregistrer la fonction dans l'environnement AVANT de compiler le corps
    ;; pour supporter la récursivité
    (setf (gethash func-name (compiler-env-functions env)) func-label)
    
    ;; Copier aussi les fonctions déjà définies dans le nouvel environnement
    ;; pour supporter les appels entre fonctions
    (maphash (lambda (k v) 
               (setf (gethash k (compiler-env-functions new-env)) v))
             (compiler-env-functions env))
    
    ;; Ajouter les paramètres à l'environnement de la fonction
    ;; Les paramètres sont accessibles via LOADARG
    (let ((param-index 0))
      (dolist (param params)
        (push (cons param param-index) (compiler-env-variables new-env))
        (incf param-index)))
    
    (append
     ;; Sauter par-dessus la définition de fonction
     (list (format nil "JUMP ~A" end-label))
     ;; Label de la fonction
     (list (format nil "~A:" func-label))
     ;; Compiler le corps de la fonction
     (compile-function-body body new-env)
     ;; Retour de fonction
     (list "RET")
     ;; Label de fin
     (list (format nil "~A:" end-label)))))

(defun compile-function-body (body env)
  "Compile le corps d'une fonction"
  (let ((result '()))
    (dolist (expr body)
      (setf result (append result (compile-expr expr env))))
    result))

;;; ----------------------------------------------------------------------------
;;; Compilation d'appels de fonction
;;; ----------------------------------------------------------------------------

(defun compile-call (expr env)
  "Compile un appel de fonction: (func arg1 arg2 ...)"
  (let ((func-name (car expr))
        (args (cdr expr)))
    
    ;; Vérifier que la fonction existe
    (let ((func-label (gethash func-name (compiler-env-functions env))))
      (unless func-label
        (error "Fonction non définie: ~A" func-name))
      
      ;; Compiler les arguments dans l'ordre inverse (ils seront dépilés dans l'ordre)
      (let ((result '()))
        (dolist (arg (reverse args))
          (setf result (append result (compile-expr arg env))))
        
        ;; Ajouter le nombre d'arguments sur la pile
        (setf result (append result (list (format nil "PUSH ~A" (length args)))))
        
        ;; Appeler la fonction
        (append result (list (format nil "CALL ~A" func-label)))))))

;;; ----------------------------------------------------------------------------
;;; Fonction principale de compilation
;;; ----------------------------------------------------------------------------

(defun compile-lisp (expr &optional (env (make-compiler-env)))
  "Compile une expression LISP complète et retourne du code ASM"
  (let ((instructions (compile-expr expr env)))
    ;; Ajouter HALT à la fin
    (append instructions (list "HALT"))))

(defun compile-lisp-to-string (expr)
  "Compile une expression LISP et retourne une chaîne ASM"
  (let ((instructions (compile-lisp expr)))
    (format nil "~{~A~%~}" instructions)))

(defun compile-lisp-to-file (expr filename)
  "Compile une expression LISP et sauvegarde dans un fichier ASM"
  (with-open-file (stream filename 
                          :direction :output 
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (instr (compile-lisp expr))
      (format stream "~A~%" instr))))

;;; ----------------------------------------------------------------------------
;;; Fonction pratique : compiler et exécuter
;;; ----------------------------------------------------------------------------

(defun compile-and-run (expr &key debug)
  "Compile une expression LISP et l'exécute directement"
  (load "loader.lisp")
  (let ((asm-code (compile-lisp-to-string expr)))
    (format t "=== Code ASM généré ===~%~A~%=====================~%~%" asm-code)
    (load-and-run-asm-string asm-code :debug debug)))

;;; ----------------------------------------------------------------------------
;;; Utilitaire pour copier l'environnement
;;; ----------------------------------------------------------------------------

(defun copy-compiler-env (env)
  "Copie un environnement de compilation"
  (make-compiler-env
   :variables (copy-list (compiler-env-variables env))
   :functions (let ((new-table (make-hash-table :test 'equal)))
                (maphash (lambda (k v) (setf (gethash k new-table) v))
                        (compiler-env-functions env))
                new-table)
   :label-counter (compiler-env-label-counter env)
   :current-function (compiler-env-current-function env)
   :depth (compiler-env-depth env)
   :in-function (compiler-env-in-function env)))

;;; ----------------------------------------------------------------------------
;;; Export des symboles
;;; ----------------------------------------------------------------------------

(export '(compile-lisp compile-lisp-to-string compile-lisp-to-file
          compile-and-run make-compiler-env))

(format t "Compilateur LISP → ASM chargé avec succès!~%")
