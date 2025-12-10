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
  (in-function nil)            ; Indique si on compile dans une fonction (pour LOADARG)
  (n-params 0)                 ; Nombre de paramètres de la fonction courante
  (closure-vars '()))          ; Variables capturées dans une fermeture ((nom . index) ...)

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
     (let ((closure-index (cdr (assoc expr (compiler-env-closure-vars env)))))
       (if closure-index
           ;; Variable capturée - utiliser LOADCLOSURE
           (list (format nil "LOADCLOSURE ~A" closure-index))
           ;; Variable normale
           (let ((index (env-lookup-var env expr)))
             (if index
                 ;; Distinguer entre paramètres (LOADARG) et variables locales (LOAD)
                 (if (and (compiler-env-in-function env) 
                          (< index (compiler-env-n-params env)))
                     ;; C'est un paramètre → LOADARG
                     (list (format nil "LOADARG ~A" index))
                     ;; C'est une variable locale → LOAD (ajuster l'index)
                     (if (compiler-env-in-function env)
                         (list (format nil "LOAD ~A" (1+ (- index (compiler-env-n-params env)))))
                         (list (format nil "LOAD ~A" index))))
                 (error "Variable non définie: ~A" expr))))))
    
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
       
       ;; Opérations sur les listes
       ((member (car expr) '(cons car cdr null list))
        (compile-list-op expr env))
       
       ;; Prédicats
       ((member (car expr) '(null? listp symbolp eq))
        (compile-predicate expr env))
       
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
       
       ;; LAMBDA (fonction anonyme / fermeture)
       ((eq (car expr) 'lambda)
        (compile-lambda expr env))
       
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

(defun compile-list-op (expr env)
  "Compile une opération sur les listes: (cons, car, cdr, null, list)"
  (let ((op (car expr))
        (args (cdr expr)))
    (case op
      (cons
       (unless (= (length args) 2)
         (error "CONS nécessite exactement 2 arguments"))
       (append (compile-expr (car args) env)
               (compile-expr (cadr args) env)
               (list "CONS")))
      
      (car
       (unless (= (length args) 1)
         (error "CAR nécessite exactement 1 argument"))
       (append (compile-expr (car args) env)
               (list "CAR")))
      
      (cdr
       (unless (= (length args) 1)
         (error "CDR nécessite exactement 1 argument"))
       (append (compile-expr (car args) env)
               (list "CDR")))
      
      (null
       (unless (= (length args) 1)
         (error "NULL nécessite exactement 1 argument"))
       (append (compile-expr (car args) env)
               (list "NULLP")))
      
      (list
       ;; (list a b c) → (cons a (cons b (cons c nil)))
       (if (null args)
           (list "PUSH 0")  ; Liste vide représentée par 0 (NIL)
           (let ((result (compile-expr (car (last args)) env)))
             (setf result (append result (list "PUSH 0" "CONS")))
             (dolist (arg (reverse (butlast args)))
               (setf result (append (compile-expr arg env) result (list "CONS"))))
             result)))
      
      (t (error "Opération de liste non supportée: ~A" op)))))

(defun compile-predicate (expr env)
  "Compile un prédicat: (null?, listp, symbolp, eq)"
  (let ((op (car expr))
        (args (cdr expr)))
    (case op
      (null?
       (unless (= (length args) 1)
         (error "NULL? nécessite exactement 1 argument"))
       (append (compile-expr (car args) env)
               (list "NULLP")))
      
      (listp
       (unless (= (length args) 1)
         (error "LISTP nécessite exactement 1 argument"))
       (append (compile-expr (car args) env)
               (list "LISTP")))
      
      (symbolp
       (unless (= (length args) 1)
         (error "SYMBOLP nécessite exactement 1 argument"))
       (append (compile-expr (car args) env)
               (list "SYMBOLP")))
      
      (eq
       (unless (= (length args) 2)
         (error "EQ nécessite exactement 2 arguments"))
       (append (compile-expr (car args) env)
               (compile-expr (cadr args) env)
               (list "EQSYM")))
      
      (t (error "Prédicat non supporté: ~A" op)))))

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
    (let ((closure-index (cdr (assoc var-name (compiler-env-closure-vars env))))
          (var-index (env-lookup-var env var-name)))
      (cond
        (closure-index
         ;; Variable capturée dans une closure → STORECLOSURE
         (append (compile-expr value env)
                 (list (format nil "STORECLOSURE ~A" closure-index))))
        (var-index
         ;; Variable normale → STORE avec index ajusté
         (append (compile-expr value env)
                 (if (and (compiler-env-in-function env)
                          (< var-index (compiler-env-n-params env)))
                     ;; Paramètre - erreur car on ne peut pas modifier un paramètre avec STORE
                     (error "Impossible de modifier un paramètre avec setq: ~A" var-name)
                     ;; Variable locale
                     (if (compiler-env-in-function env)
                         (list (format nil "STORE ~A" (1+ (- var-index (compiler-env-n-params env)))))
                         (list (format nil "STORE ~A" var-index))))))
        (t
         (error "Variable non définie: ~A" var-name))))))

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
         (new-env (make-compiler-env :in-function t :n-params (length params))))  ; Activer le mode fonction ET spécifier le nombre de paramètres
    
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
;;; Compilation de LAMBDA (fermetures)
;;; ----------------------------------------------------------------------------

(defun compile-lambda (expr env)
  "Compile une fonction lambda (fermeture): (lambda (params...) body...)"
  (let* ((params (cadr expr))
         (body (cddr expr))
         (lambda-label (generate-label env "LAMBDA"))
         (end-label (generate-label env "END_LAMBDA"))
         (new-env (make-compiler-env :in-function t
                                     :n-params (length params)
                                     :label-counter (compiler-env-label-counter env))))
    
    ;; Partager le compteur de labels
    (setf (compiler-env-label-counter env) (compiler-env-label-counter new-env))
    
    ;; Copier les fonctions définies dans le nouvel environnement
    (maphash (lambda (k v) 
               (setf (gethash k (compiler-env-functions new-env)) v))
             (compiler-env-functions env))
    
    ;; Les paramètres sont accessibles via LOADARG
    (let ((param-index 0))
      (dolist (param params)
        (push (cons param param-index) (compiler-env-variables new-env))
        (incf param-index)))
    
    ;; Identifier les variables libres (qui doivent être capturées)
    (let ((free-vars (find-free-variables body params (compiler-env-variables env))))
      (append
       ;; Sauter par-dessus la définition de lambda
       (list (format nil "JUMP ~A" end-label))
       ;; Label de la fonction lambda
       (list (format nil "~A:" lambda-label))
       ;; Corps de la lambda (les variables capturées sont accessibles via LOADCLOSURE)
       (compile-lambda-body body new-env free-vars)
       ;; Retour de fonction
       (list "RET")
       ;; Label de fin
       (list (format nil "~A:" end-label))
       ;; Capturer les variables libres sur la pile
       (compile-capture-vars free-vars env)
       ;; Nombre de variables capturées
       (list (format nil "PUSH ~A" (length free-vars)))
       ;; Créer la fermeture
       (list (format nil "MKCLOSURE ~A" lambda-label))))))

(defun find-free-variables (body params env-vars)
  "Trouve les variables libres dans le corps d'une lambda (variables non locales).
   env-vars est la liste des variables de l'environnement englobant ((nom . index) ...)"
  (let ((free-vars '()))
    (labels ((collect-vars (expr)
               (cond
                 ((symbolp expr)
                  ;; Si c'est un symbole, vérifier si c'est une variable libre
                  (when (and (not (member expr params))  ; Pas un paramètre local
                            (assoc expr env-vars)         ; Existe dans l'env englobant
                            (not (member expr free-vars))) ; Pas déjà collecté
                    (push expr free-vars)))
                 ((listp expr)
                  ;; Parcourir récursivement, sauf pour les lambdas internes
                  (unless (and (consp expr) (eq (car expr) 'lambda))
                    (dolist (sub-expr expr)
                      (collect-vars sub-expr)))))))
      (dolist (expr body)
        (collect-vars expr)))
    (nreverse free-vars)))

(defun compile-capture-vars (vars env)
  "Compile le code pour capturer des variables libres"
  (let ((result '()))
    (dolist (var vars)
      ;; Chercher dans les variables ou dans les closures
      (let ((var-index (env-lookup-var env var))
            (closure-index (cdr (assoc var (compiler-env-closure-vars env)))))
        (cond
          (closure-index
           ;; Variable déjà capturée dans une fermeture englobante
           (setf result (append result (list (format nil "LOADCLOSURE ~A" closure-index)))))
          (var-index
           ;; Variable normale - distinguer paramètre vs locale
           (if (and (compiler-env-in-function env)
                    (< var-index (compiler-env-n-params env)))
               ;; C'est un paramètre
               (setf result (append result (list (format nil "LOADARG ~A" var-index))))
               ;; C'est une variable locale
               (if (compiler-env-in-function env)
                   (setf result (append result (list (format nil "LOAD ~A" (1+ (- var-index (compiler-env-n-params env)))))))
                   (setf result (append result (list (format nil "LOAD ~A" var-index)))))))
          (t
           (error "Variable à capturer non trouvée: ~A" var)))))
    result))

(defun compile-lambda-body (body env free-vars)
  "Compile le corps d'une lambda avec accès aux variables capturées"
  ;; Créer un nouvel environnement avec les variables capturées
  (let ((new-env (copy-compiler-env env)))
    ;; Ajouter les variables capturées dans closure-vars
    (let ((closure-index 0))
      (dolist (var free-vars)
        (push (cons var closure-index) (compiler-env-closure-vars new-env))
        (incf closure-index)))
    
    ;; Compiler le corps
    (let ((result '()))
      (dolist (expr body)
        (setf result (append result (compile-expr expr new-env))))
      result)))

;;; ----------------------------------------------------------------------------
;;; Compilation d'appels de fonction
;;; ----------------------------------------------------------------------------

(defun compile-call (expr env)
  "Compile un appel de fonction: (func arg1 arg2 ...) ou ((lambda ...) arg1 arg2 ...)"
  (let ((func-expr (car expr))
        (args (cdr expr)))
    
    (cond
      ;; Appel direct d'une lambda: ((lambda (x) (* x 2)) 5)
      ((and (listp func-expr) (eq (car func-expr) 'lambda))
       (let ((result '()))
         ;; Compiler les arguments
         (dolist (arg args)
           (setf result (append result (compile-expr arg env))))
         
         ;; Ajouter le nombre d'arguments sur la pile
         (setf result (append result (list (format nil "PUSH ~A" (length args)))))
         
         ;; Compiler la lambda (qui produit une fermeture sur la pile)
         (setf result (append result (compile-expr func-expr env)))
         
         ;; La fermeture est maintenant au sommet de la pile, appel avec CALLCLOSURE
         (append result (list "CALLCLOSURE"))))
      
      ;; Appel d'une fonction nommée
      ((symbolp func-expr)
       (let ((func-label (gethash func-expr (compiler-env-functions env))))
         (unless func-label
           (error "Fonction non définie: ~A" func-expr))
         
         (let ((result '()))
           (dolist (arg args)
             (setf result (append result (compile-expr arg env))))
           (setf result (append result (list (format nil "PUSH ~A" (length args)))))
           (append result (list (format nil "CALL ~A" func-label))))))
      
      ;; Cas non supporté
      (t (error "Appel de fonction invalide: ~A" expr)))))

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
   :in-function (compiler-env-in-function env)
   :n-params (compiler-env-n-params env)
   :closure-vars (copy-list (compiler-env-closure-vars env))))

;;; ----------------------------------------------------------------------------
;;; Export des symboles
;;; ----------------------------------------------------------------------------

(export '(compile-lisp compile-lisp-to-string compile-lisp-to-file
          compile-and-run make-compiler-env))

(format t "Compilateur LISP → ASM chargé avec succès!~%")
