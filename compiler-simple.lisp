;;;; ============================================================================
;;;; COMPILATEUR LISP → ASM (VERSION SIMPLIFIÉE AUTO-COMPILABLE)
;;;; ============================================================================
;;;; Version utilisant uniquement des primitives LISP de base
;;;; Peut se compiler elle-même avec le compilateur

(load "instructions.lisp")

;;; ----------------------------------------------------------------------------
;;; Structures de données (remplacer defstruct par alists)
;;; ----------------------------------------------------------------------------

;;; Environnement représenté comme une alist:
;;; ((:variables . ((nom . index) ...))
;;;  (:functions . ((nom . label) ...))
;;;  (:label-counter . N)
;;;  (:current-function . nom)
;;;  (:depth . N)
;;;  (:in-function . T/NIL)
;;;  (:n-params . N)
;;;  (:closure-vars . ((nom . index) ...)))

(defun make-compiler-env (&key variables functions label-counter current-function 
                               depth in-function n-params closure-vars)
  "Crée un nouvel environnement de compilation"
  (list (cons ':variables (if variables variables '()))
        (cons ':functions (if functions functions '()))
        (cons ':label-counter (if label-counter label-counter 0))
        (cons ':current-function current-function)
        (cons ':depth (if depth depth 0))
        (cons ':in-function in-function)
        (cons ':n-params (if n-params n-params 0))
        (cons ':closure-vars (if closure-vars closure-vars '()))))

(defun env-get (env key)
  "Récupère une valeur de l'environnement"
  (cdr (assoc key env)))

(defun env-set (env key value)
  "Modifie une valeur de l'environnement (destructif)"
  (let ((pair (assoc key env)))
    (if pair
        (progn (setq pair (cons key value))
               (let ((result '()))
                 (let ((scan env))
                   (labels ((rebuild (lst)
                              (if (null lst)
                                  '()
                                  (let ((first-elem (car lst)))
                                    (if (eq (car first-elem) key)
                                        (cons pair (rebuild (cdr lst)))
                                        (cons first-elem (rebuild (cdr lst))))))))
                     (rebuild env)))))
        (cons (cons key value) env))))

(defun compiler-env-variables (env) (env-get env ':variables))
(defun compiler-env-functions (env) (env-get env ':functions))
(defun compiler-env-label-counter (env) (env-get env ':label-counter))
(defun compiler-env-current-function (env) (env-get env ':current-function))
(defun compiler-env-depth (env) (env-get env ':depth))
(defun compiler-env-in-function (env) (env-get env ':in-function))
(defun compiler-env-n-params (env) (env-get env ':n-params))
(defun compiler-env-closure-vars (env) (env-get env ':closure-vars))

;;; ----------------------------------------------------------------------------
;;; Utilitaires de base (remplacer format, append, etc.)
;;; ----------------------------------------------------------------------------

(defun int-to-string (n)
  "Convertit un entier en chaîne"
  (if (= n 0)
      "0"
      (let ((digits '())
            (negative (< n 0))
            (num (if (< n 0) (- 0 n) n)))
        (labels ((collect-digits (x)
                   (if (= x 0)
                       digits
                       (progn
                         (setq digits (cons (+ 48 (mod x 10)) digits))
                         (collect-digits (/ x 10))))))
          (collect-digits num))
        (let ((str-list (if negative (cons 45 digits) digits)))
          (coerce str-list 'string)))))

(defun symbol-to-string (sym)
  "Convertit un symbole en chaîne"
  (symbol-name sym))

(defun string-concat (s1 s2)
  "Concatène deux chaînes"
  (let ((list1 (coerce s1 'list))
        (list2 (coerce s2 'list)))
    (coerce (my-append list1 list2) 'string)))

(defun string-concat-3 (s1 s2 s3)
  "Concatène trois chaînes"
  (string-concat (string-concat s1 s2) s3))

(defun my-append (list1 list2)
  "Concatène deux listes récursivement"
  (if (null list1)
      list2
      (cons (car list1) (my-append (cdr list1) list2))))

(defun my-reverse (lst)
  "Inverse une liste"
  (labels ((rev-helper (l acc)
             (if (null l)
                 acc
                 (rev-helper (cdr l) (cons (car l) acc)))))
    (rev-helper lst '())))

(defun my-length (lst)
  "Calcule la longueur d'une liste"
  (if (null lst)
      0
      (+ 1 (my-length (cdr lst)))))

(defun my-last (lst)
  "Retourne le dernier élément d'une liste"
  (if (null (cdr lst))
      (car lst)
      (my-last (cdr lst))))

(defun my-butlast (lst)
  "Retourne tous les éléments sauf le dernier"
  (if (null (cdr lst))
      '()
      (cons (car lst) (my-butlast (cdr lst)))))

(defun my-member (item lst)
  "Teste si item est dans lst"
  (if (null lst)
      nil
      (if (eq item (car lst))
          t
          (my-member item (cdr lst)))))

;;; ----------------------------------------------------------------------------
;;; Génération de labels (remplacer gensym)
;;; ----------------------------------------------------------------------------

(defun generate-label (env prefix)
  "Génère un label unique avec un préfixe"
  (let ((counter (compiler-env-label-counter env))
        (prefix-str (if (symbolp prefix) (symbol-to-string prefix) prefix)))
    (let ((new-env (env-set env ':label-counter (+ counter 1))))
      (let ((label-str (string-concat-3 prefix-str "_" (int-to-string counter))))
        (cons (intern label-str) new-env)))))

;;; ----------------------------------------------------------------------------
;;; Gestion de l'environnement
;;; ----------------------------------------------------------------------------

(defun env-lookup-var (env var-name)
  "Cherche une variable dans l'environnement, retourne son index ou NIL"
  (cdr (assoc var-name (compiler-env-variables env))))

(defun env-add-var (env var-name)
  "Ajoute une variable à l'environnement et retourne (index . new-env)"
  (let ((vars (compiler-env-variables env))
        (index (my-length (compiler-env-variables env))))
    (let ((new-vars (cons (cons var-name index) vars)))
      (cons index (env-set env ':variables new-vars)))))

(defun env-add-vars (env var-names)
  "Ajoute plusieurs variables à l'environnement"
  (if (null var-names)
      env
      (let ((result (env-add-var env (car var-names))))
        (env-add-vars (cdr result) (cdr var-names)))))

(defun env-lookup-func (env func-name)
  "Cherche une fonction dans l'environnement"
  (cdr (assoc func-name (compiler-env-functions env))))

(defun env-add-func (env func-name label)
  "Ajoute une fonction à l'environnement"
  (let ((funcs (compiler-env-functions env)))
    (env-set env ':functions (cons (cons func-name label) funcs))))

;;; ----------------------------------------------------------------------------
;;; Compilation des expressions de base
;;; ----------------------------------------------------------------------------

(defun compile-expr (expr env)
  "Compile une expression LISP en une liste d'instructions ASM"
  (if (numberp expr)
      ;; Nombre
      (list (string-concat "PUSH " (int-to-string expr)))
      
      (if (symbolp expr)
          ;; Variable
          (let ((closure-index (cdr (assoc expr (compiler-env-closure-vars env)))))
            (if closure-index
                ;; Variable capturée
                (list (string-concat "LOADCLOSURE " (int-to-string closure-index)))
                ;; Variable normale
                (let ((index (env-lookup-var env expr)))
                  (if index
                      ;; Distinguer paramètres et variables locales
                      (if (if (compiler-env-in-function env)
                              (< index (compiler-env-n-params env))
                              nil)
                          ;; Paramètre
                          (list (string-concat "LOADARG " (int-to-string index)))
                          ;; Variable locale
                          (if (compiler-env-in-function env)
                              (list (string-concat "LOAD " (int-to-string (+ 1 (- index (compiler-env-n-params env))))))
                              (list (string-concat "LOAD " (int-to-string index)))))
                      (error "Variable non définie")))))
          
          (if (listp expr)
              ;; Liste (appel ou forme spéciale)
              (if (null expr)
                  (list "PUSH 0")
                  ;; Dispatcher selon le premier élément
                  (let ((op (car expr)))
                    (if (eq op '+)
                        (compile-arithmetic expr env)
                        (if (eq op '-)
                            (compile-arithmetic expr env)
                            (if (eq op '*)
                                (compile-arithmetic expr env)
                                (if (eq op '/)
                                    (compile-arithmetic expr env)
                                    (if (eq op 'mod)
                                        (compile-arithmetic expr env)
                                        (if (eq op '=)
                                            (compile-comparison expr env)
                                            (if (eq op '<)
                                                (compile-comparison expr env)
                                                (if (eq op '<=)
                                                    (compile-comparison expr env)
                                                    (if (eq op '>)
                                                        (compile-comparison expr env)
                                                        (if (eq op '>=)
                                                            (compile-comparison expr env)
                                                            (if (eq op 'if)
                                                                (compile-if expr env)
                                                                (if (eq op 'let)
                                                                    (compile-let expr env)
                                                                    (if (eq op 'progn)
                                                                        (compile-progn expr env)
                                                                        (if (eq op 'setq)
                                                                            (compile-setq expr env)
                                                                            (if (eq op 'defun)
                                                                                (compile-defun expr env)
                                                                                (if (eq op 'lambda)
                                                                                    (compile-lambda expr env)
                                                                                    (if (eq op 'labels)
                                                                                        (compile-labels expr env)
                                                                                        (compile-call expr env)))))))))))))))))))
              (error "Type non supporté")))))

;;; ----------------------------------------------------------------------------
;;; Compilation des opérations arithmétiques
;;; ----------------------------------------------------------------------------

(defun compile-arithmetic (expr env)
  "Compile une opération arithmétique"
  (let ((op (car expr))
        (args (cdr expr)))
    (if (null args)
        (error "Opération sans arguments")
        (if (null (cdr args))
            ;; Un seul argument
            (if (eq op '+)
                (compile-expr (car args) env)
                (if (eq op '-)
                    (my-append (compile-expr (car args) env)
                              (list "PUSH 0" "PUSH 0" "ROT" "SUB"))
                    (if (eq op '*)
                        (compile-expr (car args) env)
                        (error "Division/modulo avec un argument"))))
            ;; Deux arguments ou plus
            (let ((result (compile-expr (car args) env)))
              (labels ((process-args (remaining)
                         (if (null remaining)
                             result
                             (progn
                               (setq result (my-append result
                                                      (my-append (compile-expr (car remaining) env)
                                                               (list (arithmetic-op-to-instruction op)))))
                               (process-args (cdr remaining))))))
                (process-args (cdr args))))))))

(defun arithmetic-op-to-instruction (op)
  "Convertit un opérateur arithmétique en instruction"
  (if (eq op '+) "ADD"
      (if (eq op '-) "SUB"
          (if (eq op '*) "MUL"
              (if (eq op '/) "DIV"
                  (if (eq op 'mod) "MOD"
                      (error "Opérateur inconnu")))))))

;;; ----------------------------------------------------------------------------
;;; Compilation des comparaisons
;;; ----------------------------------------------------------------------------

(defun compile-comparison (expr env)
  "Compile une opération de comparaison"
  (let ((op (car expr))
        (args (cdr expr)))
    (if (= (my-length args) 2)
        (my-append (compile-expr (car args) env)
                  (my-append (compile-expr (car (cdr args)) env)
                            (list (comparison-op-to-instruction op))))
        (error "Comparaison nécessite 2 arguments"))))

(defun comparison-op-to-instruction (op)
  "Convertit un opérateur de comparaison en instruction"
  (if (eq op '=) "EQ"
      (if (eq op '<) "LT"
          (if (eq op '<=) "LE"
              (if (eq op '>) "GT"
                  (if (eq op '>=) "GE"
                      (error "Opérateur de comparaison inconnu")))))))

;;; ----------------------------------------------------------------------------
;;; Compilation de IF
;;; ----------------------------------------------------------------------------

(defun compile-if (expr env)
  "Compile une expression IF"
  (let ((test (car (cdr expr)))
        (then-expr (car (cdr (cdr expr))))
        (else-expr (car (cdr (cdr (cdr expr))))))
    (let ((label-result (generate-label env "ELSE")))
      (let ((label-else (car label-result))
            (env2 (cdr label-result)))
        (let ((label-result2 (generate-label env2 "ENDIF")))
          (let ((label-end (car label-result2))
                (env3 (cdr label-result2)))
            (my-append (compile-expr test env3)
                      (my-append (list (string-concat "JUMPNIF " (symbol-to-string label-else)))
                                (my-append (compile-expr then-expr env3)
                                          (my-append (list (string-concat "JUMP " (symbol-to-string label-end)))
                                                    (my-append (list (string-concat (symbol-to-string label-else) ":"))
                                                              (my-append (if else-expr
                                                                            (compile-expr else-expr env3)
                                                                            (list "PUSH 0"))
                                                                        (list (string-concat (symbol-to-string label-end) ":")))))))))))))

;;; ----------------------------------------------------------------------------
;;; Compilation de LET
;;; ----------------------------------------------------------------------------

(defun compile-let (expr env)
  "Compile une expression LET"
  (let ((bindings (car (cdr expr)))
        (body (cdr (cdr expr))))
    (let ((n-vars (my-length bindings)))
      (let ((result (if (> n-vars 0)
                       (list (string-concat "ALLOC " (int-to-string n-vars)))
                       '()))
            (new-env env))
        ;; Compiler chaque binding
        (labels ((process-bindings (remaining)
                   (if (null remaining)
                       '()
                       (let ((binding (car remaining)))
                         (let ((var-name (car binding))
                               (var-value (car (cdr binding))))
                           (let ((add-result (env-add-var new-env var-name)))
                             (let ((var-index (car add-result)))
                               (progn
                                 (setq new-env (cdr add-result))
                                 (setq result (my-append result
                                                        (my-append (compile-expr var-value env)
                                                                  (list (string-concat "STORE " (int-to-string var-index))))))
                                 (process-bindings (cdr remaining))))))))))
          (process-bindings bindings))
        
        ;; Compiler le corps
        (labels ((process-body (remaining)
                   (if (null remaining)
                       result
                       (progn
                         (setq result (my-append result (compile-expr (car remaining) new-env)))
                         (process-body (cdr remaining))))))
          (process-body body))
        
        ;; Désallouer
        (if (> n-vars 0)
            (my-append result (list (string-concat "DEALLOC " (int-to-string n-vars))))
            result)))))

;;; ----------------------------------------------------------------------------
;;; Compilation de PROGN
;;; ----------------------------------------------------------------------------

(defun compile-progn (expr env)
  "Compile une séquence d'expressions PROGN"
  (let ((body (cdr expr))
        (result '()))
    (labels ((process-body (remaining)
               (if (null remaining)
                   result
                   (progn
                     (setq result (my-append result (compile-expr (car remaining) env)))
                     (process-body (cdr remaining))))))
      (process-body body))))

;;; ----------------------------------------------------------------------------
;;; Compilation de SETQ
;;; ----------------------------------------------------------------------------

(defun compile-setq (expr env)
  "Compile une affectation SETQ"
  (let ((var-name (car (cdr expr)))
        (value (car (cdr (cdr expr)))))
    (let ((closure-index (cdr (assoc var-name (compiler-env-closure-vars env))))
          (var-index (env-lookup-var env var-name)))
      (if closure-index
          ;; Variable capturée
          (my-append (compile-expr value env)
                    (list (string-concat "STORECLOSURE " (int-to-string closure-index))))
          (if var-index
              ;; Variable normale
              (my-append (compile-expr value env)
                        (if (if (compiler-env-in-function env)
                                (< var-index (compiler-env-n-params env))
                                nil)
                            (error "Impossible de modifier un paramètre")
                            (if (compiler-env-in-function env)
                                (list (string-concat "STORE " (int-to-string (+ 1 (- var-index (compiler-env-n-params env))))))
                                (list (string-concat "STORE " (int-to-string var-index))))))
              (error "Variable non définie"))))))

;;; ----------------------------------------------------------------------------
;;; Compilation de DEFUN
;;; ----------------------------------------------------------------------------

(defun compile-defun (expr env)
  "Compile une définition de fonction"
  (let ((func-name (car (cdr expr)))
        (params (car (cdr (cdr expr))))
        (body (cdr (cdr (cdr expr)))))
    (let ((func-label (intern (string-concat "FUNC_" (symbol-to-string func-name)))))
      (let ((label-result (generate-label env "END_DEFUN")))
        (let ((end-label (car label-result))
              (env2 (cdr label-result)))
          (let ((new-env (make-compiler-env ':in-function t ':n-params (my-length params))))
            ;; Enregistrer la fonction
            (let ((env3 (env-add-func env2 func-name func-label)))
              ;; Copier les fonctions dans le nouvel environnement
              (let ((funcs (compiler-env-functions env3)))
                (setq new-env (env-set new-env ':functions funcs)))
              
              ;; Ajouter les paramètres
              (let ((param-index 0))
                (labels ((add-params (remaining)
                           (if (null remaining)
                               '()
                               (progn
                                 (setq new-env (env-set new-env ':variables
                                                       (cons (cons (car remaining) param-index)
                                                            (compiler-env-variables new-env))))
                                 (setq param-index (+ param-index 1))
                                 (add-params (cdr remaining))))))
                  (add-params params)))
              
              (my-append (list (string-concat "JUMP " (symbol-to-string end-label)))
                        (my-append (list (string-concat (symbol-to-string func-label) ":"))
                                  (my-append (compile-function-body body new-env)
                                            (my-append (list "RET")
                                                      (list (string-concat (symbol-to-string end-label) ":"))))))))))))

(defun compile-function-body (body env)
  "Compile le corps d'une fonction"
  (let ((result '()))
    (labels ((process (remaining)
               (if (null remaining)
                   result
                   (progn
                     (setq result (my-append result (compile-expr (car remaining) env)))
                     (process (cdr remaining))))))
      (process body))))

;;; ----------------------------------------------------------------------------
;;; Compilation d'appels de fonction
;;; ----------------------------------------------------------------------------

(defun compile-call (expr env)
  "Compile un appel de fonction"
  (let ((func-expr (car expr))
        (args (cdr expr)))
    (if (if (listp func-expr) (eq (car func-expr) 'lambda) nil)
        ;; Appel direct d'une lambda
        (let ((result '()))
          (labels ((compile-args (remaining)
                     (if (null remaining)
                         '()
                         (progn
                           (setq result (my-append result (compile-expr (car remaining) env)))
                           (compile-args (cdr remaining))))))
            (compile-args args))
          (setq result (my-append result (list (string-concat "PUSH " (int-to-string (my-length args))))))
          (setq result (my-append result (compile-expr func-expr env)))
          (my-append result (list "CALLCLOSURE")))
        
        (if (symbolp func-expr)
            ;; Appel de fonction nommée
            (let ((func-label (env-lookup-func env func-expr)))
              (if func-label
                  (let ((result '()))
                    (labels ((compile-args (remaining)
                               (if (null remaining)
                                   '()
                                   (progn
                                     (setq result (my-append result (compile-expr (car remaining) env)))
                                     (compile-args (cdr remaining))))))
                      (compile-args args))
                    (setq result (my-append result (list (string-concat "PUSH " (int-to-string (my-length args))))))
                    (my-append result (list (string-concat "CALL " (symbol-to-string func-label)))))
                  (error "Fonction non définie")))
            (error "Appel invalide")))))

;;; ----------------------------------------------------------------------------
;;; Compilation de LAMBDA (simplifié - sans closures pour l'instant)
;;; ----------------------------------------------------------------------------

(defun compile-lambda (expr env)
  "Compile une fonction lambda (version simplifiée)"
  (error "LAMBDA non encore implémenté dans la version simplifiée"))

;;; ----------------------------------------------------------------------------
;;; Compilation de LABELS
;;; ----------------------------------------------------------------------------

(defun compile-labels (expr env)
  "Compile des fonctions locales"
  (error "LABELS non encore implémenté dans la version simplifiée"))

;;; ----------------------------------------------------------------------------
;;; Fonction principale de compilation
;;; ----------------------------------------------------------------------------

(defun compile-lisp (expr)
  "Compile une expression LISP complète et retourne du code ASM"
  (let ((env (make-compiler-env))
        (instructions (compile-expr expr env)))
    (my-append instructions (list "HALT"))))

(defun compile-lisp-to-string (expr)
  "Compile une expression LISP et retourne une chaîne ASM"
  (let ((instructions (compile-lisp expr))
        (result ""))
    (labels ((concat-all (lst)
               (if (null lst)
                   result
                   (progn
                     (setq result (string-concat result (string-concat (car lst) (coerce (list 10) 'string))))
                     (concat-all (cdr lst))))))
      (concat-all instructions))))

(format t "Compilateur LISP simplifié chargé!~%")
