;;;; ============================================================================
;;;; MINI-COMPILER EN LISP PUR
;;;; ============================================================================
;;;; Compilateur LISP → ASM écrit en LISP pur
;;;; Compatible avec le sous-ensemble minimal pour bootstrap
;;;;
;;;; CONTRAINTES:
;;;;   ✗ PAS de defstruct
;;;;   ✗ PAS de hash-table
;;;;   ✗ PAS de gensym (compteurs manuels)
;;;;   ✗ PAS de format avancé
;;;;   ✓ Seulement: cons, car, cdr, list, append
;;;;   ✓ if, cond, let, defun, lambda
;;;;   ✓ +, -, *, /, =, <, >

;;; ----------------------------------------------------------------------------
;;; Utilitaires de base
;;; ----------------------------------------------------------------------------

(defun mini-lookup (key alist)
  "Cherche une clé dans une liste d'associations"
  (if (null alist)
      nil
      (if (eq key (car (car alist)))
          (cdr (car alist))
          (mini-lookup key (cdr alist)))))

(defun mini-add-binding (key value alist)
  "Ajoute une liaison à une liste d'associations"
  (cons (cons key value) alist))

(defun mini-length (lst)
  "Calcule la longueur d'une liste"
  (if (null lst)
      0
      (+ 1 (mini-length (cdr lst)))))

(defun mini-reverse (lst)
  "Inverse une liste"
  (mini-reverse-helper lst nil))

(defun mini-reverse-helper (lst acc)
  (if (null lst)
      acc
      (mini-reverse-helper (cdr lst) (cons (car lst) acc))))

(defun mini-append (lst1 lst2)
  "Concatène deux listes"
  (if (null lst1)
      lst2
      (cons (car lst1) (mini-append (cdr lst1) lst2))))

(defun mini-concat-all (lists)
  "Concatène une liste de listes"
  (if (null lists)
      nil
      (mini-append (car lists) (mini-concat-all (cdr lists)))))

;;; ----------------------------------------------------------------------------
;;; Environnement de compilation
;;; ----------------------------------------------------------------------------
;;; Structure: liste avec plusieurs composants
;;; (variables functions label-counter n-params in-function)

(defun mini-make-env ()
  "Crée un nouvel environnement vide"
  (list nil nil 0 0 nil))

(defun mini-env-vars (env)
  "Retourne les variables de l'environnement"
  (car env))

(defun mini-env-funcs (env)
  "Retourne les fonctions de l'environnement"
  (car (cdr env)))

(defun mini-env-counter (env)
  "Retourne le compteur de labels"
  (car (cdr (cdr env))))

(defun mini-env-nparams (env)
  "Retourne le nombre de paramètres"
  (car (cdr (cdr (cdr env)))))

(defun mini-env-infunc (env)
  "Retourne si on est dans une fonction"
  (car (cdr (cdr (cdr (cdr env))))))

(defun mini-env-set-vars (env vars)
  "Modifie les variables de l'environnement"
  (list vars
        (mini-env-funcs env)
        (mini-env-counter env)
        (mini-env-nparams env)
        (mini-env-infunc env)))

(defun mini-env-set-funcs (env funcs)
  "Modifie les fonctions de l'environnement"
  (list (mini-env-vars env)
        funcs
        (mini-env-counter env)
        (mini-env-nparams env)
        (mini-env-infunc env)))

(defun mini-env-set-counter (env counter)
  "Modifie le compteur de labels"
  (list (mini-env-vars env)
        (mini-env-funcs env)
        counter
        (mini-env-nparams env)
        (mini-env-infunc env)))

(defun mini-env-set-nparams (env n)
  "Modifie le nombre de paramètres"
  (list (mini-env-vars env)
        (mini-env-funcs env)
        (mini-env-counter env)
        n
        (mini-env-infunc env)))

(defun mini-env-set-infunc (env flag)
  "Modifie le flag in-function"
  (list (mini-env-vars env)
        (mini-env-funcs env)
        (mini-env-counter env)
        (mini-env-nparams env)
        flag))

;;; ----------------------------------------------------------------------------
;;; Génération de labels
;;; ----------------------------------------------------------------------------

(defun mini-generate-label (env prefix)
  "Génère un label unique et retourne (label . new-env)"
  (let ((counter (mini-env-counter env)))
    (cons (mini-concat-symbol prefix counter)
          (mini-env-set-counter env (+ counter 1)))))

(defun mini-concat-symbol (prefix num)
  "Crée un symbole à partir d'un préfixe et d'un nombre"
  (intern (mini-concat-strings (symbol-name prefix) 
                               (mini-concat-strings "_" 
                                                   (mini-number-to-string num)))))

(defun mini-number-to-string (n)
  "Convertit un nombre en chaîne"
  (if (= n 0)
      "0"
      (if (< n 0)
          (mini-concat-strings "-" (mini-number-to-string (- 0 n)))
          (mini-number-to-string-positive n))))

(defun mini-number-to-string-positive (n)
  (if (< n 10)
      (mini-digit-to-string n)
      (mini-concat-strings (mini-number-to-string-positive (floor n 10))
                          (mini-digit-to-string (mod n 10)))))

(defun mini-digit-to-string (d)
  (cond
    ((= d 0) "0")
    ((= d 1) "1")
    ((= d 2) "2")
    ((= d 3) "3")
    ((= d 4) "4")
    ((= d 5) "5")
    ((= d 6) "6")
    ((= d 7) "7")
    ((= d 8) "8")
    ((= d 9) "9")
    (t "?")))

(defun mini-concat-strings (s1 s2)
  "Concatène deux chaînes"
  (concatenate 'string s1 s2))

;;; ----------------------------------------------------------------------------
;;; Compilation des expressions
;;; ----------------------------------------------------------------------------

(defun mini-compile (expr env)
  "Compile une expression et retourne (code . new-env)"
  (cond
    ;; Nombre
    ((numberp expr)
     (cons (list (list 'PUSH expr)) env))
    
    ;; Variable
    ((symbolp expr)
     (mini-compile-var expr env))
    
    ;; Liste (opération ou forme spéciale)
    ((listp expr)
     (cond
       ((null expr)
        (cons (list (list 'PUSH 0)) env))
       
       ;; Opérateurs arithmétiques
       ((eq (car expr) '+)
        (mini-compile-arithmetic expr env '+))
       ((eq (car expr) '-)
        (mini-compile-arithmetic expr env '-))
       ((eq (car expr) '*)
        (mini-compile-arithmetic expr env '*))
       ((eq (car expr) '/)
        (mini-compile-arithmetic expr env '/))
       
       ;; Comparaisons
       ((eq (car expr) '=)
        (mini-compile-comparison expr env '=))
       ((eq (car expr) '<)
        (mini-compile-comparison expr env '<))
       ((eq (car expr) '<=)
        (mini-compile-comparison expr env '<=))
       ((eq (car expr) '>)
        (mini-compile-comparison expr env '>))
       ((eq (car expr) '>=)
        (mini-compile-comparison expr env '>=))
       
       ;; IF
       ((eq (car expr) 'if)
        (mini-compile-if expr env))
       
       ;; LET
       ((eq (car expr) 'let)
        (mini-compile-let expr env))
       
       ;; DEFUN
       ((eq (car expr) 'defun)
        (mini-compile-defun expr env))
       
       ;; PROGN
       ((eq (car expr) 'progn)
        (mini-compile-progn expr env))
       
       ;; Appel de fonction
       (t (mini-compile-call expr env))))
    
    (t (cons nil env))))

;;; ----------------------------------------------------------------------------
;;; Compilation des variables
;;; ----------------------------------------------------------------------------

(defun mini-compile-var (var env)
  "Compile une variable"
  (let ((index (mini-lookup var (mini-env-vars env))))
    (if index
        ;; Variable trouvée
        (if (mini-env-infunc env)
            ;; Dans une fonction: distinguer paramètre vs local
            (if (< index (mini-env-nparams env))
                ;; Paramètre
                (cons (list (list 'LOADARG index)) env)
                ;; Variable locale
                (cons (list (list 'LOAD (+ 1 (- index (mini-env-nparams env))))) env))
            ;; Hors fonction: LOAD normal
            (cons (list (list 'LOAD index)) env))
        ;; Variable non trouvée: erreur (pour l'instant push 0)
        (cons (list (list 'PUSH 0)) env))))

;;; ----------------------------------------------------------------------------
;;; Compilation arithmétique
;;; ----------------------------------------------------------------------------

(defun mini-compile-arithmetic (expr env op)
  "Compile une opération arithmétique"
  (let* ((args (cdr expr))
         (first-result (mini-compile (car args) env))
         (first-code (car first-result))
         (env1 (cdr first-result)))
    (if (null (cdr args))
        ;; Un seul argument
        (cons first-code env1)
        ;; Plusieurs arguments
        (mini-compile-arithmetic-multi (cdr args) env1 first-code op))))

(defun mini-compile-arithmetic-multi (args env code op)
  "Compile les arguments suivants d'une opération arithmétique"
  (if (null args)
      (cons code env)
      (let* ((arg-result (mini-compile (car args) env))
             (arg-code (car arg-result))
             (env1 (cdr arg-result))
             (instr (mini-arith-op-to-instr op))
             (new-code (mini-concat-all (list code arg-code (list (list instr))))))
        (mini-compile-arithmetic-multi (cdr args) env1 new-code op))))

(defun mini-arith-op-to-instr (op)
  "Convertit un opérateur arithmétique en instruction"
  (cond
    ((eq op '+) 'ADD)
    ((eq op '-) 'SUB)
    ((eq op '*) 'MUL)
    ((eq op '/) 'DIV)
    (t 'ADD)))

;;; ----------------------------------------------------------------------------
;;; Compilation des comparaisons
;;; ----------------------------------------------------------------------------

(defun mini-compile-comparison (expr env op)
  "Compile une comparaison"
  (let* ((arg1 (car (cdr expr)))
         (arg2 (car (cdr (cdr expr))))
         (result1 (mini-compile arg1 env))
         (code1 (car result1))
         (env1 (cdr result1))
         (result2 (mini-compile arg2 env1))
         (code2 (car result2))
         (env2 (cdr result2))
         (instr (mini-comp-op-to-instr op)))
    (cons (mini-concat-all (list code1 code2 (list (list instr)))) env2)))

(defun mini-comp-op-to-instr (op)
  "Convertit un opérateur de comparaison en instruction"
  (cond
    ((eq op '=) 'EQ)
    ((eq op '<) 'LT)
    ((eq op '<=) 'LE)
    ((eq op '>) 'GT)
    ((eq op '>=) 'GE)
    (t 'EQ)))

;;; ----------------------------------------------------------------------------
;;; Compilation de IF
;;; ----------------------------------------------------------------------------

(defun mini-compile-if (expr env)
  "Compile (if condition then else)"
  (let* ((condition (car (cdr expr)))
         (then-expr (car (cdr (cdr expr))))
         (else-expr (car (cdr (cdr (cdr expr)))))
         ;; Générer les labels
         (label-pair (mini-generate-label env 'ELSE))
         (else-label (car label-pair))
         (env1 (cdr label-pair))
         (label-pair2 (mini-generate-label env1 'ENDIF))
         (endif-label (car label-pair2))
         (env2 (cdr label-pair2))
         ;; Compiler la condition
         (cond-result (mini-compile condition env2))
         (cond-code (car cond-result))
         (env3 (cdr cond-result))
         ;; Compiler then
         (then-result (mini-compile then-expr env3))
         (then-code (car then-result))
         (env4 (cdr then-result))
         ;; Compiler else
         (else-result (mini-compile else-expr env4))
         (else-code (car else-result))
         (env5 (cdr else-result)))
    (cons (mini-concat-all
           (list cond-code
                 (list (list 'JUMPNIF else-label))
                 then-code
                 (list (list 'JUMP endif-label))
                 (list (list else-label ':))
                 else-code
                 (list (list endif-label ':))))
          env5)))

;;; ----------------------------------------------------------------------------
;;; Compilation de LET
;;; ----------------------------------------------------------------------------

(defun mini-compile-let (expr env)
  "Compile (let ((var val) ...) body)"
  (let* ((bindings (car (cdr expr)))
         (body (car (cdr (cdr expr))))
         (nvars (mini-length bindings))
         ;; Compiler les valeurs des bindings
         (values-result (mini-compile-let-values bindings env))
         (values-code (car values-result))
         (env1 (cdr values-result))
         ;; Ajouter les variables à l'environnement
         (env2 (mini-add-let-vars bindings env1))
         ;; Compiler le corps
         (body-result (mini-compile body env2))
         (body-code (car body-result))
         (env3 (cdr body-result))
         ;; Restaurer l'environnement (retirer les variables de let)
         (env4 (mini-env-set-vars env3 (mini-env-vars env1))))
    (cons (mini-concat-all
           (list (list (list 'ALLOC nvars))
                 values-code
                 body-code
                 (list (list 'DEALLOC nvars))))
          env4)))

(defun mini-compile-let-values (bindings env)
  "Compile les valeurs des bindings"
  (mini-compile-let-values-helper bindings env 0))

(defun mini-compile-let-values-helper (bindings env index)
  "Helper pour compiler les valeurs des bindings avec index"
  (if (null bindings)
      (cons nil env)
      (let* ((binding (car bindings))
             (value (car (cdr binding)))
             (result (mini-compile value env))
             (code (car result))
             (env1 (cdr result))
             (store-code (list (list 'STORE index)))
             (rest-result (mini-compile-let-values-helper (cdr bindings) env1 (+ index 1)))
             (rest-code (car rest-result))
             (env2 (cdr rest-result)))
        (cons (mini-concat-all (list code store-code rest-code)) env2))))

(defun mini-add-let-vars (bindings env)
  "Ajoute les variables de let à l'environnement"
  (if (null bindings)
      env
      (let* ((binding (car bindings))
             (var (car binding))
             (current-vars (mini-env-vars env))
             (new-index (mini-length current-vars))
             (new-vars (mini-add-binding var new-index current-vars)))
        (mini-add-let-vars (cdr bindings) (mini-env-set-vars env new-vars)))))

;;; ----------------------------------------------------------------------------
;;; Compilation de DEFUN
;;; ----------------------------------------------------------------------------

(defun mini-compile-defun (expr env)
  "Compile (defun name (params...) body)"
  (let* ((func-name (car (cdr expr)))
         (params (car (cdr (cdr expr))))
         (body (car (cdr (cdr (cdr expr)))))
         (nparams (mini-length params))
         ;; Générer les labels
         (label-pair (mini-generate-label env func-name))
         (func-label (car label-pair))
         (env1 (cdr label-pair))
         (label-pair2 (mini-generate-label env1 'END_DEFUN))
         (end-label (car label-pair2))
         (env2 (cdr label-pair2))
         ;; Enregistrer la fonction
         (env3 (mini-env-set-funcs env2 
                                   (mini-add-binding func-name func-label 
                                                    (mini-env-funcs env2))))
         ;; Créer environnement pour le corps de la fonction
         (func-env (mini-env-set-infunc
                    (mini-env-set-nparams
                     (mini-env-set-vars
                      (mini-env-set-funcs (mini-make-env) (mini-env-funcs env3))
                      (mini-params-to-vars params 0))
                     nparams)
                    t))
         ;; Compiler le corps
         (body-result (mini-compile body func-env))
         (body-code (car body-result)))
    (cons (mini-concat-all
           (list (list (list 'JUMP end-label))
                 (list (list func-label ':))
                 body-code
                 (list (list 'RET))
                 (list (list end-label ':))))
          env3)))

(defun mini-params-to-vars (params index)
  "Convertit les paramètres en bindings de variables"
  (if (null params)
      nil
      (cons (cons (car params) index)
            (mini-params-to-vars (cdr params) (+ index 1)))))

;;; ----------------------------------------------------------------------------
;;; Compilation de PROGN
;;; ----------------------------------------------------------------------------

(defun mini-compile-progn (expr env)
  "Compile (progn expr1 expr2 ...)"
  (mini-compile-progn-list (cdr expr) env))

(defun mini-compile-progn-list (exprs env)
  "Compile une liste d'expressions en séquence"
  (if (null exprs)
      (cons nil env)
      (if (null (cdr exprs))
          ;; Dernière expression
          (mini-compile (car exprs) env)
          ;; Expressions intermédiaires: ignorer le résultat
          (let* ((result (mini-compile (car exprs) env))
                 (code (car result))
                 (env1 (cdr result))
                 (rest-result (mini-compile-progn-list (cdr exprs) env1))
                 (rest-code (car rest-result))
                 (env2 (cdr rest-result)))
            (cons (mini-append code rest-code) env2)))))

;;; ----------------------------------------------------------------------------
;;; Compilation d'appels de fonction
;;; ----------------------------------------------------------------------------

(defun mini-compile-call (expr env)
  "Compile (function arg1 arg2 ...)"
  (let* ((func-name (car expr))
         (args (cdr expr))
         (nargs (mini-length args))
         (func-label (mini-lookup func-name (mini-env-funcs env)))
         ;; Compiler les arguments
         (args-result (mini-compile-args args env))
         (args-code (car args-result))
         (env1 (cdr args-result)))
    (if func-label
        (cons (mini-concat-all
               (list args-code
                     (list (list 'PUSH nargs))
                     (list (list 'CALL func-label))))
              env1)
        ;; Fonction non trouvée: erreur (push 0 pour l'instant)
        (cons (list (list 'PUSH 0)) env1))))

(defun mini-compile-args (args env)
  "Compile une liste d'arguments"
  (if (null args)
      (cons nil env)
      (let* ((result (mini-compile (car args) env))
             (code (car result))
             (env1 (cdr result))
             (rest-result (mini-compile-args (cdr args) env1))
             (rest-code (car rest-result))
             (env2 (cdr rest-result)))
        (cons (mini-append code rest-code) env2))))

;;; ----------------------------------------------------------------------------
;;; Génération de code ASM (liste → chaîne)
;;; ----------------------------------------------------------------------------

(defun mini-code-to-string (code)
  "Convertit une liste d'instructions en chaîne ASM"
  (if (null code)
      ""
      (mini-concat-strings (mini-instr-to-string (car code))
                          (mini-concat-strings (string #\Newline)
                                              (mini-code-to-string (cdr code))))))

(defun mini-instr-to-string (instr)
  "Convertit une instruction en chaîne"
  (let ((op (car instr))
        (arg (car (cdr instr))))
    (if (null arg)
        (symbol-name op)
        (if (eq arg ':)
            ;; C'est un label
            (mini-concat-strings (symbol-name op) ":")
            ;; C'est un opérande
            (mini-concat-strings (symbol-name op)
                                (mini-concat-strings " "
                                                    (if (numberp arg)
                                                        (mini-number-to-string arg)
                                                        (symbol-name arg))))))))

;;; ----------------------------------------------------------------------------
;;; Interface principale
;;; ----------------------------------------------------------------------------

(defun mini-compile-lisp (expr)
  "Compile une expression LISP en code ASM (chaîne)"
  (let* ((env (mini-make-env))
         (result (mini-compile expr env))
         (code (car result))
         ;; Ajouter HALT à la fin
         (full-code (mini-append code (list (list 'HALT)))))
    (mini-code-to-string full-code)))

;; Message de chargement
;; (format t "Mini-compiler en LISP pur chargé!~%")
