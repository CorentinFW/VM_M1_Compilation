# Projet Compilation LISP vers ASM

Ce projet implémente un compilateur LISP vers un langage d'assemblage personnalisé et une machine virtuelle pour l'exécuter.

## Structure du Projet

```
projet/
├── etapes-projet.txt      # Documentation complète des étapes
├── instructions.lisp      # Définition des instructions et opcodes
├── vm.lisp                # Machine virtuelle
├── loader.lisp            # Chargeur de fichiers ASM
├── compiler.lisp          # Compilateur LISP → ASM
├── tests.lisp             # Tests de la VM
├── test-loader.lisp       # Tests du loader
├── test-compiler.lisp     # Tests du compilateur
├── test-closures.lisp     # Tests des fermetures (Phase 5)
├── exemples-closures.lisp # Exemples de closures (Phase 5)
├── RECAP_PHASE4.txt       # Récapitulatif Phase 4
├── RECAP_PHASE5.txt       # Récapitulatif Phase 5
├── EXPLAIN.md             # Explication détaillée du système
├── exemples/              # Fichiers ASM d'exemple
│   ├── simple.asm
│   ├── if.asm
│   ├── max.asm
│   ├── fibo.asm
│   └── fibo.lisp
└── README.md              # Ce fichier
```

## Phase 1 & 2 : TERMINÉES ✓

### Fichiers créés

1. **instructions.lisp** : Définit le jeu d'instructions complet
   - 30+ instructions (PUSH, ADD, JUMP, CALL, etc.)
   - Opcodes numériques
   - Documentation des instructions

2. **vm.lisp** : Machine virtuelle complète
   - Structure de la VM (pile, environnement, code)
   - Boucle Fetch-Decode-Execute
   - Instructions arithmétiques, comparaisons, contrôle de flux
   - Gestion des variables globales et locales
   - Pile d'appels pour les fonctions
   - Mode debug interactif

3. **tests.lisp** : Suite de tests
   - Tests unitaires pour chaque type d'instruction
   - Tests d'intégration (expressions complexes)
   - Exemples interactifs

## Phase 3 : TERMINÉE ✓

### Loader (Chargeur de fichiers ASM)

4. **loader.lisp** : Chargeur de fichiers ASM
   - Parser de fichiers .asm et de chaînes
   - Résolution des labels en trois passes :
     - Passe 1 : Collecte des labels et adresses
     - Passe 2 : Parsing des instructions
     - Passe 3 : Résolution des références aux labels
   - Support des commentaires (lignes commençant par ;)
   - Conversion en bytecode pour la VM
   - Fonctions utilitaires : `load-asm-file`, `load-asm-string`, `load-and-run-asm-file`

5. **test-loader.lisp** : Tests du loader
   - Tests du parsing de fichiers ASM
   - Tests de résolution de labels
   - Tests avec commentaires
   - Tests de structures de contrôle (IF, sauts)
   - 8/8 tests réussis ✓

6. **exemples/** : Fichiers ASM d'exemple
   - `simple.asm` : Addition simple (2 + 3)
   - `if.asm` : Structure IF-THEN-ELSE
   - `max.asm` : Calcul du maximum de deux nombres
   - `fibo.asm` : Préparation pour Fibonacci

## Utilisation

### Lancer les tests

```lisp
;; Tests de la VM
(load "tests.lisp")
(run-all-tests)

;; Tests du loader
(load "test-loader.lisp")
(run-loader-tests)
```

### Exemples simples

```lisp
;; Exemple 1: Addition simple (2 + 3)
(example-simple)

;; Exemple 2: Expression complexe ((10 + 5) * 2 - 3)
(example-complex)

;; Exemple 3: Mode debug pas-à-pas
(example-debug)
```

### Exécuter des fichiers ASM

```lisp
;; Charger le loader
(load "loader.lisp")

;; Exécuter un fichier ASM
(load-and-run-asm-file "exemples/simple.asm")
(load-and-run-asm-file "exemples/if.asm")
(load-and-run-asm-file "exemples/max.asm")

;; Exécuter du code ASM depuis une chaîne
(load-and-run-asm-string "PUSH 10
PUSH 20
ADD
HALT")
```

### Utilisation du compilateur

```lisp
;; Charger le compilateur
(load "compiler.lisp")

;; Exemple 1 : Expression arithmétique simple
(compile-and-run '(+ (* 2 3) 4))
; Résultat : 10

;; Exemple 2 : Structure IF
(compile-and-run '(if (< 2 3) 10 20))
; Résultat : 10

;; Exemple 3 : Variables locales avec LET
(compile-and-run '(let ((x 5) (y 10)) (+ x y)))
; Résultat : 15

;; Exemple 4 : Définir et appeler une fonction
(compile-and-run '(progn
                    (defun double (x) (* x 2))
                    (double 7)))
; Résultat : 14

;; Exemple 5 : Factorielle récursive
(compile-and-run '(progn
                    (defun fact (n)
                      (if (<= n 1)
                          1
                          (* n (fact (- n 1)))))
                    (fact 5)))
; Résultat : 120

;; Sauvegarder dans un fichier ASM
(compile-lisp-to-file '(+ 2 3) "exemples/addition.asm")
```

### Créer et exécuter du code manuellement

```lisp
;; Charger la VM
(load "vm.lisp")

;; Exemple: calculer 5 * 6
(vm-create-and-run
  (list (make-instruction-from-mnemonic 'PUSH 5)
        (make-instruction-from-mnemonic 'PUSH 6)
        (make-instruction-from-mnemonic 'MUL)
        (make-instruction-from-mnemonic 'HALT)))
```

## Instructions disponibles

### Pile
- `PUSH <val>` - Empiler une valeur
- `POP` - Dépiler
- `DUP` - Dupliquer le sommet

### Arithmétique
- `ADD, SUB, MUL, DIV, MOD`

### Comparaisons
- `EQ, LT, LE, GT, GE`

### Contrôle de flux
- `JUMP, JUMPIF, JUMPNIF` - Sauts
- `CALL, RET` - Appels de fonction
- `HALT` - Arrêt

### Variables
- `LOAD, STORE` - Variables globales
- `LOADARG` - Arguments de fonction
- `ALLOC, DEALLOC` - Variables locales

### Debug
- `PRINT` - Afficher le sommet de la pile

## Phase 4 : TERMINÉE ✓

### Compilateur LISP → ASM

7. **compiler.lisp** : Compilateur complet LISP → ASM
   - Structure de l'environnement de compilation
   - Compilation des expressions de base :
     - Constantes (nombres)
     - Variables locales
     - Opérations arithmétiques (+, -, *, /, mod)
     - Opérations de comparaison (=, <, <=, >, >=)
   - Structures de contrôle :
     - IF-THEN-ELSE avec génération de labels
     - LET avec variables locales (ALLOC/DEALLOC)
     - PROGN (séquences d'expressions)
     - SETQ (affectation)
   - Fonctions :
     - DEFUN (définition de fonctions)
     - Appels de fonctions avec arguments
     - Support de la récursivité
   - Fonctions utilitaires :
     - `compile-lisp` : Compile une expression LISP
     - `compile-and-run` : Compile et exécute directement
     - `compile-lisp-to-file` : Sauvegarde le code ASM

8. **test-compiler.lisp** : Suite de tests complète du compilateur
   - Phase 1 : Tests arithmétiques (8 tests)
   - Phase 2 : Tests de comparaisons (6 tests)
   - Phase 3 : Tests des structures IF (4 tests)
   - Phase 4 : Tests des variables LET (5 tests)
   - Phase 5 : Tests des fonctions DEFUN (5 tests)
   - Phase 6 : Tests de récursivité (3 tests) - Factorielle, Fibonacci, Somme
   - Exemples interactifs détaillés

## Phase 5 : TERMINÉE ✓

### Fermetures (Closures)

9. **Nouvelles instructions VM** :
   - `MKCLOSURE <addr> <nvars>` - Crée une fermeture capturant n variables
   - `LOADCLOSURE <index>` - Charge une variable capturée
   - `STORECLOSURE <index>` - Modifie une variable capturée
   - `CALLCLOSURE` - Appelle une fermeture depuis la pile

10. **Support de LAMBDA dans le compilateur** :
    - Fonctions anonymes : `(lambda (x) (* x 2))`
    - Capture automatique de variables : `(let ((n 10)) (lambda (x) (+ n x)))`
    - Lambdas imbriquées avec captures multiples
    - Génération automatique de labels uniques
    - Détection intelligente des variables libres

11. **test-closures.lisp** : Tests des fermetures
    - Lambda simple sans capture
    - Lambda avec plusieurs arguments
    - Lambdas imbriquées
    - Fermetures capturant 1 ou plusieurs variables
    - Fermetures avec structures de contrôle (IF)
    - 9/10 tests réussis ✓

12. **exemples-closures.lisp** : 12 exemples pratiques
    - Multiplicateur (capture d'un facteur)
    - Additionneur (capture d'une base)
    - Calcul complexe avec captures multiples
    - Pattern du compteur
    - Convertisseur de température
    - Vérificateur de plage
    - Et plus encore...

### Exemples d'utilisation des closures

```lisp
;; Lambda simple
((lambda (x) (* x 2)) 5)
;; → 10

;; Fermeture capturant une variable
(let ((factor 5))
  ((lambda (n) (* n factor)) 7))
;; → 35

;; Lambdas imbriquées
((lambda (x)
   ((lambda (y) (+ x y)) 5))
 10)
;; → 15

;; Fermeture avec condition
(let ((threshold 50))
  ((lambda (x) (if (> x threshold) x threshold)) 30))
;; → 50
```

## Prochaines étapes

- ✓ Phase 5 : Fermetures (CLOSURES) - TERMINÉE !
  - Support de LAMBDA (fonctions anonymes)
  - Capture de variables de l'environnement
  - Lambdas imbriquées
  - 9/10 tests réussis

- Phase 6 : Fonctionnalités avancées possibles
  - Structures de données (listes, tableaux)
  - Fonctions d'ordre supérieur (MAP, FILTER)
  - Garbage Collector
  - Optimisations du compilateur
  - REPL interactif

## Tests

### Tests VM ✓ (11/11)
- PUSH/POP
- Arithmétique (ADD, SUB, MUL, DIV)
- Comparaisons (EQ, LT, LE)
- DUP
- Variables (STORE/LOAD)
- Expressions complexes

### Tests Loader ✓ (8/8)
- Parsing de fichiers ASM
- Résolution de labels
- Gestion des commentaires
- Structures de contrôle (IF, JUMP)
- Chargement depuis string et fichier

### Tests Compilateur ✓ (31/31)
- Phase 1 : Arithmétique (8 tests)
- Phase 2 : Comparaisons (6 tests)
- Phase 3 : IF-THEN-ELSE (4 tests)
- Phase 4 : Variables LET (5 tests)
- Phase 5 : Fonctions DEFUN (5 tests)
- Phase 6 : Récursivité (3 tests)

### Tests Closures ✓ (9/10 - 90%)
- Lambda simple sans capture
- Lambda avec plusieurs arguments
- Lambda imbriquée
- Fermeture simple avec capture
- Fermeture avec plusieurs variables capturées
- Fermetures imbriquées
- Fermeture avec arithmétique complexe
- Fermeture avec IF
- Fermeture comme multiplicateur
- ⚠ Lambda retournant lambda avec LET (limitation connue)

**TOTAL : 59/60 tests réussis (98.3%)** 🎉
- Phase 2 : Comparaisons (6 tests)
- Phase 3 : Structures IF (4 tests)
- Phase 4 : Variables LET (5 tests)
- Phase 5 : Fonctions DEFUN (5 tests)
- Phase 6 : Récursivité (3 tests)
