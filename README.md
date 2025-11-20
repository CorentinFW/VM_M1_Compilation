# Projet Compilation LISP vers ASM

Ce projet implémente un compilateur LISP vers un langage d'assemblage personnalisé et une machine virtuelle pour l'exécuter.

## Structure du Projet

```
projet/
├── etapes-projet.txt   # Documentation complète des étapes
├── instructions.lisp   # Définition des instructions et opcodes
├── vm.lisp             # Machine virtuelle
├── loader.lisp         # Chargeur de fichiers ASM
├── tests.lisp          # Tests de la VM
├── test-loader.lisp    # Tests du loader
├── exemples/           # Fichiers ASM d'exemple
│   ├── simple.asm
│   ├── if.asm
│   ├── max.asm
│   └── fibo.asm
└── README.md           # Ce fichier
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

## Prochaines étapes

- Phase 4 : Compilateur (compiler.lisp) - LISP → ASM
- Phase 5 : Tests avancés (récursivité, fermetures)

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
