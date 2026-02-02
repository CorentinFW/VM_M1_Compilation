# Projet de Compilation Lisp vers ASM avec Machine Virtuelle

Ce projet implémente un compilateur de Common Lisp vers un langage assembleur personnalisé, ainsi qu'une machine virtuelle pour exécuter le code compilé.

## Structure du Projet

```
V2/
├── compiler.lisp       # Compilateur Lisp → ASM
├── loader.lisp         # Chargeur de code ASM
├── vm.lisp            # Machine virtuelle
├── main.lisp          # Point d'entrée principal
├── test/              # Fichiers de test
│   ├── fibo.lisp      # Fonction Fibonacci
│   ├── call-fibo.lisp # Appel de Fibonacci
│   ├── factorial.lisp # Fonction factorielle
│   ├── let-test.lisp  # Test de variables locales
│   ├── if-test.lisp   # Test de conditions
│   └── labels-test.lisp # Test de fonctions locales
└── README.md          # Ce fichier
```

## Fonctionnalités Implémentées

### Compilateur (compiler.lisp)
- ✅ **Fonctions récursives** : Support complet des appels récursifs
- ✅ **Structures de contrôle** :
  - `let` : Variables locales avec portée
  - `if` : Conditions
  - `loop` : Boucles
  - `labels` : Fonctions locales
- ✅ **Opérateurs arithmétiques** : `+`, `-`, `*`, `/`
- ✅ **Opérateurs de comparaison** : `=`, `<`, `<=`, `>`, `>=`
- ✅ **Fermetures** : Support de `lambda` avec capture d'environnement
- ✅ **Gestion de la portée** : Variables locales et paramètres

### Machine Virtuelle (vm.lisp)
- ✅ **Pile d'exécution** : Gestion complète de la pile
- ✅ **Frames** : Environnements d'exécution pour les fonctions
- ✅ **Instructions** :
  - Pile : `PUSH`, `POP`
  - Mémoire : `LOAD`, `STORE`
  - Arithmétique : `ADD`, `SUB`, `MUL`, `DIV`
  - Comparaison : `EQ`, `LT`, `LE`, `GT`, `GE`
  - Contrôle : `JUMP`, `JUMPNIL`, `CALL`, `RETURN`, `HALT`
  - Frames : `MAKEFRAME`, `POPFRAME`
  - Fermetures : `MAKECLOSURE`
  - Affichage : `PRINT`

### Chargeur (loader.lisp)
- ✅ Validation des instructions ASM
- ✅ Résolution des labels
- ✅ Chargement depuis fichier ou liste d'instructions

## Utilisation

### Démarrage

```lisp
;; Charger le système
(load "main.lisp")

;; Passer dans le package principal
(in-package :main)
```

### Exemples d'Utilisation

#### 1. Compiler et exécuter un fichier simple

```lisp
(compile-and-run "test/let-test.lisp")
```

#### 2. Compiler une fonction et son appel (ex: Fibonacci)

```lisp
(compile-and-run-with-call "test/fibo.lisp" "test/call-fibo.lisp")
```

#### 3. Exécuter tous les tests

```lisp
(run-tests)
```

#### 4. Mode debug

```lisp
;; Activer le debug du compilateur et de la VM
(compile-and-run "test/fibo.lisp" :debug t :debug-vm t)
```

## Workflow Complet : Exemple avec Fibonacci

### Étape 1 : Définir la fonction Fibonacci (test/fibo.lisp)

```lisp
(defun fibo (n)
  (if (<= n 1)
      n
      (+ (fibo (- n 1))
         (fibo (- n 2)))))
```

### Étape 2 : Créer l'appel (test/call-fibo.lisp)

```lisp
(fibo 10)
```

### Étape 3 : Compiler et exécuter

```lisp
(compile-and-run-with-call "test/fibo.lisp" "test/call-fibo.lisp")
```

### Ce qui se passe :

1. **Compilation** : La fonction `fibo` est compilée en instructions ASM
2. **Chargement** : Le code ASM est chargé dans la VM avec résolution des labels
3. **Compilation de l'appel** : `(fibo 10)` est compilé
4. **Exécution** : La VM exécute le code et retourne le résultat (55)

## Instructions ASM Générées

Voici un exemple du code ASM généré pour une simple addition :

```lisp
;; (+ 5 3)
(PUSH 5)
(PUSH 3)
(ADD)
(HALT)
```

Pour Fibonacci, le code est plus complexe avec des labels, des appels récursifs, etc.

## Architecture

### Compilateur

Le compilateur traverse l'arbre syntaxique abstrait (AST) du code Lisp et génère des instructions ASM. Il gère :
- **Environnements** : Pile d'environnements pour la portée des variables
- **Labels** : Générés automatiquement pour les sauts et fonctions
- **Optimisations** : Pas encore implémentées

### Machine Virtuelle

La VM est une machine à pile qui exécute les instructions ASM. Elle maintient :
- **Stack** : Pile principale pour les calculs
- **Frames** : Pile de frames pour les appels de fonction
- **PC** : Program Counter pour suivre l'instruction courante
- **Labels** : Table de correspondance label → adresse

### Chargeur

Le chargeur parse et valide les instructions ASM avant de les charger dans la VM. Il effectue deux passes :
1. **Première passe** : Identifier tous les labels et leurs adresses
2. **Deuxième passe** : Construire le programme validé

## Limitations Actuelles

- **Loop** : Implémentation basique, pas de support pour `loop ... while` complexe
- **Fermetures** : Implémentation simplifiée, capture d'environnement partielle
- **Types** : Pas de vérification de types à la compilation
- **Optimisations** : Aucune optimisation du code généré
- **Garbage Collection** : Pas de gestion mémoire avancée

## Extensions Possibles

1. **Optimisations** :
   - Élimination de code mort
   - Propagation de constantes
   - Inline de fonctions simples

2. **Nouvelles structures** :
   - `cond`, `case` pour les sélections multiples
   - `do`, `dotimes`, `dolist` pour les boucles
   - Support complet de `loop` avec clauses

3. **Meilleure gestion des fermetures** :
   - Capture complète de l'environnement
   - Support des variables libres

4. **Debugging** :
   - Breakpoints dans la VM
   - Step-by-step execution
   - Inspection de la pile et des frames

5. **Performances** :
   - Compilation JIT
   - Optimisations tail-call
   - Parallélisation

## Tests

Le dossier `test/` contient plusieurs exemples :

- **fibo.lisp** : Fonction Fibonacci récursive
- **factorial.lisp** : Factorielle récursive
- **let-test.lisp** : Test de variables locales
- **if-test.lisp** : Test de conditions
- **labels-test.lisp** : Test de fonctions locales

Pour ajouter vos propres tests, créez simplement un nouveau fichier `.lisp` dans le dossier `test/`.

## Auteur

Projet de compilation M1 - 2026

## Licence

Usage académique
