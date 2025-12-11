# EXPLICATION DU SYSTÈME DE COMPILATION ET D'EXÉCUTION

Ce document explique en détail le fonctionnement de chaque composant du système : la machine virtuelle (VM), le chargeur (loader) et le compilateur.

---

## TABLE DES MATIÈRES

1. [La Machine Virtuelle (VM)](#1-la-machine-virtuelle-vm)
2. [Le Chargeur (Loader)](#2-le-chargeur-loader)
3. [Le Compilateur](#3-le-compilateur)
4. [Flux Complet d'Exécution](#4-flux-complet-dexécution)

---

## 1. LA MACHINE VIRTUELLE (VM)

### 1.1 Qu'est-ce que c'est ?

La VM est un **interpréteur de bytecode** qui exécute des instructions en assembleur. C'est l'équivalent d'un processeur logiciel.

### 1.2 Structure de la VM

```lisp
(defstruct vm
  (code #())           ; Tableau d'instructions à exécuter
  (pc 0)               ; Program Counter (pointeur d'instruction)
  (stack '())          ; Pile d'exécution (pour les calculs)
  (locals '())         ; Variables locales (par frame)
  (call-stack '())     ; Pile d'appels (pour les fonctions)
  (halt nil)           ; Flag d'arrêt
  (debug nil))         ; Mode debug
```

#### Composants clés :

1. **Code** : Tableau contenant toutes les instructions du programme
2. **PC (Program Counter)** : Indice de l'instruction courante (commence à 0)
3. **Stack** : Pile pour stocker les valeurs temporaires et résultats
4. **Locals** : Pile de frames pour les variables locales
5. **Call-stack** : Pile pour gérer les appels de fonctions
6. **Halt** : Indicateur pour arrêter l'exécution

### 1.3 Boucle Fetch-Decode-Execute

La VM fonctionne en 3 étapes répétées :

```
┌─────────────────────────────────────┐
│  1. FETCH (Récupération)            │
│     - Lire l'instruction à PC       │
│     - Charger ses opérandes         │
└──────────────┬──────────────────────┘
               │
               ▼
┌─────────────────────────────────────┐
│  2. DECODE (Décodage)                │
│     - Identifier l'opcode           │
│     - Extraire les opérandes        │
└──────────────┬──────────────────────┘
               │
               ▼
┌─────────────────────────────────────┐
│  3. EXECUTE (Exécution)              │
│     - Exécuter l'instruction        │
│     - Modifier la pile/mémoire      │
│     - Incrémenter PC (sauf sauts)   │
└──────────────┬──────────────────────┘
               │
               ▼
        Recommencer jusqu'à HALT
```

### 1.4 Exemple d'exécution : Addition Simple

**Programme** : Calculer 2 + 3

```assembly
PUSH 2      ; Instruction 0
PUSH 3      ; Instruction 1
ADD         ; Instruction 2
HALT        ; Instruction 3
```

**Trace d'exécution** :

```
PC=0, Instruction: PUSH 2
  Stack avant : []
  Stack après : [2]
  PC → 1

PC=1, Instruction: PUSH 3
  Stack avant : [2]
  Stack après : [3, 2]
  PC → 2

PC=2, Instruction: ADD
  Stack avant : [3, 2]
  Opération   : dépile 3, dépile 2, calcule 2+3=5, empile 5
  Stack après : [5]
  PC → 3

PC=3, Instruction: HALT
  Stack finale : [5]
  Résultat : 5
```

### 1.5 Types d'instructions

#### Instructions de pile
- **PUSH n** : Empile la valeur n
- **POP** : Dépile la valeur au sommet
- **DUP** : Duplique le sommet de la pile

#### Instructions arithmétiques
- **ADD** : Dépile b puis a, empile a+b
- **SUB** : Dépile b puis a, empile a-b
- **MUL** : Dépile b puis a, empile a*b
- **DIV** : Dépile b puis a, empile a/b
- **MOD** : Dépile b puis a, empile a mod b

#### Instructions de comparaison
- **EQ** : Égalité (retourne 1 si égal, 0 sinon)
- **LT** : Inférieur strict (a < b)
- **LE** : Inférieur ou égal (a ≤ b)
- **GT** : Supérieur strict (a > b)
- **GE** : Supérieur ou égal (a ≥ b)

#### Instructions de contrôle de flux
- **JUMP label** : Saut inconditionnel
- **JUMPIF label** : Saut si le sommet ≠ 0 (vrai)
- **JUMPNIF label** : Saut si le sommet = 0 (faux)
- **CALL label** : Appel de fonction
- **RET** : Retour de fonction
- **HALT** : Arrêt de la VM

#### Instructions de variables
- **LOAD n** : Charge la variable locale n sur la pile
- **STORE n** : Sauvegarde le sommet dans la variable locale n
- **LOADARG n** : Charge l'argument n de la fonction courante
- **ALLOC n** : Alloue n cellules pour variables locales
- **DEALLOC n** : Libère n cellules locales

### 1.6 Gestion des fonctions

#### Structure d'un appel de fonction

```
┌─────────────────────────────────┐
│  Avant CALL                      │
│  Stack: [arg2, arg1, n_args]    │
│  PC: 10                         │
└─────────────────────────────────┘
         │
         │ CALL fonction_addr
         ▼
┌─────────────────────────────────┐
│  Pendant CALL                    │
│  1. Dépile n_args (ex: 2)       │
│  2. Dépile les arguments        │
│  3. Crée un call-frame avec:    │
│     - return_address = PC+1 (11)│
│     - args = [arg1, arg2]       │
│  4. PC = fonction_addr          │
└─────────────────────────────────┘
         │
         │ Exécution fonction
         ▼
┌─────────────────────────────────┐
│  À la fin (RET)                 │
│  1. Dépile le call-frame        │
│  2. PC = return_address (11)    │
│  3. Résultat reste sur la pile  │
└─────────────────────────────────┘
```

#### Exemple : Fonction double(5)

```assembly
JUMP main

double:                ; Adresse 1
    LOADARG 0          ; Charge l'argument 0
    PUSH 2             ; Empile 2
    MUL                ; Multiplie
    RET                ; Retourne

main:                  ; Adresse 5
    PUSH 5             ; Argument
    PUSH 1             ; Nombre d'arguments
    CALL double        ; Appel
    HALT               ; Fin
```

**Trace** :
1. PC=0 : JUMP → PC=5
2. PC=5 : PUSH 5 → Stack=[5]
3. PC=6 : PUSH 1 → Stack=[1,5]
4. PC=7 : CALL double → Crée frame(args=[5], ret=8), PC=1
5. PC=1 : LOADARG 0 → Stack=[5]
6. PC=2 : PUSH 2 → Stack=[2,5]
7. PC=3 : MUL → Stack=[10]
8. PC=4 : RET → PC=8, Stack=[10]
9. PC=8 : HALT → Résultat=10

---

## 2. LE CHARGEUR (LOADER)

### 2.1 Qu'est-ce que c'est ?

Le loader est un **parser et assembleur** qui convertit du code assembleur textuel en bytecode exécutable par la VM.

### 2.2 Format du fichier ASM

```assembly
; Commentaire (ignoré)
label:                  ; Définition de label
    INSTRUCTION operand ; Instruction
    INSTRUCTION         ; Sans opérande
```

**Exemple** :
```assembly
; Calcul de 10 + 20
JUMP main

add_numbers:
    LOADARG 0
    LOADARG 1
    ADD
    RET

main:
    PUSH 10
    PUSH 20
    PUSH 2          ; 2 arguments
    CALL add_numbers
    HALT
```

### 2.3 Processus de chargement (3 passes)

#### **Passe 1 : Collecte des labels**

Parcourt le fichier pour enregistrer tous les labels et leur adresse.

```
Ligne                    Action
──────────────────────────────────────────
; Commentaire          → Ignoré
JUMP main               → Instruction 0
add_numbers:            → Label "add_numbers" = 1
    LOADARG 0           → Instruction 1
    LOADARG 1           → Instruction 2
    ADD                 → Instruction 3
    RET                 → Instruction 4
main:                   → Label "main" = 5
    PUSH 10             → Instruction 5
    ...
```

**Résultat** : Table des labels
```
add_numbers → 1
main        → 5
```

#### **Passe 2 : Parsing des instructions**

Convertit chaque ligne en structure d'instruction.

```
Ligne          → Parsing
──────────────────────────────────────
PUSH 10        → (PUSH "10")
CALL main      → (CALL "main")
ADD            → (ADD NIL)
```

#### **Passe 3 : Résolution des labels**

Remplace les références aux labels par leurs adresses.

```
Avant résolution    Après résolution
────────────────────────────────────
JUMP main           JUMP 5
CALL add_numbers    CALL 1
```

### 2.4 Exemple complet de chargement

**Fichier ASM** :
```assembly
JUMP end
start:
    PUSH 42
    RET
end:
    PUSH 1
    CALL start
    HALT
```

**Passe 1 - Labels** :
```
start → 1
end   → 3
```

**Passe 2 - Instructions parsées** :
```
0: (JUMP "end")
1: (PUSH 42)
2: (RET NIL)
3: (PUSH 1)
4: (CALL "start")
5: (HALT NIL)
```

**Passe 3 - Labels résolus** :
```
0: JUMP 3
1: PUSH 42
2: RET
3: PUSH 1
4: CALL 1
5: HALT
```

**Conversion en bytecode VM** :
```
Tableau d'instructions pour la VM :
[
  Instruction(opcode=30, operand=3),    ; JUMP
  Instruction(opcode=2, operand=42),    ; PUSH
  Instruction(opcode=34, operand=nil),  ; RET
  Instruction(opcode=2, operand=1),     ; PUSH
  Instruction(opcode=33, operand=1),    ; CALL
  Instruction(opcode=0, operand=nil)    ; HALT
]
```

---

## 3. LE COMPILATEUR

### 3.1 Qu'est-ce que c'est ?

Le compilateur **traduit du code LISP en code assembleur**. Il transforme des expressions de haut niveau en instructions machine de bas niveau.

### 3.2 Architecture du compilateur

```
┌─────────────────┐
│   Code LISP     │
│  (+ 2 3)        │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│   Parser        │
│  Analyse S-expr │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Générateur ASM │
│  PUSH 2         │
│  PUSH 3         │
│  ADD            │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Code ASM       │
└─────────────────┘
```

### 3.3 Environnement de compilation

```lisp
(defstruct compiler-env
  (variables '())        ; Variables locales et leur index
  (functions (hash))     ; Table des fonctions définies
  (label-counter 0)      ; Compteur pour labels uniques
  (in-function nil))     ; Indique si on est dans une fonction
```

### 3.4 Compilation par type d'expression

#### **Nombres (constantes)**

```lisp
42  →  PUSH 42
```

#### **Variables**

```lisp
x  →  LOAD <index de x>
```

Dans une fonction, les paramètres utilisent :
```lisp
x  →  LOADARG <index de x>
```

#### **Opérations arithmétiques**

```lisp
(+ 2 3)  →  PUSH 2
            PUSH 3
            ADD
```

**Ordre important** : Le premier opérande est empilé en premier !

```lisp
(- 10 3)  →  PUSH 10    ; a
             PUSH 3     ; b
             SUB        ; Calcule a - b = 7
```

#### **Opérations de comparaison**

```lisp
(< 5 10)  →  PUSH 5
             PUSH 10
             LT        ; Résultat: 1 (vrai)
```

#### **Structure IF**

```lisp
(if (< x 5)
    100
    200)
```

**Compilé en** :
```assembly
LOAD x              ; Charger x
PUSH 5              
LT                  ; x < 5 ?
JUMPNIF ELSE_0      ; Si faux, sauter vers ELSE
PUSH 100            ; Branche THEN
JUMP ENDIF_1        ; Sauter vers la fin
ELSE_0:
PUSH 200            ; Branche ELSE
ENDIF_1:
; Suite du programme
```

#### **Structure LET (variables locales)**

```lisp
(let ((x 10) (y 20))
  (+ x y))
```

**Compilé en** :
```assembly
ALLOC 2          ; Allouer 2 variables locales
PUSH 10          ; Valeur de x
STORE 0          ; Sauvegarder dans variable 0
PUSH 20          ; Valeur de y
STORE 1          ; Sauvegarder dans variable 1
LOAD 0           ; Charger x
LOAD 1           ; Charger y
ADD              ; Calculer x + y
DEALLOC 2        ; Libérer les variables
```

#### **Définition de fonction (DEFUN)**

```lisp
(defun double (x)
  (* x 2))
```

**Compilé en** :
```assembly
JUMP END_DEFUN_0          ; Sauter la définition
FUNC_DOUBLE:              ; Label de la fonction
    LOADARG 0             ; Charger le paramètre x
    PUSH 2
    MUL
    RET                   ; Retourner le résultat
END_DEFUN_0:
; Suite du programme
```

**Pourquoi le JUMP initial ?**
- Les fonctions sont compilées au début
- Sans JUMP, la VM exécuterait le code de la fonction directement
- Le JUMP permet de sauter par-dessus pour aller au programme principal

#### **Appel de fonction**

```lisp
(double 5)
```

**Compilé en** :
```assembly
PUSH 5           ; Argument
PUSH 1           ; Nombre d'arguments
CALL FUNC_DOUBLE ; Appel de la fonction
```

**Important** : Le nombre d'arguments doit être empilé juste avant CALL !

#### **Fonction récursive (Fibonacci)**

```lisp
(defun fibo (n)
  (if (<= n 1)
      n
      (+ (fibo (- n 1))
         (fibo (- n 2)))))
```

**Compilé en** :
```assembly
JUMP END_DEFUN_0
FUNC_FIBO:
    LOADARG 0             ; Charger n
    PUSH 1
    LE                    ; n <= 1 ?
    JUMPNIF ELSE_0        ; Si faux, aller au cas récursif
    LOADARG 0             ; Cas de base: retourner n
    JUMP ENDIF_1
ELSE_0:
    LOADARG 0             ; Calculer fibo(n-1)
    PUSH 1
    SUB
    PUSH 1
    CALL FUNC_FIBO        ; Résultat sur la pile
    
    LOADARG 0             ; Calculer fibo(n-2)
    PUSH 2
    SUB
    PUSH 1
    CALL FUNC_FIBO        ; Résultat sur la pile
    
    ADD                   ; Additionner les deux résultats
ENDIF_1:
    RET
END_DEFUN_0:
```

**Pourquoi ça fonctionne ?**
- Chaque appel CALL crée un nouveau call-frame
- Les arguments sont préservés dans le call-frame
- Les résultats sont laissés sur la pile
- Quand les deux appels récursifs retournent, leurs résultats sont sur la pile
- ADD les additionne

### 3.5 Génération de labels uniques

Le compilateur génère automatiquement des labels uniques avec un compteur :

```lisp
(if ...) → Labels: ELSE_0, ENDIF_1
(if ...) → Labels: ELSE_2, ENDIF_3
(if ...) → Labels: ELSE_4, ENDIF_5
```

Ceci évite les conflits de noms !

---

## 4. FLUX COMPLET D'EXÉCUTION

### 4.1 Exemple complet : De LISP à l'exécution

**Code LISP** :
```lisp
(progn
  (defun square (x)
    (* x x))
  (square 7))
```

### Étape 1 : Compilation LISP → ASM

**Le compilateur génère** :
```assembly
JUMP END_DEFUN_0      ; 0: Sauter la définition
FUNC_SQUARE:          ; 1: Début de la fonction
    LOADARG 0         ; 2: Charger x
    LOADARG 0         ; 3: Charger x encore
    MUL               ; 4: x * x
    RET               ; 5: Retourner
END_DEFUN_0:          ; 6: Fin de la définition
    PUSH 7            ; 7: Argument
    PUSH 1            ; 8: Nombre d'arguments
    CALL FUNC_SQUARE  ; 9: Appel
    HALT              ; 10: Fin
```

### Étape 2 : Chargement ASM → Bytecode

**Le loader résout les labels** :
```
Labels détectés:
  FUNC_SQUARE = 1
  END_DEFUN_0 = 6

Instructions avec adresses résolues:
  0: JUMP 6
  1: LOADARG 0
  2: LOADARG 0
  3: MUL
  4: RET
  5: (inexistant, saut par-dessus)
  6: PUSH 7
  7: PUSH 1
  8: CALL 1
  9: HALT
```

### Étape 3 : Exécution sur la VM

```
┌──────────────────────────────────────────────────────┐
│ PC=0: JUMP 6                                         │
│   Stack: []                                          │
│   Action: PC → 6                                     │
└──────────────────────────────────────────────────────┘
         ↓
┌──────────────────────────────────────────────────────┐
│ PC=6: PUSH 7                                         │
│   Stack: [7]                                         │
│   Action: Empiler 7                                  │
└──────────────────────────────────────────────────────┘
         ↓
┌──────────────────────────────────────────────────────┐
│ PC=7: PUSH 1                                         │
│   Stack: [1, 7]                                      │
│   Action: Empiler 1 (nombre d'args)                  │
└──────────────────────────────────────────────────────┘
         ↓
┌──────────────────────────────────────────────────────┐
│ PC=8: CALL 1                                         │
│   Stack: [1, 7]                                      │
│   Action: Dépiler n_args=1, dépiler arg=7            │
│          Créer frame(args=[7], ret=9)                │
│          PC → 1                                      │
│   Call-stack: [frame(args=[7], ret=9)]              │
└──────────────────────────────────────────────────────┘
         ↓
┌──────────────────────────────────────────────────────┐
│ PC=1: LOADARG 0  (dans la fonction)                 │
│   Stack: [7]                                         │
│   Action: Charger arg[0] = 7                         │
└──────────────────────────────────────────────────────┘
         ↓
┌──────────────────────────────────────────────────────┐
│ PC=2: LOADARG 0                                      │
│   Stack: [7, 7]                                      │
│   Action: Charger arg[0] = 7 encore                  │
└──────────────────────────────────────────────────────┘
         ↓
┌──────────────────────────────────────────────────────┐
│ PC=3: MUL                                            │
│   Stack: [7, 7]                                      │
│   Action: Dépiler 7, dépiler 7, calculer 7*7=49     │
│   Stack: [49]                                        │
└──────────────────────────────────────────────────────┘
         ↓
┌──────────────────────────────────────────────────────┐
│ PC=4: RET                                            │
│   Stack: [49]                                        │
│   Action: Dépiler frame, PC → 9                      │
│   Call-stack: []                                     │
└──────────────────────────────────────────────────────┘
         ↓
┌──────────────────────────────────────────────────────┐
│ PC=9: HALT                                           │
│   Stack: [49]                                        │
│   Résultat final: 49                                 │
└──────────────────────────────────────────────────────┘
```

### 4.2 Vue d'ensemble du système

```
┌─────────────────────────────────────────────────────────┐
│                    PROGRAMME LISP                        │
│  (defun fibo (n) (if (<= n 1) n (+ ...)))              │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
        ┌────────────────────────┐
        │     COMPILATEUR        │
        │   compiler.lisp        │
        │                        │
        │  - Parse S-expressions │
        │  - Génère labels       │
        │  - Produit ASM         │
        └────────┬───────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────┐
│                    CODE ASSEMBLEUR                       │
│  JUMP END_DEFUN_0                                       │
│  FUNC_FIBO:                                             │
│    LOADARG 0                                            │
│    ...                                                  │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
        ┌────────────────────────┐
        │       LOADER           │
        │     loader.lisp        │
        │                        │
        │  - Parse ASM           │
        │  - Résout labels       │
        │  - Produit bytecode    │
        └────────┬───────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────┐
│                      BYTECODE                            │
│  [Instr(JUMP,6), Instr(LOADARG,0), ...]                │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
        ┌────────────────────────┐
        │     MACHINE VIRTUELLE  │
        │       vm.lisp          │
        │                        │
        │  - Fetch instruction   │
        │  - Decode opcode       │
        │  - Execute             │
        │  - Gère pile & mémoire │
        └────────┬───────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────┐
│                     RÉSULTAT                             │
│                       13                                 │
└─────────────────────────────────────────────────────────┘
```

---

## 5. POINTS CLÉS À RETENIR

### VM (Machine Virtuelle)
- ✅ Exécute du bytecode instruction par instruction
- ✅ Utilise une pile pour les calculs
- ✅ Gère les appels de fonction avec une call-stack
- ✅ Les variables locales sont dans des frames

### Loader (Chargeur)
- ✅ Convertit ASM textuel en bytecode
- ✅ Résout les labels en 3 passes
- ✅ Produit un tableau d'instructions pour la VM

### Compilateur
- ✅ Traduit LISP en ASM
- ✅ Génère des labels uniques automatiquement
- ✅ Gère les fonctions récursives
- ✅ Optimise l'ordre des opérations sur la pile

### Flux complet
```
LISP → [Compilateur] → ASM → [Loader] → Bytecode → [VM] → Résultat
```

Chaque composant a un rôle précis et bien défini, permettant une séparation claire des responsabilités !
