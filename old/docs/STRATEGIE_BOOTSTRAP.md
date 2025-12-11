# STRATÉGIE DE BOOTSTRAP DU SYSTÈME

## Objectif

Créer un système auto-hébergé (self-hosting) capable de se compiler lui-même et de s'exécuter sur sa propre VM.

## Architecture en Couches

```
┌─────────────────────────────────────────────────────────┐
│  Niveau 0: CLISP Natif                                  │
│  - Exécute le compilateur Common Lisp                   │
└──────────────────┬──────────────────────────────────────┘
                   │ compile
                   ▼
┌─────────────────────────────────────────────────────────┐
│  Niveau 1: VM Native (vm.lisp)                          │
│  - Implémentée en Common Lisp                           │
│  - Exécute du bytecode ASM                              │
└──────────────────┬──────────────────────────────────────┘
                   │ exécute
                   ▼
┌─────────────────────────────────────────────────────────┐
│  Niveau 2: VM Compilée (vm-bootstrap.lisp)              │
│  - VM réécrite en LISP compilable                       │
│  - Compilée en bytecode par compiler.lisp               │
│  - Exécutée sur la VM Niveau 1                          │
└──────────────────┬──────────────────────────────────────┘
                   │ exécute
                   ▼
┌─────────────────────────────────────────────────────────┐
│  Niveau 3: Code Utilisateur                             │
│  - Compilé par le compilateur Niveau 2                  │
│  - Exécuté sur la VM Niveau 2                           │
└─────────────────────────────────────────────────────────┘
```

## Étapes du Bootstrap

### Phase 1: Primitives de Base ✓

**Statut**: TERMINÉ

Ajout des instructions essentielles:
- ✓ CONS, CAR, CDR - Construction et déconstruction de listes
- ✓ NULLP, LISTP - Prédicats sur les listes
- ✓ PUSHSYM - Support des symboles
- ✓ SYMBOLP, EQSYM - Opérations sur les symboles

Tests: 15/15 passent

### Phase 2: Mini-VM Compilable

**Objectif**: Réécrire la VM en LISP compilable

Contraintes:
- Pas de `defstruct` → utiliser des listes pour les structures
- Pas de `make-hash-table` → utiliser des listes d'association (alist)
- Pas de `loop` complexe → utiliser la récursion
- Pas de `format` → construire des chaînes avec CONS

Structure de données simplifiée:
```lisp
;; VM = (code pc stack env locals call-stack halt)
(defun make-vm (code)
  (list code 0 0 0 0 0 0))  ; Utiliser 0 pour NIL

(defun vm-code (vm) (car vm))
(defun vm-pc (vm) (car (cdr vm)))
(defun vm-stack (vm) (car (cdr (cdr vm))))
; etc.
```

Instructions à implémenter (version minimale):
- PUSH, POP, DUP
- ADD, SUB, MUL
- EQ, LT, LE
- JUMP, JUMPIF, JUMPNIF
- CALL, RET
- LOAD, STORE, LOADARG
- CONS, CAR, CDR
- HALT

### Phase 3: Mini-Loader Compilable

**Objectif**: Parser ASM en LISP compilable

Simplifications:
- Pas de gestion de fichiers → travailler avec des listes de strings
- Parser ligne par ligne avec récursion
- Labels dans une alist

```lisp
(defun parse-asm-line (line labels)
  (if (is-label line)
      (add-label line labels)
      (parse-instruction line)))
```

### Phase 4: Mini-Compilateur Compilable

**Objectif**: Compiler LISP → ASM en LISP compilable

C'est le plus complexe. Approche incrémentale:

1. **Version 0.1**: Expressions simples uniquement
   - Nombres
   - +, -, *, /
   - Variables locales (LET)

2. **Version 0.2**: Ajout des fonctions
   - DEFUN
   - CALL
   - Récursion simple

3. **Version 0.3**: Structures de contrôle
   - IF
   - Comparaisons

4. **Version 1.0**: Auto-compilation
   - Le compilateur peut se compiler lui-même

### Phase 5: Bootstrap Complet

**Séquence d'exécution**:

```bash
# 1. Compiler la mini-VM avec le compilateur natif
clisp -x "(load \"compiler.lisp\") 
         (compile-lisp-to-file (load-lisp-file \"vm-bootstrap.lisp\") 
                               \"vm-compiled.asm\")"

# 2. Charger la VM compilée sur la VM native
clisp -x "(load \"loader.lisp\")
          (load \"vm.lisp\")
          (setf *vm* (make-vm))
          (vm-load-code *vm* (load-asm-file \"vm-compiled.asm\"))
          (vm-run *vm*)"

# 3. Compiler du code utilisateur sur la VM compilée
clisp -x "(load \"bootstrap-full.lisp\")
          (bootstrap-compile-and-run '(+ 2 3))"
```

## Défis Techniques

### 1. Représentation des Données

**Problème**: Comment représenter les structures complexes?

**Solution**:
- Tout est une liste ou un nombre
- Structures = listes avec tag
- Exemple: `(INSTRUCTION PUSH 42)` au lieu de `(make-instruction :opcode 2 :operand 42)`

### 2. Gestion de la Mémoire

**Problème**: Pas de garbage collector explicite

**Solution**:
- Compter sur le GC de Common Lisp sous-jacent
- Dans la VM compilée, les listes non référencées seront collectées

### 3. Performance

**Problème**: VM sur VM = très lent (interprétation en double couche)

**Solution**:
- C'est normal pour un bootstrap
- Le but est pédagogique, pas la performance
- Une fois prouvé faisable, on peut optimiser

### 4. Debugging

**Problème**: Debugger du code sur une VM sur une VM est difficile

**Solution**:
- Ajouter des traces à chaque niveau
- Commencer avec des programmes très simples
- Tests unitaires à chaque étape

## Roadmap

### Court terme (Niveau 1)
- [ ] Écrire vm-bootstrap.lisp (VM simplifiée en LISP compilable)
- [ ] La compiler et la tester
- [ ] Vérifier qu'elle peut exécuter du bytecode simple

### Moyen terme (Niveau 2)
- [ ] Écrire loader-bootstrap.lisp
- [ ] Écrire compiler-bootstrap.lisp (version minimale)
- [ ] Tester la chaîne complète avec un programme trivial

### Long terme (Niveau 3)
- [ ] Améliorer le compilateur bootstrap pour qu'il puisse se compiler
- [ ] Auto-compilation réussie
- [ ] Documentation complète du processus

## Métriques de Succès

1. ✓ Primitives de listes fonctionnelles
2. [ ] VM compilée exécute (+ 2 3) correctement
3. [ ] Loader compilé parse un fichier ASM simple
4. [ ] Compilateur compilé compile (+ 2 3)
5. [ ] Compilateur compilé se compile lui-même
6. [ ] Programme utilisateur exécuté sur VM niveau 3

## État Actuel

- ✓ Phase 1 complète (primitives)
- ⏳ Phase 2 en cours (conception mini-VM)
- ⏸ Phases 3-5 à venir

## Prochaines Actions Immédiates

1. Créer `vm-bootstrap.lisp` avec:
   - Structure de VM en listes pures
   - Boucle fetch-decode-execute
   - Instructions minimales (10-15)

2. Compiler et tester:
   ```lisp
   (compile-and-run '(load "vm-bootstrap.lisp"))
   ```

3. Vérifier l'exécution:
   ```lisp
   ;; Sur la VM native, exécuter la VM compilée qui exécute (+ 2 3)
   (bootstrap-run '((PUSH 2) (PUSH 3) (ADD) (HALT)))
   ```

## Ressources

- `test-bootstrap.lisp` - Tests des primitives
- `compiler.lisp` - Compilateur natif (référence)
- `vm.lisp` - VM native (référence)
- `EXPLAIN.md` - Documentation du système

---

Ce projet est ambitieux mais faisable ! Le bootstrap est la preuve ultime
qu'un système est complet et bien conçu. 🚀
