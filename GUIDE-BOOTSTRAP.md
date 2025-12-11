# Guide de Bootstrap du Compilateur

## État actuel

✅ **Fonctionnel:**
- VM à pile (43 opcodes)
- Loader ASM → bytecode
- Compilateur LISP → ASM
- Support: fonctions, récursion, closures, LABELS

✅ **Améliorations Phase 2:**
- `defstruct` → alists
- `make-hash-table` → alists  
- `maphash` → dolist

❌ **Pas encore auto-compilable:**
- Utilise encore: `format`, `gensym`, `with-open-file`, `read`, `write`
- La VM n'a pas d'I/O fichier

## Utilisation du système

### 1. Compiler et exécuter un programme simple

```bash
# Créer un programme
cat > prog.lisp << 'END'
(defun factorial (n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(factorial 5)
END

# Compiler et exécuter
clisp -x '(load "compiler.lisp") (compile-and-run (quote (load "prog.lisp")))'
```

### 2. Utiliser le fichier ASM intermédiaire

```lisp
;; Dans CLISP
(load "compiler.lisp")

;; Compiler vers ASM
(compile-lisp-to-file '(defun add (x y) (+ x y)) "add.asm")

;; Charger et exécuter
(load "loader.lisp")
(load-and-run-asm-file "add.asm")
```

### 3. Tests complets

```bash
bash run-all-tests.sh
# Résultat: 61/61 tests (100%)
```

## Pour aller plus loin: Auto-compilation complète

Il faudrait:

1. **Remplacer format:** Implémenter la concaténation de chaînes
2. **Remplacer gensym:** Utiliser un compteur manuel
3. **Ajouter I/O à la VM:** Opcodes READ-FILE, WRITE-FILE
4. **Réécrire le compilateur:** N'utiliser QUE les formes supportées

Cela représente ~2-3 jours de travail supplémentaire.

## Résumé

Le système actuel peut:
- ✅ Compiler des programmes Lisp vers ASM
- ✅ Exécuter ASM sur la VM
- ✅ Supporter récursion, closures, fonctions locales
- ❌ Se compiler lui-même (besoin d'I/O et simplification)

**Recommandation:** Utiliser le système tel quel pour compiler des programmes utilisateur. Le bootstrap complet nécessite d'ajouter l'I/O à la VM.
