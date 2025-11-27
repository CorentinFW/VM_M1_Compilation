================================================================================
EXEMPLES FIBONACCI - RÉCAPITULATIF
================================================================================

Ce dossier contient plusieurs implémentations de Fibonacci pour démontrer
les différentes capacités du système de compilation.

================================================================================
FICHIERS DISPONIBLES
================================================================================

1. fibo.lisp
   - Définition simple de la fonction Fibonacci en LISP
   - Pas de variables globales, juste la fonction
   - Peut être chargé et utilisé directement
   
   Usage:
     (load "exemples/fibo.lisp")
     (fibo 7)  ; Appel direct en LISP natif

2. fibo-test7.lisp, fibo-test10.lisp, fibo-test12.lisp
   - Fichiers de test pour compiler et exécuter Fibonacci
   - Chaque fichier teste une valeur différente
   - Résultats attendus : 13, 55, 144
   
   Usage:
     clisp exemples/fibo-test7.lisp
     clisp exemples/fibo-test10.lisp
     clisp exemples/fibo-test12.lisp

3. fibo.asm
   - Code assembleur écrit à la main
   - Calcule fibo(7) = 13
   - Peut être exécuté directement avec le loader
   
   Usage:
     (load "loader.lisp")
     (load-and-run-asm-file "exemples/fibo.asm")

4. fibo-test.asm
   - Version compacte pour tests
   - Calcule fibo(10) = 55
   
   Usage:
     (load "loader.lisp")
     (load-and-run-asm-file "exemples/fibo-test.asm")

5. fibo-compiled.asm
   - Code ASM généré automatiquement par le compilateur
   - Résultat de la compilation des fichiers fibo-test*.lisp
   - Identique en fonctionnalité mais avec labels générés automatiquement
   
   Généré avec:
     (compile-lisp-to-file '(progn (defun fibo ...) (fibo 7)) "...")

================================================================================
COMPARAISON : MANUEL vs COMPILÉ
================================================================================

┌─────────────────────┬───────────────────┬─────────────────────┐
│ Aspect              │ fibo.asm (manuel) │ fibo-compiled.asm   │
├─────────────────────┼───────────────────┼─────────────────────┤
│ Lignes de code      │ ~25 lignes        │ ~23 instructions    │
│ Labels              │ Noms explicites   │ Générés (ELSE_0)    │
│ Lisibilité          │ Haute             │ Moyenne             │
│ Performance         │ Identique         │ Identique           │
│ Facilité création   │ Manuel, lent      │ Automatique, rapide │
└─────────────────────┴───────────────────┴─────────────────────┘

================================================================================
FLUX DE TRAVAIL COMPLET
================================================================================

Option A : Compilation depuis LISP
-----------------------------------
1. Créer un fichier de test (ex: fibo-test7.lisp)
2. Exécuter : clisp exemples/fibo-test7.lisp

Option B : Compilation manuelle
-----------------------------------
1. Charger le compilateur : (load "compiler.lisp")
2. Compiler et exécuter : 
   (compile-and-run '(progn 
                       (defun fibo (n) ...)
                       (fibo 7)))

Option C : Sauvegarder en ASM puis exécuter
--------------------------------------------
1. Compiler vers fichier :
   (compile-lisp-to-file '(progn ...) "mon-fibo.asm")
2. Exécuter : (load-and-run-asm-file "mon-fibo.asm")

Option D : Écriture directe en ASM
-----------------------------------
1. Écrire le code ASM (fibo.asm)
2. Charger le loader : (load "loader.lisp")
3. Exécuter : (load-and-run-asm-file "fibo.asm")

================================================================================
RÉSULTATS VÉRIFIÉS
================================================================================

✓ fibo(0)  = 0
✓ fibo(1)  = 1
✓ fibo(5)  = 5
✓ fibo(7)  = 13
✓ fibo(10) = 55
✓ fibo(12) = 144

Tous les tests passent avec succès sur les versions LISP et ASM !

================================================================================
DÉMONSTRATION INTERACTIVE
================================================================================

Pour une démonstration complète du processus de compilation :

   clisp demo-fibo.lisp

Ceci montrera :
- Le code LISP source
- La compilation vers ASM
- L'exécution sur la VM
- Les résultats pour plusieurs valeurs

================================================================================
AUTRES EXEMPLES DISPONIBLES
================================================================================

- simple.asm   : Addition simple (2 + 3)
- if.asm       : Structure conditionnelle IF-THEN-ELSE
- max.asm      : Calcul du maximum de deux nombres

================================================================================
