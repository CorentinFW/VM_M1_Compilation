═══════════════════════════════════════════════════════════════
  DOSSIER OLD - FICHIERS ARCHIVÉS
═══════════════════════════════════════════════════════════════

Ce dossier contient les fichiers non essentiels et anciens du
projet, organisés pour garder la racine propre.

───────────────────────────────────────────────────────────────
  STRUCTURE
───────────────────────────────────────────────────────────────

old/
├── tests/          Anciens fichiers de tests
├── docs/           Anciennes documentations et explications
├── demos/          Anciennes démos et exemples
└── vm-bootstrap.lisp

───────────────────────────────────────────────────────────────
  CONTENU: tests/
───────────────────────────────────────────────────────────────

• tests.lisp                    - Anciens tests VM (remplacé par run-vm-tests.lisp)
• test-loader.lisp              - Anciens tests loader (remplacé par run-loader-tests.lisp)
• test-compiler.lisp            - Anciens tests compiler (remplacé par run-compiler-tests.lisp)
• run-loader-tests.lisp         - Tests loader (version ancienne)
• test-bootstrap.lisp           - Tests bootstrap initiaux
• test-bootstrap-compile.lisp   - Tests compilation bootstrap
• test-bootstrap-niveau3.lisp   - Tests bootstrap niveau 3
• test-bootstrap-niveau4.lisp   - Tests bootstrap niveau 4
• test-debug-bootstrap.lisp     - Tests debug bootstrap
• test-auto-hosting.lisp        - Tests auto-hébergement

Note: Les tests actuels sont dans la racine:
  - run-vm-tests.lisp
  - run-compiler-tests.lisp
  - test-closures.lisp
  - test-labels.lisp
  - test-mini-loader.lisp
  - test-mini-compiler.lisp

───────────────────────────────────────────────────────────────
  CONTENU: docs/
───────────────────────────────────────────────────────────────

• ANALYSE_EXIGENCES.txt         - Analyse initiale des exigences
• DOC_LABELS.txt                - Documentation technique LABELS (archivée)
• ETAPES_MANQUANTES.txt         - Étapes manquantes (historique)
• etapes-projet.txt             - Anciennes étapes du projet
• ETAT_FINAL.txt                - État final (ancienne version)
• ETAT_PROJET.txt               - État du projet (historique)
• EXPLAIN_OLD.md                - Anciennes explications
• EXPLAIN_BOOTSTRAP.txt         - Explication bootstrap (archivée)
• RECAP_PHASE4.txt              - Récapitulatif phase 4
• RECAP_PHASE5.txt              - Récapitulatif phase 5
• RECAP_PHASE6_BOOTSTRAP.txt    - Récapitulatif phase 6
• STRATEGIE_BOOTSTRAP.md        - Stratégie bootstrap (archivée)
• TODO_Compile.txt              - Plan de développement (accompli)

Note: La documentation actuelle est dans la racine:
  - README.md
  - QUICKSTART.txt
  - RAPPORT_FINAL.txt
  - STATUT_FINAL.txt
  - PRESENTATION.txt
  - MINI_LISP_SPEC.txt

───────────────────────────────────────────────────────────────
  CONTENU: demos/
───────────────────────────────────────────────────────────────

• demo-bootstrap.lisp           - Ancienne démo bootstrap
• demo-fibo.lisp                - Ancienne démo fibonacci
• demo-simple-bootstrap.lisp    - Démo bootstrap simple
• exemples-avances.lisp         - Exemples avancés (archivés)
• exemples-closures.lisp        - Exemples closures (archivés)
• mini-loader-compiled.asm      - Code ASM compilé (exemple)

Note: La démo actuelle est dans la racine:
  - demo-final.lisp

───────────────────────────────────────────────────────────────
  FICHIERS RACINE OLD/
───────────────────────────────────────────────────────────────

• vm-bootstrap.lisp             - Ancienne version bootstrap de la VM

═══════════════════════════════════════════════════════════════
  UTILITÉ
═══════════════════════════════════════════════════════════════

Ces fichiers sont conservés pour:
  • Historique du développement
  • Référence aux anciennes approches
  • Exemples alternatifs
  • Traçabilité du projet

Ils ne sont PAS nécessaires pour l'utilisation normale du système.

═══════════════════════════════════════════════════════════════
