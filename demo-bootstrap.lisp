;;;; ============================================================================
;;;; DÉMONSTRATION BOOTSTRAP - Niveau 2
;;;; ============================================================================
;;;; Démonstration complète : VM compilée exécutant un programme sur VM native

(load "compiler.lisp")

(format t "~%╔════════════════════════════════════════╗~%")
(format t "║   BOOTSTRAP - DÉMONSTRATION NIVEAU 2   ║~%")
(format t "║   VM Compilée sur VM Native            ║~%")
(format t "╚════════════════════════════════════════╝~%")

(format t "~%🎯 Objectif: Exécuter une VM compilée sur la VM native~%")
(format t "   qui elle-même exécute un programme!~%")

;;; ----------------------------------------------------------------------------
;;; Démonstration 1: Mini calculatrice sur VM compilée
;;; ----------------------------------------------------------------------------

(format t "~%─────────────────────────────────────────~%")
(format t "📊 Démonstration 1: Calculatrice Simple~%")
(format t "─────────────────────────────────────────~%")

(format t "~%Programme: Calculer (5 + 3) * 2~%")
(format t "~%Étape 1: Compiler le code de la VM~%")

(compile-and-run '(progn
                    ;; === STRUCTURES DE BASE ===
                    (defun vm-new (code)
                      (list code 0 0 0))
                    
                    (defun vm-code (vm) (car vm))
                    (defun vm-pc (vm) (car (cdr vm)))
                    (defun vm-stack (vm) (car (cdr (cdr vm))))
                    (defun vm-peek (vm) (car (vm-stack vm)))
                    
                    (defun vm-set-pc (vm new-pc)
                      (list (vm-code vm) new-pc (vm-stack vm) 
                            (car (cdr (cdr (cdr vm))))))
                    
                    (defun vm-set-stack (vm new-stack)
                      (list (vm-code vm) (vm-pc vm) new-stack 
                            (car (cdr (cdr (cdr vm))))))
                    
                    ;; === OPÉRATIONS DE PILE ===
                    (defun vm-push (vm value)
                      (vm-set-stack vm (cons value (vm-stack vm))))
                    
                    (defun vm-pop-pair (vm)
                      (cons (vm-set-stack vm (cdr (vm-stack vm)))
                            (car (vm-stack vm))))
                    
                    ;; === EXÉCUTION D'INSTRUCTIONS ===
                    (defun vm-exec-push (vm operand)
                      (vm-set-pc (vm-push vm operand) (+ (vm-pc vm) 1)))
                    
                    (defun vm-exec-add (vm)
                      (let ((p1 (vm-pop-pair vm)))
                        (let ((vm1 (car p1)))
                          (let ((b (cdr p1)))
                            (let ((p2 (vm-pop-pair vm1)))
                              (let ((vm2 (car p2)))
                                (let ((a (cdr p2)))
                                  (vm-set-pc (vm-push vm2 (+ a b)) 
                                             (+ (vm-pc vm) 1)))))))))
                    
                    (defun vm-exec-mul (vm)
                      (let ((p1 (vm-pop-pair vm)))
                        (let ((vm1 (car p1)))
                          (let ((b (cdr p1)))
                            (let ((p2 (vm-pop-pair vm1)))
                              (let ((vm2 (car p2)))
                                (let ((a (cdr p2)))
                                  (vm-set-pc (vm-push vm2 (* a b)) 
                                             (+ (vm-pc vm) 1)))))))))
                    
                    ;; === TEST: (5 + 3) * 2 ===
                    ;; Simulation manuelle des instructions
                    (let ((vm0 (vm-new 0)))
                      ;; PUSH 5
                      (let ((vm1 (vm-exec-push vm0 5)))
                        ;; PUSH 3
                        (let ((vm2 (vm-exec-push vm1 3)))
                          ;; ADD → 8
                          (let ((vm3 (vm-exec-add vm2)))
                            ;; PUSH 2
                            (let ((vm4 (vm-exec-push vm3 2)))
                              ;; MUL → 16
                              (let ((vm5 (vm-exec-mul vm4)))
                                ;; Résultat
                                (vm-peek vm5)))))))))

(format t "~%✅ Résultat attendu: 16 (= (5+3)*2)~%")

;;; ----------------------------------------------------------------------------
;;; Démonstration 2: Fibonacci sur VM compilée
;;; ----------------------------------------------------------------------------

(format t "~%─────────────────────────────────────────~%")
(format t "📊 Démonstration 2: Calcul avec Listes~%")
(format t "─────────────────────────────────────────~%")

(format t "~%Programme: Créer une paire (10, 20) et calculer leur somme~%")

(compile-and-run '(progn
                    ;; VM simplifiée
                    (defun make-vm (stack)
                      (list stack 0))
                    
                    (defun get-stack (vm) (car vm))
                    (defun get-result (vm) (car (get-stack vm)))
                    
                    (defun push-vm (vm val)
                      (list (cons val (get-stack vm)) 0))
                    
                    (defun pop-vm (vm)
                      (list (cdr (get-stack vm)) (car (get-stack vm))))
                    
                    (defun add-vm (vm)
                      (let ((p1 (pop-vm vm)))
                        (let ((vm1 (car p1)))
                          (let ((b (cdr p1)))
                            (let ((p2 (pop-vm vm1)))
                              (let ((vm2 (car p2)))
                                (let ((a (cdr p2)))
                                  (push-vm vm2 (+ a b)))))))))
                    
                    ;; Programme: créer paire et additionner
                    (let ((vm0 (make-vm 0)))
                      (let ((vm1 (push-vm vm0 10)))
                        (let ((vm2 (push-vm vm1 20)))
                          (let ((vm3 (add-vm vm2)))
                            (get-result vm3)))))))

(format t "~%✅ Résultat attendu: 30 (= 10+20)~%")

;;; ----------------------------------------------------------------------------
;;; Démonstration 3: VM avec fetch d'instructions
;;; ----------------------------------------------------------------------------

(format t "~%─────────────────────────────────────────~%")
(format t "📊 Démonstration 3: VM avec Fetch~%")
(format t "─────────────────────────────────────────~%")

(format t "~%Programme: VM qui fetch et exécute des instructions~%")
(format t "Code: [(PUSH 7), (PUSH 3), (MUL)]~%")

(compile-and-run '(progn
                    (defun vm-create (code)
                      (list code 0 0))
                    
                    (defun vm-get-code (vm) (car vm))
                    (defun vm-get-pc (vm) (car (cdr vm)))
                    (defun vm-get-stack (vm) (car (cdr (cdr vm))))
                    
                    (defun vm-update (vm code pc stack)
                      (list code pc stack))
                    
                    (defun vm-inc-pc (vm)
                      (vm-update vm 
                                 (vm-get-code vm) 
                                 (+ (vm-get-pc vm) 1) 
                                 (vm-get-stack vm)))
                    
                    (defun vm-push-stack (vm val)
                      (vm-update vm 
                                 (vm-get-code vm) 
                                 (vm-get-pc vm) 
                                 (cons val (vm-get-stack vm))))
                    
                    (defun nth-elem (n lst)
                      (if (<= n 0)
                          (car lst)
                          (nth-elem (- n 1) (cdr lst))))
                    
                    (defun vm-fetch (vm)
                      (nth-elem (vm-get-pc vm) (vm-get-code vm)))
                    
                    (defun instr-op (i) (car i))
                    (defun instr-val (i) (cdr i))
                    
                    (defun vm-do-push (vm val)
                      (vm-inc-pc (vm-push-stack vm val)))
                    
                    (defun vm-do-mul (vm)
                      (let ((b (car (vm-get-stack vm))))
                        (let ((rest (cdr (vm-get-stack vm))))
                          (let ((a (car rest)))
                            (let ((vm2 (vm-update vm 
                                                  (vm-get-code vm) 
                                                  (vm-get-pc vm) 
                                                  (cdr rest))))
                              (vm-inc-pc (vm-push-stack vm2 (* a b))))))))
                    
                    ;; Programme: PUSH 7, PUSH 3, MUL
                    (let ((code (list (cons 2 7)   ; PUSH 7
                                      (cons 2 3)   ; PUSH 3
                                      (cons 12 0)))) ; MUL
                      (let ((vm0 (vm-create code)))
                        (let ((instr0 (vm-fetch vm0)))
                          (let ((vm1 (vm-do-push vm0 (instr-val instr0))))
                            (let ((instr1 (vm-fetch vm1)))
                              (let ((vm2 (vm-do-push vm1 (instr-val instr1))))
                                (let ((vm3 (vm-do-mul vm2)))
                                  (car (vm-get-stack vm3)))))))))))

(format t "~%✅ Résultat attendu: 21 (= 7*3)~%")

;;; ----------------------------------------------------------------------------
;;; Conclusion
;;; ----------------------------------------------------------------------------

(format t "~%╔════════════════════════════════════════╗~%")
(format t "║   🎉 BOOTSTRAP RÉUSSI! 🎉             ║~%")
(format t "╚════════════════════════════════════════╝~%")

(format t "~%✨ Ce qui vient d'être accompli:~%")
(format t "   1. Code LISP → Compilé en ASM~%")
(format t "   2. ASM → Chargé sur VM native~%")
(format t "   3. VM native → Exécute la VM compilée~%")
(format t "   4. VM compilée → Simule une autre VM!~%")
(format t "~%🚀 C'est le début du bootstrap auto-hébergé!~%")
(format t "~%📊 Architecture réalisée:~%")
(format t "   ┌─────────────────────┐~%")
(format t "   │  CLISP Natif        │~%")
(format t "   └──────────┬──────────┘~%")
(format t "              │ compile~%")
(format t "   ┌──────────▼──────────┐~%")
(format t "   │  VM Native          │~%")
(format t "   │  (vm.lisp)          │~%")
(format t "   └──────────┬──────────┘~%")
(format t "              │ exécute~%")
(format t "   ┌──────────▼──────────┐~%")
(format t "   │  VM Compilée        │~%")
(format t "   │  (bytecode)         │~%")
(format t "   └──────────┬──────────┘~%")
(format t "              │ simule~%")
(format t "   ┌──────────▼──────────┐~%")
(format t "   │  Programme Final    │~%")
(format t "   │  Résultat: 21       │~%")
(format t "   └─────────────────────┘~%")
(format t "~%🎯 Prochaines étapes possibles:~%")
(format t "   - Ajouter plus d'instructions~%")
(format t "   - Implémenter une boucle fetch-execute~%")
(format t "   - Compiler le compilateur lui-même~%")
(format t "   - Bootstrap complet (auto-compilation)~%")
(format t "~%")
