; ============================================================================
; FIBONACCI RÉCURSIF EN ASSEMBLEUR
; ============================================================================
; Calcule fibonacci(n) où :
;   fibo(0) = 0
;   fibo(1) = 1
;   fibo(n) = fibo(n-1) + fibo(n-2)
;
; Ce fichier définit la fonction fibo et l'appelle avec n=7
; Résultat attendu : fibo(7) = 13
; ============================================================================

JUMP main

; ----------------------------------------------------------------------------
; Fonction fibo(n)
; Arguments : n (via LOADARG 0)
; Retourne : fibonacci(n)
; ----------------------------------------------------------------------------
fibo:
    ; Charger n et tester si n <= 1
    LOADARG 0
    PUSH 1
    LE
    JUMPNIF fibo_recursive
    
    ; Cas de base : n <= 1, retourner n
    LOADARG 0
    RET

fibo_recursive:
    ; Appel récursif : fibo(n-1)
    LOADARG 0
    PUSH 1
    SUB
    PUSH 1          ; 1 argument
    CALL fibo
    ; À ce stade : fibo(n-1) est au sommet de la pile
    
    ; Appel récursif : fibo(n-2)
    LOADARG 0
    PUSH 2
    SUB
    PUSH 1          ; 1 argument
    CALL fibo
    ; À ce stade : pile = [fibo(n-2), fibo(n-1)]
    
    ; Additionner les deux résultats
    ADD
    RET

; ----------------------------------------------------------------------------
; Programme principal
; ----------------------------------------------------------------------------
main:
    ; Appeler fibo(7)
    PUSH 7
    PUSH 1          ; 1 argument
    CALL fibo
    
    ; Le résultat est maintenant au sommet de la pile
    PRINT           ; Afficher le résultat pour déboguer
    HALT
