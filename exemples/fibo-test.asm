; ============================================================================
; FIBONACCI - Version de test avec différentes valeurs
; ============================================================================
; Test avec n=10, résultat attendu = 55

JUMP main

fibo:
    LOADARG 0
    PUSH 1
    LE
    JUMPNIF fibo_recursive
    LOADARG 0
    RET

fibo_recursive:
    LOADARG 0
    PUSH 1
    SUB
    PUSH 1
    CALL fibo
    
    LOADARG 0
    PUSH 2
    SUB
    PUSH 1
    CALL fibo
    
    ADD
    RET

main:
    PUSH 10         ; Calcul de fibo(10)
    PUSH 1
    CALL fibo
    PRINT
    HALT
