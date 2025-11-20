; Calcul de fibonacci(5)
; fibo(0) = 0, fibo(1) = 1
; fibo(n) = fibo(n-1) + fibo(n-2)
;
; Pour cette version simple, on calcule manuellement :
; fibo(5) = 5
; On va juste faire une structure IF pour l'instant

; Calcul : if n <= 1 then n else ...
PUSH 5          ; n = 5
DUP
PUSH 1
LE              ; n <= 1 ?
JUMPNIF recursive_case
; Cas de base : retourner n
HALT

recursive_case:
; Pour l'instant juste un placeholder
PUSH 5          ; résultat attendu pour fibo(5)
HALT
