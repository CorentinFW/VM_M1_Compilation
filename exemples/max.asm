; Calcul du maximum de deux nombres
; max(a, b) = if a > b then a else b
; Exemple : max(15, 22) = 22

PUSH 15         ; a = 15
STORE 0         ; sauvegarder a
PUSH 22         ; b = 22
STORE 1         ; sauvegarder b

; Comparaison
LOAD 0          ; charger a
LOAD 1          ; charger b
GT              ; a > b ?
JUMPIF a_is_greater

; b est plus grand
LOAD 1          ; charger b
JUMP end

a_is_greater:
LOAD 0          ; charger a

end:
HALT
