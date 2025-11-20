; Structure IF-THEN-ELSE
; if (x > 10) then 100 else 200
; avec x = 15

PUSH 15         ; x = 15
PUSH 10
GT              ; x > 10 ?
JUMPNIF else_branch

; then branch
PUSH 100
JUMP end_if

else_branch:
PUSH 200

end_if:
HALT
