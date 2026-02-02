# Voici les etape : 
## init :
```lisp
    clisp
    (load "main.lisp")
```
# Liste des test
```lisp
(main:compile-and-run-with-call "test/fibo.lisp" "test/call-fibo.lisp")

(main:compile-and-run "test/let-test.lisp")

(main:compile-and-run-with-call "test/factorial.lisp" "test/call-factorial.lisp")

(main:compile-and-run-with-call "test/if-test.lisp" "test/call-if.lisp")

(main:compile-and-run "test/labels-test.lisp")

(main:compile-and-run "test/call-closure.lisp")

(main:compile-and-run "test/closure-multiple-vars.lisp")

```
