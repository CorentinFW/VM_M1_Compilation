;;;; Test I/O opcodes in VM
(load "vm.lisp")
(load "instructions.lisp")
(load "loader.lisp")

;; Helper to convert symbolic instructions to instruction structs
(defun make-instr (mnemonic &optional operand)
  (make-instruction :opcode (opcode-from-mnemonic mnemonic) :operand operand))

;; Test 1: String operations
(defun test-string-ops ()
  (format t "~%Test 1: String Operations~%")
  (let ((vm (make-vm :code (vector (make-instr 'PUSH "Hello")
                                    (make-instr 'PUSH " ")
                                    (make-instr 'PUSH "World")
                                    (make-instr 'STRCAT)
                                    (make-instr 'STRCAT)
                                    (make-instr 'PRINT)
                                    (make-instr 'PUSH 42)
                                    (make-instr 'NUMTOSTR)
                                    (make-instr 'PRINT)
                                    (make-instr 'PUSH 'FOO)
                                    (make-instr 'SYMTOSTR)
                                    (make-instr 'PRINT)
                                    (make-instr 'HALT)))))
    (vm-run vm)))

;; Test 2: File write and read
(defun test-file-io ()
  (format t "~%Test 2: File I/O~%")
  
  ;; Write to file
  (format t "Writing to test-output.txt...~%")
  (let ((vm (make-vm :code (vector (make-instr 'PUSH "test-output.txt")
                                    (make-instr 'FOPEN 1)  ; stack: [stream]
                                    (make-instr 'DUP)       ; stack: [stream, stream]
                                    (make-instr 'PUSH "Line 1")  ; stack: [stream, stream, "Line 1"]
                                    (make-instr 'WRITESTR)  ; pops: string, stream, pushes: success
                                    (make-instr 'POP)       ; pop success
                                    (make-instr 'FCLOSE)    ; close remaining stream
                                    (make-instr 'POP)       ; pop success
                                    (make-instr 'HALT)))))
    (vm-run vm))
  
  ;; Read from file
  (format t "Reading from test-output.txt...~%")
  (let ((vm (make-vm :code (vector (make-instr 'PUSH "test-output.txt")
                                    (make-instr 'FOPEN 0)   ; stack: [stream]
                                    (make-instr 'DUP)        ; stack: [stream, stream]
                                    (make-instr 'READSTR)    ; pops: stream, pushes: line or 0
                                    (make-instr 'PRINT)      ; should print "Line 1"
                                    (make-instr 'FCLOSE)     ; close remaining stream
                                    (make-instr 'POP)        ; pop success
                                    (make-instr 'HALT)))))
    (vm-run vm)))

;; Test 3: S-expression I/O
(defun test-sexpr-io ()
  (format t "~%Test 3: S-expression I/O~%")
  
  ;; Write s-expression
  (format t "Writing s-expression to test-sexpr.txt...~%")
  (let ((vm (make-vm :code (vector (make-instr 'PUSH "test-sexpr.txt")
                                    (make-instr 'FOPEN 1)   ; stack: [stream]
                                    (make-instr 'DUP)        ; stack: [stream, stream]
                                    (make-instr 'PUSH '(1 2 3))  ; stack: [stream, stream, (1 2 3)]
                                    (make-instr 'FWRITE)     ; pops: value, stream, pushes: success
                                    (make-instr 'POP)        ; pop success
                                    (make-instr 'FCLOSE)     ; close remaining stream
                                    (make-instr 'POP)        ; pop success
                                    (make-instr 'HALT)))))
    (vm-run vm))
  
  ;; Read s-expression
  (format t "Reading s-expression from test-sexpr.txt...~%")
  (let ((vm (make-vm :code (vector (make-instr 'PUSH "test-sexpr.txt")
                                    (make-instr 'FOPEN 0)   ; stack: [stream]
                                    (make-instr 'DUP)        ; stack: [stream, stream]
                                    (make-instr 'FREAD)      ; pops: stream, pushes: sexpr or 0
                                    (make-instr 'PRINT)      ; should print (1 2 3)
                                    (make-instr 'FCLOSE)     ; close remaining stream
                                    (make-instr 'POP)        ; pop success
                                    (make-instr 'HALT)))))
    (vm-run vm)))

;; Run all tests
(format t "=== Testing I/O Opcodes ===~%")
(test-string-ops)
(test-file-io)
(test-sexpr-io)
(format t "~%=== All I/O tests complete ===~%")
