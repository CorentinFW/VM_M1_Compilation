# I/O and String Opcodes - Documentation

## Overview

Nine new opcodes have been added to enable file I/O and string manipulation. These are essential for the compiler to become self-hosting by allowing it to read source files and write output files directly through VM operations.

## File I/O Opcodes (90-95)

### FOPEN (90)
**Description:** Opens a file and returns a stream handle  
**Operand:** Mode (0=read, 1=write, 2=append)  
**Stack:** `[filename] → [stream]`  
**Example:**
```lisp
PUSH "myfile.txt"
FOPEN 1    ; Opens for writing
```

### FCLOSE (91)
**Description:** Closes an open file stream  
**Operand:** None  
**Stack:** `[stream] → [success]`  
**Returns:** 1 for success  

### FREAD (92)
**Description:** Reads an s-expression from a file  
**Operand:** None  
**Stack:** `[stream] → [value]`  
**Returns:** The s-expression read, or 0 if EOF  
**Example:**
```lisp
PUSH "data.txt"
FOPEN 0     ; Open for reading
FREAD       ; Read one s-expression
PRINT       ; Display it
```

### FWRITE (93)
**Description:** Writes an s-expression to a file  
**Operand:** None  
**Stack:** `[value, stream] → [success]`  
**Returns:** 1 for success, 0 for error  
**Example:**
```lisp
PUSH "output.txt"
FOPEN 1
PUSH (1 2 3)
FWRITE      ; Writes (1 2 3) to file
```

### READSTR (94)
**Description:** Reads a line as a string from a file  
**Operand:** None  
**Stack:** `[stream] → [string]`  
**Returns:** String line or 0 if EOF  

### WRITESTR (95)
**Description:** Writes a string to a file  
**Operand:** None  
**Stack:** `[string, stream] → [success]`  
**Returns:** 1 for success, 0 for error  

## String Manipulation Opcodes (100-102)

### STRCAT (100)
**Description:** Concatenates two strings  
**Operand:** None  
**Stack:** `[string1, string2] → [result]`  
**Example:**
```lisp
PUSH "Hello"
PUSH " "
PUSH "World"
STRCAT      ; " " + "World" = " World"
STRCAT      ; "Hello" + " World" = "Hello World"
```

### NUMTOSTR (101)
**Description:** Converts a number to its string representation  
**Operand:** None  
**Stack:** `[number] → [string]`  
**Example:**
```lisp
PUSH 42
NUMTOSTR    ; → "42"
```

### SYMTOSTR (102)
**Description:** Converts a symbol name to a string  
**Operand:** None  
**Stack:** `[symbol] → [string]`  
**Example:**
```lisp
PUSH 'FOO
SYMTOSTR    ; → "FOO"
```

## Usage Pattern for File I/O

### Writing to a file:
```lisp
PUSH "output.txt"
FOPEN 1         ; Open for writing
DUP             ; Duplicate stream handle
PUSH "Hello"
WRITESTR        ; Write string
POP             ; Pop success flag
FCLOSE          ; Close file
POP             ; Pop success flag
```

### Reading from a file:
```lisp
PUSH "input.txt"
FOPEN 0         ; Open for reading
DUP             ; Duplicate stream handle  
READSTR         ; Read line
PRINT           ; Display it
FCLOSE          ; Close file
POP             ; Pop success flag
```

## Implementation Status

✅ All 9 opcodes fully implemented in `vm.lisp`  
✅ Opcodes defined in `instructions.lisp`  
✅ Error handling with handler-case  
✅ Tests passing in `test-io.lisp`  
✅ All 61 existing tests still pass (no regression)

## Next Steps for Bootstrap

1. **Update loader.lisp** to handle new opcodes (if needed for ASM parsing)
2. **Modify compiler.lisp** to generate I/O opcodes:
   - Replace `with-open-file` with FOPEN/FCLOSE
   - Replace `read` with FREAD
   - Replace `write`/`print` with FWRITE
3. **Replace format** with string concatenation:
   - Use STRCAT for concatenation
   - Use NUMTOSTR for numbers
   - Use SYMTOSTR for symbols
4. **Replace gensym** with manual counter
5. **Test self-compilation**

## Testing

Run tests with:
```bash
clisp test-io.lisp
```

Expected output:
- String operations: "Hello World", "42", "FOO"
- File I/O: Writes and reads "Line 1"
- S-expression I/O: Writes and reads `(1 2 3)`
