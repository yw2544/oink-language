# Oink Language 🐷  
*A Thematic DSL for Expression Parsing*

Oink is a **domain-specific programming language (DSL)** inspired by *Animal Farm*, designed for expression parsing, evaluation, and structured computation. Built with **OCaml** and **Menhir**, Oink provides an intuitive syntax for mathematical and list operations while maintaining a playful thematic approach.

---

## Installation & Setup

### 1. Install Menhir for Parsing
Oink requires **Menhir** for parsing. Install it via:
```bash
opam install menhir
```

### 2. Run the Interpreter
After installing dependencies, execute:
```bash
dune exec bin/main.exe
```

### 3. Interact with Oink
Once running, you can interact through:
- **Interactive REPL**: Enter commands for evaluation.
- **File Parsing**: Execute Oink scripts stored in `.oink` files.

---

## Features & Syntax

### 1. Mathematical Operations
- Addition: `3 pigpile 5`  *(3 + 5)*
- Subtraction: `10 snoutout 4`  *(10 - 4)*
- Multiplication: `6 mudmultiply 7`  *(6 × 7)*
- Division: `20 troughsplit 4`  *(20 ÷ 4)*

### 2. List Operations (Pen)
- Concatenate lists: `[1; 2] penpen [3; 4]`
- Prepend an element: `1 ppen [2; 3]`
- Remove an element: `[1; 2; 3] pensnatch 1`
- First element: `[1; 2; 3] pensqueal`
- List length: `[1; 2; 3] penlength`

### 3. Variable Assignments & Functions
- Assign variables: `oink x = 5;` *(semicolon required)*
- Define functions:
  ```oink
  workhorse test x # oink y = x pigpile 5; baaa y#;
  go! test 3;
  ```

### 4. Conditionals
```oink
if true { oink x = 1; } else { oink x = 2; }
```

---

## File Execution
### Example Script (`test_input.oink`)
```oink
# This is a comment
oink x = 5;
oink y = x pigpile 10;
if y snoutout 5 { oink z = true; } else { oink z = false; };
```
### Run the Script
```bash
file test/test_input.oink
```
Oink will execute each statement sequentially.

---

## Interactive Commands
- `env` → Show all defined variables  
- `remove <var>` → Delete a variable  
- `exit` → Quit the interpreter  



## Advanced Features
- **Pig Latin Translation:** Oink can translate input strings into Pig Latin.
- **Custom Error Messages:** Enjoy playful, Oink-themed errors.




# oink-language
