# cs3110-final-project
Authors:
Jenna Li (yl3647),
Isabella Guan (ig276),
Uma Wang (yw2544)

# Oink Language Main Driver

Welcome to the **Oink Language Main Driver**, the interpreter for the Oink programming language. This guide explains how to use the main driver and its features.

---

## Getting Started

### Running the Interpreter

1. Compile the program using `dune`:
   ```bash
   dune build
   ```
2. Run the main driver:
   ```bash
   dune exec bin/main.exe
   ```

---

## Features

### 1. Interactive REPL

When you start the program, the interpreter enters an interactive mode where you can:

- **Evaluate Strings:** Enter strings in quotes (e.g., `"hello"`) for processing.
- **Assign Variables:** Use the syntax `oink x = 5;` to assign variables.
- **Boolean Logic:** Write expressions like `true and false` or `true or false`.
- **Mathematical Operations:**
  - Addition: `3 pigpile 5`
  - Subtraction: `10 snoutout 4`
  - Multiplication: `6 mudmultiply 7`
  - Division: `20 troughsplit 4`
- **Pen Operations:**
  - Concatenate lists: `[1; 2] penpen [3; 4]`
  - Prepend an element: `1 ppen [2; 3]`
  - Remove an element: `[1; 2; 3] pensnatch 1`
  - Get the first element: `[1; 2; 3] pensqueal`
  - Get list length: `[1; 2; 3] penlength`
- **Define Functions:** Define reusable functions using the `workhorse` keyword.
  Example:
  ```
  workhorse test x # oink y = x pigpile 5; baaa y#;
  go! test 3;
  ```
- **Conditionals:** Write conditional statements like:
  ```
  if true { oink x = 1; } else { oink x = 2; }
  ```

### 2. File Parsing

You can parse and execute an Oink source file using the `file` command.

#### File Format

- Write Oink code line by line, separating statements with a `;`.
- Comments can be added with `#`.

#### Example File (`test_input_demo.oink`):
```oink
# This is a comment
oink x = 5;
oink y = x pigpile 10;
if y snoutout 5 { oink z = true; } else { oink z = false; }
```

#### Parsing the File

Type this line in your terminal:
```
file test/test_input.oink
```
The interpreter will execute each statement in the file sequentially.

---

## Interactive Commands

- **`env`:** Displays all currently defined variables and their values.
- **`remove <var>`:** Removes the specified variable from the environment.
- **`exit`:** Exits the interpreter.

---

## Error Handling

The interpreter provides detailed error messages for syntax errors, invalid operations, or unbound identifiers.

### Example Errors:
- **Syntax Error:**
  ```
  Syntax Error: Unexpected token near `mud`
  ```
- **Unbound Identifier:**
  ```
  *SNORT* Unbound identifier: x *SNORT*
  ```

---

## Advanced Features

- **Pig Latin Translation:** Any string entered will be translated to Pig Latin.
- **Custom Error Messages:** Every error is accompanied by a playful Oink-themed message.

---

## Conclusion

Enjoy experimenting with the Oink language! If you encounter issues or have suggestions, feel free to contribute or reach out.

Happy coding, oink oink! 🐷

