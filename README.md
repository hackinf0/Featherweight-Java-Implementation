# Featherweight Java Implementation

A complete interpreter implementation of Featherweight Java (FJ), a minimal core calculus for Java, developed as part of the **Principles and Paradigms of Programming Languages** course at the **University of Genova**, 2024.

##  Overview

Featherweight Java is a minimal functional subset of Java that captures the essential features of object-oriented programming: classes, inheritance, field access, method invocation, and dynamic dispatch. This implementation provides a full pipeline from parsing to evaluation, including comprehensive type checking.

##  Features

- **Complete FJ Syntax Support**: Classes, inheritance, constructors, fields, and methods
- **Lexer & Parser**: Built with Parsec for robust parsing of `.fj` files
- **Static Type Checker**: Ensures type safety before evaluation
  - Variable type checking
  - Field access validation
  - Method invocation type checking
  - Constructor argument validation
  - Cast operations (upcast, downcast, stupid cast)
  - Method overriding validation
- **Interpreter/Evaluator**: Step-by-step expression evaluation
  - Field access reduction
  - Method invocation with substitution
  - Object instantiation
  - Cast evaluation
- **Interactive REPL**: Command-line interface for interactive FJ programming

##  Prerequisites

- **GHC** (Glasgow Haskell Compiler) version 8.0 or higher
- **Cabal** or **Stack** (optional, for dependency management)

### Required Haskell Packages
- `base`
- `parsec`
- `containers`

##  Installation & Setup

### Option 1: Using GHC directly

```bash
# Clone or navigate to the project directory
cd fj

# Compile the main program
ghc -o fj Main.hs

# Run the interpreter
./fj
```

### Option 2: Using GHCi (Interactive)

```bash
# Load the main module in GHCi
ghci Main.hs

# Run main
*Main> main
```

### Option 3: Using Cabal (if you have a .cabal file)

```bash
cabal build
cabal run fj
```

##  Project Structure

```
fj/
├── Main.hs              # REPL and main entry point
├── Syntax.hs            # AST definitions and data types
├── Parser.hs            # Parsec-based parser for .fj files
├── Typechecker.hs       # Static type checking implementation
├── Evaluator.hs         # Expression evaluation and reduction rules
├── Example.fj           # Example Featherweight Java program
├── Error.fj             # Example with type errors
├── ErrorProgram.fj      # Example with runtime errors
├── README.md            # This file
└── Featherweight Java Implementation.pdf  # Course documentation
```

##  Usage

### Starting the REPL

```bash
./fj
```

You'll be greeted with:
```
Welcome to Featherweight Java!
Commands:
  :parse FILENAME.fj  -- parse a file
  :typecheck          -- typecheck the loaded class table
  :var NAME TYPE      -- declare a variable manually
  :ct                 -- show current class table
  :help               -- show this help
  :q                  -- quit
FJ>
```

### Basic Workflow

1. **Parse a program file**:
   ```
   FJ> :parse Example.fj
   Parsed! (but not typechecked yet)
   ```

2. **Typecheck the program**:
   ```
   FJ> :typecheck
   Typecheck succeeded!
   ```

3. **Declare variables (if needed)**:
   ```
   FJ> :var p Pair
   Added variable p :: Pair
   ```

4. **Evaluate expressions**:
   ```
   FJ> new Pair(new Object(), new Object()).fst
   Starting expression:
   FieldAccess (New "Pair" [New "Object" [],New "Object" []]) "fst"
   Type: Object
    => 
   New "Object" []
   Result:
   New "Object" []
   ```

### Example Programs

#### Example.fj - Basic Pair Class
```java
class Pair extends Object {
  Object fst;
  Object snd;

  Pair(Object fst, Object snd) {
    super();
    this.fst = fst;
    this.snd = snd;
  }

  Pair setfst(Object newfst) {
    return new Pair(newfst, this.snd);
  }
}
```

**Try it:**
```
FJ> :parse Example.fj
FJ> :typecheck
FJ> new Pair(new Object(), new Object()).setfst(new Object()).fst
```

#### Error.fj - Method Override Error
Demonstrates incorrect method overriding (incompatible return types).

#### ErrorProgram.fj - Field Access Error
Demonstrates field access to non-existent fields.

##  REPL Commands Reference

| Command | Description | Example |
|---------|-------------|---------|
| `:parse FILE` | Load and parse a `.fj` file | `:parse Example.fj` |
| `:typecheck` | Type check the loaded class table | `:typecheck` |
| `:var NAME TYPE` | Declare a variable with a type | `:var x Pair` |
| `:ct` | Display the current class table | `:ct` |
| `:help` | Show help message | `:help` |
| `:q` | Quit the interpreter | `:q` |
| `EXPRESSION` | Evaluate a Featherweight Java expression | `new Object()` |

##  Architecture

### Module Descriptions

#### **Syntax.hs**
Defines the Abstract Syntax Tree (AST) for Featherweight Java:
- `Class`: Class declarations with fields, constructor, and methods
- `Method`: Method declarations with parameters and body
- `Constructor`: Constructor with super call and field assignments
- `Expr`: Expressions (variables, field access, method invocation, object creation, casts)

#### **Parser.hs**
Implements the parser using Parsec combinators:
- Lexical analysis with token definitions
- Recursive descent parsing for classes, methods, and expressions
- Support for operator precedence (field access, method calls)

#### **Typechecker.hs**
Implements the FJ type system:
- **Typing rules**: T-Var, T-Field, T-Invk, T-New, T-Cast
- **Class table validation**: Checks for duplicate classes, validates inheritance, constructor correctness
- **Subtyping**: Implements subtype relation for inheritance
- **Method override checking**: Ensures overridden methods have compatible signatures

#### **Evaluator.hs**
Implements small-step operational semantics:
- **E-Field**: Field access from objects
- **E-Invk**: Method invocation with parameter substitution
- **E-New**: Object instantiation
- **E-Cast**: Type casting (evaluation only, no runtime check)
- Step-by-step reduction until reaching a value

#### **Main.hs**
REPL implementation:
- Interactive command loop
- State management (class table, type checking status, variable context)
- Expression parsing and evaluation pipeline

##  Featherweight Java Language Spec

### Syntax

```
Class:
  class C extends D {
    T₁ f₁; ... Tₙ fₙ;
    C(T₁ f₁, ..., Tₙ fₙ) {
      super(...);
      this.f₁ = f₁; ... this.fₙ = fₙ;
    }
    M₁ ... Mₖ
  }

Method:
  T m(T₁ x₁, ..., Tₙ xₙ) {
    return e;
  }

Expression:
  x                     (variable)
  e.f                   (field access)
  e.m(e₁, ..., eₙ)      (method invocation)
  new C(e₁, ..., eₙ)    (object creation)
  (C)e                  (cast)
```

### Typing Rules

- **T-Var**: Variables have types from the context
- **T-Field**: Field access requires the field to exist in the class
- **T-Invk**: Method invocation requires matching argument types
- **T-New**: Object creation requires matching field types
- **T-Cast**: Casts are allowed between related types (up/down/stupid cast)

### Evaluation Rules

- **E-Field**: `(new C(v₁,...,vₙ)).fᵢ → vᵢ`
- **E-Invk**: Method call substitutes arguments and `this`
- **E-Cast**: `(C)(new D(...)) → new D(...)` if valid

##  Known Limitations

- No primitive types (int, boolean) - only reference types
- No null values
- No interfaces
- Casts don't perform runtime type checking
- No garbage collection (not needed for pure evaluation)
- Single inheritance only (like Java)

##  Testing

Run the provided example files to test different features:

```bash
# Test successful program
FJ> :parse Example.fj
FJ> :typecheck
FJ> new Pair(new Object(), new Object()).fst

# Test method override error
FJ> :parse Error.fj
FJ> :typecheck
# Should fail with method override error

# Test field access error
FJ> :parse ErrorProgram.fj
FJ> :typecheck
# Should fail with field access error
```

##  References

- **"Featherweight Java: A Minimal Core Calculus for Java and GJ"**  
  Igarashi, Pierce, and Wadler (2001)  
  ACM TOPLAS 23(3), 2001

- **Course**: Principles and Paradigms of Programming Languages  
  University of Genova, 2024

##  Author

Developed as coursework for the Principles and Paradigms of Programming Languages course at the University of Genova, 2024.

##  License

This is an educational project developed for academic purposes.

---

##  Quick Start Example

```bash
# Start the interpreter
./fj

# Load an example
FJ> :parse Example.fj
Parsed! (but not typechecked yet)

# Typecheck it
FJ> :typecheck
Typecheck succeeded!

# Create and access a Pair
FJ> new Pair(new Object(), new Object()).fst
Starting expression:
FieldAccess (New "Pair" [New "Object" [],New "Object" []]) "fst"
Type: Object
 => 
New "Object" []
Result:
New "Object" []

# Try method invocation
FJ> new Pair(new Object(), new Object()).setfst(new Object())
Starting expression:
MethodInvoke (New "Pair" [New "Object" [],New "Object" []]) "setfst" [New "Object" []]
Type: Pair
 => 
New "Pair" [New "Object" [],New "Object" []]
Result:
New "Pair" [New "Object" [],New "Object" []]

# Quit
FJ> :q
Goodbye!
```

Thanks for reading! :)
