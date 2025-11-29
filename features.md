# Nexus Language Features

**Important Design Note:** Nexus has no binary or unary operators. All operations are
function calls like `addi64(a, b)`, `not(x)`, `eqi64(a, b)`, etc. This minimizes syntax
complexity and makes the language more uniform.

## Variable Declaration Modifiers
**Location:** `crates/nexus-lexer/src/token.rs`, `crates/nexus-parser/src/ast.rs`, `crates/nexus-interpreter/src/scope.rs`

Variables are declared with modifier letters followed by the variable name:
- `m` - mutable (can be reassigned)
- `l` - locked (read-write locked automatically for thread safety)
- `h` - heap allocated
- `u` - undetermined type (stores type info + pointer to value)
- `g` - global (persists beyond scope, deleted with process or manually)

Modifiers can be combined: `mh arr = [1, 2, 3]` (mutable, heap-allocated array)

---

## Function Coloring
**Location:** `crates/nexus-parser/src/ast.rs`, `crates/nexus-interpreter/src/lib.rs`, `crates/nexus-permissions/src/lib.rs`

Functions are prefixed with a color indicating their capability level:
- `std` - Pure logic only, no side effects, no system calls
- `compat` - Uses base system compatibility APIs (cross-platform)
- `plat` - Uses platform-specific code

Example: `compat main(): void {}`

A `std` function cannot call `compat` or `plat` functions. A `compat` function cannot call `plat` functions.

---

## Return Codes
**Location:** `crates/nexus-interpreter/src/lib.rs`, `crates/nexus-interpreter/src/builtins.rs`

Platform-specific return codes are accessible through `plat` functions.

---

## Structs
**Location:** `crates/nexus-types/src/structs.rs`, `crates/nexus-parser/src/ast.rs`

```
struct abc {
    int abc = 0;
    [8]int nums;
    [dyn]int nums2 @prealloc(8)
}
```

- Default values (`= 0`) are applied at instantiation unless overridden
- Fixed-size arrays: `[8]int`
- Dynamic arrays: `[dyn]int` with optional `@prealloc(n)` (preallocates 2^n slots)

---

## Preallocatable Arrays
**Location:** `crates/nexus-types/src/primitives.rs`, `crates/nexus-interpreter/src/value.rs`

Arrays and vectors are unified. The runtime handles the difference internally.
- `[N]T` - Fixed size array of N elements of type T
- `[dyn]T` - Dynamic array
- `@prealloc(n)` - Preallocate 2^n slots

---

## Bounds Checking
**Location:** `crates/nexus-interpreter/src/value.rs`, `crates/nexus-cli/src/main.rs`

Bounds checking is enabled by default. Can be disabled via:
- CLI flag: `--no-bounds-check`
- Code annotation: `@unchecked` on specific array accesses

---

## If Clauses
**Location:** `crates/nexus-parser/src/ast.rs`, `crates/nexus-interpreter/src/lib.rs`

Pattern-matching style:
```
if eqi64(a) {
    10 -> ...;
    20 -> { ... }
} else {
    ...
}
```

Boolean condition style:
```
if (eqi64(a, b)) { ... }
```

---

## Subscopes
**Location:** `crates/nexus-parser/src/ast.rs`, `crates/nexus-interpreter/src/scope.rs`

Functions can have subscopes for manual implementation of while/for loops:
```
std example(): void {
    subscope loop {
        ...
    }
}
```

---

## Macros
**Location:** `crates/nexus-parser/src/ast.rs`, `crates/nexus-interpreter/src/lib.rs`

Macros are functions prefixed with `$` that return `macro` (a string that gets parsed):
```
$mymacro(): macro {
    return "..."
}
```

`compat` and `plat` calls within macros require explicit approval per call site.

---

## Strings and Runes
**Location:** `crates/nexus-types/src/primitives.rs`, `crates/nexus-interpreter/src/value.rs`

- Strings are `[]rune`
- `rune` is a Unicode codepoint
- Zero-width joiner sequences are supported for future emoji/grapheme cluster handling

---

## File I/O
**Location:** `crates/nexus-interpreter/src/builtins.rs`

Files are read/written as `[]u8` (byte arrays).

---

## Optionals and Results
**Location:** `crates/nexus-types/src/unknown.rs`, `crates/nexus-parser/src/ast.rs`

```
unknown<A, B, C, Error, None>
```

An `unknown` can hold any of the specified variant types, enabling sum types for error handling and optionals.

---

## Memory Management
**Location:** `crates/nexus-interpreter/src/scope.rs`, `crates/nexus-interpreter/src/value.rs`

- All memory is deallocated/cleared when scope ends
- Exception: `g` (global) variables persist until process exit or manual deletion
- Heap (`h`) variables are reference counted within their scope

---

## Interpreted and Compiled Modes
**Location:** `crates/nexus-cli/src/main.rs`, `crates/nexus-interpreter/src/lib.rs`, `crates/nexus-sandbox/src/lib.rs`

- Interpreted mode (implemented first)
- Optional sandboxing for interpreted mode
- Compilation support (future)

---

## Lambdas
**Location:** `crates/nexus-parser/src/ast.rs`, `crates/nexus-interpreter/src/value.rs`

Scopes and functions can be assigned as lambdas:
```
m fn = (x: int): int { return x + 1 }
```

An automatic struct is created to store captured variables.

---

## Visibility (No public/private)
**Location:** `crates/nexus-interpreter/src/lib.rs`, `crates/nexus-lsp/src/lib.rs`

- No `public` or `private` keywords
- Underscore-prefixed items (`_foo`) trigger a mutable warning when accessed from another module

---

## Contracts
**Location:** `crates/nexus-parser/src/ast.rs`, `crates/nexus-types/src/lib.rs`

Enforced contracts restrict arguments and make claims about output:
```
std add(int a @range(0, 100), int b @range(0, 100)): int @range(0, 200) {
    return a + b
}
```

---

## Defer
**Location:** `crates/nexus-parser/src/ast.rs`, `crates/nexus-interpreter/src/lib.rs`

```
defer { cleanup() }
```

Deferred code has access to the scope state at the point of the defer statement.

---

## Methods on Structs
**Location:** `crates/nexus-parser/src/ast.rs`, `crates/nexus-types/src/structs.rs`

```
std (m App app) run(): void {
    ...
}
```

`App` must conform to an interface that describes `run(): void`.

---

## Interfaces as Function Arguments
**Location:** `crates/nexus-types/src/interface.rs`, `crates/nexus-parser/src/ast.rs`

```
std damage(m Entity e): void { ... }
```

- Function arguments must be Interfaces (except base types)
- Return types and variables can be Interfaces or Structs
- Enables flexible code reuse

---

## CLI Support
**Location:** `crates/nexus-cli/src/main.rs`

- Run from buffer: `nexus -e "code here"`
- Run from file: `nexus file.nx`
- AST output for LSP integration: `nexus --ast file.nx`
- Lint mode: `nexus --lint file.nx`

---

## Permission System
**Location:** `crates/nexus-permissions/src/lib.rs`

Per-dependency permission configuration:
- Restrict to `std` only
- Allow specific `compat` permissions: `compat.io`, `compat.net`
- Allow specific `plat` permissions: `plat.android.vibrate`, `plat.desktop_x64.c_ffi`
- Compile-time macro permissions require manual approval

---

## Struct Interface Markers
**Location:** `crates/nexus-types/src/structs.rs`, `crates/nexus-types/src/interface.rs`

```
struct Player impl Entity, Damageable {
    ...
}
```

Structs are marked with interfaces for runtime type checking on `unknown` values.

---

## Predefined Functions (Builtins)
**Location:** `crates/nexus-interpreter/src/builtins.rs`

No import required. All operations are function calls:

**Logical operations:**
- `not(bool): bool` - Logical NOT
- `and(bool, bool): bool` - Logical AND
- `or(bool, bool): bool` - Logical OR

**Integer operations (i64):**
- `addi64(i64, i64): i64` - Addition
- `subi64(i64, i64): i64` - Subtraction
- `muli64(i64, i64): i64` - Multiplication
- `divi64(i64, i64): i64` - Division
- `modi64(i64, i64): i64` - Modulo
- `negi64(i64): i64` - Negation
- `eqi64(i64, i64): bool` - Equality
- `nei64(i64, i64): bool` - Inequality
- `lti64(i64, i64): bool` - Less than
- `lei64(i64, i64): bool` - Less or equal
- `gti64(i64, i64): bool` - Greater than
- `gei64(i64, i64): bool` - Greater or equal

**Float operations (f64):**
- `addf64(f64, f64): f64`, `subf64`, `mulf64`, `divf64`, `negf64`
- `eqf64`, `nef64`, `ltf64`, `lef64`, `gtf64`, `gef64`

**Bitwise operations:**
- `band(i64, i64): i64` - Bitwise AND
- `bor(i64, i64): i64` - Bitwise OR
- `bxor(i64, i64): i64` - Bitwise XOR
- `bnot(i64): i64` - Bitwise NOT
- `shl(i64, i64): i64` - Shift left
- `shr(i64, i64): i64` - Shift right

**Type conversions:**
- `i64(any): i64`, `i32(any): i32`, `f64(any): f64`, `f32(any): f32`
- `bool(any): bool`, `rune(any): rune`

**Collection operations:**
- `len(collection): i64` - Get length
- `push(collection, elem): collection` - Push element
- `pop(collection): [collection, elem]` - Pop element
- `concat(a, b): collection` - Concatenate
- `slice(collection, start, end): collection` - Get slice
- `contains(collection, elem): bool` - Check membership

**Utility:**
- `print(...)`, `println(...)` - Output
- `typeof(any): []rune` - Get type name
- `is_none(any): bool` - Check for None
- `unwrap(any): any` - Unwrap optional
- `panic(message)` - Panic with message
- `assert(bool)`, `assert_eq(a, b)` - Assertions

---

## Statement Terminators
**Location:** `crates/nexus-lexer/src/lib.rs`

`;` and newline are interchangeable:
```
m x = 1; m y = 2
m z = 3
```

---

## String Formatting
**Location:** `crates/nexus-interpreter/src/lib.rs`

```
$format("abc ${1} def")
```

`$format` is a builtin macro that interpolates expressions into strings.