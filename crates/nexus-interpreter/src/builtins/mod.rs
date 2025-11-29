//! Builtin functions for the Nexus programming language.
//!
//! These functions are available without import and provide core operations.
//! All arithmetic, comparison, and logic operations are function calls.
//!
//! The module is split into submodules by category:
//! - `logical` - Boolean operations (not, and, or)
//! - `i64_ops` - 64-bit integer operations
//! - `i32_ops` - 32-bit integer operations
//! - `f64_ops` - 64-bit float operations
//! - `f32_ops` - 32-bit float operations
//! - `conversions` - Type conversion functions
//! - `collections` - Array/string/bytes operations
//! - `utility` - Utility functions (typeof, assert, etc.)
//! - `io` - IO functions (print, println)
//! - `bitwise` - Bitwise operations

mod bitwise;
mod collections;
mod compat_io;
mod conversions;
mod f32_ops;
mod f64_ops;
mod i32_ops;
mod i64_ops;
mod logical;
pub(crate) mod macros;
mod utility;

use crate::Value;
use nexus_core::{NexusResult, Span};
use std::collections::HashMap;

/// A builtin function
pub struct Builtin {
    /// Function name
    pub name: String,
    /// Number of expected arguments (None for variadic)
    pub arity: Option<usize>,
    /// The function implementation
    pub func: fn(&[Value], Span) -> NexusResult<Value>,
}

/// Registry of builtin functions
pub struct BuiltinRegistry {
    /// Core builtins that are always available
    builtins: HashMap<String, Builtin>,
    /// Builtins from compat.io module that require import
    compat_io_builtins: HashMap<String, Builtin>,
}

/// Macro to register a builtin function with less boilerplate
macro_rules! register_builtins {
    ($registry:expr, $( ($name:expr, $arity:expr, $func:expr) ),* $(,)?) => {
        $(
            $registry.register($name, $arity, $func);
        )*
    };
}

/// Macro to register compat.io builtins
macro_rules! register_compat_io_builtins {
    ($registry:expr, $( ($name:expr, $arity:expr, $func:expr) ),* $(,)?) => {
        $(
            $registry.register_compat_io($name, $arity, $func);
        )*
    };
}

impl BuiltinRegistry {
    /// Create a new registry with all builtin functions
    pub fn new() -> Self {
        let mut registry = Self {
            builtins: HashMap::new(),
            compat_io_builtins: HashMap::new(),
        };
        registry.register_all();
        registry
    }

    /// Get a builtin by name (only core builtins, not compat.io)
    pub fn get(&self, name: &str) -> Option<&Builtin> {
        self.builtins.get(name)
    }

    /// Get a compat.io builtin by name (requires import)
    pub fn get_compat_io(&self, name: &str) -> Option<&Builtin> {
        self.compat_io_builtins.get(name)
    }

    /// Check if a symbol is a compat.io builtin (requires import)
    pub fn is_compat_io(&self, name: &str) -> bool {
        self.compat_io_builtins.contains_key(name)
    }

    /// Register a builtin function
    fn register(
        &mut self,
        name: &str,
        arity: Option<usize>,
        func: fn(&[Value], Span) -> NexusResult<Value>,
    ) {
        self.builtins.insert(
            name.to_string(),
            Builtin {
                name: name.to_string(),
                arity,
                func,
            },
        );
    }

    /// Register a compat.io builtin function
    fn register_compat_io(
        &mut self,
        name: &str,
        arity: Option<usize>,
        func: fn(&[Value], Span) -> NexusResult<Value>,
    ) {
        self.compat_io_builtins.insert(
            name.to_string(),
            Builtin {
                name: name.to_string(),
                arity,
                func,
            },
        );
    }

    /// Register all builtin functions
    fn register_all(&mut self) {
        // Logical operations
        register_builtins!(
            self,
            ("not", Some(1), logical::builtin_not),
            ("and", Some(2), logical::builtin_and),
            ("or", Some(2), logical::builtin_or),
        );

        // i64 operations
        register_builtins!(
            self,
            ("addi64", Some(2), i64_ops::addi64),
            ("subi64", Some(2), i64_ops::subi64),
            ("muli64", Some(2), i64_ops::muli64),
            ("divi64", Some(2), i64_ops::divi64),
            ("modi64", Some(2), i64_ops::modi64),
            ("negi64", Some(1), i64_ops::negi64),
            ("eqi64", Some(2), i64_ops::eqi64),
            ("nei64", Some(2), i64_ops::nei64),
            ("lti64", Some(2), i64_ops::lti64),
            ("lei64", Some(2), i64_ops::lei64),
            ("gti64", Some(2), i64_ops::gti64),
            ("gei64", Some(2), i64_ops::gei64),
        );

        // i32 operations
        register_builtins!(
            self,
            ("addi32", Some(2), i32_ops::addi32),
            ("subi32", Some(2), i32_ops::subi32),
            ("muli32", Some(2), i32_ops::muli32),
            ("divi32", Some(2), i32_ops::divi32),
            ("modi32", Some(2), i32_ops::modi32),
            ("negi32", Some(1), i32_ops::negi32),
            ("eqi32", Some(2), i32_ops::eqi32),
            ("nei32", Some(2), i32_ops::nei32),
            ("lti32", Some(2), i32_ops::lti32),
            ("lei32", Some(2), i32_ops::lei32),
            ("gti32", Some(2), i32_ops::gti32),
            ("gei32", Some(2), i32_ops::gei32),
        );

        // f64 operations
        register_builtins!(
            self,
            ("addf64", Some(2), f64_ops::addf64),
            ("subf64", Some(2), f64_ops::subf64),
            ("mulf64", Some(2), f64_ops::mulf64),
            ("divf64", Some(2), f64_ops::divf64),
            ("negf64", Some(1), f64_ops::negf64),
            ("eqf64", Some(2), f64_ops::eqf64),
            ("nef64", Some(2), f64_ops::nef64),
            ("ltf64", Some(2), f64_ops::ltf64),
            ("lef64", Some(2), f64_ops::lef64),
            ("gtf64", Some(2), f64_ops::gtf64),
            ("gef64", Some(2), f64_ops::gef64),
        );

        // f32 operations
        register_builtins!(
            self,
            ("addf32", Some(2), f32_ops::addf32),
            ("subf32", Some(2), f32_ops::subf32),
            ("mulf32", Some(2), f32_ops::mulf32),
            ("divf32", Some(2), f32_ops::divf32),
            ("negf32", Some(1), f32_ops::negf32),
        );

        // Type conversions
        register_builtins!(
            self,
            ("i64", Some(1), conversions::to_i64),
            ("i32", Some(1), conversions::to_i32),
            ("f64", Some(1), conversions::to_f64),
            ("f32", Some(1), conversions::to_f32),
            ("bool", Some(1), conversions::to_bool),
            ("rune", Some(1), conversions::to_rune),
        );

        // Collection operations
        register_builtins!(
            self,
            ("len", Some(1), collections::len),
            ("push", Some(2), collections::push),
            ("pop", Some(1), collections::pop),
            ("concat", Some(2), collections::concat),
            ("slice", Some(3), collections::slice),
            ("contains", Some(2), collections::contains),
        );

        // Utility functions
        register_builtins!(
            self,
            ("typeof", Some(1), utility::type_of),
            ("is_none", Some(1), utility::is_none),
            ("unwrap", Some(1), utility::unwrap),
            ("panic", Some(1), utility::panic),
            ("assert", Some(1), utility::assert),
            ("assert_eq", Some(2), utility::assert_eq),
        );

        // Bitwise operations
        register_builtins!(
            self,
            ("band", Some(2), bitwise::band),
            ("bor", Some(2), bitwise::bor),
            ("bxor", Some(2), bitwise::bxor),
            ("bnot", Some(1), bitwise::bnot),
            ("shl", Some(2), bitwise::shl),
            ("shr", Some(2), bitwise::shr),
        );

        // compat.io module (requires import)
        register_compat_io_builtins!(
            self,
            ("print", None, compat_io::print),
            ("println", None, compat_io::println),
        );
    }
}

impl Default for BuiltinRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_builtin_registry() {
        let registry = BuiltinRegistry::new();
        assert!(registry.get("addi64").is_some());
        assert!(registry.get("not").is_some());
        assert!(registry.get("nonexistent").is_none());
    }

    #[test]
    fn test_addi64() {
        let args = vec![Value::I64(10), Value::I64(20)];
        let result = (i64_ops::addi64)(&args, Span::dummy()).unwrap();
        assert_eq!(result, Value::I64(30));
    }

    #[test]
    fn test_not() {
        let args = vec![Value::Bool(true)];
        let result = (logical::builtin_not)(&args, Span::dummy()).unwrap();
        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn test_len() {
        let args = vec![Value::String(vec!['h', 'e', 'l', 'l', 'o'])];
        let result = (collections::len)(&args, Span::dummy()).unwrap();
        assert_eq!(result, Value::I64(5));
    }
}
