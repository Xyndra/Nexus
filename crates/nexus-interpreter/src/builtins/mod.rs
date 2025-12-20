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
//! - `rune_ops` - Rune comparison operations
//! - `conversions` - Type conversion functions
//! - `collections` - Array/rune array/bytes operations
//! - `utility` - Utility functions (typeof, assert, etc.)
//! - `compat_io` - Compatibility IO functions (print, println)
//! - `plat_console` - Platform console functions (readln)
//! - `bitwise` - Bitwise operations
//! - `strings` - Rune array manipulation functions

mod bitwise;
mod collections;
mod compat_fs;
mod compat_io;
mod compat_proc;
mod conversions;
mod f32_ops;
mod f64_ops;
mod i32_ops;
mod i64_ops;
mod logical;
pub(crate) mod macros;
mod plat_console;
mod rune_ops;
mod strings;
mod utility;

use crate::Value;
use nexus_core::{FunctionColor, NexusResult, Span};
use std::collections::HashMap;

#[cfg(feature = "builtin-docs")]
use serde::Deserialize;
#[cfg(feature = "builtin-docs")]
use std::sync::LazyLock;

/// Documentation for a builtin function, loaded from JSON5
#[cfg(feature = "builtin-docs")]
#[derive(Debug, Clone, Deserialize)]
pub struct BuiltinDocs {
    /// Short description
    pub description: String,
    /// Detailed documentation
    pub documentation: String,
}

/// Static map of builtin documentation, loaded from docs.json5
#[cfg(feature = "builtin-docs")]
static BUILTIN_DOCS: LazyLock<HashMap<String, BuiltinDocs>> = LazyLock::new(|| {
    let json5_content = include_str!("docs.json5");
    json5::from_str(json5_content).unwrap_or_else(|e| {
        eprintln!("Warning: Failed to parse builtin docs: {}", e);
        HashMap::new()
    })
});

/// Get documentation for a builtin by name
#[cfg(feature = "builtin-docs")]
pub fn get_builtin_docs(name: &str) -> Option<&'static BuiltinDocs> {
    BUILTIN_DOCS.get(name)
}

/// Parameter information for a builtin function
#[derive(Debug, Clone)]
pub struct BuiltinParam {
    /// Parameter name
    pub name: &'static str,
    /// Parameter type
    pub ty: &'static str,
}

/// A builtin function
pub struct Builtin {
    /// Function name
    pub name: String,
    /// Number of expected arguments (None for variadic)
    pub arity: Option<usize>,
    /// Parameter information
    pub params: Vec<BuiltinParam>,
    /// Return type
    pub return_type: &'static str,
    /// Required import module (None if no import needed)
    pub required_import: Option<&'static str>,
    /// Function color requirement
    pub color: FunctionColor,
    /// The function implementation
    pub func: fn(&[Value], Span) -> NexusResult<Value>,
}

/// Registry of builtin functions
pub struct BuiltinRegistry {
    /// Core builtins that are always available
    builtins: HashMap<String, Builtin>,
    /// Builtins from compat.io module that require import
    compat_io_builtins: HashMap<String, Builtin>,
    /// Builtins from compat.fs module that require import
    compat_fs_builtins: HashMap<String, Builtin>,
    /// Builtins from compat.proc module that require import
    compat_proc_builtins: HashMap<String, Builtin>,
    /// Builtins from plat.console module that require import
    plat_console_builtins: HashMap<String, Builtin>,
}

/// Macro to register a builtin function with less boilerplate
macro_rules! register_builtins {
    ($registry:expr, $( ($name:expr, $params:expr, $ret:expr, $func:expr) ),* $(,)?) => {
        $(
            $registry.register($name, $params, $ret, $func);
        )*
    };
}

/// Macro to register compat.io builtins
macro_rules! register_compat_io_builtins {
    ($registry:expr, $( ($name:expr, $params:expr, $ret:expr, $func:expr) ),* $(,)?) => {
        $(
            $registry.register_compat_io($name, $params, $ret, $func);
        )*
    };
}

/// Macro to register compat.fs builtins
macro_rules! register_compat_fs_builtins {
    ($registry:expr, $( ($name:expr, $params:expr, $ret:expr, $func:expr) ),* $(,)?) => {
        $(
            $registry.register_compat_fs($name, $params, $ret, $func);
        )*
    };
}

/// Macro to register compat.proc builtins
macro_rules! register_compat_proc_builtins {
    ($registry:expr, $( ($name:expr, $params:expr, $ret:expr, $func:expr) ),* $(,)?) => {
        $(
            $registry.register_compat_proc($name, $params, $ret, $func);
        )*
    };
}

/// Macro to register plat.console builtins
macro_rules! register_plat_console_builtins {
    ($registry:expr, $( ($name:expr, $params:expr, $ret:expr, $func:expr) ),* $(,)?) => {
        $(
            $registry.register_plat_console($name, $params, $ret, $func);
        )*
    };
}

impl BuiltinRegistry {
    /// Create a new registry with all builtin functions
    pub fn new() -> Self {
        let mut registry = Self {
            builtins: HashMap::new(),
            compat_io_builtins: HashMap::new(),
            compat_fs_builtins: HashMap::new(),
            compat_proc_builtins: HashMap::new(),
            plat_console_builtins: HashMap::new(),
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

    /// Get a compat.fs builtin by name (requires import)
    pub fn get_compat_fs(&self, name: &str) -> Option<&Builtin> {
        self.compat_fs_builtins.get(name)
    }

    /// Get a compat.proc builtin by name (requires import)
    pub fn get_compat_proc(&self, name: &str) -> Option<&Builtin> {
        self.compat_proc_builtins.get(name)
    }

    /// Get a plat.console builtin by name (requires import)
    pub fn get_plat_console(&self, name: &str) -> Option<&Builtin> {
        self.plat_console_builtins.get(name)
    }

    /// Check if a symbol is a compat.io builtin (requires import)
    pub fn is_compat_io(&self, name: &str) -> bool {
        self.compat_io_builtins.contains_key(name)
    }

    /// Check if a symbol is a compat.fs builtin (requires import)
    pub fn is_compat_fs(&self, name: &str) -> bool {
        self.compat_fs_builtins.contains_key(name)
    }

    /// Check if a symbol is a compat.proc builtin (requires import)
    pub fn is_compat_proc(&self, name: &str) -> bool {
        self.compat_proc_builtins.contains_key(name)
    }

    /// Check if a symbol is a plat.console builtin (requires import)
    pub fn is_plat_console(&self, name: &str) -> bool {
        self.plat_console_builtins.contains_key(name)
    }

    /// Get all builtin names (core builtins only)
    pub fn builtin_names(&self) -> impl Iterator<Item = &str> {
        self.builtins.keys().map(|s| s.as_str())
    }

    /// Get all compat.io builtin names
    pub fn compat_io_names(&self) -> impl Iterator<Item = &str> {
        self.compat_io_builtins.keys().map(|s| s.as_str())
    }

    /// Get all compat.fs builtin names
    pub fn compat_fs_names(&self) -> impl Iterator<Item = &str> {
        self.compat_fs_builtins.keys().map(|s| s.as_str())
    }

    /// Get all compat.proc builtin names
    pub fn compat_proc_names(&self) -> impl Iterator<Item = &str> {
        self.compat_proc_builtins.keys().map(|s| s.as_str())
    }

    /// Get all plat.console builtin names
    pub fn plat_console_names(&self) -> impl Iterator<Item = &str> {
        self.plat_console_builtins.keys().map(|s| s.as_str())
    }

    /// Iterate over all core builtins
    pub fn iter(&self) -> impl Iterator<Item = &Builtin> {
        self.builtins.values()
    }

    /// Iterate over all compat.io builtins
    pub fn iter_compat_io(&self) -> impl Iterator<Item = &Builtin> {
        self.compat_io_builtins.values()
    }

    /// Iterate over all compat.fs builtins
    pub fn iter_compat_fs(&self) -> impl Iterator<Item = &Builtin> {
        self.compat_fs_builtins.values()
    }

    /// Iterate over all compat.proc builtins
    pub fn iter_compat_proc(&self) -> impl Iterator<Item = &Builtin> {
        self.compat_proc_builtins.values()
    }

    /// Iterate over all plat.console builtins
    pub fn iter_plat_console(&self) -> impl Iterator<Item = &Builtin> {
        self.plat_console_builtins.values()
    }

    /// Register a builtin function
    fn register(
        &mut self,
        name: &str,
        params: &[(&'static str, &'static str)],
        return_type: &'static str,
        func: fn(&[Value], Span) -> NexusResult<Value>,
    ) {
        let param_vec: Vec<BuiltinParam> = params
            .iter()
            .map(|(n, t)| BuiltinParam { name: n, ty: t })
            .collect();
        let arity = if params.iter().any(|(_, t)| t.starts_with("...")) {
            None
        } else {
            Some(params.len())
        };
        self.builtins.insert(
            name.to_string(),
            Builtin {
                name: name.to_string(),
                arity,
                params: param_vec,
                return_type,
                required_import: None,
                color: FunctionColor::Std,
                func,
            },
        );
    }

    /// Register a compat.io builtin function
    fn register_compat_io(
        &mut self,
        name: &str,
        params: &[(&'static str, &'static str)],
        return_type: &'static str,
        func: fn(&[Value], Span) -> NexusResult<Value>,
    ) {
        let param_vec: Vec<BuiltinParam> = params
            .iter()
            .map(|(n, t)| BuiltinParam { name: n, ty: t })
            .collect();
        let arity = if params.iter().any(|(_, t)| t.starts_with("...")) {
            None
        } else {
            Some(params.len())
        };
        self.compat_io_builtins.insert(
            name.to_string(),
            Builtin {
                name: name.to_string(),
                arity,
                params: param_vec,
                return_type,
                required_import: Some("compat.io"),
                color: FunctionColor::Compat,
                func,
            },
        );
    }

    /// Register a compat.fs builtin function
    fn register_compat_fs(
        &mut self,
        name: &str,
        params: &[(&'static str, &'static str)],
        return_type: &'static str,
        func: fn(&[Value], Span) -> NexusResult<Value>,
    ) {
        let param_vec: Vec<BuiltinParam> = params
            .iter()
            .map(|(n, t)| BuiltinParam { name: n, ty: t })
            .collect();
        let arity = if params.iter().any(|(_, t)| t.starts_with("...")) {
            None
        } else {
            Some(params.len())
        };
        self.compat_fs_builtins.insert(
            name.to_string(),
            Builtin {
                name: name.to_string(),
                arity,
                params: param_vec,
                return_type,
                required_import: Some("compat.fs"),
                color: FunctionColor::Compat,
                func,
            },
        );
    }

    /// Register a compat.proc builtin function
    fn register_compat_proc(
        &mut self,
        name: &str,
        params: &[(&'static str, &'static str)],
        return_type: &'static str,
        func: fn(&[Value], Span) -> NexusResult<Value>,
    ) {
        let param_vec: Vec<BuiltinParam> = params
            .iter()
            .map(|(n, t)| BuiltinParam { name: n, ty: t })
            .collect();
        let arity = if params.iter().any(|(_, t)| t.starts_with("...")) {
            None
        } else {
            Some(params.len())
        };
        self.compat_proc_builtins.insert(
            name.to_string(),
            Builtin {
                name: name.to_string(),
                arity,
                params: param_vec,
                return_type,
                required_import: Some("compat.proc"),
                color: FunctionColor::Compat,
                func,
            },
        );
    }

    /// Register a plat.console builtin function
    fn register_plat_console(
        &mut self,
        name: &str,
        params: &[(&'static str, &'static str)],
        return_type: &'static str,
        func: fn(&[Value], Span) -> NexusResult<Value>,
    ) {
        let param_vec: Vec<BuiltinParam> = params
            .iter()
            .map(|(n, t)| BuiltinParam { name: n, ty: t })
            .collect();
        let arity = if params.iter().any(|(_, t)| t.starts_with("...")) {
            None
        } else {
            Some(params.len())
        };
        self.plat_console_builtins.insert(
            name.to_string(),
            Builtin {
                name: name.to_string(),
                arity,
                params: param_vec,
                return_type,
                required_import: Some("plat.console"),
                color: FunctionColor::Plat,
                func,
            },
        );
    }

    /// Register all builtin functions
    fn register_all(&mut self) {
        // Logical operations
        register_builtins!(
            self,
            ("not", &[("value", "bool")], "bool", logical::builtin_not),
            (
                "and",
                &[("a", "bool"), ("b", "bool")],
                "bool",
                logical::builtin_and
            ),
            (
                "or",
                &[("a", "bool"), ("b", "bool")],
                "bool",
                logical::builtin_or
            ),
        );

        // i64 operations
        register_builtins!(
            self,
            (
                "addi64",
                &[("a", "i64"), ("b", "i64")],
                "i64",
                i64_ops::addi64
            ),
            (
                "subi64",
                &[("a", "i64"), ("b", "i64")],
                "i64",
                i64_ops::subi64
            ),
            (
                "muli64",
                &[("a", "i64"), ("b", "i64")],
                "i64",
                i64_ops::muli64
            ),
            (
                "divi64",
                &[("a", "i64"), ("b", "i64")],
                "i64",
                i64_ops::divi64
            ),
            (
                "modi64",
                &[("a", "i64"), ("b", "i64")],
                "i64",
                i64_ops::modi64
            ),
            ("negi64", &[("value", "i64")], "i64", i64_ops::negi64),
            (
                "eqi64",
                &[("a", "i64"), ("b", "i64")],
                "bool",
                i64_ops::eqi64
            ),
            (
                "nei64",
                &[("a", "i64"), ("b", "i64")],
                "bool",
                i64_ops::nei64
            ),
            (
                "lti64",
                &[("a", "i64"), ("b", "i64")],
                "bool",
                i64_ops::lti64
            ),
            (
                "lei64",
                &[("a", "i64"), ("b", "i64")],
                "bool",
                i64_ops::lei64
            ),
            (
                "gti64",
                &[("a", "i64"), ("b", "i64")],
                "bool",
                i64_ops::gti64
            ),
            (
                "gei64",
                &[("a", "i64"), ("b", "i64")],
                "bool",
                i64_ops::gei64
            ),
        );

        // i32 operations
        register_builtins!(
            self,
            (
                "addi32",
                &[("a", "i32"), ("b", "i32")],
                "i32",
                i32_ops::addi32
            ),
            (
                "subi32",
                &[("a", "i32"), ("b", "i32")],
                "i32",
                i32_ops::subi32
            ),
            (
                "muli32",
                &[("a", "i32"), ("b", "i32")],
                "i32",
                i32_ops::muli32
            ),
            (
                "divi32",
                &[("a", "i32"), ("b", "i32")],
                "i32",
                i32_ops::divi32
            ),
            (
                "modi32",
                &[("a", "i32"), ("b", "i32")],
                "i32",
                i32_ops::modi32
            ),
            ("negi32", &[("value", "i32")], "i32", i32_ops::negi32),
            (
                "eqi32",
                &[("a", "i32"), ("b", "i32")],
                "bool",
                i32_ops::eqi32
            ),
            (
                "nei32",
                &[("a", "i32"), ("b", "i32")],
                "bool",
                i32_ops::nei32
            ),
            (
                "lti32",
                &[("a", "i32"), ("b", "i32")],
                "bool",
                i32_ops::lti32
            ),
            (
                "lei32",
                &[("a", "i32"), ("b", "i32")],
                "bool",
                i32_ops::lei32
            ),
            (
                "gti32",
                &[("a", "i32"), ("b", "i32")],
                "bool",
                i32_ops::gti32
            ),
            (
                "gei32",
                &[("a", "i32"), ("b", "i32")],
                "bool",
                i32_ops::gei32
            ),
        );

        // f64 operations
        register_builtins!(
            self,
            (
                "addf64",
                &[("a", "f64"), ("b", "f64")],
                "f64",
                f64_ops::addf64
            ),
            (
                "subf64",
                &[("a", "f64"), ("b", "f64")],
                "f64",
                f64_ops::subf64
            ),
            (
                "mulf64",
                &[("a", "f64"), ("b", "f64")],
                "f64",
                f64_ops::mulf64
            ),
            (
                "divf64",
                &[("a", "f64"), ("b", "f64")],
                "f64",
                f64_ops::divf64
            ),
            ("negf64", &[("value", "f64")], "f64", f64_ops::negf64),
            (
                "eqf64",
                &[("a", "f64"), ("b", "f64")],
                "bool",
                f64_ops::eqf64
            ),
            (
                "nef64",
                &[("a", "f64"), ("b", "f64")],
                "bool",
                f64_ops::nef64
            ),
            (
                "ltf64",
                &[("a", "f64"), ("b", "f64")],
                "bool",
                f64_ops::ltf64
            ),
            (
                "lef64",
                &[("a", "f64"), ("b", "f64")],
                "bool",
                f64_ops::lef64
            ),
            (
                "gtf64",
                &[("a", "f64"), ("b", "f64")],
                "bool",
                f64_ops::gtf64
            ),
            (
                "gef64",
                &[("a", "f64"), ("b", "f64")],
                "bool",
                f64_ops::gef64
            ),
        );

        // f32 operations
        register_builtins!(
            self,
            (
                "addf32",
                &[("a", "f32"), ("b", "f32")],
                "f32",
                f32_ops::addf32
            ),
            (
                "subf32",
                &[("a", "f32"), ("b", "f32")],
                "f32",
                f32_ops::subf32
            ),
            (
                "mulf32",
                &[("a", "f32"), ("b", "f32")],
                "f32",
                f32_ops::mulf32
            ),
            (
                "divf32",
                &[("a", "f32"), ("b", "f32")],
                "f32",
                f32_ops::divf32
            ),
            ("negf32", &[("value", "f32")], "f32", f32_ops::negf32),
        );

        // Rune operations
        register_builtins!(
            self,
            (
                "eqr",
                &[("a", "rune"), ("b", "rune")],
                "bool",
                rune_ops::eqr
            ),
            (
                "ner",
                &[("a", "rune"), ("b", "rune")],
                "bool",
                rune_ops::ner
            ),
            (
                "ltr",
                &[("a", "rune"), ("b", "rune")],
                "bool",
                rune_ops::ltr
            ),
            (
                "ler",
                &[("a", "rune"), ("b", "rune")],
                "bool",
                rune_ops::ler
            ),
            (
                "gtr",
                &[("a", "rune"), ("b", "rune")],
                "bool",
                rune_ops::gtr
            ),
            (
                "ger",
                &[("a", "rune"), ("b", "rune")],
                "bool",
                rune_ops::ger
            ),
        );

        // Type conversions
        register_builtins!(
            self,
            ("i64", &[("value", "any")], "i64", conversions::to_i64),
            ("i32", &[("value", "any")], "i32", conversions::to_i32),
            ("f64", &[("value", "any")], "f64", conversions::to_f64),
            ("f32", &[("value", "any")], "f32", conversions::to_f32),
            ("bool", &[("value", "any")], "bool", conversions::to_bool),
            ("rune", &[("value", "any")], "rune", conversions::to_rune),
        );

        // Collection operations
        register_builtins!(
            self,
            ("len", &[("collection", "any")], "i64", collections::len),
            (
                "push",
                &[("array", "[]any"), ("value", "any")],
                "[]any",
                collections::push
            ),
            ("pop", &[("array", "[]any")], "any", collections::pop),
            (
                "concat",
                &[("a", "any"), ("b", "any")],
                "any",
                collections::concat
            ),
            (
                "slice",
                &[("collection", "any"), ("start", "i64"), ("end", "i64")],
                "any",
                collections::slice
            ),
            (
                "contains",
                &[("collection", "any"), ("value", "any")],
                "bool",
                collections::contains
            ),
        );

        // Utility functions
        register_builtins!(
            self,
            ("typeof", &[("value", "any")], "[dyn]rune", utility::type_of),
            ("str", &[("value", "any")], "[dyn]rune", utility::str),
            ("is_none", &[("value", "any")], "bool", utility::is_none),
            ("unwrap", &[("value", "any")], "any", utility::unwrap),
            ("panic", &[("message", "[dyn]rune")], "void", utility::panic),
            ("assert", &[("condition", "bool")], "void", utility::assert),
            (
                "assert_eq",
                &[("a", "any"), ("b", "any")],
                "void",
                utility::assert_eq
            ),
        );

        // Bitwise operations
        register_builtins!(
            self,
            ("band", &[("a", "i64"), ("b", "i64")], "i64", bitwise::band),
            ("bor", &[("a", "i64"), ("b", "i64")], "i64", bitwise::bor),
            ("bxor", &[("a", "i64"), ("b", "i64")], "i64", bitwise::bxor),
            ("bnot", &[("value", "i64")], "i64", bitwise::bnot),
            (
                "shl",
                &[("value", "i64"), ("amount", "i64")],
                "i64",
                bitwise::shl
            ),
            (
                "shr",
                &[("value", "i64"), ("amount", "i64")],
                "i64",
                bitwise::shr
            ),
        );

        // Rune array manipulation functions (std - always available)
        register_builtins!(
            self,
            (
                "parse_i64",
                &[("s", "[dyn]rune")],
                "i64",
                strings::parse_i64
            ),
            (
                "split",
                &[("s", "[dyn]rune"), ("delimiter", "[dyn]rune")],
                "[dyn][dyn]rune",
                strings::split
            ),
            ("trim", &[("s", "[dyn]rune")], "[dyn]rune", strings::trim),
            (
                "starts_with",
                &[("s", "[dyn]rune"), ("prefix", "[dyn]rune")],
                "bool",
                strings::starts_with
            ),
            (
                "ends_with",
                &[("s", "[dyn]rune"), ("suffix", "[dyn]rune")],
                "bool",
                strings::ends_with
            ),
            ("is_empty", &[("s", "[dyn]rune")], "bool", strings::is_empty),
            (
                "join",
                &[("arr", "[dyn][dyn]rune"), ("delimiter", "[dyn]rune")],
                "[dyn]rune",
                strings::join
            ),
            (
                "eqs",
                &[("a", "[dyn]rune"), ("b", "[dyn]rune")],
                "bool",
                strings::eqs
            ),
        );

        // compat.io module (requires import)
        register_compat_io_builtins!(
            self,
            ("print", &[("values", "...any")], "void", compat_io::print),
            (
                "println",
                &[("values", "...any")],
                "void",
                compat_io::println
            ),
        );

        // compat.fs module (requires import - file system operations)
        register_compat_fs_builtins!(
            self,
            (
                "read_file",
                &[("path", "[dyn]rune")],
                "[dyn]rune",
                compat_fs::read_file
            ),
            (
                "write_file",
                &[("path", "[dyn]rune"), ("content", "[dyn]rune")],
                "bool",
                compat_fs::write_file
            ),
            (
                "file_exists",
                &[("path", "[dyn]rune")],
                "bool",
                compat_fs::file_exists
            ),
        );

        // compat.proc module (requires import - process operations)
        register_compat_proc_builtins!(
            self,
            ("getargs", &[], "[dyn][dyn]rune", compat_proc::getargs),
            ("exit", &[("code", "i64")], "void", compat_proc::exit),
            (
                "getenv",
                &[("name", "[dyn]rune")],
                "[dyn]rune",
                compat_proc::getenv
            ),
        );

        // plat.console module (requires import - platform-specific console operations)
        register_plat_console_builtins!(self, ("readln", &[], "[dyn]rune", plat_console::readln),);
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
