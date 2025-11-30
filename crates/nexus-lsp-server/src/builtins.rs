//! Builtin function definitions and documentation for the Nexus language.
//!
//! This module contains metadata about all builtin functions available in Nexus,
//! including their signatures, parameter types, return types, and documentation.

use std::collections::HashMap;
use std::sync::LazyLock;

/// Information about a builtin function.
#[derive(Debug, Clone)]
pub struct BuiltinFunction {
    /// The function name
    pub name: &'static str,
    /// Parameter names and types as (name, type) pairs
    pub params: &'static [(&'static str, &'static str)],
    /// Return type
    pub return_type: &'static str,
    /// Short description
    pub description: &'static str,
    /// Detailed documentation
    pub documentation: &'static str,
}

impl BuiltinFunction {
    /// Format the function signature for display.
    pub fn signature(&self) -> String {
        let params: Vec<String> = self
            .params
            .iter()
            .map(|(name, ty)| format!("{}: {}", name, ty))
            .collect();
        format!("{}({}): {}", self.name, params.join(", "), self.return_type)
    }

    /// Format full hover documentation.
    pub fn hover_content(&self) -> String {
        format!(
            "```nexus\n{}\n```\n\n{}\n\n{}",
            self.signature(),
            self.description,
            self.documentation
        )
    }
}

/// Static map of all builtin functions.
pub static BUILTINS: LazyLock<HashMap<&'static str, BuiltinFunction>> = LazyLock::new(|| {
    let builtins = [
        // Logical operations
        BuiltinFunction {
            name: "not",
            params: &[("value", "bool")],
            return_type: "bool",
            description: "Logical NOT",
            documentation: "Returns the logical negation of the input.",
        },
        BuiltinFunction {
            name: "and",
            params: &[("a", "bool"), ("b", "bool")],
            return_type: "bool",
            description: "Logical AND",
            documentation: "Returns true if both arguments are true.",
        },
        BuiltinFunction {
            name: "or",
            params: &[("a", "bool"), ("b", "bool")],
            return_type: "bool",
            description: "Logical OR",
            documentation: "Returns true if either argument is true.",
        },
        // i64 operations
        BuiltinFunction {
            name: "addi64",
            params: &[("a", "i64"), ("b", "i64")],
            return_type: "i64",
            description: "Add two i64 values",
            documentation: "Returns the sum of two 64-bit integers.",
        },
        BuiltinFunction {
            name: "subi64",
            params: &[("a", "i64"), ("b", "i64")],
            return_type: "i64",
            description: "Subtract two i64 values",
            documentation: "Returns the first argument minus the second.",
        },
        BuiltinFunction {
            name: "muli64",
            params: &[("a", "i64"), ("b", "i64")],
            return_type: "i64",
            description: "Multiply two i64 values",
            documentation: "Returns the product of two 64-bit integers.",
        },
        BuiltinFunction {
            name: "divi64",
            params: &[("a", "i64"), ("b", "i64")],
            return_type: "i64",
            description: "Divide two i64 values",
            documentation: "Returns the integer division of the first by the second.",
        },
        BuiltinFunction {
            name: "modi64",
            params: &[("a", "i64"), ("b", "i64")],
            return_type: "i64",
            description: "Modulo of two i64 values",
            documentation: "Returns the remainder of integer division.",
        },
        BuiltinFunction {
            name: "negi64",
            params: &[("value", "i64")],
            return_type: "i64",
            description: "Negate an i64 value",
            documentation: "Returns the negation of the input.",
        },
        BuiltinFunction {
            name: "eqi64",
            params: &[("a", "i64"), ("b", "i64")],
            return_type: "bool",
            description: "Check equality of two i64 values",
            documentation: "Returns true if both values are equal.",
        },
        BuiltinFunction {
            name: "nei64",
            params: &[("a", "i64"), ("b", "i64")],
            return_type: "bool",
            description: "Check inequality of two i64 values",
            documentation: "Returns true if values are not equal.",
        },
        BuiltinFunction {
            name: "lti64",
            params: &[("a", "i64"), ("b", "i64")],
            return_type: "bool",
            description: "Less than comparison for i64",
            documentation: "Returns true if first < second.",
        },
        BuiltinFunction {
            name: "lei64",
            params: &[("a", "i64"), ("b", "i64")],
            return_type: "bool",
            description: "Less than or equal comparison for i64",
            documentation: "Returns true if first <= second.",
        },
        BuiltinFunction {
            name: "gti64",
            params: &[("a", "i64"), ("b", "i64")],
            return_type: "bool",
            description: "Greater than comparison for i64",
            documentation: "Returns true if first > second.",
        },
        BuiltinFunction {
            name: "gei64",
            params: &[("a", "i64"), ("b", "i64")],
            return_type: "bool",
            description: "Greater than or equal comparison for i64",
            documentation: "Returns true if first >= second.",
        },
        // i32 operations
        BuiltinFunction {
            name: "addi32",
            params: &[("a", "i32"), ("b", "i32")],
            return_type: "i32",
            description: "Add two i32 values",
            documentation: "Returns the sum of two 32-bit integers.",
        },
        BuiltinFunction {
            name: "subi32",
            params: &[("a", "i32"), ("b", "i32")],
            return_type: "i32",
            description: "Subtract two i32 values",
            documentation: "Returns the first argument minus the second.",
        },
        BuiltinFunction {
            name: "muli32",
            params: &[("a", "i32"), ("b", "i32")],
            return_type: "i32",
            description: "Multiply two i32 values",
            documentation: "Returns the product of two 32-bit integers.",
        },
        BuiltinFunction {
            name: "divi32",
            params: &[("a", "i32"), ("b", "i32")],
            return_type: "i32",
            description: "Divide two i32 values",
            documentation: "Returns the integer division of the first by the second.",
        },
        BuiltinFunction {
            name: "modi32",
            params: &[("a", "i32"), ("b", "i32")],
            return_type: "i32",
            description: "Modulo of two i32 values",
            documentation: "Returns the remainder of integer division.",
        },
        BuiltinFunction {
            name: "negi32",
            params: &[("value", "i32")],
            return_type: "i32",
            description: "Negate an i32 value",
            documentation: "Returns the negation of the input.",
        },
        BuiltinFunction {
            name: "eqi32",
            params: &[("a", "i32"), ("b", "i32")],
            return_type: "bool",
            description: "Check equality of two i32 values",
            documentation: "Returns true if both values are equal.",
        },
        BuiltinFunction {
            name: "nei32",
            params: &[("a", "i32"), ("b", "i32")],
            return_type: "bool",
            description: "Check inequality of two i32 values",
            documentation: "Returns true if values are not equal.",
        },
        BuiltinFunction {
            name: "lti32",
            params: &[("a", "i32"), ("b", "i32")],
            return_type: "bool",
            description: "Less than comparison for i32",
            documentation: "Returns true if first < second.",
        },
        BuiltinFunction {
            name: "lei32",
            params: &[("a", "i32"), ("b", "i32")],
            return_type: "bool",
            description: "Less than or equal comparison for i32",
            documentation: "Returns true if first <= second.",
        },
        BuiltinFunction {
            name: "gti32",
            params: &[("a", "i32"), ("b", "i32")],
            return_type: "bool",
            description: "Greater than comparison for i32",
            documentation: "Returns true if first > second.",
        },
        BuiltinFunction {
            name: "gei32",
            params: &[("a", "i32"), ("b", "i32")],
            return_type: "bool",
            description: "Greater than or equal comparison for i32",
            documentation: "Returns true if first >= second.",
        },
        // f64 operations
        BuiltinFunction {
            name: "addf64",
            params: &[("a", "f64"), ("b", "f64")],
            return_type: "f64",
            description: "Add two f64 values",
            documentation: "Returns the sum of two 64-bit floats.",
        },
        BuiltinFunction {
            name: "subf64",
            params: &[("a", "f64"), ("b", "f64")],
            return_type: "f64",
            description: "Subtract two f64 values",
            documentation: "Returns the first argument minus the second.",
        },
        BuiltinFunction {
            name: "mulf64",
            params: &[("a", "f64"), ("b", "f64")],
            return_type: "f64",
            description: "Multiply two f64 values",
            documentation: "Returns the product of two 64-bit floats.",
        },
        BuiltinFunction {
            name: "divf64",
            params: &[("a", "f64"), ("b", "f64")],
            return_type: "f64",
            description: "Divide two f64 values",
            documentation: "Returns the division of the first by the second.",
        },
        BuiltinFunction {
            name: "negf64",
            params: &[("value", "f64")],
            return_type: "f64",
            description: "Negate an f64 value",
            documentation: "Returns the negation of the input.",
        },
        BuiltinFunction {
            name: "eqf64",
            params: &[("a", "f64"), ("b", "f64")],
            return_type: "bool",
            description: "Check equality of two f64 values",
            documentation: "Returns true if both values are equal.",
        },
        BuiltinFunction {
            name: "nef64",
            params: &[("a", "f64"), ("b", "f64")],
            return_type: "bool",
            description: "Check inequality of two f64 values",
            documentation: "Returns true if values are not equal.",
        },
        BuiltinFunction {
            name: "ltf64",
            params: &[("a", "f64"), ("b", "f64")],
            return_type: "bool",
            description: "Less than comparison for f64",
            documentation: "Returns true if first < second.",
        },
        BuiltinFunction {
            name: "lef64",
            params: &[("a", "f64"), ("b", "f64")],
            return_type: "bool",
            description: "Less than or equal comparison for f64",
            documentation: "Returns true if first <= second.",
        },
        BuiltinFunction {
            name: "gtf64",
            params: &[("a", "f64"), ("b", "f64")],
            return_type: "bool",
            description: "Greater than comparison for f64",
            documentation: "Returns true if first > second.",
        },
        BuiltinFunction {
            name: "gef64",
            params: &[("a", "f64"), ("b", "f64")],
            return_type: "bool",
            description: "Greater than or equal comparison for f64",
            documentation: "Returns true if first >= second.",
        },
        // f32 operations
        BuiltinFunction {
            name: "addf32",
            params: &[("a", "f32"), ("b", "f32")],
            return_type: "f32",
            description: "Add two f32 values",
            documentation: "Returns the sum of two 32-bit floats.",
        },
        BuiltinFunction {
            name: "subf32",
            params: &[("a", "f32"), ("b", "f32")],
            return_type: "f32",
            description: "Subtract two f32 values",
            documentation: "Returns the first argument minus the second.",
        },
        BuiltinFunction {
            name: "mulf32",
            params: &[("a", "f32"), ("b", "f32")],
            return_type: "f32",
            description: "Multiply two f32 values",
            documentation: "Returns the product of two 32-bit floats.",
        },
        BuiltinFunction {
            name: "divf32",
            params: &[("a", "f32"), ("b", "f32")],
            return_type: "f32",
            description: "Divide two f32 values",
            documentation: "Returns the division of the first by the second.",
        },
        BuiltinFunction {
            name: "negf32",
            params: &[("value", "f32")],
            return_type: "f32",
            description: "Negate an f32 value",
            documentation: "Returns the negation of the input.",
        },
        // Type conversions
        BuiltinFunction {
            name: "i64",
            params: &[("value", "any")],
            return_type: "i64",
            description: "Convert to i64",
            documentation: "Converts a numeric value to 64-bit integer.",
        },
        BuiltinFunction {
            name: "i32",
            params: &[("value", "any")],
            return_type: "i32",
            description: "Convert to i32",
            documentation: "Converts a numeric value to 32-bit integer.",
        },
        BuiltinFunction {
            name: "f64",
            params: &[("value", "any")],
            return_type: "f64",
            description: "Convert to f64",
            documentation: "Converts a numeric value to 64-bit float.",
        },
        BuiltinFunction {
            name: "f32",
            params: &[("value", "any")],
            return_type: "f32",
            description: "Convert to f32",
            documentation: "Converts a numeric value to 32-bit float.",
        },
        BuiltinFunction {
            name: "bool",
            params: &[("value", "any")],
            return_type: "bool",
            description: "Convert to bool",
            documentation: "Converts a value to boolean.",
        },
        BuiltinFunction {
            name: "rune",
            params: &[("value", "i64")],
            return_type: "rune",
            description: "Convert to rune",
            documentation: "Converts an integer to a Unicode character.",
        },
        // Collection operations
        BuiltinFunction {
            name: "len",
            params: &[("collection", "any")],
            return_type: "i64",
            description: "Get length of collection",
            documentation: "Returns the length of an array, string, or bytes.",
        },
        BuiltinFunction {
            name: "push",
            params: &[("array", "[T]"), ("element", "T")],
            return_type: "[T]",
            description: "Push element to array",
            documentation: "Appends an element to the end of an array and returns the modified array.",
        },
        BuiltinFunction {
            name: "pop",
            params: &[("array", "[T]")],
            return_type: "T",
            description: "Pop element from array",
            documentation: "Removes and returns the last element from an array.",
        },
        BuiltinFunction {
            name: "concat",
            params: &[("a", "[T]"), ("b", "[T]")],
            return_type: "[T]",
            description: "Concatenate collections",
            documentation: "Joins two arrays or strings together.",
        },
        BuiltinFunction {
            name: "slice",
            params: &[("collection", "[T]"), ("start", "i64"), ("end", "i64")],
            return_type: "[T]",
            description: "Get slice of collection",
            documentation: "Returns a portion of the collection from start to end (exclusive).",
        },
        BuiltinFunction {
            name: "contains",
            params: &[("collection", "[T]"), ("element", "T")],
            return_type: "bool",
            description: "Check if collection contains element",
            documentation: "Returns true if the element is in the collection.",
        },
        // Utility functions
        BuiltinFunction {
            name: "typeof",
            params: &[("value", "any")],
            return_type: "string",
            description: "Get type name",
            documentation: "Returns the type name of a value as a string.",
        },
        BuiltinFunction {
            name: "is_none",
            params: &[("value", "any")],
            return_type: "bool",
            description: "Check if value is None",
            documentation: "Returns true if the value is None.",
        },
        BuiltinFunction {
            name: "unwrap",
            params: &[("optional", "unknown<T, None>")],
            return_type: "T",
            description: "Unwrap optional value",
            documentation: "Extracts the value from an optional, panics if None.",
        },
        BuiltinFunction {
            name: "panic",
            params: &[("message", "string")],
            return_type: "void",
            description: "Panic with message",
            documentation: "Terminates execution with an error message.",
        },
        BuiltinFunction {
            name: "assert",
            params: &[("condition", "bool")],
            return_type: "void",
            description: "Assert condition",
            documentation: "Panics if the condition is false.",
        },
        BuiltinFunction {
            name: "assert_eq",
            params: &[("a", "any"), ("b", "any")],
            return_type: "void",
            description: "Assert equality",
            documentation: "Panics if the two values are not equal.",
        },
        // Bitwise operations
        BuiltinFunction {
            name: "band",
            params: &[("a", "i64"), ("b", "i64")],
            return_type: "i64",
            description: "Bitwise AND",
            documentation: "Returns the bitwise AND of two integers.",
        },
        BuiltinFunction {
            name: "bor",
            params: &[("a", "i64"), ("b", "i64")],
            return_type: "i64",
            description: "Bitwise OR",
            documentation: "Returns the bitwise OR of two integers.",
        },
        BuiltinFunction {
            name: "bxor",
            params: &[("a", "i64"), ("b", "i64")],
            return_type: "i64",
            description: "Bitwise XOR",
            documentation: "Returns the bitwise XOR of two integers.",
        },
        BuiltinFunction {
            name: "bnot",
            params: &[("value", "i64")],
            return_type: "i64",
            description: "Bitwise NOT",
            documentation: "Returns the bitwise complement of an integer.",
        },
        BuiltinFunction {
            name: "shl",
            params: &[("value", "i64"), ("bits", "i64")],
            return_type: "i64",
            description: "Shift left",
            documentation: "Shifts the bits left by the specified amount.",
        },
        BuiltinFunction {
            name: "shr",
            params: &[("value", "i64"), ("bits", "i64")],
            return_type: "i64",
            description: "Shift right",
            documentation: "Shifts the bits right by the specified amount.",
        },
        // compat.io functions
        BuiltinFunction {
            name: "print",
            params: &[("values", "...any")],
            return_type: "void",
            description: "Print without newline",
            documentation: "Prints values to stdout without a trailing newline.\n\nRequires: `use { print } from compat.io`",
        },
        BuiltinFunction {
            name: "println",
            params: &[("values", "...any")],
            return_type: "void",
            description: "Print with newline",
            documentation: "Prints values to stdout with a trailing newline.\n\nRequires: `use { println } from compat.io`",
        },
        // String operations
        BuiltinFunction {
            name: "str",
            params: &[("value", "any")],
            return_type: "string",
            description: "Convert to string",
            documentation: "Converts a value to its string representation.",
        },
        BuiltinFunction {
            name: "char_at",
            params: &[("s", "string"), ("index", "i64")],
            return_type: "rune",
            description: "Get character at index",
            documentation: "Returns the character at the specified index in the string.",
        },
        BuiltinFunction {
            name: "split",
            params: &[("s", "string"), ("delimiter", "string")],
            return_type: "[string]",
            description: "Split string by delimiter",
            documentation: "Splits a string into an array of substrings using the given delimiter.",
        },
        BuiltinFunction {
            name: "join",
            params: &[("parts", "[string]"), ("separator", "string")],
            return_type: "string",
            description: "Join strings with separator",
            documentation: "Joins an array of strings using the given separator.",
        },
        BuiltinFunction {
            name: "trim",
            params: &[("s", "string")],
            return_type: "string",
            description: "Trim whitespace",
            documentation: "Removes leading and trailing whitespace from a string.",
        },
        BuiltinFunction {
            name: "starts_with",
            params: &[("s", "string"), ("prefix", "string")],
            return_type: "bool",
            description: "Check string prefix",
            documentation: "Returns true if the string starts with the given prefix.",
        },
        BuiltinFunction {
            name: "ends_with",
            params: &[("s", "string"), ("suffix", "string")],
            return_type: "bool",
            description: "Check string suffix",
            documentation: "Returns true if the string ends with the given suffix.",
        },
        // Math operations
        BuiltinFunction {
            name: "abs",
            params: &[("value", "i64")],
            return_type: "i64",
            description: "Absolute value (i64)",
            documentation: "Returns the absolute value of an integer.",
        },
        BuiltinFunction {
            name: "absf64",
            params: &[("value", "f64")],
            return_type: "f64",
            description: "Absolute value (f64)",
            documentation: "Returns the absolute value of a 64-bit float.",
        },
        BuiltinFunction {
            name: "min",
            params: &[("a", "i64"), ("b", "i64")],
            return_type: "i64",
            description: "Minimum of two values",
            documentation: "Returns the smaller of two integers.",
        },
        BuiltinFunction {
            name: "max",
            params: &[("a", "i64"), ("b", "i64")],
            return_type: "i64",
            description: "Maximum of two values",
            documentation: "Returns the larger of two integers.",
        },
        BuiltinFunction {
            name: "sqrt",
            params: &[("value", "f64")],
            return_type: "f64",
            description: "Square root",
            documentation: "Returns the square root of a 64-bit float.",
        },
        BuiltinFunction {
            name: "pow",
            params: &[("base", "f64"), ("exponent", "f64")],
            return_type: "f64",
            description: "Power function",
            documentation: "Returns base raised to the power of exponent.",
        },
        BuiltinFunction {
            name: "floor",
            params: &[("value", "f64")],
            return_type: "f64",
            description: "Floor function",
            documentation: "Returns the largest integer less than or equal to the input.",
        },
        BuiltinFunction {
            name: "ceil",
            params: &[("value", "f64")],
            return_type: "f64",
            description: "Ceiling function",
            documentation: "Returns the smallest integer greater than or equal to the input.",
        },
        BuiltinFunction {
            name: "round",
            params: &[("value", "f64")],
            return_type: "f64",
            description: "Round to nearest integer",
            documentation: "Rounds the input to the nearest integer.",
        },
    ];

    builtins.into_iter().map(|b| (b.name, b)).collect()
});

/// Get a builtin function by name.
pub fn get_builtin(name: &str) -> Option<&'static BuiltinFunction> {
    BUILTINS.get(name)
}
