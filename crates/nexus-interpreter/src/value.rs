//! Runtime values for the Nexus interpreter.
//!
//! This module defines the Value type that represents all runtime values
//! in the Nexus programming language.

use nexus_types::StructInstance;
use std::fmt;

use crate::LambdaValue;

/// A runtime value in Nexus
#[derive(Debug, Clone, Default)]
pub enum Value {
    /// Void (no value)
    #[default]
    Void,
    /// Boolean
    Bool(bool),
    /// Signed 32-bit integer
    I32(i32),
    /// Signed 64-bit integer
    I64(i64),
    /// 32-bit floating point
    F32(f32),
    /// 64-bit floating point
    F64(f64),
    /// Unicode code point
    Rune(char),
    /// String (array of runes)
    String(Vec<char>),
    /// Byte array
    Bytes(Vec<u8>),
    /// Array of values
    Array(Vec<Value>),
    /// Struct instance
    Struct(Box<StructInstance>),
    /// Lambda/closure
    Lambda(Box<LambdaValue>),
    /// None value (for optionals)
    None,
    /// Error value (for results)
    Error(String),
    /// Unknown value (sum type instance)
    Unknown {
        /// Index of the current variant
        variant_index: usize,
        /// The actual value
        value: Box<Value>,
    },
}

impl Value {
    /// Check if this value is truthy
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Void => false,
            Value::Bool(b) => *b,
            Value::I32(n) => *n != 0,
            Value::I64(n) => *n != 0,
            Value::F32(n) => *n != 0.0,
            Value::F64(n) => *n != 0.0,
            Value::Rune(c) => *c != '\0',
            Value::String(s) => !s.is_empty(),
            Value::Bytes(b) => !b.is_empty(),
            Value::Array(a) => !a.is_empty(),
            Value::Struct(_) => true,
            Value::Lambda(_) => true,
            Value::None => false,
            Value::Error(_) => false,
            Value::Unknown { value, .. } => value.is_truthy(),
        }
    }

    /// Check if this value equals another
    pub fn equals(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Void, Value::Void) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::I32(a), Value::I32(b)) => a == b,
            (Value::I64(a), Value::I64(b)) => a == b,
            (Value::I32(a), Value::I64(b)) => (*a as i64) == *b,
            (Value::I64(a), Value::I32(b)) => *a == (*b as i64),
            (Value::F32(a), Value::F32(b)) => (a - b).abs() < f32::EPSILON,
            (Value::F64(a), Value::F64(b)) => (a - b).abs() < f64::EPSILON,
            (Value::F32(a), Value::F64(b)) => ((*a as f64) - b).abs() < f64::EPSILON,
            (Value::F64(a), Value::F32(b)) => (a - (*b as f64)).abs() < f64::EPSILON,
            (Value::Rune(a), Value::Rune(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Bytes(a), Value::Bytes(b)) => a == b,
            (Value::None, Value::None) => true,
            (Value::Array(a), Value::Array(b)) => {
                if a.len() != b.len() {
                    return false;
                }
                a.iter().zip(b.iter()).all(|(x, y)| x.equals(y))
            }
            (Value::Unknown { value: a, .. }, Value::Unknown { value: b, .. }) => a.equals(b),
            _ => false,
        }
    }

    /// Get the type name of this value
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Void => "void",
            Value::Bool(_) => "bool",
            Value::I32(_) => "i32",
            Value::I64(_) => "i64",
            Value::F32(_) => "f32",
            Value::F64(_) => "f64",
            Value::Rune(_) => "rune",
            Value::String(_) => "[]rune",
            Value::Bytes(_) => "[]u8",
            Value::Array(_) => "[]",
            Value::Struct(_) => "struct",
            Value::Lambda(_) => "lambda",
            Value::None => "None",
            Value::Error(_) => "Error",
            Value::Unknown { .. } => "unknown",
        }
    }

    /// Convert to display string
    pub fn to_display_string(&self) -> String {
        match self {
            Value::Void => "void".to_string(),
            Value::Bool(b) => b.to_string(),
            Value::I32(n) => n.to_string(),
            Value::I64(n) => n.to_string(),
            Value::F32(n) => n.to_string(),
            Value::F64(n) => n.to_string(),
            Value::Rune(c) => c.to_string(),
            Value::String(s) => s.iter().collect(),
            Value::Bytes(b) => format!("{:?}", b),
            Value::Array(a) => {
                let elements: Vec<String> = a.iter().map(|v| v.to_display_string()).collect();
                format!("[{}]", elements.join(", "))
            }
            Value::Struct(s) => format!("{} {{ ... }}", s.def.name),
            Value::Lambda(_) => "<lambda>".to_string(),
            Value::None => "None".to_string(),
            Value::Error(e) => format!("Error({})", e),
            Value::Unknown { value, .. } => value.to_display_string(),
        }
    }

    /// Check if this is a None value
    pub fn is_none(&self) -> bool {
        matches!(self, Value::None)
    }

    /// Check if this is an Error value
    pub fn is_error(&self) -> bool {
        matches!(self, Value::Error(_))
    }

    /// Try to convert to i64
    pub fn as_i64(&self) -> Option<i64> {
        match self {
            Value::I64(n) => Some(*n),
            Value::I32(n) => Some(*n as i64),
            Value::F64(n) => Some(*n as i64),
            Value::F32(n) => Some(*n as i64),
            Value::Bool(b) => Some(if *b { 1 } else { 0 }),
            Value::Rune(c) => Some(*c as i64),
            _ => None,
        }
    }

    /// Try to convert to i32
    pub fn as_i32(&self) -> Option<i32> {
        match self {
            Value::I32(n) => Some(*n),
            Value::I64(n) => Some(*n as i32),
            Value::F64(n) => Some(*n as i32),
            Value::F32(n) => Some(*n as i32),
            Value::Bool(b) => Some(if *b { 1 } else { 0 }),
            Value::Rune(c) => Some(*c as i32),
            _ => None,
        }
    }

    /// Try to convert to f64
    pub fn as_f64(&self) -> Option<f64> {
        match self {
            Value::F64(n) => Some(*n),
            Value::F32(n) => Some(*n as f64),
            Value::I64(n) => Some(*n as f64),
            Value::I32(n) => Some(*n as f64),
            _ => None,
        }
    }

    /// Try to convert to f32
    pub fn as_f32(&self) -> Option<f32> {
        match self {
            Value::F32(n) => Some(*n),
            Value::F64(n) => Some(*n as f32),
            Value::I64(n) => Some(*n as f32),
            Value::I32(n) => Some(*n as f32),
            _ => None,
        }
    }

    /// Try to convert to bool
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(b) => Some(*b),
            _ => None,
        }
    }

    /// Try to get as string
    pub fn as_string(&self) -> Option<String> {
        match self {
            Value::String(s) => Some(s.iter().collect()),
            _ => None,
        }
    }

    /// Try to get as array
    pub fn as_array(&self) -> Option<&Vec<Value>> {
        match self {
            Value::Array(a) => Some(a),
            _ => None,
        }
    }

    /// Try to get as mutable array
    pub fn as_array_mut(&mut self) -> Option<&mut Vec<Value>> {
        match self {
            Value::Array(a) => Some(a),
            _ => None,
        }
    }

    /// Get the length if this is a collection type
    pub fn len(&self) -> Option<usize> {
        match self {
            Value::String(s) => Some(s.len()),
            Value::Bytes(b) => Some(b.len()),
            Value::Array(a) => Some(a.len()),
            _ => None,
        }
    }

    /// Check if this collection is empty
    pub fn is_empty(&self) -> Option<bool> {
        self.len().map(|l| l == 0)
    }

    /// Unwrap the inner value of an Unknown
    pub fn unwrap_unknown(self) -> Value {
        match self {
            Value::Unknown { value, .. } => *value,
            other => other,
        }
    }

    /// Create an Unknown value
    pub fn unknown(variant_index: usize, value: Value) -> Value {
        Value::Unknown {
            variant_index,
            value: Box::new(value),
        }
    }

    /// Create a string value from a Rust string
    pub fn from_string(s: impl AsRef<str>) -> Value {
        Value::String(s.as_ref().chars().collect())
    }

    /// Create a bytes value
    pub fn from_bytes(b: impl AsRef<[u8]>) -> Value {
        Value::Bytes(b.as_ref().to_vec())
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_display_string())
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        self.equals(other)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Bool(b)
    }
}

impl From<i64> for Value {
    fn from(n: i64) -> Self {
        Value::I64(n)
    }
}

impl From<i32> for Value {
    fn from(n: i32) -> Self {
        Value::I64(n as i64)
    }
}

impl From<f64> for Value {
    fn from(n: f64) -> Self {
        Value::F64(n)
    }
}

impl From<f32> for Value {
    fn from(n: f32) -> Self {
        Value::F64(n as f64)
    }
}

impl From<char> for Value {
    fn from(c: char) -> Self {
        Value::Rune(c)
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::String(s.chars().collect())
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Value::String(s.chars().collect())
    }
}

impl From<Vec<u8>> for Value {
    fn from(b: Vec<u8>) -> Self {
        Value::Bytes(b)
    }
}

impl From<Vec<Value>> for Value {
    fn from(a: Vec<Value>) -> Self {
        Value::Array(a)
    }
}

impl<T: Into<Value>> From<Option<T>> for Value {
    fn from(opt: Option<T>) -> Self {
        match opt {
            Some(v) => v.into(),
            None => Value::None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_truthiness() {
        assert!(!Value::Void.is_truthy());
        assert!(!Value::Bool(false).is_truthy());
        assert!(Value::Bool(true).is_truthy());
        assert!(!Value::I64(0).is_truthy());
        assert!(Value::I64(1).is_truthy());
        assert!(Value::I64(-1).is_truthy());
        assert!(!Value::None.is_truthy());
    }

    #[test]
    fn test_equality() {
        assert!(Value::I64(42).equals(&Value::I64(42)));
        assert!(!Value::I64(42).equals(&Value::I64(43)));
        assert!(Value::Bool(true).equals(&Value::Bool(true)));
        assert!(Value::None.equals(&Value::None));
    }

    #[test]
    fn test_from_impls() {
        let v: Value = 42i64.into();
        assert!(matches!(v, Value::I64(42)));

        let v: Value = true.into();
        assert!(matches!(v, Value::Bool(true)));

        let v: Value = "hello".into();
        assert!(matches!(v, Value::String(_)));
    }

    #[test]
    fn test_display() {
        assert_eq!(Value::I64(42).to_string(), "42");
        assert_eq!(Value::Bool(true).to_string(), "true");
        assert_eq!(Value::None.to_string(), "None");
        assert_eq!(Value::from_string("hello").to_string(), "hello");
    }
}
