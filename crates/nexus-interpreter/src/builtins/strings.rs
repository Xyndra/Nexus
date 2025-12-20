//! Rune array manipulation builtins (parse_i64, split, trim, etc.)
//!
//! These are standard library functions that operate on rune arrays (`[rune]`).
//! In Nexus, there is no "string" type - text is represented as rune arrays.
//! These functions don't require special permissions.

use crate::Value;
use nexus_core::{NexusError, NexusResult, Span};

/// Parse a rune array to i64
pub fn parse_i64(args: &[Value], span: Span) -> NexusResult<Value> {
    let s = match &args[0] {
        Value::String(chars) => chars.iter().collect::<String>(),
        _ => {
            return Err(NexusError::TypeError {
                message: "parse_i64 expects a rune array argument".to_string(),
                span,
            });
        }
    };

    match s.trim().parse::<i64>() {
        Ok(n) => Ok(Value::I64(n)),
        Err(_) => Err(NexusError::RuntimeError {
            message: format!("Failed to parse '{}' as i64", s),
            span: Some(span),
        }),
    }
}

/// Split a rune array by a delimiter
pub fn split(args: &[Value], span: Span) -> NexusResult<Value> {
    let s = match &args[0] {
        Value::String(chars) => chars.iter().collect::<String>(),
        _ => {
            return Err(NexusError::TypeError {
                message: "split expects a rune array as first argument".to_string(),
                span,
            });
        }
    };

    let delimiter = match &args[1] {
        Value::String(chars) => chars.iter().collect::<String>(),
        Value::Rune(c) => c.to_string(),
        _ => {
            return Err(NexusError::TypeError {
                message: "split expects a rune array or rune as delimiter".to_string(),
                span,
            });
        }
    };

    let parts: Vec<Value> = s
        .split(&delimiter)
        .map(|part| Value::String(part.chars().collect()))
        .collect();

    Ok(Value::Array(parts))
}

/// Trim whitespace from both ends of a rune array
pub fn trim(args: &[Value], span: Span) -> NexusResult<Value> {
    let s = match &args[0] {
        Value::String(chars) => chars.iter().collect::<String>(),
        _ => {
            return Err(NexusError::TypeError {
                message: "trim expects a rune array argument".to_string(),
                span,
            });
        }
    };

    Ok(Value::String(s.trim().chars().collect()))
}

/// Check if a rune array starts with a prefix
pub fn starts_with(args: &[Value], span: Span) -> NexusResult<Value> {
    let s = match &args[0] {
        Value::String(chars) => chars.iter().collect::<String>(),
        _ => {
            return Err(NexusError::TypeError {
                message: "starts_with expects a rune array as first argument".to_string(),
                span,
            });
        }
    };

    let prefix = match &args[1] {
        Value::String(chars) => chars.iter().collect::<String>(),
        _ => {
            return Err(NexusError::TypeError {
                message: "starts_with expects a rune array as prefix".to_string(),
                span,
            });
        }
    };

    Ok(Value::Bool(s.starts_with(&prefix)))
}

/// Check if a rune array ends with a suffix
pub fn ends_with(args: &[Value], span: Span) -> NexusResult<Value> {
    let s = match &args[0] {
        Value::String(chars) => chars.iter().collect::<String>(),
        _ => {
            return Err(NexusError::TypeError {
                message: "ends_with expects a rune array as first argument".to_string(),
                span,
            });
        }
    };

    let suffix = match &args[1] {
        Value::String(chars) => chars.iter().collect::<String>(),
        _ => {
            return Err(NexusError::TypeError {
                message: "ends_with expects a rune array as suffix".to_string(),
                span,
            });
        }
    };

    Ok(Value::Bool(s.ends_with(&suffix)))
}

/// Check if a rune array or array is empty
pub fn is_empty(args: &[Value], span: Span) -> NexusResult<Value> {
    match &args[0] {
        Value::String(chars) => Ok(Value::Bool(chars.is_empty())),
        Value::Array(arr) => Ok(Value::Bool(arr.is_empty())),
        _ => Err(NexusError::TypeError {
            message: "is_empty expects a rune array or array argument".to_string(),
            span,
        }),
    }
}

/// Join an array of rune arrays with a delimiter
pub fn join(args: &[Value], span: Span) -> NexusResult<Value> {
    let arr = match &args[0] {
        Value::Array(arr) => arr,
        _ => {
            return Err(NexusError::TypeError {
                message: "join expects an array as first argument".to_string(),
                span,
            });
        }
    };

    let delimiter = match &args[1] {
        Value::String(chars) => chars.iter().collect::<String>(),
        Value::Rune(c) => c.to_string(),
        _ => {
            return Err(NexusError::TypeError {
                message: "join expects a rune array or rune as delimiter".to_string(),
                span,
            });
        }
    };

    let strings: Result<Vec<String>, _> = arr
        .iter()
        .map(|v| match v {
            Value::String(chars) => Ok(chars.iter().collect::<String>()),
            _ => Err(NexusError::TypeError {
                message: "join expects an array of rune arrays".to_string(),
                span,
            }),
        })
        .collect();

    Ok(Value::String(strings?.join(&delimiter).chars().collect()))
}

/// Compare two rune arrays for equality
pub fn eqs(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = match &args[0] {
        Value::String(chars) => chars.iter().collect::<String>(),
        _ => {
            return Err(NexusError::TypeError {
                message: "eqs expects a rune array as first argument".to_string(),
                span,
            });
        }
    };

    let b = match &args[1] {
        Value::String(chars) => chars.iter().collect::<String>(),
        _ => {
            return Err(NexusError::TypeError {
                message: "eqs expects a rune array as second argument".to_string(),
                span,
            });
        }
    };

    Ok(Value::Bool(a == b))
}
