//! compat.fs module builtins (file operations)
//!
//! These functions are not core builtins - they require importing from compat.fs:
//! ```nexus
//! use { read_file } from compat.fs
//! ```

use crate::Value;
use nexus_core::{NexusError, NexusResult, Span};
use std::fs;

/// Read a file and return its contents as a rune array
pub fn read_file(args: &[Value], span: Span) -> NexusResult<Value> {
    let path = match &args[0] {
        Value::String(chars) => chars.iter().collect::<String>(),
        _ => {
            return Err(NexusError::TypeError {
                message: "read_file expects a rune array (path) argument".to_string(),
                span,
            });
        }
    };

    match fs::read_to_string(&path) {
        Ok(content) => Ok(Value::String(content.chars().collect())),
        Err(e) => Err(NexusError::RuntimeError {
            message: format!("Failed to read file '{}': {}", path, e),
            span: Some(span),
        }),
    }
}

/// Write content to a file
pub fn write_file(args: &[Value], span: Span) -> NexusResult<Value> {
    let path = match &args[0] {
        Value::String(chars) => chars.iter().collect::<String>(),
        _ => {
            return Err(NexusError::TypeError {
                message: "write_file expects a rune array (path) as first argument".to_string(),
                span,
            });
        }
    };

    let content = match &args[1] {
        Value::String(chars) => chars.iter().collect::<String>(),
        _ => {
            return Err(NexusError::TypeError {
                message: "write_file expects a rune array (content) as second argument".to_string(),
                span,
            });
        }
    };

    match fs::write(&path, content) {
        Ok(()) => Ok(Value::Bool(true)),
        Err(e) => Err(NexusError::RuntimeError {
            message: format!("Failed to write file '{}': {}", path, e),
            span: Some(span),
        }),
    }
}

/// Check if a file exists
pub fn file_exists(args: &[Value], span: Span) -> NexusResult<Value> {
    let path = match &args[0] {
        Value::String(chars) => chars.iter().collect::<String>(),
        _ => {
            return Err(NexusError::TypeError {
                message: "file_exists expects a rune array (path) argument".to_string(),
                span,
            });
        }
    };

    Ok(Value::Bool(std::path::Path::new(&path).exists()))
}
