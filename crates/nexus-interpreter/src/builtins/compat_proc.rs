//! compat.proc module builtins (process operations)
//!
//! These functions are not core builtins - they require importing from compat.proc:
//! ```nexus
//! use { getargs } from compat.proc
//! ```
//!
//! Note: getargs() is handled specially in the interpreter since it needs access
//! to interpreter state (the stored program arguments). The function defined here
//! is a placeholder for the builtin registry.

use crate::Value;
use nexus_core::{NexusError, NexusResult, Span};

/// Get program arguments as an array of rune arrays.
/// Note: This is a placeholder - the actual implementation is in the interpreter
/// since it needs access to the stored program arguments.
pub fn getargs(_args: &[Value], span: Span) -> NexusResult<Value> {
    // This should never be called directly - the interpreter handles getargs specially
    Err(NexusError::RuntimeError {
        message: "getargs() should be handled by the interpreter".to_string(),
        span: Some(span),
    })
}

/// Exit the program with a status code
pub fn exit(args: &[Value], span: Span) -> NexusResult<Value> {
    let code = match &args[0] {
        Value::I64(n) => *n as i32,
        _ => {
            return Err(NexusError::TypeError {
                message: "exit expects an i64 argument".to_string(),
                span,
            });
        }
    };

    std::process::exit(code);
}

/// Get an environment variable value
pub fn getenv(args: &[Value], span: Span) -> NexusResult<Value> {
    let name = match &args[0] {
        Value::String(chars) => chars.iter().collect::<String>(),
        _ => {
            return Err(NexusError::TypeError {
                message: "getenv expects a rune array (name) argument".to_string(),
                span,
            });
        }
    };

    match std::env::var(&name) {
        Ok(value) => Ok(Value::String(value.chars().collect())),
        Err(_) => Ok(Value::String(Vec::new())), // Return empty string if not found
    }
}
