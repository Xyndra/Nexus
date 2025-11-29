//! Logical operation builtins (not, and, or)

use crate::Value;
use nexus_core::{NexusError, NexusResult, Span};

macro_rules! extract_bool {
    ($val:expr, $span:expr) => {
        match $val {
            Value::Bool(b) => *b,
            _ => {
                return Err(NexusError::TypeError {
                    message: "Expected bool".to_string(),
                    span: $span,
                })
            }
        }
    };
}

pub fn builtin_not(args: &[Value], span: Span) -> NexusResult<Value> {
    let val = extract_bool!(&args[0], span);
    Ok(Value::Bool(!val))
}

pub fn builtin_and(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_bool!(&args[0], span);
    let b = extract_bool!(&args[1], span);
    Ok(Value::Bool(a && b))
}

pub fn builtin_or(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_bool!(&args[0], span);
    let b = extract_bool!(&args[1], span);
    Ok(Value::Bool(a || b))
}
