//! Utility builtins (typeof, is_none, unwrap, panic, assert, str)

use crate::Value;
use nexus_core::{NexusError, NexusResult, Span};

pub fn type_of(args: &[Value], _span: Span) -> NexusResult<Value> {
    Ok(Value::String(args[0].type_name().chars().collect()))
}

/// Convert any value to its string representation
pub fn str(args: &[Value], _span: Span) -> NexusResult<Value> {
    Ok(Value::String(args[0].to_display_string().chars().collect()))
}

pub fn is_none(args: &[Value], _span: Span) -> NexusResult<Value> {
    Ok(Value::Bool(matches!(args[0], Value::None)))
}

pub fn unwrap(args: &[Value], span: Span) -> NexusResult<Value> {
    match &args[0] {
        Value::None => Err(NexusError::NullPointer { span }),
        val => Ok(val.clone()),
    }
}

pub fn panic(args: &[Value], span: Span) -> NexusResult<Value> {
    let msg = args[0].to_display_string();
    Err(NexusError::RuntimeError {
        message: format!("panic: {}", msg),
        span: Some(span),
    })
}

pub fn assert(args: &[Value], span: Span) -> NexusResult<Value> {
    if !args[0].is_truthy() {
        return Err(NexusError::RuntimeError {
            message: "Assertion failed".to_string(),
            span: Some(span),
        });
    }
    Ok(Value::Void)
}

pub fn assert_eq(args: &[Value], span: Span) -> NexusResult<Value> {
    if !args[0].equals(&args[1]) {
        return Err(NexusError::RuntimeError {
            message: format!(
                "Assertion failed: {} != {}",
                args[0].to_display_string(),
                args[1].to_display_string()
            ),
            span: Some(span),
        });
    }
    Ok(Value::Void)
}
