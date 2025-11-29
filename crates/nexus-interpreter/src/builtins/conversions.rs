//! Type conversion builtins (i64, i32, f64, f32, bool, rune)

use crate::Value;
use nexus_core::{NexusError, NexusResult, Span};

pub fn to_i64(args: &[Value], span: Span) -> NexusResult<Value> {
    let val = match &args[0] {
        Value::I64(n) => *n,
        Value::I32(n) => *n as i64,
        Value::F64(n) => *n as i64,
        Value::F32(n) => *n as i64,
        Value::Bool(b) => {
            if *b {
                1
            } else {
                0
            }
        }
        Value::Rune(c) => *c as i64,
        _ => {
            return Err(NexusError::TypeError {
                message: "Cannot convert to i64".to_string(),
                span,
            });
        }
    };
    Ok(Value::I64(val))
}

pub fn to_i32(args: &[Value], span: Span) -> NexusResult<Value> {
    let val = match &args[0] {
        Value::I64(n) => *n as i32,
        Value::I32(n) => *n,
        Value::F64(n) => *n as i32,
        Value::F32(n) => *n as i32,
        Value::Bool(b) => {
            if *b {
                1
            } else {
                0
            }
        }
        Value::Rune(c) => *c as i32,
        _ => {
            return Err(NexusError::TypeError {
                message: "Cannot convert to i32".to_string(),
                span,
            });
        }
    };
    Ok(Value::I32(val))
}

pub fn to_f64(args: &[Value], span: Span) -> NexusResult<Value> {
    let val = match &args[0] {
        Value::I64(n) => *n as f64,
        Value::I32(n) => *n as f64,
        Value::F64(n) => *n,
        Value::F32(n) => *n as f64,
        _ => {
            return Err(NexusError::TypeError {
                message: "Cannot convert to f64".to_string(),
                span,
            });
        }
    };
    Ok(Value::F64(val))
}

pub fn to_f32(args: &[Value], span: Span) -> NexusResult<Value> {
    let val = match &args[0] {
        Value::I64(n) => *n as f32,
        Value::I32(n) => *n as f32,
        Value::F64(n) => *n as f32,
        Value::F32(n) => *n,
        _ => {
            return Err(NexusError::TypeError {
                message: "Cannot convert to f32".to_string(),
                span,
            });
        }
    };
    Ok(Value::F32(val))
}

pub fn to_bool(args: &[Value], _span: Span) -> NexusResult<Value> {
    Ok(Value::Bool(args[0].is_truthy()))
}

pub fn to_rune(args: &[Value], span: Span) -> NexusResult<Value> {
    let val = match &args[0] {
        Value::I64(n) => char::from_u32(*n as u32).unwrap_or('\0'),
        Value::I32(n) => char::from_u32(*n as u32).unwrap_or('\0'),
        Value::Rune(c) => *c,
        _ => {
            return Err(NexusError::TypeError {
                message: "Cannot convert to rune".to_string(),
                span,
            });
        }
    };
    Ok(Value::Rune(val))
}
