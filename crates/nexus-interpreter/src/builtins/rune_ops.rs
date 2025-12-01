//! Rune comparison operations

use crate::Value;
use nexus_core::{NexusError, NexusResult, Span};

fn extract_rune(val: &Value, span: Span) -> NexusResult<char> {
    match val {
        Value::Rune(c) => Ok(*c),
        _ => Err(NexusError::TypeError {
            message: "Expected rune".to_string(),
            span,
        }),
    }
}

pub fn eqr(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_rune(&args[0], span)?;
    let b = extract_rune(&args[1], span)?;
    Ok(Value::Bool(a == b))
}

pub fn ner(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_rune(&args[0], span)?;
    let b = extract_rune(&args[1], span)?;
    Ok(Value::Bool(a != b))
}

pub fn ltr(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_rune(&args[0], span)?;
    let b = extract_rune(&args[1], span)?;
    Ok(Value::Bool(a < b))
}

pub fn ler(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_rune(&args[0], span)?;
    let b = extract_rune(&args[1], span)?;
    Ok(Value::Bool(a <= b))
}

pub fn gtr(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_rune(&args[0], span)?;
    let b = extract_rune(&args[1], span)?;
    Ok(Value::Bool(a > b))
}

pub fn ger(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_rune(&args[0], span)?;
    let b = extract_rune(&args[1], span)?;
    Ok(Value::Bool(a >= b))
}
