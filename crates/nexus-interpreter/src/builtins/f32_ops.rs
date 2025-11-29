//! f32 arithmetic operations

use crate::Value;
use nexus_core::{NexusError, NexusResult, Span};

use super::macros::extract_f32;

pub fn addf32(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_f32!(&args[0], span);
    let b = extract_f32!(&args[1], span);
    Ok(Value::F32(a + b))
}

pub fn subf32(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_f32!(&args[0], span);
    let b = extract_f32!(&args[1], span);
    Ok(Value::F32(a - b))
}

pub fn mulf32(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_f32!(&args[0], span);
    let b = extract_f32!(&args[1], span);
    Ok(Value::F32(a * b))
}

pub fn divf32(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_f32!(&args[0], span);
    let b = extract_f32!(&args[1], span);
    Ok(Value::F32(a / b))
}

pub fn negf32(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_f32!(&args[0], span);
    Ok(Value::F32(-a))
}
