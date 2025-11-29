//! f64 arithmetic and comparison operations

use crate::Value;
use nexus_core::{NexusError, NexusResult, Span};

use super::macros::extract_f64;

pub fn addf64(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_f64!(&args[0], span);
    let b = extract_f64!(&args[1], span);
    Ok(Value::F64(a + b))
}

pub fn subf64(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_f64!(&args[0], span);
    let b = extract_f64!(&args[1], span);
    Ok(Value::F64(a - b))
}

pub fn mulf64(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_f64!(&args[0], span);
    let b = extract_f64!(&args[1], span);
    Ok(Value::F64(a * b))
}

pub fn divf64(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_f64!(&args[0], span);
    let b = extract_f64!(&args[1], span);
    Ok(Value::F64(a / b)) // f64 division by zero returns infinity
}

pub fn negf64(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_f64!(&args[0], span);
    Ok(Value::F64(-a))
}

pub fn eqf64(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_f64!(&args[0], span);
    let b = extract_f64!(&args[1], span);
    Ok(Value::Bool(a == b))
}

pub fn nef64(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_f64!(&args[0], span);
    let b = extract_f64!(&args[1], span);
    Ok(Value::Bool(a != b))
}

pub fn ltf64(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_f64!(&args[0], span);
    let b = extract_f64!(&args[1], span);
    Ok(Value::Bool(a < b))
}

pub fn lef64(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_f64!(&args[0], span);
    let b = extract_f64!(&args[1], span);
    Ok(Value::Bool(a <= b))
}

pub fn gtf64(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_f64!(&args[0], span);
    let b = extract_f64!(&args[1], span);
    Ok(Value::Bool(a > b))
}

pub fn gef64(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_f64!(&args[0], span);
    let b = extract_f64!(&args[1], span);
    Ok(Value::Bool(a >= b))
}
