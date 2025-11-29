//! i64 arithmetic and comparison operations

use crate::Value;
use nexus_core::{NexusError, NexusResult, Span};

use super::macros::extract_i64;

pub fn addi64(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i64!(&args[0], span);
    let b = extract_i64!(&args[1], span);
    Ok(Value::I64(a.wrapping_add(b)))
}

pub fn subi64(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i64!(&args[0], span);
    let b = extract_i64!(&args[1], span);
    Ok(Value::I64(a.wrapping_sub(b)))
}

pub fn muli64(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i64!(&args[0], span);
    let b = extract_i64!(&args[1], span);
    Ok(Value::I64(a.wrapping_mul(b)))
}

pub fn divi64(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i64!(&args[0], span);
    let b = extract_i64!(&args[1], span);
    if b == 0 {
        return Err(NexusError::DivisionByZero { span });
    }
    Ok(Value::I64(a / b))
}

pub fn modi64(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i64!(&args[0], span);
    let b = extract_i64!(&args[1], span);
    if b == 0 {
        return Err(NexusError::DivisionByZero { span });
    }
    Ok(Value::I64(a % b))
}

pub fn negi64(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i64!(&args[0], span);
    Ok(Value::I64(-a))
}

pub fn eqi64(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i64!(&args[0], span);
    let b = extract_i64!(&args[1], span);
    Ok(Value::Bool(a == b))
}

pub fn nei64(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i64!(&args[0], span);
    let b = extract_i64!(&args[1], span);
    Ok(Value::Bool(a != b))
}

pub fn lti64(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i64!(&args[0], span);
    let b = extract_i64!(&args[1], span);
    Ok(Value::Bool(a < b))
}

pub fn lei64(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i64!(&args[0], span);
    let b = extract_i64!(&args[1], span);
    Ok(Value::Bool(a <= b))
}

pub fn gti64(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i64!(&args[0], span);
    let b = extract_i64!(&args[1], span);
    Ok(Value::Bool(a > b))
}

pub fn gei64(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i64!(&args[0], span);
    let b = extract_i64!(&args[1], span);
    Ok(Value::Bool(a >= b))
}
