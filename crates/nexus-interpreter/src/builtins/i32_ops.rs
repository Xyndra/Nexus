//! i32 arithmetic and comparison operations

use crate::Value;
use nexus_core::{NexusError, NexusResult, Span};

use super::macros::extract_i32;

pub fn addi32(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i32!(&args[0], span);
    let b = extract_i32!(&args[1], span);
    Ok(Value::I32(a.wrapping_add(b)))
}

pub fn subi32(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i32!(&args[0], span);
    let b = extract_i32!(&args[1], span);
    Ok(Value::I32(a.wrapping_sub(b)))
}

pub fn muli32(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i32!(&args[0], span);
    let b = extract_i32!(&args[1], span);
    Ok(Value::I32(a.wrapping_mul(b)))
}

pub fn divi32(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i32!(&args[0], span);
    let b = extract_i32!(&args[1], span);
    if b == 0 {
        return Err(NexusError::DivisionByZero { span });
    }
    Ok(Value::I32(a / b))
}

pub fn modi32(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i32!(&args[0], span);
    let b = extract_i32!(&args[1], span);
    if b == 0 {
        return Err(NexusError::DivisionByZero { span });
    }
    Ok(Value::I32(a % b))
}

pub fn negi32(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i32!(&args[0], span);
    Ok(Value::I32(-a))
}

pub fn eqi32(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i32!(&args[0], span);
    let b = extract_i32!(&args[1], span);
    Ok(Value::Bool(a == b))
}

pub fn nei32(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i32!(&args[0], span);
    let b = extract_i32!(&args[1], span);
    Ok(Value::Bool(a != b))
}

pub fn lti32(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i32!(&args[0], span);
    let b = extract_i32!(&args[1], span);
    Ok(Value::Bool(a < b))
}

pub fn lei32(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i32!(&args[0], span);
    let b = extract_i32!(&args[1], span);
    Ok(Value::Bool(a <= b))
}

pub fn gti32(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i32!(&args[0], span);
    let b = extract_i32!(&args[1], span);
    Ok(Value::Bool(a > b))
}

pub fn gei32(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i32!(&args[0], span);
    let b = extract_i32!(&args[1], span);
    Ok(Value::Bool(a >= b))
}
