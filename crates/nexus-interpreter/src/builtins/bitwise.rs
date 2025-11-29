//! Bitwise operation builtins (band, bor, bxor, bnot, shl, shr)

use crate::Value;
use nexus_core::{NexusError, NexusResult, Span};

use super::macros::extract_i64;

pub fn band(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i64!(&args[0], span);
    let b = extract_i64!(&args[1], span);
    Ok(Value::I64(a & b))
}

pub fn bor(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i64!(&args[0], span);
    let b = extract_i64!(&args[1], span);
    Ok(Value::I64(a | b))
}

pub fn bxor(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i64!(&args[0], span);
    let b = extract_i64!(&args[1], span);
    Ok(Value::I64(a ^ b))
}

pub fn bnot(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i64!(&args[0], span);
    Ok(Value::I64(!a))
}

pub fn shl(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i64!(&args[0], span);
    let b = extract_i64!(&args[1], span);
    Ok(Value::I64(a << b))
}

pub fn shr(args: &[Value], span: Span) -> NexusResult<Value> {
    let a = extract_i64!(&args[0], span);
    let b = extract_i64!(&args[1], span);
    Ok(Value::I64(a >> b))
}
