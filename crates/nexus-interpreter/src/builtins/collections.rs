//! Collection operation builtins (arrays, strings, bytes)

use crate::Value;
use nexus_core::{NexusError, NexusResult, Span};

use super::macros::extract_i64;

pub fn len(args: &[Value], span: Span) -> NexusResult<Value> {
    let len = match &args[0] {
        Value::String(s) => s.len(),
        Value::Array(a) => a.len(),
        Value::Bytes(b) => b.len(),
        _ => {
            return Err(NexusError::TypeError {
                message: "len() requires a string, array, or bytes".to_string(),
                span,
            });
        }
    };
    Ok(Value::I64(len as i64))
}

pub fn push(args: &[Value], span: Span) -> NexusResult<Value> {
    match (&args[0], &args[1]) {
        (Value::Array(arr), val) => {
            let mut new_arr = arr.clone();
            new_arr.push(val.clone());
            Ok(Value::Array(new_arr))
        }
        (Value::String(s), Value::Rune(c)) => {
            let mut new_s = s.clone();
            new_s.push(*c);
            Ok(Value::String(new_s))
        }
        (Value::Bytes(b), Value::I64(byte)) => {
            let mut new_b = b.clone();
            new_b.push(*byte as u8);
            Ok(Value::Bytes(new_b))
        }
        _ => Err(NexusError::TypeError {
            message: "push() requires an array/string and a value".to_string(),
            span,
        }),
    }
}

pub fn pop(args: &[Value], span: Span) -> NexusResult<Value> {
    match &args[0] {
        Value::Array(arr) => {
            if arr.is_empty() {
                Ok(Value::None)
            } else {
                let mut new_arr = arr.clone();
                let val = new_arr.pop().unwrap();
                // Return tuple of (new_array, popped_value)
                Ok(Value::Array(vec![Value::Array(new_arr), val]))
            }
        }
        Value::String(s) => {
            if s.is_empty() {
                Ok(Value::None)
            } else {
                let mut new_s = s.clone();
                let c = new_s.pop().unwrap();
                Ok(Value::Array(vec![Value::String(new_s), Value::Rune(c)]))
            }
        }
        _ => Err(NexusError::TypeError {
            message: "pop() requires an array or string".to_string(),
            span,
        }),
    }
}

pub fn concat(args: &[Value], span: Span) -> NexusResult<Value> {
    match (&args[0], &args[1]) {
        (Value::Array(a), Value::Array(b)) => {
            let mut result = a.clone();
            result.extend(b.iter().cloned());
            Ok(Value::Array(result))
        }
        (Value::String(a), Value::String(b)) => {
            let mut result = a.clone();
            result.extend(b.iter().cloned());
            Ok(Value::String(result))
        }
        (Value::Bytes(a), Value::Bytes(b)) => {
            let mut result = a.clone();
            result.extend(b.iter().cloned());
            Ok(Value::Bytes(result))
        }
        _ => Err(NexusError::TypeError {
            message: "concat() requires two arrays, strings, or bytes".to_string(),
            span,
        }),
    }
}

pub fn slice(args: &[Value], span: Span) -> NexusResult<Value> {
    let start = extract_i64!(&args[1], span) as usize;
    let end = extract_i64!(&args[2], span) as usize;

    match &args[0] {
        Value::Array(arr) => {
            if start > arr.len() || end > arr.len() || start > end {
                return Err(NexusError::IndexOutOfBounds {
                    index: end as i64,
                    length: arr.len(),
                    span,
                });
            }
            Ok(Value::Array(arr[start..end].to_vec()))
        }
        Value::String(s) => {
            if start > s.len() || end > s.len() || start > end {
                return Err(NexusError::IndexOutOfBounds {
                    index: end as i64,
                    length: s.len(),
                    span,
                });
            }
            Ok(Value::String(s[start..end].to_vec()))
        }
        Value::Bytes(b) => {
            if start > b.len() || end > b.len() || start > end {
                return Err(NexusError::IndexOutOfBounds {
                    index: end as i64,
                    length: b.len(),
                    span,
                });
            }
            Ok(Value::Bytes(b[start..end].to_vec()))
        }
        _ => Err(NexusError::TypeError {
            message: "slice() requires an array, string, or bytes".to_string(),
            span,
        }),
    }
}

pub fn contains(args: &[Value], span: Span) -> NexusResult<Value> {
    match (&args[0], &args[1]) {
        (Value::Array(arr), val) => Ok(Value::Bool(arr.iter().any(|v| v.equals(val)))),
        (Value::String(s), Value::Rune(c)) => Ok(Value::Bool(s.contains(c))),
        (Value::String(s), Value::String(sub)) => {
            let s_str: String = s.iter().collect();
            let sub_str: String = sub.iter().collect();
            Ok(Value::Bool(s_str.contains(&sub_str)))
        }
        _ => Err(NexusError::TypeError {
            message: "contains() requires a collection and a value".to_string(),
            span,
        }),
    }
}
