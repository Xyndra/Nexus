//! Helper macros for builtin function implementations.

/// Extract an i64 value from a Value, with automatic i32 conversion
macro_rules! extract_i64 {
    ($val:expr, $span:expr) => {
        match $val {
            Value::I64(n) => *n,
            Value::I32(n) => *n as i64,
            _ => {
                return Err(NexusError::TypeError {
                    message: "Expected i64".to_string(),
                    span: $span,
                })
            }
        }
    };
}

/// Extract an i32 value from a Value, with automatic i64 conversion
macro_rules! extract_i32 {
    ($val:expr, $span:expr) => {
        match $val {
            Value::I32(n) => *n,
            Value::I64(n) => *n as i32,
            _ => {
                return Err(NexusError::TypeError {
                    message: "Expected i32".to_string(),
                    span: $span,
                })
            }
        }
    };
}

/// Extract an f64 value from a Value, with automatic numeric conversion
macro_rules! extract_f64 {
    ($val:expr, $span:expr) => {
        match $val {
            Value::F64(n) => *n,
            Value::F32(n) => *n as f64,
            Value::I64(n) => *n as f64,
            Value::I32(n) => *n as f64,
            _ => {
                return Err(NexusError::TypeError {
                    message: "Expected f64".to_string(),
                    span: $span,
                })
            }
        }
    };
}

/// Extract an f32 value from a Value, with automatic numeric conversion
macro_rules! extract_f32 {
    ($val:expr, $span:expr) => {
        match $val {
            Value::F32(n) => *n,
            Value::F64(n) => *n as f32,
            Value::I64(n) => *n as f32,
            Value::I32(n) => *n as f32,
            _ => {
                return Err(NexusError::TypeError {
                    message: "Expected f32".to_string(),
                    span: $span,
                })
            }
        }
    };
}

pub(crate) use extract_f32;
pub(crate) use extract_f64;
pub(crate) use extract_i32;
pub(crate) use extract_i64;
