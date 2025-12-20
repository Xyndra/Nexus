//! plat.console module builtins (readln)
//!
//! These functions are platform-specific console operations that require importing from plat.console:
//! ```nexus
//! use { readln } from plat.console
//! ```
//!
//! Note: stdin reading is not available on all platforms (e.g., not on web/WASM),
//! hence this is in plat.console rather than compat.io.

use crate::Value;
use nexus_core::{NexusError, NexusResult, Span};
use std::io::{self, BufRead};

/// Read a line from stdin
pub fn readln(_args: &[Value], span: Span) -> NexusResult<Value> {
    let stdin = io::stdin();
    let mut line = String::new();
    match stdin.lock().read_line(&mut line) {
        Ok(_) => {
            // Remove trailing newline
            if line.ends_with('\n') {
                line.pop();
                if line.ends_with('\r') {
                    line.pop();
                }
            }
            Ok(Value::String(line.chars().collect()))
        }
        Err(e) => Err(NexusError::RuntimeError {
            message: format!("Failed to read line: {}", e),
            span: Some(span),
        }),
    }
}
