//! compat.io module builtins (print, println)
//!
//! These functions are not core builtins - they require importing from compat.io:
//! ```nexus
//! use { println } from compat.io
//! ```

use crate::Value;
use nexus_core::{NexusResult, Span};

pub fn print(args: &[Value], _span: Span) -> NexusResult<Value> {
    for (i, arg) in args.iter().enumerate() {
        if i > 0 {
            print!(" ");
        }
        print!("{}", arg.to_display_string());
    }
    Ok(Value::Void)
}

pub fn println(args: &[Value], _span: Span) -> NexusResult<Value> {
    for (i, arg) in args.iter().enumerate() {
        if i > 0 {
            print!(" ");
        }
        print!("{}", arg.to_display_string());
    }
    println!();
    Ok(Value::Void)
}
