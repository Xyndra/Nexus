//! Builtin function definitions for the Nexus language LSP.
//!
//! This module provides access to builtin function metadata by wrapping
//! the interpreter's BuiltinRegistry. Documentation is loaded from the
//! interpreter when the `builtin-docs` feature is enabled.

use nexus_core::FunctionColor;
use nexus_interpreter::builtins::{Builtin, BuiltinRegistry};
use std::sync::LazyLock;

/// Static builtin registry instance
pub static REGISTRY: LazyLock<BuiltinRegistry> = LazyLock::new(BuiltinRegistry::new);

/// Information about a builtin function for LSP features
pub struct BuiltinInfo<'a> {
    pub builtin: &'a Builtin,
    pub description: &'a str,
    pub documentation: &'a str,
}

impl<'a> BuiltinInfo<'a> {
    /// Format the function signature for display
    pub fn signature(&self) -> String {
        let params: Vec<String> = self
            .builtin
            .params
            .iter()
            .map(|p| format!("{}: {}", p.name, p.ty))
            .collect();
        format!(
            "{}({}): {}",
            self.builtin.name,
            params.join(", "),
            self.builtin.return_type
        )
    }

    /// Format full hover documentation
    pub fn hover_content(&self) -> String {
        let import_note = if let Some(module) = self.builtin.required_import {
            format!(
                "\n\n**Requires:** `use {{ {} }} from {}`",
                self.builtin.name, module
            )
        } else {
            String::new()
        };

        format!(
            "```nexus\n{}\n```\n\n{}{}",
            self.signature(),
            self.documentation,
            import_note
        )
    }
}

/// Get a builtin by name (core builtins only)
pub fn get_builtin(name: &str) -> Option<BuiltinInfo<'static>> {
    REGISTRY.get(name).map(|builtin| {
        #[cfg(feature = "builtin-docs")]
        let (description, documentation) = {
            nexus_interpreter::builtins::get_builtin_docs(name)
                .map(|docs| (docs.description.as_str(), docs.documentation.as_str()))
                .unwrap_or(("", ""))
        };
        #[cfg(not(feature = "builtin-docs"))]
        let (description, documentation) = ("", "");

        BuiltinInfo {
            builtin,
            description,
            documentation,
        }
    })
}

/// Get a compat.io builtin by name
pub fn get_compat_builtin(name: &str) -> Option<BuiltinInfo<'static>> {
    REGISTRY.get_compat_io(name).map(|builtin| {
        #[cfg(feature = "builtin-docs")]
        let (description, documentation) = {
            nexus_interpreter::builtins::get_builtin_docs(name)
                .map(|docs| (docs.description.as_str(), docs.documentation.as_str()))
                .unwrap_or(("", ""))
        };
        #[cfg(not(feature = "builtin-docs"))]
        let (description, documentation) = ("", "");

        BuiltinInfo {
            builtin,
            description,
            documentation,
        }
    })
}

/// Get any builtin by name (checks both core and compat)
pub fn get_any_builtin(name: &str) -> Option<BuiltinInfo<'static>> {
    get_builtin(name).or_else(|| get_compat_builtin(name))
}

/// Check if a name is a core builtin (no import required)
#[allow(dead_code)]
pub fn is_core_builtin(name: &str) -> bool {
    REGISTRY.get(name).is_some()
}

/// Check if a name is a compat.io builtin (requires import)
#[allow(dead_code)]
pub fn is_compat_builtin(name: &str) -> bool {
    REGISTRY.is_compat_io(name)
}

/// Check if a name is any builtin
#[allow(dead_code)]
pub fn is_builtin(name: &str) -> bool {
    is_core_builtin(name) || is_compat_builtin(name)
}

/// Get the required import module for a builtin, if any
pub fn get_required_import(name: &str) -> Option<&'static str> {
    get_any_builtin(name).and_then(|info| info.builtin.required_import)
}

/// Get the function color for a builtin
pub fn get_builtin_color(name: &str) -> Option<FunctionColor> {
    get_any_builtin(name).map(|info| info.builtin.color)
}

/// Iterate over all core builtin names
pub fn core_builtin_names() -> impl Iterator<Item = &'static str> {
    REGISTRY.builtin_names()
}

/// Iterate over all compat.io builtin names
pub fn compat_builtin_names() -> impl Iterator<Item = &'static str> {
    REGISTRY.compat_io_names()
}

/// Iterate over all builtin names (core + compat)
pub fn all_builtin_names() -> impl Iterator<Item = &'static str> {
    core_builtin_names().chain(compat_builtin_names())
}
