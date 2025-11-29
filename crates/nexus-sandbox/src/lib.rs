//! Sandbox for safe execution of Nexus programs.
//!
//! This crate provides a sandboxed interpreter that restricts
//! what operations can be performed, useful for running untrusted code.

use nexus_core::NexusError;
use nexus_interpreter::{Interpreter, InterpreterConfig, Value};
use nexus_permissions::{CompatPermission, Permission, PermissionSet, PlatPermission};
use std::collections::HashSet;
use std::time::{Duration, Instant};

/// Configuration for the sandbox
#[derive(Debug, Clone)]
pub struct SandboxConfig {
    /// Maximum number of execution steps allowed
    pub max_steps: usize,
    /// Maximum execution time
    pub max_time: Duration,
    /// Maximum memory usage in bytes (approximate)
    pub max_memory: usize,
    /// Maximum recursion depth
    pub max_recursion_depth: usize,
    /// Whether to allow any compat operations
    pub allow_compat: bool,
    /// Whether to allow any plat operations
    pub allow_plat: bool,
    /// Specific permissions to allow
    pub allowed_permissions: HashSet<String>,
    /// Whether to enable bounds checking (always true in sandbox)
    pub bounds_checking: bool,
}

impl Default for SandboxConfig {
    fn default() -> Self {
        Self {
            max_steps: 1_000_000,
            max_time: Duration::from_secs(5),
            max_memory: 64 * 1024 * 1024, // 64 MB
            max_recursion_depth: 100,
            allow_compat: false,
            allow_plat: false,
            allowed_permissions: HashSet::new(),
            bounds_checking: true,
        }
    }
}

impl SandboxConfig {
    /// Create a new sandbox config with default settings
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a strict sandbox that only allows std operations
    pub fn strict() -> Self {
        Self {
            max_steps: 100_000,
            max_time: Duration::from_secs(1),
            max_memory: 16 * 1024 * 1024, // 16 MB
            max_recursion_depth: 50,
            allow_compat: false,
            allow_plat: false,
            allowed_permissions: HashSet::new(),
            bounds_checking: true,
        }
    }

    /// Create a permissive sandbox that allows compat operations
    pub fn permissive() -> Self {
        Self {
            max_steps: 10_000_000,
            max_time: Duration::from_secs(30),
            max_memory: 256 * 1024 * 1024, // 256 MB
            max_recursion_depth: 200,
            allow_compat: true,
            allow_plat: false,
            allowed_permissions: HashSet::new(),
            bounds_checking: true,
        }
    }

    /// Set max steps
    pub fn with_max_steps(mut self, steps: usize) -> Self {
        self.max_steps = steps;
        self
    }

    /// Set max time
    pub fn with_max_time(mut self, time: Duration) -> Self {
        self.max_time = time;
        self
    }

    /// Set max memory
    pub fn with_max_memory(mut self, bytes: usize) -> Self {
        self.max_memory = bytes;
        self
    }

    /// Set max recursion depth
    pub fn with_max_recursion(mut self, depth: usize) -> Self {
        self.max_recursion_depth = depth;
        self
    }

    /// Allow compat operations
    pub fn with_compat(mut self, allow: bool) -> Self {
        self.allow_compat = allow;
        self
    }

    /// Allow plat operations
    pub fn with_plat(mut self, allow: bool) -> Self {
        self.allow_plat = allow;
        self
    }

    /// Add an allowed permission
    pub fn with_permission(mut self, permission: impl Into<String>) -> Self {
        self.allowed_permissions.insert(permission.into());
        self
    }

    /// Build the permission set from this config
    fn build_permission_set(&self) -> PermissionSet {
        let mut perms = PermissionSet::new();

        // Always allow std
        perms.allow(Permission::Std);

        // Add compat if allowed
        if self.allow_compat {
            perms.allow(Permission::Compat(CompatPermission::All));
        }

        // Add plat if allowed
        if self.allow_plat {
            perms.allow(Permission::Plat(PlatPermission::All));
        }

        // Add specific permissions
        for perm_str in &self.allowed_permissions {
            if let Some(perm) = Permission::parse(perm_str) {
                perms.allow(perm);
            }
        }

        perms
    }
}

/// Result of sandbox execution
#[derive(Debug)]
pub struct SandboxResult {
    /// The result value (if successful)
    pub value: Option<Value>,
    /// Any error that occurred
    pub error: Option<NexusError>,
    /// Number of steps executed
    pub steps_executed: usize,
    /// Time taken for execution
    pub time_elapsed: Duration,
    /// Whether execution was terminated early
    pub terminated_early: bool,
    /// Reason for early termination (if applicable)
    pub termination_reason: Option<String>,
}

impl SandboxResult {
    /// Check if execution was successful
    pub fn is_ok(&self) -> bool {
        self.error.is_none() && !self.terminated_early
    }

    /// Check if execution failed
    pub fn is_err(&self) -> bool {
        self.error.is_some() || self.terminated_early
    }

    /// Get the value, panicking if there was an error
    pub fn unwrap(self) -> Value {
        if let Some(err) = self.error {
            panic!("Sandbox execution failed: {}", err);
        }
        if self.terminated_early {
            panic!(
                "Sandbox execution terminated: {}",
                self.termination_reason.unwrap_or_default()
            );
        }
        self.value.unwrap_or(Value::Void)
    }

    /// Get the error, if any
    pub fn err(&self) -> Option<&NexusError> {
        self.error.as_ref()
    }
}

/// A sandboxed execution environment for Nexus programs
pub struct Sandbox {
    config: SandboxConfig,
}

impl Sandbox {
    /// Create a new sandbox with the given configuration
    pub fn new(config: SandboxConfig) -> Self {
        Self { config }
    }

    /// Create a sandbox from an InterpreterConfig
    pub fn with_config(interp_config: nexus_interpreter::InterpreterConfig) -> Self {
        let default = SandboxConfig::default();
        let config = SandboxConfig {
            bounds_checking: interp_config.bounds_checking,
            max_recursion_depth: interp_config.max_recursion_depth,
            max_steps: interp_config.max_steps.unwrap_or(default.max_steps),
            ..default
        };
        Self::new(config)
    }

    /// Create a sandbox with default configuration
    pub fn with_defaults() -> Self {
        Self::new(SandboxConfig::default())
    }

    /// Create a strict sandbox
    pub fn strict() -> Self {
        Self::new(SandboxConfig::strict())
    }

    /// Create a permissive sandbox
    pub fn permissive() -> Self {
        Self::new(SandboxConfig::permissive())
    }

    /// Execute source code in the sandbox
    pub fn execute(&self, source: &str) -> SandboxResult {
        let start_time = Instant::now();

        // Parse the source
        let program = match nexus_parser::parse(source) {
            Ok(prog) => prog,
            Err(err) => {
                return SandboxResult {
                    value: None,
                    error: Some(err),
                    steps_executed: 0,
                    time_elapsed: start_time.elapsed(),
                    terminated_early: false,
                    termination_reason: None,
                };
            }
        };

        // Create interpreter with sandbox config
        let interp_config = InterpreterConfig {
            bounds_checking: self.config.bounds_checking,
            max_recursion_depth: self.config.max_recursion_depth,
            max_steps: Some(self.config.max_steps),
        };

        let mut interpreter = Interpreter::with_config(interp_config);

        // Set up permissions
        interpreter
            .permissions_mut()
            .set_module_permissions("sandbox", self.config.build_permission_set());
        interpreter.set_current_module("sandbox");

        // Load the program
        if let Err(err) = interpreter.load_program(&program) {
            return SandboxResult {
                value: None,
                error: Some(err),
                steps_executed: 0,
                time_elapsed: start_time.elapsed(),
                terminated_early: false,
                termination_reason: None,
            };
        }

        // Run with timeout check
        let result = interpreter.run();
        let elapsed = start_time.elapsed();
        let steps = interpreter.step_count();

        // Check if we exceeded time limit
        if elapsed > self.config.max_time {
            return SandboxResult {
                value: None,
                error: None,
                steps_executed: steps,
                time_elapsed: elapsed,
                terminated_early: true,
                termination_reason: Some(format!(
                    "Execution time exceeded limit of {:?}",
                    self.config.max_time
                )),
            };
        }

        match result {
            Ok(value) => SandboxResult {
                value: Some(value),
                error: None,
                steps_executed: steps,
                time_elapsed: elapsed,
                terminated_early: false,
                termination_reason: None,
            },
            Err(err) => {
                // Check if it was a sandbox violation
                let terminated_early = matches!(err, NexusError::SandboxViolation { .. });
                let termination_reason = if terminated_early {
                    Some(err.to_string())
                } else {
                    None
                };

                SandboxResult {
                    value: None,
                    error: Some(err),
                    steps_executed: steps,
                    time_elapsed: elapsed,
                    terminated_early,
                    termination_reason,
                }
            }
        }
    }

    /// Execute a buffer (same as execute, but clearer intent)
    pub fn execute_buffer(&self, buffer: &str) -> SandboxResult {
        self.execute(buffer)
    }

    /// Run source code (convenience method that returns NexusResult)
    pub fn run_source(&self, source: &str) -> nexus_core::NexusResult<Value> {
        let result = self.execute(source);
        if let Some(err) = result.error {
            return Err(err);
        }
        if result.terminated_early {
            return Err(NexusError::SandboxViolation {
                message: result.termination_reason.unwrap_or_default(),
                span: None,
            });
        }
        Ok(result.value.unwrap_or(Value::Void))
    }

    /// Execute a file
    pub fn execute_file(&self, path: impl AsRef<std::path::Path>) -> SandboxResult {
        let content = match std::fs::read_to_string(path) {
            Ok(c) => c,
            Err(err) => {
                return SandboxResult {
                    value: None,
                    error: Some(NexusError::IoError {
                        message: err.to_string(),
                    }),
                    steps_executed: 0,
                    time_elapsed: Duration::ZERO,
                    terminated_early: false,
                    termination_reason: None,
                };
            }
        };

        self.execute(&content)
    }

    /// Get the configuration
    pub fn config(&self) -> &SandboxConfig {
        &self.config
    }

    /// Update the configuration
    pub fn set_config(&mut self, config: SandboxConfig) {
        self.config = config;
    }
}

impl Default for Sandbox {
    fn default() -> Self {
        Self::with_defaults()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sandbox_creation() {
        let sandbox = Sandbox::with_defaults();
        assert_eq!(sandbox.config().max_steps, 1_000_000);
    }

    #[test]
    fn test_strict_sandbox() {
        let sandbox = Sandbox::strict();
        assert_eq!(sandbox.config().max_steps, 100_000);
        assert!(!sandbox.config().allow_compat);
        assert!(!sandbox.config().allow_plat);
    }

    #[test]
    fn test_permissive_sandbox() {
        let sandbox = Sandbox::permissive();
        assert!(sandbox.config().allow_compat);
        assert!(!sandbox.config().allow_plat);
    }

    #[test]
    fn test_config_builder() {
        let config = SandboxConfig::new()
            .with_max_steps(500)
            .with_max_time(Duration::from_millis(100))
            .with_compat(true)
            .with_permission("compat.io");

        assert_eq!(config.max_steps, 500);
        assert!(config.allow_compat);
        assert!(config.allowed_permissions.contains("compat.io"));
    }

    #[test]
    fn test_permission_set_building() {
        let config = SandboxConfig::new()
            .with_compat(true)
            .with_permission("plat.desktop_x64.clipboard");

        let perms = config.build_permission_set();
        assert!(perms.is_allowed(&Permission::Std));
        assert!(perms.is_allowed(&Permission::Compat(CompatPermission::Io)));
    }
}
