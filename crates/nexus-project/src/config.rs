//! Project configuration parsing from `nexus.json5` files.

use crate::dependency::Dependency;
use nexus_core::{NexusError, NexusResult};
use serde::Deserialize;
use std::collections::HashMap;
use std::fs;
use std::path::Path;

/// Known permission keys that are valid in nexus.json5
const KNOWN_PERMISSIONS: &[&str] = &[
    "compat.io",
    "compat.fs",
    "compat.net",
    "compat.proc",
    "plat.console",
    "plat.desktop",
];

/// Raw configuration as parsed from JSON5
#[derive(Debug, Deserialize)]
struct RawProjectConfig {
    /// Module name
    #[serde(rename = "mod")]
    module: String,

    /// Permissions configuration (HashMap to catch unknown keys)
    #[serde(default)]
    permissions: HashMap<String, Vec<String>>,

    /// Dependencies list
    #[serde(default)]
    deps: Vec<String>,
}

/// Parsed and validated project configuration
#[derive(Debug, Clone)]
pub struct ProjectConfig {
    /// Module name for this project
    pub module_name: String,

    /// Permissions granted to modules
    pub permissions: ProjectPermissions,

    /// Parsed dependencies
    pub dependencies: Vec<Dependency>,
}

/// Permissions configuration for the project
#[derive(Debug, Clone, Default)]
pub struct ProjectPermissions {
    /// Modules allowed to use compat.io
    pub compat_io: Vec<String>,

    /// Modules allowed to use compat.fs
    pub compat_fs: Vec<String>,

    /// Modules allowed to use compat.net
    pub compat_net: Vec<String>,

    /// Modules allowed to use compat.proc
    pub compat_proc: Vec<String>,

    /// Modules allowed to use plat.console
    pub plat_console: Vec<String>,

    /// Modules allowed to use plat.desktop functions
    pub plat_desktop: Vec<String>,
}

impl ProjectConfig {
    /// Parse a project configuration from a file path
    pub fn from_file(path: &Path) -> NexusResult<Self> {
        let content = fs::read_to_string(path).map_err(|e| NexusError::IoError {
            message: format!("Failed to read config file '{}': {}", path.display(), e),
        })?;

        Self::parse_str(&content)
    }

    /// Parse a project configuration from a string
    pub fn parse_str(content: &str) -> NexusResult<Self> {
        let raw: RawProjectConfig = json5::from_str(content).map_err(|e| NexusError::IoError {
            message: format!("Failed to parse nexus.json5: {}", e),
        })?;

        // Check for unknown permission keys
        for key in raw.permissions.keys() {
            if !KNOWN_PERMISSIONS.contains(&key.as_str()) {
                return Err(NexusError::IoError {
                    message: format!(
                        "Unknown permission '{}' in nexus.json5. Valid permissions are: {}",
                        key,
                        KNOWN_PERMISSIONS.join(", ")
                    ),
                });
            }
        }

        // Parse dependencies
        let dependencies = raw
            .deps
            .iter()
            .map(|dep_str| Dependency::parse(dep_str))
            .collect::<NexusResult<Vec<_>>>()?;

        Ok(ProjectConfig {
            module_name: raw.module,
            permissions: ProjectPermissions {
                compat_io: raw
                    .permissions
                    .get("compat.io")
                    .cloned()
                    .unwrap_or_default(),
                compat_fs: raw
                    .permissions
                    .get("compat.fs")
                    .cloned()
                    .unwrap_or_default(),
                compat_net: raw
                    .permissions
                    .get("compat.net")
                    .cloned()
                    .unwrap_or_default(),
                compat_proc: raw
                    .permissions
                    .get("compat.proc")
                    .cloned()
                    .unwrap_or_default(),
                plat_console: raw
                    .permissions
                    .get("plat.console")
                    .cloned()
                    .unwrap_or_default(),
                plat_desktop: raw
                    .permissions
                    .get("plat.desktop")
                    .cloned()
                    .unwrap_or_default(),
            },
            dependencies,
        })
    }

    /// Check if a module has permission to use compat.io
    pub fn has_compat_io_permission(&self, module: &str) -> bool {
        self.permissions.compat_io.iter().any(|m| m == module)
    }

    /// Check if a module has permission to use compat.fs
    pub fn has_compat_fs_permission(&self, module: &str) -> bool {
        self.permissions.compat_fs.iter().any(|m| m == module)
    }

    /// Check if a module has permission to use compat.net
    pub fn has_compat_net_permission(&self, module: &str) -> bool {
        self.permissions.compat_net.iter().any(|m| m == module)
    }

    /// Check if a module has permission to use compat.proc
    pub fn has_compat_proc_permission(&self, module: &str) -> bool {
        self.permissions.compat_proc.iter().any(|m| m == module)
    }

    /// Check if a module has permission to use plat.console
    pub fn has_plat_console_permission(&self, module: &str) -> bool {
        self.permissions.plat_console.iter().any(|m| m == module)
    }

    /// Check if a module has permission to use plat.desktop functions
    pub fn has_plat_desktop_permission(&self, module: &str) -> bool {
        self.permissions.plat_desktop.iter().any(|m| m == module)
    }

    /// Check if a module has permission to use any plat functions
    pub fn has_plat_permission(&self, module: &str) -> bool {
        self.has_plat_console_permission(module) || self.has_plat_desktop_permission(module)
    }

    /// Get all modules that have any permissions
    pub fn permitted_modules(&self) -> Vec<&str> {
        let mut modules: Vec<&str> = Vec::new();

        for m in &self.permissions.compat_io {
            if !modules.contains(&m.as_str()) {
                modules.push(m);
            }
        }
        for m in &self.permissions.compat_fs {
            if !modules.contains(&m.as_str()) {
                modules.push(m);
            }
        }
        for m in &self.permissions.compat_net {
            if !modules.contains(&m.as_str()) {
                modules.push(m);
            }
        }
        for m in &self.permissions.compat_proc {
            if !modules.contains(&m.as_str()) {
                modules.push(m);
            }
        }
        for m in &self.permissions.plat_console {
            if !modules.contains(&m.as_str()) {
                modules.push(m);
            }
        }
        for m in &self.permissions.plat_desktop {
            if !modules.contains(&m.as_str()) {
                modules.push(m);
            }
        }

        modules
    }

    /// Convert permissions to a HashMap for easier lookup
    pub fn permissions_map(&self) -> HashMap<String, Vec<String>> {
        let mut map = HashMap::new();

        if !self.permissions.compat_io.is_empty() {
            map.insert("compat.io".to_string(), self.permissions.compat_io.clone());
        }
        if !self.permissions.compat_fs.is_empty() {
            map.insert("compat.fs".to_string(), self.permissions.compat_fs.clone());
        }
        if !self.permissions.compat_net.is_empty() {
            map.insert(
                "compat.net".to_string(),
                self.permissions.compat_net.clone(),
            );
        }
        if !self.permissions.compat_proc.is_empty() {
            map.insert(
                "compat.proc".to_string(),
                self.permissions.compat_proc.clone(),
            );
        }
        if !self.permissions.plat_console.is_empty() {
            map.insert(
                "plat.console".to_string(),
                self.permissions.plat_console.clone(),
            );
        }
        if !self.permissions.plat_desktop.is_empty() {
            map.insert(
                "plat.desktop".to_string(),
                self.permissions.plat_desktop.clone(),
            );
        }

        map
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_minimal_config() {
        let content = r#"{
            mod: 'myproject',
            permissions: {},
            deps: []
        }"#;

        let config = ProjectConfig::parse_str(content).unwrap();
        assert_eq!(config.module_name, "myproject");
        assert!(config.dependencies.is_empty());
    }

    #[test]
    fn test_parse_with_permissions() {
        let content = r#"{
            mod: 'myproject',
            permissions: {
                "compat.io": ['main', 'utils'],
                "compat.fs": ['fileops']
            },
            deps: []
        }"#;

        let config = ProjectConfig::parse_str(content).unwrap();
        assert!(config.has_compat_io_permission("main"));
        assert!(config.has_compat_io_permission("utils"));
        assert!(!config.has_compat_io_permission("other"));
        assert!(config.has_compat_fs_permission("fileops"));
    }

    #[test]
    fn test_error_on_unknown_permission() {
        let content = r#"{
            mod: 'myproject',
            permissions: {
                "compatIO": ['main']
            },
            deps: []
        }"#;

        let result = ProjectConfig::parse_str(content);
        assert!(result.is_err());
        let err = result.unwrap_err();
        match err {
            NexusError::IoError { message } => {
                assert!(message.contains("Unknown permission"));
                assert!(message.contains("compatIO"));
            }
            _ => panic!("Expected IoError, got {:?}", err),
        }
    }

    #[test]
    fn test_error_on_old_format() {
        let content = r#"{
            mod: 'myproject',
            permissions: {
                "compatFS": ['fileops']
            },
            deps: []
        }"#;

        let result = ProjectConfig::parse_str(content);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_with_local_deps() {
        let content = r#"{
            mod: 'myproject',
            permissions: {},
            deps: [
                "local:./mathlib",
                "local:../shared/utils"
            ]
        }"#;

        let config = ProjectConfig::parse_str(content).unwrap();
        assert_eq!(config.dependencies.len(), 2);
    }

    #[test]
    fn test_permissions_map() {
        let content = r#"{
            mod: 'myproject',
            permissions: {
                "compat.io": ['main', 'utils'],
                "plat.console": ['native']
            },
            deps: []
        }"#;

        let config = ProjectConfig::parse_str(content).unwrap();
        let map = config.permissions_map();

        assert_eq!(
            map.get("compat.io"),
            Some(&vec!["main".to_string(), "utils".to_string()])
        );
        assert_eq!(map.get("plat.console"), Some(&vec!["native".to_string()]));
        assert!(!map.contains_key("compat.fs"));
    }

    #[test]
    fn test_plat_console_permission() {
        let content = r#"{
            mod: 'myproject',
            permissions: {
                "plat.console": ['native']
            },
            deps: []
        }"#;

        let config = ProjectConfig::parse_str(content).unwrap();
        assert!(config.has_plat_console_permission("native"));
        assert!(config.has_plat_permission("native"));
    }

    #[test]
    fn test_plat_desktop_permission() {
        let content = r#"{
            mod: 'myproject',
            permissions: {
                "plat.desktop": ['native']
            },
            deps: []
        }"#;

        let config = ProjectConfig::parse_str(content).unwrap();
        assert!(config.has_plat_desktop_permission("native"));
        assert!(config.has_plat_permission("native"));
    }
}
