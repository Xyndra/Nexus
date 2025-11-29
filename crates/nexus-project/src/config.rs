//! Project configuration parsing from `nexus.json5` files.

use crate::dependency::Dependency;
use nexus_core::{NexusError, NexusResult};
use serde::Deserialize;
use std::collections::HashMap;
use std::fs;
use std::path::Path;

/// Raw configuration as parsed from JSON5
#[derive(Debug, Deserialize)]
struct RawProjectConfig {
    /// Module name
    #[serde(rename = "mod")]
    module: String,

    /// Permissions configuration
    #[serde(default)]
    permissions: RawPermissions,

    /// Dependencies list
    #[serde(default)]
    deps: Vec<String>,
}

/// Raw permissions as parsed from JSON5
#[derive(Debug, Default, Deserialize)]
struct RawPermissions {
    /// Modules allowed to use compat.io
    #[serde(rename = "compatIO", default)]
    compat_io: Vec<String>,

    /// Modules allowed to use compat.fs
    #[serde(rename = "compatFS", default)]
    compat_fs: Vec<String>,

    /// Modules allowed to use compat.net
    #[serde(rename = "compatNet", default)]
    compat_net: Vec<String>,

    /// Modules allowed to use plat functions
    #[serde(rename = "plat", default)]
    plat: Vec<String>,
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

    /// Modules allowed to use platform-specific functions
    pub plat: Vec<String>,
}

impl ProjectConfig {
    /// Parse a project configuration from a file path
    pub fn from_file(path: &Path) -> NexusResult<Self> {
        let content = fs::read_to_string(path).map_err(|e| NexusError::IoError {
            message: format!("Failed to read config file '{}': {}", path.display(), e),
        })?;

        Self::from_str(&content)
    }

    /// Parse a project configuration from a string
    pub fn from_str(content: &str) -> NexusResult<Self> {
        let raw: RawProjectConfig = json5::from_str(content).map_err(|e| NexusError::IoError {
            message: format!("Failed to parse nexus.json5: {}", e),
        })?;

        // Parse dependencies
        let dependencies = raw
            .deps
            .iter()
            .map(|dep_str| Dependency::parse(dep_str))
            .collect::<NexusResult<Vec<_>>>()?;

        Ok(ProjectConfig {
            module_name: raw.module,
            permissions: ProjectPermissions {
                compat_io: raw.permissions.compat_io,
                compat_fs: raw.permissions.compat_fs,
                compat_net: raw.permissions.compat_net,
                plat: raw.permissions.plat,
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

    /// Check if a module has permission to use plat functions
    pub fn has_plat_permission(&self, module: &str) -> bool {
        self.permissions.plat.iter().any(|m| m == module)
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
        for m in &self.permissions.plat {
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
        if !self.permissions.plat.is_empty() {
            map.insert("plat".to_string(), self.permissions.plat.clone());
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

        let config = ProjectConfig::from_str(content).unwrap();
        assert_eq!(config.module_name, "myproject");
        assert!(config.dependencies.is_empty());
    }

    #[test]
    fn test_parse_with_permissions() {
        let content = r#"{
            mod: 'myproject',
            permissions: {
                compatIO: ['main', 'utils'],
                compatFS: ['fileops']
            },
            deps: []
        }"#;

        let config = ProjectConfig::from_str(content).unwrap();
        assert!(config.has_compat_io_permission("main"));
        assert!(config.has_compat_io_permission("utils"));
        assert!(!config.has_compat_io_permission("other"));
        assert!(config.has_compat_fs_permission("fileops"));
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

        let config = ProjectConfig::from_str(content).unwrap();
        assert_eq!(config.dependencies.len(), 2);
    }

    #[test]
    fn test_permissions_map() {
        let content = r#"{
            mod: 'myproject',
            permissions: {
                compatIO: ['main', 'utils'],
                plat: ['native']
            },
            deps: []
        }"#;

        let config = ProjectConfig::from_str(content).unwrap();
        let map = config.permissions_map();

        assert_eq!(
            map.get("compat.io"),
            Some(&vec!["main".to_string(), "utils".to_string()])
        );
        assert_eq!(map.get("plat"), Some(&vec!["native".to_string()]));
        assert!(map.get("compat.fs").is_none());
    }
}
