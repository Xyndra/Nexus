//! Dependency type definitions and parsing.
//!
//! Supports multiple dependency formats:
//! - `local:<path>` - Local filesystem path
//! - `git:<url>:<ref>` - Git repository at a specific commit/tag/branch
//! - `registry:<name>@<version>` - Package registry (future)

use nexus_core::{NexusError, NexusResult};
use std::path::PathBuf;

/// A parsed dependency declaration
#[derive(Debug, Clone, PartialEq)]
pub struct Dependency {
    /// The kind of dependency and its source
    pub kind: DependencyKind,

    /// Optional alias for the dependency (for renaming imports)
    pub alias: Option<String>,
}

/// The source type of a dependency
#[derive(Debug, Clone, PartialEq)]
pub enum DependencyKind {
    /// Local filesystem dependency
    /// Format: `local:<path>`
    Local {
        /// Path to the dependency (relative to project root)
        path: PathBuf,
    },

    /// Git repository dependency
    /// Format: `git:<url>:<ref>`
    Git {
        /// Repository URL
        url: String,
        /// Git reference (commit hash, tag, or branch)
        git_ref: String,
    },

    /// Package registry dependency (future)
    /// Format: `registry:<name>@<version>`
    Registry {
        /// Package name
        name: String,
        /// Version specifier
        version: String,
    },
}

impl Dependency {
    /// Parse a dependency string into a Dependency
    ///
    /// Supported formats:
    /// - `local:./path/to/dep`
    /// - `git:https://github.com/user/repo:v1.0.0`
    /// - `git:https://github.com/user/repo:abc1234`
    /// - `registry:package-name@1.0.0`
    pub fn parse(dep_str: &str) -> NexusResult<Self> {
        // Check for alias syntax: "alias=kind:source"
        let (alias, rest) = if let Some(eq_pos) = dep_str.find('=') {
            let alias = dep_str[..eq_pos].trim().to_string();
            let rest = &dep_str[eq_pos + 1..];
            (Some(alias), rest.trim())
        } else {
            (None, dep_str.trim())
        };

        // Parse the dependency kind
        let kind = if let Some(path) = rest.strip_prefix("local:") {
            DependencyKind::Local {
                path: PathBuf::from(path.trim()),
            }
        } else if let Some(git_spec) = rest.strip_prefix("git:") {
            // Format: git:<url>:<ref>
            // Need to handle URLs that contain colons (e.g., https://...)
            // Find the last colon which separates URL from ref
            if let Some(last_colon) = git_spec.rfind(':') {
                let url = git_spec[..last_colon].trim().to_string();
                let git_ref = git_spec[last_colon + 1..].trim().to_string();

                if url.is_empty() || git_ref.is_empty() {
                    return Err(NexusError::IoError {
                        message: format!(
                            "Invalid git dependency format '{}'. Expected 'git:<url>:<ref>'",
                            dep_str
                        ),
                    });
                }

                DependencyKind::Git { url, git_ref }
            } else {
                return Err(NexusError::IoError {
                    message: format!(
                        "Invalid git dependency format '{}'. Expected 'git:<url>:<ref>'",
                        dep_str
                    ),
                });
            }
        } else if let Some(registry_spec) = rest.strip_prefix("registry:") {
            // Format: registry:<name>@<version>
            if let Some(at_pos) = registry_spec.find('@') {
                let name = registry_spec[..at_pos].trim().to_string();
                let version = registry_spec[at_pos + 1..].trim().to_string();

                if name.is_empty() || version.is_empty() {
                    return Err(NexusError::IoError {
                        message: format!(
                            "Invalid registry dependency format '{}'. Expected 'registry:<name>@<version>'",
                            dep_str
                        ),
                    });
                }

                DependencyKind::Registry { name, version }
            } else {
                return Err(NexusError::IoError {
                    message: format!(
                        "Invalid registry dependency format '{}'. Expected 'registry:<name>@<version>'",
                        dep_str
                    ),
                });
            }
        } else {
            return Err(NexusError::IoError {
                message: format!(
                    "Unknown dependency format '{}'. Expected 'local:', 'git:', or 'registry:' prefix",
                    dep_str
                ),
            });
        };

        Ok(Dependency { kind, alias })
    }

    /// Get a human-readable description of this dependency
    pub fn description(&self) -> String {
        match &self.kind {
            DependencyKind::Local { path } => {
                format!("local dependency at '{}'", path.display())
            }
            DependencyKind::Git { url, git_ref } => {
                format!("git dependency '{}' at ref '{}'", url, git_ref)
            }
            DependencyKind::Registry { name, version } => {
                format!("registry dependency '{}' version '{}'", name, version)
            }
        }
    }

    /// Check if this dependency is a local path
    pub fn is_local(&self) -> bool {
        matches!(self.kind, DependencyKind::Local { .. })
    }

    /// Check if this dependency is a git repository
    pub fn is_git(&self) -> bool {
        matches!(self.kind, DependencyKind::Git { .. })
    }

    /// Check if this dependency is from a registry
    pub fn is_registry(&self) -> bool {
        matches!(self.kind, DependencyKind::Registry { .. })
    }

    /// Get the local path if this is a local dependency
    pub fn local_path(&self) -> Option<&PathBuf> {
        match &self.kind {
            DependencyKind::Local { path } => Some(path),
            _ => None,
        }
    }
}

impl DependencyKind {
    /// Check if this dependency kind is currently implemented
    pub fn is_implemented(&self) -> bool {
        match self {
            DependencyKind::Local { .. } => true,
            DependencyKind::Git { .. } => false, // TODO: Implement git cloning
            DependencyKind::Registry { .. } => false, // TODO: Implement registry
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_local_dependency() {
        let dep = Dependency::parse("local:./mathlib").unwrap();
        assert!(dep.is_local());
        assert_eq!(dep.local_path(), Some(&PathBuf::from("./mathlib")));
        assert!(dep.alias.is_none());
    }

    #[test]
    fn test_parse_local_dependency_with_parent() {
        let dep = Dependency::parse("local:../shared/utils").unwrap();
        assert!(dep.is_local());
        assert_eq!(dep.local_path(), Some(&PathBuf::from("../shared/utils")));
    }

    #[test]
    fn test_parse_git_dependency_with_tag() {
        let dep = Dependency::parse("git:https://github.com/user/repo:v1.0.0").unwrap();
        assert!(dep.is_git());
        match &dep.kind {
            DependencyKind::Git { url, git_ref } => {
                assert_eq!(url, "https://github.com/user/repo");
                assert_eq!(git_ref, "v1.0.0");
            }
            _ => panic!("Expected Git dependency"),
        }
    }

    #[test]
    fn test_parse_git_dependency_with_commit() {
        let dep = Dependency::parse("git:https://github.com/user/repo:abc1234def5678").unwrap();
        assert!(dep.is_git());
        match &dep.kind {
            DependencyKind::Git { url, git_ref } => {
                assert_eq!(url, "https://github.com/user/repo");
                assert_eq!(git_ref, "abc1234def5678");
            }
            _ => panic!("Expected Git dependency"),
        }
    }

    #[test]
    fn test_parse_registry_dependency() {
        let dep = Dependency::parse("registry:my-package@1.2.3").unwrap();
        assert!(dep.is_registry());
        match &dep.kind {
            DependencyKind::Registry { name, version } => {
                assert_eq!(name, "my-package");
                assert_eq!(version, "1.2.3");
            }
            _ => panic!("Expected Registry dependency"),
        }
    }

    #[test]
    fn test_parse_dependency_with_alias() {
        let dep = Dependency::parse("math=local:./mathlib").unwrap();
        assert!(dep.is_local());
        assert_eq!(dep.alias, Some("math".to_string()));
        assert_eq!(dep.local_path(), Some(&PathBuf::from("./mathlib")));
    }

    #[test]
    fn test_parse_invalid_format() {
        let result = Dependency::parse("invalid-format");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_empty_git_url() {
        let result = Dependency::parse("git::v1.0.0");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_empty_registry_version() {
        let result = Dependency::parse("registry:package@");
        assert!(result.is_err());
    }

    #[test]
    fn test_dependency_description() {
        let local = Dependency::parse("local:./lib").unwrap();
        assert!(local.description().contains("local dependency"));

        let git = Dependency::parse("git:https://example.com/repo:main").unwrap();
        assert!(git.description().contains("git dependency"));

        let registry = Dependency::parse("registry:pkg@1.0").unwrap();
        assert!(registry.description().contains("registry dependency"));
    }

    #[test]
    fn test_is_implemented() {
        let local = DependencyKind::Local {
            path: PathBuf::from("./lib"),
        };
        assert!(local.is_implemented());

        let git = DependencyKind::Git {
            url: "https://example.com".to_string(),
            git_ref: "main".to_string(),
        };
        assert!(!git.is_implemented());

        let registry = DependencyKind::Registry {
            name: "pkg".to_string(),
            version: "1.0".to_string(),
        };
        assert!(!registry.is_implemented());
    }
}
