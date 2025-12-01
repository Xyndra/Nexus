//! Project configuration and dependency resolution for Nexus.
//!
//! This crate handles:
//! - Parsing `nexus.json5` project configuration files
//! - Resolving dependencies (local paths, git repos, etc.)
//! - Test discovery and execution

mod config;
mod dependency;
mod resolver;
mod test_runner;

pub use config::{ProjectConfig, ProjectPermissions};
pub use dependency::{Dependency, DependencyKind};
pub use resolver::{
    DependencyResolver, ModuleSource, ResolvedDependency, ResolvedProject, find_stdlib_path,
};
pub use test_runner::{TestDiscovery, TestFunction, TestResult, TestRunner, TestSummary};

use nexus_core::{NexusError, NexusResult};
use std::path::Path;

/// Load a project from a directory containing `nexus.json5`
pub fn load_project(project_dir: &Path) -> NexusResult<ProjectConfig> {
    let config_path = project_dir.join("nexus.json5");
    if !config_path.exists() {
        return Err(NexusError::IoError {
            message: format!(
                "No nexus.json5 found in project directory: {}",
                project_dir.display()
            ),
        });
    }

    ProjectConfig::from_file(&config_path)
}

/// Load and resolve a project with all its dependencies
pub fn load_and_resolve_project(project_dir: &Path) -> NexusResult<ResolvedProject> {
    let config = load_project(project_dir)?;
    let resolver = DependencyResolver::new(project_dir.to_path_buf());
    resolver.resolve(&config)
}

/// Load and resolve a project with all its dependencies, including the standard library
pub fn load_and_resolve_project_with_stdlib(project_dir: &Path) -> NexusResult<ResolvedProject> {
    let config = load_project(project_dir)?;
    let resolver = DependencyResolver::new(project_dir.to_path_buf());
    resolver.resolve_with_stdlib(&config)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn test_load_project_missing_config() {
        let dir = tempdir().unwrap();
        let result = load_project(dir.path());
        assert!(result.is_err());
    }

    #[test]
    fn test_load_project_valid_config() {
        let dir = tempdir().unwrap();
        let config_path = dir.path().join("nexus.json5");
        fs::write(
            &config_path,
            r#"{
                mod: 'test',
                permissions: {},
                deps: []
            }"#,
        )
        .unwrap();

        let result = load_project(dir.path());
        assert!(result.is_ok());
        let config = result.unwrap();
        assert_eq!(config.module_name, "test");
    }
}
