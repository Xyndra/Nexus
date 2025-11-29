//! Dependency resolution for Nexus projects.
//!
//! This module handles resolving dependencies from their declarations
//! to actual loadable source code.

use crate::config::ProjectConfig;
use crate::dependency::{Dependency, DependencyKind};
use nexus_core::{NexusError, NexusResult};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

/// A fully resolved project with all dependencies loaded
#[derive(Debug, Clone)]
pub struct ResolvedProject {
    /// The root project configuration
    pub config: ProjectConfig,

    /// Path to the project root
    pub root_path: PathBuf,

    /// Resolved dependencies (module name -> resolved dependency)
    pub dependencies: HashMap<String, ResolvedDependency>,

    /// Combined source from all modules (in load order)
    pub sources: Vec<ModuleSource>,
}

/// A resolved dependency ready to be loaded
#[derive(Debug, Clone)]
pub struct ResolvedDependency {
    /// The original dependency declaration
    pub dependency: Dependency,

    /// Module name from the dependency's nexus.json5
    pub module_name: String,

    /// Resolved absolute path to the dependency
    pub resolved_path: PathBuf,

    /// The dependency's own configuration
    pub config: ProjectConfig,
}

/// Source code for a single module
#[derive(Debug, Clone)]
pub struct ModuleSource {
    /// Module name
    pub name: String,

    /// Path to the source file
    pub path: PathBuf,

    /// Source code content
    pub content: String,
}

/// Resolver for project dependencies
pub struct DependencyResolver {
    /// Root path of the main project
    root_path: PathBuf,

    /// Already resolved dependencies (to avoid cycles and duplicates)
    resolved: HashMap<PathBuf, ResolvedDependency>,
}

impl DependencyResolver {
    /// Create a new resolver rooted at the given path
    pub fn new(root_path: PathBuf) -> Self {
        Self {
            root_path,
            resolved: HashMap::new(),
        }
    }

    /// Resolve all dependencies for a project
    pub fn resolve(&self, config: &ProjectConfig) -> NexusResult<ResolvedProject> {
        let mut resolver = DependencyResolver {
            root_path: self.root_path.clone(),
            resolved: HashMap::new(),
        };

        let mut dependencies = HashMap::new();
        let mut sources = Vec::new();

        // Resolve each dependency
        for dep in &config.dependencies {
            let resolved = resolver.resolve_dependency(dep, &self.root_path)?;

            // Load the dependency's source
            let source = resolver.load_module_source(&resolved)?;
            sources.push(source);

            dependencies.insert(resolved.module_name.clone(), resolved);
        }

        // Load the main module source (all .nx files in the root directory)
        let main_source = self.collect_module_source(&config.module_name, &self.root_path)?;
        sources.push(main_source);

        Ok(ResolvedProject {
            config: config.clone(),
            root_path: self.root_path.clone(),
            dependencies,
            sources,
        })
    }

    /// Collect all .nx files from a module directory into a single ModuleSource
    fn collect_module_source(&self, module_name: &str, dir: &PathBuf) -> NexusResult<ModuleSource> {
        let mut nx_files: Vec<PathBuf> = Vec::new();
        collect_nx_files_recursive(dir, &mut nx_files)?;

        if nx_files.is_empty() {
            return Err(NexusError::IoError {
                message: format!(
                    "No .nx files found in module '{}' at '{}'",
                    module_name,
                    dir.display()
                ),
            });
        }

        // Sort for consistent ordering
        nx_files.sort();

        // Combine all sources
        let mut combined_content = String::new();
        for file in &nx_files {
            let content = fs::read_to_string(file).map_err(|e| NexusError::IoError {
                message: format!("Failed to read '{}': {}", file.display(), e),
            })?;
            combined_content.push_str(&content);
            combined_content.push('\n');
        }

        Ok(ModuleSource {
            name: module_name.to_string(),
            path: dir.clone(),
            content: combined_content,
        })
    }

    /// Resolve a single dependency
    fn resolve_dependency(
        &mut self,
        dep: &Dependency,
        relative_to: &Path,
    ) -> NexusResult<ResolvedDependency> {
        match &dep.kind {
            DependencyKind::Local { path } => self.resolve_local(dep, path, relative_to),
            DependencyKind::Git { url, git_ref } => self.resolve_git(dep, url, git_ref),
            DependencyKind::Registry { name, version } => self.resolve_registry(dep, name, version),
        }
    }

    /// Resolve a local filesystem dependency
    fn resolve_local(
        &mut self,
        dep: &Dependency,
        path: &PathBuf,
        relative_to: &Path,
    ) -> NexusResult<ResolvedDependency> {
        // Resolve the path relative to the parent project
        let resolved_path = if path.is_absolute() {
            path.clone()
        } else {
            relative_to.join(path)
        };

        // Canonicalize to handle .. and symlinks
        let canonical_path = fs::canonicalize(&resolved_path).map_err(|e| NexusError::IoError {
            message: format!(
                "Failed to resolve local dependency path '{}': {}",
                resolved_path.display(),
                e
            ),
        })?;

        // Check if already resolved (cycle detection)
        if let Some(existing) = self.resolved.get(&canonical_path) {
            return Ok(existing.clone());
        }

        // Check that the path exists and is a directory
        if !canonical_path.is_dir() {
            return Err(NexusError::IoError {
                message: format!(
                    "Local dependency path '{}' is not a directory",
                    canonical_path.display()
                ),
            });
        }

        // Load the dependency's configuration
        let config_path = canonical_path.join("nexus.json5");
        if !config_path.exists() {
            return Err(NexusError::IoError {
                message: format!(
                    "Local dependency '{}' has no nexus.json5",
                    canonical_path.display()
                ),
            });
        }

        let config = ProjectConfig::from_file(&config_path)?;
        let module_name = dep
            .alias
            .clone()
            .unwrap_or_else(|| config.module_name.clone());

        let resolved = ResolvedDependency {
            dependency: dep.clone(),
            module_name,
            resolved_path: canonical_path.clone(),
            config,
        };

        // Cache the resolved dependency
        self.resolved.insert(canonical_path, resolved.clone());

        // Recursively resolve the dependency's own dependencies
        for sub_dep in &resolved.config.dependencies {
            self.resolve_dependency(sub_dep, &resolved.resolved_path)?;
        }

        Ok(resolved)
    }

    /// Resolve a git repository dependency (NOT IMPLEMENTED)
    fn resolve_git(
        &mut self,
        _dep: &Dependency,
        url: &str,
        git_ref: &str,
    ) -> NexusResult<ResolvedDependency> {
        // Git dependencies are not yet implemented
        // In the future, this would:
        // 1. Clone or fetch the repository to a cache directory
        // 2. Checkout the specified ref
        // 3. Load the nexus.json5 from the repo
        // 4. Return the resolved dependency

        Err(NexusError::IoError {
            message: format!(
                "Git dependencies are not yet implemented. Cannot resolve: git:{}:{}",
                url, git_ref
            ),
        })
    }

    /// Resolve a registry dependency (NOT IMPLEMENTED)
    fn resolve_registry(
        &mut self,
        _dep: &Dependency,
        name: &str,
        version: &str,
    ) -> NexusResult<ResolvedDependency> {
        // Registry dependencies are not yet implemented
        // In the future, this would:
        // 1. Query the package registry for the package
        // 2. Download and cache the package
        // 3. Load the nexus.json5 from the package
        // 4. Return the resolved dependency

        Err(NexusError::IoError {
            message: format!(
                "Registry dependencies are not yet implemented. Cannot resolve: registry:{}@{}",
                name, version
            ),
        })
    }

    /// Load the source code for a resolved dependency
    fn load_module_source(&self, resolved: &ResolvedDependency) -> NexusResult<ModuleSource> {
        self.collect_module_source(&resolved.module_name, &resolved.resolved_path)
    }
}

/// Recursively collect .nx files, stopping at subdirectories with their own nexus.json5
fn collect_nx_files_recursive(dir: &PathBuf, files: &mut Vec<PathBuf>) -> NexusResult<()> {
    let entries = fs::read_dir(dir).map_err(|e| NexusError::IoError {
        message: format!("Failed to read directory '{}': {}", dir.display(), e),
    })?;

    for entry in entries {
        let entry = entry.map_err(|e| NexusError::IoError {
            message: format!("Failed to read directory entry: {}", e),
        })?;
        let path = entry.path();

        if path.is_dir() {
            // Check if this subdirectory has its own nexus.json5
            // If so, it's a separate module and we skip it
            let subdir_config = path.join("nexus.json5");
            if !subdir_config.exists() {
                collect_nx_files_recursive(&path, files)?;
            }
        } else if path.extension().is_some_and(|ext| ext == "nx") {
            files.push(path);
        }
    }

    Ok(())
}

impl ResolvedProject {
    /// Get all source code concatenated in dependency order
    pub fn combined_source(&self) -> String {
        self.sources
            .iter()
            .map(|s| s.content.as_str())
            .collect::<Vec<_>>()
            .join("\n\n")
    }

    /// Get the source for a specific module
    pub fn get_module_source(&self, name: &str) -> Option<&ModuleSource> {
        self.sources.iter().find(|s| s.name == name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    fn create_test_project(
        dir: &std::path::Path,
        name: &str,
        main_content: &str,
    ) -> std::io::Result<()> {
        let config = format!(
            r#"{{
            mod: '{}',
            permissions: {{}},
            deps: []
        }}"#,
            name
        );
        fs::write(dir.join("nexus.json5"), config)?;
        fs::write(dir.join("module.nx"), main_content)?;
        Ok(())
    }

    #[test]
    fn test_resolve_local_dependency() {
        let temp = tempdir().unwrap();
        let root = temp.path();

        // Create main project
        let main_config = r#"{
            mod: 'main',
            permissions: {},
            deps: ["local:./lib"]
        }"#;
        fs::write(root.join("nexus.json5"), main_config).unwrap();
        fs::write(root.join("app.nx"), "// main").unwrap();

        // Create lib dependency
        let lib_dir = root.join("lib");
        fs::create_dir(&lib_dir).unwrap();
        create_test_project(&lib_dir, "lib", "// lib source").unwrap();

        // Resolve
        let config = ProjectConfig::from_file(&root.join("nexus.json5")).unwrap();
        let resolver = DependencyResolver::new(root.to_path_buf());
        let resolved = resolver.resolve(&config).unwrap();

        assert_eq!(resolved.dependencies.len(), 1);
        assert!(resolved.dependencies.contains_key("lib"));
        assert_eq!(resolved.sources.len(), 2); // lib + main
    }

    #[test]
    fn test_resolve_missing_dependency() {
        let temp = tempdir().unwrap();
        let root = temp.path();

        let config_content = r#"{
            mod: 'main',
            permissions: {},
            deps: ["local:./nonexistent"]
        }"#;
        fs::write(root.join("nexus.json5"), config_content).unwrap();
        fs::write(root.join("app.nx"), "// main").unwrap();

        let config = ProjectConfig::from_file(&root.join("nexus.json5")).unwrap();
        let resolver = DependencyResolver::new(root.to_path_buf());
        let result = resolver.resolve(&config);

        assert!(result.is_err());
    }

    #[test]
    fn test_combined_source() {
        let project = ResolvedProject {
            config: ProjectConfig {
                module_name: "test".to_string(),
                permissions: Default::default(),
                dependencies: vec![],
            },
            root_path: PathBuf::from("."),
            dependencies: HashMap::new(),
            sources: vec![
                ModuleSource {
                    name: "lib".to_string(),
                    path: PathBuf::from("lib"),
                    content: "// lib code".to_string(),
                },
                ModuleSource {
                    name: "main".to_string(),
                    path: PathBuf::from("."),
                    content: "// main code".to_string(),
                },
            ],
        };

        let combined = project.combined_source();
        assert!(combined.contains("// lib code"));
        assert!(combined.contains("// main code"));
    }
}
