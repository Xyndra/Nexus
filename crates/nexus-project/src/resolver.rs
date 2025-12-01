//! Dependency resolution for Nexus projects.
//!
//! This module handles resolving dependencies from their declarations
//! to actual loadable source code.
//!
//! ## Hierarchical Module Structure
//!
//! Modules can contain submodules through subdirectories. A subdirectory
//! automatically becomes a submodule with a dotted name based on its path.
//!
//! For example, given this directory structure:
//! ```text
//! std/
//!   lib.nx           # std module code
//!   util/
//!     lib.nx         # std.util module code
//!     strings/
//!       lib.nx       # std.util.strings module code
//! ```
//!
//! The resolver will load three modules: `std`, `std.util`, and `std.util.strings`.
//!
//! Subdirectories with a `nexus.json5` file are treated as independent modules
//! and are NOT loaded as submodules (they must be imported separately as dependencies).

use crate::config::ProjectConfig;
use crate::dependency::{Dependency, DependencyKind};
use nexus_core::{NexusError, NexusResult};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

/// Get the path to the standard library root directory
/// This looks in several locations:
/// 1. NEXUS_STDLIB environment variable
/// 2. Relative to the executable (for installed versions)
/// 3. Development path (relative to crate)
pub fn find_stdlib_path() -> Option<PathBuf> {
    // Check environment variable first
    if let Ok(path) = std::env::var("NEXUS_STDLIB") {
        let p = PathBuf::from(path);
        if p.exists() {
            return Some(p);
        }
    }

    // Check relative to executable
    if let Ok(exe) = std::env::current_exe() {
        if let Some(exe_dir) = exe.parent() {
            let stdlib = exe_dir.join("stdlib").join("std");
            if stdlib.exists() {
                return Some(stdlib);
            }
            // Also check one level up (for bin/ layout)
            let stdlib = exe_dir.parent().map(|p| p.join("stdlib").join("std"));
            if let Some(ref s) = stdlib {
                if s.exists() {
                    return stdlib;
                }
            }
        }
    }

    // Development path - relative to this crate's manifest
    let dev_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .map(|p| p.join("stdlib").join("std"));
    if let Some(ref p) = dev_path {
        if p.exists() {
            return dev_path;
        }
    }

    None
}

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
    /// This includes the main module and all submodules
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
    /// Module name (may be dotted for submodules, e.g., "std.util.strings")
    pub name: String,

    /// Path to the module directory
    pub path: PathBuf,

    /// Source code content (combined from all .nx files in the module directory)
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

    /// Resolve all dependencies for a project (without stdlib)
    pub fn resolve(&self, config: &ProjectConfig) -> NexusResult<ResolvedProject> {
        self.resolve_with_options(config, false)
    }

    /// Resolve all dependencies for a project, optionally including stdlib
    pub fn resolve_with_stdlib(&self, config: &ProjectConfig) -> NexusResult<ResolvedProject> {
        self.resolve_with_options(config, true)
    }

    /// Resolve all dependencies for a project with options
    fn resolve_with_options(
        &self,
        config: &ProjectConfig,
        include_stdlib: bool,
    ) -> NexusResult<ResolvedProject> {
        let mut resolver = DependencyResolver {
            root_path: self.root_path.clone(),
            resolved: HashMap::new(),
        };

        let mut dependencies = HashMap::new();
        let mut sources = Vec::new();

        // Load stdlib first if requested (with all submodules)
        if include_stdlib {
            if let Some(stdlib_path) = find_stdlib_path() {
                let stdlib_sources = self.collect_module_with_submodules("std", &stdlib_path)?;
                sources.extend(stdlib_sources);
            }
        }

        // Resolve each dependency
        for dep in &config.dependencies {
            let resolved = resolver.resolve_dependency(dep, &self.root_path)?;

            // Load the dependency's source with all submodules
            let dep_sources = resolver
                .load_module_with_submodules(&resolved.module_name, &resolved.resolved_path)?;
            sources.extend(dep_sources);

            dependencies.insert(resolved.module_name.clone(), resolved);
        }

        // Load the main module source with all submodules
        let main_sources =
            self.collect_module_with_submodules(&config.module_name, &self.root_path)?;
        sources.extend(main_sources);

        Ok(ResolvedProject {
            config: config.clone(),
            root_path: self.root_path.clone(),
            dependencies,
            sources,
        })
    }

    /// Collect a module and all its submodules recursively.
    /// Returns a list of ModuleSource in hierarchical order (parent before children).
    fn collect_module_with_submodules(
        &self,
        module_name: &str,
        dir: &PathBuf,
    ) -> NexusResult<Vec<ModuleSource>> {
        let mut sources = Vec::new();

        // First, collect .nx files in the current directory (not recursive)
        let module_source = self.collect_module_source(module_name, dir)?;
        sources.push(module_source);

        // Then, find and process subdirectories as submodules
        let entries = fs::read_dir(dir).map_err(|e| NexusError::IoError {
            message: format!("Failed to read directory '{}': {}", dir.display(), e),
        })?;

        let mut subdirs: Vec<(String, PathBuf)> = Vec::new();

        for entry in entries {
            let entry = entry.map_err(|e| NexusError::IoError {
                message: format!("Failed to read directory entry: {}", e),
            })?;
            let path = entry.path();

            if path.is_dir() {
                // Check if this subdirectory has its own nexus.json5
                // If so, it's an independent module and should NOT be loaded as a submodule
                let subdir_config = path.join("nexus.json5");
                if subdir_config.exists() {
                    continue;
                }

                // Check if it has any .nx files (directly or in subdirs)
                if has_nx_files(&path) {
                    if let Some(subdir_name) = path.file_name().and_then(|n| n.to_str()) {
                        let submodule_name = format!("{}.{}", module_name, subdir_name);
                        subdirs.push((submodule_name, path));
                    }
                }
            }
        }

        // Sort subdirectories for consistent ordering
        subdirs.sort_by(|a, b| a.0.cmp(&b.0));

        // Recursively collect submodules
        for (submodule_name, subdir_path) in subdirs {
            let submodule_sources =
                self.collect_module_with_submodules(&submodule_name, &subdir_path)?;
            sources.extend(submodule_sources);
        }

        Ok(sources)
    }

    /// Collect .nx files from a single directory (non-recursive) into a ModuleSource.
    fn collect_module_source(&self, module_name: &str, dir: &PathBuf) -> NexusResult<ModuleSource> {
        let mut nx_files: Vec<PathBuf> = Vec::new();

        let entries = fs::read_dir(dir).map_err(|e| NexusError::IoError {
            message: format!("Failed to read directory '{}': {}", dir.display(), e),
        })?;

        for entry in entries {
            let entry = entry.map_err(|e| NexusError::IoError {
                message: format!("Failed to read directory entry: {}", e),
            })?;
            let path = entry.path();

            // Only collect .nx files directly in this directory (not subdirectories)
            if path.is_file() && path.extension().is_some_and(|ext| ext == "nx") {
                nx_files.push(path);
            }
        }

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

    /// Load a module and its submodules for a resolved dependency
    fn load_module_with_submodules(
        &self,
        module_name: &str,
        dir: &PathBuf,
    ) -> NexusResult<Vec<ModuleSource>> {
        self.collect_module_with_submodules(module_name, dir)
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
        Err(NexusError::IoError {
            message: format!(
                "Registry dependencies are not yet implemented. Cannot resolve: registry:{}@{}",
                name, version
            ),
        })
    }
}

/// Check if a directory contains any .nx files (recursively)
fn has_nx_files(dir: &Path) -> bool {
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_file() && path.extension().is_some_and(|ext| ext == "nx") {
                return true;
            }
            if path.is_dir() && has_nx_files(&path) {
                return true;
            }
        }
    }
    false
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

    /// Get the source for a specific module (exact match)
    pub fn get_module_source(&self, name: &str) -> Option<&ModuleSource> {
        self.sources.iter().find(|s| s.name == name)
    }

    /// Get all modules that start with a given prefix (e.g., "std" matches "std", "std.util", etc.)
    pub fn get_modules_with_prefix(&self, prefix: &str) -> Vec<&ModuleSource> {
        self.sources
            .iter()
            .filter(|s| s.name == prefix || s.name.starts_with(&format!("{}.", prefix)))
            .collect()
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
        fs::write(dir.join("lib.nx"), main_content)?;
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
    fn test_hierarchical_submodules() {
        let temp = tempdir().unwrap();
        let root = temp.path();

        // Create main project with submodules
        let main_config = r#"{
            mod: 'mymod',
            permissions: {},
            deps: []
        }"#;
        fs::write(root.join("nexus.json5"), main_config).unwrap();
        fs::write(root.join("lib.nx"), "// mymod root").unwrap();

        // Create submodule: mymod.util
        let util_dir = root.join("util");
        fs::create_dir(&util_dir).unwrap();
        fs::write(util_dir.join("lib.nx"), "// mymod.util").unwrap();

        // Create sub-submodule: mymod.util.strings
        let strings_dir = util_dir.join("strings");
        fs::create_dir(&strings_dir).unwrap();
        fs::write(strings_dir.join("lib.nx"), "// mymod.util.strings").unwrap();

        // Resolve
        let config = ProjectConfig::from_file(&root.join("nexus.json5")).unwrap();
        let resolver = DependencyResolver::new(root.to_path_buf());
        let resolved = resolver.resolve(&config).unwrap();

        // Should have 3 modules: mymod, mymod.util, mymod.util.strings
        assert_eq!(resolved.sources.len(), 3);

        let module_names: Vec<&str> = resolved.sources.iter().map(|s| s.name.as_str()).collect();
        assert!(module_names.contains(&"mymod"));
        assert!(module_names.contains(&"mymod.util"));
        assert!(module_names.contains(&"mymod.util.strings"));

        // Check content
        let mymod = resolved.get_module_source("mymod").unwrap();
        assert!(mymod.content.contains("// mymod root"));

        let util = resolved.get_module_source("mymod.util").unwrap();
        assert!(util.content.contains("// mymod.util"));

        let strings = resolved.get_module_source("mymod.util.strings").unwrap();
        assert!(strings.content.contains("// mymod.util.strings"));
    }

    #[test]
    fn test_subdir_with_nexus_json5_is_independent() {
        let temp = tempdir().unwrap();
        let root = temp.path();

        // Create main project
        let main_config = r#"{
            mod: 'main',
            permissions: {},
            deps: []
        }"#;
        fs::write(root.join("nexus.json5"), main_config).unwrap();
        fs::write(root.join("lib.nx"), "// main").unwrap();

        // Create subdir with its own nexus.json5 (should NOT be loaded as submodule)
        let other_dir = root.join("other");
        fs::create_dir(&other_dir).unwrap();
        create_test_project(&other_dir, "other", "// other module").unwrap();

        // Resolve
        let config = ProjectConfig::from_file(&root.join("nexus.json5")).unwrap();
        let resolver = DependencyResolver::new(root.to_path_buf());
        let resolved = resolver.resolve(&config).unwrap();

        // Should only have 1 module (main), not main.other
        assert_eq!(resolved.sources.len(), 1);
        assert_eq!(resolved.sources[0].name, "main");
    }

    #[test]
    fn test_get_modules_with_prefix() {
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
                    name: "std".to_string(),
                    path: PathBuf::from("std"),
                    content: "// std".to_string(),
                },
                ModuleSource {
                    name: "std.util".to_string(),
                    path: PathBuf::from("std/util"),
                    content: "// std.util".to_string(),
                },
                ModuleSource {
                    name: "std.util.strings".to_string(),
                    path: PathBuf::from("std/util/strings"),
                    content: "// std.util.strings".to_string(),
                },
                ModuleSource {
                    name: "other".to_string(),
                    path: PathBuf::from("other"),
                    content: "// other".to_string(),
                },
            ],
        };

        let std_modules = project.get_modules_with_prefix("std");
        assert_eq!(std_modules.len(), 3);

        let util_modules = project.get_modules_with_prefix("std.util");
        assert_eq!(util_modules.len(), 2);

        let other_modules = project.get_modules_with_prefix("other");
        assert_eq!(other_modules.len(), 1);
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
