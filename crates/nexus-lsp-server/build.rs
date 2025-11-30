//! Build script for nexus-lsp-server
//!
//! During local development, this script prints instructions for copying
//! the compiled LSP binary to Zed's extension directory for easy testing.
//!
//! This behavior is disabled in CI builds (when NEXUS_CI env var is set).

use std::env;
use std::path::PathBuf;

fn main() {
    // Skip in CI builds
    if env::var("NEXUS_CI").is_ok() {
        return;
    }

    // Only show instructions for release builds
    let profile = env::var("PROFILE").unwrap_or_default();
    if profile != "release" {
        return;
    }

    // Determine the Zed extensions directory based on OS
    let zed_extension_dir = get_zed_extension_dir();
    if let Some(dir) = zed_extension_dir {
        let nexus_ext_dir = dir.join("work").join("nexus");

        // Only proceed if the nexus extension is installed
        if nexus_ext_dir.exists() {
            let binary_name = if cfg!(windows) {
                "nexus-lsp.exe"
            } else {
                "nexus-lsp"
            };
            let local_binary_name = if cfg!(windows) {
                "local-nexus-lsp.exe"
            } else {
                "local-nexus-lsp"
            };

            let target_dir = get_target_dir();
            let source = target_dir.join("release").join(binary_name);
            let dest = nexus_ext_dir.join(local_binary_name);

            println!("cargo:warning=");
            println!(
                "cargo:warning=━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            );
            println!("cargo:warning=  Nexus LSP: To install to Zed, run after build completes:");
            println!("cargo:warning=");

            if cfg!(windows) {
                println!(
                    "cargo:warning=    copy \"{}\" \"{}\"",
                    source.display(),
                    dest.display()
                );
            } else {
                println!(
                    "cargo:warning=    cp \"{}\" \"{}\"",
                    source.display(),
                    dest.display()
                );
            }

            println!("cargo:warning=");
            println!("cargo:warning=  To revert to GitHub releases version, delete:");
            println!("cargo:warning=    {}", dest.display());
            println!(
                "cargo:warning=━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            );
            println!("cargo:warning=");
        }
    }

    println!("cargo:rerun-if-env-changed=NEXUS_CI");
}

fn get_zed_extension_dir() -> Option<PathBuf> {
    if cfg!(windows) {
        // Windows: %LOCALAPPDATA%\Zed\extensions
        env::var("LOCALAPPDATA")
            .ok()
            .map(|p| PathBuf::from(p).join("Zed").join("extensions"))
    } else if cfg!(target_os = "macos") {
        // macOS: ~/Library/Application Support/Zed/extensions
        env::var("HOME").ok().map(|p| {
            PathBuf::from(p)
                .join("Library")
                .join("Application Support")
                .join("Zed")
                .join("extensions")
        })
    } else {
        // Linux: ~/.local/share/zed/extensions
        env::var("HOME").ok().map(|p| {
            PathBuf::from(p)
                .join(".local")
                .join("share")
                .join("zed")
                .join("extensions")
        })
    }
}

fn get_target_dir() -> PathBuf {
    // Try to find the target directory from OUT_DIR
    // OUT_DIR is typically: <target_dir>/<profile>/build/<crate>/out
    let out_dir = env::var("OUT_DIR").unwrap();
    let out_path = PathBuf::from(&out_dir);

    // Navigate up to find target dir
    // out -> <hash> -> build -> <profile> -> target
    out_path
        .parent()
        .and_then(|p| p.parent())
        .and_then(|p| p.parent())
        .and_then(|p| p.parent())
        .map(|p| p.to_path_buf())
        .unwrap_or_else(|| PathBuf::from("target"))
}
