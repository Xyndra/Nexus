//! Nexus Language Extension for Zed
//!
//! This extension provides language support for the Nexus programming language,
//! including LSP integration for code intelligence features.

use std::fs;
use zed_extension_api::{
    self as zed, current_platform, download_file, set_language_server_installation_status,
    Architecture, Command, DownloadedFileType, LanguageServerId, LanguageServerInstallationStatus,
    Os, Result,
};

const GITHUB_REPO: &str = "Xyndra/Nexus";
const LSP_VERSION: &str = "v0.1.0"; // Update this when releasing new versions

struct NexusExtension {
    cached_binary_path: Option<String>,
}

impl zed::Extension for NexusExtension {
    fn new() -> Self {
        Self {
            cached_binary_path: None,
        }
    }

    fn language_server_command(
        &mut self,
        language_server_id: &LanguageServerId,
        _worktree: &zed::Worktree,
    ) -> Result<Command> {
        if let Some(path) = &self.cached_binary_path {
            if fs::metadata(path).is_ok() {
                return Ok(Command {
                    command: path.clone(),
                    args: vec![],
                    env: vec![],
                });
            }
        }

        let binary_path = self.get_lsp_binary(language_server_id)?;
        self.cached_binary_path = Some(binary_path.clone());

        Ok(Command {
            command: binary_path,
            args: vec![],
            env: vec![],
        })
    }
}

impl NexusExtension {
    fn get_lsp_binary(&self, language_server_id: &LanguageServerId) -> Result<String> {
        let (os, arch) = current_platform();

        let (binary_name, target) = match (&os, &arch) {
            (Os::Windows, Architecture::X8664) => ("nexus-lsp.exe", "x86_64-pc-windows-msvc"),
            (Os::Linux, Architecture::X8664) => ("nexus-lsp", "x86_64-unknown-linux-gnu"),
            (Os::Mac, Architecture::X8664) => ("nexus-lsp", "x86_64-apple-darwin"),
            (Os::Mac, Architecture::Aarch64) => ("nexus-lsp", "aarch64-apple-darwin"),
            _ => return Err(format!("Unsupported platform: {:?} {:?}", os, arch)),
        };

        let binary_path = format!("./{}", binary_name);

        // Check if we have a local override binary
        let local_binary = format!("./local-{}", binary_name);
        if fs::metadata(&local_binary).is_ok() {
            // Copy to the expected location
            fs::copy(&local_binary, &binary_path)
                .map_err(|e| format!("Failed to copy local binary: {}", e))?;
            zed::make_file_executable(&binary_path)
                .map_err(|e| format!("Failed to make binary executable: {}", e))?;
            return Ok(binary_path);
        }

        // Check if binary already exists
        if fs::metadata(&binary_path).is_ok() {
            return Ok(binary_path);
        }

        // Download from GitHub releases
        set_language_server_installation_status(
            language_server_id,
            &LanguageServerInstallationStatus::CheckingForUpdate,
        );

        let download_url = format!(
            "https://github.com/{}/releases/download/{}/nexus-lsp-{}.zip",
            GITHUB_REPO, LSP_VERSION, target
        );

        set_language_server_installation_status(
            language_server_id,
            &LanguageServerInstallationStatus::Downloading,
        );

        // Download and extract the zip file
        let zip_path = format!("./nexus-lsp-{}.zip", target);
        download_file(&download_url, &zip_path, DownloadedFileType::Zip)
            .map_err(|e| format!("Failed to download LSP binary from {}: {}", download_url, e))?;

        // The zip contains the binary directly, move it to the right place
        if fs::metadata(binary_name).is_ok() {
            fs::rename(binary_name, &binary_path)
                .map_err(|e| format!("Failed to move binary: {}", e))?;
        }

        // Clean up the zip file
        let _ = fs::remove_file(&zip_path);

        // Make executable
        zed::make_file_executable(&binary_path)
            .map_err(|e| format!("Failed to make binary executable: {}", e))?;

        set_language_server_installation_status(
            language_server_id,
            &LanguageServerInstallationStatus::None,
        );

        Ok(binary_path)
    }
}

zed::register_extension!(NexusExtension);
