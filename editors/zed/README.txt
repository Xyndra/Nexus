Nexus Language Extension for Zed
================================

This extension provides language support for the Nexus programming language,
including LSP integration for code intelligence features.


How the LSP Binary is Obtained
------------------------------

By default, the extension downloads the LSP binary from GitHub releases:
https://github.com/Xyndra/Nexus/releases

The extension automatically detects your platform and downloads the appropriate
binary for:
  - Windows x64 (x86_64-pc-windows-msvc)
  - Linux x64 (x86_64-unknown-linux-gnu)
  - macOS x64 (x86_64-apple-darwin)
  - macOS ARM64 (aarch64-apple-darwin)


Local Development
-----------------

When building the LSP server locally for development, the build process will
print instructions for installing to Zed:

  cargo build --release -p nexus-lsp-server

After the build completes, you'll see a message like:

  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    Nexus LSP: To install to Zed, run after build completes:

      copy "...\target\release\nexus-lsp.exe" "...\local-nexus-lsp.exe"

    To revert to GitHub releases version, delete:
      ...\local-nexus-lsp.exe
  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Simply copy and run the displayed command to install your local build.

The extension checks for a local-nexus-lsp binary first, and if found, uses it
instead of the downloaded version. This allows you to test changes without
affecting the released version.

To revert to the released version, delete the local-nexus-lsp binary file and
restart Zed. The extension will then download the latest release from GitHub.


Updating the LSP Version
------------------------

The LSP version used by the extension is defined in src/lib.rs as LSP_VERSION.
When a new version is released on GitHub, update this constant and reinstall
the extension to use the new version.
