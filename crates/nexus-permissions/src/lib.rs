//! Permission system for the Nexus programming language.
//!
//! This crate provides the permission system that controls what capabilities
//! dependencies and code can use. It enforces function coloring rules and
//! manages platform-specific permissions.

use nexus_core::{FunctionColor, NexusError, Span};
use std::collections::{HashMap, HashSet};

/// A specific permission that can be granted or denied.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Permission {
    /// Pure logic only (always allowed)
    Std,
    /// Base compatibility API
    Compat(CompatPermission),
    /// Platform-specific permission
    Plat(PlatPermission),
}

impl Permission {
    /// Parse a permission from a string like "compat.io" or "plat.android.vibrate"
    pub fn parse(s: &str) -> Option<Self> {
        let parts: Vec<&str> = s.split('.').collect();
        match *parts.first()? {
            "std" => Some(Permission::Std),
            "compat" => {
                if parts.len() == 1 {
                    Some(Permission::Compat(CompatPermission::All))
                } else {
                    CompatPermission::parse(&parts[1..]).map(Permission::Compat)
                }
            }
            "plat" => {
                if parts.len() == 1 {
                    Some(Permission::Plat(PlatPermission::All))
                } else {
                    PlatPermission::parse(&parts[1..]).map(Permission::Plat)
                }
            }
            _ => None,
        }
    }

    /// Get the function color required for this permission
    pub fn required_color(&self) -> FunctionColor {
        match self {
            Permission::Std => FunctionColor::Std,
            Permission::Compat(_) => FunctionColor::Compat,
            Permission::Plat(_) => FunctionColor::Plat,
        }
    }
}

impl std::fmt::Display for Permission {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Permission::Std => write!(f, "std"),
            Permission::Compat(c) => write!(f, "compat.{}", c),
            Permission::Plat(p) => write!(f, "plat.{}", p),
        }
    }
}

/// Compatibility layer permissions (cross-platform APIs)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CompatPermission {
    /// All compat permissions
    All,
    /// File I/O operations
    Io,
    /// Network operations
    Net,
    /// Environment variables
    Env,
    /// Process operations
    Process,
    /// Time operations
    Time,
    /// Random number generation
    Random,
    /// Filesystem operations
    Fs,
    /// Console/terminal operations
    Console,
    /// Custom/other permission
    Custom(String),
}

impl CompatPermission {
    fn parse(parts: &[&str]) -> Option<Self> {
        if parts.is_empty() {
            return Some(CompatPermission::All);
        }
        match parts[0] {
            "io" => Some(CompatPermission::Io),
            "net" => Some(CompatPermission::Net),
            "env" => Some(CompatPermission::Env),
            "process" => Some(CompatPermission::Process),
            "time" => Some(CompatPermission::Time),
            "random" => Some(CompatPermission::Random),
            "fs" => Some(CompatPermission::Fs),
            "console" => Some(CompatPermission::Console),
            other => Some(CompatPermission::Custom(other.to_string())),
        }
    }

    /// Check if this permission includes another permission
    pub fn includes(&self, other: &CompatPermission) -> bool {
        match self {
            CompatPermission::All => true,
            _ => self == other,
        }
    }
}

impl std::fmt::Display for CompatPermission {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompatPermission::All => write!(f, "*"),
            CompatPermission::Io => write!(f, "io"),
            CompatPermission::Net => write!(f, "net"),
            CompatPermission::Env => write!(f, "env"),
            CompatPermission::Process => write!(f, "process"),
            CompatPermission::Time => write!(f, "time"),
            CompatPermission::Random => write!(f, "random"),
            CompatPermission::Fs => write!(f, "fs"),
            CompatPermission::Console => write!(f, "console"),
            CompatPermission::Custom(s) => write!(f, "{}", s),
        }
    }
}

/// Platform-specific permissions
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PlatPermission {
    /// All platform permissions
    All,
    /// Android-specific permissions
    Android(AndroidPermission),
    /// iOS-specific permissions
    Ios(IosPermission),
    /// Desktop x64 permissions
    DesktopX64(DesktopX64Permission),
    /// Web/WASM permissions
    Web(WebPermission),
    /// Custom platform permission
    Custom(String, String),
}

impl PlatPermission {
    fn parse(parts: &[&str]) -> Option<Self> {
        if parts.is_empty() {
            return Some(PlatPermission::All);
        }
        match parts[0] {
            "android" => {
                if parts.len() == 1 {
                    Some(PlatPermission::Android(AndroidPermission::All))
                } else {
                    AndroidPermission::parse(parts[1]).map(PlatPermission::Android)
                }
            }
            "ios" => {
                if parts.len() == 1 {
                    Some(PlatPermission::Ios(IosPermission::All))
                } else {
                    IosPermission::parse(parts[1]).map(PlatPermission::Ios)
                }
            }
            "desktop_x64" => {
                if parts.len() == 1 {
                    Some(PlatPermission::DesktopX64(DesktopX64Permission::All))
                } else {
                    DesktopX64Permission::parse(parts[1]).map(PlatPermission::DesktopX64)
                }
            }
            "web" => {
                if parts.len() == 1 {
                    Some(PlatPermission::Web(WebPermission::All))
                } else {
                    WebPermission::parse(parts[1]).map(PlatPermission::Web)
                }
            }
            platform => {
                let feature = if parts.len() > 1 { parts[1] } else { "*" };
                Some(PlatPermission::Custom(
                    platform.to_string(),
                    feature.to_string(),
                ))
            }
        }
    }

    /// Check if this permission includes another permission
    pub fn includes(&self, other: &PlatPermission) -> bool {
        match self {
            PlatPermission::All => true,
            PlatPermission::Android(a) => {
                matches!(other, PlatPermission::Android(b) if a.includes(b))
            }
            PlatPermission::Ios(a) => {
                matches!(other, PlatPermission::Ios(b) if a.includes(b))
            }
            PlatPermission::DesktopX64(a) => {
                matches!(other, PlatPermission::DesktopX64(b) if a.includes(b))
            }
            PlatPermission::Web(a) => {
                matches!(other, PlatPermission::Web(b) if a.includes(b))
            }
            PlatPermission::Custom(p1, f1) => {
                matches!(other, PlatPermission::Custom(p2, f2) if p1 == p2 && (f1 == "*" || f1 == f2))
            }
        }
    }
}

impl std::fmt::Display for PlatPermission {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PlatPermission::All => write!(f, "*"),
            PlatPermission::Android(p) => write!(f, "android.{}", p),
            PlatPermission::Ios(p) => write!(f, "ios.{}", p),
            PlatPermission::DesktopX64(p) => write!(f, "desktop_x64.{}", p),
            PlatPermission::Web(p) => write!(f, "web.{}", p),
            PlatPermission::Custom(plat, feat) => write!(f, "{}.{}", plat, feat),
        }
    }
}

/// Android-specific permissions
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AndroidPermission {
    All,
    Vibrate,
    Camera,
    Location,
    Storage,
    Bluetooth,
    Nfc,
    Custom(String),
}

impl AndroidPermission {
    fn parse(s: &str) -> Option<Self> {
        match s {
            "*" => Some(AndroidPermission::All),
            "vibrate" => Some(AndroidPermission::Vibrate),
            "camera" => Some(AndroidPermission::Camera),
            "location" => Some(AndroidPermission::Location),
            "storage" => Some(AndroidPermission::Storage),
            "bluetooth" => Some(AndroidPermission::Bluetooth),
            "nfc" => Some(AndroidPermission::Nfc),
            other => Some(AndroidPermission::Custom(other.to_string())),
        }
    }

    fn includes(&self, other: &AndroidPermission) -> bool {
        match self {
            AndroidPermission::All => true,
            _ => self == other,
        }
    }
}

impl std::fmt::Display for AndroidPermission {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AndroidPermission::All => write!(f, "*"),
            AndroidPermission::Vibrate => write!(f, "vibrate"),
            AndroidPermission::Camera => write!(f, "camera"),
            AndroidPermission::Location => write!(f, "location"),
            AndroidPermission::Storage => write!(f, "storage"),
            AndroidPermission::Bluetooth => write!(f, "bluetooth"),
            AndroidPermission::Nfc => write!(f, "nfc"),
            AndroidPermission::Custom(s) => write!(f, "{}", s),
        }
    }
}

/// iOS-specific permissions
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IosPermission {
    All,
    Camera,
    Photos,
    Location,
    Microphone,
    Contacts,
    HealthKit,
    Custom(String),
}

impl IosPermission {
    fn parse(s: &str) -> Option<Self> {
        match s {
            "*" => Some(IosPermission::All),
            "camera" => Some(IosPermission::Camera),
            "photos" => Some(IosPermission::Photos),
            "location" => Some(IosPermission::Location),
            "microphone" => Some(IosPermission::Microphone),
            "contacts" => Some(IosPermission::Contacts),
            "healthkit" => Some(IosPermission::HealthKit),
            other => Some(IosPermission::Custom(other.to_string())),
        }
    }

    fn includes(&self, other: &IosPermission) -> bool {
        match self {
            IosPermission::All => true,
            _ => self == other,
        }
    }
}

impl std::fmt::Display for IosPermission {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IosPermission::All => write!(f, "*"),
            IosPermission::Camera => write!(f, "camera"),
            IosPermission::Photos => write!(f, "photos"),
            IosPermission::Location => write!(f, "location"),
            IosPermission::Microphone => write!(f, "microphone"),
            IosPermission::Contacts => write!(f, "contacts"),
            IosPermission::HealthKit => write!(f, "healthkit"),
            IosPermission::Custom(s) => write!(f, "{}", s),
        }
    }
}

/// Desktop x64 permissions
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DesktopX64Permission {
    All,
    CFfi,
    SystemCalls,
    Gpu,
    Audio,
    Clipboard,
    Notifications,
    Custom(String),
}

impl DesktopX64Permission {
    fn parse(s: &str) -> Option<Self> {
        match s {
            "*" => Some(DesktopX64Permission::All),
            "c_ffi" => Some(DesktopX64Permission::CFfi),
            "system_calls" => Some(DesktopX64Permission::SystemCalls),
            "gpu" => Some(DesktopX64Permission::Gpu),
            "audio" => Some(DesktopX64Permission::Audio),
            "clipboard" => Some(DesktopX64Permission::Clipboard),
            "notifications" => Some(DesktopX64Permission::Notifications),
            other => Some(DesktopX64Permission::Custom(other.to_string())),
        }
    }

    fn includes(&self, other: &DesktopX64Permission) -> bool {
        match self {
            DesktopX64Permission::All => true,
            _ => self == other,
        }
    }
}

impl std::fmt::Display for DesktopX64Permission {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DesktopX64Permission::All => write!(f, "*"),
            DesktopX64Permission::CFfi => write!(f, "c_ffi"),
            DesktopX64Permission::SystemCalls => write!(f, "system_calls"),
            DesktopX64Permission::Gpu => write!(f, "gpu"),
            DesktopX64Permission::Audio => write!(f, "audio"),
            DesktopX64Permission::Clipboard => write!(f, "clipboard"),
            DesktopX64Permission::Notifications => write!(f, "notifications"),
            DesktopX64Permission::Custom(s) => write!(f, "{}", s),
        }
    }
}

/// Web/WASM permissions
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum WebPermission {
    All,
    Dom,
    Fetch,
    WebSocket,
    LocalStorage,
    IndexedDb,
    WebGl,
    WebAudio,
    Custom(String),
}

impl WebPermission {
    fn parse(s: &str) -> Option<Self> {
        match s {
            "*" => Some(WebPermission::All),
            "dom" => Some(WebPermission::Dom),
            "fetch" => Some(WebPermission::Fetch),
            "websocket" => Some(WebPermission::WebSocket),
            "localstorage" => Some(WebPermission::LocalStorage),
            "indexeddb" => Some(WebPermission::IndexedDb),
            "webgl" => Some(WebPermission::WebGl),
            "webaudio" => Some(WebPermission::WebAudio),
            other => Some(WebPermission::Custom(other.to_string())),
        }
    }

    fn includes(&self, other: &WebPermission) -> bool {
        match self {
            WebPermission::All => true,
            _ => self == other,
        }
    }
}

impl std::fmt::Display for WebPermission {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WebPermission::All => write!(f, "*"),
            WebPermission::Dom => write!(f, "dom"),
            WebPermission::Fetch => write!(f, "fetch"),
            WebPermission::WebSocket => write!(f, "websocket"),
            WebPermission::LocalStorage => write!(f, "localstorage"),
            WebPermission::IndexedDb => write!(f, "indexeddb"),
            WebPermission::WebGl => write!(f, "webgl"),
            WebPermission::WebAudio => write!(f, "webaudio"),
            WebPermission::Custom(s) => write!(f, "{}", s),
        }
    }
}

/// Permission set for a dependency or module
#[derive(Debug, Clone, Default)]
pub struct PermissionSet {
    /// Allowed permissions
    allowed: HashSet<Permission>,
    /// Explicitly denied permissions (overrides allowed)
    denied: HashSet<Permission>,
}

impl PermissionSet {
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a permission set that only allows std
    pub fn std_only() -> Self {
        let mut set = Self::new();
        set.allow(Permission::Std);
        set
    }

    /// Create a permission set that allows everything
    pub fn allow_all() -> Self {
        let mut set = Self::new();
        set.allow(Permission::Std);
        set.allow(Permission::Compat(CompatPermission::All));
        set.allow(Permission::Plat(PlatPermission::All));
        set
    }

    /// Allow a permission
    pub fn allow(&mut self, permission: Permission) {
        self.allowed.insert(permission);
    }

    /// Deny a permission
    pub fn deny(&mut self, permission: Permission) {
        self.denied.insert(permission);
    }

    /// Check if a permission is allowed
    pub fn is_allowed(&self, permission: &Permission) -> bool {
        // Check explicit denial first
        if self.denied.contains(permission) {
            return false;
        }

        // Check explicit allowance
        if self.allowed.contains(permission) {
            return true;
        }

        // Check for wildcard permissions
        match permission {
            Permission::Std => self.allowed.contains(&Permission::Std),
            Permission::Compat(specific) => self.allowed.iter().any(|p| {
                if let Permission::Compat(allowed) = p {
                    allowed.includes(specific)
                } else {
                    false
                }
            }),
            Permission::Plat(specific) => self.allowed.iter().any(|p| {
                if let Permission::Plat(allowed) = p {
                    allowed.includes(specific)
                } else {
                    false
                }
            }),
        }
    }

    /// Check if a function color is allowed
    pub fn is_color_allowed(&self, color: FunctionColor) -> bool {
        match color {
            FunctionColor::Std => true, // std is always allowed
            FunctionColor::Compat => self
                .allowed
                .iter()
                .any(|p| matches!(p, Permission::Compat(_))),
            FunctionColor::Plat => self
                .allowed
                .iter()
                .any(|p| matches!(p, Permission::Plat(_))),
        }
    }
}

/// Permission manager for tracking and enforcing permissions
#[derive(Debug, Default)]
pub struct PermissionManager {
    /// Permission sets per dependency/module
    module_permissions: HashMap<String, PermissionSet>,
    /// Pending macro approval requests
    pending_approvals: Vec<MacroApprovalRequest>,
    /// Approved macro calls
    approved_macro_calls: HashSet<(String, String)>, // (macro_name, call_site)
}

impl PermissionManager {
    pub fn new() -> Self {
        Self::default()
    }

    /// Set permissions for a module/dependency
    pub fn set_module_permissions(
        &mut self,
        module: impl Into<String>,
        permissions: PermissionSet,
    ) {
        self.module_permissions.insert(module.into(), permissions);
    }

    /// Get permissions for a module
    pub fn get_module_permissions(&self, module: &str) -> Option<&PermissionSet> {
        self.module_permissions.get(module)
    }

    /// Check if a module can use a permission
    pub fn check_permission(
        &self,
        module: &str,
        permission: &Permission,
        span: Span,
    ) -> Result<(), NexusError> {
        let permissions = self
            .module_permissions
            .get(module)
            .cloned()
            .unwrap_or_default();

        if permissions.is_allowed(permission) {
            Ok(())
        } else {
            Err(NexusError::PermissionDenied {
                permission: permission.to_string(),
                context: module.to_string(),
                span,
            })
        }
    }

    /// Check if a function color call is allowed
    pub fn check_color_call(
        &self,
        caller_module: &str,
        caller_color: FunctionColor,
        callee_color: FunctionColor,
        callee_name: &str,
        span: Span,
    ) -> Result<(), NexusError> {
        // First check if caller can call callee based on color rules
        if !caller_color.can_call(callee_color) {
            return Err(NexusError::ColorViolation {
                caller_color: caller_color.to_string(),
                callee_color: callee_color.to_string(),
                callee_name: callee_name.to_string(),
                span,
            });
        }

        // Then check if the module has permission for the callee's color
        let permissions = self
            .module_permissions
            .get(caller_module)
            .cloned()
            .unwrap_or_default();

        if !permissions.is_color_allowed(callee_color) {
            return Err(NexusError::PermissionDenied {
                permission: callee_color.to_string(),
                context: caller_module.to_string(),
                span,
            });
        }

        Ok(())
    }

    /// Request approval for a macro calling a colored function
    pub fn request_macro_approval(&mut self, request: MacroApprovalRequest) {
        self.pending_approvals.push(request);
    }

    /// Approve a macro call
    pub fn approve_macro_call(&mut self, macro_name: &str, call_site: &str) {
        self.approved_macro_calls
            .insert((macro_name.to_string(), call_site.to_string()));
    }

    /// Check if a macro call is approved
    pub fn is_macro_call_approved(&self, macro_name: &str, call_site: &str) -> bool {
        self.approved_macro_calls
            .contains(&(macro_name.to_string(), call_site.to_string()))
    }

    /// Get pending approval requests
    pub fn pending_approvals(&self) -> &[MacroApprovalRequest] {
        &self.pending_approvals
    }

    /// Clear pending approvals
    pub fn clear_pending_approvals(&mut self) {
        self.pending_approvals.clear();
    }
}

/// A request for macro approval
#[derive(Debug, Clone)]
pub struct MacroApprovalRequest {
    /// Name of the macro
    pub macro_name: String,
    /// The function being called
    pub function_name: String,
    /// The color of the function
    pub function_color: FunctionColor,
    /// Source location of the call
    pub span: Span,
    /// Description of what the macro is trying to do
    pub description: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_permission_parsing() {
        assert_eq!(Permission::parse("std"), Some(Permission::Std));
        assert_eq!(
            Permission::parse("compat.io"),
            Some(Permission::Compat(CompatPermission::Io))
        );
        assert_eq!(
            Permission::parse("plat.android.vibrate"),
            Some(Permission::Plat(PlatPermission::Android(
                AndroidPermission::Vibrate
            )))
        );
    }

    #[test]
    fn test_permission_set() {
        let mut perms = PermissionSet::new();
        perms.allow(Permission::Std);
        perms.allow(Permission::Compat(CompatPermission::Io));

        assert!(perms.is_allowed(&Permission::Std));
        assert!(perms.is_allowed(&Permission::Compat(CompatPermission::Io)));
        assert!(!perms.is_allowed(&Permission::Compat(CompatPermission::Net)));
    }

    #[test]
    fn test_wildcard_permissions() {
        let mut perms = PermissionSet::new();
        perms.allow(Permission::Compat(CompatPermission::All));

        assert!(perms.is_allowed(&Permission::Compat(CompatPermission::Io)));
        assert!(perms.is_allowed(&Permission::Compat(CompatPermission::Net)));
        assert!(!perms.is_allowed(&Permission::Plat(PlatPermission::All)));
    }

    #[test]
    fn test_color_allowed() {
        let perms = PermissionSet::std_only();
        assert!(perms.is_color_allowed(FunctionColor::Std));
        assert!(!perms.is_color_allowed(FunctionColor::Compat));
        assert!(!perms.is_color_allowed(FunctionColor::Plat));

        let perms = PermissionSet::allow_all();
        assert!(perms.is_color_allowed(FunctionColor::Std));
        assert!(perms.is_color_allowed(FunctionColor::Compat));
        assert!(perms.is_color_allowed(FunctionColor::Plat));
    }
}
