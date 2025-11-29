//! Type system for the Nexus programming language.
//!
//! This crate defines all types used in Nexus, including primitives,
//! structs, interfaces, and the unknown sum type.

mod interface;
mod primitives;
mod structs;
mod unknown;

pub use interface::*;
pub use primitives::*;
pub use structs::*;
pub use unknown::*;

use nexus_core::Span;
use std::collections::HashMap;
use std::sync::Arc;

/// Unique identifier for types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub u64);

impl TypeId {
    pub fn new(id: u64) -> Self {
        Self(id)
    }
}

/// The main type representation in Nexus
#[derive(Debug, Clone, PartialEq)]
pub enum NexusType {
    /// Primitive types (i8, i16, i32, i64, u8, u16, u32, u64, f32, f64, bool, rune, void)
    Primitive(PrimitiveType),

    /// Array type: [N]T for fixed, [dyn]T for dynamic
    Array(ArrayType),

    /// Struct type
    Struct(Arc<StructDef>),

    /// Interface type
    Interface(Arc<InterfaceDef>),

    /// Unknown sum type: unknown<A, B, C, Error, None>
    Unknown(UnknownType),

    /// Function type
    Function(FunctionType),

    /// Macro type (returns string that gets parsed)
    Macro,

    /// Type reference by name (unresolved)
    Named(String),

    /// Type parameter (for generics in the future)
    TypeParam(String),

    /// Error type for error recovery
    Error,
}

impl NexusType {
    /// Check if this type is a primitive type
    pub fn is_primitive(&self) -> bool {
        matches!(self, NexusType::Primitive(_))
    }

    /// Check if this type is an interface
    pub fn is_interface(&self) -> bool {
        matches!(self, NexusType::Interface(_))
    }

    /// Check if this type is a struct
    pub fn is_struct(&self) -> bool {
        matches!(self, NexusType::Struct(_))
    }

    /// Check if this type is void
    pub fn is_void(&self) -> bool {
        matches!(self, NexusType::Primitive(PrimitiveType::Void))
    }

    /// Check if this type is an array
    pub fn is_array(&self) -> bool {
        matches!(self, NexusType::Array(_))
    }

    /// Check if this type is a string ([]rune)
    pub fn is_string(&self) -> bool {
        match self {
            NexusType::Array(arr) => {
                matches!(
                    &*arr.element_type,
                    NexusType::Primitive(PrimitiveType::Rune)
                )
            }
            _ => false,
        }
    }

    /// Check if this type is a byte array ([]u8)
    pub fn is_bytes(&self) -> bool {
        match self {
            NexusType::Array(arr) => {
                matches!(&*arr.element_type, NexusType::Primitive(PrimitiveType::U8))
            }
            _ => false,
        }
    }

    /// Get the size in bytes for this type (None for dynamically sized types)
    pub fn size(&self) -> Option<usize> {
        match self {
            NexusType::Primitive(p) => Some(p.size()),
            NexusType::Array(arr) => {
                if let ArraySize::Fixed(n) = arr.size {
                    arr.element_type.size().map(|s| s * n)
                } else {
                    None // Dynamic arrays don't have fixed size
                }
            }
            NexusType::Struct(s) => s.size(),
            NexusType::Interface(_) => None, // Interfaces are pointers
            NexusType::Unknown(_) => None,   // Unknown size depends on variant
            NexusType::Function(_) => Some(std::mem::size_of::<usize>()), // Function pointer
            NexusType::Macro => None,
            NexusType::Named(_) => None, // Unresolved
            NexusType::TypeParam(_) => None,
            NexusType::Error => None,
        }
    }

    /// Check if a value of this type can be assigned to a variable of another type
    pub fn is_assignable_to(&self, target: &NexusType) -> bool {
        match (self, target) {
            // Same types are always assignable
            (a, b) if a == b => true,

            // Primitives must match exactly
            (NexusType::Primitive(a), NexusType::Primitive(b)) => a == b,

            // Struct can be assigned to interface if it implements it
            (NexusType::Struct(s), NexusType::Interface(i)) => s.implements_interface(i),

            // Unknown can be assigned if the source is one of the variants
            (src, NexusType::Unknown(unknown)) => {
                unknown.variants.iter().any(|v| src.is_assignable_to(v))
            }

            // Error type is assignable to anything (for error recovery)
            (NexusType::Error, _) | (_, NexusType::Error) => true,

            _ => false,
        }
    }
}

impl std::fmt::Display for NexusType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NexusType::Primitive(p) => write!(f, "{}", p),
            NexusType::Array(arr) => write!(f, "{}", arr),
            NexusType::Struct(s) => write!(f, "{}", s.name),
            NexusType::Interface(i) => write!(f, "{}", i.name),
            NexusType::Unknown(u) => write!(f, "{}", u),
            NexusType::Function(func) => write!(f, "{}", func),
            NexusType::Macro => write!(f, "macro"),
            NexusType::Named(name) => write!(f, "{}", name),
            NexusType::TypeParam(name) => write!(f, "{}", name),
            NexusType::Error => write!(f, "<error>"),
        }
    }
}

/// Type registry for managing all types in a program
#[derive(Debug, Default)]
pub struct TypeRegistry {
    /// All struct definitions by name
    structs: HashMap<String, Arc<StructDef>>,
    /// All interface definitions by name
    interfaces: HashMap<String, Arc<InterfaceDef>>,
    /// Type ID counter
    next_id: u64,
}

impl TypeRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a struct definition
    pub fn register_struct(&mut self, def: StructDef) -> Arc<StructDef> {
        let arc = Arc::new(def);
        self.structs.insert(arc.name.clone(), arc.clone());
        arc
    }

    /// Register an interface definition
    pub fn register_interface(&mut self, def: InterfaceDef) -> Arc<InterfaceDef> {
        let arc = Arc::new(def);
        self.interfaces.insert(arc.name.clone(), arc.clone());
        arc
    }

    /// Look up a struct by name
    pub fn get_struct(&self, name: &str) -> Option<Arc<StructDef>> {
        self.structs.get(name).cloned()
    }

    /// Look up an interface by name
    pub fn get_interface(&self, name: &str) -> Option<Arc<InterfaceDef>> {
        self.interfaces.get(name).cloned()
    }

    /// Resolve a named type to its actual type
    pub fn resolve_type(&self, ty: &NexusType) -> NexusType {
        match ty {
            NexusType::Named(name) => {
                if let Some(s) = self.get_struct(name) {
                    NexusType::Struct(s)
                } else if let Some(i) = self.get_interface(name) {
                    NexusType::Interface(i)
                } else if let Some(p) = PrimitiveType::from_name(name) {
                    NexusType::Primitive(p)
                } else {
                    ty.clone()
                }
            }
            NexusType::Array(arr) => {
                let resolved_element = self.resolve_type(&arr.element_type);
                NexusType::Array(ArrayType {
                    element_type: Box::new(resolved_element),
                    size: arr.size,
                    prealloc: arr.prealloc,
                })
            }
            _ => ty.clone(),
        }
    }

    /// Generate a new unique type ID
    pub fn next_type_id(&mut self) -> TypeId {
        let id = self.next_id;
        self.next_id += 1;
        TypeId(id)
    }
}

/// Function type representation
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    /// Parameter types
    pub params: Vec<NexusType>,
    /// Return type
    pub return_type: Box<NexusType>,
    /// Function color (std, compat, plat)
    pub color: nexus_core::FunctionColor,
}

impl std::fmt::Display for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for (i, param) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", param)?;
        }
        write!(f, "): {}", self.return_type)
    }
}

/// Contract for function arguments or return values
#[derive(Debug, Clone, PartialEq)]
pub struct Contract {
    /// The span where the contract is defined
    pub span: Span,
    /// Contract kind
    pub kind: ContractKind,
}

/// Kinds of contracts
#[derive(Debug, Clone, PartialEq)]
pub enum ContractKind {
    /// Value must be in range [min, max]
    Range { min: i64, max: i64 },
    /// Value must not be null/None
    NotNull,
    /// Value must satisfy a predicate function
    Predicate(String),
    /// Custom contract expression
    Custom(String),
}

impl Contract {
    pub fn range(min: i64, max: i64, span: Span) -> Self {
        Self {
            span,
            kind: ContractKind::Range { min, max },
        }
    }

    pub fn not_null(span: Span) -> Self {
        Self {
            span,
            kind: ContractKind::NotNull,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitive_types() {
        let i32_type = NexusType::Primitive(PrimitiveType::I32);
        assert!(i32_type.is_primitive());
        assert!(!i32_type.is_void());
        assert_eq!(i32_type.size(), Some(4));
    }

    #[test]
    fn test_string_type() {
        let string_type = NexusType::Array(ArrayType {
            element_type: Box::new(NexusType::Primitive(PrimitiveType::Rune)),
            size: ArraySize::Dynamic,
            prealloc: None,
        });
        assert!(string_type.is_string());
        assert!(!string_type.is_bytes());
    }

    #[test]
    fn test_bytes_type() {
        let bytes_type = NexusType::Array(ArrayType {
            element_type: Box::new(NexusType::Primitive(PrimitiveType::U8)),
            size: ArraySize::Dynamic,
            prealloc: None,
        });
        assert!(bytes_type.is_bytes());
        assert!(!bytes_type.is_string());
    }
}
