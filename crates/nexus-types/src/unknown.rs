//! Unknown sum type for Nexus.
//!
//! The `unknown` type is a sum type that can hold any of its variant types,
//! similar to Rust's enum but more flexible. It's used for optionals, results,
//! and general sum types.
//!
//! Example: `unknown<A, B, C, Error, None>`

use super::NexusType;
use std::fmt;

/// The unknown sum type - can hold any of its variant types
#[derive(Debug, Clone, PartialEq)]
pub struct UnknownType {
    /// The variant types this unknown can hold
    pub variants: Vec<NexusType>,
}

impl UnknownType {
    /// Create a new unknown type with the given variants
    pub fn new(variants: Vec<NexusType>) -> Self {
        Self { variants }
    }

    /// Create an optional type: unknown<T, None>
    pub fn optional(inner: NexusType) -> Self {
        Self {
            variants: vec![inner, NexusType::Named("None".to_string())],
        }
    }

    /// Create a result type: unknown<T, Error>
    pub fn result(ok: NexusType, err: NexusType) -> Self {
        Self {
            variants: vec![ok, err],
        }
    }

    /// Check if this unknown type contains a specific variant type
    pub fn contains_variant(&self, ty: &NexusType) -> bool {
        self.variants.iter().any(|v| v == ty)
    }

    /// Check if this unknown can be None
    pub fn can_be_none(&self) -> bool {
        self.variants
            .iter()
            .any(|v| matches!(v, NexusType::Named(name) if name == "None"))
    }

    /// Check if this unknown can be Error
    pub fn can_be_error(&self) -> bool {
        self.variants
            .iter()
            .any(|v| matches!(v, NexusType::Named(name) if name == "Error"))
    }

    /// Get the "success" variants (non-None, non-Error)
    pub fn success_variants(&self) -> Vec<&NexusType> {
        self.variants
            .iter()
            .filter(|v| !matches!(v, NexusType::Named(name) if name == "None" || name == "Error"))
            .collect()
    }

    /// Get the number of variants
    pub fn variant_count(&self) -> usize {
        self.variants.len()
    }

    /// Check if a value of another type could be one of the variants
    pub fn could_be(&self, ty: &NexusType) -> bool {
        self.variants.iter().any(|v| ty.is_assignable_to(v))
    }
}

impl fmt::Display for UnknownType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "unknown<")?;
        for (i, variant) in self.variants.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", variant)?;
        }
        write!(f, ">")
    }
}

/// A runtime value of an unknown type, storing which variant it currently holds
#[derive(Debug, Clone)]
pub struct UnknownValue {
    /// The type definition of this unknown
    pub ty: UnknownType,
    /// Index of the current variant in the type's variant list
    pub variant_index: usize,
}

impl UnknownValue {
    /// Create a new unknown value
    pub fn new(ty: UnknownType, variant_index: usize) -> Self {
        debug_assert!(variant_index < ty.variants.len());
        Self { ty, variant_index }
    }

    /// Get the current variant type
    pub fn current_variant(&self) -> &NexusType {
        &self.ty.variants[self.variant_index]
    }

    /// Check if this value is currently None
    pub fn is_none(&self) -> bool {
        matches!(self.current_variant(), NexusType::Named(name) if name == "None")
    }

    /// Check if this value is currently Error
    pub fn is_error(&self) -> bool {
        matches!(self.current_variant(), NexusType::Named(name) if name == "Error")
    }

    /// Check if this value is a "success" (non-None, non-Error)
    pub fn is_success(&self) -> bool {
        !self.is_none() && !self.is_error()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::PrimitiveType;

    #[test]
    fn test_optional_type() {
        let optional = UnknownType::optional(NexusType::Primitive(PrimitiveType::I32));

        assert_eq!(optional.variant_count(), 2);
        assert!(optional.can_be_none());
        assert!(!optional.can_be_error());
        assert!(optional.contains_variant(&NexusType::Primitive(PrimitiveType::I32)));
    }

    #[test]
    fn test_result_type() {
        let result = UnknownType::result(
            NexusType::Primitive(PrimitiveType::I32),
            NexusType::Named("Error".to_string()),
        );

        assert_eq!(result.variant_count(), 2);
        assert!(!result.can_be_none());
        assert!(result.can_be_error());
    }

    #[test]
    fn test_unknown_display() {
        let unknown = UnknownType::new(vec![
            NexusType::Primitive(PrimitiveType::I32),
            NexusType::Primitive(PrimitiveType::Bool),
            NexusType::Named("None".to_string()),
        ]);

        let display = format!("{}", unknown);
        assert_eq!(display, "unknown<i32, bool, None>");
    }

    #[test]
    fn test_unknown_value() {
        let ty = UnknownType::optional(NexusType::Primitive(PrimitiveType::I32));

        let some_value = UnknownValue::new(ty.clone(), 0);
        assert!(!some_value.is_none());
        assert!(some_value.is_success());

        let none_value = UnknownValue::new(ty, 1);
        assert!(none_value.is_none());
        assert!(!none_value.is_success());
    }
}
