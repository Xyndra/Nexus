//! Primitive types for the Nexus programming language.

/// Primitive types in Nexus
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    /// Void type (no value)
    Void,
    /// Boolean type
    Bool,
    /// Signed 8-bit integer
    I8,
    /// Signed 16-bit integer
    I16,
    /// Signed 32-bit integer
    I32,
    /// Signed 64-bit integer
    I64,
    /// Unsigned 8-bit integer
    U8,
    /// Unsigned 16-bit integer
    U16,
    /// Unsigned 32-bit integer
    U32,
    /// Unsigned 64-bit integer
    U64,
    /// 32-bit floating point
    F32,
    /// 64-bit floating point
    F64,
    /// Unicode code point (32-bit)
    Rune,
}

impl PrimitiveType {
    /// Get the size in bytes of this primitive type
    pub fn size(&self) -> usize {
        match self {
            PrimitiveType::Void => 0,
            PrimitiveType::Bool => 1,
            PrimitiveType::I8 | PrimitiveType::U8 => 1,
            PrimitiveType::I16 | PrimitiveType::U16 => 2,
            PrimitiveType::I32 | PrimitiveType::U32 | PrimitiveType::F32 => 4,
            PrimitiveType::I64 | PrimitiveType::U64 | PrimitiveType::F64 => 8,
            PrimitiveType::Rune => 4, // Unicode code point (u32)
        }
    }

    /// Check if this is a signed integer type
    pub fn is_signed_int(&self) -> bool {
        matches!(
            self,
            PrimitiveType::I8 | PrimitiveType::I16 | PrimitiveType::I32 | PrimitiveType::I64
        )
    }

    /// Check if this is an unsigned integer type
    pub fn is_unsigned_int(&self) -> bool {
        matches!(
            self,
            PrimitiveType::U8 | PrimitiveType::U16 | PrimitiveType::U32 | PrimitiveType::U64
        )
    }

    /// Check if this is any integer type
    pub fn is_integer(&self) -> bool {
        self.is_signed_int() || self.is_unsigned_int()
    }

    /// Check if this is a floating point type
    pub fn is_float(&self) -> bool {
        matches!(self, PrimitiveType::F32 | PrimitiveType::F64)
    }

    /// Check if this is a numeric type (integer or float)
    pub fn is_numeric(&self) -> bool {
        self.is_integer() || self.is_float()
    }

    /// Get the primitive type from its name
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "void" => Some(PrimitiveType::Void),
            "bool" => Some(PrimitiveType::Bool),
            "i8" => Some(PrimitiveType::I8),
            "i16" => Some(PrimitiveType::I16),
            "i32" | "int" => Some(PrimitiveType::I32),
            "i64" => Some(PrimitiveType::I64),
            "u8" => Some(PrimitiveType::U8),
            "u16" => Some(PrimitiveType::U16),
            "u32" => Some(PrimitiveType::U32),
            "u64" => Some(PrimitiveType::U64),
            "f32" => Some(PrimitiveType::F32),
            "f64" | "float" => Some(PrimitiveType::F64),
            "rune" => Some(PrimitiveType::Rune),
            _ => None,
        }
    }

    /// Get the default value for this primitive type as a string representation
    pub fn default_value_repr(&self) -> &'static str {
        match self {
            PrimitiveType::Void => "()",
            PrimitiveType::Bool => "false",
            PrimitiveType::I8
            | PrimitiveType::I16
            | PrimitiveType::I32
            | PrimitiveType::I64
            | PrimitiveType::U8
            | PrimitiveType::U16
            | PrimitiveType::U32
            | PrimitiveType::U64 => "0",
            PrimitiveType::F32 | PrimitiveType::F64 => "0.0",
            PrimitiveType::Rune => "'\\0'",
        }
    }

    /// Check if this type can be implicitly converted to another type
    pub fn can_coerce_to(&self, target: &PrimitiveType) -> bool {
        if self == target {
            return true;
        }

        match (self, target) {
            // Smaller integers can coerce to larger integers of the same signedness
            (PrimitiveType::I8, PrimitiveType::I16 | PrimitiveType::I32 | PrimitiveType::I64) => {
                true
            }
            (PrimitiveType::I16, PrimitiveType::I32 | PrimitiveType::I64) => true,
            (PrimitiveType::I32, PrimitiveType::I64) => true,

            (PrimitiveType::U8, PrimitiveType::U16 | PrimitiveType::U32 | PrimitiveType::U64) => {
                true
            }
            (PrimitiveType::U16, PrimitiveType::U32 | PrimitiveType::U64) => true,
            (PrimitiveType::U32, PrimitiveType::U64) => true,

            // Smaller floats can coerce to larger floats
            (PrimitiveType::F32, PrimitiveType::F64) => true,

            // Integers can coerce to floats (with potential precision loss)
            (int, PrimitiveType::F64) if int.is_integer() => true,
            (int, PrimitiveType::F32) if int.is_integer() && int.size() <= 2 => true,

            // Rune can coerce to u32
            (PrimitiveType::Rune, PrimitiveType::U32) => true,
            (PrimitiveType::U32, PrimitiveType::Rune) => true,

            _ => false,
        }
    }

    /// Get the minimum value for this type (for signed integers)
    pub fn min_value(&self) -> Option<i128> {
        match self {
            PrimitiveType::I8 => Some(i8::MIN as i128),
            PrimitiveType::I16 => Some(i16::MIN as i128),
            PrimitiveType::I32 => Some(i32::MIN as i128),
            PrimitiveType::I64 => Some(i64::MIN as i128),
            PrimitiveType::U8 | PrimitiveType::U16 | PrimitiveType::U32 | PrimitiveType::U64 => {
                Some(0)
            }
            _ => None,
        }
    }

    /// Get the maximum value for this type
    pub fn max_value(&self) -> Option<i128> {
        match self {
            PrimitiveType::I8 => Some(i8::MAX as i128),
            PrimitiveType::I16 => Some(i16::MAX as i128),
            PrimitiveType::I32 => Some(i32::MAX as i128),
            PrimitiveType::I64 => Some(i64::MAX as i128),
            PrimitiveType::U8 => Some(u8::MAX as i128),
            PrimitiveType::U16 => Some(u16::MAX as i128),
            PrimitiveType::U32 => Some(u32::MAX as i128),
            PrimitiveType::U64 => Some(u64::MAX as i128),
            _ => None,
        }
    }
}

impl std::fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimitiveType::Void => write!(f, "void"),
            PrimitiveType::Bool => write!(f, "bool"),
            PrimitiveType::I8 => write!(f, "i8"),
            PrimitiveType::I16 => write!(f, "i16"),
            PrimitiveType::I32 => write!(f, "i32"),
            PrimitiveType::I64 => write!(f, "i64"),
            PrimitiveType::U8 => write!(f, "u8"),
            PrimitiveType::U16 => write!(f, "u16"),
            PrimitiveType::U32 => write!(f, "u32"),
            PrimitiveType::U64 => write!(f, "u64"),
            PrimitiveType::F32 => write!(f, "f32"),
            PrimitiveType::F64 => write!(f, "f64"),
            PrimitiveType::Rune => write!(f, "rune"),
        }
    }
}

/// Array size specification
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ArraySize {
    /// Fixed size array: [N]T
    Fixed(usize),
    /// Dynamic array: [dyn]T
    Dynamic,
}

impl std::fmt::Display for ArraySize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArraySize::Fixed(n) => write!(f, "{}", n),
            ArraySize::Dynamic => write!(f, "dyn"),
        }
    }
}

/// Array type representation
#[derive(Debug, Clone, PartialEq)]
pub struct ArrayType {
    /// The element type
    pub element_type: Box<super::NexusType>,
    /// The size (fixed or dynamic)
    pub size: ArraySize,
    /// Preallocation hint (2^n slots)
    pub prealloc: Option<u8>,
}

impl ArrayType {
    /// Create a new fixed-size array type
    pub fn fixed(element_type: super::NexusType, size: usize) -> Self {
        Self {
            element_type: Box::new(element_type),
            size: ArraySize::Fixed(size),
            prealloc: None,
        }
    }

    /// Create a new dynamic array type
    pub fn dynamic(element_type: super::NexusType) -> Self {
        Self {
            element_type: Box::new(element_type),
            size: ArraySize::Dynamic,
            prealloc: None,
        }
    }

    /// Create a new dynamic array type with preallocation
    pub fn dynamic_prealloc(element_type: super::NexusType, prealloc_power: u8) -> Self {
        Self {
            element_type: Box::new(element_type),
            size: ArraySize::Dynamic,
            prealloc: Some(prealloc_power),
        }
    }

    /// Get the preallocation size (2^n)
    pub fn prealloc_size(&self) -> Option<usize> {
        self.prealloc.map(|n| 1 << n)
    }

    /// Check if this is a fixed-size array
    pub fn is_fixed(&self) -> bool {
        matches!(self.size, ArraySize::Fixed(_))
    }

    /// Check if this is a dynamic array
    pub fn is_dynamic(&self) -> bool {
        matches!(self.size, ArraySize::Dynamic)
    }

    /// Get the fixed size, if applicable
    pub fn fixed_size(&self) -> Option<usize> {
        match self.size {
            ArraySize::Fixed(n) => Some(n),
            ArraySize::Dynamic => None,
        }
    }
}

impl std::fmt::Display for ArrayType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]{}", self.size, self.element_type)?;
        if let Some(n) = self.prealloc {
            write!(f, " @prealloc({})", n)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitive_sizes() {
        assert_eq!(PrimitiveType::Void.size(), 0);
        assert_eq!(PrimitiveType::Bool.size(), 1);
        assert_eq!(PrimitiveType::I32.size(), 4);
        assert_eq!(PrimitiveType::I64.size(), 8);
        assert_eq!(PrimitiveType::Rune.size(), 4);
    }

    #[test]
    fn test_from_name() {
        assert_eq!(PrimitiveType::from_name("i32"), Some(PrimitiveType::I32));
        assert_eq!(PrimitiveType::from_name("int"), Some(PrimitiveType::I32));
        assert_eq!(PrimitiveType::from_name("rune"), Some(PrimitiveType::Rune));
        assert_eq!(PrimitiveType::from_name("unknown"), None);
    }

    #[test]
    fn test_coercion() {
        assert!(PrimitiveType::I8.can_coerce_to(&PrimitiveType::I16));
        assert!(PrimitiveType::I8.can_coerce_to(&PrimitiveType::I64));
        assert!(!PrimitiveType::I64.can_coerce_to(&PrimitiveType::I8));
        assert!(PrimitiveType::F32.can_coerce_to(&PrimitiveType::F64));
        assert!(PrimitiveType::Rune.can_coerce_to(&PrimitiveType::U32));
    }

    #[test]
    fn test_array_prealloc() {
        let arr =
            ArrayType::dynamic_prealloc(super::super::NexusType::Primitive(PrimitiveType::I32), 8);
        assert_eq!(arr.prealloc_size(), Some(256)); // 2^8 = 256
    }
}
