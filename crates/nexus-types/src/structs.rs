//! Struct type definitions for Nexus.

use crate::{Contract, InterfaceDef, NexusType};
use nexus_core::Span;
use std::collections::HashMap;
use std::sync::Arc;

/// A struct definition in Nexus
#[derive(Debug, Clone, PartialEq)]
pub struct StructDef {
    /// Name of the struct
    pub name: String,
    /// Fields in declaration order
    pub fields: Vec<StructField>,
    /// Interfaces this struct implements
    pub implements: Vec<String>,
    /// Span where the struct is defined
    pub span: Span,
    /// Cached field lookup by name
    field_indices: HashMap<String, usize>,
}

impl StructDef {
    /// Create a new struct definition
    pub fn new(name: impl Into<String>, span: Span) -> Self {
        Self {
            name: name.into(),
            fields: Vec::new(),
            implements: Vec::new(),
            span,
            field_indices: HashMap::new(),
        }
    }

    /// Add a field to the struct
    pub fn add_field(&mut self, field: StructField) {
        let index = self.fields.len();
        self.field_indices.insert(field.name.clone(), index);
        self.fields.push(field);
    }

    /// Add an interface implementation
    pub fn add_impl(&mut self, interface_name: impl Into<String>) {
        self.implements.push(interface_name.into());
    }

    /// Get a field by name
    pub fn get_field(&self, name: &str) -> Option<&StructField> {
        self.field_indices.get(name).map(|&i| &self.fields[i])
    }

    /// Get a field index by name
    pub fn get_field_index(&self, name: &str) -> Option<usize> {
        self.field_indices.get(name).copied()
    }

    /// Check if this struct implements a given interface
    pub fn implements_interface(&self, interface: &InterfaceDef) -> bool {
        self.implements.contains(&interface.name)
    }

    /// Check if this struct declares implementing a named interface
    pub fn implements_named(&self, interface_name: &str) -> bool {
        self.implements.contains(&interface_name.to_string())
    }

    /// Calculate the size of this struct in bytes (None if contains dynamically sized fields)
    pub fn size(&self) -> Option<usize> {
        let mut total = 0;
        for field in &self.fields {
            total += field.field_type.size()?;
        }
        Some(total)
    }

    /// Get all field names
    pub fn field_names(&self) -> impl Iterator<Item = &str> {
        self.fields.iter().map(|f| f.name.as_str())
    }

    /// Get the number of fields
    pub fn field_count(&self) -> usize {
        self.fields.len()
    }
}

/// A field within a struct
#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    /// Name of the field
    pub name: String,
    /// Type of the field
    pub field_type: NexusType,
    /// Default value expression (as source string for now)
    pub default_value: Option<String>,
    /// Contracts on this field
    pub contracts: Vec<Contract>,
    /// Span where the field is defined
    pub span: Span,
}

impl StructField {
    /// Create a new struct field
    pub fn new(name: impl Into<String>, field_type: NexusType, span: Span) -> Self {
        Self {
            name: name.into(),
            field_type,
            default_value: None,
            contracts: Vec::new(),
            span,
        }
    }

    /// Set the default value
    pub fn with_default(mut self, default: impl Into<String>) -> Self {
        self.default_value = Some(default.into());
        self
    }

    /// Add a contract
    pub fn with_contract(mut self, contract: Contract) -> Self {
        self.contracts.push(contract);
        self
    }

    /// Check if this field has a default value
    pub fn has_default(&self) -> bool {
        self.default_value.is_some()
    }
}

/// A struct instance (runtime value)
#[derive(Debug, Clone)]
pub struct StructInstance {
    /// The struct definition
    pub def: Arc<StructDef>,
    /// Field values (indexed by field order)
    pub values: Vec<StructFieldValue>,
}

/// A field value in a struct instance
#[derive(Debug, Clone)]
pub struct StructFieldValue {
    /// Whether this value was explicitly set (vs default)
    pub is_set: bool,
    /// The actual value (will be replaced with proper Value type from interpreter)
    pub value: FieldValue,
}

/// Placeholder for field values (will be replaced with proper Value type)
#[derive(Debug, Clone)]
pub enum FieldValue {
    Uninitialized,
    I64(i64),
    F64(f64),
    Bool(bool),
    Rune(char),
    String(Vec<char>),
    Bytes(Vec<u8>),
    Array(Vec<FieldValue>),
    Struct(Box<StructInstance>),
    None,
}

impl StructInstance {
    /// Create a new struct instance with default values
    pub fn new(def: Arc<StructDef>) -> Self {
        let values = def
            .fields
            .iter()
            .map(|_| StructFieldValue {
                is_set: false,
                value: FieldValue::Uninitialized,
            })
            .collect();

        Self { def, values }
    }

    /// Get a field value by name
    pub fn get_field(&self, name: &str) -> Option<&StructFieldValue> {
        self.def.get_field_index(name).map(|i| &self.values[i])
    }

    /// Set a field value by name
    pub fn set_field(&mut self, name: &str, value: FieldValue) -> bool {
        if let Some(index) = self.def.get_field_index(name) {
            self.values[index] = StructFieldValue {
                is_set: true,
                value,
            };
            true
        } else {
            false
        }
    }

    /// Check if all required fields (those without defaults) are set
    pub fn is_fully_initialized(&self) -> bool {
        for (i, field) in self.def.fields.iter().enumerate() {
            if !field.has_default() && !self.values[i].is_set {
                return false;
            }
        }
        true
    }

    /// Get names of uninitialized required fields
    pub fn uninitialized_fields(&self) -> Vec<&str> {
        self.def
            .fields
            .iter()
            .enumerate()
            .filter_map(|(i, field)| {
                if !field.has_default() && !self.values[i].is_set {
                    Some(field.name.as_str())
                } else {
                    None
                }
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::PrimitiveType;

    #[test]
    fn test_struct_definition() {
        let mut def = StructDef::new("Player", Span::dummy());
        def.add_field(
            StructField::new(
                "health",
                NexusType::Primitive(PrimitiveType::I32),
                Span::dummy(),
            )
            .with_default("100"),
        );
        def.add_field(StructField::new(
            "name",
            NexusType::Array(crate::ArrayType {
                element_type: Box::new(NexusType::Primitive(PrimitiveType::Rune)),
                size: crate::ArraySize::Dynamic,
                prealloc: None,
            }),
            Span::dummy(),
        ));

        assert_eq!(def.field_count(), 2);
        assert!(def.get_field("health").unwrap().has_default());
        assert!(!def.get_field("name").unwrap().has_default());
    }

    #[test]
    fn test_struct_instance() {
        let mut def = StructDef::new("Point", Span::dummy());
        def.add_field(StructField::new(
            "x",
            NexusType::Primitive(PrimitiveType::I32),
            Span::dummy(),
        ));
        def.add_field(
            StructField::new("y", NexusType::Primitive(PrimitiveType::I32), Span::dummy())
                .with_default("0"),
        );

        let arc_def = Arc::new(def);
        let mut instance = StructInstance::new(arc_def);

        assert!(!instance.is_fully_initialized());
        assert_eq!(instance.uninitialized_fields(), vec!["x"]);

        instance.set_field("x", FieldValue::I64(10));
        assert!(instance.is_fully_initialized());
    }
}
