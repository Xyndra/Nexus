//! Interface type definitions for Nexus.
//!
//! Interfaces define a set of methods that a struct must implement.
//! Only interfaces (and primitive types) can be used as function argument types.

use crate::{FunctionType, NexusType};
use nexus_core::Span;
use std::collections::HashMap;

/// Definition of an interface
#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceDef {
    /// Name of the interface
    pub name: String,
    /// Source location where the interface is defined
    pub span: Span,
    /// Methods required by this interface
    pub methods: HashMap<String, InterfaceMethod>,
    /// Parent interfaces (for interface inheritance)
    pub extends: Vec<String>,
}

impl InterfaceDef {
    /// Create a new interface definition
    pub fn new(name: impl Into<String>, span: Span) -> Self {
        Self {
            name: name.into(),
            span,
            methods: HashMap::new(),
            extends: Vec::new(),
        }
    }

    /// Add a method to the interface
    pub fn add_method(&mut self, method: InterfaceMethod) {
        self.methods.insert(method.name.clone(), method);
    }

    /// Check if this interface has a method with the given name
    pub fn has_method(&self, name: &str) -> bool {
        self.methods.contains_key(name)
    }

    /// Get a method by name
    pub fn get_method(&self, name: &str) -> Option<&InterfaceMethod> {
        self.methods.get(name)
    }

    /// Add a parent interface
    pub fn extend(&mut self, parent: impl Into<String>) {
        self.extends.push(parent.into());
    }

    /// Get all method names
    pub fn method_names(&self) -> impl Iterator<Item = &String> {
        self.methods.keys()
    }
}

/// A method signature in an interface
#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceMethod {
    /// Name of the method
    pub name: String,
    /// Source location
    pub span: Span,
    /// Whether the receiver is mutable (m Self)
    pub receiver_mutable: bool,
    /// Parameter types (not including receiver)
    pub params: Vec<MethodParam>,
    /// Return type
    pub return_type: NexusType,
}

impl InterfaceMethod {
    /// Create a new interface method
    pub fn new(name: impl Into<String>, return_type: NexusType, span: Span) -> Self {
        Self {
            name: name.into(),
            span,
            receiver_mutable: false,
            params: Vec::new(),
            return_type,
        }
    }

    /// Set the receiver as mutable
    pub fn with_mutable_receiver(mut self) -> Self {
        self.receiver_mutable = true;
        self
    }

    /// Add a parameter
    pub fn with_param(mut self, param: MethodParam) -> Self {
        self.params.push(param);
        self
    }

    /// Convert to a function type (for type checking)
    /// Note: The color is determined by the implementing method, not the interface
    pub fn to_function_type(&self, receiver_type: NexusType) -> FunctionType {
        let mut params = vec![receiver_type];
        params.extend(self.params.iter().map(|p| p.ty.clone()));

        FunctionType {
            params,
            return_type: Box::new(self.return_type.clone()),
            // Interfaces don't restrict color; use Std as default for type checking
            color: nexus_core::FunctionColor::Std,
        }
    }
}

/// A parameter in a method signature
#[derive(Debug, Clone, PartialEq)]
pub struct MethodParam {
    /// Parameter name
    pub name: String,
    /// Parameter type (must be an interface or primitive)
    pub ty: NexusType,
    /// Source location
    pub span: Span,
    /// Contracts on this parameter
    pub contracts: Vec<crate::Contract>,
}

impl MethodParam {
    /// Create a new method parameter
    pub fn new(name: impl Into<String>, ty: NexusType, span: Span) -> Self {
        Self {
            name: name.into(),
            ty,
            span,
            contracts: Vec::new(),
        }
    }

    /// Add a contract to this parameter
    pub fn with_contract(mut self, contract: crate::Contract) -> Self {
        self.contracts.push(contract);
        self
    }
}

/// Trait for types that can implement interfaces
pub trait Implementor {
    /// Check if this type implements the given interface
    fn implements(&self, interface: &InterfaceDef) -> bool;

    /// Get the list of interfaces this type implements
    fn interfaces(&self) -> &[String];
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::PrimitiveType;

    #[test]
    fn test_interface_creation() {
        let mut interface = InterfaceDef::new("Damageable", Span::dummy());

        let method = InterfaceMethod::new(
            "take_damage",
            NexusType::Primitive(PrimitiveType::Void),
            Span::dummy(),
        )
        .with_mutable_receiver()
        .with_param(MethodParam::new(
            "amount",
            NexusType::Primitive(PrimitiveType::I32),
            Span::dummy(),
        ));

        interface.add_method(method);

        assert!(interface.has_method("take_damage"));
        assert!(!interface.has_method("heal"));
    }

    #[test]
    fn test_interface_method_to_function_type() {
        let method = InterfaceMethod::new(
            "get_health",
            NexusType::Primitive(PrimitiveType::I32),
            Span::dummy(),
        );

        let receiver = NexusType::Named("Entity".to_string());
        let func_type = method.to_function_type(receiver);

        assert_eq!(func_type.params.len(), 1);
        assert_eq!(
            *func_type.return_type,
            NexusType::Primitive(PrimitiveType::I32)
        );
    }
}
