//! Scope management for the Nexus interpreter.
//!
//! This module provides the scope system for variable storage and lookup,
//! supporting nested scopes, global variables, and the modifier system.
//!
//! The scope uses Rc<RefCell<...>> for parent references so that modifications
//! to parent variables are reflected in the actual parent scope.

use crate::Value;
use nexus_core::{NexusError, NexusResult, Span, VarModifiers};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::{Arc, RwLock};

/// A variable stored in a scope
#[derive(Debug, Clone)]
pub struct Variable {
    /// Variable name
    pub name: String,
    /// Current value
    pub value: Value,
    /// Variable modifiers (m, l, h, u, g, c)
    pub modifiers: VarModifiers,
    /// Source location where defined
    pub span: Span,
}

impl Variable {
    /// Check if this variable is mutable
    pub fn is_mutable(&self) -> bool {
        self.modifiers.mutable
    }

    /// Check if this variable is locked (thread-safe)
    pub fn is_locked(&self) -> bool {
        self.modifiers.locked
    }

    /// Check if this variable is heap-allocated
    pub fn is_heap(&self) -> bool {
        self.modifiers.heap
    }

    /// Check if this variable is undetermined type
    pub fn is_undetermined(&self) -> bool {
        self.modifiers.undetermined
    }

    /// Check if this variable is global
    pub fn is_global(&self) -> bool {
        self.modifiers.global
    }

    /// Check if this variable is const (read-only)
    pub fn is_const(&self) -> bool {
        self.modifiers.const_
    }
}

/// Inner data for a scope, wrapped in Rc<RefCell<...>>
#[derive(Debug)]
struct ScopeInner {
    /// Variables in this scope
    variables: FxHashMap<String, Variable>,
    /// Parent scope (for nested scopes) - uses Rc for shared mutable access
    parent: Option<Rc<RefCell<ScopeInner>>>,
    /// Whether this is the global scope
    is_global: bool,
    /// Optional label for this scope (for subscopes/loops)
    label: Option<String>,
}

/// A scope for variable storage
///
/// Uses Rc<RefCell<...>> internally so that child scopes can mutate
/// parent scope variables and have those changes reflected in the parent.
#[derive(Debug, Clone)]
pub struct Scope {
    inner: Rc<RefCell<ScopeInner>>,
}

impl Scope {
    /// Create a new empty scope
    pub fn new() -> Self {
        Self {
            inner: Rc::new(RefCell::new(ScopeInner {
                variables: FxHashMap::default(),
                parent: None,
                is_global: false,
                label: None,
            })),
        }
    }

    /// Create a new global scope
    pub fn new_global() -> Self {
        Self {
            inner: Rc::new(RefCell::new(ScopeInner {
                variables: FxHashMap::default(),
                parent: None,
                is_global: true,
                label: None,
            })),
        }
    }

    /// Create a child scope with this scope as parent
    ///
    /// The parent is shared via Rc, so modifications to parent variables
    /// through the child will be reflected in the parent.
    pub fn child(&self) -> Self {
        Self {
            inner: Rc::new(RefCell::new(ScopeInner {
                variables: FxHashMap::default(),
                parent: Some(Rc::clone(&self.inner)),
                is_global: false,
                label: None,
            })),
        }
    }

    /// Set a label for this scope
    pub fn set_label(&mut self, label: String) {
        self.inner.borrow_mut().label = Some(label);
    }

    /// Get the label for this scope
    pub fn label(&self) -> Option<String> {
        self.inner.borrow().label.clone()
    }

    /// Define a new variable in this scope
    ///
    /// If a variable with the same name already exists in this scope,
    /// it will be redefined (shadowed within the same scope).
    pub fn define(&mut self, var: Variable) -> NexusResult<()> {
        // Allow redefinition in the same scope (variable shadowing)
        self.inner
            .borrow_mut()
            .variables
            .insert(var.name.clone(), var);
        Ok(())
    }

    /// Define a new variable, but error if it already exists in this scope
    pub fn define_unique(&mut self, var: Variable) -> NexusResult<()> {
        let mut inner = self.inner.borrow_mut();
        if inner.variables.contains_key(&var.name) {
            return Err(NexusError::VariableAlreadyDefined {
                name: var.name.clone(),
                span: var.span,
            });
        }
        inner.variables.insert(var.name.clone(), var);
        Ok(())
    }

    /// Look up a variable by name (searches parent scopes)
    pub fn get(&self, name: &str) -> Option<Variable> {
        let inner = self.inner.borrow();
        if let Some(var) = inner.variables.get(name) {
            Some(var.clone())
        } else if let Some(parent) = &inner.parent {
            // Temporarily drop the borrow before recursing
            let parent_rc = Rc::clone(parent);
            drop(inner);
            Self::get_from_inner(&parent_rc, name)
        } else {
            None
        }
    }

    /// Look up only the value of a variable by name (more efficient than get())
    /// Use this when only need the value is needed, not the full Variable metadata
    pub fn get_value(&self, name: &str) -> Option<Value> {
        let inner = self.inner.borrow();
        if let Some(var) = inner.variables.get(name) {
            Some(var.value.clone())
        } else if let Some(parent) = &inner.parent {
            let parent_rc = Rc::clone(parent);
            drop(inner);
            Self::get_value_from_inner(&parent_rc, name)
        } else {
            None
        }
    }

    /// Helper to get a variable from a ScopeInner Rc
    fn get_from_inner(inner_rc: &Rc<RefCell<ScopeInner>>, name: &str) -> Option<Variable> {
        let inner = inner_rc.borrow();
        if let Some(var) = inner.variables.get(name) {
            Some(var.clone())
        } else if let Some(parent) = &inner.parent {
            let parent_rc = Rc::clone(parent);
            drop(inner);
            Self::get_from_inner(&parent_rc, name)
        } else {
            None
        }
    }

    /// Helper to get only the value from a ScopeInner Rc
    fn get_value_from_inner(inner_rc: &Rc<RefCell<ScopeInner>>, name: &str) -> Option<Value> {
        let inner = inner_rc.borrow();
        if let Some(var) = inner.variables.get(name) {
            Some(var.value.clone())
        } else if let Some(parent) = &inner.parent {
            let parent_rc = Rc::clone(parent);
            drop(inner);
            Self::get_value_from_inner(&parent_rc, name)
        } else {
            None
        }
    }

    /// Assign a value to an existing variable (searches parent scopes)
    pub fn assign(&mut self, name: &str, value: Value, span: Span) -> NexusResult<()> {
        Self::assign_in_inner(&self.inner, name, value, span)
    }

    /// Helper to assign in a ScopeInner Rc chain
    fn assign_in_inner(
        inner_rc: &Rc<RefCell<ScopeInner>>,
        name: &str,
        value: Value,
        span: Span,
    ) -> NexusResult<()> {
        let mut inner = inner_rc.borrow_mut();
        if let Some(var) = inner.variables.get_mut(name) {
            if !var.is_mutable() {
                return Err(NexusError::ImmutableVariable {
                    name: name.to_string(),
                    span,
                });
            }
            var.value = value;
            Ok(())
        } else if let Some(parent) = &inner.parent {
            let parent_rc = Rc::clone(parent);
            drop(inner);
            Self::assign_in_inner(&parent_rc, name, value, span)
        } else {
            Err(NexusError::UndefinedVariable {
                name: name.to_string(),
                span,
            })
        }
    }

    /// Check if a variable exists in this scope (not parents)
    pub fn has_local(&self, name: &str) -> bool {
        self.inner.borrow().variables.contains_key(name)
    }

    /// Check if a variable exists in this scope or any parent
    pub fn has(&self, name: &str) -> bool {
        let inner = self.inner.borrow();
        if inner.variables.contains_key(name) {
            true
        } else if let Some(parent) = &inner.parent {
            let parent_rc = Rc::clone(parent);
            drop(inner);
            Self::has_in_inner(&parent_rc, name)
        } else {
            false
        }
    }

    /// Helper to check if a variable exists in a ScopeInner Rc chain
    fn has_in_inner(inner_rc: &Rc<RefCell<ScopeInner>>, name: &str) -> bool {
        let inner = inner_rc.borrow();
        if inner.variables.contains_key(name) {
            true
        } else if let Some(parent) = &inner.parent {
            let parent_rc = Rc::clone(parent);
            drop(inner);
            Self::has_in_inner(&parent_rc, name)
        } else {
            false
        }
    }

    /// Get all variable names in this scope (not parents)
    pub fn local_names(&self) -> Vec<String> {
        self.inner.borrow().variables.keys().cloned().collect()
    }

    /// Get all variables in this scope (not parents)
    pub fn local_variables(&self) -> Vec<Variable> {
        self.inner.borrow().variables.values().cloned().collect()
    }

    /// Remove a variable from this scope
    pub fn remove(&mut self, name: &str) -> Option<Variable> {
        self.inner.borrow_mut().variables.remove(name)
    }

    /// Clear all variables from this scope
    pub fn clear(&mut self) {
        self.inner.borrow_mut().variables.clear();
    }

    /// Get the depth of this scope (0 for top-level)
    pub fn depth(&self) -> usize {
        let inner = self.inner.borrow();
        match &inner.parent {
            Some(p) => {
                let parent_rc = Rc::clone(p);
                drop(inner);
                1 + Self::depth_of_inner(&parent_rc)
            }
            None => 0,
        }
    }

    /// Helper to get depth from a ScopeInner Rc
    fn depth_of_inner(inner_rc: &Rc<RefCell<ScopeInner>>) -> usize {
        let inner = inner_rc.borrow();
        match &inner.parent {
            Some(p) => {
                let parent_rc = Rc::clone(p);
                drop(inner);
                1 + Self::depth_of_inner(&parent_rc)
            }
            None => 0,
        }
    }

    /// Check if this is the global scope
    pub fn is_global(&self) -> bool {
        self.inner.borrow().is_global
    }

    /// Find a scope with a specific label (returns true if found in chain)
    pub fn has_labeled(&self, label: &str) -> bool {
        let inner = self.inner.borrow();
        if inner.label.as_deref() == Some(label) {
            true
        } else if let Some(parent) = &inner.parent {
            let parent_rc = Rc::clone(parent);
            drop(inner);
            Self::has_labeled_in_inner(&parent_rc, label)
        } else {
            false
        }
    }

    /// Helper to check for label in parent chain
    fn has_labeled_in_inner(inner_rc: &Rc<RefCell<ScopeInner>>, label: &str) -> bool {
        let inner = inner_rc.borrow();
        if inner.label.as_deref() == Some(label) {
            true
        } else if let Some(parent) = &inner.parent {
            let parent_rc = Rc::clone(parent);
            drop(inner);
            Self::has_labeled_in_inner(&parent_rc, label)
        } else {
            false
        }
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

/// A thread-safe locked variable
#[derive(Debug)]
pub struct LockedVariable {
    inner: Arc<RwLock<Variable>>,
}

impl LockedVariable {
    /// Create a new locked variable
    pub fn new(var: Variable) -> Self {
        Self {
            inner: Arc::new(RwLock::new(var)),
        }
    }

    /// Read the variable
    pub fn read(&self) -> NexusResult<Variable> {
        self.inner
            .read()
            .map(|guard| guard.clone())
            .map_err(|_| NexusError::RuntimeError {
                message: "Failed to acquire read lock".to_string(),
                span: None,
            })
    }

    /// Write to the variable
    pub fn write(&self, value: Value) -> NexusResult<()> {
        let mut guard = self.inner.write().map_err(|_| NexusError::RuntimeError {
            message: "Failed to acquire write lock".to_string(),
            span: None,
        })?;
        if !guard.is_mutable() {
            return Err(NexusError::ImmutableVariable {
                name: guard.name.clone(),
                span: guard.span,
            });
        }
        guard.value = value;
        Ok(())
    }
}

impl Clone for LockedVariable {
    fn clone(&self) -> Self {
        Self {
            inner: Arc::clone(&self.inner),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scope_define_and_get() {
        let mut scope = Scope::new();
        scope
            .define(Variable {
                name: "x".to_string(),
                value: Value::I64(42),
                modifiers: VarModifiers::default(),
                span: Span::dummy(),
            })
            .unwrap();

        let var = scope.get("x").unwrap();
        assert!(matches!(var.value, Value::I64(42)));
    }

    #[test]
    fn test_scope_child() {
        let mut parent = Scope::new();
        parent
            .define(Variable {
                name: "x".to_string(),
                value: Value::I64(1),
                modifiers: VarModifiers::default(),
                span: Span::dummy(),
            })
            .unwrap();

        let mut child = parent.child();
        child
            .define(Variable {
                name: "y".to_string(),
                value: Value::I64(2),
                modifiers: VarModifiers::default(),
                span: Span::dummy(),
            })
            .unwrap();

        // Child can see parent's variable
        assert!(child.get("x").is_some());
        // Child has its own variable
        assert!(child.get("y").is_some());
        // Parent cannot see child's variable
        assert!(parent.get("y").is_none());
    }

    #[test]
    fn test_immutable_assignment() {
        let mut scope = Scope::new();
        scope
            .define(Variable {
                name: "x".to_string(),
                value: Value::I64(1),
                modifiers: VarModifiers::default(), // not mutable
                span: Span::dummy(),
            })
            .unwrap();

        let result = scope.assign("x", Value::I64(2), Span::dummy());
        assert!(result.is_err());
    }

    #[test]
    fn test_mutable_assignment() {
        let mut scope = Scope::new();
        scope
            .define(Variable {
                name: "x".to_string(),
                value: Value::I64(1),
                modifiers: VarModifiers {
                    mutable: true,
                    ..Default::default()
                },
                span: Span::dummy(),
            })
            .unwrap();

        scope.assign("x", Value::I64(2), Span::dummy()).unwrap();
        let var = scope.get("x").unwrap();
        assert!(matches!(var.value, Value::I64(2)));
    }

    #[test]
    fn test_scope_depth() {
        let parent = Scope::new();
        let child = parent.child();
        let grandchild = child.child();

        assert_eq!(parent.depth(), 0);
        assert_eq!(child.depth(), 1);
        assert_eq!(grandchild.depth(), 2);
    }

    #[test]
    fn test_labeled_scope() {
        let mut scope = Scope::new();
        scope.set_label("loop1".to_string());

        let child = scope.child();
        assert!(child.has_labeled("loop1"));
        assert!(!child.has_labeled("loop2"));
    }

    #[test]
    fn test_child_modifies_parent() {
        let mut parent = Scope::new();
        parent
            .define(Variable {
                name: "x".to_string(),
                value: Value::I64(1),
                modifiers: VarModifiers {
                    mutable: true,
                    ..Default::default()
                },
                span: Span::dummy(),
            })
            .unwrap();

        let mut child = parent.child();
        // Modify parent's variable through child
        child.assign("x", Value::I64(100), Span::dummy()).unwrap();

        // Check that parent sees the change
        let var = parent.get("x").unwrap();
        assert!(matches!(var.value, Value::I64(100)));
    }

    #[test]
    fn test_redefinition_in_same_scope() {
        let mut scope = Scope::new();
        scope
            .define(Variable {
                name: "x".to_string(),
                value: Value::I64(1),
                modifiers: VarModifiers::default(),
                span: Span::dummy(),
            })
            .unwrap();

        // Redefine in same scope should succeed
        scope
            .define(Variable {
                name: "x".to_string(),
                value: Value::I64(2),
                modifiers: VarModifiers::default(),
                span: Span::dummy(),
            })
            .unwrap();

        let var = scope.get("x").unwrap();
        assert!(matches!(var.value, Value::I64(2)));
    }

    #[test]
    fn test_define_unique_fails_on_redefinition() {
        let mut scope = Scope::new();
        scope
            .define_unique(Variable {
                name: "x".to_string(),
                value: Value::I64(1),
                modifiers: VarModifiers::default(),
                span: Span::dummy(),
            })
            .unwrap();

        // define_unique should fail
        let result = scope.define_unique(Variable {
            name: "x".to_string(),
            value: Value::I64(2),
            modifiers: VarModifiers::default(),
            span: Span::dummy(),
        });
        assert!(result.is_err());
    }
}
