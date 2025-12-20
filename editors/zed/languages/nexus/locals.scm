; Scopes

(source_file) @local.scope

(function_definition) @local.scope
(method_definition) @local.scope
(macro_definition) @local.scope

(block) @local.scope
(subscope_statement) @local.scope

(if_statement) @local.scope
(else_clause) @local.scope

(lambda_expression) @local.scope

; Definitions

; Function parameters
(parameter name: (identifier) @local.definition)

; Receiver parameter in methods
(receiver (identifier) @local.definition)

; Variable declarations
(variable_declaration name: (identifier) @local.definition)

; Function definitions
(function_definition name: (identifier) @local.definition)

; Method definitions
(method_definition name: (identifier) @local.definition)

; Macro definitions
(macro_definition name: (identifier) @local.definition)

; Struct definitions
(struct_definition name: (identifier) @local.definition)

; Interface definitions
(interface_definition name: (identifier) @local.definition)

; Field definitions
(field_definition name: (identifier) @local.definition)

; Subscope names (labels)
(subscope_statement name: (identifier) @local.definition)

; References

; Variable references
(variable_declaration name: (identifier) @local.reference)

; Identifiers in expressions
(call_expression function: (identifier) @local.reference)
(field_access_expression object: (identifier) @local.reference)
(index_expression array: (identifier) @local.reference)

; Goto references
(goto_statement (identifier) @local.reference)

; Type references
(named_type) @local.reference
