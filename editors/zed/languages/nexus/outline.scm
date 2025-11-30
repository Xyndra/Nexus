; Code outline/structure queries for Nexus

; Function definitions
(function_definition
  name: (identifier) @name) @item

; Method definitions
(method_definition
  name: (identifier) @name) @item

; Macro definitions
(macro_definition
  name: (identifier) @name) @item

; Struct definitions
(struct_definition
  name: (identifier) @name) @item

; Interface definitions
(interface_definition
  name: (identifier) @name) @item

; Field definitions within structs (as children)
(field_definition
  name: (identifier) @name) @item

; Method signatures within interfaces (as children)
(method_signature
  name: (identifier) @name) @item

; Subscope definitions (labeled loops)
(subscope_statement
  name: (identifier) @name) @item

; Use statements provide context
(use_statement) @context
