; Keywords
[
  "use"
  "from"
  "struct"
  "interface"
  "impl"
  "extends"
  "if"
  "else"
  "return"
  "defer"
  "subscope"
  "goto"
  "break"
  "unknown"
  "dyn"
  "macro"
] @keyword

; Function colors (special keywords)
(function_color) @keyword

; Variable modifiers
(variable_modifier) @keyword

; Primitive types
(primitive_type) @type.builtin

; Named types (user-defined)
(named_type (type_name) @type)

; Array type size
(array_type) @type

; Type in struct definition
(struct_definition name: (identifier) @type)

; Interface definition name
(interface_definition name: (identifier) @type)

; Implements clause types
(implements_clause (identifier) @type)

; Extends clause types
(extends_clause (identifier) @type)

; Parameter types
(parameter (_) @type)

; Function definitions
(function_definition name: (identifier) @function)

; Method definitions
(method_definition name: (identifier) @function.method)

; Macro definitions
(macro_definition name: (identifier) @function.macro)

; Method signatures in interfaces
(method_signature name: (identifier) @function.method)

; Function calls
(call_expression function: (identifier) @function.call)
(call_expression function: (field_access_expression field: (identifier) @function.call))

; Method calls
(method_call_expression method: (identifier) @function.method.call)

; Macro calls
(macro_call_expression name: (identifier) @function.macro)
"$" @punctuation.special

; Field access
(field_access_expression field: (identifier) @property)

; Field definitions in struct
(field_definition name: (identifier) @property)

; Field initialization
(field_init name: (identifier) @property)

; Parameters
(parameter name: (identifier) @variable.parameter)

; Receiver in method
(receiver (identifier) @variable.parameter)

; Variable declarations
(variable_declaration name: (identifier) @variable)

; Assignment targets
(assignment_statement target: (identifier) @variable)

; Subscope names
(subscope_statement name: (identifier) @label)

; Goto labels
(goto_statement (identifier) @label)

; Break labels
(break_statement (identifier) @label)

; Imports
(use_symbol_list (identifier) @function)
(module_path (identifier) @module)

; Literals
(integer_literal) @number
(float_literal) @number.float
(string_literal) @string
(char_literal) @character
(escape_sequence) @string.escape
(boolean_literal) @boolean
(none_literal) @constant.builtin
(error_literal) @constant.builtin

; Operators and punctuation
"=" @operator
"->" @operator
"<" @punctuation.bracket
">" @punctuation.bracket
"(" @punctuation.bracket
")" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket
"[" @punctuation.bracket
"]" @punctuation.bracket
":" @punctuation.delimiter
"," @punctuation.delimiter
"." @punctuation.delimiter
"@" @punctuation.special

; Annotations
(annotation "@" @attribute)
(annotation (identifier) @attribute)

; @unchecked special annotation in index
"@unchecked" @attribute

; Comments
(comment) @comment

; Identifiers (fallback)
(identifier) @variable
