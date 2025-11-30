; Indentation rules for Nexus

; Blocks increase indentation
(block "{" @indent)
(block "}" @outdent)

; Struct definitions
(struct_definition "{" @indent)
(struct_definition "}" @outdent)

; Interface definitions
(interface_definition "{" @indent)
(interface_definition "}" @outdent)

; Array expressions
(array_expression "[" @indent)
(array_expression "]" @outdent)

; Struct initialization
(struct_init_expression "{" @indent)
(struct_init_expression "}" @outdent)

; Parameter lists (for multi-line parameters)
(parameter_list "(" @indent)
(parameter_list ")" @outdent)

; Argument lists (for multi-line arguments)
(argument_list "(" @indent)
(argument_list ")" @outdent)
