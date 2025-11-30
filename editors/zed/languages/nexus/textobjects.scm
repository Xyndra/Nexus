; Text objects for Vim-style navigation
; Zed only supports: @function.around, @function.inside, @class.around, @class.inside, @comment.around, @comment.inside

; Functions
(function_definition
  (block
    "{"
    (_)* @function.inside
    "}")) @function.around

(function_definition) @function.around

; Methods
(method_definition
  (block
    "{"
    (_)* @function.inside
    "}")) @function.around

(method_definition) @function.around

; Macro definitions (treated as functions)
(macro_definition
  (block
    "{"
    (_)* @function.inside
    "}")) @function.around

(macro_definition) @function.around

; Subscopes (also function-like for navigation)
(subscope_statement
  (block
    "{"
    (_)* @function.inside
    "}")) @function.around

; Structs (as class-like constructs)
(struct_definition
  "{"
  (_)* @class.inside
  "}") @class.around

(struct_definition) @class.around

; Interfaces (as class-like constructs)
(interface_definition
  "{"
  (_)* @class.inside
  "}") @class.around

(interface_definition) @class.around

; Comments
(comment) @comment.around
(comment)+ @comment.around
