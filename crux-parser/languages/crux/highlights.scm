(comment) @comment

; Keywords
"let" @keyword
"fn" @keyword
"if" @keyword
"else" @keyword
"while" @keyword
"for" @keyword
"return" @keyword
"break" @keyword
"continue" @keyword
"match" @keyword
"default" @keyword
"use" @keyword
"from" @keyword
"pub" @keyword
"struct" @keyword
"as" @keyword
"panic" @keyword
"give" @keyword

; Match patterns
"Ok" @constructor
"Err" @constructor

; Boolean literals
(boolean) @boolean

; Nil literal
(nil) @constant.builtin

; Numbers
(number) @number

; Strings
(string) @string

; Identifiers
(identifier) @variable

; Function declarations
(function_declaration (identifier) @function)

; Function expressions
(function_expression (identifier) @function)

; Parameters
(parameters (identifier) @variable.parameter)

; Struct declarations
(struct_declaration (identifier) @type)

; Operators
(binary_operator) @operator
(unary_operator) @operator

; Punctuation
";" @punctuation.delimiter
"," @punctuation.delimiter
":" @punctuation.delimiter

; Brackets
"(" @punctuation.bracket
")" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket
"[" @punctuation.bracket
"]" @punctuation.bracket

; Property access
(property_expression (identifier) @property)

; Table keys
(table_pair (string) @property)

; Field names
(field_pair (identifier) @property)
