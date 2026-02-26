; block covers function bodies, if/else, while, for
(block "}" @end) @indent

; these contain their own braces directly
(match_expression "}" @end) @indent
(array_expression "]" @end) @indent
(table_expression "}" @end) @indent
(struct_instantiation "}" @end) @indent
(struct_declaration "}" @end) @indent
