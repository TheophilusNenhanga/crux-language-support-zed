; Runnable code detection for Crux
; Top-level .cx files are runnable
(program (variable_declaration) @run)
(program (function_declaration) @run)
