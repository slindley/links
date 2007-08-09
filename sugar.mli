(** Converts the tree returned by the parser into our internal
    representation *)
exception ConcreteSyntaxError of (string * (Lexing.position * Lexing.position))
exception PatternDuplicateNameError of (Syntax.position * string * string)
exception RedundantPatternMatch of Syntax.position

val desugar_expression : (Sugartypes.pposition -> Syntax.position) -> Sugartypes.phrase -> Syntax.untyped_expression
val desugar_definitions : (Sugartypes.pposition -> Syntax.position) -> Sugartypes.toplevel list -> Syntax.untyped_definition list
val desugar_datatype : Sugartypes.datatype -> Types.assumption
val fresh_type_variable : unit -> Sugartypes.datatype
