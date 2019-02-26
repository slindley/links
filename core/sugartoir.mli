(** Converts the tree returned by the parser into our internal
    representation *)

val show_compiled_ir : bool Settings.setting

type nenv = Var.var Env.String.t
type tenv = Types.datatype Env.Int.t

type env = nenv * tenv * Types.row

val desugar_expression : env -> Desugartypes.phrase -> Ir.computation
val desugar_definitions : env -> Desugartypes.binding list ->
  Ir.binding list * nenv
val desugar_program : env -> Desugartypes.program ->
  Ir.binding list * Ir.computation * nenv
