val generalise : Types.environment -> Types.datatype -> ((Types.quantifier list * Types.type_arg list) * Types.datatype)
val generalise_rigid : Types.environment -> Types.datatype -> ((Types.quantifier list * Types.type_arg list) * Types.datatype)
val get_type_args : Types.environment -> Types.datatype -> Types.type_arg list
(* val get_quantifiers : Types.environment -> Types.datatype -> Types.quantifier list *)
(* val extract_quantifiers : Types.quantifier list -> Types.quantifier list *)
