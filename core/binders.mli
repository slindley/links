open SourceCode

type name = string [@@deriving show]

module Binder : sig
  type t = name * Types.datatype option
  and with_pos = t WithPos.t
      [@@deriving show]

  val to_name     : with_pos -> name
  val to_type     : with_pos -> Types.datatype option
  val to_type_exn : with_pos -> Types.datatype

  val set_name : with_pos -> name -> with_pos
  val set_type : with_pos -> Types.datatype -> with_pos

  val erase_type : with_pos -> with_pos
  val has_type   : with_pos -> bool

  val traverse_map :
    with_pos ->
    o:'o ->
    f_pos:('o -> Position.t -> 'a * Position.t) ->
    f_name:('a -> name -> 'b * name) ->
    f_ty:('b -> Types.datatype option -> 'c * Types.datatype option) ->
    'c * with_pos
end
