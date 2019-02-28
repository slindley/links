open SourceCode

type name = string [@@deriving show]

module Binder : sig
  type t = (name * Types.datatype option) WithPos.t
      [@@deriving show]

  val to_name     : t -> name
  val to_type     : t -> Types.datatype option
  val to_type_exn : t -> Types.datatype

  val set_name : t -> name -> t
  val set_type : t -> Types.datatype -> t

  val erase_type : t -> t
  val has_type   : t -> bool

  val traverse_map :
    t ->
    o:'o ->
    f_pos:('o -> Position.t -> 'a * Position.t) ->
    f_name:('a -> name -> 'b * name) ->
    f_ty:('b -> Types.datatype option -> 'c * Types.datatype option) ->
    'c * t
end
