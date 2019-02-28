open Utility
open SourceCode

type name = string [@@deriving show]

module Binder = struct
  type t = name * Types.datatype option
  and with_pos = t WithPos.t
  [@@deriving show]

  let to_name b = let (n, _ ) = WithPos.node b in n
  let to_type b = let (_, ty) = WithPos.node b in ty

  let to_type_exn b = to_type b |> OptionUtils.val_of

  let set_name b name = WithPos.map ~f:(fun (_   , ty) -> name, ty      ) b
  let set_type b typ  = WithPos.map ~f:(fun (name, _ ) -> name, Some typ) b

  let erase_type b = WithPos.map ~f:(fun (name, _) -> name, None) b
  let has_type   b = to_type b |> OptionUtils.is_some

  let traverse_map : with_pos -> o:'o
            -> f_pos:('o -> Position.t -> 'a * Position.t)
            -> f_name:('a -> name -> 'b * name)
            -> f_ty:('b -> Types.datatype option -> 'c * Types.datatype option)
            -> 'c * with_pos = fun b ~o ~f_pos ~f_name ~f_ty ->
    WithPos.traverse_map b ~o ~f_pos ~f_node:(fun o (n, ty) ->
        let o, name = f_name o n  in
        let o, typ  = f_ty   o ty in
        o, (name, typ)
      )
end
