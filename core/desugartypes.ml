open CommonTypes
open Operators
open Utility
open SourceCode
open Binders

type kind = PrimaryKind.t * subkind option
    [@@deriving show]

type type_variable = name * kind * freedom
    [@@deriving show]

(* type variable of primary kind Type? *)
type quantifier = type_variable
  [@@deriving show]

type fn_dep = string * string
    [@@deriving show]

type tyvar = Types.quantifier
  [@@deriving show]

type tyarg = Types.type_arg
  [@@deriving show]

module Datatype = struct
  type t =
    | TypeVar         of known_type_variable
    | QualifiedTypeApplication of (name list * type_arg list)
    | Function        of with_pos list * row * with_pos
    | Lolli           of with_pos list * row * with_pos
    | Mu              of name * with_pos
    | Forall          of quantifier list * with_pos
    | Unit
    | Tuple           of with_pos list
    | Record          of row
    | Variant         of row
    | Effect          of row
    | Table           of with_pos * with_pos * with_pos
    | List            of with_pos
    | TypeApplication of (string * type_arg list)
    | Primitive       of Primitive.t
    | DB
    | Input           of with_pos * with_pos
    | Output          of with_pos * with_pos
    | Select          of row
    | Choice          of row
    | Dual            of with_pos
    | End
  and with_pos = t WithPos.t
  and row = (string * fieldspec) list * row_var
  and row_var =
    | Closed
    | Open of known_type_variable
    | Recursive of name * row
  and fieldspec =
    | Present of with_pos
    | Absent
    | Var of known_type_variable
  and type_arg =
    | Type of with_pos
    | Row of row
    | Presence of fieldspec
      [@@deriving show]
end

(* Store the denotation along with the notation once it's computed *)
type datatype' = Datatype.with_pos * Types.datatype option
    [@@deriving show]

module Pattern = struct
  type t =
    | Any
    | Nil
    | Cons     of with_pos * with_pos
    | List     of with_pos list
    | Variant  of name * with_pos option
    | Effect   of name * with_pos list * with_pos
    | Negative of name list
    | Record   of (name * with_pos) list * with_pos option
    | Tuple    of with_pos list
    | Constant of Constant.t
    | Variable of Binder.with_pos
    | As       of Binder.with_pos * with_pos
    | HasType  of with_pos * datatype'
  and with_pos = t WithPos.t
   [@@deriving show]
end

type given_spawn_location =
  | ExplicitSpawnLocation of phrase (* spawnAt function *)
  | SpawnClient (* spawnClient function *)
  | NoSpawnLocation (* spawn function *)
and clause = Pattern.with_pos * phrase
and funlit = Pattern.with_pos list list * phrase
and handler_parameterisation = {
  shp_bindings: (phrase * Pattern.with_pos) list;
  shp_types: Types.datatype list
}
and handler_descriptor = {
  shd_depth: handler_depth;
  shd_types: Types.row * Types.datatype * Types.row * Types.datatype;
  shd_raw_row: Types.row;
  shd_params: handler_parameterisation option
}
and handler = {
  sh_expr: phrase;
  sh_effect_cases: clause list;
  sh_value_cases: clause list;
  sh_descr: handler_descriptor
}
and phrasenode =
  | Constant         of Constant.t
  | Var              of name
  | Query            of (phrase * phrase) option * phrase *
                          Types.datatype option
  | RangeLit         of (phrase * phrase)
  | ListLit          of phrase list * Types.datatype
  | Escape           of Binder.with_pos * phrase
  | Section          of Section.t
  | Conditional      of phrase * phrase * phrase
  | Block            of block_body
  | InfixAppl        of (tyarg list * BinaryOp.t) * phrase * phrase
  | UnaryAppl        of (tyarg list * UnaryOp.t) * phrase
  | FnAppl           of phrase * phrase list
  | TAbstr           of tyvar list ref * phrase
  | TAppl            of phrase * tyarg list
  | TupleLit         of phrase list
  | RecordLit        of (name * phrase) list * phrase option
  | Projection       of phrase * name
  | With             of phrase * (name * phrase) list
  | TypeAnnotation   of phrase * datatype'
  | Upcast           of phrase * datatype' * datatype'
  | ConstructorLit   of name * phrase option * Types.datatype
  | DoOperation      of name * phrase list * Types.datatype
  | Handle           of handler
  | Switch           of phrase * (Pattern.with_pos * phrase) list *
                          Types.datatype
  | DatabaseLit      of phrase * (phrase option * phrase option)
  | TableLit         of phrase * (Datatype.with_pos * (Types.datatype *
                           Types.datatype * Types.datatype)) *
                          (name * fieldconstraint list) list * phrase * phrase
  | DBDelete         of Pattern.with_pos * phrase * phrase option
  | DBUpdate         of Pattern.with_pos * phrase * phrase option *
                          (name * phrase) list
  | LensLit          of phrase * Types.lens_sort
  (* the lens keys lit is a literal that takes an expression and is converted
     into a LensLit with the corresponding table keys marked in the lens_sort *)
  | LensDropLit      of phrase * string * string * phrase *
                          Types.lens_sort
  | LensSelectLit    of phrase * phrase * Types.lens_sort
  | LensJoinLit      of phrase * phrase * phrase * phrase * phrase *
                          Types.lens_sort
  | LensGetLit       of phrase * Types.datatype
  | LensPutLit       of phrase * phrase * Types.datatype
  | Xml              of name * (name * (phrase list)) list * phrase option *
                          phrase list
  | TextNode         of string
  (* choose *)
  | Select           of name * phrase
  (* choice *)
  | Offer            of phrase * (Pattern.with_pos * phrase) list *
                          Types.datatype
and phrase = phrasenode WithPos.t
and bindingnode =
  | Val     of (Pattern.with_pos * (tyvar list * phrase) * Location.t *
                  datatype' option)
  | Fun     of (Binder.with_pos * DeclaredLinearity.t * (tyvar list * funlit) *
                  Location.t * datatype' option)
  | Funs    of (Binder.with_pos * DeclaredLinearity.t *
                  ((tyvar list *
                   (Types.datatype * Types.quantifier option list) option)
                   * funlit) * Location.t * datatype' option * Position.t) list
  | Foreign of (Binder.with_pos * name * name * name * datatype')
  | Type    of (name * (quantifier * tyvar option) list * datatype')
  | Infix
  | Exp     of phrase
and binding = bindingnode WithPos.t
and block_body = binding list * phrase
                  [@@deriving show]

type directive = string * string list
                            [@@deriving show]

type sentence =
  | Definitions of binding list
  | Expression  of phrase
  | Directive   of directive
    [@@deriving show]

type program = binding list * phrase option
  [@@deriving show]

module Desugar = struct
  (* JSTOLAREK: this module is here to prevent name clash of various definitions
     of with_pos.  If we get rid of positions in the desugared AST there will no
     longer be a name clash and thus this module can be removed. *)
  module Datatype = struct
    let rec datatype : Sugartypes.Datatype.t -> Datatype.t =
      function
      | Sugartypes.Datatype.TypeVar ktv ->
         Datatype.TypeVar ktv
      | Sugartypes.Datatype.QualifiedTypeApplication (names, tyargs) ->
         let tyargs' = List.map type_arg tyargs in
         Datatype.QualifiedTypeApplication (names, tyargs')
      | Sugartypes.Datatype.Function (dts, r, dt) ->
         let dts' = List.map with_pos dts in
         let r'   = row r in
         let dt'  = with_pos dt in
         Datatype.Function (dts', r', dt')
      | Sugartypes.Datatype.Lolli (dts, r, dt) ->
         let dts' = List.map with_pos dts in
         let r'   = row r in
         let dt'  = with_pos dt in
         Datatype.Lolli (dts', r', dt')
      | Sugartypes.Datatype.Mu (name, wp) ->
         Datatype.Mu (name, with_pos wp)
      | Sugartypes.Datatype.Forall (quantifires, wp) ->
         Datatype.Forall (quantifires, with_pos wp)
      | Sugartypes.Datatype.Unit ->
         Datatype.Unit
      | Sugartypes.Datatype.Tuple components ->
         Datatype.Tuple (List.map with_pos components)
      | Sugartypes.Datatype.Record r ->
         Datatype.Record (row r)
      | Sugartypes.Datatype.Variant r ->
         Datatype.Variant (row r)
      | Sugartypes.Datatype.Effect r ->
         Datatype.Effect (row r)
      | Sugartypes.Datatype.Table (wp1, wp2, wp3) ->
         Datatype.Table (with_pos wp1, with_pos wp2, with_pos wp3)
      | Sugartypes.Datatype.List wp ->
         Datatype.List (with_pos wp)
      | Sugartypes.Datatype.TypeApplication (name, tyargs) ->
         let tyargs' = List.map type_arg tyargs in
         Datatype.TypeApplication (name, tyargs')
      | Sugartypes.Datatype.Primitive p ->
         Datatype.Primitive p
      | Sugartypes.Datatype.DB ->
         Datatype.DB
      | Sugartypes.Datatype.Input (wp1, wp2) ->
         Datatype.Input (with_pos wp1, with_pos wp2)
      | Sugartypes.Datatype.Output (wp1, wp2) ->
         Datatype.Output (with_pos wp1, with_pos wp2)
      | Sugartypes.Datatype.Select r ->
         Datatype.Select (row r)
      | Sugartypes.Datatype.Choice r ->
         Datatype.Choice (row r)
      | Sugartypes.Datatype.Dual wp ->
         Datatype.Dual (with_pos wp)
      | Sugartypes.Datatype.End ->
         Datatype.End
    and with_pos {WithPos.node; pos} = WithPos.make ~pos (datatype node)
    and row = fun (fspecs, rvar) ->
      let fspecs' = List.map (fun (str, fsp) -> str, fieldspec fsp) fspecs in
      let rvar'   = row_var rvar in
      (fspecs', rvar')
    and row_var = function
      | Sugartypes.Datatype.Closed   -> Datatype.Closed
      | Sugartypes.Datatype.Open ktv -> Datatype.Open ktv
      | Sugartypes.Datatype.Recursive (name, r) ->
         Datatype.Recursive (name, row r)
    and fieldspec = function
      | Sugartypes.Datatype.Present wp -> Datatype.Present (with_pos wp)
      | Sugartypes.Datatype.Absent     -> Datatype.Absent
      | Sugartypes.Datatype.Var ktv    -> Datatype.Var ktv
    and type_arg = function
      | Sugartypes.Datatype.Type wp     -> Datatype.Type (with_pos wp)
      | Sugartypes.Datatype.Row r       -> Datatype.Row (row r)
      | Sugartypes.Datatype.Presence fs -> Datatype.Presence (fieldspec fs)
                                             [@@deriving show]
  end

  let datatype' : Sugartypes.datatype' -> datatype' =
    fun (dt_wp, type_opt) -> (Datatype.with_pos dt_wp, type_opt)

  module Pattern = struct
    let rec pattern = function
      | Sugartypes.Pattern.Any -> Pattern.Any
      | Sugartypes.Pattern.Nil -> Pattern.Nil
      | Sugartypes.Pattern.Cons (hd, tl) ->
         Pattern.Cons (with_pos hd, with_pos tl)
      | Sugartypes.Pattern.List xs ->
         Pattern.List (List.map with_pos xs)
      | Sugartypes.Pattern.Variant (n, wp_opt) ->
         Pattern.Variant (n, OptionUtils.opt_map with_pos wp_opt)
      | Sugartypes.Pattern.Effect (name, wps, wp) ->
         Pattern.Effect (name, List.map with_pos wps, with_pos wp)
      | Sugartypes.Pattern.Negative names ->
         Pattern.Negative names
      | Sugartypes.Pattern.Record (fields, wp_opt) ->
         let fields' = List.map (fun (n, wp) -> (n, with_pos wp)) fields in
         Pattern.Record (fields', OptionUtils.opt_map with_pos wp_opt)
      | Sugartypes.Pattern.Tuple components ->
         Pattern.Tuple (List.map with_pos components)
      | Sugartypes.Pattern.Constant constant ->
         Pattern.Constant constant
      | Sugartypes.Pattern.Variable bndr ->
         Pattern.Variable bndr
      | Sugartypes.Pattern.As (bndr, pat) ->
         Pattern.As (bndr, with_pos pat)
      | Sugartypes.Pattern.HasType (pat, ty) ->
         Pattern.HasType (with_pos pat,  datatype' ty)
    and with_pos {WithPos.node; pos} = WithPos.make ~pos (pattern node)
  end

  let rec given_spawn_location :
        Sugartypes.given_spawn_location -> given_spawn_location = function
    | Sugartypes.ExplicitSpawnLocation phr -> ExplicitSpawnLocation (phrase phr)
    | Sugartypes.SpawnClient               -> SpawnClient
    | Sugartypes.NoSpawnLocation           -> NoSpawnLocation
  and clause : Sugartypes.clause -> clause =
    fun (pat, phr) -> (Pattern.with_pos pat, phrase phr)
  and funlit : Sugartypes.funlit -> funlit =
    fun (patss, phr) ->
    let patss' = List.map (List.map Pattern.with_pos) patss in
    (patss', phrase phr)
  and handler : Sugartypes.handler -> handler =
    fun {Sugartypes.sh_expr; Sugartypes.sh_effect_cases;
         Sugartypes.sh_value_cases; Sugartypes.sh_descr} ->
    { sh_expr         = phrase sh_expr
    ; sh_effect_cases = List.map clause sh_effect_cases
    ; sh_value_cases  = List.map clause sh_value_cases
    ; sh_descr        = handler_descriptor sh_descr
    }
  and handler_descriptor : Sugartypes.handler_descriptor -> handler_descriptor =
    fun {Sugartypes.shd_depth; Sugartypes.shd_types; Sugartypes.shd_raw_row;
         Sugartypes.shd_params } ->
    { shd_depth; shd_types; shd_raw_row
    ; shd_params = OptionUtils.opt_map handler_parameterisation shd_params
    }
  and handler_parameterisation :
        Sugartypes.handler_parameterisation -> handler_parameterisation =
    fun {Sugartypes.shp_bindings; Sugartypes.shp_types} ->
    { shp_bindings =
        List.map (fun (phr, pat) -> (phrase phr, Pattern.with_pos pat))
                 shp_bindings
    ; shp_types
    }
  and phrasenode : Sugartypes.phrasenode -> phrasenode = function
    | Sugartypes.Constant c ->
       Constant c
    | Sugartypes.Var name ->
       Var name
    | Sugartypes.Query (ph_opt, ph, ty_opt) ->
       let ph_opt' = OptionUtils.opt_map (fun (p1, p2) -> (phrase p1,
                                                           phrase p2)) ph_opt in
       Query (ph_opt', phrase ph, ty_opt)
    | Sugartypes.RangeLit (p1, p2) ->
       RangeLit (phrase p1, phrase p2)
    | Sugartypes.ListLit (elems, Some ty) ->
       ListLit (phrases elems, ty)
    | Sugartypes.Escape (bndr, phr) ->
       Escape (bndr, phrase phr)
    | Sugartypes.Section sec ->
       Section sec
    | Sugartypes.Conditional (if_, then_, else_) ->
       Conditional (phrase if_, phrase then_, phrase else_)
    | Sugartypes.Block blk ->
       Block (block_body blk)
    | Sugartypes.InfixAppl ((tyargs, op), arg1, arg2) ->
       InfixAppl ((tyargs, op), phrase arg1, phrase arg2)
    | Sugartypes.UnaryAppl ((tyargs, op), arg) ->
       UnaryAppl ((tyargs, op), phrase arg)
    | Sugartypes.FnAppl (fn, args) ->
       FnAppl (phrase fn, phrases args)
    | Sugartypes.TAbstr (tv_ref, body) ->
       TAbstr (tv_ref, phrase body)
    | Sugartypes.TAppl (ty, ty_args) ->
       TAppl (phrase ty, ty_args)
    | Sugartypes.TupleLit components ->
       TupleLit (phrases components)
    | Sugartypes.RecordLit (fields, phr) ->
       RecordLit (List.map (fun (name, v) -> (name, phrase v)) fields, phrase_opt phr)
    | Sugartypes.Projection (phr, name) ->
       Projection (phrase phr, name)
    | Sugartypes.With (phr, fields) ->
       With (phrase phr, List.map (fun (n, p) -> (n, phrase p)) fields)
    | Sugartypes.TypeAnnotation (phr, ty') ->
       TypeAnnotation (phrase phr, datatype' ty')
    | Sugartypes.Upcast (phr, ty1, ty2) ->
       Upcast (phrase phr, datatype' ty1, datatype' ty2)
    | Sugartypes.ConstructorLit (name, phr_opt, Some ty) ->
       ConstructorLit (name, phrase_opt phr_opt, ty)
    | Sugartypes.DoOperation (name, phrs, Some ty) ->
       DoOperation (name, phrases phrs, ty)
    | Sugartypes.Handle hdlr ->
       Handle (handler hdlr)
    | Sugartypes.Switch (scrut, cases, Some ty) ->
       Switch (phrase scrut, List.map (fun (pat, case) ->
                                 (Pattern.with_pos pat, phrase case)) cases,
               ty)
    | Sugartypes.DatabaseLit (phr, (p1, p2)) ->
       DatabaseLit (phrase phr, (phrase_opt p1, phrase_opt p2))
    | Sugartypes.TableLit (phr, (dt, Some tys), constraints, p1, p2) ->
       TableLit (phrase phr, (Datatype.with_pos dt, tys), constraints,
                 phrase p1, phrase p2)
    | Sugartypes.DBDelete (pat, phr, phr_opt) ->
       DBDelete (Pattern.with_pos pat, phrase phr, phrase_opt phr_opt)
    | Sugartypes.DBUpdate (pat, p1, phr_opt, exps) ->
       DBUpdate (Pattern.with_pos pat, phrase p1, phrase_opt phr_opt,
                 List.map (fun (n,p) -> (n, phrase p)) exps)
    | Sugartypes.LensLit (phr, Some sort) ->
       LensLit (phrase phr, sort)
    | Sugartypes.LensDropLit (p1, s1, s2, p2, Some sort) ->
       LensDropLit (phrase p1, s1, s2, phrase p2, sort)
    | Sugartypes.LensSelectLit (p1, p2, Some sort) ->
       LensSelectLit (phrase p1, phrase p2, sort)
    | Sugartypes.LensJoinLit (p1, p2, p3, p4, p5, Some sort) ->
       LensJoinLit (phrase p1, phrase p2, phrase p3, phrase p4, phrase p5,
                    sort)
    | Sugartypes.LensGetLit (p, Some ty) ->
       LensGetLit (phrase p, ty)
    | Sugartypes.LensPutLit (p1, p2, Some ty) ->
       LensPutLit (phrase p1, phrase p2, ty)
    | Sugartypes.Xml (n, attrs, p, children) ->
       Xml (n, List.map (fun (n, phs) -> (n, phrases phs)) attrs, phrase_opt p,
            phrases children)
    | Sugartypes.TextNode str ->
       TextNode str
    | Sugartypes.Select (n, p) ->
       Select (n, phrase p)
    | Sugartypes.Offer (p, cases, Some ty) ->
       Offer (phrase p, List.map (fun (pat, case) ->
                            (Pattern.with_pos pat, phrase case)) cases, ty)
    | Sugartypes.ListLit (_, None)
    | Sugartypes.ConstructorLit (_, _, None)
    | Sugartypes.Switch (_, _, None)
    | Sugartypes.TableLit (_, (_, None), _, _, _)
    | Sugartypes.LensLit (_, None)
    | Sugartypes.LensPutLit (_, _, None)
    | Sugartypes.LensDropLit (_, _, _, _, None)
    | Sugartypes.LensSelectLit (_, _, None)
    | Sugartypes.LensJoinLit (_, _, _, _, _, None)
    | Sugartypes.LensGetLit (_, None)
    | Sugartypes.LensKeysLit _
    | Sugartypes.LensFunDepsLit _
    | Sugartypes.Offer (_, _, None)
    | Sugartypes.DoOperation (_, _, None)
    | Sugartypes.QualifiedVar _
    | Sugartypes.FunLit _
    | Sugartypes.Page _
    | Sugartypes.FormletPlacement _
    | Sugartypes.PagePlacement _
    | Sugartypes.FormBinding _
    | Sugartypes.Formlet _
    | Sugartypes.Regex _
    | Sugartypes.Iteration _
    | Sugartypes.DBInsert _
    | Sugartypes.Spawn _
    | Sugartypes.Receive _
    | Sugartypes.Raise
    | Sugartypes.HandlerLit _
    | Sugartypes.TryInOtherwise _
    | Sugartypes.CP _ -> assert false
  and phrase_opt : Sugartypes.phrase option -> phrase option =
    fun phr_opt -> OptionUtils.opt_map (fun phr -> phrase phr) phr_opt
  and phrase {WithPos.node; pos} = WithPos.make ~pos (phrasenode node)
  and phrases phrs = List.map phrase phrs
  and bindingnode : Sugartypes.bindingnode -> bindingnode = function
    | Sugartypes.Val (pat, (tvs, p), loc, ty') ->
       Val (Pattern.with_pos pat, (tvs, phrase p), loc,
            OptionUtils.opt_map datatype' ty')
    | Sugartypes.Fun (bndr, lin, (tvs, flit), loc, ty') ->
       Fun (bndr, lin, (tvs, funlit flit), loc,
            OptionUtils.opt_map datatype' ty')
  | Sugartypes.Funs decls ->
     let desugar_decl (bndr, lin, (tvs, flit), loc, ty', pos) =
       (bndr, lin, (tvs, funlit flit), loc, OptionUtils.opt_map datatype' ty',
        pos)
     in Funs (List.map desugar_decl decls)
  | Sugartypes.Foreign (bndr, n1, n2, n3, ty') ->
     Foreign (bndr, n1, n2, n3, datatype' ty')
  | Sugartypes.Type (n, tvs, ty') ->
     Type (n, tvs, datatype' ty')
  | Sugartypes.Infix ->
     Infix
  | Sugartypes.Exp p ->
     Exp (phrase p)
  | Sugartypes.Handler _
  | Sugartypes.QualifiedImport _
  | Sugartypes.Module _
  | Sugartypes.AlienBlock _ -> assert false
  and binding {WithPos.node; pos} = WithPos.make ~pos (bindingnode node)
  and block_body : Sugartypes.block_body -> block_body =
    fun (binds, body) -> (List.map binding binds, phrase body)

  and directive : Sugartypes.directive -> directive =
    fun d -> d

  let sentence : Sugartypes.sentence -> sentence = function
    | Sugartypes.Definitions binds ->
       Definitions (List.map binding binds)
    | Sugartypes.Expression p ->
       Expression (phrase p)
    | Sugartypes.Directive d ->
       Directive (directive d)

  let program : Sugartypes.program -> program =
    fun (binds, phr) -> (List.map binding binds, phrase_opt phr)
end
