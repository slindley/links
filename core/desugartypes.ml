open CommonTypes
open Operators
open Utility
open SourceCode
open Sugartypes

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
    | Variable of Binder.t
    | As       of Binder.t * with_pos
    | HasType  of with_pos * datatype'
  and with_pos = t WithPos.t
   [@@deriving show]
end

type replace_rhs =
  | Literal     of string
  | SpliceExpr  of phrase
and given_spawn_location =
  | ExplicitSpawnLocation of phrase (* spawnAt function *)
  | SpawnClient (* spawnClient function *)
  | NoSpawnLocation (* spawn function *)
and regex =
  | Range     of (char * char)
  | Simply    of string
  | Quote     of regex
  | Any
  | StartAnchor
  | EndAnchor
  | Seq       of regex list
  | Alternate of (regex * regex)
  | Group     of regex
  | Repeat    of (Regex.repeat * regex)
  | Splice    of phrase
  | Replace   of (regex * replace_rhs)
and clause = Pattern.with_pos * phrase
and funlit = Pattern.with_pos list list * phrase
and handlerlit =
  handler_depth * Pattern.with_pos * clause list *
    Pattern.with_pos list list option (* computation arg, cases, parameters *)
and handler = {
  sh_expr: phrase;
  sh_effect_cases: clause list;
  sh_value_cases: clause list;
  sh_descr: handler_descriptor
}
and handler_descriptor = {
  shd_depth: handler_depth;
  shd_types: Types.row * Types.datatype * Types.row * Types.datatype;
  shd_raw_row: Types.row;
  shd_params: handler_parameterisation option
}
and handler_parameterisation = {
  shp_bindings: (phrase * Pattern.with_pos) list;
  shp_types: Types.datatype list
}
and iterpatt =
  | List  of (Pattern.with_pos * phrase)
  | Table of (Pattern.with_pos * phrase)
and phrasenode =
  | Constant         of Constant.t
  | Var              of name
  | QualifiedVar     of name list
  | FunLit           of ((Types.datatype * Types.row) list) option *
                          DeclaredLinearity.t * funlit * Location.t
  | HandlerLit       of handlerlit
  (* Spawn kind, expression referring to spawn location (client n, server...),
      spawn block, row opt *)
  | Spawn            of spawn_kind * given_spawn_location * phrase *
                          Types.row option
  | Query            of (phrase * phrase) option * phrase *
                          Types.datatype option
  | RangeLit         of (phrase * phrase)
  | ListLit          of phrase list * Types.datatype option
  | Iteration        of iterpatt list * phrase
                        * (*where:*)   phrase option
                        * (*orderby:*) phrase option
  | Escape           of Binder.t * phrase
  | Section          of Section.t
  | Conditional      of phrase * phrase * phrase
  | Block            of block_body
  | InfixAppl        of (tyarg list * BinaryOp.t) * phrase * phrase
  | Regex            of regex
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
  | ConstructorLit   of name * phrase option * Types.datatype option
  | DoOperation      of name * phrase list * Types.datatype option
  | Handle           of handler
  | Switch           of phrase * (Pattern.with_pos * phrase) list *
                          Types.datatype option
  | Receive          of (Pattern.with_pos * phrase) list * Types.datatype option
  | DatabaseLit      of phrase * (phrase option * phrase option)
  | TableLit         of phrase * (Datatype.with_pos * (Types.datatype *
                           Types.datatype * Types.datatype) option) *
                          (name * fieldconstraint list) list * phrase * phrase
  | DBDelete         of Pattern.with_pos * phrase * phrase option
  | DBInsert         of phrase * name list * phrase * phrase option
  | DBUpdate         of Pattern.with_pos * phrase * phrase option *
                          (name * phrase) list
  | LensLit          of phrase * Types.lens_sort option
  (* the lens keys lit is a literal that takes an expression and is converted
     into a LensLit with the corresponding table keys marked in the lens_sort *)
  | LensKeysLit      of phrase * phrase * Types.lens_sort option
  | LensFunDepsLit   of phrase * (string list * string list) list *
                          Types.lens_sort option
  | LensDropLit      of phrase * string * string * phrase *
                          Types.lens_sort option
  | LensSelectLit    of phrase * phrase * Types.lens_sort option
  | LensJoinLit      of phrase * phrase * phrase * phrase * phrase *
                          Types.lens_sort option
  | LensGetLit       of phrase * Types.datatype option
  | LensPutLit       of phrase * phrase * Types.datatype option
  | Xml              of name * (name * (phrase list)) list * phrase option *
                          phrase list
  | TextNode         of string
  | Formlet          of phrase * phrase
  | Page             of phrase
  | FormletPlacement of phrase * phrase * phrase
  | PagePlacement    of phrase
  | FormBinding      of phrase * Pattern.with_pos
  (* choose *)
  | Select           of name * phrase
  (* choice *)
  | Offer            of phrase * (Pattern.with_pos * phrase) list *
                          Types.datatype option
  | TryInOtherwise   of (phrase * Pattern.with_pos * phrase * phrase *
                           Types.datatype option)
  | Raise
and phrase = phrasenode WithPos.t
and bindingnode =
  | Val     of (Pattern.with_pos * (tyvar list * phrase) * Location.t *
                  datatype' option)
  | Fun     of (Binder.t * DeclaredLinearity.t * (tyvar list * funlit) *
                  Location.t * datatype' option)
  | Funs    of (Binder.t * DeclaredLinearity.t *
                  ((tyvar list *
                   (Types.datatype * Types.quantifier option list) option)
                   * funlit) * Location.t * datatype' option * Position.t) list
  | Handler of (Binder.t * handlerlit * datatype' option)
  | Foreign of (Binder.t * name * name * name * datatype')
               (* Binder, raw function name, language, external file, type *)
  | QualifiedImport of name list
  | Type    of (name * (quantifier * tyvar option) list * datatype')
  | Infix
  | Exp     of phrase
  | Module  of (name * binding list)
  | AlienBlock of (name * name * ((Binder.t * datatype') list))
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
    and with_pos {WithPos.Legacy.node; pos} =
      {WithPos.Legacy.node=datatype node; pos}
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
    and with_pos {WithPos.Legacy.node; pos} =
      {WithPos.Legacy.node=pattern node; pos}
  end

  let rec replace_rhs : Sugartypes.replace_rhs -> replace_rhs = function
    | Sugartypes.Literal    str -> Literal str
    | Sugartypes.SpliceExpr phr -> SpliceExpr (phrase phr)
  and given_spawn_location :
        Sugartypes.given_spawn_location -> given_spawn_location = function
    | Sugartypes.ExplicitSpawnLocation phr -> ExplicitSpawnLocation (phrase phr)
    | Sugartypes.SpawnClient               -> SpawnClient
    | Sugartypes.NoSpawnLocation           -> NoSpawnLocation
  and regex : Sugartypes.regex -> regex = function
    | Sugartypes.Range (start, stop)    -> Range (start, stop)
    | Sugartypes.Simply str             -> Simply str
    | Sugartypes.Quote rgx              -> Quote (regex rgx)
    | Sugartypes.Any                    -> Any
    | Sugartypes.StartAnchor            -> StartAnchor
    | Sugartypes.EndAnchor              -> EndAnchor
    | Sugartypes.Seq regexes            -> Seq (List.map regex regexes)
    | Sugartypes.Alternate (rgx1, rgx2) -> Alternate (regex rgx1, regex rgx2)
    | Sugartypes.Group rgx              -> Group (regex rgx)
    | Sugartypes.Repeat (repeat, rgx)   -> Repeat (repeat, regex rgx)
    | Sugartypes.Splice phr             -> Splice (phrase phr)
    | Sugartypes.Replace (rgx, rhs)     -> Replace (regex rgx, replace_rhs rhs)
  and clause : Sugartypes.clause -> clause =
    fun (pat, phr) -> (Pattern.with_pos pat, phrase phr)
  and funlit : Sugartypes.funlit -> funlit =
    fun (patss, phr) ->
    let patss' = List.map (List.map Pattern.with_pos) patss in
    (patss', phrase phr)
  and handlerlit : Sugartypes.handlerlit -> handlerlit =
    fun (depth, pat, clauses, patss_opt) ->
    let patss_opt' = OptionUtils.opt_map (List.map (List.map Pattern.with_pos))
                                         patss_opt in
    (depth, Pattern.with_pos pat, List.map clause clauses, patss_opt')
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
  and iterpatt : Sugartypes.iterpatt -> iterpatt = function
    | Sugartypes.List (pat, phr) ->
       List (Pattern.with_pos pat, phrase phr)
    | Sugartypes.Table (pat, phr) ->
       Table (Pattern.with_pos pat, phrase phr)
  and phrasenode : Sugartypes.phrasenode -> phrasenode = function
    | Sugartypes.Constant c ->
       Constant c
    | Sugartypes.Var name ->
       Var name
    | Sugartypes.QualifiedVar names ->
       QualifiedVar names
    | Sugartypes.FunLit (params, lin, flit, loc) ->
       FunLit (params, lin, funlit flit, loc)
    | Sugartypes.HandlerLit hlit ->
       HandlerLit (handlerlit hlit)
    | Sugartypes.Spawn (sp_kind, sp_loc, phr, row_opt) ->
       Spawn (sp_kind, given_spawn_location sp_loc, phrase phr, row_opt)
    | Sugartypes.Query (ph_opt, ph, ty_opt) ->
       let ph_opt' = OptionUtils.opt_map (fun (p1, p2) -> (phrase p1,
                                                           phrase p2)) ph_opt in
       Query (ph_opt', phrase ph, ty_opt)
    | Sugartypes.RangeLit (p1, p2) ->
       RangeLit (phrase p1, phrase p2)
    | Sugartypes.ListLit (elems, ty_opt) ->
       ListLit (phrases elems, ty_opt)
    | Sugartypes.Iteration (pats, gen, where_opt, orderby_opt) ->
       Iteration (List.map iterpatt pats, phrase gen, phrase_opt where_opt  ,
                  phrase_opt orderby_opt)
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
    | Sugartypes.Regex rgx ->
       Regex (regex rgx)
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
    | Sugartypes.ConstructorLit (name, phr_opt, ty_opt) ->
       ConstructorLit (name, phrase_opt phr_opt, ty_opt)
    | Sugartypes.DoOperation (name, phrs, ty_opt) ->
       DoOperation (name, phrases phrs, ty_opt)
    | Sugartypes.Handle hdlr ->
       Handle (handler hdlr)
    | Sugartypes.Switch (scrut, cases, ty_opt) ->
       Switch (phrase scrut, List.map (fun (pat, case) ->
                                 (Pattern.with_pos pat, phrase case)) cases,
               ty_opt)
    | Sugartypes.Receive (cases, ty_opt) ->
       Receive (List.map (fun (pat, case) ->
                    (Pattern.with_pos pat, phrase case)) cases,
               ty_opt)
    | Sugartypes.DatabaseLit (phr, (p1, p2)) ->
       DatabaseLit (phrase phr, (phrase_opt p1, phrase_opt p2))
    | Sugartypes.TableLit (phr, (dt, tys), constraints, p1, p2) ->
       TableLit (phrase phr, (Datatype.with_pos dt, tys), constraints, phrase p1, phrase p2)
    | Sugartypes.DBDelete (pat, phr, phr_opt) ->
       DBDelete (Pattern.with_pos pat, phrase phr, phrase_opt phr_opt)
    | Sugartypes.DBInsert (p1, names, p2, phr_opt) ->
       DBInsert (phrase p1, names, phrase p2, phrase_opt phr_opt)
    | Sugartypes.DBUpdate (pat, p1, phr_opt, exps) ->
       DBUpdate (Pattern.with_pos pat, phrase p1, phrase_opt phr_opt,
                 List.map (fun (n,p) -> (n, phrase p)) exps)
    | Sugartypes.LensLit (phr, sort) ->
       LensLit (phrase phr, sort)
    | Sugartypes.LensKeysLit (p1, p2, sort) ->
       LensKeysLit (phrase p1, phrase p2, sort)
    | Sugartypes.LensFunDepsLit (p1, fundeps, sort) ->
       LensFunDepsLit (phrase p1, fundeps, sort)
    | Sugartypes.LensDropLit (p1, s1, s2, p2, sort) ->
       LensDropLit (phrase p1, s1, s2, phrase p2, sort)
    | Sugartypes.LensSelectLit (p1, p2, sort) ->
       LensSelectLit (phrase p1, phrase p2, sort)
    | Sugartypes.LensJoinLit (p1, p2, p3, p4, p5, sort) ->
       LensJoinLit (phrase p1, phrase p2, phrase p3, phrase p4, phrase p5,
                    sort)
    | Sugartypes.LensGetLit (p, ty) ->
       LensGetLit (phrase p, ty)
    | Sugartypes.LensPutLit (p1, p2, ty) ->
       LensPutLit (phrase p1, phrase p2, ty)
    | Sugartypes.Xml (n, attrs, p, children) ->
       Xml (n, List.map (fun (n, phs) -> (n, phrases phs)) attrs, phrase_opt p,
            phrases children)
    | Sugartypes.TextNode str ->
       TextNode str
    | Sugartypes.Formlet (p1, p2) ->
       Formlet (phrase p1, phrase p2)
    | Sugartypes.Page p ->
       Page (phrase p)
    | Sugartypes.FormletPlacement (p1, p2, p3) ->
       FormletPlacement (phrase p1, phrase p2, phrase p3)
    | Sugartypes.PagePlacement p ->
       PagePlacement (phrase p)
    | Sugartypes.FormBinding (p, pat) ->
       FormBinding (phrase p, Pattern.with_pos pat)
    | Sugartypes.Select (n, p) ->
       Select (n, phrase p)
    | Sugartypes.Offer (p, cases, ty) ->
       Offer (phrase p, List.map (fun (pat, case) ->
                            (Pattern.with_pos pat, phrase case)) cases, ty)
    | Sugartypes.TryInOtherwise (p1, pat, p2, p3, ty) ->
       TryInOtherwise (phrase p1, Pattern.with_pos pat, phrase p2, phrase p3, ty)
    | Sugartypes.Raise -> Raise
    | Sugartypes.CP _ -> assert false
  and phrase_opt : Sugartypes.phrase option -> phrase option =
    fun phr_opt -> OptionUtils.opt_map (fun phr -> phrase phr) phr_opt
  and phrase {WithPos.Legacy.node; pos} =
    {WithPos.Legacy.node=phrasenode node; pos}
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
  | Sugartypes.Handler (bndr, hlit, ty') ->
     Handler (bndr, handlerlit hlit, OptionUtils.opt_map datatype' ty')
  | Sugartypes.Foreign (bndr, n1, n2, n3, ty') ->
     Foreign (bndr, n1, n2, n3, datatype' ty')
  | Sugartypes.QualifiedImport names ->
     QualifiedImport names
  | Sugartypes.Type (n, tvs, ty') ->
     Type (n, tvs, datatype' ty')
  | Sugartypes.Infix ->
     Infix
  | Sugartypes.Exp p ->
     Exp (phrase p)
  | Sugartypes.Module (n, binds) ->
     Module (n, List.map binding binds)
  | Sugartypes.AlienBlock (n1, n2, binds) ->
     AlienBlock (n1, n2, List.map (fun (bndr, ty) -> (bndr, datatype' ty))
                                  binds)
  and binding {WithPos.Legacy.node; pos} =
    {WithPos.Legacy.node=bindingnode node; pos}
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

(*
(* Why does ConcreteSyntaxError take an
   unresolved position and yet
   PatternDuplicateNameError and
   RedundantPatternMatch take resolved positions?
*)
exception ConcreteSyntaxError of (string * position)
exception PatternDuplicateNameError of (SourceCode.pos * string)
exception RedundantPatternMatch of SourceCode.pos

let tabstr : tyvar list * phrasenode -> phrasenode = fun (tyvars, e) ->
  match tyvars with
    | [] -> e
    | _  -> TAbstr (Types.box_quantifiers tyvars, with_dummy_pos e)

let tappl : phrasenode * tyarg list -> phrasenode = fun (e, tys) ->
  match tys with
    | [] -> e
    | _  -> TAppl (with_dummy_pos e, tys)

module Freevars =
struct
  open Utility
  open StringSet

  let union_map f = union_all -<- List.map f
  let option_map f = opt_app f empty

  let rec pattern ({node; _} : Pattern.with_pos) : StringSet.t =
    let open Pattern in
    match node with
    | Any
    | Nil
    | Constant _
    | Negative _            -> empty
    | Tuple ps
    | List ps               -> union_map pattern ps
    | Cons (p1, p2)         -> union (pattern p1) (pattern p2)
    | Variant (_, popt)     -> option_map pattern popt
    | Effect (_, ps, kopt)  -> union (union_map pattern ps) (pattern kopt)
    | Record (fields, popt) ->
       union (option_map pattern popt)
         (union_map (snd ->- pattern) fields)
    | Variable bndr         -> singleton (name_of_binder bndr)
    | As (bndr, pat)        -> add (name_of_binder bndr) (pattern pat)
    | HasType (pat, _)      -> pattern pat


  let rec formlet_bound ({node; _} : phrase) : StringSet.t = match node with
    | Xml (_, _, _, children) -> union_map formlet_bound children
    | FormBinding (_, pat) -> pattern pat
    | _ -> empty

  let rec phrase (p : phrase) : StringSet.t =
    let p = p.node in
    match p with
    | Var v -> singleton v
    | Section (Section.Name n) -> singleton n

    | Constant _
    | TextNode _
    | Section (Section.Minus|Section.FloatMinus|Section.Project _) -> empty

    | Spawn (_, _, p, _)
    | TAbstr (_, p)
    | TAppl (p, _)
    | FormBinding (p, _)
    | Projection (p, _)
    | Page p
    | PagePlacement p
    | Upcast (p, _, _)
    | Select (_, p)
    | TypeAnnotation (p, _) -> phrase p

    | ListLit (ps, _)
    | TupleLit ps -> union_map phrase ps

    | LensLit (l, _) -> phrase l
    (* this should be converted to `LensLit during typeSugar *)
    | LensFunDepsLit _ -> assert false
    | LensKeysLit (l, _, _) -> phrase l
    | LensSelectLit (l, _, _) -> phrase l
    | LensDropLit (l, _, _, _, _) -> phrase l
    | LensJoinLit (l1, l2, _, _, _, _) -> union_all [phrase l1; phrase l2]

    | LensGetLit (l, _) -> phrase l
    | LensPutLit (l, data, _) -> union_all [phrase l; phrase data]

    | Query (None, p, _) -> phrase p
    | Query (Some (limit, offset), p, _) ->
       union_all [phrase limit; phrase offset; phrase p]

    | Escape (v, p) -> diff (phrase p) (singleton (name_of_binder v))
    | FormletPlacement (p1, p2, p3)
    | Conditional (p1, p2, p3) -> union_map phrase [p1;p2;p3]
    | Block b -> block b
    | InfixAppl ((_, BinaryOp.Name n), p1, p2) ->
       union (singleton n) (union_map phrase [p1;p2])
    | InfixAppl (_, p1, p2) -> union_map phrase [p1;p2]
    | RangeLit (p1, p2) -> union_map phrase [p1;p2]
    | Regex r -> regex r
    | UnaryAppl (_, p) -> phrase p
    | FnAppl (p, ps) -> union_map phrase (p::ps)
    | RecordLit (fields, p) ->
        union (union_map (snd ->- phrase) fields)
          (option_map phrase p)
    | With (p, fields) ->
        union (union_map (snd ->- phrase) fields)
          (phrase p)
    | ConstructorLit (_, popt, _) -> option_map phrase popt
    | DatabaseLit (p, (popt1, popt2)) ->
        union_all [phrase p; option_map phrase popt1; option_map phrase popt2]
    | DBInsert (p1, _labels, p2, popt) ->
        union_all [phrase p1; phrase p2; option_map phrase popt]
    | TableLit (p1, _, _, _, p2) -> union (phrase p1) (phrase p2)
    | Xml (_, attrs, attrexp, children) ->
        union_all
          [union_map (snd ->- union_map phrase) attrs;
           option_map phrase attrexp;
           union_map phrase children]
    | Formlet (xml, yields) ->
        let binds = formlet_bound xml in
          union (phrase xml) (diff (phrase yields) binds)
    | HandlerLit hnlit -> handlerlit hnlit
    | FunLit (_, _, fnlit, _) -> funlit fnlit
    | Iteration (generators, body, where, orderby) ->
        let xs = union_map (function
                             | List (_, source)
                             | Table (_, source) -> phrase source) generators in
        let pat_bound = union_map (function
                                  | List (pat, _)
                                  | Table (pat, _) -> pattern pat) generators in
          union_all [xs;
                     diff (phrase body) pat_bound;
                     diff (option_map phrase where) pat_bound;
                     diff (option_map phrase orderby) pat_bound]
    | Handle { sh_expr = e; sh_effect_cases = eff_cases;
               sh_value_cases = val_cases; sh_descr = descr } ->
       let params_bound =
         option_map
           (fun params -> union_map (snd ->- pattern) params.shp_bindings)
           descr.shd_params
       in
       union_all [phrase e;
                  union_map case eff_cases;
                  union_map case val_cases;
                  diff (option_map (fun params -> union_map (fst ->- phrase)
                                                    params.shp_bindings)
                          descr.shd_params) params_bound]
    | Switch (p, cases, _)
    | Offer (p, cases, _) -> union (phrase p) (union_map case cases)
    | CP cp -> cp_phrase cp
    | Receive (cases, _) -> union_map case cases
    | DBDelete (pat, p, where) ->
        union (phrase p)
          (diff (option_map phrase where)
             (pattern pat))
    | DBUpdate (pat, from, where, fields) ->
        let pat_bound = pattern pat in
          union_all [phrase from;
                     diff (option_map phrase where) pat_bound;
                     diff (union_map (snd ->- phrase) fields) pat_bound]
    | DoOperation (_, ps, _) -> union_map phrase ps
    | QualifiedVar _ -> empty
    | TryInOtherwise (p1, pat, p2, p3, _ty) ->
       union (union_map phrase [p1; p2; p3]) (pattern pat)
    | Raise -> empty
  and binding ({node = binding; _}: binding)
      : StringSet.t (* vars bound in the pattern *)
      * StringSet.t (* free vars in the rhs *) =
    match binding with
    | Val (pat, (_, rhs), _, _) -> pattern pat, phrase rhs
    | Handler (bndr, hnlit, _) ->
       let name = singleton (name_of_binder bndr) in
       name, (diff (handlerlit hnlit) name)
    | Fun (bndr, _, (_, fn), _, _) ->
       let name = singleton (name_of_binder bndr) in
       name, (diff (funlit fn) name)
    | Funs funs ->
        let names, rhss =
          List.fold_right
            (fun (bndr, _, (_, rhs), _, _, _) (names, rhss) ->
               (add (name_of_binder bndr) names, rhs::rhss))
            funs
            (empty, []) in
          names, union_map (fun rhs -> diff (funlit rhs) names) rhss
    | Foreign (bndr, _, _, _, _) -> singleton (name_of_binder bndr), empty
    | QualifiedImport _
    | Type _
    | Infix -> empty, empty
    | Exp p -> empty, phrase p
    | AlienBlock (_, _, decls) ->
        let bound_foreigns =
          List.fold_left (fun acc (bndr, _) ->
              StringSet.add (name_of_binder bndr) acc)
            (StringSet.empty) decls in
        bound_foreigns, empty
        (* TODO: this needs to be implemented *)
    | Module _ -> failwith "Freevars for modules not implemented yet"
  and funlit (args, body : funlit) : StringSet.t =
    diff (phrase body) (union_map (union_map pattern) args)
  and handlerlit (_, m, cases, params : handlerlit) : StringSet.t =
    union_all [diff (union_map case cases)
                 (option_map (union_map (union_map pattern)) params); pattern m]
  and block (binds, expr : binding list * phrase) : StringSet.t =
    ListLabels.fold_right binds ~init:(phrase expr)
      ~f:(fun bind bodyfree ->
            let patbound, exprfree = binding bind in
              union exprfree (diff bodyfree patbound))
  and case (pat, body) : StringSet.t = diff (phrase body) (pattern pat)
  and regex = function
    | Range _
    | Simply _
    | Any
    | StartAnchor
    | EndAnchor
    | Quote _ -> empty
    | Seq rs -> union_map regex rs
    | Alternate (r1, r2) -> union (regex r1) (regex r2)
    | Group r
    | Repeat (_, r) -> regex r
    | Splice p -> phrase p
    | Replace (r, Literal _) -> regex r
    | Replace (r, SpliceExpr p) -> union (regex r) (phrase p)
  and cp_phrase {node = p; _ } = match p with
    | CPUnquote e -> block e
    | CPGrab ((c, _t), Some bndr, p) ->
      union (singleton c) (diff (cp_phrase p) (singleton (name_of_binder bndr)))
    | CPGrab ((c, _t), None, p) -> union (singleton c) (cp_phrase p)
    | CPGive ((c, _t), e, p) -> union (singleton c) (union (option_map phrase e)
                                                           (cp_phrase p))
    | CPGiveNothing bndr -> singleton (name_of_binder bndr)
    | CPSelect (bndr, _label, p) ->
      union (singleton (name_of_binder bndr)) (cp_phrase p)
    | CPOffer (bndr, cases) ->
      union (singleton (name_of_binder bndr))
            (union_map (fun (_label, p) -> cp_phrase p) cases)
    | CPLink (bndr1, bndr2) ->
      union (singleton (name_of_binder bndr1))
            (singleton (name_of_binder bndr2))
    | CPComp (bndr, left, right) ->
       diff (union (cp_phrase left) (cp_phrase right))
            (singleton (name_of_binder bndr))
end
*)
