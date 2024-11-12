let to_loc (begin_p, end_p) = Navi.Location.from_lex_poss begin_p end_p

module Id
    (SW : Wrap.T)
    (SIW : Wrap.T)
    (EW : Wrap.T)
    (PW : Wrap.T)
    (CW : Wrap.T)
    (TEW : Wrap.T)
    (TDW : Wrap.T) : sig
  type t = Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Id.t

  val id : string -> t
end = struct
  module Ast = Ast.Make (SW) (SIW) (EW) (PW) (CW) (TEW) (TDW)
  include Ast.Id

  let id s = Id s
end

module Ty
    (SW : Wrap.T)
    (SIW : Wrap.T)
    (EW : Wrap.T)
    (PW : Wrap.T)
    (CW : Wrap.T)
    (TEW : Wrap.T)
    (TDW : Wrap.T)
    (EWB : Wrap.Builder(TEW).T)
    (DWB : Wrap.Builder(TDW).T) : sig
  module Var : sig
    type t = Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Ty.Var.t

    val tyvar : string -> t
  end

  module Expr : sig
    type t = Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Ty.Expr.t
    type 'a w

    val wrap : 'a -> Lexing.position * Lexing.position -> 'a w
    val tvar : Var.t w -> t
    val tarrow : t w -> t w -> t
    val ttuple : t w list -> t
    val tconstr : t w list -> Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Id.t w -> t
  end

  module Decl : sig
    type constr
    type representation
    type t = Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Ty.Decl.t
    type 'a w

    val wrap : 'a -> Lexing.position * Lexing.position -> 'a w

    val constr
      :  Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Id.t w
      -> Expr.t w option
      -> constr

    val fromtyexpr : Expr.t w -> representation
    val fromconstrs : constr w list -> representation

    val decl
      :  Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Id.t w
      -> Var.t w list
      -> representation w
      -> t
  end
end = struct
  module Ast = Ast.Make (SW) (SIW) (EW) (PW) (CW) (TEW) (TDW)

  module Var = struct
    include Ast.Ty.Var

    let tyvar s = TyVar s
  end

  module Expr = struct
    include Ast.Ty.Expr

    type 'a w = 'a TEW.t

    let wrap t loc = to_loc loc |> EWB.wrap t
    let tvar var = TVar var
    let tarrow fst snd = TArrow (fst, snd)
    let ttuple wts = TTuple wts
    let tconstr wts id = TConstr (wts, id)
  end

  module Decl = struct
    include Ast.Ty.Decl

    type 'a w = 'a TDW.t

    let wrap t loc = to_loc loc |> DWB.wrap t
    let constr name args = { name; args }
    let fromtyexpr tyexpr = FromTyExpr tyexpr
    let fromconstrs cs = FromConstrs cs
    let decl name params representation = { name; params; representation }
  end
end

module Constant
    (SW : Wrap.T)
    (SIW : Wrap.T)
    (EW : Wrap.T)
    (PW : Wrap.T)
    (CW : Wrap.T)
    (TEW : Wrap.T)
    (TDW : Wrap.T)
    (WB : Wrap.Builder(CW).T) : sig
  type t = Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Constant.t
  type 'a w

  val wrap : 'a -> Lexing.position * Lexing.position -> 'a w
  val cint : int w -> t
  val cstring : string w -> t
end = struct
  module Ast = Ast.Make (SW) (SIW) (EW) (PW) (CW) (TEW) (TDW)
  include Ast.Constant

  type 'a w = 'a CW.t

  let wrap t loc = to_loc loc |> WB.wrap t
  let cint i = CInt i
  let cstring s = CString s
end

module Pattern
    (SW : Wrap.T)
    (SIW : Wrap.T)
    (EW : Wrap.T)
    (PW : Wrap.T)
    (CW : Wrap.T)
    (TEW : Wrap.T)
    (TDW : Wrap.T)
    (WB : Wrap.Builder(PW).T) : sig
  type t = Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Pattern.t
  type 'a w

  val wrap : 'a -> Lexing.position * Lexing.position -> 'a w
  val pany : t
  val pvar : Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Id.t w -> t
  val pconst : Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Constant.t w -> t
  val ptuple : t w list -> t
  val pconstruct : Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Id.t w -> t w option -> t
  val ptype : t w -> Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Ty.Expr.t w -> t
  val por : t w -> t w -> t
end = struct
  module Ast = Ast.Make (SW) (SIW) (EW) (PW) (CW) (TEW) (TDW)
  include Ast.Pattern

  type 'a w = 'a PW.t

  let wrap t loc = to_loc loc |> WB.wrap t
  let pany = PAny
  let pvar v = PVar v
  let pconst c = PConst c
  let ptuple ps = PTuple ps
  let pconstruct n p_opt = PConstruct (n, p_opt)
  let ptype p t = PType (p, t)
  let por p1 p2 = POr (p1, p2)
end

module Expr
    (SW : Wrap.T)
    (SIW : Wrap.T)
    (EW : Wrap.T)
    (PW : Wrap.T)
    (CW : Wrap.T)
    (TEW : Wrap.T)
    (TDW : Wrap.T)
    (WB : Wrap.Builder(EW).T) : sig
  type rec_flag = Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Expr.rec_flag
  type value_binding = Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Expr.value_binding
  type case = Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Expr.case
  type fun_body = Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Expr.fun_body
  type t = Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Expr.t
  type 'a w

  val wrap : 'a -> Lexing.position * Lexing.position -> 'a w
  val recursive : rec_flag
  val nonrecursive : rec_flag

  val value_binding
    :  Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Pattern.t w
    -> t w
    -> value_binding

  val case : Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Pattern.t w -> t w -> value_binding
  val funbody : t w -> fun_body
  val funcases : case w list -> fun_body
  val evar : Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Id.t w -> t
  val econst : Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Constant.t w -> t
  val elet : rec_flag w -> value_binding w list -> t w -> t

  val efun
    :  Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Pattern.t w list
    -> Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Ty.Expr.t w option
    -> fun_body w
    -> t

  val eapply : t w -> t w list -> t
  val ematch : t w -> case w list -> t
  val etuple : t w list -> t
  val econstruct : Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Id.t w -> t w option -> t
  val eifthenelse : t w -> t w -> t w option -> t
end = struct
  module Ast = Ast.Make (SW) (SIW) (EW) (PW) (CW) (TEW) (TDW)
  include Ast.Expr

  type 'a w = 'a EW.t

  let wrap t loc = to_loc loc |> WB.wrap t
  let recursive = Recursive
  let nonrecursive = Nonrecursive
  let value_binding p t : value_binding = p, t
  let case p t : case = p, t
  let funbody t = FunBody t
  let funcases cs = FunCases cs
  let evar v = EVar v
  let econst c = EConst c
  let elet rf vbs t = ELet (rf, vbs, t)
  let efun ps t_opt fb = EFun (ps, t_opt, fb)
  let eapply t ts = EApply (t, ts)
  let ematch t cs = EMatch (t, cs)
  let etuple ts = ETuple ts
  let econstruct n t_opt = EConstruct (n, t_opt)
  let eifthenelse i t e = EIfThenElse (i, t, e)
end

module StructureItem
    (SW : Wrap.T)
    (SIW : Wrap.T)
    (EW : Wrap.T)
    (PW : Wrap.T)
    (CW : Wrap.T)
    (TEW : Wrap.T)
    (TDW : Wrap.T)
    (WB : Wrap.Builder(SIW).T) : sig
  type t = Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).StructureItem.t
  type 'a w

  val wrap : 'a -> Lexing.position * Lexing.position -> 'a w
  val sitydecl : Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Ty.Decl.t w -> t
  val siexpr : Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Expr.t w -> t

  val silet
    :  Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Expr.rec_flag w
    -> Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Expr.value_binding w list
    -> t
end = struct
  module Ast = Ast.Make (SW) (SIW) (EW) (PW) (CW) (TEW) (TDW)
  include Ast.StructureItem

  type 'a w = 'a SIW.t

  let wrap t loc = to_loc loc |> WB.wrap t
  let sitydecl td = SITyDecl td
  let siexpr e = SIExpr e
  let silet rf vbs = SILet (rf, vbs)
end

module Structure
    (SW : Wrap.T)
    (SIW : Wrap.T)
    (EW : Wrap.T)
    (PW : Wrap.T)
    (CW : Wrap.T)
    (TEW : Wrap.T)
    (TDW : Wrap.T)
    (WB : Wrap.Builder(SW).T) : sig
  type t = Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Structure.t
  type 'a w

  val wrap : 'a -> Lexing.position * Lexing.position -> 'a w
  val structure : t -> t
end = struct
  module Ast = Ast.Make (SW) (SIW) (EW) (PW) (CW) (TEW) (TDW)
  include Ast.Structure

  type 'a w = 'a SW.t

  let wrap t loc = to_loc loc |> WB.wrap t
  let structure sis : t = sis
end

module Make
    (SW : Wrap.T)
    (SIW : Wrap.T)
    (EW : Wrap.T)
    (PW : Wrap.T)
    (CW : Wrap.T)
    (TEW : Wrap.T)
    (TDW : Wrap.T)
    (SWB : Wrap.Builder(SW).T)
    (SIWB : Wrap.Builder(SIW).T)
    (EWB : Wrap.Builder(EW).T)
    (PWB : Wrap.Builder(PW).T)
    (CWB : Wrap.Builder(CW).T)
    (TEWB : Wrap.Builder(TEW).T)
    (TDWB : Wrap.Builder(TDW).T) =
struct
  module Id = Id (SW) (SIW) (EW) (PW) (CW) (TEW) (TDW)
  module Ty = Ty (SW) (SIW) (EW) (PW) (CW) (TEW) (TDW) (TEWB) (TDWB)
  module Constant = Constant (SW) (SIW) (EW) (PW) (CW) (TEW) (TDW) (CWB)
  module Pattern = Pattern (SW) (SIW) (EW) (PW) (CW) (TEW) (TDW) (PWB)
  module Expr = Expr (SW) (SIW) (EW) (PW) (CW) (TEW) (TDW) (EWB)
  module StructureItem = StructureItem (SW) (SIW) (EW) (PW) (CW) (TEW) (TDW) (SIWB)
  module Structure = Structure (SW) (SIW) (EW) (PW) (CW) (TEW) (TDW) (SWB)
end
