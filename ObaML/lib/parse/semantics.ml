module WrapBuilder (W : Ast.Wrap) = struct
  module type T = sig
    val wrap : 'a -> Navi.Location.t -> 'a W.t
  end
end

let to_loc (begin_p, end_p) = Navi.Location.from_lex_poss begin_p end_p

module Id : sig
  type t = Ast.Id.t

  val id : string -> t
end = struct
  include Ast.Id

  let id s = Id s
end

module Ty
    (EW : Ast.Wrap)
    (DW : Ast.Wrap)
    (EWB : WrapBuilder(EW).T)
    (DWB : WrapBuilder(DW).T) : sig
  module Var : sig
    type t = Ast.Ty(EW)(DW).Var.t

    val tyvar : string -> t
  end

  module Expr : sig
    type t = Ast.Ty(EW)(DW).Expr.t
    type 'a w

    val wrap : 'a -> Lexing.position * Lexing.position -> 'a w
    val tvar : Var.t w -> t
    val tarrow : t w -> t w -> t
    val ttuple : t w list -> t
    val tconstr : t w list -> Id.t w -> t
  end

  module Decl : sig
    type constr
    type representation
    type t = Ast.Ty(EW)(DW).Decl.t
    type 'a w

    val wrap : 'a -> Lexing.position * Lexing.position -> 'a w
    val constr : Id.t w -> Expr.t w option -> constr
    val fromtyexpr : Expr.t w -> representation
    val fromconstrs : constr w list -> representation
    val decl : Id.t w -> Var.t w list -> representation w -> t
  end
end = struct
  module AstTy = Ast.Ty (EW) (DW)

  module Var = struct
    include AstTy.Var

    let tyvar s = TyVar s
  end

  module Expr = struct
    include AstTy.Expr

    type 'a w = 'a W.t

    let wrap t loc = to_loc loc |> EWB.wrap t
    let tvar var = TVar var
    let tarrow fst snd = TArrow (fst, snd)
    let ttuple wts = TTuple wts
    let tconstr wts id = TConstr (wts, id)
  end

  module Decl = struct
    include AstTy.Decl

    type 'a w = 'a W.t

    let wrap t loc = to_loc loc |> DWB.wrap t
    let constr name args = { name; args }
    let fromtyexpr tyexpr = FromTyExpr tyexpr
    let fromconstrs cs = FromConstrs cs
    let decl name params representation = { name; params; representation }
  end
end

module Constant (CW : Ast.Wrap) (WB : WrapBuilder(CW).T) : sig
  type t = Ast.Constant(CW).t
  type 'a w

  val wrap : 'a -> Lexing.position * Lexing.position -> 'a w
  val cint : int w -> t
  val cstring : string w -> t
end = struct
  include Ast.Constant (CW)

  type 'a w = 'a W.t

  let wrap t loc = to_loc loc |> WB.wrap t
  let cint i = CInt i
  let cstring s = CString s
end

module Make
    (SW : Ast.Wrap)
    (SIW : Ast.Wrap)
    (EW : Ast.Wrap)
    (PW : Ast.Wrap)
    (CW : Ast.Wrap)
    (TEW : Ast.Wrap)
    (TDW : Ast.Wrap)
    (SWB : WrapBuilder(SW).T)
    (SIWB : WrapBuilder(SIW).T)
    (EWB : WrapBuilder(EW).T)
    (PWB : WrapBuilder(PW).T)
    (CWB : WrapBuilder(CW).T)
    (TEWB : WrapBuilder(TEW).T)
    (TDWB : WrapBuilder(TDW).T) =
struct
  module Id = Id
  module Ty = Ty (TEW) (TDW) (TEWB) (TDWB)
  module Constant = Constant (CW) (CWB)
end

(* module Semantics : sig
   module Id : sig
   type t

   val equal : t -> t -> bool
   val compare : t -> t -> int
   val pp : Format.formatter -> t -> unit
   val show : t -> string
   val id : string -> t
   end

   module Ty : sig
   module Var : sig
   type t

   val equal : t -> t -> bool
   val compare : t -> t -> int
   val pp : Format.formatter -> t -> unit
   val show : t -> string
   val tyvar : string -> t
   end

   module Expr : sig
   type t

   val equal : t -> t -> bool
   val pp : Format.formatter -> t -> unit
   val show : t -> string

   type 'a w

   val wrap : 'a -> Lexing.position * Lexing.position -> 'a w
   val tvar : Var.t w -> t
   val tarrow : t w -> t w -> t
   val ttuple : t w list -> t
   val tconstr : t w list -> Id.t w -> t
   end

   module Decl : sig
   type constr

   val equal_constr : constr -> constr -> bool
   val pp_constr : Format.formatter -> constr -> unit
   val show_constr : constr -> string

   type representation

   val equal_representation : representation -> representation -> bool
   val pp_representation : Format.formatter -> representation -> unit
   val show_representation : representation -> string

   type t

   val equal : t -> t -> bool
   val pp : Format.formatter -> t -> unit
   val show : t -> string

   type 'a w

   val wrap : 'a -> Lexing.position * Lexing.position -> 'a w
   val constr : Id.t w -> Expr.t w option -> constr
   val fromtyexpr : Expr.t w -> representation
   val fromconstrs : constr w list -> representation
   val decl : Id.t w -> Var.t w list -> representation w -> t
   end
   end

   module Constant : sig
   type t

   val equal : t -> t -> bool
   val pp : Format.formatter -> t -> unit
   val show : t -> string

   type 'a w

   val wrap : 'a -> Lexing.position * Lexing.position -> 'a w
   val cint : int w -> t
   val cstring : string w -> t
   end

   (* module Pattern : sig
   type t
   end *)

   (* module Expr : sig
   type rec_flag
   type value_binding
   type case
   type t
   end *)

   (* module StructureItem : sig
   type t
   end *)

   (* module Structure : sig
   type t
   end *)
   end = struct
   module Id = IdSemantics
   module Ty = TySemantics
   module Constant = ConstantSemantics

   module Pattern = struct
   type t
   end

   module Expr = struct
   type rec_flag
   type value_binding
   type case
   type t
   end

   module StructureItem = struct
   type t
   end

   module Structure = struct
   type t
   end
   end *)
