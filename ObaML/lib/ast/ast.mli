module Make : functor
    (SW : Wrap.T)
    (SIW : Wrap.T)
    (EW : Wrap.T)
    (PW : Wrap.T)
    (CW : Wrap.T)
    (TyExprW : Wrap.T)
    (TyDeclW : Wrap.T)
    -> sig
  module Id : sig
    type t = Id of string

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Constant : sig
    type t =
      | CInt of int CW.t
      | CString of string CW.t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Ty : sig
    module Var : sig
      type t = TyVar of string

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val pp : Format.formatter -> t -> unit
      val show : t -> string
    end

    module Expr : sig
      type t =
        | TVar of Var.t TyExprW.t
        | TArrow of t TyExprW.t * t TyExprW.t
        | TTuple of t TyExprW.t list
        | TConstr of t TyExprW.t list * Id.t TyExprW.t

      val equal : t -> t -> bool
      val pp : Format.formatter -> t -> unit
      val show : t -> string
    end

    module Decl : sig
      type constr =
        { name : Id.t TyDeclW.t
        ; args : Expr.t TyDeclW.t option
        }

      val equal_constr : constr -> constr -> bool
      val pp_constr : Format.formatter -> constr -> unit
      val show_constr : constr -> string

      type representation =
        | FromTyExpr of Expr.t TyDeclW.t
        | FromConstrs of constr TyDeclW.t list

      val equal_representation : representation -> representation -> bool
      val pp_representation : Format.formatter -> representation -> unit
      val show_representation : representation -> string

      type t =
        { name : Id.t TyDeclW.t
        ; params : Var.t TyDeclW.t list
        ; representation : representation TyDeclW.t
        }

      val equal : t -> t -> bool
      val pp : Format.formatter -> t -> unit
      val show : t -> string
    end
  end

  module Pattern : sig
    type t =
      | PAny
      | PVar of Id.t PW.t
      | PConst of Constant.t PW.t
      | PTuple of t PW.t list
      | PConstruct of Id.t PW.t * t PW.t option
      | PType of t PW.t * Ty.Expr.t PW.t
      | POr of t PW.t * t PW.t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Expr : sig
    type rec_flag =
      | Recursive
      | Nonrecursive

    val equal_rec_flag : rec_flag -> rec_flag -> bool
    val pp_rec_flag : Format.formatter -> rec_flag -> unit
    val show_rec_flag : rec_flag -> string

    type value_binding = Pattern.t EW.t * t EW.t
    and case = Pattern.t EW.t * t EW.t

    and fun_body =
      | FunBody of t EW.t
      | FunCases of case EW.t list

    and t =
      | EVar of Id.t EW.t
      | EConst of Constant.t EW.t
      | ELet of rec_flag EW.t * value_binding EW.t list * t EW.t
      | EFun of Pattern.t EW.t list * Ty.Expr.t EW.t option * fun_body EW.t
      | EApply of t EW.t * t EW.t list
      | EMatch of t EW.t * case EW.t list
      | ETuple of t EW.t list
      | EConstruct of Id.t EW.t * t EW.t option
      | EIfThenElse of t EW.t * t EW.t * t EW.t option

    val equal_value_binding : value_binding -> value_binding -> bool
    val equal_case : case -> case -> bool
    val equal_fun_body : fun_body -> fun_body -> bool
    val equal : t -> t -> bool
    val pp_value_binding : Format.formatter -> value_binding -> unit
    val show_value_binding : value_binding -> string
    val pp_case : Format.formatter -> case -> unit
    val show_case : case -> string
    val pp_fun_body : Format.formatter -> fun_body -> unit
    val show_fun_body : fun_body -> string
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module StructureItem : sig
    type t =
      | SITyDecl of Ty.Decl.t SIW.t
      | SIExpr of Expr.t SIW.t
      | SILet of Expr.rec_flag SIW.t * Expr.value_binding SIW.t list

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Structure : sig
    type t = StructureItem.t SW.t list

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end
end
