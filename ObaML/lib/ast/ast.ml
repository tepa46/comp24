(********************************Identifier********************************)

module Id = struct
  type t = Id of string [@@deriving eq, ord, show { with_path = false }]
end

(********************************Types********************************)

module Ty (ExprW : Wrap.T) (DeclW : Wrap.T) = struct
  module Id = Id

  module Var = struct
    type t = TyVar of string [@@deriving eq, ord, show { with_path = false }]
  end

  module Expr = struct
    module W = ExprW

    type t =
      | TVar of Var.t W.t (** ['a] *)
      | TArrow of t W.t * t W.t (** ['a -> 'b] *)
      | TTuple of t W.t list (** ['a * 'b * 'c], invariant: [list length >= 2] *)
      | TConstr of t W.t list * Id.t W.t (** [int] | ['a list] | [('a, 'b) Result] *)
    [@@deriving eq, show { with_path = false }]
  end

  module Decl = struct
    module W = DeclW

    (** [Leaf] | [Node of 'a] *)
    type constr =
      { name : Id.t W.t
      ; args : Expr.t W.t option
      }
    [@@deriving eq, show { with_path = false }]

    type representation =
      | FromTyExpr of Expr.t W.t (** [int] | [int * bool] *)
      | FromConstrs of constr W.t list (** [Some of 'a] *)
    [@@deriving eq, show { with_path = false }]

    (** [('a, 'b) name = A of 'a | B of 'b] *)
    type t =
      { name : Id.t W.t (** starts with lowercase *)
      ; params : Var.t W.t list (** [('a, 'b)] *)
      ; representation : representation W.t (** [A of 'a | B of 'b] *)
      }
    [@@deriving eq, show { with_path = false }]
  end
end

(********************************Constants********************************)

module Constant (W : Wrap.T) = struct
  module W = W

  type t =
    | CInt of int W.t (** [52] *)
    | CString of string W.t (** [ "hola" ] *)
  [@@deriving eq, show { with_path = false }]
end

(********************************Patterns********************************)

module Pattern (W : Wrap.T) (CW : Wrap.T) (TyExprW : Wrap.T) (TyDeclW : Wrap.T) = struct
  module Id = Id
  module Constant = Constant (CW)
  module Ty = Ty (TyExprW) (TyDeclW)
  module W = W

  type t =
    | PAny (** [_] *)
    | PVar of Id.t W.t (** [variable] *)
    | PConst of Constant.t W.t (** [52] | ["hola"]*)
    | PTuple of t W.t list (** [first, second, third], invariant: [list length >= 2] *)
    | PConstruct of Id.t W.t * t W.t option (** [Tree(left, right)] | [None] *)
    | PType of t W.t * Ty.Expr.t W.t (** [x : int] *)
    | POr of t W.t * t W.t (** [First | Second | Third] *)
  [@@deriving eq, show { with_path = false }]
end

(********************************Expressions********************************)

module Expr (W : Wrap.T) (PW : Wrap.T) (CW : Wrap.T) (TyExprW : Wrap.T) (TyDeclW : Wrap.T) =
struct
  module Id = Id
  module Constant = Constant (CW)
  module Ty = Ty (TyExprW) (TyDeclW)
  module Pattern = Pattern (PW) (CW) (TyExprW) (TyDeclW)
  module W = W

  type rec_flag =
    | Recursive (** with [rec] modifier *)
    | Nonrecursive (** without [rec] modifier *)
  [@@deriving eq, show { with_path = false }]

  (** [let a = e;;] | [let fst, snd = pair in fst + snd] *)
  type value_binding = Pattern.t W.t * t W.t [@@deriving eq, show { with_path = false }]

  (** [Tree(left, right) -> left, right] *)
  and case = Pattern.t W.t * t W.t [@@deriving eq, show { with_path = false }]

  and fun_body =
    | FunBody of t W.t (** [fun x y -> x + y] *)
    | FunCases of case W.t list
  [@@deriving eq, show { with_path = false }]

  and t =
    | EVar of Id.t W.t (** [x] *)
    | EConst of Constant.t W.t (** [52] | ["hola"] *)
    | ELet of rec_flag W.t * value_binding W.t list * t W.t
    (** [let rec p1 = e1 and p2 = e2 in e] *)
    | EFun of Pattern.t W.t list * Ty.Expr.t W.t option * fun_body W.t
    (** [fun x y z -> x + y + z] | [function 1 -> true | _ -> false] *)
    | EApply of t W.t * t W.t list (** [f x y], invariant: [list length > 0] *)
    | EMatch of t W.t * case W.t list (** [match x with | 1 | 2 -> true | _ -> false] *)
    | ETuple of t W.t list (** [fst, snd, trd] *)
    | EConstruct of Id.t W.t * t W.t option (** [Some 42] *)
    | EIfThenElse of t W.t * t W.t * t W.t option (** [if true then false else true] *)
  [@@deriving eq, show { with_path = false }]
end

(********************************StructureItems********************************)

module StructureItem
    (W : Wrap.T)
    (EW : Wrap.T)
    (PW : Wrap.T)
    (CW : Wrap.T)
    (TyExprW : Wrap.T)
    (TyDeclW : Wrap.T) =
struct
  module Ty = Ty (TyExprW) (TyDeclW)
  module Expr = Expr (EW) (PW) (CW) (TyExprW) (TyDeclW)
  module W = W

  type t =
    | SITyDecl of Ty.Decl.t W.t (** [type bool = true | false] *)
    | SIExpr of Expr.t W.t (** [5 + 2] *)
    | SILet of Expr.rec_flag W.t * Expr.value_binding W.t list (** [let rec f = None;;] *)
  [@@deriving eq, show { with_path = false }]
end

(********************************Structure********************************)

module Structure
    (W : Wrap.T)
    (SIW : Wrap.T)
    (EW : Wrap.T)
    (PW : Wrap.T)
    (CW : Wrap.T)
    (TyExprW : Wrap.T)
    (TyDeclW : Wrap.T) =
struct
  module StructureItem = StructureItem (SIW) (EW) (PW) (CW) (TyExprW) (TyDeclW)

  type t = StructureItem.t W.t list [@@deriving eq, show { with_path = false }]
end

(********************************Make********************************)

module Make
    (SW : Wrap.T)
    (SIW : Wrap.T)
    (EW : Wrap.T)
    (PW : Wrap.T)
    (CW : Wrap.T)
    (TyExprW : Wrap.T)
    (TyDeclW : Wrap.T) : sig
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
end = struct
  module Id = Id
  module Constant = Constant (CW)
  module Ty = Ty (TyExprW) (TyDeclW)
  module Pattern = Pattern (PW) (CW) (TyExprW) (TyDeclW)
  module Expr = Expr (EW) (PW) (CW) (TyExprW) (TyDeclW)
  module StructureItem = StructureItem (SIW) (EW) (PW) (CW) (TyExprW) (TyDeclW)
  module Structure = Structure (SW) (SIW) (EW) (PW) (CW) (TyExprW) (TyDeclW)
end
