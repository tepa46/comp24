(********************************Wrap********************************)

module type Wrap = sig
  type 'a t

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
  val escape : 'a t -> 'a
end

module Fiction : Wrap = struct
  type 'a t = 'a [@@deriving eq, show { with_path = false }]

  let escape x = x
end

(********************************Identifier********************************)

module Id = struct
  type t = Id of string [@@deriving eq, ord, show { with_path = false }]
end

(********************************Types********************************)

module Ty (ExprW : Wrap) (DeclW : Wrap) = struct
  module Var = struct
    type t = TyVar of string [@@deriving eq, ord, show { with_path = false }]
  end

  module Expr = struct
    module W = ExprW

    type t =
      | TVar of Var.t W.t (** ['a] *)
      | TArrow of t W.t * t W.t (** ['a --> 'b] *)
      | TTuple of t W.t list (** ['a * 'b * 'c] *)
      | TConstr of t W.t list * Id.t W.t (** [int] | ['a list] | [('a, 'b) Result] *)
    [@@deriving eq, show { with_path = false }]
  end

  module Decl = struct
    module W = DeclW

    (** [Leaf] | [Node of 'a] *)
    type constr =
      { name : Id.t W.t
      ; args : Expr.t W.t
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

(********************************Constans********************************)

module Constant (W : Wrap) = struct
  type t =
    | CInt of int W.t (** [52] *)
    | CString of string W.t (** [ "hola" ] *)
  [@@deriving eq, show { with_path = false }]
end

(********************************Patterns********************************)

module Pattern (W : Wrap) (CW : Wrap) (TyExprW : Wrap) (TyDeclW : Wrap) = struct
  module Constant = Constant (CW)
  module Ty = Ty (TyExprW) (TyDeclW)

  type t =
    | PAny (** [_] *)
    | PVar of Id.t W.t (** [variable] *)
    | PConst of Constant.t W.t (** [52] | ["hola"]*)
    | PTuple of t W.t list (** [first, second, third] *)
    | PConstruct of Id.t W.t * t W.t option (** [Tree(left, right)] | [None] *)
    | PType of t W.t * Ty.Expr.t W.t (** [x : int] *)
    | POr of t W.t * t W.t (** [First | Second | Third] *)
  [@@deriving eq, show { with_path = false }]
end

(********************************Expressions********************************)

module Expr (W : Wrap) (PW : Wrap) (CW : Wrap) (TyExprW : Wrap) (TyDeclW : Wrap) = struct
  module Pattern = Pattern (PW) (CW) (TyExprW) (TyDeclW)
  module Constant = Pattern.Constant
  module Ty = Pattern.Ty

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
    | ELet of rec_flag * value_binding W.t list * t W.t
    (** [let rec p1 = e1 and p2 = e2 in e] *)
    | EFun of Pattern.t W.t list * Ty.Expr.t W.t option * fun_body W.t
    (** [fun x y z -> x + y + z] | [function 1 -> true | _ -> false] *)
    | EApply of t W.t * t W.t (** [f x] *)
    | EMatch of t W.t * case W.t list (** [match x with | 1 | 2 -> true | _ -> false] *)
    | ETuple of t W.t list (** [fst, snd, trd] *)
    | EConstruct of Id.t W.t * t W.t option (** [Some 42] *)
    | EIfThenElse of t W.t * t W.t * t W.t option (** [if true then false else true] *)
  [@@deriving eq, show { with_path = false }]
end

(********************************StructureItems********************************)

module StructureItem
    (W : Wrap)
    (EW : Wrap)
    (PW : Wrap)
    (CW : Wrap)
    (TyExprW : Wrap)
    (TyDeclW : Wrap) =
struct
  module Expr = Expr (EW) (PW) (CW) (TyExprW) (TyDeclW)
  module Pattern = Expr.Pattern
  module Constant = Expr.Constant
  module Ty = Expr.Ty

  type t =
    | SITyDecl of Ty.Decl.t W.t (** [type bool = true | false] *)
    | SIExpr of Expr.t W.t (** [5 + 2] *)
    | SILet of Expr.rec_flag * Expr.value_binding W.t list (** [let rec f = None;;] *)
  [@@deriving eq, show { with_path = false }]
end

(********************************Ast********************************)

module Ast
    (W : Wrap)
    (SIW : Wrap)
    (EW : Wrap)
    (PW : Wrap)
    (CW : Wrap)
    (TyExprW : Wrap)
    (TyDeclW : Wrap) =
struct
  module StructureItem = StructureItem (SIW) (EW) (PW) (CW) (TyExprW) (TyDeclW)
  module Expr = StructureItem.Expr
  module Pattern = StructureItem.Pattern
  module Constant = StructureItem.Constant
  module Ty = StructureItem.Ty

  type t = StructureItem.t W.t list [@@deriving eq, show { with_path = false }]
end

(********************************Examples********************************)

(** let fun with arguments: [let f x y = e1 in e2]
    processed as [let f = fun x y -> e1 in e2] as
    [ELet(
      Nonrecursive,
name: [(PVar(f), EFun(
            args: [PVar(x); PVar(y)],
            type: None,
                  FunBody(EVar(e1))))],
      e2)] *)

(** let pattern: [let (((fst, snd) : int * int): int * int): int * int = e1 in e2]
    [ELet(
      Nonrecursive,
      [(PType(
          PType(
            PType(
              PTuple([PVar fst; PVar snd]), 
              TTuple([int; int])),
            TTuple([int; int])),
          TTuple([int; int])), e1)],
      e2)]*)

(** typed let fun with arguments: [let f x y: string = e1 in e2]
    processed as [let f = fun x y: string -> e1 in e2]
    [ELet(
      Nonrecursive,
name: [(PVar(f), EFun(
            args: [PVar(x); PVar(y)],
            type: Some(TConstr([], string)),
                  FunBody(EVar(e1))))],
      e2)] *)

(** typed let with function: [let f : func_type = function p1 -> e1 | p2 -> e2 in e3]
    [ELet(
      Nonrecursive,
      [(PVar(f), EFun(
                  [],
            type: Some(TConstr([], func_type))
                  FunCases([(p1, e1); (p2, e2)])))],
      e3)] *)
