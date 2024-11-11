%parameter<Semantics : sig
  module Id : sig
    type t

    val id : string -> t
  end

  module Ty : sig
    module Var : sig
      type t

      val tyvar : string -> t
    end

    module Expr : sig
      type t
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
      type t
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
end>

%{
    open Semantics
%}

%start <Semantics.Id.t> parse_id

%start <Semantics.Constant.t> parse_constant

%start <Semantics.Ty.Var.t> parse_ty_var

%start <Semantics.Ty.Expr.t> parse_ty_expr

%start <Semantics.Ty.Decl.t> parse_ty_decl

%start <Semantics.Ty.Decl.t> parse

%%

parse_id : identifier EOF { $1 }

parse_constant : constant EOF { $1 }

parse_ty_var : ty_var EOF { $1 }

parse_ty_expr : ty_expr EOF { $1 }

parse_ty_decl : ty_decl EOF { $1 }

parse : ty_decl EOF { $1 }

(***************************Constant***************************)

constant :
    | INT { Constant.cint @@ Constant.wrap $1 $loc }
    | STRING { let s, loc = $1 in
               Constant.cstring @@ Constant.wrap s loc }

(***************************Id***************************)

lowerid : LOWERID { Id.id $1 }

upperid : UPPERID { Id.id $1 }

name :
    | LOWERID { $1 }
    | UPPERID { $1 }

identifier : name { Id.id $1 }

(***************************Ty.Var***************************)

ty_var : QUOTE name { Ty.Var.tyvar $2 }

(***************************Ty.Expr***************************)

ty_expr :
    | molecular_ty_expr { $1 }
    | w_molecular_ty_expr ARROW w_ty_expr { Ty.Expr.tarrow $1 $3 }

%inline w_ty_expr : ty_expr { Ty.Expr.wrap $1 $loc }

molecular_ty_expr :
    | atomic_ty_expr { $1 }
    | separated_nontrivial_list(ASTERISK, w_atomic_ty_expr) { Ty.Expr.ttuple $1 }

%inline w_molecular_ty_expr : molecular_ty_expr { Ty.Expr.wrap $1 $loc }

atomic_ty_expr :
    | parens(ty_expr) { $1 }
    | ty_var { Ty.Expr.tvar @@ Ty.Expr.wrap $1 $loc }
    | tconstr_params identifier { Ty.Expr.wrap $2 $loc($2) |> Ty.Expr.tconstr $1 }

%inline w_atomic_ty_expr : atomic_ty_expr { Ty.Expr.wrap $1 $loc }

tconstr_params :
    | parens(separated_nontrivial_list(COMMA, w_ty_expr)) { $1 }
    | w_atomic_ty_expr { [ $1 ] }
    | { [] }

(***************************Ty.Decl***************************)

ty_decl : TYPE ty_decl_params lowerid w_ty_decl_representation { Ty.Decl.decl (Ty.Decl.wrap $3 $loc($3)) $2 $4 }

ty_decl_params :
    | parens(separated_nonempty_list(COMMA, ty_var { Ty.Decl.wrap $1 $loc })) { $1 }
    | ty_var { [ Ty.Decl.wrap $1 $loc ] }
    | { [] }

%inline w_ty_decl_representation : ty_decl_representation { Ty.Decl.wrap $1 $loc }

ty_decl_representation :
    | EQUAL ty_expr { Ty.Decl.fromtyexpr @@ Ty.Decl.wrap $2 $loc($2) }
    | EQUAL BAR separated_nonempty_list(BAR, w_ty_decl_constr ) { Ty.Decl.fromconstrs $3 }
    | EQUAL separated_nonempty_list(BAR, w_ty_decl_constr ) { Ty.Decl.fromconstrs $2 }
    | { Ty.Decl.fromconstrs [] }

%inline w_ty_decl_constr : ty_decl_constr { Ty.Decl.wrap $1 $loc }

ty_decl_constr :
    | ty_decl_constr_name OF ty_expr { Some (Ty.Decl.wrap $3 $loc($3)) |> Ty.Decl.constr $1 }
    | ty_decl_constr_name { Ty.Decl.constr $1 None }

ty_decl_constr_name : constr_name { Ty.Decl.wrap $1 $loc }

constr_name :
    | upperid { $1 }
    | constr_true { $1 }
    | constr_false { $1 }
    | constr_coloncolon { $1 }
    | constr_brackets { $1 }
    | constr_parens { $1 }

(***************************helpers***************************)

%inline constr_true : TRUE { Id.id "true" }

%inline constr_false : FALSE { Id.id "false" }

%inline constr_coloncolon : parens(COLONCOLON) { Id.id "::" }

%inline constr_brackets : LBRACKET RBRACKET { Id.id "[]" }

%inline constr_parens : LPAREN RPAREN { Id.id "()" }

%inline parens(x) : LPAREN x RPAREN { $2 }

%inline separated_nontrivial_list(separator, x) : x separator separated_nonempty_list(separator, x) { $1 :: $3 }
