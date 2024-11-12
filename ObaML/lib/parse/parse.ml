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
  module Semantics =
    Semantics.Make (SW) (SIW) (EW) (PW) (CW) (TEW) (TDW) (SWB) (SIWB) (EWB) (PWB) (CWB)
      (TEWB)
      (TDWB)

  module Parse = struct
    module Parser = Parser.Make (Semantics)
    module I = Parser.MenhirInterpreter

    exception SyntaxError of string option

    let rec loop lexbuf checkpoint =
      match checkpoint with
      | I.InputNeeded _ ->
        let token = Lexer.token lexbuf in
        let startpos = lexbuf.lex_start_p in
        let endpos = lexbuf.lex_curr_p in
        let checkpoint = I.offer checkpoint (token, startpos, endpos) in
        loop lexbuf checkpoint
      | I.Shifting _ | I.AboutToReduce _ ->
        let checkpoint = I.resume checkpoint in
        loop lexbuf checkpoint
      | I.HandlingError _ ->
        (* let line, pos = get_pos lexbuf in *)
        (* let err = get_err env in *)
        raise @@ SyntaxError (Some "HandlingError")
      | I.Accepted v -> v
      | I.Rejected -> raise @@ SyntaxError (Some "Rejected")
    ;;

    let parse parse lexbuf =
      try
        let v = loop lexbuf (parse lexbuf.lex_curr_p) in
        Ok v
      with
      | SyntaxError msg_opt -> Error msg_opt
    ;;

    let parse_id = parse Parser.Incremental.parse_id
    let id_from_string s = parse_id @@ Lexing.from_string s
    let parse_constant = parse Parser.Incremental.parse_constant
    let constant_from_string s = parse_constant @@ Lexing.from_string s
    let parse_ty_var = parse Parser.Incremental.parse_ty_var
    let ty_var_from_string s = parse_ty_var @@ Lexing.from_string s
    let parse_ty_expr = parse Parser.Incremental.parse_ty_expr
    let ty_expr_from_string s = parse_ty_expr @@ Lexing.from_string s
    let parse_ty_decl = parse Parser.Incremental.parse_ty_decl
    let ty_decl_from_string s = parse_ty_decl @@ Lexing.from_string s
  end

  let id_from_string = Parse.id_from_string
  let constant_from_string = Parse.constant_from_string
  let ty_var_from_string = Parse.ty_var_from_string
  let ty_expr_from_string = Parse.ty_expr_from_string
  let ty_decl_from_string = Parse.ty_decl_from_string
end

(***************************Tests***************************)

module Fiction : Wrap.T with type 'a t = 'a = struct
  type 'a t = 'a [@@deriving eq, show { with_path = false }]
end

module FictionBuilder : Wrap.Builder(Fiction).T = struct
  let wrap v _ = v
end

module Parse =
  Make (Fiction) (Fiction) (Fiction) (Fiction) (Fiction) (Fiction) (Fiction)
    (FictionBuilder)
    (FictionBuilder)
    (FictionBuilder)
    (FictionBuilder)
    (FictionBuilder)
    (FictionBuilder)
    (FictionBuilder)

module Ast =
  Ast.Make (Fiction) (Fiction) (Fiction) (Fiction) (Fiction) (Fiction) (Fiction)

let print_result val_pp = function
  | Ok v -> Stdlib.Format.printf "%a" val_pp v
  | Error _ -> Stdlib.Printf.printf "Error"
;;

(***************************Id*Parser*Tests***************************)

let parse_and_print s = Parse.id_from_string s |> print_result Ast.Id.pp

let%expect_test "Id test" =
  parse_and_print {| variable |};
  [%expect {| (Id "variable") |}]
;;

let%expect_test "Id start lower test" =
  parse_and_print {| hEaD_52_tAiL_ |};
  [%expect {| (Id "hEaD_52_tAiL_") |}]
;;

let%expect_test "Id start upper test" =
  parse_and_print {| HeAd_52_TaIl |};
  [%expect {| (Id "HeAd_52_TaIl") |}]
;;

(***************************Constant*Parser*Tests***************************)

let parse_and_print s = Parse.constant_from_string s |> print_result Ast.Constant.pp

let%expect_test "Constant int test" =
  parse_and_print {| 52 |};
  [%expect {| (CInt 52) |}]
;;

let%expect_test "Constant int leading zero test" =
  parse_and_print {| 052 |};
  [%expect {| (CInt 52) |}]
;;

let%expect_test "Constant int leading plus test" =
  parse_and_print {| +52 |};
  [%expect {| (CInt 52) |}]
;;

let%expect_test "Constant int leading minus test" =
  parse_and_print {| -52 |};
  [%expect {| (CInt -52) |}]
;;

let%expect_test "Constant int leading sign zero test" =
  parse_and_print {| +052 |};
  [%expect {| (CInt 52) |}]
;;

let%expect_test "Constant string empty test" =
  parse_and_print {| "" |};
  [%expect {| (CString "") |}]
;;

let%expect_test "Constant string letters test" =
  parse_and_print {| " lEtTeRs " |};
  [%expect {| (CString " lEtTeRs ") |}]
;;

let%expect_test "Constant string digits test" =
  parse_and_print {| " 0123456789 " |};
  [%expect {| (CString " 0123456789 ") |}]
;;

let%expect_test "Constant string math symbols test" =
  parse_and_print {| " + - * / = < > ( ) & | ^ " |};
  [%expect {| (CString " + - * / = < > ( ) & | ^ ") |}]
;;

let%expect_test "Constant string grammar symbols test" =
  parse_and_print {| " . , ? ! ` : ; ' " |};
  [%expect {| (CString " . , ? ! ` : ; ' ") |}]
;;

let%expect_test "Constant string special symbols test" =
  parse_and_print {| " ~ [ ] { } # $ _ @ " |};
  [%expect {| (CString " ~ [ ] { } # $ _ @ ") |}]
;;

(***************************Ty.Var***************************)

let parse_and_print s = Parse.ty_var_from_string s |> print_result Ast.Ty.Var.pp

let%expect_test "Ty.Var upper test" =
  parse_and_print {| 'Upper52_ |};
  [%expect {| (TyVar "Upper52_") |}]
;;

let%expect_test "Ty.Var lower test" =
  parse_and_print {| 'lower52_ |};
  [%expect {| (TyVar "lower52_") |}]
;;

(***************************Ty.Expr***************************)

let parse_and_print s = Parse.ty_expr_from_string s |> print_result Ast.Ty.Expr.pp

let%expect_test "Ty.Expr var test" =
  parse_and_print {| 'a52_ |};
  [%expect {| (TVar (TyVar "a52_")) |}]
;;

let%expect_test "Ty.Expr arrow test" =
  parse_and_print {| ('a -> 'b) -> ('c -> 'd)|};
  [%expect
    {|
    (TArrow ((TArrow ((TVar (TyVar "a")), (TVar (TyVar "b")))),
       (TArrow ((TVar (TyVar "c")), (TVar (TyVar "d")))))) |}]
;;

let%expect_test "Ty.Expr tuple test" =
  parse_and_print {| 'a * ('b -> 'c) * 'd |};
  [%expect
    {|
    (TTuple
       [(TVar (TyVar "a")); (TArrow ((TVar (TyVar "b")), (TVar (TyVar "c"))));
         (TVar (TyVar "d"))]) |}]
;;

let%expect_test "Ty.Expr arrow tuple test" =
  parse_and_print {| 'a * 'b -> 'c * 'd |};
  [%expect
    {|
    (TArrow ((TTuple [(TVar (TyVar "a")); (TVar (TyVar "b"))]),
       (TTuple [(TVar (TyVar "c")); (TVar (TyVar "d"))]))) |}]
;;

let%expect_test "Ty.Expr simple constr test" =
  parse_and_print {| int |};
  [%expect {| (TConstr ([], (Id "int"))) |}]
;;

let%expect_test "Ty.Expr single parametr constr test" =
  parse_and_print {| 'a list |};
  [%expect {| (TConstr ([(TVar (TyVar "a"))], (Id "list"))) |}]
;;

let%expect_test "Ty.Expr parametrized constr test" =
  parse_and_print {| ('a * string, 'a * 'b -> 'c * 'd ) Result |};
  [%expect
    {|
    (TConstr (
       [(TTuple [(TVar (TyVar "a")); (TConstr ([], (Id "string")))]);
         (TArrow ((TTuple [(TVar (TyVar "a")); (TVar (TyVar "b"))]),
            (TTuple [(TVar (TyVar "c")); (TVar (TyVar "d"))])))
         ],
       (Id "Result"))) |}]
;;

(***************************Ty.Decl***************************)

let parse_and_print s = Parse.ty_decl_from_string s |> print_result Ast.Ty.Decl.pp

let%expect_test "Ty.Decl simple from ty expr test" =
  parse_and_print {| type int = string |};
  [%expect
    {|
    { name = (Id "int"); params = [];
      representation = (FromTyExpr (TConstr ([], (Id "string")))) } |}]
;;

let%expect_test "Ty.Decl single parametr from ty expr test" =
  parse_and_print {| type 'a list = ('a, 'a) Result |};
  [%expect
    {|
    { name = (Id "list"); params = [(TyVar "a")];
      representation =
      (FromTyExpr
         (TConstr ([(TVar (TyVar "a")); (TVar (TyVar "a"))], (Id "Result"))))
      } |}]
;;

let%expect_test "Ty.Decl parametrized from ty expr test" =
  parse_and_print {| type ('a, 'b) list = ('a, 'b) Result |};
  [%expect
    {|
    { name = (Id "list"); params = [(TyVar "a"); (TyVar "b")];
      representation =
      (FromTyExpr
         (TConstr ([(TVar (TyVar "a")); (TVar (TyVar "b"))], (Id "Result"))))
      } |}]
;;

let%expect_test "Ty.Decl empty type test" =
  parse_and_print {| type int |};
  [%expect
    {|
    { name = (Id "int"); params = []; representation = (FromConstrs []) } |}]
;;

let%expect_test "Ty.Decl empty constructors test" =
  parse_and_print {| type ab = A | B |};
  [%expect
    {|
    { name = (Id "ab"); params = [];
      representation =
      (FromConstrs
         [{ name = (Id "A"); args = None }; { name = (Id "B"); args = None }])
      } |}]
;;

let%expect_test "Ty.Decl empty constructors leading bar test" =
  parse_and_print {| type ab = | A | B |};
  [%expect
    {|
    { name = (Id "ab"); params = [];
      representation =
      (FromConstrs
         [{ name = (Id "A"); args = None }; { name = (Id "B"); args = None }])
      } |}]
;;

let%expect_test "Ty.Decl single constructor test" =
  parse_and_print {| type a = | A of int |};
  [%expect
    {|
    { name = (Id "a"); params = [];
      representation =
      (FromConstrs
         [{ name = (Id "A"); args = (Some (TConstr ([], (Id "int")))) }])
      } |}]
;;

let%expect_test "Ty.Decl Result test" =
  parse_and_print {| type ('a, 'b) result = Ok of 'a | Error of 'b |};
  [%expect
    {|
    { name = (Id "result"); params = [(TyVar "a"); (TyVar "b")];
      representation =
      (FromConstrs
         [{ name = (Id "Ok"); args = (Some (TVar (TyVar "a"))) };
           { name = (Id "Error"); args = (Some (TVar (TyVar "b"))) }])
      } |}]
;;

let%expect_test "Ty.Decl option test" =
  parse_and_print {| type 'a option = Some of 'a | None |};
  [%expect
    {|
    { name = (Id "option"); params = [(TyVar "a")];
      representation =
      (FromConstrs
         [{ name = (Id "Some"); args = (Some (TVar (TyVar "a"))) };
           { name = (Id "None"); args = None }])
      } |}]
;;

let%expect_test "Ty.Decl bool test" =
  parse_and_print {| type bool = true | false |};
  [%expect
    {|
    { name = (Id "bool"); params = [];
      representation =
      (FromConstrs
         [{ name = (Id "true"); args = None };
           { name = (Id "false"); args = None }])
      } |}]
;;

let%expect_test "Ty.Decl list test" =
  parse_and_print {| type ('a) list = (::) of 'a * 'a list | [] |};
  [%expect
    {|
    { name = (Id "list"); params = [(TyVar "a")];
      representation =
      (FromConstrs
         [{ name = (Id "::");
            args =
            (Some (TTuple
                     [(TVar (TyVar "a"));
                       (TConstr ([(TVar (TyVar "a"))], (Id "list")))]))
            };
           { name = (Id "[]"); args = None }])
      } |}]
;;

let%expect_test "Ty.Decl overriding special constructors test" =
  parse_and_print
    {| type t = | true of int | false of int | (::) of string | [] of string | () of bool |};
  [%expect
    {|
    { name = (Id "t"); params = [];
      representation =
      (FromConstrs
         [{ name = (Id "true"); args = (Some (TConstr ([], (Id "int")))) };
           { name = (Id "false"); args = (Some (TConstr ([], (Id "int")))) };
           { name = (Id "::"); args = (Some (TConstr ([], (Id "string")))) };
           { name = (Id "[]"); args = (Some (TConstr ([], (Id "string")))) };
           { name = (Id "()"); args = (Some (TConstr ([], (Id "bool")))) }])
      } |}]
;;
