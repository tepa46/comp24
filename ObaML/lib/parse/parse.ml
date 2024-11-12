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
  [%expect
    {|
    (CInt
       { data = 52;
         location =
         { begin_pos = { line = 1; col = 1 }; end_pos = { line = 1; col = 3 } } }) |}]
;;

let%expect_test "Constant int leading zero test" =
  parse_and_print {| 052 |};
  [%expect
    {|
    (CInt
       { data = 52;
         location =
         { begin_pos = { line = 1; col = 1 }; end_pos = { line = 1; col = 4 } } }) |}]
;;

let%expect_test "Constant int leading plus test" =
  parse_and_print {| +52 |};
  [%expect
    {|
    (CInt
       { data = 52;
         location =
         { begin_pos = { line = 1; col = 1 }; end_pos = { line = 1; col = 4 } } }) |}]
;;

let%expect_test "Constant int leading minus test" =
  parse_and_print {| -52 |};
  [%expect
    {|
    (CInt
       { data = -52;
         location =
         { begin_pos = { line = 1; col = 1 }; end_pos = { line = 1; col = 4 } } }) |}]
;;

let%expect_test "Constant int leading sign zero test" =
  parse_and_print {| +052 |};
  [%expect
    {|
    (CInt
       { data = 52;
         location =
         { begin_pos = { line = 1; col = 1 }; end_pos = { line = 1; col = 5 } } }) |}]
;;

let%expect_test "Constant string empty test" =
  parse_and_print {| "" |};
  [%expect
    {|
    (CString
       { data = "";
         location =
         { begin_pos = { line = 1; col = 2 }; end_pos = { line = 1; col = 2 } } }) |}]
;;

let%expect_test "Constant string letters test" =
  parse_and_print {| " lEtTeRs " |};
  [%expect
    {|
    (CString
       { data = " lEtTeRs ";
         location =
         { begin_pos = { line = 1; col = 2 }; end_pos = { line = 1; col = 11 } }
         }) |}]
;;

let%expect_test "Constant string digits test" =
  parse_and_print {| " 0123456789 " |};
  [%expect
    {|
    (CString
       { data = " 0123456789 ";
         location =
         { begin_pos = { line = 1; col = 2 }; end_pos = { line = 1; col = 14 } }
         }) |}]
;;

let%expect_test "Constant string math symbols test" =
  parse_and_print {| " + - * / = < > ( ) & | ^ " |};
  [%expect
    {|
    (CString
       { data = " + - * / = < > ( ) & | ^ ";
         location =
         { begin_pos = { line = 1; col = 2 }; end_pos = { line = 1; col = 27 } }
         }) |}]
;;

let%expect_test "Constant string grammar symbols test" =
  parse_and_print {| " . , ? ! ` : ; ' " |};
  [%expect
    {|
    (CString
       { data = " . , ? ! ` : ; ' ";
         location =
         { begin_pos = { line = 1; col = 2 }; end_pos = { line = 1; col = 19 } }
         }) |}]
;;

let%expect_test "Constant string special symbols test" =
  parse_and_print {| " ~ [ ] { } # $ _ @ " |};
  [%expect
    {|
    (CString
       { data = " ~ [ ] { } # $ _ @ ";
         location =
         { begin_pos = { line = 1; col = 2 }; end_pos = { line = 1; col = 21 } }
         }) |}]
;;

(***************************Ty.Var***************************)

let parse_and_print s = Parse.ty_var_from_string s |> print_result Ast.Ty.Var.pp

let%expect_test "Ty.Var upper test" =
  parse_and_print {| 'Upper52_ |};
  [%expect {|
    (TyVar "Upper52_") |}]
;;

let%expect_test "Ty.Var lower test" =
  parse_and_print {| 'lower52_ |};
  [%expect {|
    (TyVar "lower52_") |}]
;;

(***************************Ty.Expr***************************)

let parse_and_print s = Parse.ty_expr_from_string s |> print_result Ast.Ty.Expr.pp

let%expect_test "Ty.Expr var test" =
  parse_and_print {| 'a52_ |};
  [%expect
    {|
    (TVar
       { data = (TyVar "a52_");
         location =
         { begin_pos = { line = 1; col = 1 }; end_pos = { line = 1; col = 6 } } }) |}]
;;

let%expect_test "Ty.Expr arrow test" =
  parse_and_print {| ('a -> 'b) -> ('c -> 'd)|};
  [%expect
    {|
    (TArrow (
       { data =
         (TArrow (
            { data =
              (TVar
                 { data = (TyVar "a");
                   location =
                   { begin_pos = { line = 1; col = 2 };
                     end_pos = { line = 1; col = 4 } }
                   });
              location =
              { begin_pos = { line = 1; col = 2 };
                end_pos = { line = 1; col = 4 } }
              },
            { data =
              (TVar
                 { data = (TyVar "b");
                   location =
                   { begin_pos = { line = 1; col = 8 };
                     end_pos = { line = 1; col = 10 } }
                   });
              location =
              { begin_pos = { line = 1; col = 8 };
                end_pos = { line = 1; col = 10 } }
              }
            ));
         location =
         { begin_pos = { line = 1; col = 1 }; end_pos = { line = 1; col = 11 } }
         },
       { data =
         (TArrow (
            { data =
              (TVar
                 { data = (TyVar "c");
                   location =
                   { begin_pos = { line = 1; col = 16 };
                     end_pos = { line = 1; col = 18 } }
                   });
              location =
              { begin_pos = { line = 1; col = 16 };
                end_pos = { line = 1; col = 18 } }
              },
            { data =
              (TVar
                 { data = (TyVar "d");
                   location =
                   { begin_pos = { line = 1; col = 22 };
                     end_pos = { line = 1; col = 24 } }
                   });
              location =
              { begin_pos = { line = 1; col = 22 };
                end_pos = { line = 1; col = 24 } }
              }
            ));
         location =
         { begin_pos = { line = 1; col = 15 }; end_pos = { line = 1; col = 25 } }
         }
       )) |}]
;;

let%expect_test "Ty.Expr tuple test" =
  parse_and_print {| 'a * ('b -> 'c) * 'd |};
  [%expect
    {|
    (TTuple
       [{ data =
          (TVar
             { data = (TyVar "a");
               location =
               { begin_pos = { line = 1; col = 1 };
                 end_pos = { line = 1; col = 3 } }
               });
          location =
          { begin_pos = { line = 1; col = 1 }; end_pos = { line = 1; col = 3 } }
          };
         { data =
           (TArrow (
              { data =
                (TVar
                   { data = (TyVar "b");
                     location =
                     { begin_pos = { line = 1; col = 7 };
                       end_pos = { line = 1; col = 9 } }
                     });
                location =
                { begin_pos = { line = 1; col = 7 };
                  end_pos = { line = 1; col = 9 } }
                },
              { data =
                (TVar
                   { data = (TyVar "c");
                     location =
                     { begin_pos = { line = 1; col = 13 };
                       end_pos = { line = 1; col = 15 } }
                     });
                location =
                { begin_pos = { line = 1; col = 13 };
                  end_pos = { line = 1; col = 15 } }
                }
              ));
           location =
           { begin_pos = { line = 1; col = 6 }; end_pos = { line = 1; col = 16 }
             }
           };
         { data =
           (TVar
              { data = (TyVar "d");
                location =
                { begin_pos = { line = 1; col = 19 };
                  end_pos = { line = 1; col = 21 } }
                });
           location =
           { begin_pos = { line = 1; col = 19 }; end_pos = { line = 1; col = 21 }
             }
           }
         ]) |}]
;;

let%expect_test "Ty.Expr arrow tuple test" =
  parse_and_print {| 'a * 'b -> 'c * 'd |};
  [%expect
    {|
    (TArrow (
       { data =
         (TTuple
            [{ data =
               (TVar
                  { data = (TyVar "a");
                    location =
                    { begin_pos = { line = 1; col = 1 };
                      end_pos = { line = 1; col = 3 } }
                    });
               location =
               { begin_pos = { line = 1; col = 1 };
                 end_pos = { line = 1; col = 3 } }
               };
              { data =
                (TVar
                   { data = (TyVar "b");
                     location =
                     { begin_pos = { line = 1; col = 6 };
                       end_pos = { line = 1; col = 8 } }
                     });
                location =
                { begin_pos = { line = 1; col = 6 };
                  end_pos = { line = 1; col = 8 } }
                }
              ]);
         location =
         { begin_pos = { line = 1; col = 1 }; end_pos = { line = 1; col = 8 } } },
       { data =
         (TTuple
            [{ data =
               (TVar
                  { data = (TyVar "c");
                    location =
                    { begin_pos = { line = 1; col = 12 };
                      end_pos = { line = 1; col = 14 } }
                    });
               location =
               { begin_pos = { line = 1; col = 12 };
                 end_pos = { line = 1; col = 14 } }
               };
              { data =
                (TVar
                   { data = (TyVar "d");
                     location =
                     { begin_pos = { line = 1; col = 17 };
                       end_pos = { line = 1; col = 19 } }
                     });
                location =
                { begin_pos = { line = 1; col = 17 };
                  end_pos = { line = 1; col = 19 } }
                }
              ]);
         location =
         { begin_pos = { line = 1; col = 12 }; end_pos = { line = 1; col = 19 } }
         }
       )) |}]
;;

let%expect_test "Ty.Expr simple constr test" =
  parse_and_print {| int |};
  [%expect
    {|
    (TConstr ([],
       { data = (Id "int");
         location =
         { begin_pos = { line = 1; col = 1 }; end_pos = { line = 1; col = 4 } } }
       )) |}]
;;

let%expect_test "Ty.Expr single parametr constr test" =
  parse_and_print {| 'a list |};
  [%expect
    {|
    (TConstr (
       [{ data =
          (TVar
             { data = (TyVar "a");
               location =
               { begin_pos = { line = 1; col = 1 };
                 end_pos = { line = 1; col = 3 } }
               });
          location =
          { begin_pos = { line = 1; col = 1 }; end_pos = { line = 1; col = 3 } }
          }
         ],
       { data = (Id "list");
         location =
         { begin_pos = { line = 1; col = 4 }; end_pos = { line = 1; col = 8 } } }
       )) |}]
;;

let%expect_test "Ty.Expr parametrized constr test" =
  parse_and_print {| ('a * string, 'a * 'b -> 'c * 'd ) Result |};
  [%expect
    {|
    (TConstr (
       [{ data =
          (TTuple
             [{ data =
                (TVar
                   { data = (TyVar "a");
                     location =
                     { begin_pos = { line = 1; col = 2 };
                       end_pos = { line = 1; col = 4 } }
                     });
                location =
                { begin_pos = { line = 1; col = 2 };
                  end_pos = { line = 1; col = 4 } }
                };
               { data =
                 (TConstr ([],
                    { data = (Id "string");
                      location =
                      { begin_pos = { line = 1; col = 7 };
                        end_pos = { line = 1; col = 13 } }
                      }
                    ));
                 location =
                 { begin_pos = { line = 1; col = 6 };
                   end_pos = { line = 1; col = 13 } }
                 }
               ]);
          location =
          { begin_pos = { line = 1; col = 2 }; end_pos = { line = 1; col = 13 } }
          };
         { data =
           (TArrow (
              { data =
                (TTuple
                   [{ data =
                      (TVar
                         { data = (TyVar "a");
                           location =
                           { begin_pos = { line = 1; col = 15 };
                             end_pos = { line = 1; col = 17 } }
                           });
                      location =
                      { begin_pos = { line = 1; col = 15 };
                        end_pos = { line = 1; col = 17 } }
                      };
                     { data =
                       (TVar
                          { data = (TyVar "b");
                            location =
                            { begin_pos = { line = 1; col = 20 };
                              end_pos = { line = 1; col = 22 } }
                            });
                       location =
                       { begin_pos = { line = 1; col = 20 };
                         end_pos = { line = 1; col = 22 } }
                       }
                     ]);
                location =
                { begin_pos = { line = 1; col = 15 };
                  end_pos = { line = 1; col = 22 } }
                },
              { data =
                (TTuple
                   [{ data =
                      (TVar
                         { data = (TyVar "c");
                           location =
                           { begin_pos = { line = 1; col = 26 };
                             end_pos = { line = 1; col = 28 } }
                           });
                      location =
                      { begin_pos = { line = 1; col = 26 };
                        end_pos = { line = 1; col = 28 } }
                      };
                     { data =
                       (TVar
                          { data = (TyVar "d");
                            location =
                            { begin_pos = { line = 1; col = 31 };
                              end_pos = { line = 1; col = 33 } }
                            });
                       location =
                       { begin_pos = { line = 1; col = 31 };
                         end_pos = { line = 1; col = 33 } }
                       }
                     ]);
                location =
                { begin_pos = { line = 1; col = 26 };
                  end_pos = { line = 1; col = 33 } }
                }
              ));
           location =
           { begin_pos = { line = 1; col = 15 }; end_pos = { line = 1; col = 33 }
             }
           }
         ],
       { data = (Id "Result");
         location =
         { begin_pos = { line = 1; col = 36 }; end_pos = { line = 1; col = 42 } }
         }
       )) |}]
;;

(***************************Ty.Decl***************************)

let parse_and_print s = Parse.ty_decl_from_string s |> print_result Ast.Ty.Decl.pp

let%expect_test "Ty.Decl simple from ty expr test" =
  parse_and_print {| type int = string |};
  [%expect
    {|
    { name =
      { data = (Id "int");
        location =
        { begin_pos = { line = 1; col = 6 }; end_pos = { line = 1; col = 9 } } };
      params = [];
      representation =
      { data =
        (FromTyExpr
           { data =
             (TConstr ([],
                { data = (Id "string");
                  location =
                  { begin_pos = { line = 1; col = 12 };
                    end_pos = { line = 1; col = 18 } }
                  }
                ));
             location =
             { begin_pos = { line = 1; col = 11 };
               end_pos = { line = 1; col = 18 } }
             });
        location =
        { begin_pos = { line = 1; col = 10 }; end_pos = { line = 1; col = 18 } }
        }
      } |}]
;;

let%expect_test "Ty.Decl single parametr from ty expr test" =
  parse_and_print {| type 'a list = ('a, 'a) Result |};
  [%expect
    {|
    { name =
      { data = (Id "list");
        location =
        { begin_pos = { line = 1; col = 9 }; end_pos = { line = 1; col = 13 } } };
      params =
      [{ data = (TyVar "a");
         location =
         { begin_pos = { line = 1; col = 6 }; end_pos = { line = 1; col = 8 } } }
        ];
      representation =
      { data =
        (FromTyExpr
           { data =
             (TConstr (
                [{ data =
                   (TVar
                      { data = (TyVar "a");
                        location =
                        { begin_pos = { line = 1; col = 17 };
                          end_pos = { line = 1; col = 19 } }
                        });
                   location =
                   { begin_pos = { line = 1; col = 17 };
                     end_pos = { line = 1; col = 19 } }
                   };
                  { data =
                    (TVar
                       { data = (TyVar "a");
                         location =
                         { begin_pos = { line = 1; col = 21 };
                           end_pos = { line = 1; col = 23 } }
                         });
                    location =
                    { begin_pos = { line = 1; col = 21 };
                      end_pos = { line = 1; col = 23 } }
                    }
                  ],
                { data = (Id "Result");
                  location =
                  { begin_pos = { line = 1; col = 25 };
                    end_pos = { line = 1; col = 31 } }
                  }
                ));
             location =
             { begin_pos = { line = 1; col = 16 };
               end_pos = { line = 1; col = 31 } }
             });
        location =
        { begin_pos = { line = 1; col = 14 }; end_pos = { line = 1; col = 31 } }
        }
      } |}]
;;

let%expect_test "Ty.Decl parametrized from ty expr test" =
  parse_and_print {| type ('a, 'b) list = ('a, 'b) Result |};
  [%expect
    {|
    { name =
      { data = (Id "list");
        location =
        { begin_pos = { line = 1; col = 15 }; end_pos = { line = 1; col = 19 } }
        };
      params =
      [{ data = (TyVar "a");
         location =
         { begin_pos = { line = 1; col = 7 }; end_pos = { line = 1; col = 9 } } };
        { data = (TyVar "b");
          location =
          { begin_pos = { line = 1; col = 11 }; end_pos = { line = 1; col = 13 }
            }
          }
        ];
      representation =
      { data =
        (FromTyExpr
           { data =
             (TConstr (
                [{ data =
                   (TVar
                      { data = (TyVar "a");
                        location =
                        { begin_pos = { line = 1; col = 23 };
                          end_pos = { line = 1; col = 25 } }
                        });
                   location =
                   { begin_pos = { line = 1; col = 23 };
                     end_pos = { line = 1; col = 25 } }
                   };
                  { data =
                    (TVar
                       { data = (TyVar "b");
                         location =
                         { begin_pos = { line = 1; col = 27 };
                           end_pos = { line = 1; col = 29 } }
                         });
                    location =
                    { begin_pos = { line = 1; col = 27 };
                      end_pos = { line = 1; col = 29 } }
                    }
                  ],
                { data = (Id "Result");
                  location =
                  { begin_pos = { line = 1; col = 31 };
                    end_pos = { line = 1; col = 37 } }
                  }
                ));
             location =
             { begin_pos = { line = 1; col = 22 };
               end_pos = { line = 1; col = 37 } }
             });
        location =
        { begin_pos = { line = 1; col = 20 }; end_pos = { line = 1; col = 37 } }
        }
      } |}]
;;

let%expect_test "Ty.Decl empty type test" =
  parse_and_print {| type int |};
  [%expect
    {|
    { name =
      { data = (Id "int");
        location =
        { begin_pos = { line = 1; col = 6 }; end_pos = { line = 1; col = 9 } } };
      params = [];
      representation =
      { data = (FromConstrs []);
        location =
        { begin_pos = { line = 1; col = 9 }; end_pos = { line = 1; col = 9 } } }
      } |}]
;;

let%expect_test "Ty.Decl empty constructors test" =
  parse_and_print {| type ab = A | B |};
  [%expect
    {|
    { name =
      { data = (Id "ab");
        location =
        { begin_pos = { line = 1; col = 6 }; end_pos = { line = 1; col = 8 } } };
      params = [];
      representation =
      { data =
        (FromConstrs
           [{ data =
              { name =
                { data = (Id "A");
                  location =
                  { begin_pos = { line = 1; col = 11 };
                    end_pos = { line = 1; col = 12 } }
                  };
                args = None };
              location =
              { begin_pos = { line = 1; col = 11 };
                end_pos = { line = 1; col = 12 } }
              };
             { data =
               { name =
                 { data = (Id "B");
                   location =
                   { begin_pos = { line = 1; col = 15 };
                     end_pos = { line = 1; col = 16 } }
                   };
                 args = None };
               location =
               { begin_pos = { line = 1; col = 15 };
                 end_pos = { line = 1; col = 16 } }
               }
             ]);
        location =
        { begin_pos = { line = 1; col = 9 }; end_pos = { line = 1; col = 16 } } }
      } |}]
;;

let%expect_test "Ty.Decl empty constructors leading bar test" =
  parse_and_print {| type ab = | A | B |};
  [%expect
    {|
    { name =
      { data = (Id "ab");
        location =
        { begin_pos = { line = 1; col = 6 }; end_pos = { line = 1; col = 8 } } };
      params = [];
      representation =
      { data =
        (FromConstrs
           [{ data =
              { name =
                { data = (Id "A");
                  location =
                  { begin_pos = { line = 1; col = 13 };
                    end_pos = { line = 1; col = 14 } }
                  };
                args = None };
              location =
              { begin_pos = { line = 1; col = 13 };
                end_pos = { line = 1; col = 14 } }
              };
             { data =
               { name =
                 { data = (Id "B");
                   location =
                   { begin_pos = { line = 1; col = 17 };
                     end_pos = { line = 1; col = 18 } }
                   };
                 args = None };
               location =
               { begin_pos = { line = 1; col = 17 };
                 end_pos = { line = 1; col = 18 } }
               }
             ]);
        location =
        { begin_pos = { line = 1; col = 9 }; end_pos = { line = 1; col = 18 } } }
      } |}]
;;

let%expect_test "Ty.Decl single constructor test" =
  parse_and_print {| type a = | A of int |};
  [%expect
    {|
    { name =
      { data = (Id "a");
        location =
        { begin_pos = { line = 1; col = 6 }; end_pos = { line = 1; col = 7 } } };
      params = [];
      representation =
      { data =
        (FromConstrs
           [{ data =
              { name =
                { data = (Id "A");
                  location =
                  { begin_pos = { line = 1; col = 12 };
                    end_pos = { line = 1; col = 13 } }
                  };
                args =
                (Some { data =
                        (TConstr ([],
                           { data = (Id "int");
                             location =
                             { begin_pos = { line = 1; col = 17 };
                               end_pos = { line = 1; col = 20 } }
                             }
                           ));
                        location =
                        { begin_pos = { line = 1; col = 16 };
                          end_pos = { line = 1; col = 20 } }
                        })
                };
              location =
              { begin_pos = { line = 1; col = 12 };
                end_pos = { line = 1; col = 20 } }
              }
             ]);
        location =
        { begin_pos = { line = 1; col = 8 }; end_pos = { line = 1; col = 20 } } }
      } |}]
;;

let%expect_test "Ty.Decl Result test" =
  parse_and_print {| type ('a, 'b) result = Ok of 'a | Error of 'b |};
  [%expect
    {|
    { name =
      { data = (Id "result");
        location =
        { begin_pos = { line = 1; col = 15 }; end_pos = { line = 1; col = 21 } }
        };
      params =
      [{ data = (TyVar "a");
         location =
         { begin_pos = { line = 1; col = 7 }; end_pos = { line = 1; col = 9 } } };
        { data = (TyVar "b");
          location =
          { begin_pos = { line = 1; col = 11 }; end_pos = { line = 1; col = 13 }
            }
          }
        ];
      representation =
      { data =
        (FromConstrs
           [{ data =
              { name =
                { data = (Id "Ok");
                  location =
                  { begin_pos = { line = 1; col = 24 };
                    end_pos = { line = 1; col = 26 } }
                  };
                args =
                (Some { data =
                        (TVar
                           { data = (TyVar "a");
                             location =
                             { begin_pos = { line = 1; col = 30 };
                               end_pos = { line = 1; col = 32 } }
                             });
                        location =
                        { begin_pos = { line = 1; col = 30 };
                          end_pos = { line = 1; col = 32 } }
                        })
                };
              location =
              { begin_pos = { line = 1; col = 24 };
                end_pos = { line = 1; col = 32 } }
              };
             { data =
               { name =
                 { data = (Id "Error");
                   location =
                   { begin_pos = { line = 1; col = 35 };
                     end_pos = { line = 1; col = 40 } }
                   };
                 args =
                 (Some { data =
                         (TVar
                            { data = (TyVar "b");
                              location =
                              { begin_pos = { line = 1; col = 44 };
                                end_pos = { line = 1; col = 46 } }
                              });
                         location =
                         { begin_pos = { line = 1; col = 44 };
                           end_pos = { line = 1; col = 46 } }
                         })
                 };
               location =
               { begin_pos = { line = 1; col = 35 };
                 end_pos = { line = 1; col = 46 } }
               }
             ]);
        location =
        { begin_pos = { line = 1; col = 22 }; end_pos = { line = 1; col = 46 } }
        }
      } |}]
;;

let%expect_test "Ty.Decl option test" =
  parse_and_print {| type 'a option = Some of 'a | None |};
  [%expect
    {|
    { name =
      { data = (Id "option");
        location =
        { begin_pos = { line = 1; col = 9 }; end_pos = { line = 1; col = 15 } } };
      params =
      [{ data = (TyVar "a");
         location =
         { begin_pos = { line = 1; col = 6 }; end_pos = { line = 1; col = 8 } } }
        ];
      representation =
      { data =
        (FromConstrs
           [{ data =
              { name =
                { data = (Id "Some");
                  location =
                  { begin_pos = { line = 1; col = 18 };
                    end_pos = { line = 1; col = 22 } }
                  };
                args =
                (Some { data =
                        (TVar
                           { data = (TyVar "a");
                             location =
                             { begin_pos = { line = 1; col = 26 };
                               end_pos = { line = 1; col = 28 } }
                             });
                        location =
                        { begin_pos = { line = 1; col = 26 };
                          end_pos = { line = 1; col = 28 } }
                        })
                };
              location =
              { begin_pos = { line = 1; col = 18 };
                end_pos = { line = 1; col = 28 } }
              };
             { data =
               { name =
                 { data = (Id "None");
                   location =
                   { begin_pos = { line = 1; col = 31 };
                     end_pos = { line = 1; col = 35 } }
                   };
                 args = None };
               location =
               { begin_pos = { line = 1; col = 31 };
                 end_pos = { line = 1; col = 35 } }
               }
             ]);
        location =
        { begin_pos = { line = 1; col = 16 }; end_pos = { line = 1; col = 35 } }
        }
      } |}]
;;

let%expect_test "Ty.Decl bool test" =
  parse_and_print {| type bool = true | false |};
  [%expect
    {|
    { name =
      { data = (Id "bool");
        location =
        { begin_pos = { line = 1; col = 6 }; end_pos = { line = 1; col = 10 } } };
      params = [];
      representation =
      { data =
        (FromConstrs
           [{ data =
              { name =
                { data = (Id "true");
                  location =
                  { begin_pos = { line = 1; col = 13 };
                    end_pos = { line = 1; col = 17 } }
                  };
                args = None };
              location =
              { begin_pos = { line = 1; col = 13 };
                end_pos = { line = 1; col = 17 } }
              };
             { data =
               { name =
                 { data = (Id "false");
                   location =
                   { begin_pos = { line = 1; col = 20 };
                     end_pos = { line = 1; col = 25 } }
                   };
                 args = None };
               location =
               { begin_pos = { line = 1; col = 20 };
                 end_pos = { line = 1; col = 25 } }
               }
             ]);
        location =
        { begin_pos = { line = 1; col = 11 }; end_pos = { line = 1; col = 25 } }
        }
      } |}]
;;

let%expect_test "Ty.Decl list test" =
  parse_and_print {| type ('a) list = (::) of 'a list | [] |};
  [%expect
    {|
    { name =
      { data = (Id "list");
        location =
        { begin_pos = { line = 1; col = 11 }; end_pos = { line = 1; col = 15 } }
        };
      params =
      [{ data = (TyVar "a");
         location =
         { begin_pos = { line = 1; col = 7 }; end_pos = { line = 1; col = 9 } } }
        ];
      representation =
      { data =
        (FromConstrs
           [{ data =
              { name =
                { data = (Id "::");
                  location =
                  { begin_pos = { line = 1; col = 18 };
                    end_pos = { line = 1; col = 22 } }
                  };
                args =
                (Some { data =
                        (TConstr (
                           [{ data =
                              (TVar
                                 { data = (TyVar "a");
                                   location =
                                   { begin_pos = { line = 1; col = 26 };
                                     end_pos = { line = 1; col = 28 } }
                                   });
                              location =
                              { begin_pos = { line = 1; col = 26 };
                                end_pos = { line = 1; col = 28 } }
                              }
                             ],
                           { data = (Id "list");
                             location =
                             { begin_pos = { line = 1; col = 29 };
                               end_pos = { line = 1; col = 33 } }
                             }
                           ));
                        location =
                        { begin_pos = { line = 1; col = 26 };
                          end_pos = { line = 1; col = 33 } }
                        })
                };
              location =
              { begin_pos = { line = 1; col = 18 };
                end_pos = { line = 1; col = 33 } }
              };
             { data =
               { name =
                 { data = (Id "[]");
                   location =
                   { begin_pos = { line = 1; col = 36 };
                     end_pos = { line = 1; col = 38 } }
                   };
                 args = None };
               location =
               { begin_pos = { line = 1; col = 36 };
                 end_pos = { line = 1; col = 38 } }
               }
             ]);
        location =
        { begin_pos = { line = 1; col = 16 }; end_pos = { line = 1; col = 38 } }
        }
      } |}]
;;

let%expect_test "Ty.Decl overriding special constructors test" =
  parse_and_print
    {| type t = | true of int | false of int | (::) of string | [] of string | () of bool |};
  [%expect
    {|
    { name =
      { data = (Id "t");
        location =
        { begin_pos = { line = 1; col = 6 }; end_pos = { line = 1; col = 7 } } };
      params = [];
      representation =
      { data =
        (FromConstrs
           [{ data =
              { name =
                { data = (Id "true");
                  location =
                  { begin_pos = { line = 1; col = 12 };
                    end_pos = { line = 1; col = 16 } }
                  };
                args =
                (Some { data =
                        (TConstr ([],
                           { data = (Id "int");
                             location =
                             { begin_pos = { line = 1; col = 20 };
                               end_pos = { line = 1; col = 23 } }
                             }
                           ));
                        location =
                        { begin_pos = { line = 1; col = 19 };
                          end_pos = { line = 1; col = 23 } }
                        })
                };
              location =
              { begin_pos = { line = 1; col = 12 };
                end_pos = { line = 1; col = 23 } }
              };
             { data =
               { name =
                 { data = (Id "false");
                   location =
                   { begin_pos = { line = 1; col = 26 };
                     end_pos = { line = 1; col = 31 } }
                   };
                 args =
                 (Some { data =
                         (TConstr ([],
                            { data = (Id "int");
                              location =
                              { begin_pos = { line = 1; col = 35 };
                                end_pos = { line = 1; col = 38 } }
                              }
                            ));
                         location =
                         { begin_pos = { line = 1; col = 34 };
                           end_pos = { line = 1; col = 38 } }
                         })
                 };
               location =
               { begin_pos = { line = 1; col = 26 };
                 end_pos = { line = 1; col = 38 } }
               };
             { data =
               { name =
                 { data = (Id "::");
                   location =
                   { begin_pos = { line = 1; col = 41 };
                     end_pos = { line = 1; col = 45 } }
                   };
                 args =
                 (Some { data =
                         (TConstr ([],
                            { data = (Id "string");
                              location =
                              { begin_pos = { line = 1; col = 49 };
                                end_pos = { line = 1; col = 55 } }
                              }
                            ));
                         location =
                         { begin_pos = { line = 1; col = 48 };
                           end_pos = { line = 1; col = 55 } }
                         })
                 };
               location =
               { begin_pos = { line = 1; col = 41 };
                 end_pos = { line = 1; col = 55 } }
               };
             { data =
               { name =
                 { data = (Id "[]");
                   location =
                   { begin_pos = { line = 1; col = 58 };
                     end_pos = { line = 1; col = 60 } }
                   };
                 args =
                 (Some { data =
                         (TConstr ([],
                            { data = (Id "string");
                              location =
                              { begin_pos = { line = 1; col = 64 };
                                end_pos = { line = 1; col = 70 } }
                              }
                            ));
                         location =
                         { begin_pos = { line = 1; col = 63 };
                           end_pos = { line = 1; col = 70 } }
                         })
                 };
               location =
               { begin_pos = { line = 1; col = 58 };
                 end_pos = { line = 1; col = 70 } }
               };
             { data =
               { name =
                 { data = (Id "()");
                   location =
                   { begin_pos = { line = 1; col = 73 };
                     end_pos = { line = 1; col = 75 } }
                   };
                 args =
                 (Some { data =
                         (TConstr ([],
                            { data = (Id "bool");
                              location =
                              { begin_pos = { line = 1; col = 79 };
                                end_pos = { line = 1; col = 83 } }
                              }
                            ));
                         location =
                         { begin_pos = { line = 1; col = 78 };
                           end_pos = { line = 1; col = 83 } }
                         })
                 };
               location =
               { begin_pos = { line = 1; col = 73 };
                 end_pos = { line = 1; col = 83 } }
               }
             ]);
        location =
        { begin_pos = { line = 1; col = 8 }; end_pos = { line = 1; col = 83 } } }
      } |}]
;;
