module WrapBuilder = Semantics.WrapBuilder

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
    (TDWB : WrapBuilder(TDW).T) : sig
  val from_string
    :  string
    -> (Ast.Make(SW)(SIW)(EW)(PW)(CW)(TEW)(TDW).Ty.Decl.t, string option) result
end = struct
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

    let parse lexbuf =
      try
        let structure = loop lexbuf (Parser.Incremental.parse lexbuf.lex_curr_p) in
        Ok structure
      with
      | SyntaxError msg_opt -> Error msg_opt
    ;;

    let from_string s =
      let lexbuf = Lexing.from_string s in
      parse lexbuf
    ;;
  end

  let from_string = Parse.from_string
end

(***************************checks***************************)

open Located
module L = Located
module LB = LocatedBuilder
module P = Make (L) (L) (L) (L) (L) (L) (L) (LB) (LB) (LB) (LB) (LB) (LB) (LB)
(* let used_pp = Semantics.Semantics.Constant.pp *)
(* let used_pp = Semantics.Semantics.Ty.Expr.pp *)
(* let used_pp = Semantics.Semantics.Ty.Decl.pp *)

(* let parse_and_print s =
   match Parse.from_string s with
   | Ok v -> Stdlib.Format.printf "%a" used_pp v
   | Error _ -> Stdlib.Printf.printf "Error"
   ;; *)

(* let%expect_test _ =
  (* parse_and_print {|'a * 'a * 'a list|}; *)
  parse_and_print {|type ('a, 'b) t = A of 'a list | B of 'b * int -> 'b option |};
  [%expect
    {|
    { name =
      { data = (Id "t");
        location =
        { begin_pos = { line = 1; col = 14 }; end_pos = { line = 1; col = 15 } }
        };
      params =
      [{ data = (TyVar "a");
         location =
         { begin_pos = { line = 1; col = 6 }; end_pos = { line = 1; col = 8 } } };
        { data = (TyVar "b");
          location =
          { begin_pos = { line = 1; col = 10 }; end_pos = { line = 1; col = 12 }
            }
          }
        ];
      representation =
      { data =
        (FromConstrs
           [{ data =
              { name =
                { data = (Id "A");
                  location =
                  { begin_pos = { line = 1; col = 18 };
                    end_pos = { line = 1; col = 19 } }
                  };
                args =
                (Some { data =
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
                              }
                             ],
                           { data = (Id "list");
                             location =
                             { begin_pos = { line = 1; col = 26 };
                               end_pos = { line = 1; col = 30 } }
                             }
                           ));
                        location =
                        { begin_pos = { line = 1; col = 23 };
                          end_pos = { line = 1; col = 30 } }
                        })
                };
              location =
              { begin_pos = { line = 1; col = 18 };
                end_pos = { line = 1; col = 30 } }
              };
             { data =
               { name =
                 { data = (Id "B");
                   location =
                   { begin_pos = { line = 1; col = 33 };
                     end_pos = { line = 1; col = 34 } }
                   };
                 args =
                 (Some { data =
                         (TArrow (
                            { data =
                              (TTuple
                                 [{ data =
                                    (TVar
                                       { data = (TyVar "b");
                                         location =
                                         { begin_pos = { line = 1; col = 38 };
                                           end_pos = { line = 1; col = 40 } }
                                         });
                                    location =
                                    { begin_pos = { line = 1; col = 38 };
                                      end_pos = { line = 1; col = 40 } }
                                    };
                                   { data =
                                     (TConstr ([],
                                        { data = (Id "int");
                                          location =
                                          { begin_pos = { line = 1; col = 43 };
                                            end_pos = { line = 1; col = 46 } }
                                          }
                                        ));
                                     location =
                                     { begin_pos = { line = 1; col = 42 };
                                       end_pos = { line = 1; col = 46 } }
                                     }
                                   ]);
                              location =
                              { begin_pos = { line = 1; col = 38 };
                                end_pos = { line = 1; col = 46 } }
                              },
                            { data =
                              (TConstr (
                                 [{ data =
                                    (TVar
                                       { data = (TyVar "b");
                                         location =
                                         { begin_pos = { line = 1; col = 50 };
                                           end_pos = { line = 1; col = 52 } }
                                         });
                                    location =
                                    { begin_pos = { line = 1; col = 50 };
                                      end_pos = { line = 1; col = 52 } }
                                    }
                                   ],
                                 { data = (Id "option");
                                   location =
                                   { begin_pos = { line = 1; col = 53 };
                                     end_pos = { line = 1; col = 59 } }
                                   }
                                 ));
                              location =
                              { begin_pos = { line = 1; col = 50 };
                                end_pos = { line = 1; col = 59 } }
                              }
                            ));
                         location =
                         { begin_pos = { line = 1; col = 38 };
                           end_pos = { line = 1; col = 59 } }
                         })
                 };
               location =
               { begin_pos = { line = 1; col = 33 };
                 end_pos = { line = 1; col = 59 } }
               }
             ]);
        location =
        { begin_pos = { line = 1; col = 16 }; end_pos = { line = 1; col = 59 } }
        }
      }
    |}]
;; *)
