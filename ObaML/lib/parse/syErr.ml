open Navi

type t =
  | UnexpectedChar of Location.t * string
  | WithMsg of string
[@@deriving eq, show { with_path = false }]

exception SyntaxError of t

let unexpected_char_err lexbuf =
  let begin_p = Lexing.lexeme_start_p lexbuf in
  let end_p = Lexing.lexeme_end_p lexbuf in
  let location = Location.from_lex_poss begin_p end_p in
  SyntaxError (UnexpectedChar (location, Lexing.lexeme lexbuf))
;;

let of_string msg = SyntaxError (WithMsg msg)
