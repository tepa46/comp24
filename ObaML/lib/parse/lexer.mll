{
    open Tokens
    open SyErr
}

let white_space = [' ' '\t' '\n' '\r']+

(************symbol*groups************)
let digit = ['0'-'9']
let sign = ['+' '-']
let lower_letter = ['a'-'z']
let upper_letter = ['A'-'Z']
let letter = lower_letter | upper_letter
let math = ['+' '-' '*' '/' '=' '<' '>' '(' ')' '&' '|' '^']
let grammar = ['.' ',' '?' '!' '`' ':' ';' '\'']
let special = ['~' '[' ']' '{' '}' '#' '$' '_' '@']
let core_operator_char = ['$' '&' '*' '+' '-' '/' '=' '>' '@' '^' '|']
let operator_char = ['~' '!' '?' '%' '<' ':' '.'] | core_operator_char

(************constants************)
let c_int = sign? digit+

let c_string = '"' (letter | digit | math | grammar | special)* '"'

(************Names************)

let name_tail = (letter | digit | '_')*

let lower_id = lower_letter name_tail

let upper_id = upper_letter name_tail

(************Infix*and*Prefix*operators************)

let infix_symbol = ((core_operator_char | ['%' '<']) operator_char*) | ('#' operator_char+)

let prefix_op = ('!' operator_char*) | (['?' '~'] operator_char+)

(* let core_operator_char =  *)

rule token = parse
    | white_space { token lexbuf }
    | c_int { INT (Lexing.lexeme lexbuf |> int_of_string) }
    | c_string { let ({ pos_fname; pos_lnum; pos_bol; pos_cnum } : Lexing.position) = Lexing.lexeme_start_p lexbuf in
                 let (start_p : Lexing.position) = { pos_fname; pos_lnum; pos_bol; pos_cnum = pos_cnum + 1 } in
                 let ({ pos_fname; pos_lnum; pos_bol; pos_cnum } : Lexing.position) = Lexing.lexeme_end_p lexbuf in
                 let (end_p : Lexing.position) = { pos_fname; pos_lnum; pos_bol; pos_cnum = pos_cnum - 1 } in
                 let s = Lexing.lexeme lexbuf in
                 let s = String.sub s 1 (String.length s - 2) in
                 STRING (s, (start_p, end_p)) }
    | "_" { UNDERSCORE }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "[" { LBRACKET }
    | "]" { RBRACKET }
    | "*" { ASTERISK }
    | "," { COMMA }
    | "->" { ARROW }
    | "'" { QUOTE }
    | "|" { BAR }
    | "::" { COLONCOLON }
    | "=" { EQUAL }
    | "of" { OF }
    | "type" { TYPE }
    | "true" { TRUE }
    | "false" { FALSE }
    | lower_id { LOWERID (Lexing.lexeme lexbuf) }
    | upper_id { UPPERID (Lexing.lexeme lexbuf) }
    | eof { EOF }
    | _ { raise (unexpected_char_err lexbuf) }