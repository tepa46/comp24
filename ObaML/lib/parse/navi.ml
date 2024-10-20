open Ast

module Location = struct
  type t =
    { begin_pos : Lexing.position
    ; end_pos : Lexing.position
    }

  let show_lex_pos : Lexing.position -> string =
    fun { pos_fname = fname; pos_lnum = lnum; pos_bol = bol; pos_cnum = cnum } ->
    Printf.sprintf "[%s: %d %d %d]" fname lnum bol cnum
  ;;

  let show loc =
    let begin_pos = show_lex_pos loc.begin_pos in
    let end_pos = show_lex_pos loc.end_pos in
    Printf.sprintf "{begin_pos: %s; end_pos: %s}" begin_pos end_pos
  ;;

  let pp fmt loc = show loc |> Format.pp_print_string fmt

  let equal { begin_pos = lbp; end_pos = lep } { begin_pos = rbp; end_pos = rep } =
    lbp = rbp && lep = rep
  ;;

  let by_begin_end begin_pos end_pos = { begin_pos; end_pos }
end

module Located : Wrap = struct
  type 'a t =
    { data : 'a
    ; location : Location.t
    }
  [@@deriving eq, show { with_path = false }]

  let escape { data; location = _ } = data

  let by_data_begin_end data begin_pos end_pos =
    { data; location = Location.by_begin_end begin_pos end_pos }
  ;;
end

module LocatedAst =
  Ast (Located) (Located) (Located) (Located) (Located) (Located) (Located)
