open Ast

module Position = struct
  type t =
    { line : int
    ; col : int
    }
  [@@deriving eq, show { with_path = false }]

  let from_lex ({ pos_lnum = line; pos_bol = bol; pos_cnum = cnum; _ } : Lexing.position) =
    { line; col = cnum - bol }
  ;;
end

module Location = struct
  type t =
    { begin_pos : Position.t
    ; end_pos : Position.t
    }
  [@@deriving eq, show { with_path = false }]

  let from_lex_poss begin_pos end_pos =
    { begin_pos = Position.from_lex begin_pos; end_pos = Position.from_lex end_pos }
  ;;
end

module Located = struct
  type 'a t =
    { data : 'a
    ; location : Location.t
    }
  [@@deriving eq, show { with_path = false }]

  let escape { data; location = _ } = data

  let from_lex_poss data begin_pos end_pos =
    { data; location = Location.from_lex_poss begin_pos end_pos }
  ;;

  let from_lex_pos_pair data (begin_pos, end_pos) = from_lex_poss data begin_pos end_pos
end

module LocatedAst =
  Ast (Located) (Located) (Located) (Located) (Located) (Located) (Located)
