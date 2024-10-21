module Position = struct
  type t =
    { line : int
    ; col : int
    }
  [@@deriving eq, show { with_path = false }]

  let from_lex_pos
    ({ pos_lnum = line; pos_bol = bol; pos_cnum = cnum; _ } : Lexing.position)
    =
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
    { begin_pos = Position.from_lex_pos begin_pos
    ; end_pos = Position.from_lex_pos end_pos
    }
  ;;
end
