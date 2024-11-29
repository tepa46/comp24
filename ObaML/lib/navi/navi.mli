module Position : sig
  type t =
    { line : int
    ; col : int
    }

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val from_lex_pos : Lexing.position -> t
end

module Location : sig
  type t =
    { begin_pos : Position.t
    ; end_pos : Position.t
    }

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val from_lex_poss : Lexing.position -> Lexing.position -> t
end
