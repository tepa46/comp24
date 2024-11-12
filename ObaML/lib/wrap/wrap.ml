module type T = sig
  type 'a t

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
end

module Builder (W : T) = struct
  module type T = sig
    val wrap : 'a -> Navi.Location.t -> 'a W.t
  end
end
