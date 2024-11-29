open Navi

module Wrap : Wrap.T = struct
  type 'a t =
    { data : 'a
    ; location : Location.t
    }
  [@@deriving eq, show { with_path = false }]
end

module Ast = Ast.Make (Wrap) (Wrap) (Wrap) (Wrap) (Wrap) (Wrap) (Wrap)
