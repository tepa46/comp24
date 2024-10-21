open Navi

module Located = struct
  type 'a t =
    { data : 'a
    ; location : Location.t
    }
  [@@deriving eq, show { with_path = false }]

  let escape { data; location = _ } = data
end

module LocatedBuilder = struct
  let wrap t loc : 'a Located.t = { data = t; location = loc }
end

module LocatedAst =
  Ast.Make (Located) (Located) (Located) (Located) (Located) (Located) (Located)
