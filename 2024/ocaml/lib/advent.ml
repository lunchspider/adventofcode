type point = { x:int; y: int }  [@@deriving show]

let ( ++ ) a b = {x = a.x + b.x; y = a.y + b.y}


module CoordSet = Set.Make (struct
  type t = point

  let compare a b = 
    let cmp_x = Int.compare a.x b.x in
    if  cmp_x <> 0 then cmp_x
    else Int.compare a.y b.y
end)

module Direction = struct
    type dir = Up | Down | Left | Right

    let of_char = function
        | '^' -> Up
        | '>' -> Right
        | '<' -> Left
        | 'v' -> Down
        | _ -> assert false

    let to_string = function
        | Up -> "^"
        | Down -> "v"
        | Right -> ">"
        | Left -> "<"

    let turn_right = function
        | Up -> Right
        | Right -> Down
        | Down -> Left
        | Left -> Up

    let turn_left = function
        | Up -> Left
        | Right -> Up
        | Down -> Right
        | Left -> Down

    let to_di = function
        | Up -> {x = -1; y = 0}
        | Down -> { x = 1; y = 0}
        | Right -> { x = 0; y = 1}
        | Left -> {x = 0; y = -1}
end
