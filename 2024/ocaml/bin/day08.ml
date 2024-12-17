type pos = {
  x: int;
  y: int;
}


module CoordSet = Set.Make (struct
  type t = pos

  let compare a b = 
    let cmp_x = Int.compare a.x b.x in
    if  cmp_x <> 0 then cmp_x
    else Int.compare a.y b.y
end)

let add_if_in_change a b set =
    if a.x >= b.x || a.x < 0 || a.y >= b.y || a.y < 0 then
        set
    else
        CoordSet.add a set

let parse antenna channel = 
    let rec parse_line i j str =
        if j == String.length str then 
            ()
        else if str.[j] != '.' then
            (
                Hashtbl.add antenna str.[j] ({ x = i; y = j});
                parse_line i (j + 1) str
            )
        else
        parse_line i (j + 1) str
    in
    let lines = In_channel.input_lines channel in
    List.iteri (fun i x -> parse_line i 0 x) lines;
    (antenna, List.length lines, String.length (List.nth lines 0))

let first filename = 
    let (table, rows, cols) = open_in filename 
        |> parse (Hashtbl.create 26) 
    in
    let range = { x = rows; y = cols } in

    let get_antinodes a b set = 
        let (dx, dy) = ((a.x - b.x), (a.y - b.y)) in
        add_if_in_change {x = (a.x + dx); y = (a.y + dy)} range set 
            |> add_if_in_change {x = (b.x - dx); y = (b.y - dy)} range
    in
    
    let get_all_antinodes tbl =
        let set = CoordSet.empty in
        let rec inner list set = 
            match list with 
                | [] -> set
                | a::list ->
                    let set = List.fold_left (fun set b -> get_antinodes a b set) set list in
                    inner list set
        in
        Hashtbl.to_seq_keys tbl
            |> Seq.fold_left (fun acc x -> inner (Hashtbl.find_all tbl x) acc) 
            (set)
    in
    let cords = table
        |> get_all_antinodes
        |> CoordSet.to_list
    in
    let res = cords
        |> List.length
    in
    Printf.printf "first result: %d\n" res

let second filename = 
    let (table, rows, cols) = open_in filename 
        |> parse (Hashtbl.create 26) 
    in
    let range = { x = rows; y = cols } in

    let rec add_while_in_range pos dpos set =
        if pos.x + dpos.x < 0 || pos.x + dpos.x >= range.x || pos.y + dpos.y < 0 || pos.y + dpos.y >= range.y then
            set
        else
        (
            let pos = {x = pos.x + dpos.x; y = pos.y + dpos.y} in
            CoordSet.add pos set 
                |> add_while_in_range pos dpos
        )
    in

    let get_antinodes_second a b set = 
        let (dx, dy) = ((a.x - b.x), (a.y - b.y)) in
        add_while_in_range a {x = dx; y = dy} set
        |> add_while_in_range a {x = -dx; y = -dy}
        |> add_while_in_range b {x = dx; y = dy}
        |> add_while_in_range b {x = -dx; y = -dy}
    in
    
    let get_all_antinodes_second tbl =
        let set = CoordSet.empty in
        let rec inner list set = 
            match list with 
                | [] -> set
                | a::list ->
                    let set = List.fold_left (fun set b -> get_antinodes_second a b set) set list in
                    inner list set
        in
        Hashtbl.to_seq_keys tbl
            |> Seq.fold_left (fun acc x -> inner (Hashtbl.find_all tbl x) acc) 
            (set)
    in
    let cords = table
        |> get_all_antinodes_second
        |> CoordSet.to_list
    in
    let res = cords
        |> List.length
    in
    Printf.printf "second result: %d\n" res
let () =
    first "../inputs/8.txt";;
    second "../inputs/8.txt";;
