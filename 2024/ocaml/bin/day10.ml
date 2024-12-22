open Batteries

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


let convert_to_ints s =
    String.to_list s
        |> List.map (fun x -> int_of_char x - int_of_char '0')
        |> DynArray.of_list

let rec parse res channel = 
    match input_line channel with
        | line -> 
            let line = convert_to_ints line in
            DynArray.add res line;
            parse res channel
        | exception End_of_file ->
            close_in channel;
            res

let get_i_j arr i j = 
    DynArray.get (DynArray.get arr i) j

let find_all_start map =
    let rows = DynArray.length map 
    and cols = DynArray.length (DynArray.get map 0) in
    let res = DynArray.create() in

    let rec inner i j =
        if i >= rows then
            res
        else if j >= cols then
            inner (i + 1) (0)
        else 
        (
            let x = get_i_j map i j  in
            match x with 
            | 0 -> 
                DynArray.add res (i , j);
                inner i (j + 1)
            | _ -> inner i (j + 1)
        )
    in
    inner 0 0 

let get_scores1 map (start_x, start_y) =
    let rows = DynArray.length map 
    and cols = DynArray.length (DynArray.get map 0) in
    let directions = [(1, 0); (-1, 0); (0, 1); (0, -1)] in
    let set = ref CoordSet.empty in

    let rec inner prev i j =
        if i < 0 
            || i >= rows 
            || j < 0 
            || j >= cols 
            || get_i_j map i j != prev + 1 
        then
            ()
        else if get_i_j map i j == 9 && prev == 8 then
        (
            set := CoordSet.add { x= i; y= j } !set;
            ()
        )
        else
            List.iter (fun (dx,dy) -> inner (get_i_j map i j) (i + dx) (j + dy)) 
                directions
    in

    inner (-1) start_x start_y;
    CoordSet.cardinal !set


let get_scores2 map (start_x, start_y) =
    let rows = DynArray.length map 
    and cols = DynArray.length (DynArray.get map 0) in
    let directions = [(1, 0); (-1, 0); (0, 1); (0, -1)] in

    let rec inner prev i j =
        if i < 0 
            || i >= rows 
            || j < 0 
            || j >= cols 
            || get_i_j map i j != prev + 1 
        then
            0
        else if get_i_j map i j == 9 && prev == 8 then
            1
        else
            List.map (fun (dx,dy) -> inner (get_i_j map i j) (i + dx) (j + dy)) 
                directions
            |> List.fold_left ( + ) 0
    in

    inner (-1) start_x start_y


let first filename = 
    let map = open_in filename 
        |> parse (DynArray.create())
    in
    DynArray.iter (fun x -> 
        DynArray.iter (Printf.printf "%d ") x;
        Printf.printf "\n";
    ) map;
    let starting_points = find_all_start map in
    let res = DynArray.map (get_scores1 map) starting_points
    |> DynArray.fold_left ( + ) 0 in
    Printf.printf "first_result: %d\n" res;
    ()

let second filename = 
    let map = open_in filename 
        |> parse (DynArray.create())
    in
    DynArray.iter (fun x -> 
        DynArray.iter (Printf.printf "%d ") x;
        Printf.printf "\n";
    ) map;
    let starting_points = find_all_start map in
    let res = DynArray.map (get_scores2 map) starting_points
    |> DynArray.fold_left ( + ) 0 in
    Printf.printf "second_result: %d\n" res;
    ()

let () = 
    first "../inputs/10.txt";;
    second "../inputs/10.txt";;
