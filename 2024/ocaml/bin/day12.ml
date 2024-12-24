type point = {x : int; y: int}

let ( ++ ) a b = {x = a.x + b.x; y = a.y + b.y}

let adjacent pos = 
    List.map (fun x -> pos ++ x) [ { x = 0; y = 1}; {x = 0; y = -1}; {x = 1; y = 0}; {x = -1; y = 0} ]


module CoordSet = Set.Make (struct
  type t = point

  let compare a b = 
    let cmp_x = Int.compare a.x b.x in
    if  cmp_x <> 0 then cmp_x
    else Int.compare a.y b.y
end)

let get_regions map =
    let rows = Array.length map 
    and cols = String.length map.(0) 
    and visited = ref CoordSet.empty
    in

    let rec dfs ch points point =
        if point.x < 0 || point.y < 0 
            || point.x >= rows || point.y >= cols 
            || map.(point.x).[point.y] != ch
        then
            ()
        else
            match CoordSet.mem point !visited with
            | false -> 
                visited := CoordSet.add point !visited;
                Dynarray.add_last points point;
                List.iter (dfs ch points) (adjacent point)
            | true -> 
                ()
    in

    let res = Dynarray.create() in
    for i = 0 to rows - 1 do
        for j = 0 to cols - 1 do
            let point = { x = i; y = j } in
            if not (CoordSet.mem point !visited) then
            (
                let arr  = Dynarray.create() in
                dfs map.(i).[j] arr point;
                Dynarray.add_last res arr;
            )
        done;
    done;

    res

let count_sides region =
    let set = CoordSet.of_seq @@ Dynarray.to_seq region in
    let num_edge = ref 0 in
    let rec loop l = 
        match l () with
        | Seq.Nil -> ()
        | Seq.Cons(h, t) ->
            let north = { x = h.x - 1; y = h.y}
            and north_west = { x = h.x - 1; y = h.y - 1} 
            and west = { x = h.x ; y = h.y - 1} 
            and south_west = { x = h.x + 1; y = h.y - 1} 
            and south = { x = h.x + 1; y = h.y} 
            and east = { x = h.x; y = h.y + 1}
            and north_east = { x = h.x - 1; y = h.y + 1}
            in

            if CoordSet.mem north set |> not then
                if not (CoordSet.mem west set && CoordSet.mem north_west set |> not) then
                    num_edge := !num_edge + 1;

            if CoordSet.mem south set |> not then
                if not(CoordSet.mem west set && CoordSet.mem south_west set |> not) then
                    num_edge := !num_edge + 1;

            if CoordSet.mem west set |> not then
                if not(CoordSet.mem north set && CoordSet.mem north_west set |> not) then
                    num_edge := !num_edge + 1;

            if CoordSet.mem east set |> not then
                if not(CoordSet.mem north set && CoordSet.mem north_east set |> not) then
                    num_edge := !num_edge + 1;


            loop t
    in
    loop (Dynarray.to_seq region);
    !num_edge

let find_parameter region =
    let set = CoordSet.of_seq @@ Dynarray.to_seq region in
    let rec loop res l = 
        match l () with
        | Seq.Nil -> res
        | Seq.Cons(h, t) ->
            let res = List.map (fun x -> if CoordSet.mem x set then 0 else 1) (adjacent h)
            |> List.fold_left ( + ) res in
            loop res t
    in
    loop 0 (Dynarray.to_seq region)

let parse channel = 
    let lines = In_channel.input_lines channel |> Array.of_list in
    close_in channel;
    lines

let first filename =
    let regions = open_in filename 
        |> parse 
        |> get_regions
    in
    Dynarray.to_seq regions
    |> Seq.map (fun region -> find_parameter region * Dynarray.length region)
    |> Seq.fold_left ( + ) 0
    |> Printf.printf "first_result: %d\n";
    ()

let second filename =
    let regions = open_in filename 
        |> parse 
        |> get_regions
    in
    Dynarray.to_seq regions
    |> Seq.map (fun region -> count_sides region * Dynarray.length region)
    |> Seq.fold_left ( + ) 0
    |> Printf.printf "second_result: %d\n";
    ()
let () = 
    first "../inputs/12.txt";;
    second "../inputs/12.txt";;
