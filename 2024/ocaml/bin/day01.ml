(*let print_list list = 
    List.iter (Printf.printf "%d \n") list;
    list*)

let rec quicksort list = 
    match list with 
        | [] -> []
        | pivot::t -> 
            let left = quicksort @@ List.filter (fun x -> x < pivot ) t in
            let right = quicksort @@ List.filter (fun x -> x >= pivot) t in
            List.append (List.append left [pivot]) right

let rec parse left_arr right_arr channel = 
    match input_line channel with
        | line -> 
            let r = String.split_on_char ' ' line 
                |> List.filter (fun x -> not (String.equal x "" || String.equal x " "))
                |> List.map int_of_string in
            let left = List.nth r 0 in
            let right = List.nth r 1 in

            parse (List.append left_arr [left]) (List.append right_arr [right]) channel
        | exception End_of_file ->
            close_in channel;
            (left_arr, right_arr)

let rec sum_of_diff left right = 
    match (left, right) with
        | ([], []) -> 0
        | (a::b, c::d) -> 
            abs(a - c) + (sum_of_diff b d)
        | _ -> 0

let rec count_of_elements list hash_map =
    match list with
        | [] -> hash_map
        | h::t ->
            let count_head = Hashtbl.find_opt hash_map h in
            (match count_head with
                | Some x -> Hashtbl.replace hash_map h (x + 1)
                | None -> Hashtbl.add hash_map h 1);
            count_of_elements t hash_map

let rec sum_of_count list hash_map =
    match list with
        | [] -> 0
        | h::t -> 
            let count = Hashtbl.find_opt hash_map h |> Option.value ~default:0 in
            h * count + (sum_of_count t hash_map)
                

let first = 
    let (left_arr, right_arr) = parse [] [] @@ open_in "../inputs/1.txt" in
    let left_arr = quicksort left_arr in
    let right_arr = quicksort right_arr in
    Printf.printf "%d\n" @@ sum_of_diff left_arr right_arr;;
    ()

let second = 
    let (left_arr, right_arr) = parse [] [] @@ open_in "../inputs/1.txt" in
    let left_arr = quicksort left_arr in
    let right_arr = quicksort right_arr in
    let hash_map = Hashtbl.create 12345 in
    let counts = count_of_elements right_arr hash_map in
    Printf.printf "%d \n" @@ sum_of_count left_arr counts;
    ()

let () = 
    first;;
    second;;
