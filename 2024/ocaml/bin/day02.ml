type level_state = 
    | Increasing of int
    | Decreasing of int
    | None
    | UnSafe

let _print_level_state state = 
    match state with
    | Increasing x -> Printf.printf "Increasing %d\n" x
    | Decreasing x -> Printf.printf "Decreasing %d\n" x
    | None -> Printf.printf "None\n"
    | UnSafe -> Printf.printf "UnSafe\n";;

let is_level_safe level_state = 
    match level_state with
    | Increasing diff -> diff <= 3
    | Decreasing diff -> diff <= 3
    | None -> true
    | UnSafe -> false


let rec parse list channel =
    match input_line channel with
        | line -> 
            let x = String.split_on_char ' ' line
                |> List.map int_of_string 
                |> fun reports -> List.append list [reports] in
            parse x channel
        | exception End_of_file ->
            close_in channel;
            list

let handle_state res last_elem curr_elem = 
    match res with
    | Increasing max_diff -> 
        if last_elem >= curr_elem then
            UnSafe
        else Increasing (max max_diff (curr_elem - last_elem))
    | Decreasing max_diff -> 
        if last_elem <= curr_elem then
            UnSafe
        else Decreasing (max max_diff (last_elem - curr_elem))
    | None -> 
        if last_elem == curr_elem then
            UnSafe
        else if last_elem == 0 then
            None
        else if last_elem < curr_elem then
            Increasing (curr_elem - last_elem)
        else Decreasing (last_elem - curr_elem)
    | UnSafe -> UnSafe

let rec get_level_state res last_elem list = 
    match list with 
        | [] -> res
        | h::t ->
            let res = handle_state res last_elem h in
            get_level_state res h t

let rec remove_one_and_try index list = 
    if index == List.length list then
        false
    else
        let new_list = List.filteri (fun x _ -> x != index) list in
        let is_safe = get_level_state None 0 new_list |> is_level_safe in
        if is_safe then
            true
        else
            remove_one_and_try (index + 1) list

    
        
let rec retry list =
    match list with 
        | [] -> 0
        | (_,h)::t ->
            if remove_one_and_try 0 h then
                1 + retry t
            else
                retry t
    
let first = 
    let parsed = open_in "../inputs/2.txt"
        |> parse [] in
    List.map (get_level_state None 0) parsed 
        |> List.filter (fun res -> match res with
            | Increasing diff -> diff <= 3
            | Decreasing diff -> diff <= 3
            | None -> true
            | UnSafe -> false) 
        |> List.length
        |> fun x -> Printf.printf "%d\n" x

let second = 
    let parsed = open_in "../inputs/2.txt"
        |> parse [] in
    List.map (fun x -> ((get_level_state None 0 x), x)) parsed 
        (*|> List.map (fun (x, l) -> print_level_state x; (x, l))*)
        |> List.partition (fun (x ,_l) -> is_level_safe x)
        |> fun (a, b) -> List.length a + (retry b)
        |> fun x -> Printf.printf "%d\n" x

let () = 
    first;;
    second;;
