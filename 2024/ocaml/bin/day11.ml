let parse channel = 
    match input_line channel with
    | x -> 
        String.split_on_char ' ' x
        |> List.map (int_of_string)
    | exception End_of_file -> 
        close_in channel;
        []

let rec get_digit x =
    match x with
    | 0 -> []
    | x -> 
        let digit = x mod 10 in
        get_digit (x / 10) @ [digit]

let split list n =
    let rec aux (i : int) (acc : int list) = function
    | [] -> List.rev acc, []
    | h :: t as l -> if i = 0 then List.rev acc, l
      else aux (i-1) (h :: acc) t  in
    aux n [] list

let construct_num list =
    let rec aux acc list =
        match list with
        | [] -> acc
        | h::t ->
            aux (acc * 10 + h) t
    in
    aux 0 list

let rec simulate i tbl res =
    match Hashtbl.find_opt tbl (i , res) with
    | Some(x) -> x
    | None -> 
        let digits = get_digit res in
        let x = if i == 0 then
            1
        else if res == 0 then
            simulate (i - 1) tbl 1
        else if List.length digits mod 2 == 0 then
            (
                let (left, right) = split digits ((List.length digits) / 2) in
                simulate  (i - 1) tbl (construct_num left)
                    + simulate (i - 1) tbl (construct_num right)
            )
        else
            simulate (i - 1) tbl (res * 2024)
        in
        Hashtbl.add tbl (i, res) x;
        x

let first filename = 
    open_in filename
        |> parse
        |> List.map (simulate 25 (Hashtbl.create 10000)) 
        |> List.fold_left ( + ) 0
        |> Printf.printf "first_result: %d\n"

let second filename = 
    open_in filename
        |> parse
        |> List.map (simulate 75 (Hashtbl.create 10000)) 
        |> List.fold_left ( + ) 0
        |> Printf.printf "second_result: %d\n"

let () =
    first "../inputs/11.txt";;
    second "../inputs/11.txt";;
