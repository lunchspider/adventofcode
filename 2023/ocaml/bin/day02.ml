type round = { mutable red : int; mutable blue: int; mutable green: int}

let result = ref 0;;

let handle_round round_info r = 
    let r = List.map String.trim (String.split_on_char ' ' r) in
    let num = List.nth r 0 |> int_of_string_opt |> Option.value ~default:0 in
    let color = List.nth r 1 in
    match color with
        | "green" -> round_info.green <- max round_info.green num
        | "blue" -> round_info.blue <- max round_info.blue num
        | "red" -> round_info.red <- max round_info.red num
        | _ -> ()

let parse_round rounds = 
    let round_info = { red = 0; blue = 0; green = 0} in
    let x = List.map String.trim (String.split_on_char ',' rounds) in
    List.iter (handle_round round_info) x;
    round_info


let parse_game str = 
    let x = List.map String.trim (String.split_on_char ':' str) in 
    match x with
        | [] -> [];
        | _::y -> let rounds = (List.map (String.split_on_char ';') y |> List.flatten) in
    List.map parse_round rounds

let round_check x = x.red <= 12 && x.green <= 13 && x.blue <= 14

let find_max_round m x =
    m.green <- max m.green x.green;
    m.blue <- max m.blue x.blue;
    m.red <- max m.red x.red;;


let rec first game_number channel = 
    let is_ok = ref true in
    match input_line channel with
    | line -> 
        let rounds = parse_game line in
        List.iter (fun x -> is_ok := !is_ok && round_check x) rounds;
        if !is_ok then begin
            result := !result + game_number;
        end;
        first (game_number + 1) channel
    | exception End_of_file ->
     close_in channel;
        !result;;

let rec second game_number channel =
    match input_line channel with
    | line  -> 
        let rounds = parse_game line in
        let m  = List.nth rounds 0 in
        List.iter (find_max_round m) rounds;
        let pow = m.red * m.green * m.blue in
        result := !result + pow;
        second (game_number + 1) channel
    | exception End_of_file ->
        close_in channel;
        !result;;

let () = 
    Printf.printf "first: %d\n" @@ first 1 @@ open_in "../inputs/2.txt";;
    result := 0;
    Printf.printf "second: %d\n" @@ second 1 @@ open_in "../inputs/2.txt";;
