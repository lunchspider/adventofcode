let parse_line r =
    let nums  = String.split_on_char ' ' r in
    List.map (fun x -> int_of_string_opt x |> Option.value ~default:0) nums

let rec build_tree nums results = 
    if List.filter (fun x -> x = 0) nums |> List.length != List.length nums then begin
        let rec loop list n result =
            if n = (List.length list) - 1 then result
            else begin 
                let num = (List.nth list (n + 1)) -  (List.nth list n) in
                loop list (n + 1) @@ List.append result [num]
                end
        in
        let result = loop nums 0 [] in
        build_tree result @@ List.append results [result]
        end
    else begin
        results
        end


let last l = List.length l - 1 |> List.nth l

let rec first result channel =
    match input_line channel with
        | line -> 
        let nums = parse_line line in
        let r = build_tree nums [nums] 
            |> List.fold_left (fun acc l -> acc + last l) 0 in
        first (result+r) channel
        | exception End_of_file ->
            close_in channel;
            result


let rec second result channel =
    match input_line channel with
        | line -> 
        let nums = parse_line line in
        let r = build_tree nums [nums] 
            |> List.fold_left (fun acc l -> List.hd l - acc) 0 in
        second (result+r) channel
        | exception End_of_file ->
            close_in channel;
            result

let () =
    Printf.printf "%d\n" @@ first 0 @@ open_in "../inputs/9.txt" ;
    Printf.printf "%d\n" @@ second 0 @@ open_in "../inputs/9.txt" ;
