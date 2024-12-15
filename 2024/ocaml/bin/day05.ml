let is_rule_ok x y rules =
    let res = ref false in
    Hashtbl.find_all rules x 
    |> List.iter (fun child -> 
        if child == y then
            res := true
    );
    !res

let is_update_ok rules update =
    let update_len = Array.length update in
    let res = ref true in
    for i = 0 to update_len - 1 do
        for j = i + 1 to update_len - 1 do
            let is_ok = is_rule_ok update.(i) update.(j) rules in
            let is_rev = is_rule_ok update.(j) update.(i) rules in
            if is_rev then
                res := false;
            if is_ok then
                res := !res && true;
        done;
    done;
    !res

let make_update_ok rules update =
    Array.sort (fun x y -> 
        let is_ok = is_rule_ok x y rules in
        let is_rev = is_rule_ok y x rules in
        if is_rev then
            -1
        else if is_ok then
            1
        else
            0
    ) update;
    update

    

let get_mid arr = arr.(Array.length arr / 2)
    

let parse_rules rules = 
    let my_hash = Hashtbl.create @@ List.length rules in
    let insert_in_hashtbl x y = Hashtbl.add my_hash x y in
    List.iter (fun x -> Scanf.sscanf x "%d|%d" (insert_in_hashtbl)) rules;
    my_hash

let parse_updates update =
    let parse_single_update str =
        String.split_on_char ',' str
        |> List.map int_of_string
        |> Array.of_list
    in
    List.map parse_single_update update

let parse channel = 
    let (rules, update) = In_channel.input_lines channel 
        |> List.filter (fun x -> String.length x != 0)
        |> List.partition (fun x -> String.contains x '|')
    in 
    let rules = parse_rules rules in
    let update = parse_updates update in
    (rules, update)


let first = 
    let (rules, updates) = open_in "../inputs/5.txt"
    |> parse in
    let first_result = List.filter (is_update_ok rules) updates
        |> List.map get_mid
        |> List.fold_left (+) 0 in
    Printf.printf "first_result: %d\n" first_result

let second = 
    let (rules, updates) = open_in "../inputs/5.txt"
    |> parse in
    let second_result = List.filter (fun x -> not @@ is_update_ok rules x) updates
        |> List.map @@ make_update_ok rules
        |> List.map get_mid
        |> List.fold_left (+) 0 in
    Printf.printf "second_result: %d\n" second_result

let () =
    first;
    second;
