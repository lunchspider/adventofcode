let find_mul =
    Str.regexp {|mul(\([0-9][0-9]?[0-9]?\),\([0-9][0-9]?[0-9]?\))|}

let find_do = Str.regexp {|do()|}
let find_dont = Str.regexp {|don\'t()|}

let rec get_first_res str index = 
    if index >= String.length str then
        0
    else
        try
        (
            let index = Str.search_forward find_mul str index in
            let a = int_of_string @@ Str.matched_group 1 str in
            let b = int_of_string @@ Str.matched_group 2 str in
            if a >= 1000 || b >= 1000 then
                    get_first_res str (index + 1)
            else
                a * b + get_first_res str (index + 1)
        )
        with Not_found -> 
            0

let should_include str index = 
    let dont_index = 
        try Str.search_backward find_dont str index
        with Not_found -> String.length str
    in
    let do_index = 
        try Str.search_backward find_do str index
        with Not_found -> String.length str
    in
(*
    Printf.printf "do_index : %d, dont_index: %d\n" do_index dont_index;
*)
    if do_index == String.length str  then
        dont_index == String.length str
    else
        do_index >= dont_index

let rec get_second_res str index = 
    if index >= String.length str then
        0
    else
        try
        (
            let index = Str.search_forward find_mul str index in
            let a = int_of_string @@ Str.matched_group 1 str in
            let b = int_of_string @@ Str.matched_group 2 str in
            let should_include = should_include str index in
(*
            Printf.printf "a: %d, b: %d should_include: %b\n" a b should_include;
*)
            if a >= 1000 || b >= 1000 || not should_include then
                    get_second_res str (index + 1)
            else
                a * b + get_second_res str (index + 1)
        )
        with Not_found -> 
            0

let first = 
    let ch = open_in_bin "../inputs/3.txt" in
    let str = really_input_string ch (in_channel_length ch) in
    close_in ch;
    Printf.printf "first result: %d\n" @@ get_first_res str 0

let second = 
    let ch = open_in_bin "../inputs/3.txt" in
    let str = really_input_string ch (in_channel_length ch) in
    close_in ch;
    Printf.printf "first result: %d\n" @@ get_second_res str 0



let () =
    first;;
    second;;
