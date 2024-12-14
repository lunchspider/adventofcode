let explode s =
    Array.init (String.length s) (String.get s)

let search_first sub s =
    let rows = Array.length s in
    let cols = Array.length @@ s.(0) in
    let acc = ref 0 in
    let directions = [(1, 0); (-1, 0); (0, 1); (0, -1); (1, 1); (-1, -1); (1, -1); (-1, 1)] in
    let rec find i j pos di dj = 
        if i < 0 
            || i >= rows 
            || j < 0 
            || j >= cols 
            || s.(i).(j) != String.get sub pos
        then
            0
        else
            if pos == String.length sub - 1 then
                1 
            else 
                find (i + di) (j + dj) (pos + 1) di dj
    in
    for i = 0 to rows - 1 do
        for j = 0 to cols - 1 do
            let res = List.map (fun (di, dj) -> find i j 0 di dj) directions
                |> List.fold_left ( + ) 0 in
            acc := !acc + res
        done;
    done;
    !acc

let search_second s =
    let rows = Array.length s and cols = Array.length s.(0) in
    let acc = ref 0 in
    for i = 1 to rows - 2 do
        for j = 1 to cols - 2 do
            if s.(i).(j) == 'A' 
                && (
                    (
                        s.(i + 1).(j + 1) == 'S' 
                        && s.(i - 1).(j - 1) == 'M'
                    )
                    || (
                        s.(i + 1).(j + 1) == 'M' 
                        && s.(i - 1).(j - 1) == 'S'
                    )
                )
                && (
                    (
                        s.(i - 1).(j + 1) == 'S' 
                        && s.(i + 1).(j - 1) == 'M'
                    )
                    || (
                        s.(i - 1).(j + 1) == 'M' 
                        && s.(i + 1).(j - 1) == 'S'
                    )
                )
            then
                acc := !acc  + 1
        done;
    done;
    !acc


let parse channel =
    In_channel.input_lines channel
        |> List.map explode
        |> Array.of_list

let first =
    let first_res = open_in "../inputs/4.txt"
        |> parse
        |> search_first "XMAS" in

    Printf.printf "first_result: %d\n" first_res

let second =
    let second_res = open_in "../inputs/4.txt"
        |> parse
        |> search_second  in

    Printf.printf "second_result: %d\n" second_res

let () = 
    first;;
    second;;
