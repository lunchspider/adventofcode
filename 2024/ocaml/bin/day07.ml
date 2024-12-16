module Operation = struct
    type t = { 
        result: int;
        inputs: int Array.t
    }[@@deriving field]

    let create result inputs = 
        { result = result; inputs = inputs }

    let is_equal_first op =
        let inputs_len = Array.length op.inputs in
        let rec solve result i =
            if i == inputs_len then
                result == op.result
            else if result > op.result then
                false
            else 
                solve (result * op.inputs.(i)) (i + 1) || solve (result + op.inputs.(i)) (i + 1)
        in
        solve op.inputs.(0) 1

    let combine a b =
        let rec pow res x y =
            if y == 1 then res
            else pow (res * x) x (y - 1)
        in
        let rec num_digits x =
            if x < 10 then 1
            else 1 + num_digits (x / 10)
        in
        let num_b_digits = num_digits b in
        a  * (pow 10 10 num_b_digits) + b

    let is_equal_second op =
        let inputs_len = Array.length op.inputs in
        let rec solve result i =
            if i == inputs_len then
                result == op.result
            else if result > op.result then
                false
            else 
                solve (result * op.inputs.(i)) (i + 1) 
                || solve (result + op.inputs.(i)) (i + 1)
                || solve (combine result op.inputs.(i)) (i + 1)
        in
        solve op.inputs.(0) 1

    let get_result op = op.result
end

let parse channel =
    let parse_line line = 
        let x = Str.split (Str.regexp ": ") line in
        let result = List.nth x 0
            |> int_of_string in
        let inputs = List.nth x 1
            |> String.split_on_char ' '
            |> List.map int_of_string 
            |> Array.of_list
        in
        Operation.create result inputs 
    in
    In_channel.input_lines channel
        |> List.map parse_line

let first filename = 
    let res = open_in filename
        |> parse 
        |> List.filter Operation.is_equal_first
        |> List.fold_left (fun acc x -> (acc + Operation.get_result x)) 0
    in
    Printf.printf "first_result: %d\n" res;
    ()

let second filename = 
    let res = open_in filename
        |> parse 
        |> List.filter Operation.is_equal_second
        |> List.fold_left (fun acc x -> (acc + Operation.get_result x)) 0
    in
    Printf.printf "second_result: %d\n" res;
    ()
let () = 
    first "../inputs/7.txt";;
    second "../inputs/7.txt";;
