open Batteries
open Stdlib

let convert_to_ints s =
    String.to_seq s
        |> Seq.map (fun x -> int_of_char x - int_of_char '0')


let parse channel = 
    In_channel.input_line channel
        |> fun x -> Option.value x ~default:""
        |> convert_to_ints

let decode input size = 
    let res = Array.make size (-1) in
    let size = DynArray.create() in
    let pos = DynArray.create() in
    let rec put_in_arr id times index = 
        match times with
            | 0 -> index
            | x -> 
                res.(index) <- id;
                put_in_arr id (x - 1) (index + 1)
    in

    let rec inner id index input = 
        match input with 
            | [] -> res
            | [h] -> 
                BatDynArray.add pos index;
                BatDynArray.add size h;
                let _index = put_in_arr id h index in res
            | space_used::free_space::t ->
                BatDynArray.add pos index;
                BatDynArray.add size space_used;
                let index = put_in_arr id space_used index in
                let index = put_in_arr (-1) free_space index in
                inner (id + 1) index t
    in
    let res = inner 0 0 input in
(*
    DynArray.iter (Printf.printf "size: %d ") size;
    Printf.printf "\n";
    DynArray.iter (Printf.printf "pos: %d ") pos;
*)
    (size, pos, res)

let make_compact arr =
    let rec inner left right =
        if left >= right then
            arr
        else if arr.(left) != -1 then
            inner (left + 1) right
        else if arr.(right) == -1 then
            inner left (right - 1)
        else
            let left_val = arr.(left) and right_val = arr.(right) in
            arr.(left) <- right_val;
            arr.(right) <- left_val;
            inner (left + 1) (right - 1)
    in
    inner 0 (Array.length arr - 1)

    
let find_space arr len =
    let start = ref 0 in
    let i = ref 0 in

    while !i - !start < len && !i < Array.length arr - 1 do
        if arr.(!i) <> -1 then (
        i := !i + 1;
        start := !i)
        else
        i := !i + 1
    done;

    if !i - !start >= len then
        Some !start
    else
        None

let arr_swap arr left right = 
    let temp = arr.(left) in
    arr.(left) <- arr.(right);
    arr.(right) <- temp;
    ()
    

let move_whole_files (size, pos, arr) =
  let i = ref (DynArray.length size - 1) in

  while !i > 1 do
    let start = DynArray.get pos !i and len = DynArray.get size !i in
    match find_space arr len with
    | Some empty_start when empty_start < start ->
        for i = 0 to len - 1 do
            arr_swap arr (empty_start + i) (start + i);
        done;
        i := !i - 1
    | Some _ -> i := !i - 1
    | _ -> i := !i - 1
  done;
  arr


let calculate_checksum arr =
    let rec inner i acc =
        if i == Array.length arr then
            acc
        else if arr.(i) == -1 then
            inner (i + 1) acc
        else
            inner (i + 1) (acc + arr.(i) * i)
    in
    inner 0 0

let first filename = 
    let input = open_in filename
        |> parse 
        |> List.of_seq
    in
    let total_size_required = List.fold_left ( + ) 0 input in
    let res = decode input total_size_required 
        |> fun (_a, _b, c) -> make_compact c
        |> calculate_checksum
    in
    Printf.printf "first_result: %d\n" res;
    ()

let second filename = 
    let input = open_in filename
        |> parse 
        |> List.of_seq
    in
    let total_size_required = List.fold_left ( + ) 0 input in
    let res = decode input (total_size_required  + 1)
        |> move_whole_files
        |> calculate_checksum
    in
    Printf.printf "second: %d\n" res;
    ()

let () =
    first "../inputs/9.txt";;
    second "../inputs/9.txt";;
