module Board = struct 
    open Advent
    type tile = 
        | Obstacle 
        | Empty
        | Gaurd of Direction.dir
        | Visited of Direction.dir

    let of_char x = match x with
        | '#' -> Obstacle
        | '.' -> Empty
        | x -> Gaurd (Direction.of_char x)

    let to_string d = match d with
        | Obstacle -> "#"
        | Empty-> "."
        | Gaurd x -> Direction.to_string x
        | Visited x -> Direction.to_string x

    let find_gaurd map = 
        let rows = Array.length map and cols = Array.length map.(0) in
        let rec find_inner i j = 
            if i == rows then
                (0, 0, Direction.Up)
            else
                if j == cols then
                    find_inner (i + 1) 0
                else
                    match map.(i).(j) with
                        | Gaurd(x) -> (i, j, x)
                        | _ -> find_inner i (j + 1)
        in
        find_inner 0 0 

    let print_board map =
        let rows = Array.length map and cols = Array.length map.(0) in
        let rec print_inner i j = 
            if i == rows then
                ()
            else
                if j == cols then begin
                    Printf.printf "\n";
                    print_inner (i + 1) 0
                end
                else begin
                    Printf.printf "%s" @@ to_string map.(i).(j);
                    print_inner i (j + 1)
                end
        in
        print_inner 0 0 
        
end 

let count_position map start_x start_y dir = 
    let rows = Array.length map and cols = Array.length map.(0) in
    let rec count_inner i j dir =
        let d = Advent.Direction.to_di dir in
        if i < 0 || j < 0 || i >= rows || j >= cols then
            0
        else
            match map.(i).(j) with
                | Board.Obstacle ->
                    (*go back and turn right*)
                    count_inner (i - d.x) (j - d.y) (Advent.Direction.turn_right dir)
                | Board.Empty ->
                    map.(i).(j) <- Board.Visited(dir);
                    1 + count_inner (i + d.x) (d.y + j) dir
                | Board.Visited(_) ->
                    count_inner (i + d.x) (d.y + j) dir
                | _ -> assert false
    in
    count_inner start_x start_y dir

let count_loops_locations map start_x start_y dir =
    let rows = Array.length map and cols = Array.length map.(0) in
    let res = ref 0 in

(*
    let rec reset_board i j = 
        if i >= rows then
            ()
        else
            if j >= cols then
                reset_board (i + 1) 0
            else
                match map.(i).(j) with
                    | Board.Visited(_) ->
                        map.(i).(j) <- Board.Empty;
                        reset_board i (j + 1)
                    | _ -> reset_board i (j + 1)
    in
*)

    let rec is_looping i j dir visited =
        let d = Advent.Direction.to_di dir in
        if i < 0 || j < 0 || i >= rows || j >= cols then
            0
        else
            if visited >= 10000 then
                1
            else
                match map.(i).(j) with
                    | Board.Obstacle ->
                        is_looping (i - d.x) (j - d.y) (Advent.Direction.turn_right dir) visited
                    | Board.Empty -> 
                        is_looping (i + d.x) (j + d.y) (dir) (visited + 1)
                    | _ -> assert false
    in

    let rec put_obstacle i j =
        if i >= rows then
            0
        else
            if j >= cols then
                put_obstacle (i + 1) 0
            else
                match map.(i).(j) with
                    | Board.Obstacle ->
                        put_obstacle i (j + 1)
                    | Board.Empty ->
                        if i == start_x && j == start_y then begin
                            let is_loop = is_looping start_x start_y dir 0 in
                            res := !res + is_loop;
                            put_obstacle i (j + 1) 
                        end
                        else begin
                            map.(i).(j) <- Board.Obstacle;
                            let is_loop = is_looping start_x start_y dir 0 in
                            map.(i).(j) <- Board.Empty;
                            res := !res + is_loop;
                            put_obstacle i (j + 1)
                        end
                    | _ -> assert false
    in
    let _ = put_obstacle 0 0 in
    !res

let string_to_array str =
    Array.init (String.length str) (fun i -> String.get str i |> Board.of_char)

let parse channel =
    In_channel.input_lines channel
        |> List.map string_to_array
        |> Array.of_list

let first filename = 
    let map = open_in filename
        |> parse in
    let (start_x, start_y, dir) = Board.find_gaurd map in
    map.(start_x).(start_y) <- Board.Empty;
    let res = count_position map start_x start_y dir in
    Board.print_board map;
    Printf.printf "first_result: %d\n" res

let second filename = 
    let map = open_in filename
        |> parse in
    let (start_x, start_y, dir) = Board.find_gaurd map in
    map.(start_x).(start_y) <- Board.Empty;
    let res = count_loops_locations map start_x start_y dir in
    Board.print_board map;
    Printf.printf "second_result: %d\n" res

let () =
    first "../inputs/6.txt";
    second "../inputs/6.txt";
