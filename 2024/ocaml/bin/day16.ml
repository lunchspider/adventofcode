open Advent
open Batteries
open Stdlib
open Format

module Cell = struct
    type t = Empty | Obstacle | Reindeer | End

    let of_char = function 
        '#' -> Obstacle
        | 'S' -> Reindeer
        | '.' -> Empty
        | 'E' -> End
        | _ -> assert false
end
type elem = { point: Advent.point; dir: Direction.dir; cost: int;}

module PriorityQueue = Heap.Make (struct
    type t = elem
    let compare a b = Int.compare a.cost b.cost
end)


type point_with_dir = (Advent.point * Advent.Direction.dir)

module CoordSetWithDir = Set.Make (struct
  type t = point_with_dir

  let compare (a, a_dir) (b, b_dir) = 
    let cmp_x = Int.compare a.x b.x
    and cmp_y = Int.compare a.y b.y 
    and cmp_dir = a_dir = b_dir
    in
    if cmp_x == 0 && cmp_y == 0 && cmp_dir then
        0
    else 1
end)


module Board = struct
    type t = {
        row: int;
        col: int;
        dir: Direction.dir;
        start: point;
        dest: point;
        arr: Cell.t Array.t Array.t
    }

    let of_list l =
        let col = String.length @@ List.nth l 0 
        and row = List.length l
        in

        let board = Array.make_matrix row col Cell.Empty in
        let start_pos = ref { x = 0; y = 0 } in
        let end_pos = ref { x = 0; y = 0 } in

        let rec aux i l = 
            match l with
            | [] -> ()
            | h::t -> 
                for j = 0 to col - 1 do
                    board.(i).(j) <- Cell.of_char h.[j];
                    if board.(i).(j) == Cell.Reindeer then
                        start_pos := { x = i; y = j};
                    if board.(i).(j) == Cell.End then
                        end_pos := { x = i; y = j};
                done;
                aux (i + 1) t
        in

        aux 0 l;
        { row = row; col = col; start = !start_pos; arr = board; dir = Direction.Right; dest = !end_pos}

    let solve board = 
        let pq = ref PriorityQueue.empty
        and map = Hashtbl.create @@ board.row * board.col * 4
        and lowest_cost = ref Int.max_int
        and backtrack = Hashtbl.create @@ board.row * board.col
        and end_state = ref CoordSetWithDir.empty
        in
        pq := PriorityQueue.add {point = board.start; cost = 0; dir = board.dir;} !pq;
        Hashtbl.add map (board.start, board.dir) 0;
        let rec loop pq = 
            if PriorityQueue.size !pq == 0 then
                ()
            else
                let {point; dir; cost} = PriorityQueue.find_min !pq in
                pq := PriorityQueue.del_min !pq;
                if point = board.dest && cost > !lowest_cost then
                    ()
                else if cost > (Hashtbl.find_opt map (point, dir) |> Option.value ~default:Int.max_int) then
                    loop pq
                else
                    if point = board.dest then begin
                        end_state :=  CoordSetWithDir.add (point, dir) !end_state;
                        lowest_cost := min cost !lowest_cost;
                    end;
                    [
                        (Direction.to_di dir ++ point, dir, cost + 1);
                        (point, Direction.turn_right dir, cost + 1000);
                        (point, Direction.turn_left dir, cost + 1000);
                    ]
                        |> List.filter (fun (point, _, _) -> point.x > 0 && point.y > 0 && point.x < board.row && point.y < board.col && board.arr.(point.x).(point.y) != Cell.Obstacle)
                        |> List.iter (fun (new_point, new_dir, new_cost) -> 
                            let old_min = Hashtbl.find_opt map (new_point, new_dir)
                                |> Option.value ~default:Int.max_int 
                            in
                            if old_min >= new_cost then begin
                                let set = Hashtbl.find_opt backtrack (new_point, new_dir) |> Option.value ~default:CoordSetWithDir.empty in
                                let set = if old_min > new_cost then
                                    CoordSetWithDir.empty
                                else
                                    set
                                in
                                Hashtbl.replace map (new_point, new_dir) new_cost;
                                let set = CoordSetWithDir.add (point, dir) set in
                                Hashtbl.replace backtrack (new_point, new_dir) set;
                                pq := PriorityQueue.add { point = new_point; cost = new_cost; dir = new_dir;} !pq;
                            end;
                        ());
                    loop pq
        in
        loop pq;
(*         Hashtbl.iter (fun (point, dir) v -> Printf.printf "(%d %d, %s): %d \n" point.x point.y (Direction.to_string dir) @@ CoordSetWithDir.cardinal v) backtrack; *)
        CoordSetWithDir.iter (fun (point, dir) -> Printf.printf "%d, %d: %s\n" point.x point.y @@ Direction.to_string dir) !end_state;
        let queue = CoordSetWithDir.to_seq !end_state |> Queue.of_seq 
        and seen = CoordSetWithDir.to_seq !end_state |> CoordSetWithDir.of_seq |> ref
        in

        while Queue.length queue <> 0 do
            let key = Queue.take queue in
            Hashtbl.find_opt backtrack key |> Option.value ~default:CoordSetWithDir.empty
                |> CoordSetWithDir.to_seq
                |> Seq.filter (fun x -> CoordSetWithDir.mem x !seen |> not)
                |> Seq.iter (fun x -> 
                    seen := CoordSetWithDir.add x !seen;
                    Queue.add x queue;
                    ()
                )
        done;

        let len = CoordSetWithDir.to_seq !seen |> Seq.map (fun (point, _dir) -> point) |> CoordSet.of_seq |> CoordSet.cardinal in

        (!lowest_cost, len)
end


let parse channel =
    let board = In_channel.input_lines channel |> Board.of_list
    in
    close_in channel;
    board

let first filename =
    let (min_cost, _)  = open_in filename |> parse |> Board.solve in
    Printf.printf "first_result: %d\n" min_cost;
    ()

let second filename =
    let (_, res) = open_in filename |> parse |> Board.solve in
    Printf.printf "second_result: %d\n" res;
    ()

let () =
    first "../inputs/16.txt";;
    second "../inputs/16.txt";;
