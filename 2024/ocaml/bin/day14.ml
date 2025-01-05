open Advent

module Robot = struct
    type t = {
        mutable pos: Advent.point;
        velocity: Advent.point;
    }

    let get_pos self =
        self.pos

    let of_str str =
        Scanf.sscanf str "p=%d,%d v=%d,%d" (fun px py vx vy -> { pos = {x = px; y = py}; velocity = { x = vx; y = vy}})
end

module State = struct
    type t = {
        width: int;
        height: int;
        robots: Robot.t Array.t;
    }

    let create width height robots =
        { width = width; height = height; robots = robots }

    let print state =
        let arr = Array.make_matrix state.height state.width '.' in
        let rec aux i = 
            if i >= Array.length state.robots then
                ()
            else
            (
                arr.(state.robots.(i).pos.y).(state.robots.(i).pos.x) <- '*';
                aux (i + 1)
            )
        in
        aux 0;
        let rec print i = 
            if i >= Array.length arr then
                ()
            else 
            (
                Array.iter (Printf.printf "%c") arr.(i);
                Printf.printf "\n";
                print (i + 1)
            )
        in
        print 0
        
    let simulate steps state = 
        let (mod) x y = ((x mod y) + y) mod y
        in
        let rec aux i =
            if i >= Array.length state.robots then
                ()
            else
            (
                let y = (state.robots.(i).pos.y + steps * state.robots.(i).velocity.y) mod state.height 
                and x = (state.robots.(i).pos.x + steps * state.robots.(i).velocity.x) mod state.width
                in
                state.robots.(i).pos <- { x = x; y = y};
                aux (i + 1)
            )
        in 
        aux 0

    let divide_in_quadrants state =
        let (a, b) = Array.to_seq state.robots
            |> Seq.filter (fun r -> (Robot.get_pos r).x <> state.width / 2)
            |> Seq.partition (fun r -> (Robot.get_pos r).x < state.width / 2)
        in

        let (i, j) = Seq.filter (fun r -> (Robot.get_pos r).y <> state.height / 2) a
        |> Seq.partition (fun r -> (Robot.get_pos r).y < state.height / 2) in

        let (k, l) = Seq.filter (fun r -> (Robot.get_pos r).y <> state.height / 2) b
        |> Seq.partition (fun r -> (Robot.get_pos r).y < state.height / 2) in

        (i, j, k , l)
end

let parse channel =
    let robots = In_channel.input_lines channel |> List.map Robot.of_str in
    close_in channel;
    robots |> Array.of_list 


let first filename =
    let state = open_in filename
    |> parse 
    |> State.create 101 103
    in
    State.simulate 100 state;
    State.divide_in_quadrants state
    |> fun (a, b, c, d) -> Seq.length a * Seq.length b * Seq.length c * Seq.length d
    |> Printf.printf "first_result: %d\n";
    ()

let second filename = 
    let state = open_in filename
        |> parse
        |> State.create 101 103
    in
    let rec aux i =
        State.simulate 1 state;
        let set = Array.to_seq state.robots
            |> Seq.map (fun x -> Robot.get_pos x)
            |> CoordSet.of_seq in
        if CoordSet.cardinal set == Array.length state.robots then
        (
            Printf.printf "\nsecond_result: %d\n" i;
            State.print state;
        )
        else
            aux (i + 1)
    in
    aux 1

let () = 
    first "../inputs/14.txt";;
    second "../inputs/14.txt";;
