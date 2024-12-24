module Machine = struct
    type point = {mutable x: int; mutable y: int}

    type t = {
        a: point;
        b: point;
        prize: point;
    }

    let parse_a str = 
        Scanf.sscanf str "Button A: X+%d, Y+%d" (fun x y -> { x = x; y = y})

    let parse_b str = 
        Scanf.sscanf str "Button B: X+%d, Y+%d" (fun x y -> { x = x; y = y})

    let parse_prize str = 
        Scanf.sscanf str "Prize: X=%d, Y=%d" (fun x y -> { x = x; y = y})

    let parse a b prize =
        { a = parse_a a; b = parse_b b; prize = parse_prize prize }

    let to_str t =
        Printf.sprintf 
            "Button A: X+%d, Y+%d\nButton B: X+%d, Y+%d\nPrize: X=%d, Y=%d\n" 
            t.a.x t.a.y t.b.x t.b.y t.prize.x t.prize.y

    let solve self =
        let m = float(self.prize.y * self.a.x - self.prize.x * self.a.y) /. float(self.b.y * self.a.x - self.b.x * self.a.y) in
        let n = (float(self.prize.x) -. m *. float(self.b.x)) /.
            (float(self.a.x)) in
        (m, n)

    let is_solution_correct (m, n) =
        if m < 0. 
            || n < 0. 
            || (Float.round (m *. 100.) |> int_of_float) mod 100 != 0
            || (Float.round (n *. 100.) |> int_of_float) mod 100 != 0 
        then
            false
        else
            true

    let add_to_prize v t =
        t.prize.x <- t.prize.x + v;
        t.prize.y <- t.prize.y + v;
        t
end

let parse channel = 
    let lines = In_channel.input_lines channel 
    and res = Dynarray.create() in
    let rec aux l = 
        match l with 
        | [] -> ()
        | a::b::prize::_empty::t ->
            Dynarray.add_last res @@ Machine.parse a b prize;
            aux t
        | a::b::prize::t ->
            Dynarray.add_last res @@ Machine.parse a b prize;
            aux t
        | _ -> assert false
    in
    aux lines;
    close_in channel;
    res

let first filename = 
    open_in filename
        |> parse
        |> Dynarray.map Machine.solve
        |> Dynarray.filter Machine.is_solution_correct
        |> Dynarray.filter (fun (n, m) -> n < 100. && m < 100.)
        |> Dynarray.map (fun (n , m) -> int_of_float(m) * 3 + int_of_float(n))
        |> Dynarray.fold_left ( + ) 0
        |> Printf.printf "first_result: %d\n";
    ()

let second filename = 
    open_in filename
        |> parse
        |> Dynarray.map (Machine.add_to_prize 10000000000000)
        |> Dynarray.map Machine.solve
        |> Dynarray.filter Machine.is_solution_correct
        |> Dynarray.map (fun (n , m) -> int_of_float(m) * 3 + int_of_float(n))
        |> Dynarray.fold_left ( + ) 0
        |> Printf.printf "second_result: %d\n";
    ()
let () =
    first "../inputs/13.txt";;
    second "../inputs/13.txt";;

