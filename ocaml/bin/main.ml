module T = Tablecloth

let print_day ~day ~part_1 ?part_2 () =
  print_endline (Printf.sprintf "Day %d" day) ;
  print_endline (Printf.sprintf "Part 1: %d" part_1) ;
  match part_2 with
  | None ->
      ()
  | Some p2 ->
      print_endline (Printf.sprintf "Part 2: %d" p2)


(* module type Aoc_day.................... *)

module Day1 = struct
  let part_1 () =
    Aoc2019.Util.fold_file_lines
      "../rust/input/2019/day1.txt"
      (fun acc line -> line |> int_of_string |> fun x -> acc + (x / 3) - 2)
      0


  let rec fuel_cost x =
    let cost = (x / 3) - 2 in
    if cost < 0 then 0 else cost + fuel_cost cost


  let part_2 () =
    Aoc2019.Util.fold_file_lines
      "../rust/input/2019/day1.txt"
      (fun acc line -> line |> int_of_string |> fun x -> acc + fuel_cost x)
      0


  let run () = print_day ~day:1 ~part_1:(part_1 ()) ~part_2:(part_2 ()) ()
end

module Day2 = struct
  let rec run_intcode pos memory =
    let code = memory.(pos) in
    if code == 1
    then (
      let loc_1 = memory.(pos + 1) in
      let loc_2 = memory.(pos + 2) in
      let result = memory.(loc_1) + memory.(loc_2) in
      let out = memory.(pos + 3) in
      memory.(out) <- result ;
      run_intcode (pos + 4) memory )
    else if code == 2
    then (
      let loc_1 = memory.(pos + 1) in
      let loc_2 = memory.(pos + 2) in
      let result = memory.(loc_1) * memory.(loc_2) in
      let out = memory.(pos + 3) in
      memory.(out) <- result ;
      run_intcode (pos + 4) memory )
    else if code == 99
    then memory
    else failwith (Printf.sprintf "unexpected opcode: %d" code)


  let set_gravity_assist_memory noun verb memory =
    memory.(1) <- noun ;
    memory.(2) <- verb


  let init_memory =
    let input =
      Aoc2019.Util.fold_file_lines
        "../rust/input/2019/day2.txt"
        (fun _acc line -> line (* there's only one line in the file *))
        ""
    in
    T.String.split ~on:"," input |> T.List.map ~f:int_of_string


  let try_pair noun verb =
    let memory = init_memory |> T.Array.from_list in
    set_gravity_assist_memory noun verb memory ;
    let memory' = run_intcode 0 memory in
    memory'.(0)


  let part_1 () = try_pair 12 2

  let part_2 () =
    let noun = ref 0 in
    let verb = ref 0 in
    let _ =
      while not (try_pair !noun !verb = 19690720) do
        if !verb >= !noun
        then (
          noun := !noun + 1 ;
          verb := 0 )
        else verb := !verb + 1
      done
    in
    (100 * !noun) + !verb


  let run () = print_day ~day:2 ~part_1:(part_1 ()) ~part_2:(part_2 ()) ()
end

module Day3 = struct
  type direction =
    | Left
    | Right
    | Up
    | Down

  type move = direction * int

  let move (x, y) = function
    | Left, d ->
        (x - d, y)
    | Right, d ->
        (x + d, y)
    | Up, d ->
        (x, y - d)
    | Down, d ->
        (x, y + d)


  type intersec_details =
    { p : int * int
    ; d : int
    }

  let intersections init_as init_bs =
    let rec run_intersections a_total (a_x, a_y) b_total (b_x, b_y) as_ bs =
      match (as_, bs) with
      | [], _ ->
          []
      | (a_dir, a_d) :: other_as, [] ->
          run_intersections
            (a_total + a_d)
            (move (a_x, a_y) (a_dir, a_d))
            0
            (0, 0)
            other_as
            init_bs
      | (a_dir, a_d) :: _other_as, (b_dir, b_d) :: other_bs ->
          let within dx d = dx > 0 && dx < d in
          let intersec =
            match (a_dir, b_dir) with
            | Up, Left when within (b_x - a_x) b_d && within (a_y - b_y) a_d ->
                Some
                  { p = (a_x, b_y)
                  ; d = a_total + b_total + (b_x - a_x) + (a_y - b_y)
                  }
            | Up, Right when within (a_x - b_x) b_d && within (a_y - b_y) a_d
              ->
                Some
                  { p = (a_x, b_y)
                  ; d = a_total + b_total + (a_x - b_x) + (a_y - b_y)
                  }
            | Down, Left when within (b_x - a_x) b_d && within (b_y - a_y) a_d
              ->
                Some
                  { p = (a_x, b_y)
                  ; d = a_total + b_total + (b_x - a_x) + (b_y - a_y)
                  }
            | Down, Right when within (a_x - b_x) b_d && within (b_y - a_y) a_d
              ->
                Some
                  { p = (a_x, b_y)
                  ; d = a_total + b_total + (a_x - b_x) + (b_y - a_y)
                  }
            | Left, Up when within (a_x - b_x) a_d && within (b_y - a_y) b_d ->
                Some
                  { p = (b_x, a_y)
                  ; d = a_total + b_total + (a_x - b_x) + (b_y - a_y)
                  }
            | Left, Down when within (a_x - b_x) a_d && within (a_y - b_y) b_d
              ->
                Some
                  { p = (b_x, a_y)
                  ; d = a_total + b_total + (a_x - b_x) + (a_y - b_y)
                  }
            | Right, Up when within (b_x - a_x) a_d && within (b_y - a_y) b_d
              ->
                Some
                  { p = (b_x, a_y)
                  ; d = a_total + b_total + (b_x - a_x) + (b_y - a_y)
                  }
            | Right, Down when within (b_x - a_x) a_d && within (a_y - b_y) b_d
              ->
                Some
                  { p = (b_x, a_y)
                  ; d = a_total + b_total + (b_x - a_x) + (a_y - b_y)
                  }
            | _ ->
                None
          in
          T.Option.to_list intersec
          @ run_intersections
              a_total
              (a_x, a_y)
              (b_total + b_d)
              (move (b_x, b_y) (b_dir, b_d))
              as_
              other_bs
    in
    run_intersections 0 (0, 0) 0 (0, 0) init_as init_bs


  let distance_from_central_port { p = x, y; _ } =
    Pervasives.abs x + Pervasives.abs y


  let example_1 () =
    intersections
      [ (Right, 8); (Up, 5); (Left, 5); (Down, 3) ]
      [ (Up, 7); (Right, 6); (Down, 4); (Left, 4) ]
    |> T.List.map ~f:distance_from_central_port
    |> T.List.minimum
    |> T.Option.getExn


  let parse_line line =
    line
    |> T.String.split ~on:","
    |> T.List.map ~f:(fun part ->
           match T.String.toList part with
           | 'R' :: d ->
               (Right, int_of_string (T.String.fromList d))
           | 'L' :: d ->
               (Left, int_of_string (T.String.fromList d))
           | 'U' :: d ->
               (Up, int_of_string (T.String.fromList d))
           | 'D' :: d ->
               (Down, int_of_string (T.String.fromList d))
           | c :: _ ->
               failwith (Printf.sprintf "unexpected direction in input: %c" c)
           | [] ->
               failwith "empty input")


  let read_lines () =
    let lines =
      Aoc2019.Util.fold_file_lines
        "../rust/input/2019/day3.txt"
        (fun acc line -> acc @ [ parse_line line ])
        []
    in
    match lines with
    | [ a; b ] ->
        (a, b)
    | _ ->
        failwith "wrong number of lines in input"


  let part_1 () =
    let a, b = read_lines () in
    intersections a b
    |> T.List.map ~f:distance_from_central_port
    |> T.List.minimum
    |> T.Option.getExn


  let distance_travelled { d; _ } = d

  let part_2 () =
    let a, b = read_lines () in
    intersections a b
    |> T.List.map ~f:distance_travelled
    |> T.List.minimum
    |> T.Option.getExn


  let run () = print_day ~day:3 ~part_1:(part_1 ()) ~part_2:(part_2 ()) ()
end

let () = Day3.run ()
