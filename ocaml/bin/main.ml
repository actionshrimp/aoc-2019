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


  let set_gravity_assist_memory memory =
    memory.(1) <- 12 ;
    memory.(2) <- 2


  let part_1 () =
    let input =
      Aoc2019.Util.fold_file_lines
        "../rust/input/2019/day2.txt"
        (fun _acc line -> line (* there's only one line in the file *))
        ""
    in
    let memory =
      Tablecloth.String.split ~on:"," input
      |> Tablecloth.List.map ~f:int_of_string
      |> Tablecloth.Array.from_list
    in
    set_gravity_assist_memory memory ;
    let memory' = run_intcode 0 memory in
    memory'.(0)


  let run () = print_day ~day:2 ~part_1:(part_1 ()) ()
end

let () = Day2.run ()
