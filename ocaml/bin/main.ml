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


  let run () =
    print_endline (Printf.sprintf "Part 1: %d" (part_1 ())) ;
    print_endline (Printf.sprintf "Part 2: %d" (part_2 ()))
end

let () = Day1.run ()
