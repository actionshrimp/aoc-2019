let fold_file_lines name f init =
  let c_in = Pervasives.open_in name in
  let rec aux acc =
    try
      let line = Pervasives.input_line c_in in
      aux (f acc line)
    with
    | End_of_file ->
        close_in c_in ;
        acc
  in
  aux init
