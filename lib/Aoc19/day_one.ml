let rec read_file rc : string list =
    try
        let l = input_line rc in
        let r = read_file rc in
        l :: r
    with End_of_file -> []

let rec solve x = 
    let y = (x / 3) - 2 in
    if y <= 0 then 0 else y + solve y

let () =
    let rc = open_in "day_one_input" in
    let lines = read_file rc in
    let handle line = solve (int_of_string line) in
    let solved_lines = List.map handle lines in
    let sum = List.fold_left (+) 0 solved_lines in
    print_int sum;
    print_newline ();
    close_in rc;;
