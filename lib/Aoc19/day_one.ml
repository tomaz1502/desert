let () =
    let rec read_file rc : string list =
        try
            let l = input_line rc in
            let r = read_file rc in
            l :: r
        with End_of_file -> []
    in
    let rc = open_in "day_one_small" in
    let lines = read_file rc in
    let rec f x =
        let y = (x / 3) - 2 in
        if y <= 0 then 0 else y + f y
    in
    let sum = List.fold_left (fun acc line -> acc + f (int_of_string line)) 0 lines in
    print_int sum;
    print_newline ();
    close_in rc;;
