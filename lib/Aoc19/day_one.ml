open Util

let rec aux x = 
    let y = (x / 3) - 2 in
    if y <= 0 then 0 else y + aux y

let solve =
    let lines = read_file "./lib/Aoc19/day_one_input" in
    let handle line = aux (int_of_string line) in
    let solved_lines = List.map handle lines in
    let sum = List.fold_left (+) 0 solved_lines in
    print_int sum;
    print_newline ();
