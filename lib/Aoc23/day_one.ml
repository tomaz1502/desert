open Util

let conv_ascii (c : char) = int_of_char c - int_of_char '0'

let f (line : string) =
    let line_chars = List.of_seq (String.to_seq line) in
    let x = ref (-1) in
    let y = ref (-1) in
    for i = 0 to List.length line_chars - 1 do
        let c = List.nth line_chars i in
        let c_is_digit = c >= '0' && c <= '9' in
        if c_is_digit then
            y := conv_ascii c;
        if c_is_digit && !x == -1 then
            x := conv_ascii c
    done;
    10 * !x + !y

let solve =
    let lines = read_file "./lib/Aoc23/day_one_input" in
    let sum = ref 0 in
    for i = 0 to List.length lines - 1 do
        sum := !sum + (f (List.nth lines i));
    done;
    print_int !sum;
    print_newline ()
