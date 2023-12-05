open Util

let conv_ascii (c : char) = int_of_char c - int_of_char '0'

let numbers = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"]

let idx (n : string) = List.find_index (fun x -> x = n) numbers

let update (v : int) (x : int ref) (y : int ref) =
    if !x = -1 then x := v;
    y := v

let f (line : string) =
    let line_chars = List.of_seq (String.to_seq line) in
    let x = ref (-1) in
    let y = ref (-1) in
    for i = 0 to List.length line_chars - 1 do
        let c = List.nth line_chars i in
        if c >= '0' && c <= '9' then
            update (conv_ascii c) x y
        else
            let curr = ref "" in
            for j = i to i + 4 do
                if j < List.length line_chars then
                    let c_iter = List.nth line_chars j in
                    curr := !curr ^ (String.make 1 c_iter);
                    match idx !curr with
                    | None   -> ()
                    | Some i -> update (i + 1) x y
            done;
    done;
    10 * !x + !y

let solve =
    let lines = read_file "./lib/Aoc23/day_one_input" in
    let sum = ref 0 in
    for i = 0 to List.length lines - 1 do
        sum := !sum + (f (List.nth lines i));
    done;
    (* print_int !sum; *)
    (* print_newline () *)
