open Util

type matrix = char array array;;

let is_symbol (c : char) = c <> '.' && (c < '0' || c > '9')

let is_digit (c : char) = c >= '0' && c <= '9'

let int_of_dig (c : char) = int_of_char c - int_of_char '0'

let is_inside (m : matrix) (i : int) (j : int) : bool =
    if i >= 0 && i < Array.length m then
        let line_length = Array.length (m.(i)) in
        j >= 0 && j < line_length
    else false

let rec build_number (cs : char list) : int =
    match cs with
    | [] -> 0
    | h::t -> int_of_dig h + (10 * build_number t)

let calc (mat : matrix) : int =
    let n = Array.length mat in
    let m = Array.length mat.(0) in
    let accs = Array.make_matrix n m [] in
    for i = 0 to n - 1 do
        let j = ref 0 in 
        while !j < m do
            let curr = ref [] in
            let j_init = !j in
            while !j < m && is_digit mat.(i).(!j) do
                curr := mat.(i).(!j) :: !curr;
                incr j
            done;
            let n = build_number !curr in
            if n > 0 then
                for k = j_init - 1 to !j do
                    for i' = i - 1 to i + 1 do
                        if is_inside mat i' k && mat.(i').(k) == '*' then
                            accs.(i').(k) <- n :: accs.(i').(k);
                    done;
                done;
            incr j
        done;
    done;
    let answer = ref 0 in
    for i = 0 to n - 1 do
        for j = 0 to m - 1 do
            if mat.(i).(j) == '*' then
                match accs.(i).(j) with
                | [n1; n2] -> answer := !answer + n1 * n2
                | _ -> ()
        done;
    done;
    !answer

let solve =
    let lines = read_file "./data/day_three_input" in
    let m = List.map (comp Array.of_seq String.to_seq) lines in
    calc (Array.of_list m)
