open Util

type matrix = char list list;;

let is_symbol (c : char) = c <> '.' && (c < '0' || c > '9')

let is_digit (c : char) = c >= '0' && c <= '9'

let int_of_dig (c : char) = int_of_char c - int_of_char '0'

let is_inside (m : matrix) (i : int) (j : int) : bool =
    if i >= 0 && i < List.length m then
        let line_length = List.length (List.nth m i) in
        j >= 0 && j < line_length
    else false

let is_adjacent (m : matrix) (i : int) (j1 : int) (j2 : int) : bool =
    let answer = ref false in
    for i' = i - 1 to i + 1 do
        for j' = j1 - 1 to j2 + 1 do
            if is_inside m i' j' && is_symbol (List.nth (List.nth m i') j') then
                answer := true
        done;
    done;
    !answer

let build_number (cs : char list) : int =
    let rec aux (cs_rev : char list) : int = 
        match cs_rev with
        | [] -> 0
        | h::t -> int_of_dig h + (10 * aux t)
    in
    aux (List.rev cs)

let rec get_number_positions (l : char list) (i : int) : (int * int) list =
    match l with
    | [] -> []
    | '0'..'9'::_ ->
        let (_, r, cnt) = cnt_take_drop is_digit l in
        (i, cnt) :: get_number_positions r (i + cnt)
    | _::_ ->
        let (_, r, cnt) = cnt_take_drop (fun x -> not (is_digit x)) l in
        get_number_positions r (i + cnt)

let calc_pos (m : matrix) (i : int) (j : int) (cnt : int) : int =
    let j2 = j + cnt - 1 in
    if is_adjacent m i j j2 then
        let cs = take cnt (drop j (List.nth m i)) in
        let x = build_number cs in
        x
    else 0

let calc (m : matrix) : int =
    let answer = ref 0 in
    for i = 0 to List.length m - 1 do
        let l = List.nth m i in
        let pos = get_number_positions l 0 in
        let vals = List.map (fun (j, cnt) -> calc_pos m i j cnt) pos in
        let curr = List.fold_left (+) 0 vals in
        answer := !answer + curr
    done;
    !answer

let solve =
    let lines = read_file "./data/day_three_input" in
    let m = List.map (comp List.of_seq String.to_seq) lines in
    calc m
