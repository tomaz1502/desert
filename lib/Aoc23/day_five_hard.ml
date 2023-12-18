open Util

type range = { dst: int
             ; src: int
             ; len: int
             } [@@deriving show]

type map = range list [@@deriving show]

type maps = map list [@@deriving show]

let rec step (s: int) (m: map) : int =
    match m with
    | [] -> s
    | {dst; src; len}::tl ->
        if s >= src && s < src + len then
            dst + (s - src)
        else step s tl

let rec nxt_change (s: int) (m: map): int =
    match m with
    | [] -> Int.max_int
    | {src; len; _}::tl ->
        let r = nxt_change s tl in
        if s >= src && s < src + len then
            min r (src + len - s)
        else if s < src then
            min r (src - s)
        else r

let rec read_maps (input: string list): maps =
    let input = drop 1 input in
    let (curr, rest, _) = cnt_take_drop (function | "" -> false | _ -> true) input in
    match curr with
    | [] -> []
    | _::tl ->
        let get_range = function
            | [a; b; c] -> {dst = a; src = b; len = c}
            | _ -> failwith "unexpected input" in
        let curr_map = List.map (comp get_range scan_int_list) tl in
        let other_maps = read_maps rest in
        curr_map :: other_maps

let rec pair_up (xs: 'a list): ('a * 'a) list =
    match xs with
    | [] -> []
    | x1::x2::tl -> (x1, x2) :: pair_up tl
    | _  -> failwith "[pair_up]: odd length"

let join (m: map): (int * int) -> (int * int) = fun (curr_pos, min_change) ->
    let nxt_pos = step curr_pos m in
    let change = nxt_change curr_pos m in
    (nxt_pos, min min_change change)

let rec run (seed_init: int) (seed_max: int) (ms: maps): int =
    (* Printf.printf "seed_init: %d -- seed_max: %d\n" seed_init seed_max; *)
    let (pos, add) = List.fold_left (flip join) (seed_init, Int.max_int) ms in
    if add == Int.max_int then
        pos
    else if seed_init + add >= seed_max then
        pos
    else min pos (run (seed_init + add) seed_max ms)

let () =
    let lines = read_file "./data/day_five_input" in
    match lines with
    | [] -> failwith "unexpected input"
    | hd::tl ->
        let seeds =
            String.split_on_char ' ' hd |>
            drop 1 |>
            List.filter_map int_of_string_opt |>
            pair_up in
        let ms = read_maps tl in
        let vals = List.map (fun (init, len) -> run init (init + len) ms) seeds in
        Printf.printf "%d\n" (minimum vals)
