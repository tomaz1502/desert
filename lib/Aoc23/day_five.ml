open Util

type range = { dst: int
             ; src: int
             ; len: int
             } [@@deriving show]

type map = range list [@@deriving show]

type maps = map list [@@deriving show]

let rec step (s: int) (m: map) : int =
    (* Printf.printf "current: %d\n" s; *)
    match m with
    | [] -> s
    | {dst; src; len}::tl ->
        if s >= src && s < src + len then
            dst + (s - src)
        else step s tl

let run (init: int) (ms: maps): int =
    List.fold_left step init ms

let rec read_maps (input: string list): maps =
    let input = drop 1 input in
    let (curr, rest, _) = cnt_take_drop (fun s -> s <> "") input in
    match curr with
    | [] -> []
    | _::tl ->
        let get_range = function
            | [a; b; c] -> {dst = a; src = b; len = c}
            | _ -> failwith "unexpected input" in
        let curr_map = List.map (comp get_range scan_int_list) tl in
        let other_maps = read_maps rest in
        curr_map :: other_maps

let () =
    let lines = read_file "./data/day_five_input" in
    match lines with
    | [] -> failwith "unexpected input"
    | hd::tl ->
        let seeds =
            String.split_on_char ' ' hd |>
            drop 1 |>
            List.filter_map int_of_string_opt in
        let ms = read_maps tl in
        (* print_list string_of_int seeds; *)
        (* print_newline (); *)
        let vals = List.map (fun s -> run s ms) seeds in
        (* print_list string_of_int vals *)
        Printf.printf "%d\n" (minimum vals)
