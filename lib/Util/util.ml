let read_file (file_name: string) : string list =
    let rec read_chan (rc: in_channel): string list =
        try
            let l = input_line rc in
            let r = read_chan rc in
            l :: r
        with End_of_file -> []
    in
    let chan = open_in file_name in
    let result = read_chan chan in
    close_in chan;
    result

let rec show_list (show: 'a -> string) (xs: 'a list): string =
    match xs with
    | [] -> "\n"
    | hd::tl ->
            show hd ^ " " ^ (show_list show tl)

let scan_int_list (s: string): int list =
    String.split_on_char ' ' s |> List.filter_map int_of_string_opt

let find_index (p : 'a -> bool) (xs : 'a list) : int option =
    let rec aux (i : int) (curr : 'a list) =
        match curr with
        | [] -> None
        | x'::xs' -> if p x' then Some i else aux (i + 1) xs'
    in
    aux 0 xs

let rec take_while (p : 'a -> bool) (xs : 'a list) : 'a list =
    match xs with
    | [] -> []
    | h::t -> if p h then h :: take_while p t else []

let rec drop_while (p : 'a -> bool) (xs : 'a list) : 'a list =
    match xs with
    | [] -> []
    | h::t -> if p h then drop_while p t else xs

(* returns take list, drop list and number of elements dropped *)
let rec cnt_take_drop (p : 'a -> bool) (xs : 'a list) : ('a list * 'a list * int) =
    match xs with
    | [] -> ([], [], 0)
    | h::t ->
        if p h then
            let (xs1, xs2, cnt) = cnt_take_drop p t in
            (h :: xs1, xs2, cnt + 1)
        else ([], xs, 0)

let rec drop (n : int) (xs : 'a list) : 'a list =
    match n, xs with
    | 0, _        -> xs
    | _, []       -> []
    | _, (_::xs') -> drop (n - 1) xs'

let rec take (n : int) (xs : 'a list) : 'a list =
    match n, xs with
    | 0, _    -> []
    | _, []   -> []
    | _, h::t -> h :: take (n - 1) t

let rec all (f : 'a -> bool) (xs : 'a list) : bool =
    match xs with
    | [] -> true
    | x :: xs' -> if not (f x) then false else all f xs'

let minimum (xs: int list): int =
    List.fold_left min Int.max_int xs

let assoc_def (x : 'a) (xs : ('a * 'b) list) (def : 'b) : 'b =
    match List.assoc_opt x xs with
    | None -> def
    | Some b' -> b'

let rec split3 (xs : ('a * 'b * 'c) list) : ('a list) * ('b list) * ('c list) =
    match xs with
    | [] -> ([], [], [])
    | (a,b,c) :: t ->
            let (a', b', c') = split3 t in
            (a::a', b::b', c::c')

let comp f g x = f (g x)
let flip f x y = f y x

let find_indices (p : 'a -> bool) (xs : 'a list) : int list =
    let rec aux i = function
        | [] -> []
        | x::xs' ->
            let r = aux (i + 1) xs' in
            if p x then i :: r else r
    in
    aux 0 xs

let opt_cnt = function
    | None -> 0
    | Some _ -> 1

let is_digit = function '0'..'9' -> true | _ -> false
let is_alfa  = function 'a'..'z' -> true | 'A'..'Z' -> true | _ -> false
let is_white_space = function ' ' -> true | _ -> false
let is_colon = function ':' -> true | _ -> false

let not_white_space_p = Angstrom.take_while (fun x -> x |> is_white_space |> not)
let white_space_p     = Angstrom.take_while is_white_space
let pipe_p            = Angstrom.take_while (function '|' -> true | _ -> false)
let name_p            = Angstrom.take_while is_alfa
let natural_p         = Angstrom.take_while1 is_digit
let not_colon_p       = Angstrom.take_while (comp not is_colon)
