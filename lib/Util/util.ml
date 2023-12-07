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
