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

let rec drop (n : int) (xs : 'a list) : 'a list =
    match n, xs with
    | 0, _        -> xs
    | _, []       -> []
    | _, (_::xs') -> drop (n - 1) xs'

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

