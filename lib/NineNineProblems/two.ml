let rec last_two (xs : 'a list) : ('a * 'a) option =
    match xs with
    | [] | [_]       -> None
    | x1 :: x2 :: [] -> Some (x1, x2)
    | _ :: tl        -> last_two tl
