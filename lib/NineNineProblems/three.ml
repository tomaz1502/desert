let rec at (i : int) (xs : 'a list) : 'a option =
    match xs, i with
    | [],    _ -> None
    | x::_,  0 -> Some x
    | _::xs, _ -> at (i - 1) xs

