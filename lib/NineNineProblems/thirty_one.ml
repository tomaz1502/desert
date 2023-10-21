let is_prime (n: int): bool =
    let rec check (d: int): bool =
        if d = 1 then true
        else if n mod d = 0 then false
        else check (d - 1)
    in
    match n with
    | 1 -> false
    | 2 -> true
    | 3 -> true
    | _ ->
        check (1 + (Int.of_float (Float.ceil (sqrt (Float.of_int n))))) 
