type 'a rle =
    | One of 'a
    | Many of (int * 'a)

let encode (xs: 'a list): 'a rle list =
    let mkRle i a =
        if i = 1 then One a else Many (i, a)
    in
    let rec encode' (cnt: int) (x: 'a) (xs: 'a list) =
        match xs with
        | [] -> [mkRle cnt x]
        | y :: xs ->
            if x = y then encode' (cnt + 1) y xs
            else (mkRle cnt x) :: (encode' 1 y xs) 
    in encode' 1 (List.hd xs) (List.tl xs)

let test = encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
