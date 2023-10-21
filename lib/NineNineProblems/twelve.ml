type 'a rle =
    | One of 'a
    | Many of (int * 'a);;

let rec decode (xs: 'a rle list): 'a list =
    match xs with
    | [] -> []
    | One a :: t -> a :: decode t
    | Many (n, a) :: t ->
        let rec rep i a =
            match i with
            | 0 -> []
            | _ -> a :: rep (i - 1) a
        in rep n a @ decode t;;

let test = decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
