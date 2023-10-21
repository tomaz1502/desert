exception IndexTooLarge

let () = Random.self_init ()

let rec removeNth (n: int) (xs: 'a list) =
    match n, xs with
    | _, [] -> raise IndexTooLarge
    | 0, _ :: xs -> xs
    | _, x :: xs -> x :: removeNth (n - 1) xs

let rec permutation (xs: 'a list): 'a list =
    match xs with
    | [] -> []
    | _ ->
        let l = List.length xs in
        let idx = Random.int l in
        let r = List.nth xs idx in
        r :: permutation (removeNth idx xs)

let test = permutation ["a"; "b"; "c"; "d"; "e"; "f"];;
