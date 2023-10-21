let compress (xs: 'a list) =
    let rec compress' (acc : 'a list) (xs : 'a list) =
        match acc, xs with
        | acc, [] -> acc
        | [], x::xs' -> compress' [x] xs'
        | h::t, x::xs' ->
            let newAcc =
                if x = h then h :: t
                else x :: h :: t
            in compress' newAcc xs'
    in List.rev (compress' [] xs);;

let test = compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

let compressAnswer = function
    | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
    | smaller -> smaller;;

let test2 = compressAnswer ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
