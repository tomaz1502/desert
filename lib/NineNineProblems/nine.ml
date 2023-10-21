let pack (xs : 'a list) : 'a list list =
    let rec pack' (acc : 'a list list) (xs : 'a list) =
        match acc, xs with
        | _, [] -> acc
        | [], x::xs -> pack' [[x]] xs
        | [] :: t, x :: xs -> pack' ([x]::t) xs
        | (h :: t) :: t', x :: xs ->
            if x = h then pack' ((x :: h :: t) :: t') xs
            else pack' ((x :: []) :: (h :: t) :: t') xs
    in List.rev (pack' [] xs)

let test = pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;

let packAnswer list =
    let rec packAnswer' (acc : 'a list) (accs : 'a list list) (xs : 'a list) =
      match xs with
      | [] -> []
      | [x] -> (x :: acc) :: accs
      | x1 :: (x2 :: _ as t) ->
         if x1 = x2 then packAnswer' (x1 :: acc) accs t
         else packAnswer' [] ((x1 :: acc) :: accs) t  in
    List.rev (packAnswer' [] [] list);;

let test2 = pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;
