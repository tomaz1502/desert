type 'a node =
    | One of 'a
    | Many of 'a node list;;

let flatten (ns: 'a node list): 'a list =
    let rec flatten' (n: 'a node): 'a list =
        match n with
        | One x -> [x]
        | Many xs -> List.flatten (List.map flatten' xs)
    in List.flatten (List.map flatten' ns)

let test = flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]
(* ["a"; "b"; "c"; "d"; "e"] *)
