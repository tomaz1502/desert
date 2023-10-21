exception Unreachable

type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or  of bool_expr * bool_expr

let rec eval2 (val1 : (string * bool)) (val2 : (string * bool)) (e : bool_expr) : bool =
    match e with
    | Var s ->
        if s = fst val1 then snd val1 else snd val2
    | Not e' -> not (eval2 val1 val2 e')
    | And (e1, e2) -> (eval2 val1 val2 e1) && (eval2 val1 val2 e2)
    | Or  (e1, e2) -> (eval2 val1 val2 e1) || (eval2 val1 val2 e2);;

let table2 (id1 : string) (id2 : string) (e : bool_expr) : (bool * bool * bool) list =
    let vals = [ [(id1, false), (id2, false)]
               ; [(id1, false), (id2, true)]
               ; [(id1, true), (id2, false)]
               ; [(id1, true), (id2, true)]
               ]
    in let go = function
        | [v1, v2] -> (snd v1, snd v2, eval2 v1 v2 e)
        | _        -> raise Unreachable
    in List.map go vals;;

let test = table2 "a" "b" (And (Var "a", Or (Var "a", Var "b")));;
