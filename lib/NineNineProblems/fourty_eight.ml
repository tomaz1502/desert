exception Unreachable

type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or  of bool_expr * bool_expr;;

type valuation = (string * bool) list;;

let getVal (v: valuation) (id: string): bool = List.assoc id v

let rec eval (e : bool_expr) (v : valuation) : bool =
    match e with
    | Var s -> getVal v s
    | Not e' -> not (eval e' v)
    | And (e1, e2) -> (eval e1 v) && (eval e2 v)
    | Or  (e1, e2) -> (eval e1 v) || (eval e2 v);;

let table (ids : string list) (e : bool_expr) : (((string * bool) list) * bool) list =
    let rec allVals (ids: string list): valuation list =
        match ids with
        | [] -> []
        | [id] -> [[(id, false)]; [(id, true)]]
        | id :: ids ->
            let mapCons v = List.map (fun xs -> (id, v) :: xs) in
            let recAllVals = allVals ids in
            let l1 = mapCons false recAllVals in
            let l2 = mapCons true recAllVals in
            l1 @ l2
    in let f v = (v, eval e v)
    in List.map f (allVals ids)

let test =
    let a = Var "a" and b = Var "b" and c = Var "c" in
      table ["a"; "b"; "c"] (Or (And (a, Or (b,c)), Or (And (a,b), And (a,c))));;
