open Util
open Angstrom

type color = Red | Green | Blue;;

type round = (color * int) list;;

type game = { id: int; rounds: round list  };;

let is_digit = function '0'..'9' -> true | _ -> false
let is_alfa  = function 'a'..'z' -> true | 'A'..'Z' -> true | _ -> false
let is_white_space = function ' ' -> true | _ -> false

let not_white_space_p = take_while (fun x -> x |> is_white_space |> not)
let white_space_p     = take_while is_white_space
let name_p            = take_while is_alfa
let natural_p         = take_while1 is_digit

let color_p =
    name_p >>= function
             | "red"   -> return Red
             | "green" -> return Green
             | "blue"  -> return Blue
             | _       -> fail "failed to parse color";;

let draw =
    skip_many (char ' ') *>
    natural_p >>= fun n ->
    white_space_p *>
    color_p >>= fun c ->
    return (c, int_of_string n);;

let round_p = many1 (draw <* (skip_many (char ',')))

let aux =
    peek_char >>=
    function
    | None     -> return ()
    | Some ';' -> advance 1 *> return ()
    | _ -> fail ""

let game_p =
    not_white_space_p *>
    white_space_p     *>
    natural_p         >>= fun game_id ->
    not_white_space_p *>
    white_space_p     *>
    many (round_p <* aux) >>= fun r ->
        return { id = (int_of_string game_id); rounds = r };;

let calc (r : round) : (int * int * int) =
    (assoc_def Red r 0, assoc_def Green r 0, assoc_def Blue r 0)

let calc (g : game) : int =
    let vals = List.map calc g.rounds in
    let (rs, gs, bs) = split3 vals    in
    let r_max = List.fold_left max 0 rs in
    let g_max = List.fold_left max 0 gs in
    let b_max = List.fold_left max 0 bs in
    r_max * g_max * b_max

let process_line (line : string) : int =
    match parse_string ~consume:Prefix game_p line with
    | Ok    g -> calc g
    | Error _ -> failwith "failed parsing game";;

let solve =
  let lines = read_file "./data/day_two_input" in
  List.fold_left (+) 0 (List.map process_line lines) 
