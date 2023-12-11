open Util
open Angstrom

type color = Red | Green | Blue;;

type round = (color * int) list;;

type game = { id: int; rounds: round list  };;

let is_ok (r : round) : bool =
    let check = function
        | (Red,   x) -> x <= 12
        | (Green, x) -> x <= 13
        | (Blue,  x) -> x <= 14 in
    all check r

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

let parse_game =
    not_white_space_p *>
    white_space_p     *>
    natural_p   >>= fun game_id ->
    not_white_space_p *>
    white_space_p     *>
    many (round_p <* aux) >>= fun r ->
        return { id = (int_of_string game_id); rounds = r };;

let process_line (line : string) : int =
    match parse_string ~consume:Prefix parse_game line with
    | Ok g -> if all is_ok g.rounds then g.id else 0
    | Error _ -> failwith "failed parsing game";;

let solve =
  let lines = read_file "./data/day_two_input" in
  List.fold_left (+) 0 (List.map process_line lines) 
