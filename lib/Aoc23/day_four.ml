open Angstrom
open Util

module SI = Set.Make (struct
                        type t = int
                        let compare = compare
                      end);;

type card = (SI.t * SI.t);;

let calc (c : card) : int =
  match c with
  | (need, have) ->
    let inter = SI.inter need have in
    1 lsl (SI.cardinal inter - 1)

let nat_list_p = many (natural_p <* white_space_p)

let card_p =
  not_colon_p   *>
  advance 1     *>
  white_space_p *>
  nat_list_p >>= fun xs ->
  white_space_p *>
  pipe_p        *>
  white_space_p *>
  nat_list_p >>= fun xs' ->
  return (xs, xs');;

let print_list (xs : int list) =
  List.iter (fun x -> Printf.printf "%d " x) xs;
  print_newline ()

let answer: int =
  let lines = read_file "./test/day_four_input_cp" in
  let parser = parse_string ~consume:Prefix card_p in
  let vals = List.map parser lines in
  List.fold_left (fun acc res ->
    match res with
      | Result.Ok (xs1, xs2) ->
        let xs1' = List.map int_of_string xs1 in
        let xs2' = List.map int_of_string xs2 in
        let c = SI.of_list xs1', SI.of_list xs2' in
        acc + calc c
      | Result.Error _ -> failwith "unreachable") 0 vals

