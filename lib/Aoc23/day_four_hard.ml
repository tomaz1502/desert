open Angstrom
open Util

module SI = Set.Make (struct
                        type t = int
                        let compare = compare
                      end);;

type card = (SI.t * SI.t);;

let inter_card (c : card) : int =
  match c with
  | (need, have) ->
    let inter = SI.inter need have in
    SI.cardinal inter

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

let update (occs : int array ref) (i : int) (posis : int) (amt : int) : unit =
  for j = i + 1 to i + posis do
    !occs.(j) <- !occs.(j) + amt
  done

let solution: int =
  let lines = read_file "./data/day_four_input" in
  let parser = parse_string ~consume:Prefix card_p in
  let vals = List.map parser lines in
  let occs = ref (Array.make (List.length lines) 1) in
  let answer = ref 0 in
  List.iteri (fun i res ->
    match res with
      | Result.Ok (xs1, xs2) ->
        let xs1' = List.map int_of_string xs1 in
        let xs2' = List.map int_of_string xs2 in
        let posis = inter_card (SI.of_list xs1', SI.of_list xs2') in
        let amt = !occs.(i) in
        answer := !answer + amt;
        update occs i posis amt
      | Result.Error _ -> failwith "unreachable") vals;
  !answer
