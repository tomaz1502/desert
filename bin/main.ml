open Angstrom

let parser =
  let item = take_while1 (fun c -> c <> ',') in
  let comma_or_end =
      peek_char >>= function
                    | None -> return ()
                    | Some ',' -> advance 1 *> return ()
                    | _ -> fail "" in
  many (item <* comma_or_end)

let input = "apple,orange,banana,bleh"
let result = Angstrom.parse_string ~consume:Prefix parser input

let () =
    match result with
    | Result.Ok values -> Printf.printf "Parsed list: [%s]\n" (String.concat "; " values)
    | Result.Error _ -> Printf.printf "Invalid input\n"
