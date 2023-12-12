open Stdlib.Scanf

(* sscanf source fmt f -> source is a string to be read *)

(* bscanf source fmt f -> source is a I/O channel (e.g. Scanf.Scanning.stdin) *)

let () =
    bscanf Scanning.stdin "%d" (fun x -> Printf.printf "%d\n" (x + 1))

let () =
    bscanf (Scanning.open_in "input.txt") "%d" (fun x -> Printf.printf "%d\n" (x * 10))

let () =
    Printf.printf "%d\n" (sscanf "21" "%d" (fun x -> x * 2))
