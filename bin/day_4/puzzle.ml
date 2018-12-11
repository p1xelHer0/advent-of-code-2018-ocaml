open Lib

(* ----- puzzle_1 ----- *)

let parsed_input = Utils.read_file "./bin/day_4/input"

(* sample parsing *)
(* type claim = {id: int; x: int; y: int; w: int; h: int} *)

(* let parse_claim row = *)
(*   Scanf.sscanf row "#%d @ %d,%d: %dx%d" (fun id x y w h -> {id; x; y; w; h}) *)

(* let claim_input = List.map parse_claim parsed_input *)

let solve_puzzle_1 xs = List.length xs

let puzzle_1 =
  parsed_input |> solve_puzzle_1 |> Printf.printf "day_4_puzzle_1: %d\n%!"

(* ----- puzzle_2 ----- *)

let solve_puzzle_2 xs = List.length xs

let puzzle_2 =
  parsed_input |> solve_puzzle_2 |> Printf.printf "day_4_puzzle_2: %d\n%!"
