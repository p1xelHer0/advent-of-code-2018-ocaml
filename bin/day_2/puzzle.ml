open Lib

(* ----- puzzle_1 ----- *)

let parsed_input_puzzle_1 = Utils.read_file "./bin/day_2/input_puzzle_1"

let char_input_puzzle_1 = List.map Utils.explode parsed_input_puzzle_1

let solve_puzzle_1 (xs : char list list) =
  let module CharMap = Map.Make (Char) in
  let inc_until_3 key map =
    try
      let value = CharMap.find key map in
      if value < 3 then CharMap.add key (value + 1) map else map
    with Not_found -> CharMap.add key 1 map
  in
  let rec loop chars map =
    match chars with
    | [] -> map
    | char :: rest -> loop rest (inc_until_3 char map)
  in
  let rec solve twos threes numbers =
    match numbers with
    | [] -> twos * threes
    | 3 :: tl -> solve twos (threes + 1) tl
    | 2 :: tl -> solve (twos + 1) threes tl
    | _hd :: tl -> solve twos threes tl
  in
  xs
  |> List.map (fun x -> loop x CharMap.empty)
  |> List.map CharMap.bindings
  |> List.map (List.map snd)
  |> List.map (List.filter (fun x -> x > 1))
  |> List.map Utils.remove_duplicates
  |> List.fold_left List.append []
  |> solve 0 0

let puzzle_1 =
  solve_puzzle_1 char_input_puzzle_1 |> Printf.printf "day_2_puzzle_1: %d\n%!"

(* ----- puzzle_2 ----- *)

let parsed_input_puzzle_2 = Utils.read_file "./bin/day_2/input_puzzle_2"

let char_input_puzzle_2 = List.map Utils.explode parsed_input_puzzle_2

let puzzle_2 = ()
