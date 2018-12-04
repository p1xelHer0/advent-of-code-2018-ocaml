open Lib

(* ----- puzzle_1 ----- *)

let parsed_input =
  Utils.read_file "./bin/day_1/input" |> List.map int_of_string

let puzzle_1 =
  parsed_input |> List.fold_left ( + ) 0
  |> Printf.printf "day_1_puzzle_1: %d\n%!"

(* ----- puzzle_2 ----- *)

let solve_puzzle_2 x =
  let module IntSet = Set.Make (struct
    let compare = Pervasives.compare

    type t = int
  end) in
  let rec loop value acc_freq set =
    match value with
    | [] -> loop x acc_freq set
    | inputFreq :: rest ->
        let new_freq = acc_freq + inputFreq in
        if IntSet.mem new_freq set then new_freq
        else loop rest new_freq (IntSet.add new_freq set)
  in
  loop x 0 IntSet.empty

let puzzle_2 =
  parsed_input |> solve_puzzle_2 |> Printf.printf "day_1_puzzle_2: %d\n%!"
