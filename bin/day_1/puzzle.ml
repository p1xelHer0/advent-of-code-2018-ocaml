open Lib

let parsed_input =
  Utils.read_file "./bin/day_1/input" |> List.map int_of_string

let puzzle_1 =
  parsed_input |> List.fold_left ( + ) 0 |> Printf.printf "puzzle_1: %d\n%!"

let solve_puzzle_2 x =
  let module IntSet = Set.Make (struct
    let compare = Pervasives.compare

    type t = int
  end) in
  let rec loop value acc_freq set =
    match value with
    (* we reached the end of the list, start at the beginning *)
    | [] -> loop x acc_freq set
    | inputFreq :: rest ->
        let new_freq = acc_freq + inputFreq in
        (* we found our duplicate *)
        if IntSet.mem new_freq set then new_freq
        (* else, continue *)
        else loop rest new_freq (IntSet.add new_freq set)
  in
  loop x 0 IntSet.empty

let puzzle_2 =
  parsed_input |> solve_puzzle_2 |> Printf.printf "puzzle_2: %d\n%!"
