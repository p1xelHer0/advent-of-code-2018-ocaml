open Lib

let parsed_input = Utils.read_file "./bin/day_2/input"

let char_input = List.map Utils.explode parsed_input

(* ----- puzzle_1 ----- *)

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
  solve_puzzle_1 char_input |> Printf.printf "day_2_puzzle_1: %d\n%!"

(* ----- puzzle_2 ----- *)

let solve_puzzle_2 (xs : char list list) =
  let rec calc_diff words count =
    match words with
    | _w1, [] -> count
    | [], _w2 -> count
    | hd1 :: tl1, hd2 :: tl2 ->
        if hd1 = hd2 then calc_diff (tl1, tl2) count
        else calc_diff (tl1, tl2) (count + 1)
  in
  let rec solve xs =
    let rec loop start rest =
      match rest with
      | [] -> solve (List.tl xs)
      | word :: words ->
          if calc_diff (start, word) 0 = 1 then (start, word)
          else loop start words
    in
    match xs with hd :: tl -> loop hd tl | _ -> ([], [])
  in
  let remove_common_character words =
    let rec loop words acc =
      match words with
      | [], w2 -> acc @ w2
      | w1, [] -> acc @ w1
      | hd1 :: tl1, hd2 :: tl2 ->
          loop (tl1, tl2) (if hd1 = hd2 then hd1 :: acc else acc)
    in
    loop words [] |> List.rev
  in
  xs |> solve |> remove_common_character |> Utils.string_of_chars

let puzzle_2 =
  solve_puzzle_2 char_input |> Printf.printf "day_2_puzzle_2: %s\n%!"
