open Lib

let parsed_input = Utils.read_file "./bin/day_2/input"

(* convert each string to a list of characters, because im used to haskell... *)
let char_input = List.map Utils.explode parsed_input

(* ----- puzzle_1 ----- *)

let solve_puzzle_1 (xs : char list list) =
  let module CharMap = Map.Make (Char) in
  (* will add new value or increment an old one until it reaches 3 *)
  let inc_until_3 key map =
    try
      let value = CharMap.find key map in
      if value < 3 then CharMap.add key (value + 1) map else map
    with Not_found -> CharMap.add key 1 map
  in
  (* 
   * for each character in a word, we create a map
   * { 'a': 0, 'b': 0, 'c': 0 }
   * where the value is the number of times said char is present in the word
   *)
  let rec count_2_and_3 chars map =
    match chars with
    | [] -> map
    | char :: tl -> count_2_and_3 tl (inc_until_3 char map)
  in
  (* count the numbers of 2's and 3's in a list, then multiply the result *)
  let rec solve twos threes numbers =
    match numbers with
    | [] -> twos * threes
    | 3 :: tl -> solve twos (threes + 1) tl
    | 2 :: tl -> solve (twos + 1) threes tl
    | _hd :: tl -> solve twos threes tl
  in
  xs
  |> List.map (fun x ->
         (* 
          * for each word, we cound the number of characters up to 3
          * remove 1's
          * remove duplicates (according to puzzle instructions)
          *)
         count_2_and_3 x CharMap.empty
         |> CharMap.bindings |> List.map snd
         |> List.filter (fun x -> x > 1)
         |> Utils.remove_duplicates )
  (* we now have a list of [2] | [3] | [2, 3] *)
  (* smoosh the list *)
  |> List.fold_left List.append []
  (* count the number of 2's and 3's, multiply the result, we done! *)
  |> solve 0 0

let puzzle_1 =
  solve_puzzle_1 char_input |> Printf.printf "day_2_puzzle_1: %d\n%!"

(* ----- puzzle_2 ----- *)

let solve_puzzle_2 (xs : char list list) =
  (* calculate the amount of different characters in a word *)
  let rec calc_diff words count =
    match words with
    | _w1, [] -> count
    | [], _w2 -> count
    | hd1 :: tl1, hd2 :: tl2 ->
        (* increment the count by on if hd and tail are different *)
        let new_count = if hd1 = hd2 then count else count + 1 in
        calc_diff (tl1, tl2) new_count
  in
  (* find a pair of words where the difference is 1 character *)
  let rec solve xs =
    let rec loop start rest =
      match rest with
      | [] -> solve (List.tl xs)
      | word :: tl ->
          (* if the difference between the words is 1, we have our match! *)
          if calc_diff (start, word) 0 = 1 then (start, word)
            (* continue looking *)
          else loop start tl
    in
    match xs with hd :: tl -> loop hd tl | _ -> ([], [])
  in
  (* given a pair of words, remove the common characters, return that word *)
  let remove_common_character words =
    let rec loop words acc =
      match words with
      | [], w2 -> acc @ w2
      | w1, [] -> acc @ w1
      | hd1 :: tl1, hd2 :: tl2 ->
          let next_acc = if hd1 = hd2 then hd1 :: acc else acc in
          loop (tl1, tl2) next_acc
    in
    loop words [] |> List.rev |> Utils.string_of_chars
  in
  xs |> solve |> remove_common_character

let puzzle_2 =
  solve_puzzle_2 char_input |> Printf.printf "day_2_puzzle_2: %s\n%!"
