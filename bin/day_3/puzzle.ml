open Lib

let parsed_input = Utils.read_file "./bin/day_3/input"

(* ----- puzzle_1 ----- *)

type claim = {id: int; x: int; y: int; w: int; h: int}

let parse_claim row =
  Scanf.sscanf row "#%d @ %d,%d: %dx%d" (fun id x y w h -> {id; x; y; w; h})

let claim_input = List.map parse_claim parsed_input

let solve_puzzle_1 cs =
  let module IntIntMap = Map.Make (struct
    let compare = Pervasives.compare

    type t = int * int
  end) in
  (* add or increment the value of key by 1 *)
  let inc key map =
    try
      let value = IntIntMap.find key map in
      IntIntMap.add key (value + 1) map
    with Not_found -> IntIntMap.add key 1 map
  in
  let add_cords_to_map claim map =
    let x = claim.x in
    let y = claim.y in
    let w = claim.w in
    let h = claim.h in
    let x_end = x + w - 1 in
    let y_end = y + h - 1 in
    let rec cols dx dy w h map =
      let rows dx dy w h map =
        if dy < y_end then cols dx (dy + 1) w h map else map
      in
      let next_map = inc (dx, dy) map in
      if dx < x_end then cols (dx + 1) dy w h next_map
      else rows x dy w h next_map
    in
    cols x y w h map
  in
  (* do so for each claim *)
  let rec solve claims map =
    match claims with
    | [] -> map
    | claim :: tl -> solve tl (add_cords_to_map claim map)
  in
  solve cs IntIntMap.empty
  (* we only care about overlapping claims *)
  |> IntIntMap.filter (fun _key value -> value > 1)
  |> IntIntMap.cardinal

let puzzle_1 =
  claim_input |> solve_puzzle_1 |> Printf.printf "day_3_puzzle_1: %d\n%!"

(* ----- puzzle_2 ----- *)

(* checks if two claims are overlapping *)
let overlapping c1 c2 =
  (* B E A U T I F U L   C O D E *)
  let r1x1 = c1.x in
  let r1y1 = c1.y in
  let w1 = c1.w in
  let h1 = c1.h in
  let r1x2 = r1x1 + w1 - 1 in
  let r1y2 = r1y1 + h1 - 1 in
  let r2x1 = c2.x in
  let r2y1 = c2.y in
  let w2 = c2.w in
  let h2 = c2.h in
  let r2x2 = r2x1 + w2 - 1 in
  let r2y2 = r2y1 + h2 - 1 in
  (* google this shit *)
  ((r1x1 >= r2x1 && r1x1 <= r2x2) || (r2x1 >= r1x1 && r2x1 <= r1x2))
  && ((r1y1 >= r2y1 && r1y1 <= r2y2) || (r2y1 >= r1y1 && r2y1 <= r1y2))

let solve_puzzle_2 cs =
  let module IntSet = Set.Make (struct
    let compare = Pervasives.compare

    type t = int
  end) in
  (* get a set of all the ids of claims overlapping *)
  let rec overlappings xs set =
    let rec add_overlapping_to_set start rest set =
      match rest with
      | [] -> set
      | claim :: tl ->
          (* add both ids to set if the overlap *)
          let next_set =
            if overlapping start claim then
              set |> IntSet.add start.id |> IntSet.add claim.id
            else set
          in
          (* continue with the rest of claims *)
          add_overlapping_to_set start tl next_set
    in
    (* check overlapping for claim with each other claim not compared before
     * c1 check [1;2;3]
     * c2 check [2;3]
     * c3 check [3]
     *)
    match xs with
    | [] -> set
    | claim :: tl -> overlappings tl (add_overlapping_to_set claim tl set)
  in
  let overlapping_ids = overlappings cs IntSet.empty in
  let rec solve claims id_set =
    match claims with
    | [] -> failwith "claim should be in list according to puzzle"
    | claim :: tl ->
        (* when we find an id the isnt in the set, we have our match *)
        if IntSet.mem claim.id id_set then solve tl id_set else claim.id
  in
  solve cs overlapping_ids

let puzzle_2 =
  claim_input |> solve_puzzle_2 |> Printf.printf "day_3_puzzle_2: %d\n%!"
