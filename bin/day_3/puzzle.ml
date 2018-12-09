open Lib

let parsed_input = Utils.read_file "./bin/day_3/input"

(* ----- puzzle_1 ----- *)

type rect = {x1: int; y1: int; x2: int; y2: int}

type fabric = {id: int; rect: rect}

let parse_fabric row =
  Scanf.sscanf row "#%d @ %d,%d: %dx%d" (fun id offset_x offset_y w h ->
      { id
      ; rect=
          { x1= offset_x + 1
          ; y1= offset_y + 1
          ; x2= w + offset_x - 1
          ; y2= h + offset_y - 1 } } )

let overlapping f1 f2 =
  f1.x1 < f1.x2 && f1.x2 > f2.x1 && f1.y1 > f2.y2 && f1.y2 < f2.y1

let fabric_input = List.map parse_fabric parsed_input

module IntMap = Map.Make (struct
  let compare = Pervasives.compare

  type t = int
end)

let solve_puzzle_1 xs =
  let rec overlappings xs map =
    let rec find_overlap start rest overlaps =
      match rest with
      | [] -> overlaps
      | fabric :: fabrics ->
          let next_list =
            if overlapping start.rect fabric.rect then overlaps @ [fabric.id]
            else overlaps
          in
          find_overlap start fabrics next_list
    in
    match xs with
    | [] -> map
    | hd :: tl ->
        overlappings tl (IntMap.add hd.id (find_overlap hd tl []) map)
  in
  let map = overlappings xs IntMap.empty in
  map

let o = fabric_input |> solve_puzzle_1

(* let puzzle_1 = *)
(*   fabric_input |> solve_puzzle_1 |> IntMap.find 1 |> List.hd *)
(*   |> Printf.printf "%d\n%!" *)
