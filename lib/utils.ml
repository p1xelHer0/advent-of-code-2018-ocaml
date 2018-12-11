let replace input output = Str.global_replace (Str.regexp_string input) output

let read_file filename =
  let chan = open_in filename in
  let try_read () = try Some (input_line chan) with End_of_file -> None in
  let rec loop lines =
    match try_read () with
    | Some s -> loop (s :: lines)
    | None -> close_in chan ; List.rev lines
  in
  loop []

let explode s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let string_of_chars chars =
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars ;
  Buffer.contents buf

let cons_uniq tl hd = if List.mem hd tl then tl else hd :: tl

let remove_duplicates xs = List.rev (List.fold_left cons_uniq [] xs)
