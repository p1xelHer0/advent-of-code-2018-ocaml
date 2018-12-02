let rec print_list = function
  | [] -> ()
  | hd :: tl -> print_string hd ; print_string " " ; print_list tl

let replace input output = Str.global_replace (Str.regexp_string input) output

let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done ;
    !lines
  with End_of_file -> close_in chan ; List.rev !lines
