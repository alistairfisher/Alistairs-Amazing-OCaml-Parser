open Printf
 
let in_file =  "verilator.lint.bak"
let out_file = "example.txt"

let rec skiplines fd n =
  match n with
    0 -> ()
    |n -> input_line fd;skiplines fd (n-1);;

let split_words = 
  let space_regexp = Str.regexp " " in
  Str.split space_regexp

let member element elements =
  let eq x = (x = element) in
  List.exists eq elements
  
let int_of_string_option s =
  try
    Some (int_of_string s)
  with
    Failure _ -> None
    
let numbers_in_line l =
  let rec helper = function
  [] -> []
  |x::xs ->
    match int_of_string_option x with
    Some v -> v::(helper xs)
    |None -> helper xs
  in
  let line_list = split_words l in
  helper line_list
  
let check_line s =
  let line_list = split_words s in
  if
    (member "expects" line_list) && (member "generates" line_list) then
      let numbers = numbers_in_line s in
      (List.nth numbers 0) > (List.nth numbers 1)
  else false
  
let rec readline ic oc =
  
  (* Read file and display the first line *)
  try 
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    if (check_line line) then (fprintf oc "%s\n" line);
    readline ic oc               (*attempt to read next line*)
  
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    raise e                      (* exit with error: files are closed but
                                    channels are not flushed *)
  
  (* normal exit: all channels are flushed and closed *)
  
let () =
  let ic = open_in in_file in
  let oc = open_out out_file in
  readline ic oc
