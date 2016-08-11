open Printf
 
let out_file = "example.txt"
let failure = ref 0

let split_words = (*split a single string into a list of strings, each corresponding to a word in the original string*)
  let space_regexp = Str.regexp " " in
  Str.split space_regexp
(*string -> string list*)

let member element elements = (*returns true if element is a member of list elements*)
  let eq x = (x = element) in
  List.exists eq elements
(*'a -> 'a list -> bool*)
  
let index element elements = (*returns the index of the first instance of element in elements*)
  let rec helper x (y::ys) n =
    if x = y then n
    else helper x ys (n+1)
  in helper element elements 0
(*'a -> 'a list -> int*)
  
let check_line s = (*returns true if the string s contains "generates" and
                   "expects" and the expected number is greater than the generated number*)
  let line_list = split_words s in
  if
    (member "expects" line_list) && (member "generates" line_list) then
      (int_of_string(List.nth line_list ((index "expects" line_list)+1)))
      > (int_of_string(List.nth line_list ((index "generates" line_list)+1)))
  else false
(*string -> bool*)


let pretty_print line = (*reformats a line of the ugly lint output to be pretty*)
  let line_list = split_words line in
  let file_name = List.nth line_list 1 in
  let length = String.length file_name in
  let truncate_one = Str.first_chars file_name (length-1) in (*remove trailing colon*)
  let fileandline = Str.split (Str.regexp ":") truncate_one in
  let file = List.nth fileandline 0 in
  let line = List.nth fileandline 1 in
  let message = List.tl(List.tl(line_list)) in
  let rec print_list = function
    [] -> ""
    |x::xs -> (sprintf " %s" x) ^ (print_list xs)
  in
  let errors = print_list message in
  sprintf "Warning in file %s on line %s.%s" file line errors
  
let rec readline oc = (*reads a line, if it is flagged by the checker output it and increment the failure counter, read next line*)
  
  (* Read file and display the first line *)
  try 
    let line = input_line stdin in
    if (check_line line) then
      (failure := (!failure)+1;
      let pretty_string = pretty_print line in
      fprintf oc "%s\n" pretty_string);
    readline oc  
  with
    |End_of_file -> 
      if ((!failure)>0) then (printf "Failed with %d deadly warnings.\n" (!failure);exit 1)
    |e ->                      (* some unexpected exception occurs *)
      close_out_noerr oc;           (* emergency closing *)
      raise e                      (* exit with error: files are closed but
                                    channels are not flushed *)
(*unit -> unit*)
  
let () = (*main*)
  let oc = open_out out_file in
  readline oc;
(*unit -> unit*)
