open Unix

let parse s =
  let ic = open_process_in ("date -d \"" ^ s ^ "\" +%s") in
  let line = input_line ic in
  ignore (close_process_in ic);
  int_of_string line

let to_string t =
  let ic = open_process_in ("date -d @" ^ string_of_int t ^ " +%F") in
  let line = input_line ic in
  ignore (close_process_in ic);
  line
