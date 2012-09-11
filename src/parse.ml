(* .vdoc parser *)
open Coqtop

module Parser = struct
  exception Invalid_keyword of string

  let command = Str.regexp "^\\(.*\\)@\\([a-zA-Z0-9_-]+\\){\\(.*\\)}\\(.*\\)$"

  let rec parse_line ic oc line =
    if Str.string_match command line 0 then
      let before = Str.matched_group 1 line
      and kw =  Str.matched_group 2 line
      and content = Str.matched_group 3 line
      and after = Str.matched_group 4 line in
      match kw with
      "code" -> ((parse_line ic oc before)
            ^ if content <> "" then (Coqtop.send_coqtop ic oc content) else ""
            ^ after)
      | s -> raise (Invalid_keyword s);
      else
        line

  let parse_file ic oc src dest =
    let src_file = open_in src and dst_file = open_out dest
    and cur_line = ref "" in
    try
      while true do
        cur_line := parse_line ic oc (input_line src_file);
      output_string dst_file (!cur_line ^ "\n");
      done;

    with End_of_file -> close_in src_file; close_out dst_file;

end
