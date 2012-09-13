(* .vdoc parser *)

(*open Coq*)
open Interface
open Coqtop

module Parser = struct
  exception Invalid_keyword of string
  exception Error_msg of string

  let command = Str.regexp "^\\(.*\\)@\\([a-zA-Z0-9_-]+\\){\\(.*\\)}\\(.*\\)$"


  let treat_content content coqtop =
    if content <> "" then
      match (Coqtop.interp coqtop Coqtop.default_logger content) with
      | Good str -> str
      | Unsafe str -> str
      | Fail (loc, str) -> raise (Error_msg ("Error: " ^ str))
    else
      ""

  let print_goal pref goal =
    ("Id: " ^ goal.goal_id ^ "\n"
      ^ (List.fold_right (fun a b -> pref ^ a ^ "\n" ^ b) goal.goal_hyp "") ^ "\n"
      ^ goal.goal_ccl ^ "\n")

  let treat_goals content coqtop =
    match (Coqtop.goals coqtop) with
       | Good (Some opt) -> "Focused goals:\n"
       ^ (List.fold_left (fun b a -> b ^ ((print_goal "\t" a))) "" opt.fg_goals)
       |_ -> ""


  let rec parse_line coqtop line =
    if Str.string_match command line 0 then
      begin
        let before = Str.matched_group 1 line
        and kw =  Str.matched_group 2 line
        and content = Str.matched_group 3 line
        and after = Str.matched_group 4 line in
        ((parse_line coqtop before) ^
        (match kw with
        | "code" -> treat_content content coqtop
        | "goal" -> treat_goals content coqtop
        | s -> raise (Invalid_keyword s))
        ^ after)
      end
    else
      line

  let parse_file coqtop src dest =
    let src_file = open_in src and dst_file = open_out dest
    and cur_line = ref "" in
    try
      while true do
        cur_line := parse_line coqtop (input_line src_file);
      output_string dst_file (!cur_line ^ "\n");
      done;
        with End_of_file -> close_in src_file; close_out dst_file;

      end
