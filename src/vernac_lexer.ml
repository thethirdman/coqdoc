(* Custom lexer for the first expression splitting phase.
 * This lexer splits the entry into a Cst.source list. *)

open Lexing
open Parser

module Vernac_lexer = struct

  exception Lexer_Error of string

  let reg = Str.regexp "\\((\\*\\*\\)\\|\\((\\*\\)\\|\\(\\*)\\)"
  let (tokens : 'a (*Parser.token*) Queue.t)= Queue.create ()
  let dummy_pos = {pos_fname = "dummy"; pos_lnum = 1; pos_bol = 42; pos_cnum =
    1}

  let rec gen_tokens tokens depth = function
    [] -> Queue.push (CONTENT "") tokens; depth
    |(Str.Delim e)::l when e = "(*" -> begin
        if depth = 0 then Queue.push STARTCOM tokens
        else Queue.push (CONTENT "(*") tokens;
      gen_tokens tokens (depth + 1) l end
    |(Str.Delim e)::l when e = "*)" -> begin
        if depth = 1 then Queue.push ENDCOM tokens
        else Queue.push (CONTENT "*)") tokens;
      gen_tokens tokens (depth - 1) l end
    |(Str.Delim e)::l when e = "(**" -> begin
        if depth = 0 then Queue.push STARTDOC tokens
        else Queue.push (CONTENT "(**") tokens;
      gen_tokens tokens (depth + 1) l end
    |(Str.Delim e)::l | (Str.Text e)::l -> begin
      Queue.push (CONTENT e) tokens; gen_tokens tokens depth l end

  let lex in_chan =
    let depth = ref 0 in
    let aux () =
        if (Queue.is_empty tokens) then
            begin try depth := gen_tokens tokens !depth
              (Str.full_split reg ((input_line in_chan ) ^ "\n"))
            with End_of_file -> Queue.push EOF tokens end;
        (Queue.pop tokens, dummy_pos, dummy_pos)
    in aux


end
