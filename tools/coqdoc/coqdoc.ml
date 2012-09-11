(* Main definition file for coqdoc *)

let start_coqtop () =
  let (doc2top_r, doc2top_w) = Unix.pipe ()
  and (top2doc_r,top2doc_w) = Unix.pipe () in
  let pid = Unix.create_process "coqtop" [|"coqtop"; "-ideslave"|]
  doc2top_r top2doc_w Unix.stderr in
  assert (pid <> 0);
  Unix.close doc2top_r;
  Unix.close top2doc_w;
  let oc = Unix.out_channel_of_descr doc2top_w in
  let ic = Unix.in_channel_of_descr top2doc_r in
  set_binary_mode_out oc true;
  set_binary_mode_in ic true;
  (pid,ic,oc)

let rec read_command ic =
  let reg = Str.regexp ".*</value>" and buff = Buffer.create 1024 in
  while not (Str.string_match reg (Buffer.contents buff) 0) do
    let c = input_char ic in
    if c <> '\n' then
      Buffer.add_char buff c;
  done;
  Buffer.contents buff

(* Handler function for doc generation *)
let gen_doc () =
    let (pid, ic, oc) = start_coqtop () in
    output oc "<call val=\"interp\">Check 5.</call>" 0 34; flush oc;
    print_string (read_command ic);
    Unix.kill pid Sys.sigkill

(* Coqdoc's command line parser *)
let usage = "This is coqdoc ...\nUsage: " ^ Sys.argv.(0) ^ " [options] [files]\n"
let file = ref ""
let html_or_tex = ref true
let speclist = [
  ("-html", Arg.Unit (fun _ -> html_or_tex := true ), "Select html output");
  ("-tex", Arg.String (fun s -> html_or_tex := false), "Select tex output")]

let parse_anon = function
  s when Sys.file_exists s -> file := s;
  | x -> raise (Arg.Bad ("Invalid argument: " ^ x))

let _ =
  Arg.parse speclist parse_anon usage;
  if !file <> "" then
    gen_doc ()
  else
    print_string usage;
