(* This is the option which handles the command line interaction *)

open Str

type frontend_type =
  Vdoc
  | Coqtex

type backend_type =
  Html
  | Latex
  | PrettyPrint

type file_io = {
  i_file : string       ref;
  i_chan : in_channel   ref;
  i_type : frontend_type ref;

  o_file : string       ref;
  o_chan : out_channel  ref;
  o_type : backend_type ref; }

(** If the output file is unspecified the default output will stdout.
 *  If no extension is specified on the output file, coqdoc will chose the
 *  one adapted to the output type*)

let io = { o_file = ref ""; i_file = ref ""; o_chan = ref stdout;
                i_chan = ref stdin; o_type = ref Html; i_type = ref Vdoc}

(* FIXME: make a real usage doc_string *)
let usage = "This is coqdoc ...\n\n" ^
            "Usage: " ^ Sys.argv.(0) ^ " [options] [files]\n"

let print_help = ref false

(* Option list for coqdoc *)
let speclist = [
  ("-h", Arg.Set print_help, "Print this help and exits");
  ("-o", Arg.String (fun s -> io.o_file := s), "Specify output file. If unspecified, default output will be stdout");
  ("--html", Arg.Unit (fun () -> io.o_type := Html), "produce a html document (default)");
  ("--latex", Arg.Unit (fun () -> io.o_type := Latex), "produce a Latex document");
  ("--pp", Arg.Unit (fun () -> io.o_type := PrettyPrint), "produce a Latex document");
  ]

(* Function for parsing anonymous arguments *)
let parse_anon = function
    s when Sys.file_exists s -> io.i_file := s;
    | x -> raise (Arg.Bad ("Invalid argument: " ^ x))

let get_ext str =
    let reg = Str.regexp "\\." in
  try
    let off = Str.search_backward reg str (String.length str) in
    Some (Str.string_after str off)
  with Not_found -> None

let gen_ext prefix = function
  Html ->          (prefix ^ ".html")
  | Latex ->       (prefix ^ ".tex")
  | PrettyPrint -> (prefix ^ ".pp")

let set_entry_type = function
  Some ".tex" -> Coqtex
  | _ -> Vdoc

(* Parses the command line and sets up the variables *)
let parse () =
  Arg.parse speclist parse_anon usage;
  if (!print_help) then
    (print_string (Arg.usage_string speclist usage); exit 0)
  else
    io.i_chan := open_in !(io.i_file);
    if !(io.o_file) <> "" then begin
      let ext = get_ext !(io.o_file) in
      if ext = None then begin
        io.o_file := gen_ext !(io.o_file) !(io.o_type)
      end;
      io.o_chan := open_out !(io.o_file)
    end;
    io.i_type := set_entry_type (get_ext !(io.i_file))
