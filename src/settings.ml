(* This is the option which handles the command line interaction *)

type backend_type =
  Html
  | Latex
  | PrettyPrint

type file_io = {
  o_file : string       ref;
  i_file : string       ref;
  o_chan : out_channel  ref;
  i_chan : in_channel   ref;
  o_type : backend_type ref}

let io = { o_file = ref ""; i_file = ref ""; o_chan = ref stdout;
                i_chan = ref stdin; o_type = ref Html}

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

let parse () =
  Arg.parse speclist parse_anon usage;
  if (!print_help) then
    (print_string (Arg.usage_string speclist usage); exit 0)
  else
    io.i_chan := open_in !(io.i_file);
    if !(io.o_file) <> "" then
      io.o_chan := open_out !(io.o_file)
