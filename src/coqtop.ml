open Interface

module Coqtop = struct

exception Coqtop_crash

type coqtop = {
  (*sup_args : string list;*)
  pid : int;
  (* Unix process id *)
  cout : in_channel;
  cin : out_channel;
  xml_parser : Xml_parser.t;
}

let default_logger lvl msg = match lvl with
  | Debug s -> print_string ("Debug: " ^ msg ^ "\n")
  | Info    -> print_string ("Info: " ^ msg ^ "\n")
  | Notice  -> print_string ("Notice: " ^ msg ^ "\n")
  | Warning -> print_string ("Warning: " ^ msg ^ "\n")
  | Error   -> print_string ("Error: " ^ msg ^ "\n")

let open_process_pid prog args =
  let (ide2top_r,ide2top_w) = Unix.pipe () in
  let (top2ide_r,top2ide_w) = Unix.pipe () in
  let pid = Unix.create_process prog args ide2top_r top2ide_w Unix.stderr in
  assert (pid <> 0);
  Unix.close ide2top_r;
  Unix.close top2ide_w;
  let oc = Unix.out_channel_of_descr ide2top_w in
  let ic = Unix.in_channel_of_descr top2ide_r in
  set_binary_mode_out oc true;
  set_binary_mode_in ic true;
  (pid,ic,oc)

  (*FIXME: have real coqtop path *)
let spawn args =
  let prog = "/home/ripault/Prog/coq/trunk/bin/" ^ "coqtop"  ^ ".byte" in
  let args = Array.of_list (prog :: "-ideslave" :: args) in
  let (pid, ic, oc) = open_process_pid prog args in
  let p = Xml_parser.make (Xml_parser.SChannel ic) in
  Xml_parser.check_eof p false;
  {
    pid = pid;
    cin = oc;
    cout = ic;
    xml_parser = p;
  }

let eval_call coqtop logger (c:'a Serialize.call) =
  (** Retrieve the messages sent by coqtop until an answer has been received *)
  let rec loop () =
    let xml = Xml_parser.parse coqtop.xml_parser in
    if Serialize.is_message xml then
      begin
      let message = Serialize.to_message xml in
      let level = message.Interface.message_level in
      let content = message.Interface.message_content in
      let () = logger level content  in
      loop ()
      end
    else (Serialize.to_answer xml c)
  in
  try
    Xml_utils.print_xml coqtop.cin (Serialize.of_call c);
    flush coqtop.cin;
    loop ()
  with
  | Serialize.Marshal_error s ->
    (* the protocol was not respected... *)
    raise (Serialize.Marshal_error s)
  | err ->
    (* if anything else happens here, coqtop is most likely dead *)
    let msg = Printf.sprintf "Error communicating with pid [%i]: %s"
      coqtop.pid (Printexc.to_string err)
    in
    prerr_string msg; (*FIXME: real error handling*)
    raise Coqtop_crash

module PrintOpt =
struct
  type t = string list

  let width_ref = ref None
  let set_printing_width w = width_ref := Some w

  let width = ["Printing"; "Width"]
  let implicit = ["Printing"; "Implicit"]
  let coercions = ["Printing"; "Coercions"]
  let raw_matching = ["Printing"; "Matching"]
  let notations = ["Printing"; "Notations"]
  let all_basic = ["Printing"; "All"]
  let existential = ["Printing"; "Existential"; "Instances"]
  let universes = ["Printing"; "Universes"]

  let state_hack = Hashtbl.create 11
  let _ = List.iter (fun opt -> Hashtbl.add state_hack opt false)
            [ implicit; coercions; raw_matching; notations; all_basic; existential; universes ]

  let set coqtop options =
    let () = List.iter (fun (name, v) -> Hashtbl.replace state_hack name v) options in
    let options = List.map (fun (name, v) -> (name, Interface.BoolValue v)) options in
    let options = (width, Interface.IntValue !width_ref):: options in
    match eval_call coqtop default_logger (Serialize.set_options options) with
    | Interface.Good () -> ()
    | _ -> raise (Failure "Cannot set options.")

  let enforce_hack coqtop =
    let elements = Hashtbl.fold (fun opt v acc -> (opt, v) :: acc) state_hack [] in
    set coqtop elements

end

let evars coqtop =
  let () = PrintOpt.enforce_hack coqtop in
  eval_call coqtop default_logger Serialize.evars

let goals coqtop =
  let () = PrintOpt.enforce_hack coqtop in
  eval_call coqtop default_logger Serialize.goals

let interp coqtop log ?(raw=false) ?(verbose=true) s =
  eval_call coqtop log (Serialize.interp (raw,verbose,s))


let hints coqtop = eval_call coqtop default_logger Serialize.hints
let inloadpath coqtop s = eval_call coqtop default_logger (Serialize.inloadpath s)
let mkcases coqtop s = eval_call coqtop default_logger (Serialize.mkcases s)
let rewind coqtop i = eval_call coqtop default_logger (Serialize.rewind i)
let search coqtop flags = eval_call coqtop default_logger (Serialize.search flags)
let status coqtop = eval_call coqtop default_logger Serialize.status
let parse coqtop s = eval_call coqtop default_logger (Serialize.parse s)
end
