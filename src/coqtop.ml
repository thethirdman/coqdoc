(* coqtop interaction file *)

module Coqtop = struct

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
    let reg = Str.regexp "\\(\n\\|.\\)*</value>" and buff = Buffer.create 1024 in
    while not (Str.string_match reg (Buffer.contents buff) 0) do
      let c = input_char ic in
      Buffer.add_char buff c;
    done;
  Buffer.contents buff

  let send_coqtop ic oc str =
    output_string oc str; flush oc; read_command ic

end
