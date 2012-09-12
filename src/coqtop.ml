(* coqtop interaction file *)

module Coqtop = struct

  (* Launches coqtop in ide slave. Returns the pid of the process, and
   * the in/out_channel *)
  let start_coqtop () =
    let (doc2top_r, doc2top_w) = Unix.pipe ()
    and (top2doc_r,top2doc_w) = Unix.pipe () in
    let pid = Unix.create_process "coqtop" [|"coqtop"; "-ideslave"|]
    doc2top_r top2doc_w Unix.stderr in
    assert (pid <> 0);
    Unix.close doc2top_r;
    Unix.close top2doc_w;
    let out_channel = Unix.out_channel_of_descr doc2top_w in
    let in_channel = Unix.in_channel_of_descr top2doc_r in
    set_binary_mode_out oc true;
    set_binary_mode_in ic true;
    (pid,in_channel,out_channel)

    (* Reads a response from coqtop. Those responses are always terminated
     * by '</value>' *)
  let rec read_command in_channel =
    let reg = Str.regexp "\\(\n\\|.\\)*</value>"
    and buff = Buffer.create 1024 in
    while not (Str.string_match reg (Buffer.contents buff) 0) do
      let c = input_char in_channel in
      Buffer.add_char buff c;
    done;
  Buffer.contents buff

  let send_coqtop in_channel out_channel str =
    output_string out_channel str; flush out_channel; read_command in_channel

end
