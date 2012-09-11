(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

class type message_view =
  object
    inherit GObj.widget
    method clear : unit -> unit
    method push : Interface.message_level -> string -> unit
    method buffer : GText.buffer
  end

let message_view () : message_view =
  let buffer = GText.buffer ~tag_table:Tags.Message.table () in
  let view = GText.view ~buffer
    ~editable:false ~cursor_visible:false ~wrap_mode:`WORD ()
  in
  let default_clipboard = GData.clipboard Gdk.Atom.primary in
  let _ = buffer#add_selection_clipboard default_clipboard in
  let () = view#set_left_margin 2 in
  object
    inherit GObj.widget view#as_widget

    method clear () =
      buffer#set_text ""

    method push level msg =
      let tags = match level with
      | Interface.Error -> [Tags.Message.error]
      | _ -> []
      in
      buffer#insert ~tags msg;
      buffer#insert ~tags "\n"

    method buffer = buffer

  end
