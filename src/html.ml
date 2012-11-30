(** Html output module, does the translation vdoc -> html *)


open Vdoc
open Printf
open Cst

exception Unhandled_case

let (in_doc: [`None | `Code | `Doc] ref) = ref `None

let doc s = if !in_doc = `Doc then s
            else if !in_doc = `None then (in_doc := `Doc;
              "<div class=\"doc\">" ^ s)
            else (in_doc := `Doc;
              "</div><div class=\"doc\">" ^ s)

let code s = if !in_doc = `Code then s
             else if !in_doc = `None then (in_doc := `Code;
             "<div class=\"code\">" ^ s)
             else (in_doc := `Code; "</div><div class=\"code\">" ^ s)

let initialize () = ()

let header () =
  (*FIXME: add title and charset *)
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n"              ^
  "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"                  ^
  "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n<head>\n"                   ^
  "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf8\"/>\n" ^
  "<link href=\"coqdoc.css\" rel=\"stylesheet\" type=\"text/css\"/>\n"        ^
  "<title>FIXME: TITLE</title>\n</head>\n\n"                                  ^
  "<body>\n\n<div id=\"page\">\n\n<div id=\"header\">\n</div>\n\n"            ^
  "<div id=\"main\">\n\n"

let print_code = function
  Keyword s ->   sprintf "<span class=\"id\" type=\"keyword\">%s</span>" s
  | Ident s ->   sprintf "<span class=\"id\" type=\"var\">%s</span>" s
  | Literal s -> sprintf "%s" s

let doc cst =
  let rec aux = function
    `Vernac s          -> doc(sprintf"[%s]" s)
    | `Pretty_print s  -> doc(sprintf "[[%s]]" s)
    | `Section (lvl,s) -> doc(sprintf "<h%d class=\"section\">%s</h%d>" lvl s
    lvl)
    | `Item (lvl,d)    -> doc("<li>"^ (aux d) ^ "</li>" (* FIXME: nolvl ?*))
    | `Hrule           -> doc("<hr/>")
    | `Emphasis d      -> doc(sprintf "<i>%s</i>" (aux d))
    | `Raw raw         -> doc(raw.html)
    | `Verbatim s      -> doc(sprintf"<tt>%s</tt>" s)
    | `Content s       -> s
    | `List lst        -> doc("<ul>" ^
      (List.fold_right (fun elt acc -> (aux elt) ^ acc) lst "") ^ "</ul>")
    | `Seq lst -> doc((List.fold_right (fun elt acc -> (aux elt) ^ acc) lst ""))
    | `Code c_lst      -> code((List.fold_right (fun elt acc -> (print_code elt) ^ " "^ acc) c_lst ""))
    | _ -> raise Unhandled_case in
    try Some (aux cst) with
      Unhandled_case -> None

(* FIXME: make real function *)
let indent n = " "

let newline () = "<br />"

(*FIXME*)
let index lst = ""

let footer () =
    "<hr/>\nThis page has been generated by " ^
	  "<a href=\"#\">coqdoc</a>\n"            ^
	  "</div>\n\n</div>\n\n</body>\n</html>"
